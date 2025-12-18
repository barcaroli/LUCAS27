# app.R
options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB upload

library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(webshot2)
library(mapview)  # solo per mapshot()

ui <- fluidPage(
  titlePanel("LUCAS modules map viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file", accept = ".csv"),
      
      selectInput(
        "module", "Module to display",
        choices = c("SOIL", "GRASSLAND", "LF", "COPERNICUS"),
        selected = "COPERNICUS"
      ),
      
      uiOutput("nuts_ui"),
      
      sliderInput(
        "reach_range",
        "Reach probability range",
        min = 0, max = 1, value = c(0, 1), step = 0.01
      ),
      
      selectInput(
        "facet_mode",
        "Faceting mode",
        choices = c("None" = "none", "By LC_pred" = "lc", "By module" = "module"),
        selected = "none"
      ),
      
      selectInput(
        "basemap", "Basemap",
        choices = c(
          "OpenStreetMap"      = "OpenStreetMap",
          "Esri World Imagery" = "Esri.WorldImagery",
          "Esri World Street"  = "Esri.WorldStreetMap",
          "CartoDB Positron"   = "CartoDB.Positron",
          "Stamen Toner Lite"  = "Stamen.TonerLite"
        ),
        selected = "OpenStreetMap"
      ),
      
      hr(),
      downloadButton("download_map", "Download map (PNG)")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map", height = "650px")),
        tabPanel(
          "LC_pred frequencies",
          br(),
          tableOutput("lc_table"),
          br(),
          plotOutput("lc_plot", height = "400px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ---- Read CSV ------------------------------------------------------------
  data_raw <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })
  
  # ---- Dynamic NUTS selector ----------------------------------------------
  output$nuts_ui <- renderUI({
    df <- data_raw()
    validate(need("NUTS2" %in% names(df), "Column 'NUTS2' not found in dataset."))
    nuts_codes <- sort(unique(substr(df$NUTS2, 1, 2)))
    selectInput(
      "nuts2_prefix",
      "Country (NUTS2 prefix)",
      choices = c("EU", nuts_codes),
      selected = "EU"
    )
  })
  
  # ---- Update reach_prob slider dynamically --------------------------------
  observeEvent(data_raw(), {
    df <- data_raw()
    validate(need("reach_prob" %in% names(df), "Column 'reach_prob' not found in dataset."))
    
    rp <- suppressWarnings(as.numeric(gsub(",", ".", df$reach_prob)))
    rp <- rp[is.finite(rp)]
    validate(need(length(rp) > 0, "reach_prob has no finite values."))
    
    rmin <- min(rp)
    rmax <- max(rp)
    step <- if (rmax > rmin) (rmax - rmin) / 200 else 0.01
    
    updateSliderInput(session, "reach_range",
                      min = rmin, max = rmax, value = c(rmin, rmax), step = step
    )
  }, ignoreInit = TRUE)
  
  # ---- Filtered data as sf -------------------------------------------------
  filtered_sf <- reactive({
    df <- data_raw()
    req(input$module, input$nuts2_prefix, input$reach_range)
    
    validate(need(input$module %in% names(df),
                  paste("Column", input$module, "not found in dataset")))
    
    # module filter
    df <- df[df[[input$module]] > 0, , drop = FALSE]
    
    # NUTS filter
    if (input$nuts2_prefix != "EU") {
      df <- df[substr(df$NUTS2, 1, 2) == input$nuts2_prefix, , drop = FALSE]
    }
    
    # reach_prob robust numeric
    validate(need("reach_prob" %in% names(df), "Column 'reach_prob' not found in dataset."))
    df$reach_prob <- suppressWarnings(as.numeric(gsub(",", ".", df$reach_prob)))
    df <- df[is.finite(df$reach_prob), , drop = FALSE]
    
    # apply range
    df <- df[df$reach_prob >= input$reach_range[1] &
               df$reach_prob <= input$reach_range[2], , drop = FALSE]
    
    validate(need(nrow(df) > 0, "No points match the selected filters."))
    
    sf::st_as_sf(df, coords = c("X_WGS84", "Y_WGS84"), crs = 4326, remove = FALSE)
  })
  
  # ---- LC_pred frequencies (with TOTAL) ------------------------------------
  lc_freq <- reactive({
    df <- sf::st_drop_geometry(filtered_sf())
    validate(need("LC_pred" %in% names(df), "Column 'LC_pred' not found in dataset."))
    
    tab <- table(df$LC_pred, useNA = "no")
    out <- data.frame(LC_pred = names(tab), freq = as.numeric(tab), stringsAsFactors = FALSE)
    out$prop <- out$freq / sum(out$freq)
    
    rbind(out, data.frame(LC_pred = "TOTAL", freq = sum(out$freq), prop = 1))
  })
  
  output$lc_table <- renderTable({ lc_freq() }, digits = 3)
  
  output$lc_plot <- renderPlot({
    tab <- lc_freq()
    tab <- tab[tab$LC_pred != "TOTAL", , drop = FALSE]
    barplot(
      tab$freq,
      names.arg = tab$LC_pred,
      main = "LC_pred frequencies (current subset)",
      xlab = "LC_pred",
      ylab = "Frequency"
    )
  })
  
  # ---- Base map: created once ----------------------------------------------
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers[[input$basemap]]) %>%
      setView(lng = 12, lat = 42, zoom = 5)
  })
  
  # ---- Update map with leafletProxy (fix slider left/right) ----------------
  observeEvent(
    list(filtered_sf(), input$facet_mode, input$basemap),
    {
      df_sf <- filtered_sf()
      df0 <- sf::st_drop_geometry(df_sf)
      
      # palette for LC_pred
      classes_all <- sort(unique(df0$LC_pred))
      pal <- colorFactor(palette = "Set2", domain = classes_all)
      
      proxy <- leafletProxy("map", data = df0)
      
      # full reset
      proxy %>%
        clearTiles() %>%
        addProviderTiles(providers[[input$basemap]]) %>%
        clearMarkers() %>%
        clearControls()
      
      # center on subset
      proxy %>%
        setView(
          lng = mean(df0$X_WGS84, na.rm = TRUE),
          lat = mean(df0$Y_WGS84, na.rm = TRUE),
          zoom = 5
        )
      
      if (input$facet_mode == "none") {
        proxy %>%
          addCircleMarkers(
            lng = ~X_WGS84, lat = ~Y_WGS84,
            radius = 3, stroke = FALSE, fillOpacity = 0.7,
            color = ~pal(LC_pred),
            group = "All points"
          ) %>%
          addLegend("topright", pal = pal, values = ~LC_pred, title = "LC_pred", opacity = 1)
        
      } else if (input$facet_mode == "lc") {
        
        classes <- sort(unique(df0$LC_pred))
        groups  <- paste0("LC_pred: ", classes)
        
        for (k in classes) {
          sub <- df0[df0$LC_pred == k, , drop = FALSE]
          proxy %>%
            addCircleMarkers(
              data = sub,
              lng = ~X_WGS84, lat = ~Y_WGS84,
              radius = 3, stroke = FALSE, fillOpacity = 0.7,
              color = pal(k),
              group = paste0("LC_pred: ", k)
            )
        }
        
        proxy %>%
          addLayersControl(
            overlayGroups = groups,
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          # start with all visible (comment these 2 lines if you want all ON)
          # hideGroup(groups) %>% showGroup(groups[1]) %>%
          addLegend("topright", pal = pal, values = ~LC_pred, title = "LC_pred", opacity = 1)
        
      } else { # facet by module
        
        mods   <- c("SOIL", "GRASSLAND", "LF", "COPERNICUS")
        groups <- paste0("Module: ", mods)
        
        for (m in mods) {
          if (!m %in% names(df0)) next
          sub <- df0[df0[[m]] > 0, , drop = FALSE]
          if (nrow(sub) == 0) next
          
          proxy %>%
            addCircleMarkers(
              data = sub,
              lng = ~X_WGS84, lat = ~Y_WGS84,
              radius = 3, stroke = FALSE, fillOpacity = 0.7,
              color = ~pal(LC_pred),
              group = paste0("Module: ", m)
            )
        }
        
        proxy %>%
          addLayersControl(
            overlayGroups = groups,
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          addLegend("topright", pal = pal, values = ~LC_pred, title = "LC_pred", opacity = 1)
      }
    },
    ignoreInit = TRUE
  )
  
  # ---- Download PNG of current map (leaflet widget) ------------------------
  output$download_map <- downloadHandler(
    filename = function() {
      paste0(
        "map_", input$facet_mode, "_",
        input$module, "_",
        input$nuts2_prefix, "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".png"
      )
    },
    content = function(file) {
      # take a fresh snapshot of the CURRENT leaflet widget
      m <- leaflet_map_for_export()
      mapview::mapshot(m, file = file, vwidth = 1400, vheight = 900)
    }
  )
  
  # Build a standalone leaflet map identical to the current view for export
  leaflet_map_for_export <- reactive({
    df_sf <- filtered_sf()
    df0 <- sf::st_drop_geometry(df_sf)
    
    classes_all <- sort(unique(df0$LC_pred))
    pal <- colorFactor(palette = "Set2", domain = classes_all)
    
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers[[input$basemap]]) %>%
      setView(
        lng = mean(df0$X_WGS84, na.rm = TRUE),
        lat = mean(df0$Y_WGS84, na.rm = TRUE),
        zoom = 5
      )
    
    if (input$facet_mode == "none") {
      m %>%
        addCircleMarkers(
          data = df0,
          lng = ~X_WGS84, lat = ~Y_WGS84,
          radius = 3, stroke = FALSE, fillOpacity = 0.7,
          color = ~pal(LC_pred),
          group = "All points"
        ) %>%
        addLegend("topright", pal = pal, values = df0$LC_pred, title = "LC_pred", opacity = 1)
    } else if (input$facet_mode == "lc") {
      classes <- sort(unique(df0$LC_pred))
      groups  <- paste0("LC_pred: ", classes)
      for (k in classes) {
        sub <- df0[df0$LC_pred == k, , drop = FALSE]
        m <- m %>% addCircleMarkers(
          data = sub,
          lng = ~X_WGS84, lat = ~Y_WGS84,
          radius = 3, stroke = FALSE, fillOpacity = 0.7,
          color = pal(k),
          group = paste0("LC_pred: ", k)
        )
      }
      m %>%
        addLayersControl(overlayGroups = groups,
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend("topright", pal = pal, values = df0$LC_pred, title = "LC_pred", opacity = 1)
    } else {
      mods   <- c("SOIL", "GRASSLAND", "LF", "COPERNICUS")
      groups <- paste0("Module: ", mods)
      for (mm in mods) {
        if (!mm %in% names(df0)) next
        sub <- df0[df0[[mm]] > 0, , drop = FALSE]
        if (nrow(sub) == 0) next
        m <- m %>% addCircleMarkers(
          data = sub,
          lng = ~X_WGS84, lat = ~Y_WGS84,
          radius = 3, stroke = FALSE, fillOpacity = 0.7,
          color = ~pal(LC_pred),
          group = paste0("Module: ", mm)
        )
      }
      m %>%
        addLayersControl(overlayGroups = groups,
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend("topright", pal = pal, values = df0$LC_pred, title = "LC_pred", opacity = 1)
    }
  })
}

shinyApp(ui, server)
