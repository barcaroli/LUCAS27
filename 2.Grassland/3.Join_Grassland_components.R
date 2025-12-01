# ------------------------------------------------------------------------------
# Overview: final consolidation of components 1 (Grassland) and 2 (Extended
# Grassland) to assemble the full LUCAS 2027 sample.
# Main inputs:
#   - Grassland2027_component1.csv (output of component-1 script)
#   - Grassland2027_component2.csv (output of component-2 script)
# Output:
#   - Grassland2027_sample.csv (combined sample with unified metadata)
# ------------------------------------------------------------------------------
#---------------------------
# 2027 Grassland sub-sample 
#---------------------------
# --- Set working directory ---
setwd("D:/Google Drive/LUCAS 2026/dati")
comp1 <- read.csv("Grassland2027_component1.csv")
comp2 <- read.csv("Grassland2027_component2.csv")

# --- Combine components and export final sample ---
samp <- rbind(comp1,comp2)
samp <- samp[!duplicated(samp$POINT_ID),]
write.table(samp,"Grassland2027_sample.csv",sep=",",quote=F,row.names=F)



