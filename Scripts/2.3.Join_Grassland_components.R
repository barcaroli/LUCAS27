# ------------------------------------------------------------------------------
# Overview: joins Grassland components 1 and 2, calibrates weights to the
# Grassland frame, and writes the final 2027 Grassland sample.
# Main inputs:
#   - Grassland2027_component1.csv (non-extended Grassland sample)
#   - Grassland2027_component2.csv (extended Grassland sample)
#   - master_complete.RData (frame with LC predictions/strata)
# Outputs:
#   - Grassland2027_sample.csv (final combined sample with calibrated weights)
#   - 2.3.1.Grassland27_distribution_by_NUTS0.png (diagnostic plot)
#   - 2.3.2.Grassland27_distribution_by_LCpred.png (diagnostic plot)
# ------------------------------------------------------------------------------

setwd("D:/Google Drive/LUCAS 2026/dati")

GR1 <- read.csv("Grassland2027_component1.csv")
table(GR1$LC1)
GR1$ones<-1
GR1<-GR1[GR1$LC1 %in% c("E","D"),]
GR1$NUTS0<-substr(GR1$NUTS2,1,2)
GR2 <- read.csv("Grassland2027_component2.csv")
GR2$ones<-1
GR2<-GR2[GR2$LC1 %in% c("E","D"),]
GR2$NUTS0<-substr(GR2$NUTS2,1,2)
table(GR2$LC1)
GR1$wgt_22<-GR1$WGT_LUCAS*(1/GR1$eligibility_comp)*GR1$wgt_module_22*GR1$wgt_correction_22*GR1$WGT_comp_27
GR2$wgt_22<-GR2$WGT_LUCAS*(1/GR2$eligibility_comp)*GR2$wgt_module_22*GR2$wgt_correction_22*GR2$WGT_comp_27
sum(GR1$WGT_LUCAS*(1/GR1$eligibility_comp)*GR1$wgt_module_22*GR1$wgt_correction_22*GR1$WGT_comp_27)
sum(GR2$WGT_LUCAS*(1/GR2$eligibility_comp)*GR2$wgt_module_22*GR2$wgt_correction_22*GR2$WGT_comp_27)
sum(GR1$wgt_22)
sum(GR2$wgt_22)

load("master_complete.RData")
# Population of interest

frame <- master_tot[master_tot$LC_pred %in% c("E","D"),]
frame$ones<-1
nrow(frame)

table(frame$LC_pred)
a<-aggregate(data=frame, ones~NUTS2_24+STR25,FUN=sum)
b<-aggregate(data=GR1, wgt_22~NUTS2+STR25,FUN=sum)
c<-aggregate(data=GR2, wgt_22~NUTS2+STR25,FUN=sum)
names(a)<-c("NUTS2","STR25","Nh")
names(b)<-c("NUTS2","STR25","Nh_Gr1")
names(c)<-c("NUTS2","STR25","Nh_Gr2")

d<-merge(a,b,by=c("NUTS2","STR25"), all.x = T)
d<-merge(d,c,by=c("NUTS2","STR25"), all.x = T)
d$post1<-d$Nh/d$Nh_Gr1
d$post2<-d$Nh/d$Nh_Gr2

GR1<-merge(GR1,d[, c("NUTS2","STR25","post1")],by=c("NUTS2","STR25"),all.x=TRUE)
GR1$post1 <- ifelse(is.na(GR1$post1),1,GR1$post1)

frame[frame$NAME_NUTS2_24=="DE92",]
GR2<-merge(GR2,d[, c("NUTS2","STR25","post2")],by=c("NUTS2","STR25"),all.x=TRUE)
GR1$post<-GR1$post1
GR2$post<-GR2$post2
GR1$post1<-NULL
GR2$post2<-NULL
GR1$wgt_22_post<-GR1$wgt_22*GR1$post
GR2$wgt_22_post<-GR2$wgt_22*GR2$post
aggregate(data=GR1,wgt_22_post~STR25,FUN=sum)
aggregate(data=GR2,wgt_22_post~STR25,FUN=sum)
table(master_tot$STR25)
GR<-rbind(GR1[,],GR2)
addmargins(table(GR$component))

head(GR[GR$component=="component1",])
head(GR[GR$component=="component2",])
sum(GR$WGT_LUCAS*(1/GR$eligibility_comp)*GR$wgt_module_22*GR$wgt_correction_22*GR$WGT_comp_27*GR$post)

GR$STRATUM <- paste(GR$NUTS2,GR$STR25,sep="*")
frame$STRATUM <- paste(frame$NUTS2_24,frame$STR25,sep="*")
frame$ones <- 1
frame_totals <- aggregate(ones~STRATUM,data=frame,FUN=sum)
sum(frame_totals$ones)

#--------------------------------------------------------
# Calibration component 1
panel <- GR[GR$component == "component1",]
panel$WGT_GR <- panel$wgt_22_post
sum(panel$WGT_GR)

sum(frame_totals$ones)

sample_totals_1 <- aggregate(WGT_GR~STRATUM,data=panel,FUN=sum)
correction <- merge(frame_totals,sample_totals_1,by="STRATUM")
correction$correction_factor <- correction$ones / correction$WGT_GR
summary(correction$correction_factor)

# Here there is all.x=TRUE because some strata in panel are not present in correction
panel <- merge(panel,correction[,c("STRATUM","correction_factor")],all.x=TRUE)
panel$correction_factor[is.na(panel$correction_factor)] <- 1
summary(panel$correction_factor)
panel$WGT_panel <- panel$WGT_GR * panel$correction_factor
sum(panel$WGT_panel)

panel$WGT_GR <- panel$correction_factor <- NULL
colnames(panel)[colnames(panel)=="WGT_panel"] <- "WGT_GR"
sum(panel$WGT_GR)

#--------------------------------------------------------
# Calibration component 2
nonpanel <- GR[GR$component == "component2",]
summary(nonpanel$eligibility_comp)
nonpanel$WGT_GR <- nonpanel$wgt_22_post

sum(nonpanel$WGT_GR)

sum(frame_totals$ones)

sample_totals_2 <- aggregate(WGT_GR~STRATUM,data=nonpanel,FUN=sum)
correction <- merge(frame_totals,sample_totals_2,by="STRATUM")
correction$correction_factor <- correction$ones / correction$WGT_GR
summary(correction$correction_factor)
nonpanel <- merge(nonpanel,correction[,c("STRATUM","correction_factor")])
nonpanel$WGT_nonpanel <- nonpanel$WGT_GR * nonpanel$correction_factor
sum(nonpanel$WGT_nonpanel)

nonpanel$WGT_GR <- nonpanel$correction_factor <- NULL
colnames(nonpanel)[colnames(nonpanel)=="WGT_nonpanel"] <- "WGT_GR"

#--------------------------------------------------------
# Final calibration
GR_join <- rbind(panel,nonpanel)
sum(GR_join$WGT_GR)

correction <- merge(frame_totals,sample_totals_1,by="STRATUM")
correction <- merge(correction,sample_totals_2,by="STRATUM")
correction$WGT_GR_tot <- correction$WGT_GR.x + correction$WGT_GR.y
correction$correction_factor_1 <- correction$WGT_GR.x / correction$WGT_GR_tot
correction$correction_factor_2 <- correction$WGT_GR.y / correction$WGT_GR_tot
GR_join <- merge(GR_join,correction[,c("STRATUM","correction_factor_1","correction_factor_2")],all.x=TRUE)
GR_join$correction_factor_1[is.na(GR_join$correction_factor_1)] <- 1
GR_join$correction_factor_2[is.na(GR_join$correction_factor_2)] <- 1
GR_join$WGT_module_27[GR_join$component == "component1"] <- GR_join$WGT_GR[GR_join$component == "component1"] * 
  GR_join$correction_factor_1[GR_join$component == "component1"]
GR_join$WGT_module_27[GR_join$component == "component2"] <- GR_join$WGT_GR[GR_join$component == "component2"] * 
  GR_join$correction_factor_2[GR_join$component == "component2"]
aggregate(data=GR_join,WGT_module_27~STR25,FUN=sum)

summary(GR_join$correction_factor_1)
summary(GR_join$correction_factor_2)
GR_join$WGT_GR <- GR_join$correction_factor_1 <- GR_join$correction_factor_2 <- GR_join$STRATUM_LF <- NULL

samp <- NULL
samp$STRATUM <- paste(GR_join$NUTS2,GR_join$STR25,sep="*")
samp$POINT_ID <- GR_join$POINT_ID
samp$module <- "GRASSLAND"
samp$component <- GR_join$component
samp$NUTS2 <- GR_join$NUTS2
samp$LC_pred <- GR_join$LC_pred
samp$STR25 <- GR_join$STR25
samp$WGT_LUCAS <- GR_join$WGT_LUCAS 
samp$eligibility_comp <- GR_join$eligibility_comp
samp$wgt_module_22 <- GR_join$wgt_module_22
samp$wgt_correction_22 <- GR_join$wgt_correction_22
samp$WGT_comp_27 <- GR_join$wgt_22_post
samp$WGT_module_27 <- GR_join$WGT_module_27
samp <- as.data.frame(samp)

table(samp$component)
sum(samp$WGT_comp_27)

sum(samp$WGT_module_27)

samp2 <- samp[!duplicated(samp$POINT_ID),]
write.table(samp,"Grassland2027_sample.csv",sep=",",quote=F,row.names=F)

# ---- Diagnostic barplots: percent distributions vs master -----------
# NUTS0 (first 2 chars of NUTS2)
# Colors shared across plots to distinguish sample vs master
png("2.3.1.Grassland27_distribution_by_NUTS0.png", width = 1200, height = 800)
sample_nuts0 <- substr(GR$NUTS2, 1, 2)
master_nuts0 <- substr(frame$NUTS2_24, 1, 2)
all_nuts0 <- sort(unique(c(sample_nuts0, master_nuts0)))
sample_nuts0_pct <- 100 * (table(factor(sample_nuts0, levels = all_nuts0)) / length(sample_nuts0))
master_nuts0_pct <- 100 * (table(factor(master_nuts0, levels = all_nuts0)) / length(master_nuts0))
barplot(rbind(sample_nuts0_pct, master_nuts0_pct),
        beside = TRUE,
        main = "GR 2027 percent distribution by NUTS0 (sample vs master)",
        xlab = "NUTS0",
        ylab = "Percent of points",
        las = 2,
        legend.text = c("Sample", "Master"),
        col = c("steelblue3", "darkorange2"),
        args.legend = list(x = "topright", bty = "n"))
dev.off()
barplot(rbind(sample_nuts0_pct, master_nuts0_pct),
        beside = TRUE,
        main = "GR 2027 percent distribution by NUTS0 (sample vs master)",
        xlab = "NUTS0",
        ylab = "Percent of points",
        las = 2,
        legend.text = c("Sample", "Master"),
        col = c("steelblue3", "darkorange2"),
        args.legend = list(x = "topright", bty = "n"))

# LC_pred distribution
png("2.3.2.Grassland27_distribution_by_LCpred.png", width = 1200, height = 800)
all_lcpred <- sort(unique(c(GR$LC_pred, frame$LC_pred)))
sample_lcpred_pct <- 100 * (table(factor(GR$LC_pred, levels = all_lcpred)) / nrow(GR))
master_lcpred_pct <- 100 * (table(factor(frame$LC_pred, levels = all_lcpred)) / nrow(frame))
barplot(rbind(sample_lcpred_pct, master_lcpred_pct),
        beside = TRUE,
        main = "GR 2027 percent distribution by LC_pred (sample vs master)",
        xlab = "LC_pred",
        ylab = "Percent of points",
        las = 2,
        legend.text = c("Sample", "Master"),
        col = c("steelblue3", "darkorange2"),
        args.legend = list(x = "topright", bty = "n"))
dev.off()
barplot(rbind(sample_lcpred_pct, master_lcpred_pct),
        beside = TRUE,
        main = "GR 2027 percent distribution by LC_pred (sample vs master)",
        xlab = "LC_pred",
        ylab = "Percent of points",
        las = 2,
        legend.text = c("Sample", "Master"),
        col = c("steelblue3", "darkorange2"),
        args.legend = list(x = "topright", bty = "n"))

