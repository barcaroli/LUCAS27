#---------------------------
# 2027 LF sub-sample 
#---------------------------
# Description: Combines the component 1 (panel) and component 2
# (non-panel) selections into a unified LF 2027 sample with the
# required weights and identifiers.
#---------------------------
# Input datasets:
# - LF2027_panel.csv (component 1 sample)
# - LF2027_nonpanel.csv (component 2 sample)
#---------------------------
# Output datasets:
# - LF2027_sample.csv (merged LF 2027 sample)
#---------------------------

# ---- Setup environment ------------------------------------------------
setwd("D:/Google Drive/LUCAS 2026/dati")

comp1 <- read.csv("LF2027_panel.csv")
comp2 <- read.csv("LF2027_nonpanel.csv")


# ---- Merge components and export sample ------------------------------
samp <- rbind(comp1,comp2)
samp <- samp[!duplicated(samp$POINT_ID),]

write.table(samp,"LF2027_sample_STR25.csv",sep=",",quote=F,row.names=F)



