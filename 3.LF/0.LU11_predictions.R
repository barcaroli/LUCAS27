#----------------------------------------------------------
# Script name: 0.LU11_predictions.R
#----------------------------------------------------------
# Description: Builds a supervised learning model (random forest)
# to predict LU11 membership for every unit in the LUCAS master
# frame as an eligibility proxy for Landscape Features analysis.
# Main steps:
# 1) Load master frame and labelled survey data.
# 2) Prepare training/testing samples for LU11.
# 3) Train random forest classifier and inspect importance.
# 4) Score the master frame and flag LF eligibility.
# 5) Export model and updated master with LU11 predictions.
#----------------------------------------------------------
# Input datasets:
# - master_complete.RData  (master frame with explanatory vars)
# - Survey_2022_wgt_2nd_phase.txt (labelled survey observations)
#-----------------------------------------------------------
# Output datasets:
# - rf_U11.RData (trained LU11 classifier)
# - master_complete.RData (updated with LU11 predictions)
# - eligibility_LF_by_countries.csv (eligibility summary)
#-----------------------------------------------------------

# ---- Setup environment and load data ---------------------------------
setwd("D:/Google Drive/LUCAS 2026/dati")
load("master_complete.RData")
library(data.table)
library(caret)
library(randomForest)
esplicative <- c("NUTS0_24","ELEV2","CLC18_R","TCD18_100","FTY18_10",
                 "GRA18_100","IMP18_100_cl","STR25","DIST_cl_CLC18_m","LC_pred")


# ---- Ingest and prepare labelled survey sample -----------------------
s <- fread("Survey_2022_wgt_2nd_phase.txt")
s <- as.data.frame(s)
s$LU <- as.factor(substr(s$SURVEY_LU1,1,2))
table(s$LU)
s$LU11 <- ifelse(s$SURVEY_LU1 == "U111" | s$SURVEY_LU1 == "U112" | s$SURVEY_LU1 == "U113",1,0)
s$LU11 <- as.factor(s$LU11)
table(s$LU11)

# Merge
sm <- merge(master_tot,s[,c("POINT_ID","LU","LU11")])
sm <- sm[,c("POINT_ID","LU11",esplicative)]
sm <- sm[complete.cases(sm),]

# ---- Create train/test split -----------------------------------------
set.seed(4321)
t1   <- sample(c(1:nrow(sm)),round(nrow(sm)*0.75))
train<- sm[t1,]
prop.table(table(train$LU11))
test <- sm[-t1,]
prop.table(table(test$LU11))

# ---- Train random forest classifier ----------------------------------
set.seed(1234)
rf <- randomForest(y=train$LU11,
                    x=train[, esplicative],
                    data=train,ntree=100, localImp=TRUE,do.trace=10)
save(rf,file="rf_U11.RData")
plot(rf)
imp <- as.data.frame(importance(rf))
imp[order(imp$MeanDecreaseAccuracy),]
imp[order(imp$MeanDecreaseGini,decreasing = T),]


test$pred_LU11 <- predict(rf,newdata=test)
cm <- confusionMatrix(test$pred_LU11,test$LU11)
cm

# ---- Score master frame and flag LF eligibility ----------------------
master <- master_tot[,c("POINT_ID",esplicative)]
master <- master[complete.cases(master),]


master$LU11pred <- predict(rf,newdata=master)
table(master$LU11)


master$eligible_landscape <- ifelse(
  # substr(master$CLC18_R,1,1) == 2 |
  (master$STR25 == 1 |master$STR25 == 2 | master$STR25 == 3) |
    master$LU11 == 1,  1,0)
table(master$eligible_landscape)

t <- as.data.frame(table(master$NUTS0_24,master$eligible_landscape))
write.table(t,"eligibility_LF_by_countries.csv",sep=";",quote=F,row.names=F)

master <- master[,c("POINT_ID","LU11pred")]

master_tot <- merge(master_tot,master)
save(master_tot,file="master_complete.Rdata")
