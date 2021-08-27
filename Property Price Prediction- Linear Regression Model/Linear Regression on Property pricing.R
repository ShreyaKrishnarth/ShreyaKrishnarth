Train <- read.csv(file.choose(), stringsAsFactors = F)
Test <- read.csv(file.choose(), stringsAsFactors = F)
head(Train)
head(Test)
names(Train)
Test$Sale_Price <- NA
Train$is.Train <- T
Test$is.Train <- F
all <- rbind(Train, Test)
which(sapply(all, is.numeric))
which(sapply(all, is.character))
a <- which(colSums(is.na(all))>0)
sort(colSums(sapply(all[a], is.na)), decreasing = T)
# pool variables
table(all$Pool_Quality)
# replacing na values with none
all$Pool_Quality[is.na(all$Pool_Quality)] <- 'None'

# Label encoding
quality <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
library(plyr)
all$Pool_Quality <- as.integer(revalue(all$Pool_Quality, quality))
table(all$Pool_Quality)
all[all$Pool_Area>0 & all$Pool_Quality==0, c('Pool_Area', 'Pool_Quality')]
all$Pool_Quality[2420] <- 3
all$Pool_Quality[2503] <- 2
all$Pool_Quality[2599] <- 4
# Miscellaneous_Feature  
table(all$Miscellaneous_Feature)
all$Miscellaneous_Feature[is.na(all$Miscellaneous_Feature)] <- 'None'
all$Miscellaneous_Feature <- as.factor(all$Miscellaneous_Feature)
all$Miscellaneous_Feature <- as.factor(all$Miscellaneous_Feature)
# Lane_type
table(all$Lane_Type)
all$Lane_Type[is.na(all$Lane_Type)] <- 'None'
all$Lane_Type <- as.factor(all$Lane_Type)
ggplot(all[!is.na(all$Sale_Price),], aes(x=Lane_Type, y=Sale_Price)) +
  geom_bar(stat = "summary", data = all) 
# Fence_Quality
table(all$Fence_Quality)
all$Fence_Quality[is.na(all$Fence_Quality)] <- 'None'
all$Fence_Quality <- as.factor(all$Fence_Quality)
ggplot(all[!is.na(all$Sale_Price),], aes(x=Fence_Quality, y=Sale_Price)) +
  geom_bar(stat = "summary", data = all) 
# Fireplace_Quality
table(all$Fireplace_Quality)
all$Fireplace_Quality[is.na(all$Fireplace_Quality)] <- 'None'
all$Fireplace_Quality <- as.integer(revalue(all$Fireplace_Quality, quality))
table(all$Fireplaces)
# Lot_Extent
summary(all$Lot_Extent)
all$Lot_Extent[is.na(all$Lot_Extent)] <- 69
all$Neighborhood
# Lot_Size
summary(all$Lot_Size)
all$Lot_Size[is.na(all$Lot_Size)] <- 9600
all$Lot_Configuration
table(all$Lot_Configuration)
all$Lot_Configuration <- as.factor(all$Lot_Configuration)
# Garage Variables
all$Garage_Built_Year[is.na(all$Garage_Built_Year)] <- all$Construction_Year[is.na(all$Garage_Built_Year)]
table(all$Garage)
all$Garage[is.na(all$Garage)] <- ' No Garage'
all$Garage <- as.factor(all$Garage)
table(all$Garage)
all$Garage_Finish_Year
table(all$Garage_Finish_Year)
all$Garage_Finish_Year[is.na(all$Garage_Finish_Year)] <- 'None'
finish <- c('None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)
all$Garage_Finish_Year <- as.integer(revalue(all$Garage_Finish_Year, finish))
table(all$Garage_Quality)
all$Garage_Quality[is.na(all$Garage_Quality)] <- 'None'
all$Garage_Quality <- as.integer(revalue(all$Garage_Quality, quality))
table(all$Garage_Condition)
all$Garage_Condition[is.na(all$Garage_Condition)] <- 'None'
all$Garage_Condition <- as.integer(revalue(all$Garage_Condition, quality))
table(all$Garage_Size)
all$Garage_Size[is.na(all$Garage_Size)] <- 3
summary(all$Garage_Area)
all$Garage_Area[is.na(all$Garage_Area)] <- 0
table(all$Basement_Condition)
all$Basement_Condition[is.na(all$Basement_Condition)] <- 'None'
all$Basement_Condition <- as.integer(revalue(all$Basement_Condition, quality))
library(naniar)
missing_values <- miss_var_summary(all)
view(missing_values)
table(all$Basement_Height)
all$Basement_Height[is.na(all$Basement_Height)] <- 'None'
all$Basement_Height <- as.integer(revalue(all$Basement_Height, quality))
table(all$Basement_Height)
summary(all$Total_Basement_Area)
all$Total_Basement_Area[is.na(all$Total_Basement_Area)] <- 989
table(all$BsmtFinType1)
fintype <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
all$BsmtFinType1 <- as.integer(revalue(all$BsmtFinType1, fintype))
table(all$BsmtFinType1)
table(all$BsmtFinType2)
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'
all$BsmtFinType2 <- as.integer(revalue(all$BsmtFinType2, fintype))
table(all$BsmtFinType2)
summary(all$BsmtFinSF2)
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <- 368
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <- 0
summary(all$BsmtUnfSF)
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <- 467
table(all$Exposure_Level)
expo <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd'=4) 
all$Exposure_Level[is.na(all$Exposure_Level)] <- 'None'
all$Exposure_Level <- as.integer(revalue(all$Exposure_Level, expo))
table(all$Exposure_Level)
# Brick variables
summary(all$Brick_Veneer_Area)
all$Brick_Veneer_Area[is.na(all$Brick_Veneer_Area)] <- 0
table(all$Brick_Veneer_Type)
Brk <- c('None' = 0, 'BrkCmn' = 0, 'BrkFace' = 1, 'Stone' = 2) 
all$Brick_Veneer_Type[is.na(all$Brick_Veneer_Type)] <- 'None'
all$Brick_Veneer_Type <- as.integer(revalue(all$Brick_Veneer_Type, Brk))
# zoning class
table(all$Zoning_Class)
all$Zoning_Class[is.na(all$Zoning_Class)] <- 'RMD'
all$Zoning_Class <- as.factor(all$Zoning_Class)
table(all$Utility_Type)
all$Utility_Type <- NULL
table(all$Underground_Full_Bathroom)
all$Underground_Full_Bathroom[is.na(all$Underground_Full_Bathroom)] <- 3
all$Underground_Half_Bathroom[is.na(all$Underground_Half_Bathroom)] <- 2
# functional variables
table(all$Functional_Rate)
Func <- c('MajD1' = 0, 'MajD2' = 1, 'MD' = 2, 'MD1' = 3, 'MD2' = 4, 'Mod' = 5, 'MS'=6, 'SD'=7, 'Sev'=8, 'TF'=9) 
all$Functional_Rate[is.na(all$Functional_Rate)] <- 9
all$Functional_Rate <- as.integer(revalue(all$Functional_Rate, Func))
# exterior variables
table(all$Exterior2nd)
all$Exterior2nd[is.na(all$Exterior2nd)] <- 'CBlock'
all$Exterior2nd <- as.factor(all$Exterior2nd)
table(all$Exterior1st)
all$Exterior1st[is.na(all$Exterior1st)] <- 'Stone'
all$Exterior1st <- as.factor(all$Exterior1st)
table(all$Exterior_Condition)
all$Exterior_Condition <- as.integer(revalue(all$Exterior_Condition, quality))
table(all$Exterior_Material)
all$Exterior_Material <- as.integer(revalue(all$Exterior_Material, quality))
table(all$Electrical_System)
all$Electrical_System[is.na(all$Electrical_System)] <- 'FuseP'
all$Electrical_System <- as.factor(all$Electrical_System)
table(all$Kitchen_Quality)
all$Kitchen_Quality[is.na(all$Kitchen_Quality)] <- 3
all$Kitchen_Quality <- as.integer(revalue(all$Kitchen_Quality, quality))
# sale type
table(all$Sale_Type)
all$Sale_Type[is.na(all$Sale_Type)] <- 'Con'
all$Sale_Type <- as.factor(all$Sale_Type)
table(all$Sale_Condition)
all$Sale_Condition <- as.factor(all$Sale_Condition)
# roads
table(all$Road_Type)
# one hot encoding
all$Road_Type <- ifelse(all$Road_Type=='Gravel', 0,1 )
table(all$Pavedd_Drive)
all$Pavedd_Drive <- as.integer(revalue(all$Pavedd_Drive, c('N'=0, 'P'=1, 'Y'=2)))
# foundation 
table(all$Foundation_Type)
all$Foundation_Type <- as.factor(all$Foundation_Type)
# heating and air condition
table(all$Heating_Quality)
all$Heating_Quality <- as.integer(revalue(all$Heating_Quality, quality ))
table(all$Heating_Type)
all$Heating_Type <- as.factor(all$Heating_Type)
table(all$Air_Conditioning)
all$Air_Conditioning <- ifelse(all$Air_Conditioning=='N', 0,1 )
# neighborhood
table(all$Neighborhood)
all$Neighborhood <- as.factor(all$Neighborhood)
# land and roof
table(all$Roof_Quality)
all$Roof_Quality <- as.factor(all$Roof_Quality)
table(all$Roof_Design)
all$Roof_Design <- as.factor(all$Roof_Design)
table(all$Land_Outline)
all$Land_Outline <- as.factor(all$Land_Outline)
table(all$Property_Shape)
all$Property_Shape <- as.integer(revalue(all$Property_Shape, c('Reg'=0, 'IR1'=1, 'IR2'=2, 'IR3'=3)))
table(all$Property_Slope)
all$Property_Slope <- as.integer(revalue(all$Property_Slope, c('GS'=0, 'MS'=1, 'SS'=2)))
#house 
table(all$House_Condition)
table(all$House_Design)
all$House_Design <- as.factor(all$House_Design)
table(all$House_Type)
all$House_Type <- as.factor(all$House_Type)
# condition 1&2
table(all$Condition1)
all$Condition1 <- as.factor(all$Condition1)
table(all$Condition2)
all$Condition2 <- as.factor(all$Condition2)
numvar <- which(sapply(all, is.numeric))
all.num <- all[, numvar]
facvar <- which(sapply(all, is.factor))
all.factor <- all[, facvar]
str(all)
ggplot(all, aes(x=Miscellaneous_Feature, y=Sale_Price) )+
geom_bar(stat = 'summary', data= all)
# one hot encoding
library(fastDummies)
dummies <- dummy_cols(all.factor, remove_selected_columns = T, remove_most_frequent_dummy = T)
final <- cbind(all.num, dummies)
# checking correlation
cor <- cor(final, use = "pairwise.complete.obs")
# sorting of correlation
cor_sort <- as.matrix(sort(cor[, 'Sale_Price'], decreasing = T))
cor_high <- names(which(apply(cor_sort, 1, function(x) abs(x)>0.5)))
cor <- cor[cor_high, cor_high]
library(corrplot)
corrplot.mixed(cor, tl.col="black", tl.pos="lt", tl.cex=0.7, tl.cex=0.7, number.cex=0.7)
cor_high
final$Overall_Material <- NULL
final$Grade_Living_Area <- NULL
final$Exterior_Material <- NULL 
final$Kitchen_Quality <- NULL
final$Garage_Size <- NULL
final$Total_Basement_Area <- NULL
final$First_Floor_Area <- NULL
final$Basement_Height <- NULL 
final$Full_Bathroom_Above_Grade <- NULL 
final$Garage_Finish_Year <- NULL 
final$Rooms_Above_Grade <- NULL 
final$Construction_Year <- NULL 
final$Fireplace_Quality <- NULL 
final$Garage_Built_Year <- NULL
final$Remodel_Year <- NULL
isTrain <- all$is.Train
final <- cbind(final, isTrain)
# splitting the data
Train <- final[final$isTrain==T, ]
table(final$isTrain)
Test <- final[final$isTrain==F, ]
Train$isTrain <- NULL
Test$isTrain <- NULL
library(car)
model <- lm(formula = Sale_Price~., data = Train)
vif(model)
alias(model)
model2 <- lm(formula = Sale_Price ~ Id + Building_Class + Lot_Extent + Lot_Size + Road_Type + 
               Property_Shape + Property_Slope + House_Condition + Brick_Veneer_Type + 
               Brick_Veneer_Area + Exterior_Condition + Basement_Condition + 
               Exposure_Level + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + 
               BsmtFinSF2 + BsmtUnfSF + Heating_Quality + Air_Conditioning + 
               Second_Floor_Area + LowQualFinSF + Underground_Full_Bathroom + 
               Underground_Half_Bathroom + Half_Bathroom_Above_Grade + Bedroom_Above_Grade + 
               Kitchen_Above_Grade + Functional_Rate + Fireplaces + Garage_Area + 
               Garage_Quality + Garage_Condition + Pavedd_Drive + W_Deck_Area + 
               Open_Lobby_Area + Enclosed_Lobby_Area + Three_Season_Lobby_Area + 
               Screen_Lobby_Area + Pool_Area + Pool_Quality + Miscellaneous_Value + 
               Month_Sold + Year_Sold + Zoning_Class_Commer + Zoning_Class_FVR + 
               Zoning_Class_RHD + Zoning_Class_RMD + Lane_Type_Grvl + Lane_Type_Paved + 
               Land_Outline_Bnk + Land_Outline_HLS + Land_Outline_Low + 
               Lot_Configuration_C + Lot_Configuration_CulDSac + Lot_Configuration_FR2P + 
               Lot_Configuration_FR3P + Neighborhood_Blmngtn + Neighborhood_Blueste + 
               Neighborhood_BrDale + Neighborhood_BrkSide + Neighborhood_ClearCr + 
               Neighborhood_CollgCr + Neighborhood_Crawfor + Neighborhood_Edwards + 
               Neighborhood_Gilbert + Neighborhood_IDOTRR + Neighborhood_MeadowV + 
               Neighborhood_Mitchel + Neighborhood_NoRidge + Neighborhood_NPkVill + 
               Neighborhood_NridgHt + Neighborhood_NWAmes + Neighborhood_OldTown + 
               Neighborhood_Sawyer + Neighborhood_SawyerW + Neighborhood_Somerst + 
               Neighborhood_StoneBr + Neighborhood_SWISU + Neighborhood_Timber + 
               Neighborhood_Veenker + Condition1_Artery + Condition1_Feedr + 
               Condition1_NoRMD + Condition1_PosA + Condition1_PosN + Condition1_RRAe + 
               Condition1_RRAn + Condition1_RRNe + Condition1_RRNn + Condition2_Artery + 
               Condition2_Feedr + Condition2_NoRMD + Condition2_PosA + Condition2_PosN + 
               Condition2_RRAe + Condition2_RRAn + Condition2_RRNn + House_Type_2fmCon + 
               House_Type_Duplex + House_Type_Twnhs + House_Type_TwnhsE + 
               House_Design_1.5Fin + House_Design_1.5Unf + House_Design_2.5Fin + 
               House_Design_2.5Unf + House_Design_2Story + House_Design_SFoyer + 
               House_Design_SLvl + Roof_Design_Flat + Roof_Design_Gambrel + 
               Roof_Design_Hip + Roof_Design_Mansard + Roof_Design_Shed + 
               Roof_Quality_CT + Roof_Quality_M + Roof_Quality_ME + Roof_Quality_R + 
               Roof_Quality_TG + Roof_Quality_WS + Roof_Quality_WSh + Exterior1st_AsbShng + 
               Exterior1st_AsphShn + Exterior1st_BrkComm + Exterior1st_BrkFace + 
               Exterior1st_CB + Exterior1st_CBlock + Exterior1st_CemntBd + 
               Exterior1st_HdBoard + Exterior1st_ImStucc + Exterior1st_MetalSd + 
               Exterior1st_Plywood + Exterior1st_Stone + Exterior1st_Stucco + 
               `Exterior1st_Wd Sdng` + Exterior1st_WdShing + Exterior2nd_AsbShng + 
               Exterior2nd_AsphShn + `Exterior2nd_Brk Cmn` + Exterior2nd_BrkFace + 
               Exterior2nd_CBlock + Exterior2nd_CmentBd + Exterior2nd_HdBoard + 
               Exterior2nd_ImStucc + Exterior2nd_MetalSd + Exterior2nd_Other + 
               Exterior2nd_Plywood + Exterior2nd_Stone + Exterior2nd_Stucco + 
               `Exterior2nd_Wd Sdng` + `Exterior2nd_Wd Shng` + Foundation_Type_BT + 
               Foundation_Type_CB + Foundation_Type_S + Foundation_Type_SL + 
               Foundation_Type_W + Heating_Type_Floor + Heating_Type_GasW + 
               Heating_Type_Grav + Heating_Type_OthW + Heating_Type_Wall + 
               Electrical_System_FuseA + Electrical_System_FuseF + Electrical_System_FuseP + 
               Electrical_System_Mix + `Garage_ No Garage` + Garage_2TFes + 
               Garage_2Types + Garage_Basment + Garage_BuiltIn + Garage_CarPort + 
               Garage_Detchd + Fence_Quality_GdPrv + Fence_Quality_GdWo + 
               Fence_Quality_MnPrv + Fence_Quality_MnWw + Miscellaneous_Feature_Gar2 + 
               Miscellaneous_Feature_Othr + Miscellaneous_Feature_Shed + 
               Miscellaneous_Feature_TenC + Sale_Type_COD + Sale_Type_Con + 
               Sale_Type_ConLD + Sale_Type_ConLI + Sale_Type_ConLw + Sale_Type_CWD + 
               Sale_Type_New + Sale_Type_Oth + Sale_Condition_AbnoRMDl + 
               Sale_Condition_Abnorml + Sale_Condition_AdjLand + Sale_Condition_Alloca + 
               Sale_Condition_Family + Sale_Condition_NoRMDal + Sale_Condition_Partial, data = Train)
vif(model2)
#step(model)
model2 <- lm(formula = Sale_Price ~ Building_Class + Lot_Extent + Lot_Size + 
               Road_Type + House_Condition + Brick_Veneer_Type + Brick_Veneer_Area + 
               Basement_Condition + Exposure_Level + BsmtFinSF1 + BsmtFinSF2 + 
               BsmtUnfSF + Heating_Quality + LowQualFinSF + 
               Underground_Full_Bathroom + Half_Bathroom_Above_Grade + Bedroom_Above_Grade + 
               Kitchen_Above_Grade + Fireplaces + Garage_Area + 
               Open_Lobby_Area + Three_Season_Lobby_Area + Screen_Lobby_Area + 
               Pool_Quality + Month_Sold + Zoning_Class_Commer + Land_Outline_HLS + 
               Land_Outline_Low + Lot_Configuration_CulDSac + Lot_Configuration_FR3P + 
               Neighborhood_Blmngtn + Neighborhood_Blueste + Neighborhood_BrDale + 
               Neighborhood_BrkSide + Neighborhood_CollgCr + Neighborhood_Crawfor + 
               Neighborhood_Gilbert + Neighborhood_Mitchel + Neighborhood_NoRidge + 
               Neighborhood_NPkVill + Neighborhood_NridgHt + Neighborhood_NWAmes + 
               Neighborhood_Sawyer + Neighborhood_SawyerW + Neighborhood_Somerst + 
               Neighborhood_StoneBr + Neighborhood_Timber + Neighborhood_Veenker + 
               Condition1_Artery + Condition1_Feedr + Condition1_RRAe + 
               Condition2_PosA + Condition2_PosN + House_Type_Twnhs + House_Type_TwnhsE + 
               House_Design_1.5Fin + House_Design_2.5Fin + House_Design_2.5Unf + 
               Roof_Design_Hip + Roof_Quality_CT + 
               Roof_Quality_WSh + Exterior1st_AsphShn + Exterior1st_BrkFace + 
               Exterior1st_Stone + Exterior1st_Stucco + Exterior2nd_CmentBd + 
               Exterior2nd_HdBoard + Exterior2nd_Other + Exterior2nd_Stucco + 
               Foundation_Type_BT + Foundation_Type_CB + Foundation_Type_SL + 
               Foundation_Type_W  + Garage_Basment + 
               Fence_Quality_GdPrv + Miscellaneous_Feature_TenC + Sale_Type_ConLD + 
               Sale_Type_CWD + Sale_Type_New + Sale_Condition_Abnorml, data = Train)
vif(model2) # should not be more than 5
par(mfrow= c(2, 2))
plot(model2)
boxplot(Train$Sale_Price)
quantile(Train$Sale_Price, seq(0,1,0.02))
Train <- Train[Train$Sale_Price<394939, ]
boxplot(Train$Sale_Price)$stat
Train$Sale_Price <- ifelse(Train$Sale_Price>328000, 328000, Train$Sale_Price)
model3 <- lm(formula = Sale_Price ~ Building_Class + Lot_Extent + Lot_Size + 
               Road_Type + House_Condition + Brick_Veneer_Type + Brick_Veneer_Area + 
               Basement_Condition + Exposure_Level + BsmtFinSF1 + BsmtFinSF2 + 
               BsmtUnfSF + Heating_Quality + LowQualFinSF + 
               Underground_Full_Bathroom + Half_Bathroom_Above_Grade + Bedroom_Above_Grade + 
               Kitchen_Above_Grade + Fireplaces + Garage_Area + 
               Open_Lobby_Area + Three_Season_Lobby_Area + Screen_Lobby_Area + 
               Pool_Quality + Month_Sold + Zoning_Class_Commer + Land_Outline_HLS + 
               Land_Outline_Low + Lot_Configuration_CulDSac + Lot_Configuration_FR3P + 
               Neighborhood_Blmngtn + Neighborhood_Blueste + Neighborhood_BrDale + 
               Neighborhood_BrkSide + Neighborhood_CollgCr + Neighborhood_Crawfor + 
               Neighborhood_Gilbert + Neighborhood_Mitchel + Neighborhood_NoRidge + 
               Neighborhood_NPkVill + Neighborhood_NridgHt + Neighborhood_NWAmes + 
               Neighborhood_Sawyer + Neighborhood_SawyerW + Neighborhood_Somerst + 
               Neighborhood_StoneBr + Neighborhood_Timber + Neighborhood_Veenker + 
               Condition1_Artery + Condition1_Feedr + Condition1_RRAe + 
               Condition2_PosA + Condition2_PosN + House_Type_Twnhs + House_Type_TwnhsE + 
               House_Design_1.5Fin + House_Design_2.5Fin + House_Design_2.5Unf + 
               Roof_Design_Hip + Roof_Quality_CT + 
               Roof_Quality_WSh + Exterior1st_AsphShn + Exterior1st_BrkFace + 
               Exterior1st_Stone + Exterior1st_Stucco + Exterior2nd_CmentBd + 
               Exterior2nd_HdBoard + Exterior2nd_Other + Exterior2nd_Stucco + 
               Foundation_Type_BT + Foundation_Type_CB + Foundation_Type_SL + 
               Foundation_Type_W  + Garage_Basment + 
               Fence_Quality_GdPrv + Miscellaneous_Feature_TenC + Sale_Type_ConLD + 
               Sale_Type_CWD + Sale_Type_New + Sale_Condition_Abnorml, data = Train)
plot(model3)
par(mar=c(1,1,1,1))
library(predictmeans)
cook <- cooks.distance(model3)
plot(cook)
CookD(model3)
Train <- Train[-c(524,1066,826),]
Test$Sale_Price <- predict(model3, Test)
Train.predict <- predict(model3, Train)
actual <- Train$Sale_Price
check <- cbind(Train.predict, actual)
check <- as.data.frame(check)
ggplot(check, aes(x=actual, y=Train.predict))+
  geom_point(data =  check, stat = 'identity')+
  geom_smooth(method = 'lm')
