## Load the required libraries
library(readr)
library(data.table)
library(r2r)

## Set seed and clean env
set.seed(1) 
rm(list=ls()) 
dev.off()

## Load the required data
setwd('/Users/sanjeevsingh/Dropbox/PHD/Qualifiers/Data Analysis/')
X = read_csv("data/train_X.csv")
X_full = read_csv("data/train_full_X.csv")
y = read_csv("data/train_y.csv")
y = log(y$SalePrice)

## Provide grouping information for group lasso
nom_vars=c(
   'MSZoning'
  ,'Street'
  ,'Alley'
  ,'LandContour'
  ,'LotConfig'
  ,'LandSlope'
  ,'Neighborhood'
  ,'Condition1'
  ,'Condition2'
  ,'BldgType'
  ,'HouseStyle'
  ,'RoofStyle'
  ,'RoofMatl'
  ,'Exterior1st'
  ,'Exterior2nd'
  ,'MasVnrType'
  ,'Foundation'
  ,'Heating'
  ,'CentralAir'
  ,'Electrical'
  ,'GarageType'
  ,'MiscFeature'
  ,'SaleType'
  ,'SaleCondition'
  ,'YrSold'
  ,'MoSold'
  ,'Fence'  
)

group = c(
   'Quality_Cond'	,'Quality_Cond'	,'MSSubClass'	,'Quality_Cond'	,'Quality_Cond'	,'Basement_State'	,'Basement_State'	,'Basement_State'	,'Basement_State'	,'Basement_State'	,'Heating'	,'Kitchen'	,'Functional'
  ,'Heating'	,'Garage_state'	,'Garage_state'	,'Pool'	,'Garage'	,'Garage'	,'Land'	,'Utilities'	,'Land'	,'Land'	,'Land'	,'Land'	,'Land'
  ,'Access'	,'Access'	,'Access'	,'Access'	,'Access'	,'Land'	,'Land'	,'Land'	,'Land'	,'Land'	,'Land'	,'Land'	,'Land'
  ,'Land'	,'Land'	,'Land'	,'Land'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'
  ,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Neighborhood'
  ,'Neighborhood'	,'Neighborhood'	,'Neighborhood'	,'Condition'	,'Condition'	,'Condition'	,'Condition'	,'Condition'	,'Condition'	,'Condition'	,'Condition'	,'Condition'	,'Condition'
  ,'Condition'	,'Condition'	,'Condition'	,'Condition'	,'Condition'	,'Condition'	,'Condition'	,'building'	,'building'	,'building'	,'building'	,'building'	,'building'
  ,'building'	,'building'	,'building'	,'building'	,'building'	,'building'	,'building'	,'Roof'	,'Roof'	,'Roof'	,'Roof'	,'Roof'	,'Roof'
  ,'Roof'	,'Roof'	,'Roof'	,'Roof'	,'Roof'	,'Roof'	,'Roof'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'
  ,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'
  ,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'	,'Exterior'
  ,'Exterior'	,'Exterior'	,'Exterior'	,'Foundation'	,'Foundation'	,'Foundation'	,'Foundation'	,'Foundation'	,'Foundation'				
  ,'HVAC'	,'HVAC'	,'HVAC'	,'HVAC'	,'HVAC'	,'HVAC'	,'HVAC'	,'HVAC'	,'HVAC'	,'HVAC'	,'HVAC'	,'HVAC'	,'HVAC'
  ,'GarageType'	,'GarageType'	,'GarageType'	,'GarageType'	,'GarageType'	,'GarageType'	,'GarageType'	,'MiscFeature'	,'MiscFeature'	,'MiscFeature'	,'MiscFeature'	,'MiscFeature'	,'Sale'
  ,'Sale'	,'Sale'	,'Sale'	,'Sale'	,'Sale'	,'Sale'	,'Sale'	,'Sale'	,'Sale'	,'Sale'	,'Sale'	,'Sale'	,'Sale'
  ,'Sale'	,'YrSold'	,'YrSold'	,'YrSold'	,'YrSold'	,'YrSold'	,'MoSold'	,'MoSold'	,'MoSold'	,'MoSold'	,'MoSold'	,'MoSold'	,'MoSold'
  ,'MoSold'	,'MoSold'	,'MoSold'	,'MoSold'	,'MoSold'	,'Fence'	,'Fence'	,'Fence'	,'Fence'	,'Fence'	,'Outdoor Area'	,'Outdoor Area'	,'Outdoor Area'
  ,'Basement_Area'	,'Basement_Area'	,'Basement_Area'	,'Basement_Area'	,'Liv_Area'	,'Liv_Area'	,'Liv_Area'	,'Liv_Area'	,'Garage'	,'Porch_Deck'	,'Porch_Deck'	,'Porch_Deck'	,'Porch_Deck'
  ,'Porch_Deck'	,'Pool'	,'MiscVal'	,'Bath'	,'Bath'	,'Bath'	,'Bath'	,'Rooms'	,'Kitchen'	,'Rooms'	,'HVAC'	,'Garage'	,'Age'
  ,'Age'	,'Age'											  
)

X_full_cols=c(
   'OverallQual'	,'OverallCond'	,'MSSubClass'	,'ExterQual'	,'ExterCond'	,'BsmtQual'	,'BsmtCond'	,'BsmtExposure'	,'BsmtFinType1'	,'BsmtFinType2'	,'HeatingQC'	,'KitchenQual'	,'Functional'
  ,'FireplaceQu'	,'GarageQual'	,'GarageCond'	,'PoolQC'	,'GarageFinish'	,'PavedDrive'	,'LotShape'	,'Utilities'	,'MSZoning_C (all)'	,'MSZoning_FV'	,'MSZoning_RH'	,'MSZoning_RL'	,'MSZoning_RM'
  ,'Street_Grvl'	,'Street_Pave'	,'Alley_Grvl'	,'Alley_None'	,'Alley_Pave'	,'LandContour_Bnk'	,'LandContour_HLS'	,'LandContour_Low'	,'LandContour_Lvl'	,'LotConfig_Corner'	,'LotConfig_CulDSac'	,'LotConfig_FR2'	,'LotConfig_FR3'
  ,'LotConfig_Inside'	,'LandSlope_Gtl'	,'LandSlope_Mod'	,'LandSlope_Sev'	,'Neighborhood_Blmngtn'	,'Neighborhood_Blueste'	,'Neighborhood_BrDale'	,'Neighborhood_BrkSide'	,'Neighborhood_ClearCr'	,'Neighborhood_CollgCr'	,'Neighborhood_Crawfor'	,'Neighborhood_Edwards'	,'Neighborhood_Gilbert'
  ,'Neighborhood_IDOTRR'	,'Neighborhood_MeadowV'	,'Neighborhood_Mitchel'	,'Neighborhood_NAmes'	,'Neighborhood_NPkVill'	,'Neighborhood_NWAmes'	,'Neighborhood_NoRidge'	,'Neighborhood_NridgHt'	,'Neighborhood_OldTown'	,'Neighborhood_SWISU'	,'Neighborhood_Sawyer'	,'Neighborhood_SawyerW'	,'Neighborhood_Somerst'
  ,'Neighborhood_StoneBr'	,'Neighborhood_Timber'	,'Neighborhood_Veenker'	,'Condition1_Artery'	,'Condition1_Feedr'	,'Condition1_Norm'	,'Condition1_PosA'	,'Condition1_PosN'	,'Condition1_RRAe'	,'Condition1_RRAn'	,'Condition1_RRNe'	,'Condition1_RRNn'	,'Condition2_Artery'
  ,'Condition2_Feedr'	,'Condition2_Norm'	,'Condition2_PosA'	,'Condition2_PosN'	,'Condition2_RRAe'	,'Condition2_RRAn'	,'Condition2_RRNn'	,'BldgType_1Fam'	,'BldgType_2fmCon'	,'BldgType_Duplex'	,'BldgType_Twnhs'	,'BldgType_TwnhsE'	,'HouseStyle_1.5Fin'
  ,'HouseStyle_1.5Unf'	,'HouseStyle_1Story'	,'HouseStyle_2.5Fin'	,'HouseStyle_2.5Unf'	,'HouseStyle_2Story'	,'HouseStyle_SFoyer'	,'HouseStyle_SLvl'	,'RoofStyle_Flat'	,'RoofStyle_Gable'	,'RoofStyle_Gambrel'	,'RoofStyle_Hip'	,'RoofStyle_Mansard'	,'RoofStyle_Shed'
  ,'RoofMatl_CompShg'	,'RoofMatl_Membran'	,'RoofMatl_Metal'	,'RoofMatl_Roll'	,'RoofMatl_Tar&Grv'	,'RoofMatl_WdShake'	,'RoofMatl_WdShngl'	,'Exterior1st_AsbShng'	,'Exterior1st_AsphShn'	,'Exterior1st_BrkComm'	,'Exterior1st_BrkFace'	,'Exterior1st_CBlock'	,'Exterior1st_CemntBd'
  ,'Exterior1st_HdBoard'	,'Exterior1st_ImStucc'	,'Exterior1st_MetalSd'	,'Exterior1st_Plywood'	,'Exterior1st_Stone'	,'Exterior1st_Stucco'	,'Exterior1st_VinylSd'	,'Exterior1st_Wd Sdng'	,'Exterior1st_WdShing'	,'Exterior2nd_AsbShng'	,'Exterior2nd_AsphShn'	,'Exterior2nd_Brk Cmn'	,'Exterior2nd_BrkFace'
  ,'Exterior2nd_CBlock'	,'Exterior2nd_CmentBd'	,'Exterior2nd_HdBoard'	,'Exterior2nd_ImStucc'	,'Exterior2nd_MetalSd'	,'Exterior2nd_Other'	,'Exterior2nd_Plywood'	,'Exterior2nd_Stone'	,'Exterior2nd_Stucco'	,'Exterior2nd_VinylSd'	,'Exterior2nd_Wd Sdng'	,'Exterior2nd_Wd Shng'	,'MasVnrType_BrkCmn'
  ,'MasVnrType_BrkFace'	,'MasVnrType_None'	,'MasVnrType_Stone'	,'Foundation_BrkTil'	,'Foundation_CBlock'	,'Foundation_PConc'	,'Foundation_Slab'	,'Foundation_Stone'	,'Foundation_Wood'				
  ,'Heating_Floor'	,'Heating_GasA'	,'Heating_GasW'	,'Heating_Grav'	,'Heating_OthW'	,'Heating_Wall'	,'CentralAir_N'	,'CentralAir_Y'	,'Electrical_FuseA'	,'Electrical_FuseF'	,'Electrical_FuseP'	,'Electrical_Mix'	,'Electrical_SBrkr'
  ,'GarageType_2Types'	,'GarageType_Attchd'	,'GarageType_Basment'	,'GarageType_BuiltIn'	,'GarageType_CarPort'	,'GarageType_Detchd'	,'GarageType_None'	,'MiscFeature_Gar2'	,'MiscFeature_None'	,'MiscFeature_Othr'	,'MiscFeature_Shed'	,'MiscFeature_TenC'	,'SaleType_COD'
  ,'SaleType_CWD'	,'SaleType_Con'	,'SaleType_ConLD'	,'SaleType_ConLI'	,'SaleType_ConLw'	,'SaleType_New'	,'SaleType_Oth'	,'SaleType_WD'	,'SaleCondition_Abnorml'	,'SaleCondition_AdjLand'	,'SaleCondition_Alloca'	,'SaleCondition_Family'	,'SaleCondition_Normal'
  ,'SaleCondition_Partial'	,'YrSold_2006'	,'YrSold_2007'	,'YrSold_2008'	,'YrSold_2009'	,'YrSold_2010'	,'MoSold_1'	,'MoSold_10'	,'MoSold_11'	,'MoSold_12'	,'MoSold_2'	,'MoSold_3'	,'MoSold_4'
  ,'MoSold_5'	,'MoSold_6'	,'MoSold_7'	,'MoSold_8'	,'MoSold_9'	,'Fence_GdPrv'	,'Fence_GdWo'	,'Fence_MnPrv'	,'Fence_MnWw'	,'Fence_None'	,'LotFrontage'	,'LotArea'	,'MasVnrArea'
  ,'BsmtFinSF1'	,'BsmtFinSF2'	,'BsmtUnfSF'	,'TotalBsmtSF'	,'1stFlrSF'	,'2ndFlrSF'	,'LowQualFinSF'	,'GrLivArea'	,'GarageArea'	,'WoodDeckSF'	,'OpenPorchSF'	,'EnclosedPorch'	,'3SsnPorch'
  ,'ScreenPorch'	,'PoolArea'	,'MiscVal'	,'BsmtFullBath'	,'BsmtHalfBath'	,'FullBath'	,'HalfBath'	,'BedroomAbvGr'	,'KitchenAbvGr'	,'TotRmsAbvGrd'	,'Fireplaces'	,'GarageCars'	,'house_age'
  ,'remod_age'	,'garage_age'											
)

## Create function to do unit change analysis
unit_change_analysis <- function(model, y) {
  df <- data.table(coef(summary(model)), keep.rownames = 'term')
  setDF(df)
  df = df[df$`Pr(>|t|)` < 0.05,][-1,]
  df <- df[order(-abs(exp(df$Estimate)-1)*mean(exp(y))),]
  unit_change=(exp(df$Estimate)-1)*mean(exp(y))
  df=cbind(df$term, unit_change)
  res <- resid(model)
  plot(fitted(model), res)
  abline(0,0)
  qqnorm(res)
  qqline(res)
  
  plot(density(res), main="Density Plot for Residuals", xlab="Residuals")
  return (df)
} 

# create map for group assignment
m <- hashmap()
for (i in 1:length(X_full_cols)) {
  strip_col <- sub("\\_.*", "", X_full_cols[i])
  if (any(strip_col==nom_vars)) {
    m[X_full_cols[i]] <- strip_col
  }else{
    m[X_full_cols[i]] <- X_full_cols[i]
  }

} 
predictors=colnames(X_full)
lasso_grp=unlist(m[predictors])

### 
### 1. Analysis with linear model with no regularization
###
## Results: 
# Multiple R-squared:  0.9065,	Adjusted R-squared:  0.8951 
# Condition number: 16.39
# 157 predictors
# Significant factors: 55
##

lm_no_reg = lm(y ~ ., data=cbind(y, X))
summary(lm_no_reg)
unit_change_analysis(lm_no_reg, y)

### 
### 2. Analysis with LASSO regularization
###

## Results:
# Multiple R-squared:  0.9322,	Adjusted R-squared:  0.9294 
# Condition number: 11.77463
# 218 -> 58 predictors
# Significant factors: 41
##

lm_lasso <- glmnet(x = X_full, y = y, alpha = 1)
plot(lm_lasso, xvar = "lambda", label = TRUE)
cv_mod <- cv.glmnet(x = data.matrix(X_full), y = y, alpha = 1)
plot(cv_mod)
plot(coef(cv_mod))

cm_coef=coef(cv_mod)
cm_coef <- cm_coef[2:length(cm_coef)]
lasso_df <- data.frame(predictors, cm_coef)
X_lasso = X_full[lasso_df[lasso_df$cm_coef!=0,]$predictors]
lm_lasso = lm(y ~ ., data=cbind(y, X_lasso))
summary(lm_lasso)
unit_change_analysis(lm_lasso, y)

### 
### 3. Analysis with group LASSO regularization
###

## Results:
# Multiple R-squared:  0.9412,	Adjusted R-squared:  0.9352 
# Condition number: 44.14341
##

table(lasso_grp)
fit <- grpreg(X_full, y, lasso_grp, penalty="grLasso")
plot(fit)
cvfit <- cv.grpreg(X_full, y, lasso_grp, penalty="grLasso")
plot(cvfit)

# Infer the model
#opt_lmda=cvfit$lambda.min+cvfit$cvse[cvfit$min]
opt_lmda=cvfit$lambda.min
cm_coef=coef(fit, lambda=opt_lmda)
cm_coef <- cm_coef[2:length(cm_coef)]
grp_lasso_df <- data.frame(predictors, cm_coef)
X_grp_lasso = X_full[grp_lasso_df[grp_lasso_df$cm_coef!=0,]$predictors]
print("Condition number of design matrix: ", kappa(data.matrix(X_grp_lasso)))
lm_grp_lasso = lm(y ~ ., data=cbind(y, X_grp_lasso))
summary(lm_grp_lasso)
unit_change_analysis(lm_grp_lasso, y)


### 
### 4. Analysis with Bi-Level group LASSO regularization
###
## Results:
# Multiple R-squared:  0.9345,	Adjusted R-squared:  0.9312 
# Condition number: 9
# 79 group -> 46
# 218 -> 69 predictors
# Significant factors: 43
##

table(lasso_grp)
fit <- grpreg(X_full, y, lasso_grp, penalty="cMCP", lambda = seq(0.1,0,length=100))
plot(fit)
cvfit <- cv.grpreg(X_full, y, lasso_grp, penalty="cMCP")
plot(cvfit)

# Infer the model
#opt_lmda=cvfit$lambda.min+cvfit$cvse[cvfit$min]
opt_lmda=cvfit$lambda.min
cm_coef=coef(fit, lambda=opt_lmda)
cm_coef <- cm_coef[2:length(cm_coef)]
grp_lasso_df <- data.frame(predictors, cm_coef)
X_grp_lasso = X_full[grp_lasso_df[grp_lasso_df$cm_coef!=0,]$predictors]
sprintf("Condition number of design matrix:  %s", kappa(data.matrix(X_grp_lasso)))
lm_grp_lasso = lm(y ~ ., data=cbind(y, X_grp_lasso))
sink("/Users/sanjeevsingh/Dropbox/PHD/Qualifiers/Data Analysis/bilevel.txt")
summary(lm_grp_lasso)
sink()
df=unit_change_analysis(lm_grp_lasso, y)
# write.csv(df,"/Users/sanjeevsingh/Dropbox/PHD/Qualifiers/Data Analysis/unit_price_cMCP.csv", row.names = FALSE)

# Reference
# 1. Group Lasso: https://pbreheny.github.io/grpreg/articles/getting-started.html
# 2. Lasso: https://mathstat.slu.edu/~speegle/Spring2020/4870/_book/variable-selection.html
# 3. glmnet: https://glmnet.stanford.edu/articles/glmnet.html