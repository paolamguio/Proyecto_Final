
# Modelos Multivariables - WTI
# Proyecto Final
# Grupo 16
# Andres Martinez, Paola Morales y Oscar Cortes 
#--------------------------------------------------
  
## preparación del espacio
rm(list = ls())

setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Proyecto_Final/4. Stores")
setwd("C:/Users/ocaco/OneDrive/15. Maestria Economia/9. Big Data/GitHub/Proyecto_Final/4. Stores")



require(pacman)
p_load(
  tidyverse,
  rvest,
  writexl,
  rio,
  skimr,
  pastecs,
  PerformanceAnalytics,
  naniar,
  gtsummary,
  caret,
  modelsummary,
  gamlr,
  ROCR,
  pROC,
  smotefamily,
  rpart,
  randomForest,
  fastAdaboost,
  glmnet,
  rpart.plot,
  xlsx,
  tsibble,
  feasts,
  fable,
  urca,
  forecast,
  fGarch,
  rugarch,
  lubridate,
  xgboost
)

###*** 1. Base de datos ***###
## Llamado base de datos
df <- import("df.rds")

## ajustes a base de datos 
df$month <- as.factor(month(df$Date))

# se crea variable de logaritmo del WTI
function1 <- function(x){
  log <- log(x)
  colnames(log) <- paste("log", colnames(x))
  return(log)
}

log_df <- function1(df[,3:5])

df <- bind_cols(df, log_df)

df2 <- df[,c(1, 6:27, 30:34)]

df <- df[,c(1, 27, 30:34)]

df2 <- df2[181:401,]

df <- df[2:401,]

function2 <- function(x, k){
  lag <- lag(x, k)
  colnames(lag) <- paste("lag", k, colnames(x))
  return(lag)
}

lag_df <- NULL

for (i in 1:6) {
  lag_df <- c(lag_df, function2(df[,c(3, 5:7)], i))
}

lag_df <- as.data.frame(lag_df)

df <- bind_cols(df, lag_df)

df <- df[,c(3, 5:31)]

# se crea base de datos test
df_test <- df[320:400,]

df <- df[7:400,]

lag_df2 <- NULL

for (i in 1:6) {
  lag_df2 <- c(lag_df2, function2(df2[,c(2:22, 24, 26:28)], i))
}

lag_df2 <- as.data.frame(lag_df2)

df2 <- bind_cols(df2, lag_df2)

df2 <- df2[,c(2:22, 24, 26:178)]

df_test2 <- df2[176:221,]

df2 <- df2[7:221,]

###*** 2. Creación de modelos ***###

## Serie WTI mensual de enero de 1989 a mayo de 2022
rWTI_ts<-ts(df$rWTI_A, start = c(1989, 8), frequency = 12) 
rWTI_ts<-window(rWTI_ts, start=c(2015, 9))

## Serie WTI mensual + variables macro de USA + variables google trend
rWTI_ts2<-ts(df2$rWTI_A, start = c(2004, 7), frequency = 12)
rWTI_ts2<-window(rWTI_ts2, start=c(2018, 8))

set.seed(1)

control <- trainControl(method = "timeslice",
                              initialWindow = 313,
                              horizon = 1,
                              fixedWindow = FALSE)
# 2.1 Elastic Net
elasticnet1 <- train(
  rWTI_A~.,
  data = df,
  method = "glmnet",
  trControl = control,
  metric = "RMSE",
  preProcess = c("center", "scale")
)

predElasticnet1<-predict(elasticnet1,df_test)

set.seed(1)

control2 <- trainControl(method = "timeslice",
                        initialWindow = 169,
                        horizon = 1,
                        fixedWindow = FALSE)

elasticnet2 <- train(
  rWTI_A~.,
  data = df2,
  method = "glmnet",
  trControl = control2,
  metric = "RMSE",
  preProcess = c("center", "scale")
)

predElasticnet2<-predict(elasticnet2,df_test2)

# 2.2. Boost
# se define grilla
grid_default <- expand.grid(nrounds = c(100,250),
                            max_depth = c(4,6,8),
                            eta = c(0.01,0.3,0.5),
                            gamma = c(0,1),
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))

start_xg <- Sys.time()

xgboost1 <- train(
  rWTI_A~.,
  data = df,
  method = "xgbTree",
  trControl = control,
  na.action  = na.pass,
  tuneGrid = grid_default,
  metric = "RMSE",
  preProcess = c("center", "scale")
)

predxgboost1<-predict(xgboost1,df_test)

xgboost2 <- train(
  rWTI_A~.,
  data = df2,
  method = "xgbTree",
  trControl = control2,
  na.action  = na.pass,
  tuneGrid = grid_default,
  metric = "RMSE",
  preProcess = c("center", "scale")
)

predxgboost2<-predict(xgboost2,df_testing2)

pca <- prcomp(df[,2:28], scale = TRUE)

pca <- pca$x

pca <- as.data.frame(pca)

pca <- pca[,1:4]

pca <- bind_cols(rWTI_A = df$rWTI_A, pca)

pca_test <- pca[314:394,]

pca2 <- prcomp(df2[,c(1:21, 23:175)], scale = TRUE)

pca2 <- pca2$x

pca2 <- as.data.frame(pca2)

pca2 <- pca2[,1:4]

pca2 <- bind_cols(rWTI_A = df2$rWTI_A, pca2)

pca_test2 <- pca[170:215,]

pcamodel <- train(
  rWTI_A~.,
  data = pca,
  method = "lm",
  trControl = control,
  preProcess = c("center", "scale")
)

predpca <- predict(pcamodel , pca_test)

pcamodel2 <- train(
  rWTI_A~.,
  data = pca2,
  method = "lm",
  trControl = control2,
  preProcess = c("center", "scale")
)

predpca2 <- predict(pcamodel2 , pca_test2)

rWTI_ts<-ts(df$rWTI_A, start = c(1989, 8), frequency = 12)
rWTI_train<-window(rWTI_ts, end=c(2015, 8))
rWTI_ts<-window(rWTI_ts, start=c(2015, 9))

rWTI_ts2<-ts(df2$rWTI_A, start = c(2004, 7), frequency = 12)
rWTI_train2<-window(rWTI_ts2, end=c(2018, 7))
rWTI_ts2<-window(rWTI_ts2, start=c(2018, 8))

df_ts<-ts(df, start = c(1989, 8), frequency = 12)
x_train<-window(df_ts[,2:28], end=c(2015, 8))
x_test<-window(df_ts[,2:28], start=c(2015, 9))

df_ts2<-ts(df2, start = c(2004, 7), frequency = 12)
x_train2<-window(df_ts2[,c(1:21, 23:175)], end=c(2018, 7))
x_test2<-window(df_ts2[,c(1:21, 23:175)], start=c(2018, 8))

fourier_xreg<-fourier(rWTI_train, K = 5)

set.seed(1)

nnet<-nnetar(rWTI_train, repeats = 150, p = 1, 
                     P = 1, size = 3, xreg = fourier_xreg)

prednnet<-forecast(nnet, h=81, xreg = fourier(rWTI_train, K = 5, h = 81))

prednnet <- prednnet$mean

set.seed(1)

nnet2<-nnetar(rWTI_train, repeats = 150, p = 1, 
             P = 1, size = 3, xreg = x_train)

prednnet2<-forecast(nnet2, h=81, xreg = x_test)

prednnet2 <- prednnet2$mean

set.seed(1)

nnet3<-nnetar(rWTI_train2, repeats = 150, p = 1, 
              P = 1, size = 3, xreg = x_train2)

prednnet3<-forecast(nnet3, h=46, xreg = x_test2)

prednnet3 <- prednnet3$mean

predElasticnet1<-ts(predElasticnet1, start = c(2015, 9), frequency = 12)
predElasticnet2<-ts(predElasticnet2, start = c(2018, 8), frequency = 12)
predpca<-ts(predpca, start = c(2015, 9), frequency = 12)
predpca2<-ts(predpca2, start = c(2018, 8), frequency = 12)
predxgboost1<-ts(predxgboost1, start = c(2015, 9), frequency = 12)
predxgboost2<-ts(predxgboost2, start = c(2018, 8), frequency = 12)

###*** 3. Accuracy modelos ***###
accuracyElasticnet <- accuracy(predElasticnet1, rWTI_ts)
rownames(accuracyElasticnet) <- "Elastic Net Model 1"

accuracyElasticnet2 <- accuracy(predElasticnet2, rWTI_ts2)
rownames(accuracyElasticnet2) <- "Elastic Net Model 2"

accuracyXGBoost1 <- accuracy(predxgboost1, rWTI_ts)
rownames(accuracyXGBoost1) <- "XGBoost Model 1"

accuracyXGBoost2 <- accuracy(predxgboost2, rWTI_ts)
rownames(accuracyXGBoost2) <- "XGBoost Model 2"

accuracyPCA <- accuracy(predpca, rWTI_ts)
rownames(accuracyPCA) <- "Principal Component Model 1"

accuracyPCA2 <- accuracy(predpca2, rWTI_ts2)
rownames(accuracyPCA2) <- "Principal Component Model 2"

accuracyNNET1 <- accuracy(prednnet, rWTI_ts)
rownames(accuracyNNET1) <- "NNETAR Model"

accuracyNNET2 <- accuracy(prednnet2, rWTI_ts)
rownames(accuracyNNET2) <- "NNETAR Model 1"

accuracyNNET3 <- accuracy(prednnet3, rWTI_ts2)
rownames(accuracyNNET3) <- "NNETAR Model 2"

accuracy <- rbind(accuracyElasticnet, accuracyElasticnet2, accuracyXGBoost1, accuracyXGBoost2, accuracyPCA, accuracyPCA2, accuracyNNET1, accuracyNNET2, accuracyNNET3)

accuracy <- as.data.frame(accuracy)

accuracy$Model <- row.names(accuracy)
accuracy <- accuracy %>% select(Model, everything())

# generación base de datos
write_xlsx(accuracy, "WTI Models2.xlsx")