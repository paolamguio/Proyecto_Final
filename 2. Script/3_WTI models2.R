
# Modelos multivariados - WTI
# Proyecto Final
# Grupo 16
# Andres Martinez, Paola Morales y Oscar Cortes 
--------------------------------------------------
  
## preparación del espacio
rm(list = ls())

setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Proyecto_Final/4. Stores")

## llamado librerías de la sesión
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

df$month <- as.factor(month(df$Date))

rWTI_A <- df$rWTI_A

rWTI_A<-ts(rWTI_A, start = c(1989, 1), frequency = 12)

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

df3 <- df[7:400,]

# se crea base de datos test
df_test <- df[310:383,]

df_forecast <- df[384:400,]

df <- df[7:383,]

lag_df2 <- NULL

for (i in 1:6) {
  lag_df2 <- c(lag_df2, function2(df2[,c(2:22, 24, 26:28)], i))
}

lag_df2 <- as.data.frame(lag_df2)

df2 <- bind_cols(df2, lag_df2)

df2 <- df2[,c(2:22, 24, 26:178)]

df4 <- df2[7:221,]

df_test2 <- df2[165:204,]

df_forecast2 <- df2[205:221,]

df2 <- df2[7:204,]

###*** 2. Creación de modelos ***###

## Serie WTI mensual de enero de 1989 a mayo de 2022
rWTI_ts<-ts(df$rWTI_A, start = c(1989, 8), frequency = 12)
rWTI_ts<-window(rWTI_ts, start=c(2014, 11))

## Serie WTI mensual + variables gas natural + variables google trend
rWTI_ts2<-ts(df2$rWTI_A, start = c(2004, 7), frequency = 12)
rWTI_ts2<-window(rWTI_ts2, start=c(2017, 9))

set.seed(1)

control <- trainControl(method = "timeslice",
                              initialWindow = 303,
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
                        initialWindow = 158,
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

# 2.2. Booststrap
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

predxgboost2<-predict(xgboost2,df_test2)

pca <- prcomp(df3[,2:28], scale = TRUE)

pca <- pca$x

pca <- as.data.frame(pca)

pca_forecast <- pca[378:394,1:4]

pca <- pca[1:377,1:4]

pca <- bind_cols(rWTI_A = df$rWTI_A, pca)

pca_test <- pca[304:377,]

pca2 <- prcomp(df4[,c(1:21, 23:175)], scale = TRUE)

pca2 <- pca2$x

pca2 <- as.data.frame(pca2)

pca_forecast2 <- pca2[199:215,1:4]

pca2 <- pca2[1:198,1:4]

pca2 <- bind_cols(rWTI_A = df2$rWTI_A, pca2)

pca_test2 <- pca[159:198,]

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
rWTI_train<-window(rWTI_ts, end=c(2014, 10))
rWTI<-window(rWTI_ts, end=c(2020, 12))
rWTI_ts<-window(rWTI_ts, start=c(2014, 11))

rWTI_ts2<-ts(df2$rWTI_A, start = c(2004, 7), frequency = 12)
rWTI_train2<-window(rWTI_ts2, end=c(2017, 8))
rWTI_ts2<-window(rWTI_ts2, start=c(2017, 9))

df_ts<-ts(df, start = c(1989, 8), frequency = 12)
x_train<-window(df_ts[,2:28], end=c(2014, 10))
x_test<-window(df_ts[,2:28], start=c(2014, 11))

df_ts2<-ts(df2, start = c(2004, 7), frequency = 12)
x_train2<-window(df_ts2[,c(1:21, 23:175)], end=c(2017, 8))
x_test2<-window(df_ts2[,c(1:21, 23:175)], start=c(2017, 9))

fourier_xreg<-fourier(rWTI_train, K = 5)

set.seed(1)

nnet<-nnetar(rWTI_train, repeats = 150, p = 1, 
                     P = 1, size = 3, xreg = fourier_xreg)

prednnet<-forecast(nnet, h=74, xreg = fourier(rWTI_train, K = 5, h = 81))

prednnet <- prednnet$mean

set.seed(1)

nnet2<-nnetar(rWTI_train, repeats = 150, p = 1, 
             P = 1, size = 3, xreg = x_train)

prednnet2<-forecast(nnet2, h=74, xreg = x_test)

prednnet2 <- prednnet2$mean

set.seed(1)

nnet3<-nnetar(rWTI_train2, repeats = 150, p = 1, 
              P = 1, size = 3, xreg = x_train2)

prednnet3<-forecast(nnet3, h=40, xreg = x_test2)

prednnet3 <- prednnet3$mean

predElasticnet1<-ts(predElasticnet1, start = c(2014, 11), frequency = 12)
predElasticnet2<-ts(predElasticnet2, start = c(2017, 9), frequency = 12)
predpca<-ts(predpca, start = c(2014, 11), frequency = 12)
predpca2<-ts(predpca2, start = c(2017, 9), frequency = 12)
predxgboost1<-ts(predxgboost1, start = c(2014, 11), frequency = 12)
predxgboost2<-ts(predxgboost2, start = c(2017, 9), frequency = 12)

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

plot(predpca2)
plot(predElasticnet2)
plot(rWTI_ts2)

WTI_proyecciones <- cbind(predpca, predElasticnet1)

WTI_proyecciones2 <- cbind(predpca2, predElasticnet2)

plot(WTI_proyecciones)

rWTI_ts<-ts(df$rWTI_A, start = c(1989, 8), frequency = 12)
rWTI_ts<-window(rWTI_ts, start=c(2012, 1))

WTI_proyecciones <- ts.union(rWTI_A = rWTI_ts, WTI_proyecciones, WTI_proyecciones2)

WTI_proyecciones %>% autoplot()

# generación base de datos
WTI_proyecciones3 <- import("WTI proyeccion.rds")

WTI_proyecciones <- ts.union(WTI_proyecciones, WTI_proyecciones3)

saveRDS(WTI_proyecciones, file = "WTI proyecciones.rds")

WTI_proyecciones %>% autoplot()

rWTI_Elasticnet_forecast<-predict(elasticnet2,df_forecast2)

rWTI_Elasticnet_forecast=ts(rWTI_Elasticnet_forecast,
                       start = c(2021, 1), frequency = 12)

plot(rWTI_Elasticnet_forecast)

rWTI_PCA_forecast <- predict(pcamodel2 , pca_forecast2)

rWTI_PCA_forecast=ts(rWTI_PCA_forecast,
                            start = c(2021, 1), frequency = 12)

plot(rWTI_PCA_forecast)

fourier_xreg<-fourier(rWTI, K = 5)

set.seed(1)

nnet<-nnetar(rWTI, repeats = 150, p = 1, 
             P = 1, size = 3, xreg = fourier_xreg)

rWTI_NNET_forecast<-forecast(nnet, h=17, xreg = fourier(rWTI, K = 5, h = 17))

rWTI_NNET_forecast <- rWTI_NNET_forecast$mean

plot(rWTI_NNET_forecast)

rWTI_A<-window(rWTI_A, start=c(2018, 1))

WTI_forecast <- ts.union(rWTI_A = rWTI_A, rWTI_Elasticnet_forecast, rWTI_PCA_forecast, rWTI_NNET_forecast)

WTI_forecast[36,2] <- WTI_forecast[36,1]
WTI_forecast[36,3] <- WTI_forecast[36,1]
WTI_forecast[36,4] <- WTI_forecast[36,1]

WTI_forecast %>% autoplot()