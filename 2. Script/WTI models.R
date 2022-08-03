rm(list = ls())

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
  rugarch
)

df <- read.xlsx("WTI.xlsx", 1)

df <- df %>%
  mutate(Month = yearmonth(Date)) %>%
  as_tsibble(index = Month)

df

df %>% autoplot(WTI) + labs(title = "WTI Price", y = "WTI Price")

df <- df %>% mutate(lWTI = log(WTI))
df <- df %>% mutate(rWTI = difference(lWTI))

df %>% autoplot(rWTI) + labs(title = "WTI Price Returns", y = "WTI Price Returns")

impute_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.025, 0.975), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}

df <- df %>% mutate(rWTI_A = impute_outliers(rWTI))

df %>% autoplot(rWTI_A) + labs(title = "WTI Price Returns Outliers Adjusted", y = "WTI Price Returns Outliers Adjusted")

df_train <- df %>% filter_index(. ~ "2014 Oct")
df_test <- df %>% filter_index("2014 Sep" ~ "2020 Dic")

df_train %>%
  gg_tsdisplay(rWTI_A, plot_type='partial')

rWTI_fit <- df_train %>%
  model(arima100 = ARIMA(rWTI_A ~ pdq(1,0,0)),
        arima001 = ARIMA(rWTI_A ~ pdq(0,0,1)),
        arima101 = ARIMA(rWTI_A ~ pdq(1,0,1)),
        stepwise = ARIMA(rWTI_A),
        search = ARIMA(rWTI_A, stepwise=FALSE))

glance(rWTI_fit) %>% arrange(AICc) %>% select(.model:BIC)

rWTI_fit %>%
  select(search) %>%
  gg_tsresiduals()

augment(rWTI_fit) %>%
  filter(.model=='search') %>%
  features(.innov, ljung_box, lag = 10, dof = 3)

rWTI_arima <- arima(df_train$rWTI_A,c(1,0,0))

rWTI_ts<-ts(df[1:384,]$rWTI_A, start = c(1989, 1), frequency = 12)
rWTI_ts<-window(rWTI_ts, start=c(2014, 11))

fun_m1<-function(x, h=h){
  forecast(Arima(x,order=c(1,0,0), include.drift = T), h=h)
}

err_pron1<-tsCV(rWTI_ts,fun_m1, h=1)

checkresiduals(err_pron1[!is.na(err_pron1)])

Box.test(err_pron1[!is.na(err_pron1)], type = "Ljung-Box")

pron_1<-rWTI_ts-err_pron1

plot(pron_1)

lm_m1<-lm(rWTI_ts~pron_1, na.action = na.omit)
summary(lm_m1)

df_train$residuals <- rWTI_arima$residuals
df_train$residuals2 <- df_train$residuals^2

df_train %>%
  gg_tsdisplay(residuals2, plot_type='partial')

rWTI_train=ts(df_train$rWTI_A,
         start = c(1989, 1), frequency = 12)

rWTI=ts(df[1:384,]$rWTI_A,
              start = c(1989, 1), frequency = 12)

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 0),
                                     include.mean = FALSE),
                   distribution.model = "norm")

rWTI_garch<-ugarchfit(spec=spec,
                   data=rWTI_train[2:310])

f1 = NULL

for (i in 0:74) {
  rWTI_garch<-ugarchfit(spec=spec,
                        data=rWTI[2:(310+i)])
  rWTI_garch_forecast <- ugarchforecast(rWTI_garch, n.ahead = 1)
  f1[i] <- rWTI_garch_forecast@forecast$seriesFor[1]
}

f1=ts(f1,
        start = c(2015, 9), frequency = 12)

plot(f1)

err_pron2 <- rWTI_ts - f1

checkresiduals(err_pron2[!is.na(err_pron2)])

Box.test(err_pron2[!is.na(err_pron2)], type = "Ljung-Box")

lm_m2<-lm(rWTI_ts~f1, na.action = na.omit)
summary(lm_m2)

spec2 <- ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 0),
                                     include.mean = FALSE),
                   distribution.model = "norm")

rWTI_tgarch<-ugarchfit(spec=spec2,
                      data=rWTI_train[2:310])

f2 = NULL

for (i in 0:74) {
  rWTI_tgarch<-ugarchfit(spec=spec2,
                        data=rWTI[2:(310+i)])
  rWTI_tgarch_forecast <- ugarchforecast(rWTI_tgarch, n.ahead = 1)
  f2[i] <- rWTI_tgarch_forecast@forecast$seriesFor[1]
}

f2=ts(f2,
      start = c(2015, 9), frequency = 12)

plot(f2)

err_pron3 <- rWTI_ts - f2

checkresiduals(err_pron3[!is.na(err_pron3)])

Box.test(err_pron3[!is.na(err_pron3)], type = "Ljung-Box")

lm_m3<-lm(rWTI_ts~f2, na.action = na.omit)
summary(lm_m3)

accuracyARIMA <- accuracy(pron_1, rWTI_ts)
rownames(accuracyARIMA) <- "ARIMA Model"

accuracyGARCH <- accuracy(f1, rWTI_ts)
rownames(accuracyGARCH) <- "GARCH Model"

accuracyTGARCH <- accuracy(f2, rWTI_ts)
rownames(accuracyTGARCH) <- "TGARCH Model"

accuracy <- rbind(accuracyARIMA, accuracyGARCH, accuracyTGARCH)

accuracy <- as.data.frame(accuracy)

accuracy$Model <- row.names(accuracy)
accuracy <- accuracy %>% select(Model, everything())

write_xlsx(accuracy, "WTI Models.xlsx")

WTI_proyecciones <- cbind(f1, f2)

saveRDS(WTI_proyecciones, file = "WTI proyeccion.rds")

saveRDS(df, file = "df.rds")

rWTI_garch<-ugarchfit(spec=spec,
                      data=rWTI[2:384])

rWTI_garch_forecast <- ugarchforecast(rWTI_garch, n.ahead = 17)

rWTI_garch_forecast <- rWTI_garch_forecast@forecast$seriesFor

rWTI_garch_forecast=ts(rWTI_garch_forecast,
      start = c(2021, 1), frequency = 12)

plot(rWTI_garch_forecast)