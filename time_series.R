#1953 – 1974 yılları ABD imalatında sermaye harcamaları verisinde uygun modeli kurarak 1974 yılı tüm değerler için öngörüde bulunma
install.packages("openxlsx")
library(openxlsx)
veri <- read.xlsx("odev3.xlsx")
is.data.frame(veri)
install.packages("forecast")
library(forecast)
install.packages("fpp")
library(fpp)
par(mar=c(1,1,1,1))

veri_ts <- ts(veri, start = c(1953),end = c(1973, 4),frequency = 4)
veri_capital_ts <-ts(veri$Capital, start = c(1953),end = c(1973, 4),frequency = 4)
fit1 <- holt(veri_capital_ts, 4) #holt üstel düzgünlestirmesi
accuracy(fit1) #h0lt için mape değerleri
fit2 <- hw(veri_capital_ts, 4) #holt winters üstel düzgünlestirmesi
accuracy(fit2) #holt winters için mape degeri
fit2 # holt winters


autoplot(veri_ts)
tsdisplay(veri_ts)
seasonplot(veri_ts, s=4, col = rainbow(21), year.labels =TRUE, main = "seasonal plot" )
decomposed_veri <- decompose(x=veri_ts, type = "multiplicative")
str(decomposed_veri)
plot(decomposed_veri)
lambda=BoxCox.lambda(veri_capital_ts); lambda
veri_ts_boxcox <- BoxCox(veri_capital_ts, lambda)
veri_ts_boxcox
adf_test <- adf.test(veri_ts_boxcox); adf_test
diff_1 <- diff(veri_ts_boxcox);diff_1
adf_1_test <-  adf.test(diff_1); adf_1_test
bestmodel <- auto.arima(veri_capital_ts, trace = TRUE, ic ="aicc", approximation = FALSE)
fitt <- Arima(veri_capital_ts,c(1,1,0), c(0,0,1))


forecast(fitt, h=4)
plot(forecast(fitt))
shapiro.test(res)
Box.test(bestmodel$residuals, lag=10, type = "Ljung-Box")
tsdisplay(residuals(bestmodel))
Acf(res, plot = TRUE)




