library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(ggplot2)
library(car)
library(forecast)
library(vars)

Monthly_Inflation <- read_csv("Monthly Inflation Rate.csv")
WTI_Oil_Prices <- read_csv("Cushing_OK_WTI_Spot_Price_FOB.csv")
FEDFUNDS <- read_csv("FEDFUNDS.csv")
lumber_prices_historical_chart_data <- read_csv("lumber-prices-historical-chart-data.csv")
Monthly_Unemployment <- read_csv("Monthly Unemployment.csv")
PRAWMINDEXM <- read_csv("PRAWMINDEXM.csv")
UMCSENT <- read_csv("UMCSENT.csv")

# Cleaning
unemployment_long <- gather(Monthly_Unemployment, Month, Unemployment, Jan:Dec)
unemployment_long$Month <- match(unemployment_long$Month, month.abb)

UMCSENT$DATE <- ymd(UMCSENT$DATE)
UMCSENT$Year <- year(UMCSENT$DATE)
UMCSENT$Month <- month(UMCSENT$DATE)
UMCSENT <- select(UMCSENT, select=-c(DATE))
UMCSENT$UMCSENT <- as.numeric(ifelse(UMCSENT$UMCSENT==".",NaN,UMCSENT$UMCSENT))
UMCSENT <- drop_na(UMCSENT)

PRAWMINDEXM$DATE <- ymd(PRAWMINDEXM$DATE)
PRAWMINDEXM$Year <- year(PRAWMINDEXM$DATE)
PRAWMINDEXM$Month <- month(PRAWMINDEXM$DATE)
PRAWMINDEXM <- select(PRAWMINDEXM, select=-c(DATE))
PRAWMINDEXM$PRAWMINDEXM <- ts(PRAWMINDEXM$PRAWMINDEXM, start=1990, frequency=12)
autoplot(PRAWMINDEXM$PRAWMINDEXM)

FEDFUNDS$DATE <- ymd(FEDFUNDS$DATE)
FEDFUNDS$Year <- year(FEDFUNDS$DATE)
FEDFUNDS$Month <- month(FEDFUNDS$DATE)
FEDFUNDS <- select(FEDFUNDS, select=-c(DATE))
FEDFUNDS$FEDFUNDS <- ts(FEDFUNDS$FEDFUNDS, start=1954, frequency=12)
autoplot(FEDFUNDS$FEDFUNDS)

oil_dates <- str_split(WTI_Oil_Prices$Month, pattern="-", simplify=TRUE)
WTI_Oil_Prices$Month <- oil_dates[,1]
WTI_Oil_Prices$Year <- oil_dates[,2]
WTI_Oil_Prices$Month <- match(WTI_Oil_Prices$Month, month.abb)
WTI_Oil_Prices$Year <- ifelse(WTI_Oil_Prices$Year<23, as.numeric(paste0("20",WTI_Oil_Prices$Year)), as.numeric(paste0("19",WTI_Oil_Prices$Year)))

inflation_long <- gather(Monthly_Inflation, Month, Rate, Jan:Dec)
inflation_long$Month <- match(inflation_long$Month, month.abb)

cpi_1950 <- merge(inflation_long, FEDFUNDS, by=c("Year", "Month"))
cpi_1950 <- merge(cpi_1950, unemployment_long, by=c("Year", "Month"))
cpi_1950 <- merge(cpi_1950, UMCSENT, by=c("Year", "Month"))
cpi_full <- merge(cpi_1950, WTI_Oil_Prices, by=c("Year", "Month"))
cpi_full <- merge(cpi_full, PRAWMINDEXM, by=c("Year", "Month"))
colnames(cpi_1950) <- c("Year", "Month", "Inf_Rate", "Fed_Rate", "Unemployment", "Consumer_Sentiment")
colnames(cpi_full) <- c("Year", "Month", "Inf_Rate", "Fed_Rate", "Unemployment", "Consumer_Sentiment", "Oil_Price_Index", "Raw_Material_Index")

ggplot(data=cpi_full, mapping=aes(x=Inf_Rate)) +
  geom_density()

ggplot(data=cpi_full, mapping=aes(x=1:length(Inf_Rate),y=Inf_Rate)) +
  geom_line()

ggplot(data=cpi_full, mapping=aes(x=Unemployment, y=Inf_Rate)) +
  geom_point(mapping=aes(col=`Year`>2010)) +
  geom_smooth(method="lm")

pairs(cpi_full[,3:8])

colnames(cpi_full)

# Look at log transforming the variables
full_model <- lm(data=cpi_full, Inf_Rate~Unemployment+Fed_Rate+Consumer_Sentiment+Oil_Price_Index+Raw_Material_Index)
summary(full_model)
plot(full_model)
model_1950 <- lm(data=cpi_1950, Inf_Rate~Unemployment+Fed_Rate+Consumer_Sentiment)
summary(model_1950)
plot(model_1950)

# Normality of Errors
shapiro.test(full_model$residuals)
ks.test(full_model$residuals, "pnorm")
hist(full_model$residuals)

shapiro.test(model_1950$residuals)
ks.test(model_1950$residuals, "pnorm")
hist(model_1950$residuals)

# Auto-correlation
ggplot(mapping = aes(x=full_model$fitted.values, y=full_model$residuals)) +
  geom_point() +
  geom_line()

durbinWatsonTest(full_model) # Highly autocorrelated
durbinWatsonTest(model_1950) # Highly autocorrelated again

fit1 <- auto.arima(cpi_full$Inf_Rate)
summary(fit1)
autoplot(forecast(fit1, h=30))

fit2 <- auto.arima(cpi_1950$Inf_Rate, xreg=as.matrix(cpi_1950[4:6]))
summary(fit2)

fit3 <- auto.arima(cpi_full$Inf_Rate, xreg=as.matrix(cpi_full[4:8]))
summary(fit3)


ggplot(data=cpi_1950, mapping=aes(x=1:length(lag(`Fed_Rate`, 48)))) +
  geom_line(mapping=aes(y=lag(`Fed_Rate`, 48)), col="darkblue", alpha=0.5) +
  geom_line(mapping=aes(y=lag(`Fed_Rate`, 1)), col="darkgreen", alpha=0.25) +
  geom_line(mapping=aes(y=`Inf_Rate`), col="red", alpha=0.5) +
  theme_bw()

plot(lag(cpi_1950$Fed_Rate, 48), cpi_1950$Inf_Rate, col=ifelse(cpi_1950$Year<2000, "blue", "red"))
cor(lag(cpi_1950$Fed_Rate, 48), cpi_1950$Inf_Rate, use = "complete.obs")
