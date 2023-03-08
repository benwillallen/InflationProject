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
library(gganimate)
library(psych)
library(corrplot)
library(ragg)

# Loading Data
Monthly_Inflation <- read_csv("Monthly Inflation Rate All.csv")
WTI_Oil_Prices <- read_csv("WTISPLC.csv")
FEDFUNDS <- read_csv("FEDFUNDS.csv")
Monthly_Unemployment <- read_csv("Monthly Unemployment.csv")
AllCommodities_Index <- read_csv("AllCommodities_Change.csv")
UMCSENT <- read_csv("UMCSENT.csv")
M2SL <- read_csv("CURRSL.csv")

M2SL$DATE <- mdy(M2SL$DATE)
M2SL$Year <- year(M2SL$DATE)
M2SL$Month <- month(M2SL$DATE)
M2SL <- subset(M2SL, select=-c(DATE, CURRSL))
M2SL <- drop_na(M2SL)
M2SL$M1_Change <- ts(M2SL$M1_Change * 100, start=1948, frequency = 12)
autoplot(M2SL$M1_Change) + theme_bw() + labs(title="U.S. M1 Money Supply",
                                        subtitle=("Data from Jan. 1948 to Sep. 2022"),
                                        y="Annualized Percent Change (%)",
                                        caption="Source: St. Louis Federal Reserve Economic Data")
gglagplot(M2SL$M1_Change) + theme_bw() + labs(title="Money Supply")
plot(decompose(M2SL$M1_Change))
ggseasonplot(M2SL$M1_Change, polar=T, col=hcl.colors(75, alpha=0.8)) +
  theme_bw() + labs(title="Money Supply Seasonal Plot") + theme(text=element_text(size=20))

unemployment_long <- Monthly_Unemployment %>%
  gather(Month, Unemployment, Jan:Dec)
unemployment_long$Month <- match(unemployment_long$Month, month.abb)
unemployment_long <- unemployment_long %>%
  group_by(Year) %>%
  arrange(Year, Month)
unemployment_long$Unemployment <- ts(unemployment_long$Unemployment, start=1948, frequency=12)
autoplot(unemployment_long$Unemployment)+ theme_bw() + labs(title="United States Unemployment Rate",
                                                            subtitle=("Data from Jan. 1948 to Sep. 2022"),
                                                            y="% of Labor Force Unemployed",
                                                            caption="Source: Bureau of Labor Statistics")
gglagplot(unemployment_long$Unemployment) + theme_bw() + labs(title="Unemployment Rate")
plot(decompose(unemployment_long$Unemployment))
ggseasonplot(unemployment_long$Unemployment, polar=T, col=hcl.colors(80, alpha=0.8)) +
  theme_bw() + labs(title="Unemployment Seasonal Plot") + theme(text=element_text(size=20))

UMCSENT$DATE <- ymd(UMCSENT$DATE)
UMCSENT$Year <- year(UMCSENT$DATE)
UMCSENT$Month <- month(UMCSENT$DATE)
UMCSENT <- subset(UMCSENT, select=-c(DATE))
UMCSENT$UMCSENT <- as.numeric(ifelse(UMCSENT$UMCSENT==".",NaN,UMCSENT$UMCSENT))
UMCSENT <- UMCSENT %>%
  mutate(UMCSENT = na.approx(UMCSENT))
UMCSENT$UMCSENT <- ts(UMCSENT$UMCSENT, start=1958, frequency = 12)
autoplot(UMCSENT$UMCSENT) + theme_bw() + labs(title="United States Consumer Sentiment",
                                             subtitle=("Data from Nov. 1952 to Aug. 2022"),
                                             y="Consumer Sentiment Index", caption="Source: University of Michigan")
gglagplot(UMCSENT$UMCSENT) + theme_bw() + labs(title="Consumer Sentiment Index")
plot(decompose(UMCSENT$UMCSENT))
ggseasonplot(UMCSENT$UMCSENT, polar=T, col=hcl.colors(80, alpha=0.8)) +
  theme_bw() + labs(title="Consumer Sentiment Seasonal Plot") + theme(text=element_text(size=20))

commodities_long <- AllCommodities_Index %>%
  gather(Month, Commodities_Price, Jan:Dec)
commodities_long$Month <- match(commodities_long$Month, month.abb)
commodities_long <- commodities_long %>%
  group_by(Year) %>%
  arrange(Year, Month)
commodities_long$Commodities_Price <- ts(commodities_long$Commodities_Price * 100, start=1947, frequency=12)
autoplot(commodities_long$Commodities_Price) + theme_bw() + labs(title="Price Index for All Commodities", subtitle=("Data from Jan. 1947 to Sep. 2022"), y="Annualized Percent Change (%)", caption="Source: Bureau of Labor Statistics")
gglagplot(commodities_long$Commodities_Price) + theme_bw() + labs(title="Commodities Price Index")
plot(decompose(commodities_long$Commodities_Price))
ggseasonplot(commodities_long$Commodities_Price, polar=T, col=hcl.colors(80, alpha=0.8)) +
  theme_bw() + labs(title="Commodities Price Seasonal Plot") + theme(text=element_text(size=20))

FEDFUNDS$DATE <- ymd(FEDFUNDS$DATE)
FEDFUNDS$Year <- year(FEDFUNDS$DATE)
FEDFUNDS$Month <- month(FEDFUNDS$DATE)
FEDFUNDS <- subset(FEDFUNDS[FEDFUNDS$Year>1954,], select=-c(DATE))
FEDFUNDS$FEDFUNDS <- ts(FEDFUNDS$FEDFUNDS, start=1955, frequency=12)
autoplot(FEDFUNDS$FEDFUNDS) + theme_bw() + labs(title="Federal Funds Rate",
                                                subtitle=("Data from July. 1954 to Sep. 2022"),
                                                y="Federal Funds Rate",
                                                caption="Source: St. Louis Federal Reserve Economic Data")
gglagplot(FEDFUNDS$FEDFUNDS) + theme_bw() + labs(title="Federal Funds Rate")
plot(decompose(FEDFUNDS$FEDFUNDS))
ggseasonplot(FEDFUNDS$FEDFUNDS, polar=T, col=hcl.colors(70, alpha=0.8)) +
  theme_bw() + labs(title="Federal Funds Rate Seasonal Plot") + theme(text=element_text(size=20))

WTI_Oil_Prices$DATE <- mdy(WTI_Oil_Prices$DATE)
WTI_Oil_Prices$Year <- year(WTI_Oil_Prices$DATE)
WTI_Oil_Prices$Month <- month(WTI_Oil_Prices$DATE)
WTI_Oil_Prices <- subset(WTI_Oil_Prices, select=-c(DATE, WTISPLC))
WTI_Oil_Prices <- WTI_Oil_Prices %>%
  group_by(Year) %>%
  arrange(Year, Month)
WTI_Oil_Prices$Oil_Change <- ts(WTI_Oil_Prices$Oil_Change * 100, start=1946, frequency=12)
autoplot(WTI_Oil_Prices$Oil_Change) + theme_bw() + labs(title="Crude Oil Price Index",
                                                     subtitle=("Data from Jan. 1946 to Oct. 2022"),
                                                     y="Annualized Percent Change (%)",
                                                     caption="Source: St. Louis Federal Reserve Economic Data")
gglagplot(WTI_Oil_Prices$Oil_Change) + theme_bw() + labs(title="Oil Price Index")
plot(decompose(WTI_Oil_Prices$Oil_Change))
ggseasonplot(WTI_Oil_Prices$Oil_Change, polar=T, col=hcl.colors(80, alpha=0.8)) +
  theme_bw() + labs(title="Crude Oil Price Seasonal Plot") + theme(text=element_text(size=20))

inflation_long <- gather(Monthly_Inflation, Month, Rate, Jan:Dec)
inflation_long$Month <- match(inflation_long$Month, month.abb)
inflation_long <- inflation_long %>%
  group_by(Year) %>%
  arrange(Year, Month)
inflation_long$Rate <- ts(inflation_long$Rate, start=1950, frequency=12)
autoplot(inflation_long$Rate) + theme_bw() + labs(title="U.S. Inflation Rate",
                                                  subtitle=("Data from Jan. 1950 to Oct. 2022"),
                                                  y="Inflation Rate",
                                                  caption="Source: St. Louis Federal Reserve Economic Data")
ggseasonplot(inflation_long$Rate, polar=T, col=hcl.colors(80, alpha=0.8)) +
  theme_bw() + labs(title="Inflation Rate Seasonal Plot") + theme(text=element_text(size=20))
gglagplot(inflation_long$Rate) + theme_bw() + labs(title="Inflation Rate")
ggAcf(inflation_long$Rate)

cpi_full <- merge(inflation_long, FEDFUNDS, by=c("Year", "Month")) %>%
  merge(M2SL, by=c("Year", "Month")) %>%
  merge(unemployment_long, by=c("Year", "Month")) %>%
  merge(UMCSENT, by=c("Year", "Month")) %>%
  merge(commodities_long, by=c("Year", "Month")) %>% 
  merge(WTI_Oil_Prices, by=c("Year", "Month")) %>%
  group_by(Year) %>%
  arrange(Year, Month)

colnames(cpi_full) <- c("Year", "Month", "Inf_Rate", "Fed_Rate", "MS_Pct_Change", "Unemployment", "Consumer_Sentiment", "Oil_Price_Index", "Commodities_Index")

cpi_full$Date <- mdy(paste(cpi_full$Month, "1", cpi_full$Year, sep="/"))

# Training and Testing Sets
cpi_full_train <- cpi_full[1:(length(cpi_full$Year)-60),]
cpi_full_test <- cpi_full[(length(cpi_full$Year)-60+1):length(cpi_full$Year),]

ggplot(data=cpi_full, mapping=aes(x=Date, Inf_Rate)) + 
  geom_vline(xintercept=as.Date("1970-2-1"), linetype="dashed", alpha=0.4) + 
  geom_vline(xintercept=as.Date("1978-3-8"), linetype="dashed", alpha=0.4) + 
  geom_vline(xintercept=as.Date("1979-8-6"), linetype="dashed", alpha=0.4) + 
  geom_vline(xintercept=as.Date("1987-8-11"), linetype="dashed", alpha=0.4) + 
  geom_vline(xintercept=as.Date("2006-2-1"), linetype="dashed", alpha=0.4) + 
  geom_vline(xintercept=as.Date("2014-2-3"), linetype="dashed", alpha=0.4) + 
  geom_vline(xintercept=as.Date("2018-2-5"), linetype="dashed", alpha=0.4) + 
  geom_line(mapping=aes(col=`Fed_Rate`), linewidth=1) +
  theme_bw() + labs(title="U.S. Inflation Rate",
                    subtitle=("Dashed lines indicate shift in Federal Reserve Chairman"),
                    y="Inflation Rate (%)",
                    caption="Sources: St. Louis Federal Reserve Economic Data, Bureau of Labor Statistics") +
  scale_colour_viridis_c(name="Fed. Rate (%)")
ggsave("InflationPrettyPlot.png", width=1000, height=750, units="px", dpi="print", scale=2)

# Fed Rate
ggplot(data=cpi_full, mapping=aes(x=Date, Inf_Rate)) + 
  geom_line(mapping=aes(col=`Fed_Rate`), linewidth=1) +
  theme_bw() + labs(title="U.S. Inflation Rate with Federal Funds Rate",
                    subtitle=("Data from Jan. 1955 to Aug. 2022"),
                    y="Inflation Rate (%)",
                    caption="Sources: St. Louis Federal Reserve Economic Data, Bureau of Labor Statistics") +
  scale_colour_viridis_c(name="Fed. Rate (%)")
ggsave("InflationFedPlot.png", width=1000, height=400, units="px", dpi="print", scale=2)

# Consumer Sentiment
ggplot(data=cpi_full, mapping=aes(x=Date, Inf_Rate)) + 
  geom_line(mapping=aes(col=`Consumer_Sentiment`), linewidth=1.2) +
  geom_line(linewidth=0.2, col="gray") +
  theme_bw() + labs(title="U.S. Inflation Rate with Consumer Sentiment",
                    y="Inflation Rate (%)",
                    caption="Sources: St. Louis Federal Reserve Economic Data, Bureau of Labor Statistics") +
  scale_color_distiller(name="Index Value", palette="RdYlGn", direction=1)
ggsave("InflationSentPlot.png", width=1000, height=400, units="px", dpi="print", scale=2)

# Commodity Prices
ggplot(data=cpi_full, mapping=aes(x=Date, Inf_Rate)) + 
  geom_line(mapping=aes(col=`Oil_Price_Index`), linewidth=1.2) +
  geom_line(linewidth=0.2, col="gray") +
  theme_bw() + labs(title="U.S. Inflation Rate with Commodities Price",
                    y="Inflation Rate (%)",
                    caption="Sources: St. Louis Federal Reserve Economic Data, Bureau of Labor Statistics") +
  scale_color_distiller(name="Change (%)", palette="RdYlBu")
ggsave("InflationCommPlot.png", width=1000, height=400, units="px", dpi="print", scale=2)

# Plotting Data
ggplot(data=cpi_full, mapping=aes(x=1:length(lag(`Fed_Rate`, 60)))) +
  geom_line(mapping=aes(y=lag(`Fed_Rate`, 60)), col="darkblue", alpha=0.5) +
  geom_line(mapping=aes(y=lag(`Fed_Rate`, 1)), col="darkgreen", alpha=0.25) +
  geom_line(mapping=aes(y=`Inf_Rate`), col="red", alpha=0.5) +
  theme_bw()

corPlot(cpi_full[,3:9], cex=0.8, scale=FALSE)
corPlot(cpi_full_train[,3:9], cex=0.8, scale=FALSE) # Training and test are different
corPlot(cpi_full_test[,3:9], cex=0.8, scale=FALSE)
corr_data <- cor(cpi_full[,3:9])
colnames(corr_data) <- c("I", "F", "M", "U", "C", "CM", "O")
row.names(corr_data) <- c("I", "F", "M", "U", "C", "CM", "O")
corrplot(corr_data, method="color", addCoef.col = 'white', tl.col="Black",
         tl.cex=1, mar=c(1, 1, 1, 1))

ggplot(data=cpi_full, mapping=aes(x=Unemployment, y=Inf_Rate)) +
  geom_point(mapping=aes(col=`Year`>2000)) +
  geom_smooth(method="lm", mapping=aes(col=`Year`>2000))

pairs(cpi_full[,3:9], cex.labels=2)

# Creating Simulated Future Data

nnar_sim_forecast <- function(vari) {
  model <- nnetar(vari)
  return(forecast(model, 60))
}

fed_rate_sim <- nnar_sim_forecast()

# Full model
full_model <- lm(data=cpi_full_train, Inf_Rate~Unemployment+Fed_Rate+Consumer_Sentiment+Oil_Price_Index+Commodities_Index+MS_Pct_Change)
summary(full_model)
plot(full_model)

lin_forecast1 <- predict(full_model, h=60, newdata=cpi_full_test)
full_model_MAE <- mean(abs(lin_forecast1-cpi_full_test$Inf_Rate))
full_model_MedAE <- median(abs(lin_forecast1-cpi_full_test$Inf_Rate))
full_model_Max_Error <- max(abs(lin_forecast1-cpi_full_test$Inf_Rate))
full_model_AIC <- AIC(full_model)
full_model_BIC <- BIC(full_model)

ggplot() +
  geom_vline(xintercept=as.Date("2017-9-1"), alpha=0.4, linetype="dashed") +
  geom_line(data=cpi_full, mapping=aes(x=Date, y=Inf_Rate), col="darkred") +
  geom_line(mapping=aes(x=cpi_full$Date[(length(cpi_full$Date)-60+1):length(cpi_full$Date)],
                        y=lin_forecast1), col="blue") +
  geom_line(mapping=aes(x=cpi_full$Date[(length(cpi_full$Date)-60+1):length(cpi_full$Date)],
                        y=lin_forecast1), col="blue", linewidth=2, alpha=0.15) +
  labs(title="Linear Regression Forecast",
       subtitle="5-Year Forecast: Sep. 2017 to Aug. 2022", y="Inflation Rate (%)") + 
  scale_x_date(date_labels="%b %Y", breaks="10 years") + theme_bw()
ggsave("LinearForecast.png", width=1000, height=750, dpi="print", units="px", scale=1.5)

# Normality of Errors
shapiro.test(full_model$residuals)
ks.test(full_model$residuals, "pnorm")
ggplot() +
  geom_density(mapping=aes(x=full_model$residuals), col="#0D2149", fill="#de6449", linewidth=0.5, alpha=0.3) +
  geom_histogram(mapping=aes(x=full_model$residuals, y=after_stat(density)), col="#de6449", fill="#de6449", binwidth=0.7, alpha=0.8) +
  labs(title="Residual Distribution", subtitle="Data from 1955 to 2022", 
       x="Residual", y="Frequency") + theme_bw() + theme(text = element_text(size = 25)) 

# Auto-correlation
ggplot(mapping = aes(x=full_model$fitted.values, y=full_model$residuals)) +
  geom_point() +
  geom_line()

durbinWatsonTest(full_model) # Highly autocorrelated

# Step Model
null_lin_fit <- lm(data=cpi_full_train, Inf_Rate~1)
step_model <- step(null_lin_fit, scope=full_model$terms, direction="forward", k=2)
summary(step_model)
plot(step_model)
lin_forecast3 <- predict(step_model, h=60, newdata=cpi_full_test)
step_model_MAE <- mean(abs(lin_forecast3-cpi_full_test$Inf_Rate))
step_model_MedAE <- median(abs(lin_forecast3-cpi_full_test$Inf_Rate))
step_model_Max_Error <- max(abs(lin_forecast3-cpi_full_test$Inf_Rate))
step_model_AIC <- AIC(step_model)
step_model_BIC <- BIC(step_model)

ggplot() +
  geom_line(data=cpi_full_train, mapping=aes(x=1:length(Year), y=Inf_Rate), col="darkred") +
  geom_line(data=cpi_full_test, mapping=aes(x=(1:length(Year))+length(cpi_full_train$Year), y=Inf_Rate), col="darkblue") +
  geom_line(mapping=aes(x=(1:length(lin_forecast3))+length(cpi_full_train$Year), y=lin_forecast1), col="blue")


# ARIMA Models
arima_eval <- function(x_train=NULL, y_train, x_test=NULL, y_test, for_period=60, variables="FMUCOR"){
  fit <- auto.arima(y_train, xreg=as.matrix(x_train))
  fit_forecast <- forecast(fit, h=for_period, xreg=as.matrix(x_test))
  print(autoplot(fit_forecast))
  checkresiduals(fit)
  pred_error <- mean(abs(fit_forecast$mean-y_test))
  med_pred_error <- median(abs(fit_forecast$mean-y_test))
  max_pred_error <- max(abs(fit_forecast$mean-y_test))
  metrics <- c("arima", variables, pred_error, med_pred_error, max_pred_error, AIC(fit), BIC(fit))
  ret_list <- list(model=fit, forecast=fit_forecast, metrics=metrics, pred_residuals=y_test-fit_forecast$mean)
  return(ret_list)
}

base_fit <- auto.arima(cpi_full_train$Inf_Rate)
summary(base_fit)
base_fit_forecast <- forecast(base_fit, h=60)
autoplot(base_fit_forecast)
base_fit_MAE <- mean(abs(base_fit_forecast$mean-cpi_full_test$Inf_Rate))
base_fit_MedAE <- median(abs(base_fit_forecast$mean-cpi_full_test$Inf_Rate))
base_fit_MaxError <- max(abs(base_fit_forecast$mean-cpi_full_test$Inf_Rate))
base_fit_AIC <- AIC(base_fit)
base_fit_BIC <- BIC(base_fit)

# Fit 2
fit2 <- arima_eval(x_train=cpi_full_train[,4:9], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,4:9], y_test=cpi_full_test$Inf_Rate)

# Fit 3
fit3 <- arima_eval(x_train=cpi_full_train[,4:8], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,4:8], y_test=cpi_full_test$Inf_Rate, variables="FMUCO")

# Fit 4
fit4 <- arima_eval(x_train=cpi_full_train[,4:7], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,4:7], y_test=cpi_full_test$Inf_Rate, variables="FMUC")

# Fit 5
fit5 <- arima_eval(x_train=cpi_full_train[,4:6], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,4:6], y_test=cpi_full_test$Inf_Rate, variables="FMU")

# Fit 6
fit6 <- arima_eval(x_train=cpi_full_train[,4:5], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,4:5], y_test=cpi_full_test$Inf_Rate, variables="FM")

# Fit 7
fit7 <- arima_eval(x_train=cpi_full_train[,4], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,4], y_test=cpi_full_test$Inf_Rate, variables="F")

# Fit 8
fit8 <- arima_eval(x_train=cpi_full_train[,5], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,5], y_test=cpi_full_test$Inf_Rate, variables="M")

# Fit 9
fit9 <- arima_eval(x_train=cpi_full_train[,6], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,6], y_test=cpi_full_test$Inf_Rate, variables="U")

# Fit 10
fit10 <- arima_eval(x_train=cpi_full_train[,7], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,7], y_test=cpi_full_test$Inf_Rate, variables="C")

# Fit 11
fit11 <- arima_eval(x_train=cpi_full_train[,8], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,8], y_test=cpi_full_test$Inf_Rate, variables="O")

# Fit 12
fit12 <- arima_eval(x_train=cpi_full_train[,9], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,9], y_test=cpi_full_test$Inf_Rate, variables="R")

# NNAR Fit
nnetar_eval <- function(x_train=NULL, y_train, x_test=NULL, y_test, for_period=60, variables="FMUCOR"){
  if (is.null(x_train)){fit <- nnetar(y_train)} else {fit <- nnetar(y_train, xreg=as.matrix(x_train))}
  if (is.null(x_train)){fit_forecast <- forecast(fit, h=for_period, PI=F)} else {fit_forecast <- forecast(fit, h=for_period, xreg=as.matrix(x_test), PI=F)}
  print(autoplot(fit_forecast))
  checkresiduals(fit)
  pred_error <- mean(abs(fit_forecast$mean-y_test))
  med_pred_error <- median(abs(fit_forecast$mean-y_test))
  max_pred_error <- max(abs(fit_forecast$mean-y_test))
  metrics <- c("nnetar", variables, pred_error, med_pred_error, max_pred_error,
               NA, NA)
  ret_list <- list(model=fit, forecast=fit_forecast,
                   prediction_error=c(pred_error, med_pred_error),
                   internal_error=accuracy(fit),
                   pred_residuals=y_test-fit_forecast$mean,
                   metrics=metrics)
  return(ret_list)
}

# Base neural net fit
base_fit_neu <- nnetar(cpi_full_train$Inf_Rate)
base_fit_forecast_neu <- forecast(base_fit_neu, 60, PI=F)
autoplot(base_fit_forecast_neu) + theme_bw()
checkresiduals(base_fit_neu)
base_fit_neu_MAE <- mean(abs(base_fit_forecast_neu$mean-cpi_full_test$Inf_Rate))
base_fit_neu_MedAE <- median(abs(base_fit_forecast_neu$mean-cpi_full_test$Inf_Rate))
base_fit_neu_MaxError <- max(abs(base_fit_forecast_neu$mean-cpi_full_test$Inf_Rate))

# Fit 2
nfit2 <- nnetar_eval(x_train=cpi_full_train[4:9], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[4:9], y_test=cpi_full_test$Inf_Rate)

# Fit 3
nfit3 <- nnetar_eval(x_train=cpi_full_train[4:8], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[4:8], y_test=cpi_full_test$Inf_Rate, variables="FMUCO")

# Fit 4
nfit4 <- nnetar_eval(x_train=cpi_full_train[4:7], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[4:7], y_test=cpi_full_test$Inf_Rate, variables="FMUC")

# Fit 5
nfit5 <- nnetar_eval(x_train=cpi_full_train[4:6], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[4:6], y_test=cpi_full_test$Inf_Rate, variables="FMU")

# Fit 6
nfit6 <- nnetar_eval(x_train=cpi_full_train[4:5], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[4:5], y_test=cpi_full_test$Inf_Rate, variables="FM")

# Fit 7
nfit7 <- nnetar_eval(x_train=cpi_full_train[,4], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,4], y_test=cpi_full_test$Inf_Rate, variables="F")

# Fit 8
nfit8 <- nnetar_eval(x_train=cpi_full_train[,5], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,5], y_test=cpi_full_test$Inf_Rate, variables="M")

# Fit 9
nfit9 <- nnetar_eval(x_train=cpi_full_train[,6], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,6], y_test=cpi_full_test$Inf_Rate, variables="U")

# Fit 10
nfit10 <- nnetar_eval(x_train=cpi_full_train[,7], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,7], y_test=cpi_full_test$Inf_Rate, variables="C")

# Fit 11
nfit11 <- nnetar_eval(x_train=cpi_full_train[,8], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,8], y_test=cpi_full_test$Inf_Rate, variables="O")

# Fit 12
nfit12 <- nnetar_eval(x_train=cpi_full_train[,9], y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,9], y_test=cpi_full_test$Inf_Rate, variables="R")

# Fit 13
nfit13 <- nnetar_eval(x_train=cpi_full_train[,c(4:8)],
                      y_train=cpi_full_train$Inf_Rate, x_test=cpi_full_test[,c(4:8)],
                      y_test=cpi_full_test$Inf_Rate, variables="FMCO")

# Model Scoring
model_scores_df <- data.frame(method=c("linear", "linear", "arima"), variables=c("FMUCOR", "FCOR", "none"), predicted_MAE=c(full_model_MAE, step_model_MAE, base_fit_MAE), predicted_MedAE=c(full_model_MedAE, step_model_MedAE, base_fit_MedAE), predicted_MaxError=c(full_model_Max_Error, step_model_Max_Error, base_fit_MaxError), AIC=c(full_model_AIC, step_model_AIC, base_fit_AIC), BIC=c(full_model_BIC, step_model_BIC, base_fit_BIC))

# ARIMA Fits
model_scores_df[4,] <- fit2$metrics
# autoplot(fit2$forecast) + theme_bw() + labs(y="Inflation Rate (%)")
# ggsave("ArimaForecast.png", width=1000, height=750, dpi="print", units="px", scale=1.5)
model_scores_df[5,] <- fit3$metrics
model_scores_df[6,] <- fit4$metrics
model_scores_df[7,] <- fit5$metrics
model_scores_df[8,] <- fit6$metrics
model_scores_df[9,] <- fit7$metrics
model_scores_df[10,] <- fit8$metrics
model_scores_df[11,] <- fit9$metrics
model_scores_df[12,] <- fit10$metrics
model_scores_df[13,] <- fit11$metrics
model_scores_df[14,] <- fit12$metrics

# NNAR Fits
model_scores_df[15,] <- c("nnetar", "none", base_fit_neu_MAE, base_fit_neu_MedAE, base_fit_neu_MaxError, NA, NA)
model_scores_df[16,] <- nfit2$metrics
# nfit2_forecasts_display <- forecast(nfit2$model, h=60, PI=TRUE, xreg=cpi_full_test[,4:9])
# autoplot(nfit2_forecasts_display) + theme_bw() + labs(y="Inflation Rate (%)")
# ggsave("NNARForecast.png", width=1000, height=750, dpi="print", units="px", scale=1.5)
checkresiduals(nfit2$model)
checkresiduals(nfit3$model)
checkresiduals(nfit11$model)
model_scores_df[17,] <- nfit3$metrics
model_scores_df[18,] <- nfit4$metrics
model_scores_df[19,] <- nfit5$metrics
model_scores_df[20,] <- nfit6$metrics
model_scores_df[21,] <- nfit7$metrics
model_scores_df[22,] <- nfit8$metrics
model_scores_df[23,] <- nfit9$metrics
model_scores_df[24,] <- nfit10$metrics
model_scores_df[25,] <- nfit11$metrics
model_scores_df[26,] <- nfit12$metrics

# Combination Forecasting
comb_forecast <- (nfit3$forecast$mean + nfit11$forecast$mean + nfit2$forecast$mean)/3
comb_predicted_MAE <- mean(abs(comb_forecast-cpi_full_test$Inf_Rate))
comb_predicted_MedAE <- median(abs(comb_forecast-cpi_full_test$Inf_Rate))
comb_predicted_MaxError <- max(abs(comb_forecast-cpi_full_test$Inf_Rate))

model_scores_df[27,] <- c("combination", "average", comb_predicted_MAE, comb_predicted_MedAE, comb_predicted_MaxError, NA, NA)

hist(comb_forecast-cpi_full_test$Inf_Rate, breaks=20)
plot(comb_forecast, abs(comb_forecast-cpi_full_test$Inf_Rate))

# Visualizing model scores
model_scores_df[1:26,] %>%
  group_by(method) %>%
  ggplot() +
    geom_col(mapping=aes(x=method, y=predicted_MaxError), alpha=0.55, fill="yellow", col="gray", position="dodge") +
    geom_col(mapping=aes(x=method, y=predicted_MedAE), alpha=0.45, fill="red", col="gray", position="dodge") +
    geom_col(mapping=aes(x=method, y=predicted_MAE), alpha=0.35, fill="orange", col="gray", position="dodge") +
    scale_y_discrete(breaks=NULL) + scale_x_discrete(labels=c("ARIMA", "Linear", "NNAR")) + 
    labs(title="Model Error Values by Method",
         subtitle="Note: Y-Axis does not represent numerical error values",
         x="Method", y="Predicted Error") + theme_bw()

forecasts <- data.frame(Date=cpi_full_test$Date,
                        nfit2=nfit2$forecast$mean, nfit3=nfit3$forecast$mean, nfit11=nfit11$forecast$mean, 
                        comb=comb_forecast, actual=cpi_full_test$Inf_Rate)

forecast_palette <- c("Actual"="darkred", "NNAR 2"="#183F8B", "NNAR 3"="#de6449",
                      "NNAR 11"="#8E3B46", "Combined"="#208AAE")
ggplot(data=forecasts, mapping=aes(x=Date)) +
  geom_line(mapping=aes(y=`actual`, col="Actual"), size=0.75) +
  geom_line(mapping=aes(y=`nfit2`, col="NNAR 2"), alpha=0.5) +
  geom_line(mapping=aes(y=`nfit3`, col="NNAR 3"), alpha=0.5) +
  geom_line(mapping=aes(y=`nfit11`, col="NNAR 11"), alpha=0.5) +
  geom_line(mapping=aes(y=`comb`, col="Combined"), size=1) +
  geom_line(mapping=aes(y=`comb`, col="Combined"), size=5, alpha=0.2) +
  geom_vline(xintercept=as.Date("2018-1-1"), linetype="dashed", alpha=0.25) +
  geom_vline(xintercept=as.Date("2019-1-1"), linetype="dashed", alpha=0.25) +
  geom_vline(xintercept=as.Date("2020-1-1"), linetype="dashed", alpha=0.25) +
  geom_vline(xintercept=as.Date("2021-1-1"), linetype="dashed", alpha=0.25) +
  geom_vline(xintercept=as.Date("2022-1-1"), linetype="dashed", alpha=0.25) +
  scale_x_date(date_labels = "%b %Y") + theme_bw() + 
  labs(title="U.S. Inflation Rate Predictions versus Actual",
       subtitle="From Sep. 2017 to Aug. 2022", y="Inflation Rate (%)",
       col="Legend", caption="Source for Actual: St. Louis Federal Reserve Economic Data") +
  scale_color_manual(values = forecast_palette)
ggsave("TestForecastPlot.png", width=1000, height=750, units="px", dpi="print", scale=1.8)

# Forecasting variables
future_fedfunds <- seq(from=FEDFUNDS$FEDFUNDS[813], to=4.5, length.out=12)
M2_Forecast <- read.csv("M2Forecast.csv")
future_M2 <- M2_Forecast$YoY.Percent.Change
future_unemployment <- seq(from=unemployment_long$Unemployment[897], to=3.6, length.out=12)
Oil_Forecast <- read.csv("WTISPLCForecast.csv")
future_oil <- Oil_Forecast$Oil_Change

sentiment_model <- nnetar(cpi_full$Consumer_Sentiment)
sentiment_forecast <- forecast(sentiment_model, h=12)
commodities_model <- nnetar(cpi_full$Commodities_Index)
commodities_forecast <- forecast(commodities_model, h=12)

# Constructing xreg data frame
future_xreg <- data.frame(Fed_Rate=future_fedfunds, MS_Pct_Change=future_M2,
                          Unemployment=future_unemployment,
                          Consumer_Sentiment=sentiment_forecast$mean,
                          Oil_Price_Index=future_oil,
                          Commodities_Index=commodities_forecast$mean)

# Train best models with all data except future values
# NNAR fit 2
final_nfit2 <- nnetar(cpi_full$Inf_Rate, xreg=as.matrix(cpi_full[,4:9]))
accuracy(final_nfit2)
final_nfit2_forecast <- forecast(final_nfit2, h=12, xreg=as.matrix(future_xreg))
autoplot(final_nfit2_forecast)

# NNAR fit 3
final_nfit3 <- nnetar(cpi_full$Inf_Rate, xreg=as.matrix(cpi_full[,4:8]))
accuracy(final_nfit3)
final_nfit3_forecast <- forecast(final_nfit3, h=12, xreg=as.matrix(future_xreg[,1:5]))
autoplot(final_nfit3_forecast)

# NNAR fit 11
final_nfit11 <- nnetar(cpi_full$Inf_Rate, xreg=as.matrix(cpi_full[,8]))
accuracy(final_nfit11)
final_nfit11_forecast <- forecast(final_nfit11, h=12, xreg=as.matrix(future_xreg$Oil_Price_Index))
autoplot(final_nfit11_forecast)

# Combination Forecast
final_comb_forecast <- (final_nfit2_forecast$mean + final_nfit3_forecast$mean + final_nfit11_forecast$mean)/3

future_inf_rate_forecasts <- data.frame(date=mdy(c("9/1/2022", "10/1/2022", "11/1/2022",
                                               "12/1/2022", "1/1/2023", "2/1/2023",
                                               "3/1/2023", "4/1/2023", "5/1/2023",
                                               "6/1/2023", "7/1/2023", "8/1/2023")),
                                        nfit2=final_nfit2_forecast$mean,
                                        nfit3=final_nfit3_forecast$mean,
                                        nfit11=final_nfit11_forecast$mean,
                                        combination=final_comb_forecast)

# Future Forecast Chart
ggplot() +
  geom_line(data=cpi_full[700:length(cpi_full$Date),], mapping=aes(Date, y=Inf_Rate, col="Actual"), size=0.75) +
  geom_line(data=future_inf_rate_forecasts,
            mapping=aes(x=date, y=`nfit2`, col="NNAR 2"), alpha=0.5) +
  geom_line(data=future_inf_rate_forecasts,
            mapping=aes(x=date, y=`nfit3`, col="NNAR 3"), alpha=0.5) +
  geom_line(data=future_inf_rate_forecasts,
            mapping=aes(x=date, y=`nfit11`, col="NNAR 11"), alpha=0.5) +
  geom_line(data=future_inf_rate_forecasts,
            mapping=aes(x=date, y=`combination`, col="Combined"), size=1) +
  geom_line(data=future_inf_rate_forecasts,
            mapping=aes(x=date, y=`combination`, col="Combined"), size=5, alpha=0.2) +
  geom_label(data=future_inf_rate_forecasts[12,],
             mapping=aes(x=date, y=`nfit2`), col="#183F8B",
             label=paste0(round(future_inf_rate_forecasts$nfit2[12], 2), "%"),
             label.padding=unit(0.15, "lines"), nudge_x = 150, nudge_y = 0.4) +
  geom_label(data=future_inf_rate_forecasts[12,],
             mapping=aes(x=date, y=`nfit3`), col="#de6449",
             label=paste0(round(future_inf_rate_forecasts$nfit3[12], 2), "%"),
             label.padding=unit(0.15, "lines"), nudge_x = 150, nudge_y = 0.4) +
  geom_label(data=future_inf_rate_forecasts[12,],
             mapping=aes(x=date, y=`nfit11`), col="#8E3B46",
             label=paste0(round(future_inf_rate_forecasts$nfit11[12], 2), "%"),
             label.padding=unit(0.15, "lines"), nudge_x = 150) +
  geom_label(data=future_inf_rate_forecasts[c(4,12),],
            mapping=aes(x=date, y=`combination`), col="#208AAE",
            label=paste0(round(future_inf_rate_forecasts$combination[c(4,12)], 2), "%"),
            label.padding=unit(0.15, "lines"), nudge_x = 150) +
  geom_vline(xintercept=as.Date("2022-9-1"), linetype="dashed", alpha=0.25) +
  scale_x_date(date_labels = "%b %Y") + theme_bw() + 
  labs(title="U.S. Inflation Rate 12-Month Forecast", y="Inflation Rate (%)",
       col="Legend", subtitle = "Forecast Period: Sep. 2022 to Aug. 2023") +
  scale_color_manual(values = forecast_palette)

ggsave("FutureForecastPlot.png", width=1500, height=750, units="px", dpi="print", scale=1.8)

