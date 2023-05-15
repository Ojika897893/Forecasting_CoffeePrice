library(dplyr)
library(tsibble)
library(lubridate)
library(ggseas)
library(forecast)
library(fpp3)
library(readxl)
library(reshape)
library(tidyverse)
library(fabletools)
library(zoo)

#Download the data set as an Excel file
ICO_pre <- read_excel("/Users/wyattnesbit/Desktop/ECON4395/project/ICO_trimed.xlsx")
head(ICO_pre)
View(ICO_pre)

#Rename your file
colnames(ICO_pre)
ICO <- ICO_pre %>% 
  filter(Month > "1965-02-01")%>%
  dplyr::rename("ICO_CIP" = "ICO COMPOSITE INDICATOR PRICE",
                "GIP_COLOMBIAN" = "GROUP INDICATOR PRICE: COLOMBIAN MILD ARABICAS",
                "GIP_OTHER" = "GROUP INDICATOR PRICE: OTHER MILD ARABICAS",
                "GIP_BRAZILIAN"="GROUP INDICATOR PRICE: BRAZILIAN AND OTHER NATURAL ARABICAS",
                "GIP_ROBUSTAS"="GROUP INDICATOR PRICE: ROBUSTAS")

colnames(ICO)
head(ICO)

#converting some data sets into a numeric one
chars <- sapply(ICO, is.character)
ICO[ , chars] <- as.data.frame(apply(ICO[ , chars], 2, as.numeric))
sapply(ICO,class)

#CPI
usa_economy <- global_economy %>%
  filter(Code == "USA") %>%
  as_tsibble(index = Year)

View(usa_economy)

#tsibble
t_ICO <- ICO %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)
View(t_ICO)
             
head(t_ICO)
duplicates(ICO)
Coffee_new1 <- t_ICO                                 # Duplicate data
Coffee_new1$Year <-  strftime(Coffee_new1$Month, "%Y")    # Create/adding year column
Coffee_new1$Month <-  strftime(Coffee_new1$Month, "%m")   # Create/adding month column

head(Coffee_new1)

chars <- sapply(Coffee_new1, is.character)
Coffee_new1[ , chars] <- as.data.frame(apply(Coffee_new1[ , chars], 2, as.numeric))
sapply(Coffee_new1,class)

Join_coffee$YearMonth <- as.yearmon(paste(Join_coffee$Year, Join_coffee$Month), "%Y %m")
head(Join_coffee$YearMonth)
View(Join_coffee)

Join_coffee <- Coffee_new1 |>
  left_join(usa_economy, by = "Year") |>
  mutate(
    Adjusted_ICO_CIP = ifelse(Year == 2023, ICO_CIP / 132.3 * 100,
                              ifelse(Year == 2022, ICO_CIP / 132.3 * 100,
                                     ifelse(Year == 2021, ICO_CIP / 124.3 * 100,
                                            ifelse(Year == 2020, ICO_CIP / 118.7 * 100,
                                                   ifelse(Year == 2019, ICO_CIP / 117.2 * 100,
                                                          ifelse(Year == 2018, ICO_CIP / 115.2 * 100,
                                                                 ICO_CIP / CPI * 100)))))),
    Adjusted_Brazil = ifelse(Year == 2023, GIP_BRAZILIAN / 132.3 * 100,
                              ifelse(Year == 2022, GIP_BRAZILIAN / 132.3 * 100,
                                     ifelse(Year == 2021, GIP_BRAZILIAN / 124.3 * 100,
                                            ifelse(Year == 2020, GIP_BRAZILIAN / 118.7 * 100,
                                                   ifelse(Year == 2019, GIP_BRAZILIAN / 117.2 * 100,
                                                          ifelse(Year == 2018, GIP_BRAZILIAN / 115.2 * 100,
                                                                 GIP_BRAZILIAN / CPI * 100)))))))

Join_coffee$YearMonth <- as.yearmon(paste(Join_coffee$Year, Join_coffee$Month), "%Y %m")
head(Join_coffee$YearMonth)
View(Join_coffee)

Joined_coffee_Bra <- Join_coffee |>
  select(Adjusted_ICO_CIP,Adjusted_Brazil,YearMonth)%>%
  mutate(YearMonth = yearmonth(YearMonth)) %>%
  as_tsibble(index = YearMonth)

View(Joined_coffee_Bra)
# plot for World price with CPI uncontrolled
ggplot(Join_coffee,aes(YearMonth,ICO_CIP))+
  geom_line() +
  labs(x="monthly",y="In US cents/lb",
       title= "ICO COMPOSITE INDICATOR PRICE") +
  theme(title = element_text(size=15)) #plot the graph 

ggplot(Join_coffee,aes(YearMonth,GIP_BRAZILIAN))+
  geom_line() +
  labs(x="monthly",y="In US cents/lb",
       title= "ICO COMPOSITE INDICATOR PRICE for Brazil") +
  theme(title = element_text(size=15)) #plot the graph

tsibble_ICO_training_Bra <- Joined_coffee_Bra %>%
  mutate(YearMonth = yearmonth(YearMonth)) %>%
  filter(YearMonth <= yearmonth("2022-03")) %>%
  as_tsibble(index = YearMonth)

tsibble_ICO_test_Bra <- Joined_coffee_Bra %>%
  mutate(YearMonth = yearmonth(YearMonth)) %>%
  filter(YearMonth > yearmonth("2022-03")) %>%
  as_tsibble(index = YearMonth)

# plot for World price with CPI controlled
tail(Joined_coffee_Bra)

ggplot(Joined_coffee_Bra,aes(YearMonth,Adjusted_Brazil))+
  geom_line() +
  labs(x="monthly",y="ICO COMPOSITE INDICATOR PRICE",
       title= "ICO COMPOSITE INDICATOR PRICE for Adjusted Brazil") +
  theme(title = element_text(size=15))+
  theme(axis.text.x=element_text(size=15))#plot the graph 

ggplot(Joined_coffee_Bra,aes(YearMonth,Adjusted_ICO_CIP))+
  geom_line() +
  labs(x="monthly",y="ICO COMPOSITE INDICATOR PRICE",
       title= "ICO COMPOSITE INDICATOR PRICE: Inflation adjusted with auxiliary line") +
  theme(title = element_text(size=15)) + #plot the graph
  geom_abline(intercept = 129, slope = 0, col="blue")

no_drift <- Joined_coffee_Bra %>%
  mutate(YearMonth = yearmonth(YearMonth)) %>%
  filter(YearMonth <= yearmonth("1990-01")) %>%
  filter(Adjusted_ICO_CIP <= 140) %>%
  as_tsibble(index = YearMonth)
View(no_drift)
View(Joined_coffee_Bra)

#Exclude the natural disaster and CPI

Join_coffee <- Coffee_new1

Join_coffee$YearMonth <- as.yearmon(paste(Join_coffee$Year, Join_coffee$Month), "%Y %m")
head(Join_coffee$YearMonth)
View(Join_coffee)

Joined_coffee_Bra <- Join_coffee |>
  select(ICO_CIP,GIP_BRAZILIAN,YearMonth) %>%
  mutate(YearMonth = yearmonth(YearMonth)) %>%
  filter(YearMonth >= yearmonth("1989-07")) %>%
  as_tsibble(index = YearMonth)

ggplot(Joined_coffee_Bra,aes(YearMonth,ICO_CIP))+
  geom_line() +
  labs(x="monthly",y="ICO COMPOSITE INDICATOR PRICE",
       title= "ICO COMPOSITE INDICATOR PRICE: starting from 1989 July") +
  theme(title = element_text(size=15))

View(Joined_coffee_Bra)

tsibble_ICO_training_Bra <- Joined_coffee_Bra %>%
  mutate(YearMonth = yearmonth(YearMonth)) %>%
  filter(YearMonth <= yearmonth("2022-03")) %>%
  as_tsibble(index = YearMonth)

tsibble_ICO_test_Bra <- Joined_coffee_Bra %>%
  mutate(YearMonth = yearmonth(YearMonth)) %>%
  filter(YearMonth > yearmonth("2022-03")) %>%
  as_tsibble(index = YearMonth)

#Diff 1st check
Joined_coffee_Bra  %>%
  gg_tsdisplay(ICO_CIP, plot_type='partial')

Joined_coffee_Bra |>
  features(ICO_CIP, unitroot_kpss) # 0.01<0.05 Thus non-stationary #not a white noise series

Joined_coffee_Bra |>
  features(ICO_CIP, unitroot_ndiffs)

Joined_coffee_Bra |>
  features(ICO_CIP, unitroot_nsdiffs)

#Diff 2nd check
Diff_C <- Joined_coffee_Bra |>
  mutate(ICO_CIP = difference(ICO_CIP))%>%
  select(ICO_CIP,YearMonth)%>%
  na.omit()

Diff_C  %>%
  gg_tsdisplay(ICO_CIP, plot_type='partial')

Diff_C |>
  features(ICO_CIP, unitroot_kpss)

Diff_C |>
  features(ICO_CIP, unitroot_ndiffs)

#SES
tail(tsibble_ICO_training_Bra)

fit <- tsibble_ICO_training_Bra  |>
  model(
    SES = ETS(ICO_CIP ~ error("A") + trend("N") + season("N")),
    Holt = ETS(ICO_CIP ~ error("A") + trend("A") + season("N")),
    Damped = ETS(ICO_CIP ~ error("A") + trend("Ad") +season("N")),
    additive = ETS(ICO_CIP ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(ICO_CIP ~ error("M") + trend("A") + season("M"))
  )

accuracy(fit)

fit_fc <- fit |>
  forecast(h=12)

accuracy(fit_fc,Joined_coffee_Bra)

tidy(fit)

fit_IC <- Joined_coffee_Bra  |>
  model(
    SES = ETS(ICO_CIP ~ error("A") + trend("N") + season("N")),
    Holt = ETS(ICO_CIP ~ error("A") + trend("A") + season("N")),
    Damped = ETS(ICO_CIP ~ error("A") + trend("Ad") +season("N")),
    additive = ETS(ICO_CIP ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(ICO_CIP ~ error("M") + trend("A") + season("M"))
  )

report(fit_IC)

fit_fc |>
  autoplot(tsibble_ICO_training_Bra)

fit_IC |>
  forecast(h=12)|>
  autoplot(Joined_coffee_Bra)

View(fit_IC |>
       forecast(h=12))

View(fit_IC |>
       select(.model = Damped)|>
       forecast(h=12))

Joined_coffee_Bra |>
  model(Damped = ETS(ICO_CIP ~ error("A") + trend("Ad") + season("N"))) |>
  gg_tsresiduals()

aug <- Joined_coffee_Bra %>%
  model(Damped = ETS(ICO_CIP ~ error("A") + trend("Ad") + season("N"))) %>%
  augment()

aug |> features(.innov, ljung_box, lag = 12)

#Time series decomposition
Com_ICO <- Joined_coffee_Bra |>
  model(stl = STL(ICO_CIP)) #seasonal-trend decomposition using LOSES

#plot the components
components(Com_ICO) %>%
  autoplot()

#filter the seasonally adjusted one and apply models. 
Com_SeaAdj <- components(Com_ICO) |>
  select(season_adjust) |>
  filter(year(YearMonth) > 2015 )

fore <- Com_SeaAdj |>
  model(NAIVE(season_adjust),
        MEAN(season_adjust),
        SNAIVE(season_adjust~ lag("year")),
        RW(season_adjust ~ drift())
  ) |>
  forecast(h = 12)

fore |>
  autoplot(Com_SeaAdj)

View(fore)

#combine seasonally adjusted data with regular plot
Joined_coffee_Bra %>%
  autoplot(ICO_CIP, color = "black") +
  autolayer(components(Com_ICO), season_adjust, color = "blue") +
  labs(
    y = "(US cents per lb)",
    title = "International Price of Coffee"
  )

fore <- tsibble_ICO_training_Bra|>
  model(Naive = NAIVE(ICO_CIP),
        Mean = MEAN(ICO_CIP),
        Snaive = SNAIVE(ICO_CIP~ lag("year")),
        Drift = RW(ICO_CIP ~ drift())
  )

RMSE <- fore|>
  forecast(h = 12)

accuracy(RMSE,Joined_coffee_Bra) |>
  select(.model,RMSE)

accuracy(fore)|>
  select(.model,RMSE)

Joined_coffee_Bra |>
  model(Naive = NAIVE(ICO_CIP)) |>
  gg_tsresiduals()

aug <- Joined_coffee_Bra %>%
  model(Naive = NAIVE(ICO_CIP)) %>%
  augment()

aug |> features(.innov, ljung_box, lag = 12)

#ARIMA

fit <- tsibble_ICO_training_Bra |>
  model(arima011 = ARIMA(ICO_CIP ~ pdq(1,1,0)),
        arima012 = ARIMA(ICO_CIP ~ pdq(0,1,2)),
        stepwise013 = ARIMA(ICO_CIP),
        search015 = ARIMA(ICO_CIP, stepwise=FALSE))

fit_fc <- fit |>forecast(h=12)

accuracy(fit)|>
  select(.model,.type,RMSE)

accuracy(fit_fc,Joined_coffee_Bra)|>
  select(.model,.type,RMSE)

report(fit) |>
  select(.model,AIC,AICc,BIC)

Joined_coffee_Bra |>
  model(search313 = ARIMA(ICO_CIP, stepwise=FALSE)) |>
  gg_tsresiduals()

aug <- Joined_coffee_Bra %>%
  model(search313 = ARIMA(ICO_CIP, stepwise=FALSE)) %>%
  augment()

aug |> features(.innov, ljung_box, lag = 12)

#VAR
#Variable are non-stationary:-	
#But they are moving together (co-movement, “common trend”) – use them as they are. Visual+ cointegration test

fit1 <- Joined_coffee_Bra |>
  model(
    aicc = VAR(vars(ICO_CIP,GIP_BRAZILIAN)),
    bic = VAR(vars(ICO_CIP,GIP_BRAZILIAN), ic = "bic")
  )
fit1

glance(fit1)|>
  select(!(sigma2:log_lik))

fit1 |>
  select(aicc) |>
  forecast(h=12)|>
  autoplot(Joined_coffee_Bra |> filter(year(YearMonth) > "2010-01"))

fit_RMSE <- tsibble_ICO_training_Bra |>
  model(
    aicc = VAR(vars(ICO_CIP,GIP_BRAZILIAN)),
    bic = VAR(vars(ICO_CIP,GIP_BRAZILIAN), ic = "bic")
  )

fit_RMSE|>
  accuracy()|>
  select(!(MAE:ACF1))|>
  select(!(ME))

fit_RMSE_fc <- fit_RMSE|>
  forecast(h=12)

View(fit_RMSE_fc)

dplyr::bind_cols(fit_RMSE_fc)

accuracy(fit_RMSE_fc,Joined_coffee_Bra)|>
  select(!(MAE:ACF1))|>
  select(!(ME))

fit1 |>
  augment() |>
  ACF(.innov) |>
  autoplot()

fit1 |>
  select(aicc) |>
  forecast(h=12)

aug <- Joined_coffee_Bra %>%
  model(aicc = VAR(vars(ICO_CIP,GIP_BRAZILIAN)))%>%
  augment()

aug |> features(.innov, ljung_box, lag = 12)

aug <- Joined_coffee_Bra %>%
  model(bic = VAR(vars(ICO_CIP,GIP_BRAZILIAN), ic = "bic"))%>%
  augment()

aug |> features(.innov, ljung_box, lag = 12)



#Neural network model

fit <- tsibble_ICO_training_Bra |>
  model(NNETAR(sqrt(ICO_CIP)))

fit|>
  accuracy()|>
  select(!(MAE:ACF1))|>
  select(!(ME))

fit_fc <- fit|>
  forecast(h=12)

accuracy(fit_fc,Joined_coffee_Bra)|>
  select(!(MAE:ACF1))|>
  select(!(ME))

aug <- Joined_coffee_Bra %>%
  model(NNETAR(sqrt(ICO_CIP)))%>%
  augment()

aug |> features(.innov, ljung_box, lag = 12)

Joined_coffee_Bra |>
  model(NNETAR(sqrt(ICO_CIP))) |>
  gg_tsresiduals()

fit <- Joined_coffee_Bra |>
  model(NNETAR(sqrt(ICO_CIP)))

fit |>
  forecast(h = 12) |>
  autoplot(Joined_coffee_Bra) +
  labs(x = "YearMonth", y = "In US cents/lb", title = "Neural network model:ICO")
