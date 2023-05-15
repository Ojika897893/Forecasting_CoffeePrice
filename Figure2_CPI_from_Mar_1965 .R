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

ggplot(ICO,aes(x=Month,y=ICO_CIP))+
  geom_line() +
  labs(x="monthly",y="(US cents per lb)",
       title= "ICO COMPOSITE INDICATOR PRICE(World Market Price)") +
  theme(title = element_text(size=15))

ggplot(ICO,aes(x=Month,y=GIP_ROBUSTAS))+
  geom_line() +
  labs(x="monthly",y="Brazil(US cents per lb)",
       title= "ICO COMPOSITE INDICATOR PRICE(Brazil and some others)") +
  theme(title = element_text(size=15))

#CPI
usa_economy <- global_economy %>%
  filter(Code == "USA") %>%
  as_tsibble(index = Year)


View(usa_economy)

#tsibble
t_ICO <- ICO %>%
  mutate(Month = yearmonth(Month)) %>%
  select(!(GIP_COLOMBIAN:GIP_OTHER))%>%
  as_tsibble(index = Month)
head(t_ICO)

Coffee_new1 <- t_ICO                                 # Duplicate data
Coffee_new1$Year <-  strftime(Coffee_new1$Month, "%Y")    # Create/adding year column
Coffee_new1$Month <-  strftime(Coffee_new1$Month, "%m")   # Create/adding month column
head(Coffee_new1)

chars <- sapply(Coffee_new1, is.character)
Coffee_new1[ , chars] <- as.data.frame(apply(Coffee_new1[ , chars], 2, as.numeric))
sapply(Coffee_new1,class)

#usa_economy
usa_economy <- global_economy |>
  filter(Code == "USA") %>%
  mutate(Year = 1960:2017)  %>%
  as_tsibble(index = Year)
View(usa_economy)

library(tidyr)
library(tsibble)
library(zoo)
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

View(Join_coffee)

Join_coffee$YearMonth <- as.yearmon(paste(Join_coffee$Year, Join_coffee$Month), "%Y %m")
head(Join_coffee$YearMonth)
View(Join_coffee)

colnames(Join_coffee)

Joined_coffee <- Join_coffee |>
  select(Adjusted_ICO_CIP,YearMonth)%>%
  mutate(YearMonth = yearmonth(YearMonth)) %>%
  as_tsibble(index = YearMonth)

head(Joined_coffee)
View(Joined_coffee)

# plot for World price with CPI controlled
ggplot(Joined_coffee,aes(YearMonth,Adjusted_ICO_CIP))+
  geom_line() +
  labs(x="monthly",y="(US cents per lb)",
       title= "ICO COMPOSITE INDICATOR PRICE(World Market Price) Adjsuted") +
  theme(title = element_text(size=15)) #plot the graph 

#split
tsibble_ICO_training <- Joined_coffee %>%
  mutate(YearMonth = yearmonth(YearMonth)) %>%
  filter(YearMonth <= yearmonth("2022-03")) %>%
  as_tsibble(index = YearMonth)

View(tsibble_ICO_training)

tsibble_ICO_test <- Joined_coffee %>%
  mutate(YearMonth = yearmonth(YearMonth)) %>%
  filter(YearMonth > yearmonth("2022-03")) %>%
  as_tsibble(index = YearMonth)

View(tsibble_ICO_test)

fit <- tsibble_ICO_training |>
  model(
    SES = ETS(Adjusted_ICO_CIP ~ error("A") + trend("N") + season("N")),
    Holt = ETS(Adjusted_ICO_CIP ~ error("A") + trend("A") + season("N")),
    Damped = ETS(Adjusted_ICO_CIP ~ error("A") + trend("Ad") +season("N")),
    additive = ETS(Adjusted_ICO_CIP ~ error("A") + trend("A") +
                     season("A")),
    multiplicative = ETS(Adjusted_ICO_CIP ~ error("M") + trend("A") +
                           season("M"))
  ) 

accuracy(fit) |>
  select(!(MAE:ACF1))

fit <- tsibble_ICO_training |>
  model(
    SES = ETS(Adjusted_ICO_CIP ~ error("A") + trend("N") + season("N")),
    Holt = ETS(Adjusted_ICO_CIP ~ error("A") + trend("A") + season("N")),
    Damped = ETS(Adjusted_ICO_CIP ~ error("A") + trend("Ad") +season("N")),
    additive = ETS(Adjusted_ICO_CIP ~ error("A") + trend("A") +
                     season("A")),
    multiplicative = ETS(Adjusted_ICO_CIP ~ error("M") + trend("A") +
                           season("M"))
  ) 

fit_fc <- fit|>forecast(h = 12)

accuracy(fit_fc,Joined_coffee )|>
  select(!(MAE:ACF1))

SESs <- Joined_coffee |>
  model(
    SES = ETS(Adjusted_ICO_CIP ~ error("A") + trend("N") + season("N")),
    Holt = ETS(Adjusted_ICO_CIP ~ error("A") + trend("A") + season("N")),
    Damped = ETS(Adjusted_ICO_CIP ~ error("A") + trend("Ad") +season("N")),
    additive = ETS(Adjusted_ICO_CIP ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Adjusted_ICO_CIP ~ error("M") + trend("A") +season("M"))
  )

tidy(SESs)  

report(SESs)|>
  select(!(MSE:MAE)) 

Forecast <- SESs |>
  forecast(h = 12)

View(Forecast)

SESs |> select(Damped)|>forecast(h=12) |>
  autoplot(Joined_coffee) +
  labs(y = "(US cents/lb )", title = "Damped/international price")

Joined_coffee |>
  model(Damped = ETS(Adjusted_ICO_CIP ~ error("A") + trend("Ad") + season("N"))) |>
  gg_tsresiduals()

aug <- Joined_coffee %>%
  model(Damped = ETS(Adjusted_ICO_CIP ~ error("A") + trend("Ad") + season("N"))) %>%
  augment()

aug |> features(.innov, ljung_box, lag = 12)
#2.89e-15 = 0.00000000000000289

#Time series decomposition
#train
Com_ICO_train <- tsibble_ICO_training |>
  model(stl = STL(Adjusted_ICO_CIP~ trend(window = 7), robust = TRUE)) #seasonal-trend decomposition using LOSES

#plot the components
components(Com_ICO_train) %>%
  autoplot()

Com_SeaAdj_train <- components(Com_ICO_train) |>
  select(season_adjust) 

fit <- Com_SeaAdj_train |>
  model(Naive = NAIVE(season_adjust),
        Mean = MEAN(season_adjust),
        Snaive = SNAIVE(season_adjust~ lag("year")),
        Drift = RW(season_adjust ~ drift())
  )

tail(Com_SeaAdj_train)

accuracy(fit)|>
  select(!(MAE:ACF1))|>
  select(!(ME))

#for the test
rm <- fit |>
  forecast(h=12)

rm

#Time series decomposition
Com_ICO <- Joined_coffee |>
  model(stl = STL(Adjusted_ICO_CIP~ trend(window = 7), robust = TRUE))
tail(Joined_coffee)

accuracy(rm,components(Com_ICO)) |>
  select(!(MAE:ACF1))|>
  select(!(ME))

Com_SeaAdj_all <- components(Com_ICO) |>
  select(season_adjust) 

#combine seasonally adjusted data with regular plot

Com_SeaAdj_all |>
  model(Naive = NAIVE(season_adjust)) |>
  gg_tsresiduals()

aug <- Com_SeaAdj  %>%
  model(Naive = NAIVE(season_adjust)) %>%
  augment()

aug |> features(.innov, ljung_box, lag = 12)

#Diff 1st check
Joined_coffee  %>%
  gg_tsdisplay(Adjusted_ICO_CIP, plot_type='partial')

Joined_coffee |>
  features(Adjusted_ICO_CIP, unitroot_kpss)
# 0.01<0.05 Thus non-stationary #not a white noise series

Joined_coffee |>
  features(Adjusted_ICO_CIP, unitroot_ndiffs)


Joined_coffee |>
  features(Adjusted_ICO_CIP, unitroot_nsdiffs)

#Diff 2nd check
Diff_C <- Joined_coffee |>
  mutate(Adjusted_ICO_CIP = difference(Adjusted_ICO_CIP))%>%
  na.omit()

Diff_C  %>%
  gg_tsdisplay(Adjusted_ICO_CIP, plot_type='partial')

Diff_C |>
  features(Adjusted_ICO_CIP, unitroot_kpss)

Diff_C |>
  features(Adjusted_ICO_CIP, unitroot_ndiffs)

#ARIMA

fit <- Joined_coffee |>
  model(arima011 = ARIMA(Adjusted_ICO_CIP ~ pdq(0,1,1)),
        arima012 = ARIMA(Adjusted_ICO_CIP ~ pdq(0,1,2)),
        arima110 = ARIMA(Adjusted_ICO_CIP ~ pdq(1,1,0)),
        search313 = ARIMA(Adjusted_ICO_CIP, stepwise=FALSE))

report(fit)|>
  select(!(ar_roots:ma_roots ))|>
  select(!(sigma2:log_lik))

fit |> forecast(h=12) |>
  autoplot(Joined_coffee) +
  labs(y = "(US cents/lb )", title = "ARIMA313/international price")

fit <- tsibble_ICO_training |>
  model(arima011 = ARIMA(Adjusted_ICO_CIP ~ pdq(0,1,1)),
        arima012 = ARIMA(Adjusted_ICO_CIP ~ pdq(0,1,2)),
        arima110 = ARIMA(Adjusted_ICO_CIP ~ pdq(1,1,0)),
        search313 = ARIMA(Adjusted_ICO_CIP, stepwise=FALSE))

fit_fc <- fit |>forecast(h=12)

accuracy(fit)|>
  select(!(MAE:ACF1))|>
  select(!(ME))

accuracy(fit_fc,Joined_coffee)|>
  select(!(MAE:ACF1))|>
  select(!(ME))

Joined_coffee |>
  model(arima110 = ARIMA(Adjusted_ICO_CIP ~ pdq(1,1,0))) |>
  gg_tsresiduals()

aug <- Joined_coffee %>%
  model(arima110 = ARIMA(Adjusted_ICO_CIP ~ pdq(1,1,0))) %>%
  augment()

aug |> features(.innov, ljung_box, lag = 12)
#VAR
#Variable are non-stationary:-	
#But they are moving together (co-movement, “common trend”) – use them as they are. Visual+ cointegration test
View(Join_coffee)
Joined_coffee_Bra <- Join_coffee |>
  select(Adjusted_ICO_CIP,Adjusted_Brazil,YearMonth)%>%
  mutate(YearMonth = yearmonth(YearMonth)) %>%
  as_tsibble(index = YearMonth)

fit1 <- Joined_coffee_Bra |>
  model(
    aicc = VAR(vars(Adjusted_ICO_CIP,Adjusted_Brazil)),
    bic = VAR(vars(Adjusted_ICO_CIP,Adjusted_Brazil), ic = "bic")
  )
fit1

glance(fit1)|>
  select(!(sigma2:log_lik))

fit1 |>
  select(aicc) |>
  forecast(h=12)|>
  autoplot(Joined_coffee_Bra |> filter(year(YearMonth) > "2010-01"))

tsibble_ICO_training <- Joined_coffee_Bra %>%
  mutate(YearMonth = yearmonth(YearMonth)) %>%
  filter(YearMonth <= yearmonth("2022-03")) %>%
  as_tsibble(index = YearMonth)

tsibble_ICO_test <- Joined_coffee_Bra %>%
  mutate(YearMonth = yearmonth(YearMonth)) %>%
  filter(YearMonth > yearmonth("2022-03")) %>%
  as_tsibble(index = YearMonth)

fit_RMSE <- tsibble_ICO_training |>
  model(
    aicc = VAR(vars(Adjusted_ICO_CIP,Adjusted_Brazil)),
    bic = VAR(vars(Adjusted_ICO_CIP,Adjusted_Brazil), ic = "bic")
  )

fit_RMSE|>
  accuracy()|>
  select(!(MAE:ACF1))|>
  select(!(ME))

fit_RMSE_fc <- fit_RMSE|>
  forecast(h=12)

accuracy(fit_RMSE_fc,Joined_coffee_Bra)|>
  select(!(MAE:ACF1))|>
  select(!(ME))

dplyr::bind_cols(fit_RMSE_fc)

fit1 |>
  augment() |>
  ACF(.innov) |>
  autoplot()

fit1 |>
  select(aicc) |>
  forecast(h=12)

aug <- Joined_coffee_Bra %>%
  model(aicc = VAR(vars(Adjusted_ICO_CIP,Adjusted_Brazil)))%>%
  augment()

aug |> features(.innov, ljung_box, lag = 12)

#Neural network model

fit <- tsibble_ICO_training |>
  model(NNETAR(sqrt(Adjusted_ICO_CIP)))

fit |>
  forecast(h = 12) |>
  autoplot(tsibble_ICO_training) +
  labs(x = "YearMonth", y = "In US cents/lb", title = "Neural network model:ICO")

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
  model(NNETAR(sqrt(Adjusted_ICO_CIP)))%>%
  augment()

aug |> features(.innov, ljung_box, lag = 12)

Joined_coffee_Bra |>
  model(NNETAR(sqrt(Adjusted_ICO_CIP))) |>
  gg_tsresiduals()


