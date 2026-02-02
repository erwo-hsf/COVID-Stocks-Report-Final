# install all packages 
install.packages(c(
  "quantmod", "xts", "zoo", "lubridate",
  "dplyr", "tibble", "tidyr", "ggplot2",
  "tidyverse",
  "tsibble", "feasts", "fable",
  "tseries", "urca",
  "lmtest", "sandwich"
))

#load all packages
library(quantmod)
library(xts)
library(zoo)

library(dplyr)
library(tibble)

library(ggplot2)


library(tsibble)
library(feasts)
library(fable)

library(tseries)
library(urca)

library(lmtest)

library(sandwich)


#load packages
#load packages
library(quantmod)
library(dplyr)
library(xts)
library(tidyverse)

# Import Johns Hopkins Github data
confirmedraw <- read.csv( "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deathsraw <- read.csv( "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recoveredraw <- read.csv( "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

confirmed <- confirmedraw %>% 
  gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>%    group_by(Country.Region, date) %>% 
  summarize(confirmed=sum(confirmed))
deaths <- deathsraw %>% 
  gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% 
  group_by(Country.Region, date) %>% 
  summarize(deaths=sum(deaths))
recovered <- recoveredraw %>% 
  gather(key="date", value="recovered", -c(Country.Region, Province.State, Lat, Long)) %>% 
  group_by(Country.Region, date) %>% 
  summarize(recovered=sum(recovered))
summary(confirmed)

# Final data: combine all three
country <- full_join(confirmed, deaths) %>% 
  full_join(recovered)

# Date variable
# repair date
country$date <- country$date %>% 
  sub("X", "", .) %>% 
  as.Date("%m.%d.%y")

# new variable: number of days
country <- country %>% 
  group_by(Country.Region) %>% 
  mutate(cumconfirmed=cumsum(confirmed), days = date - first(date) + 1)

world <- country %>% 
  group_by(date) %>% 
  summarize(confirmed=sum(confirmed), cumconfirmed=sum(cumconfirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% 
  mutate(days = date - first(date) + 1)
# Extract specific country: Germany
germany <- country %>% dplyr::filter(Country.Region=="Germany")

# SUMMARY STATISTICS
summary(country)
by(country$confirmed, country$Country.Region, summary)
by(country$cumconfirmed, country$Country.Region, summary)
by(country$deaths, country$Country.Region, summary)
by(country$recovered, country$Country.Region, summary)
summary(world)
summary(germany)

# World confirmed
ggplot(world, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))


# load packages
library(quantmod)
library(xts)



tickers <- c("LHA.DE", "BNTX", "AAPL", "SHEL")

getSymbols(Symbols = tickers,
           src = "yahoo",
           from = "2019-01-01",
           to   = "2023-12-31")



# extract closing prices
AAPL_cl  <- Cl(AAPL)
BNTX_cl  <- Cl(BNTX)
LHA_cl   <- Cl(LHA.DE)
SHEL_cl  <- Cl(SHEL)

# combine time series based on dates
merged_cl <- merge(AAPL_cl, BNTX_cl, LHA_cl, SHEL_cl, all = TRUE)

# new column names
colnames(merged_cl) <- c("Apple", "BioNTech", "Lufthansa", "Shell")

library(tibble)
library(dplyr)

closing_table <- merged_cl %>%
  as.data.frame() %>%
  rownames_to_column("Date") %>%
  mutate(Date = as.Date(Date))


# Tabelle anzeigen
View(closing_table)
print(head(closing_table))




#Timeplots
#timeplot apple

#load packages
library(tsibble)
library(dplyr)
library(quantmod)
library(dplyr)
library(tsibble)
library(feasts)
library(ggplot2)
library(tibble)

#convert time series into a dataframe
aapl_df <- AAPL |>
  as.data.frame() |>
  rownames_to_column("Date") |>
  mutate(Date = as.Date(Date)) |>
  select(Date, Close = AAPL.Close) |>
  as_tsibble(index = Date)

#plot daily closing prices
autoplot(aapl_df, Close) +
  labs(title = "Apple (AAPL)",
       subtitle = "Daily closing price, 2019–2023",
       y = "Close (USD)")

# Timeplot BioNTech

#convert time series into a dataframe
bntx_df <- BNTX |>
  as.data.frame() |>
  rownames_to_column("Date") |>
  mutate(Date = as.Date(Date)) |>
  select(Date, Close = BNTX.Close) |>
  as_tsibble(index = Date)

#plot daily closing prices
autoplot(bntx_df, Close) +
  labs(title = "BioNTech (BNTX)",
       subtitle = "Daily closing price, 2019–2023",
       y = "Close (USD)")


# Timeplot Lufthansa

#convert time series into a dataframe
lha_df <- `LHA.DE` |>
  as.data.frame() |>
  rownames_to_column("Date") |>
  mutate(Date = as.Date(Date)) |>
  select(Date, Close = `LHA.DE.Close`) |>
  as_tsibble(index = Date)

#plot daily closing prices
autoplot(lha_df, Close) +
  labs(title = "Lufthansa (LHA.DE)",
       subtitle = "Daily closing price, 2019–2023",
       y = "Close (EUR)")

#Timeplot Shell

#convert time series into a dataframe
shel_df <- SHEL |>
  as.data.frame() |>
  rownames_to_column("Date") |>
  mutate(Date = as.Date(Date)) |>
  select(Date, Close = SHEL.Close) |>
  as_tsibble(index = Date)

#plot daily closing prices
autoplot(shel_df, Close) +
  labs(title = "Shell (SHEL)",
       subtitle = "Daily closing price, 2019–2023",
       y = "Close (USD)")


# Timeplot for all stocks
#load packages
library(dplyr)
library(tsibble)
library(feasts)

#cobine all stcoks into one dataset
all_stocks <- bind_rows(
  as_tibble(aapl_df) %>% mutate(Stock = "AAPL"),
  as_tibble(bntx_df) %>% mutate(Stock = "BNTX"),
  as_tibble(lha_df)  %>% mutate(Stock = "LHA.DE"),
  as_tibble(shel_df) %>% mutate(Stock = "SHEL")
) %>%
  as_tsibble(index = Date, key = Stock)

#plot daily closing prices
autoplot(all_stocks, Close) +
  labs(
    title = "Daily closing prices (2019–2023)",
    subtitle = "AAPL vs BNTX vs LHA.DE vs SHEL",
    y = "Close Price"
  )

# Timeplot for all stocks with indexed stock prices
library(ggplot2)
library(dplyr)
library(tsibble)
library(feasts)

#combine all stocks in one dataset with computing indexed prices
all_stocks <- bind_rows(
  as_tibble(aapl_df) %>% mutate(Stock="AAPL"),
  as_tibble(bntx_df) %>% mutate(Stock="BNTX"),
  as_tibble(lha_df)  %>% mutate(Stock="LHA.DE"),
  as_tibble(shel_df) %>% mutate(Stock="SHEL")
) %>%
  arrange(Stock, Date) %>%
  group_by(Stock) %>%
  mutate(Index = Close / first(na.omit(Close)) * 100) %>%
  ungroup() %>%
  as_tsibble(index = Date, key = Stock)

#plot indexed prices
autoplot(all_stocks, Index) +
  scale_y_log10() +
  labs(
    title = "Indexed Stock Prices",
    subtitle = "AAPL vs BNTX vs LHA.DE vs SHEL",
    y = "Index",
    x = "Date"
  )

# uncontinuous forecasts

# forecast apple
# install packages
install.packages("quantmod")
install.packages("tsibble")
install.packages("fable")
install.packages("feasts")
install.packages("dplyr")
install.packages("tibble")
install.packages("tidyr")
install.packages("ggplot2")

# load packages
library(quantmod)
library(tsibble)
library(fable)
library(feasts)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)

# load data
getSymbols("AAPL", src="yahoo", from="2019-01-01", to="2023-12-31")

# convert AAPL xts object into a tidy data frame
aapl_raw <- as.data.frame(AAPL) |>
  rownames_to_column("Date") |>
  mutate(Date = as.Date(Date),
         Close = as.numeric(AAPL[,4])) |>
  select(Date, Close)

# create a complete daily date sequence
full_dates <- tibble(
  Date = seq(as.Date("2019-01-01"),
             as.Date("2023-12-31"),
             by="day")
)

# merge full calendar with observed prices and convert to tsibble
aapl <- full_dates |>
  left_join(aapl_raw, by="Date") |>
  as_tsibble(index = Date)

# define the training dataset (pre-COVID period)
aapl_train <- aapl |> filter(Date < "2020-03-01")

# fit ARIMA model on training data
aapl_fit <- aapl_train |> model(ARIMA(Close))

# generate forecast for three years
aapl_fc <- aapl_fit |> forecast(h="3 years")

# plot forecast vs actual prices
aapl_fc |>
  autoplot(aapl) +
  labs(
    title="Apple (AAPL) — ARIMA Forecast vs Actual",
    subtitle="Model trained on data before March 2020",
    x="Date",
    y="Close Price (USD)"
  )

# Lufthansa forecast

# load packages
library(quantmod)
library(tsibble)
library(fable)
library(feasts)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)

# load data
getSymbols("LHA.DE", src="yahoo", from="2019-01-01", to="2023-12-31")

# convert Lufthansa xts object into a tidy data frame
lha_raw <- as.data.frame(LHA.DE) |>
  rownames_to_column("Date") |>
  mutate(Date = as.Date(Date),
         Close = as.numeric(LHA.DE[,4])) |>
  select(Date, Close)

# create a complete daily date sequence
full_dates <- tibble(
  Date = seq(as.Date("2019-01-01"),
             as.Date("2023-12-31"),
             by="day")
)

# merge full calendar with observed prices and convert to tsibble
lha <- full_dates |>
  left_join(lha_raw, by="Date") |>
  as_tsibble(index = Date)

# define the training dataset (pre-COVID period)
lha_train <- lha |> filter(Date < "2020-03-01")

# fit ARIMA model on training data
lha_fit <- lha_train |> model(ARIMA(Close))

# generate forecast for three years
lha_fc <- lha_fit |> forecast(h="3 years")

# plot forecast vs actual prices
lha_fc |>
  autoplot(lha) +
  labs(
    title="Lufthansa (LHA.DE) — ARIMA Forecast vs Actual",
    subtitle="Model trained on data before March 2020",
    x="Date",
    y="Close Price (EUR)"
  )

# BioNtech forecast

# load data
getSymbols("BNTX", src="yahoo", from="2019-01-01", to="2023-12-31")

# convert BioNTech xts object into a tidy data frame
bntx_raw <- as.data.frame(BNTX) |>
  rownames_to_column("Date") |>
  mutate(Date = as.Date(Date),
         Close = as.numeric(BNTX[,4])) |>
  select(Date, Close)

# create a complete daily date sequence
full_dates <- tibble(
  Date = seq(as.Date("2019-01-01"),
             as.Date("2023-12-31"),
             by="day")
)

# merge full calendar with observed prices and convert to tsibble
bntx <- full_dates |>
  left_join(bntx_raw, by="Date") |>
  as_tsibble(index = Date)

# define the training dataset (pre-COVID period)
bntx_train <- bntx |> filter(Date < "2020-03-01")

# fit ARIMA model on training data
bntx_fit <- bntx_train |> model(ARIMA(Close))

# generate forecast for three years
bntx_fc <- bntx_fit |> forecast(h="3 years")

# plot forecast vs actual prices
bntx_fc |>
  autoplot(bntx) +
  labs(
    title="BioNTech (BNTX) — ARIMA Forecast vs Actual",
    subtitle="Model trained on data before March 2020",
    x="Date",
    y="Close Price (USD)"
  )

# Shell forecast

# load data
getSymbols("SHEL", src="yahoo", from="2019-01-01", to="2023-12-31")

# convert Shell xts object into a tidy data frame
shel_raw <- as.data.frame(SHEL) |>
  rownames_to_column("Date") |>
  mutate(Date = as.Date(Date),
         Close = as.numeric(SHEL[,4])) |>
  select(Date, Close)

# create a complete daily date sequence
full_dates <- tibble(
  Date = seq(as.Date("2019-01-01"),
             as.Date("2023-12-31"),
             by="day")
)

# merge full calendar with observed prices and convert to tsibble
shel <- full_dates |>
  left_join(shel_raw, by="Date") |>
  as_tsibble(index = Date)

# define the training dataset (pre-COVID period)
shel_train <- shel |> filter(Date < "2020-03-01")

# fit ARIMA model on training data
shel_fit <- shel_train |> model(ARIMA(Close))

# generate forecast for three years
shel_fc <- shel_fit |> forecast(h="3 years")

# plot forecast vs actual prices
shel_fc |>
  autoplot(shel) +
  labs(
    title="Shell (SHEL) ARIMA Forecast vs Actual",
    subtitle="Model trained on data before March 2020",
    x="Date",
    y="Close Price (USD)"
  )




#continuous forecasts
library(dplyr)
library(tibble)
library(tidyr)
library(tsibble)
library(fable)
library(feasts)
library(ggplot2)

# Apple correct forecast
# Apple: Close
aapl_raw <- closing_table %>%
  transmute(Date = as.Date(Date), Close = Apple)

# Full daily calendar
aapl <- tibble(Date = seq(as.Date("2019-01-01"), as.Date("2023-12-31"), by = "day")) %>%
  left_join(aapl_raw, by = "Date") %>%
  as_tsibble(index = Date)

# ARIMA: train pre-COVID
aapl_fit <- aapl %>%
  filter(Date < as.Date("2020-03-01")) %>%
  model(arima = ARIMA(Close))

aapl_fc <- aapl_fit %>% forecast(h = "3 years")

# Forward fill NAs for visualization only
aapl_plot <- aapl %>%
  as_tibble() %>%
  arrange(Date) %>%
  fill(Close, .direction = "down") %>%
  as_tsibble(index = Date)

# Plot
aapl_fc %>%
  autoplot(aapl_plot) +
  labs(
    title = "Apple (AAPL) — ARIMA Forecast vs Actual",
    subtitle = "Model trained on data before March 2020",
    y = "Close Price (USD)",
    x = "Date"
  )

#Lufthansa correct forecast

# Lufthansa: Close 
lha_raw <- closing_table %>%
  transmute(Date = as.Date(Date), Close = Lufthansa)

# Full daily calendar 
lha <- tibble(Date = seq(as.Date("2019-01-01"), as.Date("2023-12-31"), by = "day")) %>%
  left_join(lha_raw, by = "Date") %>%
  as_tsibble(index = Date)

# ARIMA: train pre-COVID
lha_fit <- lha %>%
  filter(Date < as.Date("2020-03-01")) %>%
  model(arima = ARIMA(Close))

lha_fc <- lha_fit %>% forecast(h = "3 years")

# Forward fill NAs for visualization only
lha_plot <- lha %>%
  as_tibble() %>%
  arrange(Date) %>%
  fill(Close, .direction = "down") %>%
  as_tsibble(index = Date)

# Plot 
lha_fc %>%
  autoplot(lha_plot) +
  labs(
    title = "Lufthansa (LHA.DE) — ARIMA Forecast vs Actual",
    subtitle = "Model trained on data before March 2020",
    y = "Close Price (EUR)",
    x = "Date"
  )

# BioNTech correct forecast

# BioNTech: Close
bntx_raw <- closing_table %>%
  transmute(Date = as.Date(Date), Close = BioNTech)

# Full daily calendar
bntx <- tibble(Date = seq(as.Date("2019-01-01"), as.Date("2023-12-31"), by = "day")) %>%
  left_join(bntx_raw, by = "Date") %>%
  as_tsibble(index = Date)

# ARIMA: train pre-COVID
bntx_fit <- bntx %>%
  filter(Date < as.Date("2020-03-01")) %>%
  model(arima = ARIMA(Close))

bntx_fc <- bntx_fit %>% forecast(h = "3 years")

# Forward fill NAs for visualization only
bntx_plot <- bntx %>%
  as_tibble() %>%
  arrange(Date) %>%
  fill(Close, .direction = "down") %>%
  as_tsibble(index = Date)

# Plot
bntx_fc %>%
  autoplot(bntx_plot) +
  labs(
    title = "BioNTech (BNTX) — ARIMA Forecast vs Actual",
    subtitle = "Model trained on data before March 2020",
    y = "Close Price (USD)",
    x = "Date"
  )


# Shell correct forecast


# Shell: Close
shel_raw <- closing_table %>%
  transmute(Date = as.Date(Date), Close = Shell)

# Full daily calendar
shel <- tibble(Date = seq(as.Date("2019-01-01"), as.Date("2023-12-31"), by = "day")) %>%
  left_join(shel_raw, by = "Date") %>%
  as_tsibble(index = Date)

# ARIMA: train pre-COVID
shel_fit <- shel %>%
  filter(Date < as.Date("2020-03-01")) %>%
  model(arima = ARIMA(Close))

shel_fc <- shel_fit %>% forecast(h = "3 years")

# Forward fill NAs for visualization only
shel_plot <- shel %>%
  as_tibble() %>%
  arrange(Date) %>%
  fill(Close, .direction = "down") %>%
  as_tsibble(index = Date)

# Plot
shel_fc %>%
  autoplot(shel_plot) +
  labs(
    title = "Shell (SHEL) ARIMA Forecast vs Actual",
    subtitle = "Model trained on data before March 2020",
    y = "Close Price (USD)",
    x = "Date"
  )


#stationarity tests

#Stocks detailed ADF test
#load package
library(urca)

# ADF test on closing prices
adf_test <- function(x, name) {
  close_prices <- na.omit(as.numeric(Cl(x)))
  test <- ur.df(close_prices, type = "drift", selectlags = "AIC")
  cat("\nADF test for", name, "\n")
  print(summary(test))
}

#ADF tests
adf_test(AAPL,   "Apple")
adf_test(BNTX,   "BioNTech")
adf_test(`LHA.DE`, "Lufthansa")
adf_test(SHEL,   "Shell")

#simplified ADF test stocks

#load package
library(tseries)

#ADF tests
adf.test(na.omit(as.numeric(AAPL_cl)))
adf.test(na.omit(as.numeric(BNTX_cl)))
adf.test(na.omit(as.numeric(LHA_cl)))
adf.test(na.omit(as.numeric(SHEL_cl)))

#covid adf test

#load package
library(tseries)

#ADF test
adf.test(na.omit(world$confirmed))


#First differences

#load packages
library(dplyr)
library(tibble)
library(ggplot2)
# Create a function to plot first differences of closing prices
plot_first_diff_cl <- function(cl, name) {
  
  # Build a data frame with Date and Close values
  df_diff <- tibble(
    Date  = as.Date(time(cl)),
    Close = as.numeric(cl)
  ) %>%
    # Restrict the sample period to 2019–2023
    filter(Date >= as.Date("2019-01-01"),
           Date <= as.Date("2023-12-31")) %>%
    # Compute first differences 
    mutate(Diff = Close - lag(Close)) %>%
    # Remove missing values 
    filter(!is.na(Diff))
  
  # Plot the first differences over time
  ggplot(df_diff, aes(x = Date, y = Diff)) +
    geom_line(color = "black", linewidth = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste(name, "– First Differences"),
      subtitle = "Daily price changes",
      x = "Date",
      y = "First Difference"
    ) +
    theme_minimal()
}

# Plot first differences for each stock
plot_first_diff_cl(AAPL_cl, "Apple (AAPL)")
plot_first_diff_cl(BNTX_cl, "BioNTech (BNTX)")
plot_first_diff_cl(LHA_cl,  "Lufthansa (LHA.DE)")
plot_first_diff_cl(SHEL_cl, "Shell (SHEL)")

# log returns

# Stocks
library(tseries)

# Create a function to compute log returns and run an ADF test
get_log_returns <- function(cl, name) {
  # Compute log returns from closing prices
  r <- diff(log(na.omit(as.numeric(cl))))  # log returns
  
  # Run ADF test on log returns
  cat("\nADF Test for", name, "\n")
  print(adf.test(r))
  
  # Return log return series
  r
}

# Compute log returns for each stock
ret_AAPL <- get_log_returns(AAPL_cl, "Apple (AAPL)")
ret_BNTX <- get_log_returns(BNTX_cl, "BioNTech (BNTX)")
ret_LHA  <- get_log_returns(LHA_cl,  "Lufthansa (LHA.DE)")
ret_SHEL <- get_log_returns(SHEL_cl, "Shell (SHEL)")

# covid log returns
library(tseries)

# Create a function to compute a stationary COVID shock series and run an ADF test
get_covid_shock <- function(x, name = "COVID confirmed") {
  # Compute log-growth differences using log1p to handle zero values
  r <- diff(log1p(na.omit(x)))
  
  # Run ADF test on the transformed COVID series
  cat("\nADF Test for", name, "\n")
  print(adf.test(r))
  
  # Return COVID shock series
  r
}

# Compute COVID shock series from global confirmed cases
covid_shock <- get_covid_shock(world$confirmed, "COVID confirmed cases")


world$covid_shock <- c(NA, diff(log1p(world$confirmed)))
if (exists("covid_shock")) rm(covid_shock)



#Granger causality tests
#load packages
library(lmtest)
library(dplyr)
library(zoo)

# lufthansa Granger
# create Lufthansa data frame with Date and LogReturn
ret_LHA_df <- tibble(
  Date = as.Date(zoo::index(LHA_cl))[-1],
  LogReturn = as.numeric(ret_LHA)
) %>%
  # compute rolling volatility 
  mutate(Volatility = zoo::rollapply(LogReturn, 21, sd, fill = NA, align = "right"))

# merge Lufthansa volatility with COVID shock series by date
granger_vol_LHA <- ret_LHA_df %>%
  select(Date, Volatility) %>%
  left_join(
    world %>% select(date, covid_shock),
    by = c("Date" = "date")

  )%>%
  # remove missing values and sort chronologically
  na.omit() %>%
  arrange(Date)

# run Granger causality test 
grangertest(
  Volatility ~ covid_shock,
  order = 5,
  data = granger_vol_LHA
)

# Apple Granger
# create Apple data frame with Date and LogReturn
ret_AAPL_df <- tibble(
  Date = as.Date(zoo::index(AAPL_cl))[-1],
  LogReturn = as.numeric(ret_AAPL)
) %>%
  # compute rolling volatility 
  mutate(Volatility = zoo::rollapply(LogReturn, 21, sd, fill = NA, align = "right"))



# merge Apple volatility with COVID shock series by date
granger_vol_AAPL <- ret_AAPL_df %>%
  select(Date, Volatility) %>%
  left_join(
    world %>% select(date, covid_shock),
    by = c("Date" = "date")

   )%>%
  # remove missing values and sort chronologically
  na.omit() %>%
  arrange(Date)

# run Granger causality test 
grangertest(
  Volatility ~ covid_shock,
  order = 5,
  data = granger_vol_AAPL
)

# BioNTech Granger
# create BioNTech data frame with Date and LogReturn
ret_BNTX_df <- tibble(
  Date = as.Date(zoo::index(BNTX_cl))[-1],
  LogReturn = as.numeric(ret_BNTX)
) %>%
  # compute rolling volatility 
  mutate(Volatility = zoo::rollapply(LogReturn, 21, sd, fill = NA, align = "right"))

# merge BioNTech volatility with COVID shock series by date
granger_vol_BNTX <- ret_BNTX_df %>%
  select(Date, Volatility) %>%
  left_join(
    world %>% select(date, covid_shock),
    by = c("Date" = "date")
  ) %>%
  # remove missing values and sort chronologically
  na.omit() %>%
  arrange(Date)

# run Granger causality test
grangertest(
  Volatility ~ covid_shock,
  order = 5,
  data = granger_vol_BNTX
)

# Shell Granger
# create Shell data frame with Date and LogReturn
ret_SHEL_df <- tibble(
  Date = as.Date(zoo::index(SHEL_cl))[-1],
  LogReturn = as.numeric(ret_SHEL)
) %>%
  # compute rolling volatility 
  mutate(Volatility = zoo::rollapply(LogReturn, 21, sd, fill = NA, align = "right"))

# merge Shell volatility with COVID shock series by date
granger_vol_SHEL <- ret_SHEL_df %>%
  select(Date, Volatility) %>%
  left_join(
    world %>% select(date, covid_shock),
    by = c("Date" = "date")
 ) %>%
  # remove missing values and sort chronologically
  na.omit() %>%
  arrange(Date)

# run Granger causality test 
grangertest(
  Volatility ~ covid_shock,
  order = 5,
  data = granger_vol_SHEL
)

#regression
#load packages
library(dplyr)
library(lmtest)
library(sandwich)

# Lufthansa regression (Volatility explained by COVID shock)
model_LHA <- lm(Volatility ~ covid_shock, data = granger_vol_LHA)
summary(model_LHA)
coeftest(model_LHA, vcov = NeweyWest(model_LHA))

# Apple regression (Volatility explained by COVID shock)
model_AAPL <- lm(Volatility ~ covid_shock, data = granger_vol_AAPL)
summary(model_AAPL)
coeftest(model_AAPL, vcov = NeweyWest(model_AAPL))

# BioNTech regression (Volatility explained by COVID shock)
model_BNTX <- lm(Volatility ~ covid_shock, data = granger_vol_BNTX)
summary(model_BNTX)
coeftest(model_BNTX, vcov = NeweyWest(model_BNTX))

# Shell regression (Volatility explained by COVID shock)
model_SHEL <- lm(Volatility ~ covid_shock, data = granger_vol_SHEL)
summary(model_SHEL)
coeftest(model_SHEL, vcov = NeweyWest(model_SHEL))

