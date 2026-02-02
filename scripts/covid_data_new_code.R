install.packages(c("tidyverse", "lubridate", "zoo"))
# Pakete laden
library(tidyverse)
library(lubridate)

# Johns-Hopkins COVID-19 Daten laden (confirmed & deaths)
confirmedraw <- read.csv(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
)

deathsraw <- read.csv(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
)

# Confirmed Cases: Wide → Long und Aggregation auf Länderebene
confirmed <- confirmedraw %>%
  pivot_longer(
    cols = -c(Country.Region, Province.State, Lat, Long),
    names_to = "date",
    values_to = "confirmed"
  ) %>%
  group_by(Country.Region, date) %>%
  summarize(confirmed = sum(confirmed), .groups = "drop")

# Deaths: Wide → Long und Aggregation auf Länderebene
deaths <- deathsraw %>%
  pivot_longer(
    cols = -c(Country.Region, Province.State, Lat, Long),
    names_to = "date",
    values_to = "deaths"
  ) %>%
  group_by(Country.Region, date) %>%
  summarize(deaths = sum(deaths), .groups = "drop")

# Zusammenführen von confirmed und deaths
country <- full_join(confirmed, deaths, by = c("Country.Region", "date")) %>%
  mutate(
    date = gsub("^X", "", date),        # führendes 'X' entfernen
    date = gsub("\\.", "/", date),      # Punkte durch '/' ersetzen
    date = mdy(date)                    # Datum korrekt parsen
  )

# Weltweite Aggregation pro Tag
world <- country %>%
  group_by(date) %>%
  summarize(
    confirmed = sum(confirmed, na.rm = TRUE),
    deaths    = sum(deaths, na.rm = TRUE),
    .groups = "drop"
  )
# Zeitreihenplot der weltweiten bestätigten Fälle
ggplot(world, aes(x = date, y = confirmed)) +
  geom_line() +
  theme_classic() +
  labs(
    title = "Global Confirmed COVID-19 Cases",
    x = "Date",
    y = "Confirmed cases"
  )
# Berechnung der täglich neuen bestätigten Fälle
world <- world %>%
  arrange(date) %>%                       # sicherstellen, dass Daten sortiert sind
  mutate(
    new_confirmed = confirmed - lag(confirmed), # Tagesdifferenz
    new_confirmed = replace_na(new_confirmed, 0),# NA in der ersten Beobachtung
    new_confirmed = pmax(new_confirmed, 0)       # negative Korrekturen entfernen
  )
# Paket für gleitenden Durchschnitt
library(zoo)

# 7-Tage-Durchschnitt und Log-Transformation
world_plot <- world %>%
  mutate(
    new_confirmed_ma7 = rollmean(new_confirmed, 7, fill = NA, align = "right"),
    log_new_confirmed_ma7 = log(new_confirmed_ma7 + 1)
  ) %>%
  filter(date >= as.Date("2020-02-01"))    # Fokus auf relevante Pandemiephase

# Zeitreihenplot der logarithmierten täglichen Neuinfektionen
ggplot(world_plot, aes(x = date, y = log_new_confirmed_ma7)) +
  geom_line() +
  theme_classic() +
  labs(
    title = "Log Daily New Confirmed COVID-19 Cases Worldwide (7-day average)",
    x = "Date",
    y = "log(daily new confirmed cases + 1)"
  )
