# Coding-assistant note: OpenAI Codex assisted with coding. The project owner made the key decisions on how to segment and manage the data.
# Dataset note: Uses the COVID-19 India (Jan 20 - Mar 20) CSV at unit-2/covid-data-set/Covid19 India (Jan 20 - Mar 20).csv in this repository. Public COVID-19 India CSV archive: https://data.covid19india.org/documentation/csv/
library(readr)
Covid19_India_Jan_20_Mar_20_ <- read_csv("covid-data-set/Covid19 India (Jan 20 - Mar 20).csv")
View(Covid19_India_Jan_20_Mar_20_)

d1=Covid19_India_Jan_20_Mar_20_

table(d1$Cured)
table(d1$Deaths)

recovery <- sum(d1$Cured > 0)
total_rows <- nrow(d1)

#persentage of cases with recovery
cpr <- (recovery / total_rows) * 100

#new dataset with has a recovery?
dr <- d1 %>%
  mutate(has_recovery = Cured > 0)

table(dr$has_recovery)

#alternative recovery persentage
hr <- sum(dr$has_recovery, na.rm = TRUE) /
  nrow(dr) * 100

hr
