# Coding-assistant note: OpenAI Codex assisted with coding. The project owner made the key decisions on how to segment and manage the data.
# Dataset note: Uses the COVID-19 India (Jan 20 - Mar 20) CSV at units-1-4-foundations/unit-2/covid-data-set/Covid19 India (Jan 20 - Mar 20).csv in this repository. Public COVID-19 India CSV archive: https://data.covid19india.org/documentation/csv/
library(readr)
Covid19_India_Jan_20_Mar_20_ <- read_csv("units-1-4-foundations/unit-2/covid-data-set/Covid19 India (Jan 20 - Mar 20).csv")
View(Covid19_India_Jan_20_Mar_20_)
d1 = Covid19_India_Jan_20_Mar_20_

dr <- d1 %>%
     mutate(has_death = Deaths > 0)


# has death true/false
table(dr$has_death)

dr$has_death <- factor(
  dr$has_death,
  levels = c(FALSE, TRUE),
  labels = c("No Deaths", "Deaths Reported")
)

# combining data nd converting to catagorical
cases <- d1$ConfirmedIndianNational + d1$ConfirmedForeignNational

dt <- d1 %>%
  mutate(
    case_level = case_when(
      cases == 0 ~ "No Cases",
      cases >= 1 & cases <= 5 ~ "Low Cases",
      cases >= 6 & cases <= 15 ~ "Medium Cases",
      cases >= 16 ~ "High Cases"
    )
  )

dt$case_level <- factor(
  dt$case_level,
  levels = c("No Cases", "Low Cases", "Medium Cases", "High Cases")
)

# state tables
table(d1$`State/UnionTerritory`)

# find highest
state_counts <- sort(
  table(d1$`State/UnionTerritory`),
  decreasing = TRUE
)

top10_states <- head(state_counts, 10)

top10_states
