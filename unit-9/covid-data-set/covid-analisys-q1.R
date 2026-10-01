# Coding-assistant note: OpenAI Codex assisted with coding. The project owner made the key decisions on how to segment and manage the data.
# Dataset note: Uses the COVID-19 India CSV and India boundary shapefile in unit-9/covid-data-set/. Keep the .csv, .shp, .dbf, .prj, .shx, and .cpg files together. Public COVID-19 India CSV archive: https://data.covid19india.org/documentation/csv/; public replacement boundary data: https://gadm.org/download_country.html
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)


Covid19_India_Jan_20_Mar_20_ <- read_csv("unit-9/covid-data-set/Covid19 India (Jan 20 - Mar 20).csv")
View(Covid19_India_Jan_20_Mar_20_)

df <- Covid19_India_Jan_20_Mar_20_
table(df$variable1, df$variable2)

#find all teratories
unique(df$`State/UnionTerritory`)

#combine antional and non-nationals
confirmed_df <- data.frame(
  State = df[["State/UnionTerritory"]],
  ConfirmedCases = df[["ConfirmedIndianNational"]] +
    df[["ConfirmedForeignNational"]]
)


#split reports into serverity level
confirmed_df$Severity <- cut(
  confirmed_df$ConfirmedCases,
  breaks = c(0, 1, 3, 9, Inf),
  labels = c("Low", "Moderate", "High", "Severe"),
  include.lowest = TRUE
)


#fix the duplicate names
satenames_df <- confirmed_df %>%
  mutate(
    State = case_when(
      State == "Union Territory of Chandigarh" ~ "Chandigarh",
      State == "Chattisgarh" ~ "Chhattisgarh",
      State == "Pondicherry" ~ "Puducherry",
      State == "Union Territory of Ladakh" ~ "Ladakh",
      State == "Union Territory of Jammu and Kashmir" ~ "Jammu and Kashmir",
      State == "Telengana" ~ "Telangana",
      TRUE ~ State
    )
  )

#combine data to regions
state_summary <- satenames_df %>%
  count(State, Severity) %>%
  pivot_wider(
    names_from = Severity,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    TotalRecords = Low + Moderate + High + Severe
  )


#cross table
cross_table <- state_summary %>%
  select(State, Low, Moderate, High, Severe)

cross_table

cross_table <- as.data.frame(state_summary)

rownames(cross_table) <- cross_table$State
cross_table$State <- NULL

cross_table


#plot graph options
plot_data <- state_summary %>%
  select(State, Low, Moderate, High, Severe) %>%
  pivot_longer(
    cols = c(Low, Moderate, High, Severe),
    names_to = "Severity",
    values_to = "Count"
  )

ggplot(plot_data, aes(x = State, y = Count, fill = Severity)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Case Severity Levels by State",
    x = "State",
    y = "Number of Records",
    fill = "Severity"
  ) +
  theme_minimal()

#use shp file
india_map <- st_read("india-polygon.shp")
map_data <- india_map %>%
  left_join(
    state_summary,
    by = c("st_nm" = "State")
  )
library(ggplot2)

#servier case map plot
ggplot(map_data) +
  geom_sf(aes(fill = Severe)) +
  labs(
    title = "Severe Case Records by State",
    fill = "Severe"
  ) +
  theme_minimal()

#servier case map plot
ggplot(map_data) +
  geom_sf(aes(fill = Severe)) +
  scale_fill_gradient(
    low = "white",
    high = "darkred"
  ) +
  labs(
    title = "Severe Case Records by State",
    fill = "Severe"
  ) +
  theme_minimal()

# total records per state
ggplot(map_data) +
  geom_sf(
    aes(fill = TotalRecords),
    colour = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    na.value = "grey90"
  ) +
  labs(
    title = "Total COVID-19 Records by State",
    subtitle = "India, January–March 2020",
    fill = "Records"s
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 10,
      colour = "grey40"
    ),
    legend.title = element_text(
      face = "bold"
    ),
    legend.position = "right"
  )
