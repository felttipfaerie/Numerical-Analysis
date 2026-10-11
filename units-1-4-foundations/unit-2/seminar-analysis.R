# Coding-assistant note: OpenAI Codex assisted with coding. The project owner made the key decisions on how to segment and manage the data.
# Dataset note: Uses the COVID-19 India (Jan 20 - Mar 20) CSV at units-1-4-foundations/unit-2/covid-data-set/Covid19 India (Jan 20 - Mar 20).csv in this repository. Public COVID-19 India CSV archive: https://data.covid19india.org/documentation/csv/
setwd("~/GitHub/Numerical Analysis/units-1-4-foundations/unit-2")

library(readr)
library(dplyr)

# Load data
Covid19_India_Jan_20_Mar_20_ <- read_csv(
  "covid-data-set/Covid19 India (Jan 20 - Mar 20).csv"
)

d1 <- Covid19_India_Jan_20_Mar_20_


# Fix state / union territory names
d1_clean <- d1 %>%
  mutate(
    `State/UnionTerritory` = case_when(
      `State/UnionTerritory` == "Chattisgarh" ~ "Chhattisgarh",
      `State/UnionTerritory` == "Pondicherry" ~ "Puducherry",
      `State/UnionTerritory` == "Telengana" ~ "Telangana",
      `State/UnionTerritory` == "Union Territory of Chandigarh" ~ "Chandigarh",
      `State/UnionTerritory` == "Union Territory of Jammu and Kashmir" ~ "Jammu and Kashmir",
      `State/UnionTerritory` == "Union Territory of Ladakh" ~ "Ladakh",
      TRUE ~ `State/UnionTerritory`
    )
  )


# Combine data by state / union territory
combined_data <- d1_clean %>%
  select(-Sno) %>%
  group_by(`State/UnionTerritory`) %>%
  summarise(
    across(
      where(is.numeric),
      \(x) sum(x, na.rm = TRUE)
    )
  )


# Histogram: Confirmed Indian Nationals by State
hist(
  combined_data$ConfirmedIndianNational,
  breaks = seq(
    0,
    max(combined_data$ConfirmedIndianNational, na.rm = TRUE) + 25,
    by = 25
  ),
  probability = TRUE,
  main = "Confirmed Indian National by State",
  xlab = "Confirmed Indian Nationals"
)

lines(
  density(
    combined_data$ConfirmedIndianNational,
    na.rm = TRUE
  ),
  lwd = 2
)


# Histogram: Confirmed Foreign Nationals by State
hist(
  combined_data$ConfirmedForeignNational,
  breaks = seq(
    0,
    max(combined_data$ConfirmedForeignNational, na.rm = TRUE) + 25,
    by = 25
  ),
  probability = TRUE,
  main = "Confirmed Foreign National by State",
  xlab = "Confirmed Foreign Nationals"
)

lines(
  density(
    combined_data$ConfirmedForeignNational,
    na.rm = TRUE
  ),
  lwd = 2
)

#histopgram and density curves week by week
# Convert Date column
d1$Date <- as.Date(d1$Date, format = "%d-%m-%Y")

# Keep Jan to end of March 2020
d1_range <- subset(
  d1,
  Date >= as.Date("2020-01-01") &
    Date <= as.Date("2020-03-31")
)

# Repeat dates based on confirmed counts
indian_dates <- rep(
  d1_range$Date,
  times = d1_range$ConfirmedIndianNational
)

foreign_dates <- rep(
  d1_range$Date,
  times = d1_range$ConfirmedForeignNational
)

# Convert dates to numbers for plotting
indian_num <- as.numeric(indian_dates)
foreign_num <- as.numeric(foreign_dates)

# Weekly breaks
week_dates <- seq(
  as.Date("2020-01-22"),
  as.Date("2020-03-25"),
  by = "7 days"
)

# Numeric version of weekly breaks
week_breaks <- as.numeric(week_dates)

# Calculate histograms without plotting
hist_indian <- hist(
  indian_num,
  breaks = week_breaks,
  probability = TRUE,
  plot = FALSE
)

hist_foreign <- hist(
  foreign_num,
  breaks = week_breaks,
  probability = TRUE,
  plot = FALSE
)

# Density curves
density_indian <- density(indian_num)
density_foreign <- density(foreign_num)

# Find maximum height
max_y <- max(
  hist_indian$density,
  hist_foreign$density,
  density_indian$y,
  density_foreign$y
)

# Plot Indian histogram
hist(
  indian_num,
  breaks = week_breaks,
  probability = TRUE,
  col = rgb(0.2, 0.4, 0.8, 0.4),
  border = "blue",
  main = "Confirmed Cases by Week",
  xlab = "Week",
  ylab = "Density",
  xaxt = "n",
  ylim = c(0, max_y * 1.1),
  cex.main = 0.9,
  cex.lab = 0.8,
  cex.axis = 0.7
)

# Overlay foreign histogram
hist(
  foreign_num,
  breaks = week_breaks,
  probability = TRUE,
  col = rgb(0.8, 0.3, 0.2, 0.4),
  border = "red",
  add = TRUE
)

# Indian density curve
lines(
  density_indian,
  col = "blue",
  lwd = 2
)

# Foreign density curve
lines(
  density_foreign,
  col = "red",
  lwd = 2
)

# Add ONE custom weekly date axis
axis(
  1,
  at = week_breaks,
  labels = format(week_dates, "%d-%b"),
  las = 2,
  cex.axis = 0.7
)

# Legend
legend(
  "topleft",
  legend = c(
    "Indian National",
    "Foreign National"
  ),
  fill = c(
    rgb(0.2, 0.4, 0.8, 0.4),
    rgb(0.8, 0.3, 0.2, 0.4)
  ),
  border = c("blue", "red"),
  cex = 0.7
)


#violin and box plot of new cases by region

library(dplyr)
library(ggplot2)

top_states <- d1_clean %>%
  count(`State/UnionTerritory`) %>%
  arrange(desc(n)) %>%
  slice_head(n = 8) %>%
  pull(`State/UnionTerritory`)

plot_data <- d1_clean %>%
  filter(
    `State/UnionTerritory` %in% top_states
  )

ggplot(
  plot_data,
  aes(
    x = `State/UnionTerritory`,
    y = ConfirmedIndianNational
  )
) +
  geom_violin(trim = FALSE) +
  labs(
    title = "Distribution of Daily New COVID-19 Cases for Indian Nationals by State",
    x = "State / Union Territory",
    y = "Daily New Cases"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggplot(
  plot_data,
  aes(
    x = `State/UnionTerritory`,
    y = ConfirmedIndianNational
  )
) +
  geom_boxplot() +
  labs(
    title = "Daily New COVID-19 Cases for Indian Nationals by State",
    x = "State / Union Territory",
    y = "Daily New Cases"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

#box plot for average number of cases per day  for ConfirmedIndianNational and ConfirmedForeignNational combined

library(dplyr)
library(ggplot2)

d1$Date <- as.Date(
  d1$Date,
  format = "%d-%m-%Y"
)

daily_totals <- d1 %>%
  mutate(
    TotalNewCases =
      ConfirmedIndianNational +
      ConfirmedForeignNational
  ) %>%
  group_by(Date) %>%
  summarise(
    TotalNewCases = sum(TotalNewCases),
    .groups = "drop"
  )

ggplot(
  daily_totals,
  aes(
    x = "",
    y = TotalNewCases
  )
) +
  geom_boxplot(
    width = 0.3
  ) +
  labs(
    title = "Daily New Confirmed COVID-19 Cases in India",
    x = NULL,
    y = "New Confirmed Cases per Day"
  ) +
  theme_minimal()
