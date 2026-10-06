library(readr)
library(dplyr)

Covid19_India_Jan_20_Mar_20_ <- read_csv("~/GitHub/Numerical Analysis/unit-9/covid-data-set/Covid19 India (Jan 20 - Mar 20).csv")

df <- Covid19_India_Jan_20_Mar_20_

table(df[["State/UnionTerritory"]])


#clean duplicate names
library(dplyr)

df_clean_states <- df %>%
  rename(State = `State/UnionTerritory`) %>%
  mutate(
    State = case_when(
      State == "Chattisgarh" ~ "Chhattisgarh",
      State == "Pondicherry" ~ "Puducherry",
      State == "Telengana" ~ "Telangana",
      State == "Union Territory of Chandigarh" ~ "Chandigarh",
      State == "Union Territory of Jammu and Kashmir" ~ "Jammu and Kashmir",
      State == "Union Territory of Ladakh" ~ "Ladakh",
      TRUE ~ State
    )
  )

#check merge
table(df_clean_states[["State"]])
length(unique(df_clean_states$State))

#probability that report comes from given state
state_percent <- round(prop.table(table(df_clean_states$State)) * 100, 2)

data.frame(
  State = names(state_percent),
  Percentage = paste0(as.numeric(state_percent), "%")
)
#Kerala     19.26%
#Delhi      7.41%


#probability a report shows recoverie
cured_group <- ifelse(df_clean_states$Cured == 0, "Cured = 0", "Cured >= 1")

round(prop.table(table(cured_group)) * 100, 2)

#20.37 % change a randomly selected sample has a reported recovery

#chance a given state shows a cured/recovery
state_cured_prob <- df_clean_states %>%
  group_by(State) %>%
  summarise(
    Total = n(),
    Cured_1_or_more = sum(Cured >= 1, na.rm = TRUE),
    Probability = Cured_1_or_more / Total,
    Percentage = round(Probability * 100, 2)
  )

state_cured_prob

#simple bar plot
library(dplyr)
library(ggplot2)

state_recovery <- df_clean_states %>%
  mutate(
    Recovery = ifelse(Cured >= 1, "Cured >= 1", "Cured = 0")
  ) %>%
  count(State, Recovery) %>%
  group_by(State) %>%
  mutate(
    Proportion = n / sum(n)
  )

ggplot(
  state_cured_prob,
  aes(x = Percentage, y = reorder(State, Percentage))
) +
  geom_point(size = 3) +
  labs(
    title = "Probability of Recovery by State",
    x = "Reports with Cured >= 1 (%)",
    y = "State"
  ) +
  xlim(0, 100) +
  theme_minimal()

#Kerala 36.5%
#Delhi 40%

#bayse theorm
# P(Kerala)
p_kerala <- mean(df_clean_states$State == "Kerala")

# P(Recovery | Kerala)
p_recovery_given_kerala <- mean(
  df_clean_states$Cured[df_clean_states$State == "Kerala"] >= 1,
  na.rm = TRUE
)

# P(Recovery)
p_recovery <- mean(df_clean_states$Cured >= 1, na.rm = TRUE)

# Bayes theorem
p_kerala_given_recovery <-
  (p_recovery_given_kerala * p_kerala) / p_recovery

round(p_kerala_given_recovery * 100, 2)
# Kerala? P(Kerala | Recovery) = 34.55% 


# alternative method
round(
  mean(df_clean_states$State[df_clean_states$Cured >= 1] == "Kerala") * 100,
  2
)
