library(tidyverse)
library(scales)

# Reading in the tidy data from the US, EU, and OECD / World Bank
# All three data sets gave a warning error of "missing column names 
# filled in". I could not figure out what the error was so I just 
# removed the added X1 column

US <- read_csv("US_data.csv") %>% 
  select(-X1)

EU <- read_csv("EU_data.csv") %>% 
  select(-X1)

indicators <- read_csv("indicators_data.csv") %>% 
  select(-X1)

# Reorder and rename the US variables so I can bind the US and EU data. I also
# dropped the avg_firmsize variable from the EU data

US <- US %>% 
  select(year, country, female_managers, male_managers, female_hours, male_hours,
         female_pt, male_pt)

EU <- EU %>% 
  select(-avg_firmsize)

# Combining the EU and US labor force survey data

managers <- bind_rows(US, EU)

# The country code for the UK is incorrect so I changed it. I also removed Norway 
# and Switzerland from the managers data as they are not in the EU

managers <- managers %>% 
  mutate(country = str_replace_all(country, "UK", "GB")) %>% 
  filter(!country %in% c("CH", "NO"))

# The EU and US data only has 2 digit country codes so I am going to add a country name
# variable so that I can combine these two data sets with the indicators data

countries <- read_csv("country_codes.csv") %>% 
  select(-X1)

managers1 <- managers %>% 
  left_join(countries, by = c("country" = "alpha-2")) %>% 
  left_join(indicators, by = c("alpha-3" = "country_code", "year" = "Year"))

# Re-organizing and tidying the column names

managers1 <- managers1 %>% 
  select(year, country, name, everything(), -country_name, -`alpha-3`) %>% 
  rename(country_code = country, country_name = name)

managers2 <- managers1 %>% 
  mutate(female_managers = percent(female_managers, accuracy = .1),
         male_managers = percent(male_managers, accuracy = .1),
         female_hours = round(female_hours),
         male_hours = round(male_hours),
         lfpr_female = percent(lfpr_female / 100, accuracy = .1),
         social_exp = percent(social_exp / 100, accuracy = .1),
         per_female_tertiary = percent(per_female_tertiary / 100, accuracy = .1),
         employment_services = percent(employment_services / 100, accuracy = .1),
         epl_score = round(epl_score, digits = 2),
         mandatory_leave_days = round(mandatory_leave_days),
         female_politicians = percent(female_politicians / 100, accuracy = .1))

# Converting the columns into percentages turned the NA values into percents, so 
# I am undoing that

managers2[managers2 == "NA%"] <- NA_character_




