library(tidyverse)

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



