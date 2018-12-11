library(tidyverse)
library(scales)
library(janitor)
library(readxl)

# Reading in the maternity leave data from the UNDP

paid_leave <- read_csv("Mandatory paid maternity leave (days).csv", skip = 1) %>% 
  select(Country, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`)

# I only want countries in the EU and the US so I have added an excel that
# has the 29 countries with their 2 and 3 digit codes. I changed the country names
# for the UK and the US because the long version in the countries excel did not
# match with the names in other data sets when I tried merging them

countries <- read_excel("country_codes.xlsx") %>% 
  mutate(name = str_replace_all(name, "United Kingdom of Great Britain and Northern Ireland", 
                                "United Kingdom"),
         name = str_replace_all(name, "United States of America", "United States"))

# Filtering for the countries I want and gathering the year and value variables so 
# that years are no longer separate columns. I also joined the paid leave data
# with the countries data in order to add a country code column so that I can eventually 
# merge it with the other spreads. I also had to convert the Year column into numeric 
# so it would match the other spreads

paid_leave <- paid_leave %>% 
  subset(Country %in% countries$name) %>% 
  gather(key = Year, value = mandatory_leave_days, `2010`:`2017`) %>% 
  left_join(countries, by = c("Country" = "name")) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  select(-`alpha-2`)

# Reading in the social spending data from the OECD

family_exp <- read_csv("Social Spending on Families - OECD.csv") %>% 
  select(COU, Country, Indicator, Year, Value)

# Filtering for my countries, years after 2008, the right indicator, and spreading the data.
# Note: this data is only available for 2009 - 2013

family_exp <- family_exp %>% 
  subset(COU %in% countries$`alpha-3`) %>% 
  filter(Year >= 2008,
         Indicator == "Total public social expenditure on families as a % of GDP") %>% 
  spread(key = Indicator, value = Value) %>% 
  rename(social_exp = "Total public social expenditure on families as a % of GDP")

# Reading in the female labor force participation data from the World Bank

female_lfpr <- read_excel("Female LFPR - World Bank.xls") %>% 
  clean_names() %>% 
  select(-series_code)

# Filtering for my countries, and gathering the year and value variables so 
# that years are no longer separate columns. Finally, I removed the characters
# from the year column

female_lfpr <- female_lfpr %>% 
  subset(country_code %in% countries$`alpha-3`) %>% 
  gather(key = Year, value = lfpr_female, x2008_yr2008:x2017_yr2017) %>% 
  select(-series_name) %>% 
  mutate(Year = parse_number(Year))

# Reading in service sector employment data from the World Bank

services_employ <- read_excel("Service Sector Employment - World Bank.xls") %>% 
  clean_names() %>% 
  select(-series_code)

# Repeat steps as above

services_employ <- services_employ %>% 
  subset(country_code %in% countries$`alpha-3`) %>% 
  gather(key = Year, value = employment_services, x2008_yr2008:x2017_yr2017) %>% 
  select(-series_name) %>% 
  mutate(Year = parse_number(Year))

# Reading in female tertiary enrollment rates from UNESCO

tertiary_female <- read_excel("Tertiary Enrollment - UNESCO.xls") %>% 
  select(-EDULIT_IND, -TIME, -Flags, -`Flag Codes`)

# Filtering for the countries and organizing variable names

tertiary_female <- tertiary_female %>% 
  subset(LOCATION %in% countries$`alpha-3`) %>% 
  rename(COU = LOCATION, Year = Time, per_female_tertiary = Value) %>% 
  select(-Indicator)

# Reading in maternal leave indicator data from the World Bank

leave_guarantee <- read_excel("Maternal Leave - World Bank.xls") %>% 
  clean_names() %>% 
  select(-series_code)

# Repeat steps as above

leave_guarantee <- leave_guarantee %>% 
  subset(country_code %in% countries$`alpha-3`) %>% 
  gather(key = Year, value = position_guarantee, x2008_yr2008:x2017_yr2017) %>% 
  select(-series_name) %>% 
  mutate(Year = parse_number(Year),
         position_guarantee = str_replace_all(position_guarantee, 
                                              c("1" = "Yes", "0" = "No")))

# Reading in the OECD Employment Protection Legislation Index data

epl <- read_csv("EPL Index - OECD.csv") %>% 
  select(COUNTRY, Country, Series, SERIES, Time, Value)

# Filter for my countries as well as Version 3 of the indicator. I have 
# chosen Version 3 because it is the most recent calculation method of the 
# index and is more comprehensive than versions 1 or 2 because it incorporates
# more data items relating to regulations on collective dismissals

epl <- epl %>% 
  subset(COUNTRY %in% countries$`alpha-3`) %>% 
  filter(SERIES == "EPRC_V3") %>% 
  rename(epl_score = Value, Year = Time, COU = COUNTRY) %>% 
  select(-SERIES, - Series)

# Reading in women's representation in government data from UNDP

female_govt <- read_csv("Share of seats in parliament (% held by women).csv",
                        skip = 1) %>% 
  select(Country, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`)

# Filtering for the countries I want and gathering the year and value variables so 
# that years are no longer separate columns. I also joined the female government data
# with the countries data in order to add a country code column so that I can eventually 
# merge it with the other spreads

female_govt <- female_govt %>% 
  subset(Country %in% countries$name) %>% 
  gather(key = Year, value = female_politicians, `2010`:`2017`) %>% 
  left_join(countries, by = c("Country" = "name")) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  select(-`alpha-2`)

# Joining the separate spreads

indicators <- female_lfpr %>% 
  left_join(family_exp, by = c("country_code" = "COU", "Year")) %>% 
  left_join(tertiary_female, by = c("country_code" = "COU", "Year")) %>% 
  left_join(leave_guarantee, by = c("country_code", "Year")) %>% 
  left_join(paid_leave, by = c("country_code" = "alpha-3", "Year")) %>% 
  left_join(services_employ, by = c("country_code", "Year")) %>% 
  left_join(epl, by = c("country_code" = "COU", "Year")) %>% 
  left_join(female_govt, by = c("country_code" = "alpha-3", "Year")) %>% 
  select(Year, country_code, country_name, everything(), -country_name.x, -Country.x, -Country.y,
         -country_name.y, -Country.x.x, -Country.y.y, -Country)

# Create file and send to main folder

directory <- "/Users/sydneysteel/Thesis"

write.csv(indicators, file = file.path(directory, "indicators_data.csv"))

write.csv(countries, file = file.path(directory, "country_codes.csv"))


