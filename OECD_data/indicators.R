library(tidyverse)

# Reading in the childcare and maternity leave data from the OECD

paid_leave <- read_csv("Childcare and Maternity Leave - OECD.csv") %>% 
  select(COU, Country, Indicator, Year, Value)

# I only want countries in the EU and the US so I will filter out
# the others and save that as an object that I will use for tidying
# the other OECD and World Bank datasets

paid_leave <- paid_leave %>% 
  filter(!COU %in% c("ISL", "NZL", "AUS", "LUX", "TUR", "JPN", "MEX", "KOR", 
                     "CHL", "ISR", "CAN", "CRI", "BRA", "COL", "IDN", "RUS", "ZAF"))
countries <- paid_leave %>% 
  select(COU, Country) %>% 
  unique()

# For this data I am only interested in the weeks of paid leave guaranteed 
# for each country so I will filter out the other indicators. I also filtered 
# the date range to only include years after 2008. Then, I spread the data so it is tidier

paid_leave <- paid_leave %>% 
  filter(Indicator == "Length of paid maternity and parental leave available to mothers in weeks",
         Year >= 2008) %>% 
  spread(key = Indicator, value = Value) %>% 
  rename(weeks_leave = "Length of paid maternity and parental leave available to mothers in weeks")

# Reading in the social spending data from the OECD

family_exp <- read_csv("Social Spending on Families - OECD.csv") %>% 
  select(COU, Country, Indicator, Year, Value)

# Filtering for my countries, years after 2008, the right indicator, and spreading the data.
# Note: this data is only available for 2009 - 2013

family_exp <- family_exp %>% 
  subset(COU %in% countries$COU) %>% 
  filter(Year >= 2008,
         Indicator == "Total public social expenditure on families as a % of GDP") %>% 
  spread(key = Indicator, value = Value) %>% 
  rename(social_exp = "Total public social expenditure on families as a % of GDP")

# Reading in the female labor force participation data from the World Bank

female_lfpr <- read_csv("Female LFPR - World Bank.csv") %>% 
  select(-`Series Code`)

# Filtering for my countries, and gathering the year and value variables so 
# that years are no longer separate columns. Finally, I removed the characters
# from the year column

female_lfpr <- female_lfpr %>% 
  subset(`Country Code` %in% countries$COU) %>% 
  gather(key = Year, value = lfpr_female, `2008 [YR2008]`:`2017 [YR2017]`) %>% 
  select(-`Series Name`) %>% 
  mutate(Year = parse_number(Year))

# Reading in service sector employment data from the World Bank

services_employ <- read_csv("Service Sector Employment - World Bank.csv") %>% 
  select(-`Series Code`)

# Repeat steps as above

services_employ <- services_employ %>% 
  subset(`Country Code` %in% countries$COU) %>% 
  gather(key = Year, value = employment_services, `2008 [YR2008]`:`2017 [YR2017]`) %>% 
  select(-`Series Name`) %>% 
  mutate(Year = parse_number(Year))

# Reading in female tertiary enrollment rates from the World Bank

tertiary_female <- read_csv("Female Tertiary Enrollment - World Bank.csv",
                            na = c("..", "NA")) %>% 
  select(-`Series Code`)

# Repeat steps as above

tertiary_female <- tertiary_female %>% 
  subset(`Country Code` %in% countries$COU) %>% 
  gather(key = Year, value = female_tertiary_enroll, `2008 [YR2008]`:`2017 [YR2017]`) %>% 
  select(-`Series Name`) %>% 
  mutate(Year = parse_number(Year))

# Reading in maternal leave indicator data from the World Bank

leave_guarantee <- read_csv("Maternal Leave - World Bank.csv") %>% 
  select(-`Series Code`)

# Repeat steps as above

leave_guarantee <- leave_guarantee %>% 
  subset(`Country Code` %in% countries$COU) %>% 
  gather(key = Year, value = position_guarantee, `2008 [YR2008]`:`2017 [YR2017]`) %>% 
  select(-`Series Name`) %>% 
  mutate(Year = parse_number(Year),
         position_guarantee = str_replace_all(position_guarantee, 
                                              c("1" = "Yes", "0" = "No", ".." = "No leave available")))

# Joining the separate spreads

indicators <- paid_leave %>% 
  left_join(family_exp, by = c("COU", "Country", "Year")) %>% 
  left_join(female_lfpr, by = c("Country" = "Country Name", "COU" = "Country Code", "Year")) %>% 
  left_join(services_employ, by = c("Country" = "Country Name", "COU" = "Country Code", "Year")) %>% 
  left_join(tertiary_female, by = c("Country" = "Country Name", "COU" = "Country Code", "Year")) %>% 
  left_join(leave_guarantee, by = c("Country" = "Country Name", "COU" = "Country Code", "Year"))

