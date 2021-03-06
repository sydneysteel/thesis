library(tidyverse)
library(ipumsr)
library(readxl)
library(janitor)
library(scales)

# Reading in the IPUMS data using ipumsr package

cps_ddi <- read_ipums_ddi("cps_00002.xml")
cps_messy <- read_ipums_micro(cps_ddi, verbose = FALSE, var_attrs = NULL)

# Adding zeros to the front of occupational codes that they were eliminated from in data read-in 
# to make all codes 4-digits. I also needed to remove all observations with NA value for ASECWT as 
# I can only use data with a final weight for the Annual Social and Economic Census to compute national estimates

cps <- cps_messy %>% 
  select(YEAR, SEX, OCC10LY, UHRSWORKLY, FULLPART, FIRMSIZE, ASECWT) %>% 
  mutate(digits = str_length(OCC10LY),
         OCC10LY = as.character(OCC10LY)) %>% 
  mutate(code = case_when(
    digits == 2 ~ paste("00", OCC10LY, sep = ""),
    digits == 3 ~ paste("0", OCC10LY, sep = ""),
    is.na(OCC10LY) ~ NA_character_,
    TRUE ~ OCC10LY
  )) %>% 
  filter(!is.na(ASECWT))

# I now need to add the code categories to the data set. I start by making a separate
# data frame which contains all occupational code values and their corresponding labels

occupations <- ipums_val_labels(cps_ddi, OCC10LY)

# I need to add the zeros back to the occupational codes which were removed during
# data read-in

occupations1 <- occupations %>%
  mutate(digits = str_length(val),
         val = as.character(val)) %>% 
  mutate(code = case_when(
    digits == 2 ~ paste("00", val, sep = ""),
    digits == 3 ~ paste("0", val, sep = ""), 
    TRUE ~ val
  )) %>% 
  select(lbl, code)

# The ASEC also uses census codes for occupational data and not SOC codes (which I
# need because they are compatible with ISCO codes for the EU data). I will need to 
# convert the census codes to their SOC counter-parts

SOC_codes <- read_excel("Census Code to SOC.xlsx") %>% 
  clean_names()

# Removing the hyphen from the SOC code variables

SOC_codes <- SOC_codes %>% 
  mutate(x2010_soc_code = str_remove_all(x2010_soc_code, "-"))

# Cleaning up the column names and merging the SOC codes with the "occupations"
# data frame. There were 6 categories that did not have exact matches, and because
# that is such a small portion I removed all NA values

colnames(SOC_codes) <- c("category", "census_code", "soc_code")

occupations2 <- occupations1 %>% 
  left_join(SOC_codes, by = c("code" = "census_code")) %>% 
  na.omit()

# Reading in the code list to convert SOC codes into ISCO-08 codes. I also have 
# to convert the variables to character type so that they will be able to join with 
# the occupations data frame

soc_isco <- read_excel("soc_to_is08.xls") %>% 
  clean_names() %>% 
  mutate(soc = as.character(soc),
         is08_3d = as.character(is08_3d))

# Merging the IS-08 codes with the occupations data frame

occupations3 <- occupations2 %>% 
  left_join(soc_isco, by = c("soc_code" = "soc"))

# Joining the occupational codes to the cps data

cps1 <- cps %>% 
  left_join(occupations3, by = "code") %>% 
  select(-digits, -OCC10LY)

# Now that I have matched the managerial codes between the US and EU datasets (so
# that they will be comporable when I compare the percentage of female managers across
# countries) I can filter for managerial occupations only and drop the other code and
# category variables

cps2 <- cps1 %>% 
  filter(grepl("^1", is08_3d)) %>% 
  select(-code, lbl, soc_code)

# Changing the coded values for firm size and sex to descriptive values

cps2 <- cps2 %>% 
  mutate(SEX = str_replace_all(SEX, c("1" = "Male", "2" = "Female"))) %>% 
  mutate(FIRMSIZE = case_when(
    FIRMSIZE == 1 ~ "Under 10 employees",
    FIRMSIZE == 2 | 3 ~ "10 to 24 employees",
    FIRMSIZE == 4 | 5 ~ "25 to 99 employees",
    FIRMSIZE == 7 ~ "100 to 499 employees",
    FIRMSIZE == 8 ~  "500 to 999 employees",
    FIRMSIZE == 9 ~ "Over 1,000 employees"
  ))

names(cps2) <- tolower(names(cps2))

# Creating a subset dataframe for the total percentage of women managers

spread_gender <- cps2 %>% 
  group_by(year, sex) %>% 
  summarize(total = sum(asecwt)) %>% 
  spread(key = sex, value = total) %>% 
  mutate(total = Female + Male, female_managers = Female / total, male_managers = Male / total) %>% 
  select(-Female, -Male, -total) %>% 
  ungroup()

# Creating a subset dataframe for the average hours worked 
# by men and women in each occupational category

spread_hours <- cps2 %>% 
  group_by(year, sex) %>% 
  summarize(avg_hrs = mean(uhrsworkly)) %>% 
  spread(key = sex, value = avg_hrs) %>% 
  rename(female_hours = Female, male_hours = Male) %>% 
  ungroup()

# Creating a subset dataframe for the percent of men and women
# working part-time in each occupational category

spread_pt <- cps2 %>% 
  group_by(year, sex, fullpart) %>% 
  tally(wt = asecwt) %>% 
  mutate(n = percent(round(n / sum(n), 2), accuracy = 1)) %>% 
  spread(key = sex, value = n) %>% 
  filter(fullpart == 2) %>% 
  rename(female_pt = Female, male_pt = Male) %>% 
  select(-fullpart) %>% 
  ungroup()

# Combining these spreads into a tidy data set. I also added a 
# country variable for when I merge the US data with the EU data

cps_tidy <- spread_gender %>% 
  left_join(spread_hours, by = "year") %>% 
  left_join(spread_pt, by = "year")

cps_tidy$country <- "US"

# Create file and send to main folder

directory <- "/Users/sydneysteel/Thesis"

write.csv(cps_tidy, file = file.path(directory, "US_data.csv"))


