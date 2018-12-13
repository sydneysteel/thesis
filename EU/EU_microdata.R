library(fst)
library(tidyverse)
library(janitor)
library(readxl)
library(rebus)
library(scales)

eu_lfs <- read_fst("eulfsSubset.fst") %>% 
  clean_names() %>% 
  as_tibble()

# Add zeros to 2 digit codes which were ommitted in data read in

occupations <- eu_lfs %>% 
  select(isco3d, year, is883d) %>% 
  mutate(digits = str_length(isco3d)) %>% 
  mutate(digits2 = str_length(is883d)) %>% 
  mutate(new = ifelse(digits < 3, paste("0", isco3d, sep = ""), isco3d)) %>% 
  mutate(new2 = ifelse(digits2 < 3, paste("0", is883d, sep = ""), is883d))

# Reading in the code structure files in order to match the codes with categories

isco08_codes <- read_excel("struct08.xls") %>% 
  clean_names()

is88_codes <- read_excel("ISCO88 Code Index.xlsx") %>% 
  clean_names() %>% 
  na.omit()

# Adding occupation column by matching code indexes 

occupations <- occupations %>% 
  left_join(isco08_codes, by = c("new" = "isco_08_code")) %>% 
  left_join(is88_codes, by = c("new2" = "isco88_com_code")) %>% 
  select(-digits, -digits2)

# Creating a new data frame that contains all unique values for 
# the ISCO codes and their corresponding categories

is08 <- occupations %>% 
  select(new, title_en) %>% 
  na.omit() %>% 
  unique() 

colnames(is08) <- c("Code", "Category")

is88 <- occupations %>% 
  select(new2, major_sub_major_minor_and_unit_groups) %>% 
  na.omit() %>% 
  unique()

colnames(is88) <- c("Code", "Category")

# I combined the IS08 and IS88 tables and filtered for unique values so 
# that I could have a list of all possible code and category combinations
# for both structures. 

occupations_neat <- bind_rows(is08, is88) %>% 
  unique()

# Changed the coded variables into their descriptions and grouped age 
# observations into buckets. I also filtered the date range for 2008 - 2017 
# so that it would match time frame of the US data (which does not go earlier than
# 2008) when I eventually combine them

lfs1 <- eu_lfs %>% 
  mutate(sex = str_replace_all(sex, c("1" = "Male", "2" = "Female")),
         marstat = str_replace_all(marstat, c("1" = "Married", "2" = "Single", "0" = "No answer")),
         age = cut(age,
                   breaks = c(-Inf, 15, 25, 45, 65, Inf),
                   labels = c("Under 15 years", "15 - 24 years", "25 - 44 years",
                              "45 - 64 years", "65 years and over"),
                   right = FALSE)) %>% 
  filter(year >= 2008) %>% 
  select(year, country, coeff, wstator, stapro, hhtype, ftpt, ftptreas, hwusual, is883d, sex,
         isco3d, age, marstat, sizefirm)

# I combined the two seperate code columns into one comprehensive one
# and eliminated the characters from this combined variable 

lfs1 <- lfs1 %>% 
  mutate(Code = paste(isco3d, is883d, sep = "_"),
         Code = str_remove_all(Code, "[NA_]")) %>% 
  select(-is883d, -isco3d)

# I realized that there were several codes that could not be matched for categories
# between the IS88 and IS08 versions. Rather than trying to find the linking categories,
# I decided to use only the ISCO08 codes and only use data from 2008 and on. I also 
# removed all observations that had a value of NA for their coefficient in order to properly 
# sum nation-wide averages

lfs1 <- lfs1 %>% 
  left_join(is08, by = "Code") %>% 
  na.omit(cols = "coeff")

# Since I am only interested in looking at cross-national variation in the percentage of
# female managers, I am filtering the data to only include occupational categories with 
# a code beginning with 1, which is the major group number for managerial occupations

lfs2 <- lfs1 %>% 
  filter(grepl("^1", Code))

# Creating a subset dataframe for the percentage of women managers in 
# each country for each year. Because I only want the aggregate managerial
# category, I can drop the Code and Catebory variables

spread_gender <- lfs2 %>% 
  group_by(year, country, sex) %>%
  summarize(total = sum(coeff)) %>%
  spread(key = sex, value = total) %>% 
  mutate(total = Female + Male, female_managers = Female / total, male_managers = Male / total) %>% 
  select(-Female, -Male, -total) %>% 
  ungroup()

# Creating a subset dataframe for the average hours works by men
# and women in each occupational category

spread_hours <- lfs2 %>% 
  group_by(year, country, sex) %>% 
  summarize(avg_hrs = mean(hwusual)) %>% 
  spread(key = sex, value = avg_hrs) %>% 
  rename(female_hours = Female, male_hours = Male) %>% 
  ungroup()

# Creating a subset dataframe for the percent of men and women
# working part-time in each occupational category

spread_pt <- lfs2 %>% 
  group_by(year, country, sex, ftpt) %>% 
  tally(wt = coeff) %>% 
  mutate(n = percent(round(n / sum(n), 2), accuracy = 1)) %>% 
  spread(key = sex, value = n) %>% 
  filter(ftpt == 2) %>% 
  rename(female_pt = Female, male_pt = Male) %>% 
  select(-ftpt) %>% 
  ungroup()

# Creating a subset dataframe for the average firm size of
# each occupational category

spread_firmsize <- lfs2 %>% 
  group_by(year, country) %>% 
  summarize(avg_firmsize = mean(sizefirm)) %>% 
  ungroup()

# Combining these spreads into a tidy data set

lfs_tidy <- spread_gender %>% 
  left_join(spread_firmsize, by = c("year", "country")) %>% 
  left_join(spread_hours, by = c("year", "country")) %>% 
  left_join(spread_pt, by = c("year", "country"))

# Create file and send to main folder

directory <- "/Users/sydneysteel/Thesis"

write.csv(lfs_tidy, file = file.path(directory, "EU_data.csv"))
