library(fst)
library(tidyverse)
library(janitor)
library(readxl)
library(rebus)
library(scales)

eu_lfs <- read_fst("eulfsSubset.fst") %>% 
  clean_names() %>% 
  as_tibble()

# add zeros to 2 digit codes which were ommitted in data read in

occupations <- eu_lfs %>% 
  select(isco3d, year, is883d) %>% 
  mutate(digits = str_length(isco3d)) %>% 
  mutate(digits2 = str_length(is883d)) %>% 
  mutate(new = ifelse(digits < 3, paste("0", isco3d, sep = ""), isco3d)) %>% 
  mutate(new2 = ifelse(digits2 < 3, paste("0", is883d, sep = ""), is883d))

isco08_codes <- read_excel("struct08.xls") %>% 
  clean_names()

is88_codes <- read_excel("ISCO88 Code Index.xlsx") %>% 
  clean_names() %>% 
  na.omit()

# adding occupation column by matching code indexes 

occupations <- occupations %>% 
  left_join(isco08_codes, by = c("new" = "isco_08_code")) %>% 
  left_join(is88_codes, by = c("new2" = "isco88_com_code")) %>% 
  select(-digits, -digits2)

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

occupations_neat <- bind_rows(is08, is88) %>% 
  unique()

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

lfs1 <- lfs1 %>% 
  mutate(Code = paste(isco3d, is883d, sep = "_"),
         Code = str_remove_all(Code, "[NA_]")) %>% 
  select(-is883d, -isco3d)

# I used only the ISCO08 codes because I am working with years past 2008.
# I removed all observations that had a value of NA for their coefficient in 
# order to properly sum nation-wide averages

lfs1 <- lfs1 %>% 
  left_join(is08, by = "Code") %>% 
  na.omit(cols = "coeff")

spread_gender <- lfs1 %>% 
  group_by(year, country, sex, Code, Category) %>%
  summarize(total = sum(coeff)) %>%
  spread(key = sex, value = total) %>%
  mutate(total = Female + Male, Female = Female / total, Male = Male / total,
         Female = percent(round(Female, 2), accuracy = 1),
         Male = percent(round(Male, 2), accuracy = 1))

spread_hours <- lfs1 %>% 
  group_by(year, country, Code, Category, sex) %>% 
  summarize(avg_hrs = mean(hwusual)) %>% 
  spread(key = sex, value = avg_hrs) %>% 
  rename(female_hours = Female, male_hours = Male)

spread_pt <- lfs1 %>% 
  group_by(year, country, sex, Code, Category, ftpt) %>% 
  tally(wt = coeff) %>% 
  mutate(n = percent(round(n / sum(n), 2), accuracy = 1)) %>% 
  spread(key = sex, value = n) %>% 
  filter(ftpt == 2) %>% 
  rename(female_pt = Female, male_pt = Male) %>% 
  select(-ftpt)

spread_firmsize <- lfs1 %>% 
  group_by(year, country, Code, Category) %>% 
  summarize(avg_firmsize = mean(sizefirm))

lfs_tidy <- spread_gender %>% 
  left_join(spread_firmsize, by = c("year", "country", "Code", "Category")) %>% 
  left_join(spread_hours, by = c("year", "country", "Code", "Category")) %>% 
  left_join(spread_pt, by = c("year", "country", "Code", "Category")) %>% 
  select(-total)
