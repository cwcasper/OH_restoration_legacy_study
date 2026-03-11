library(tidyverse)
library(here)
library(multcompView)

group_cov<-read.csv("processed_data/group_cov.csv")

# hypothesis testing ----

## ratios ----
ratio_native_df <- cov_long %>%
  filter(planting == "T",weeding=="C", year==2023) %>%
  group_by(LegacySpp, year, native_status) %>%
  summarise(sum_cover = mean(cover, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = native_status, values_from = sum_cover, values_fill = 0) %>%
  mutate(native_ratio = native / (exotic+1))

native_anova <- group_cov %>% 
  filter(year == 2023) %>% 
  aov(native_ratio ~ LegacySpp, data = .)
summary(native_anova)

native_tukeys<-TukeyHSD(native_anova)
print(native_tukeys)

native_letters<-multcompLetters4(native_anova, native_tukeys)
print(native_letters)

native_letter_df <- data.frame(
  LegacySpp = names(native_letters$LegacySpp),
  Significance_Letter = as.character(native_letters$LegacySpp),
  stringsAsFactors = FALSE
)
