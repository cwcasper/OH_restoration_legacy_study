# analyze ----
library(tidyverse)

group_cov<-read.csv("processed_data/group_cov.csv")
cov_long<- read.csv("processed_data/cov_long.csv")

# distributions ----

# summary cover ----
group_avg<-group_cov %>% 
  group_by(LegacySpp, year) %>% 
  mutate(exotic_other=exotic_total-original_legacy_cover) %>% 
  summarise(
    avg_native=mean(native_cover),
    avg_legacy=mean(original_legacy_cover),
    avg_exotic_other=mean(exotic_other),
    avg_exotic_total=mean(exotic_total),
  avg_ratio=mean(native_ratio, na.rm = TRUE),
  se_ratio=sd(native_ratio, na.rm=TRUE)/ sqrt(n())) 

group_avg_long <- group_avg %>% 
  select(-avg_ratio, -se_ratio) %>% 
  pivot_longer(
    cols = c(avg_native, avg_legacy, avg_exotic_other),  # Select the columns to pivot
    names_to = "cover_type",  # New column to store old column names
    values_to = "cover_value" # New column to store values
  )
str(group_avg_long)


## cover----

# plots ----
leg_labels<-c(  BROTEC = "Cheatgrass",
                CENSTO = "Knapweed",
                EUPESU = "Leafy spurge",
                POTREC = "Cinquefoil",
                Native= "Native")
## stack bar ----
lcp_2023 <- group_avg_long %>%
  filter(year == 2023) %>%
  mutate(
    # Explicitly set the factor levels in the correct order
    LegacySpp = factor(LegacySpp, 
                       levels = c("EUPESU", "POTREC", "CENSTO", "BROTEC", "Native"))  
  ) %>%
  ggplot(aes(x = LegacySpp, 
             y = cover_value, 
             fill = factor(cover_type, levels = c("avg_native","avg_exotic_other","avg_legacy")))) +
  geom_bar(stat = "identity") +
  labs(
    x = "",
    y = "Average % Cover",
    title = "Total Vegetation Cover - 14 Months After Planting"
  ) +
  scale_x_discrete(labels = leg_labels) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(
    values = c("avg_legacy" = "#A0A0A0",  # Medium Gray (Exotic)
               "avg_exotic_other" = "#606060",  # Dark Gray (Legacy)
               "avg_native" = "#3A803A"),  # Complementary Green (Native)
    labels = c("Native Cover", "Exotic Cover", "Legacy Cover"),
    guide = guide_legend(title = NULL)
  ) +
  theme_minimal()
lcp_2023 

## faceted bars ----
vf_2023<-group_avg_long %>% 
  filter(year==2023) %>% 
  ggplot(aes(x= LegacySpp,
             y=cover_value,
             fill=LegacySpp))+
  geom_bar(position = "dodge", stat = "identity")+
  facet_wrap(~cover_type)

vf_2023

## point ratios----
nrp_2023 <- group_avg %>%
  filter(year == 2023) %>% 
  ggplot(aes(x = reorder(LegacySpp, avg_ratio), y = avg_ratio,
             shape = LegacySpp)) +
  geom_point(size = 6, position = position_dodge(width = 0.5),
             na.rm = TRUE) +
  geom_errorbar(aes(ymin = avg_ratio - se_ratio, ymax = avg_ratio + se_ratio),
                width = 0.2,                  
                position = position_dodge(width = 0.5),  
                na.rm = TRUE) +
  scale_x_discrete(labels=leg_labels)+
  scale_shape_manual(values=c(18,15 ,16, 17,25))+
  geom_hline(yintercept = 1, linetype= "dashed", color="gray")+
  labs(
    x="",
    y = "Native:Exotic",
    title = "native to exotic ratio 14 months after planting"
  ) +
  theme_minimal()+
  theme(legend.position="none")+
  ylim(0,1.5)

nrp_2023

