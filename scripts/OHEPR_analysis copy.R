#### Setup ####
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
install.packages("multcompView")
library(multcompView)

setwd('/Users/ccasper/Documents/MPG_Projects/OHEP_restoration/Data')
counts<-read.csv("Master_CountsOHEP.csv")
cover<-read.csv("Master_CoverOHEP.csv")
nutrients<-read.csv("OHEP_NO3_SOM.csv")

str(nutrients)
#### Wrangle ####
life_form<-c(
  ACHMIL="forb",
  BOUGRA="grass",
  ELYELY="grass",
  ERISPE="forb",
  GAIARI="forb",
  KOEMAC="grass",
  LINLEW="forb",
  PENSTR="forb",
  PSESPE="grass",
  BROTEC="grass",
  CENSTO="forb",
  EUPESU="forb",
  POTREC="forb",
  UNK_grass="grass",
  UNK_forb="forb"
)

native_status <- c(
  ACHMIL = "native",
  BOUGRA = "native",
  ELYELY = "native",
  ERISPE = "native",
  GAIARI = "native",
  KOEMAC = "native",
  LINLEW = "native",
  PENSTR = "native",
  PSESPE = "native",
  UNK_grass="native",
  UNK_forb="native",
  BROTEC = "exotic",
  CENSTO = "exotic",
  EUPESU = "exotic",
  POTREC = "exotic",
  original_legacy_cover="exotic",
  new_legacy_cover="exotic",
  exotic_grass_other="exotic",
  native_grass_other="native",
  native_forb_other="native",
  exotic_total="exotic"
)

leg_labels<-c(  BROTEC = "cheatgrass",
                  CENSTO = "knapweed",
                  EUPESU = "leafy spurge",
                  POTREC = "cinquefoil",
                  Native= "native")

##### counts #####
freq_long<-counts %>% 
  filter(planting=="T") %>% 
  group_by(LegacySpp) %>% 
  pivot_longer(cols = ACHMIL:UNK_forb, names_to = "species", values_to="count") %>% 
  mutate(count=as.numeric(count),
         LegacySpp = factor(LegacySpp, levels = c("CENSTO", "EUPESU", "POTREC", "BROTEC", "Native")),
         life_form=factor(recode(species, !!!life_form), levels=c("forb","grass")),
         native_status = factor(recode(species, !!!native_status), levels = c("native", "exotic"))) %>% 
  arrange(desc(life_form))

sum_count<-freq_long %>% 
  filter(native_status=="native") %>% 
  group_by(LegacySpp, year, life_form) %>% 
  summarise(avg_count=mean(count, na.rm = TRUE))
  
##### cover #####
cov_long<-cover %>% 
  pivot_longer(cols = ACHMIL:POTREC, names_to = "species", values_to = "cover") %>% 
  mutate(cover=as.numeric(cover),
         LegacySpp = factor(LegacySpp, levels = c("CENSTO", "EUPESU", "POTREC", "BROTEC", "Native")),
         life_form=factor(recode(species, !!!life_form), levels=c("forb","grass")),
         native_status = factor(recode(species, !!!native_status), levels = c("native", "exotic")))

sum_cover<-cov_long %>%
  filter(native_status=="native", planting=="T") %>% 
  group_by(LegacySpp, year, life_form) %>% 
  summarise(avg_cover=mean(cover,na.rm = TRUE),
            se_cover = sd(cover, na.rm = TRUE) / sqrt(n())
            )

sum_prov<-cov_long %>% 
  filter(weeding=="C", planting == "T") %>% 
  group_by(LegacySpp, year, native_status, Block) %>% 
  summarise(sum_cover=sum(cover, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(LegacySpp, year, native_status) %>% 
  summarise(avg_cover=mean(sum_cover,na.rm = TRUE),
            se_cover = sd(sum_cover, na.rm = TRUE) / sqrt(n())
  )

###### forb ratio #####
ratio_forb <- cov_long %>%
  filter(planting == "T",weeding=="W",
         native_status=="native") %>%
  group_by(LegacySpp, year, life_form, Block) %>%
  summarise(sum_cover = sum(cover, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = life_form, values_from = sum_cover, values_fill = 0) %>%
  mutate(forb_ratio = forb / (grass+1)) %>%
  group_by(LegacySpp, year)%>%
  summarise(
    avg_ratio=mean(forb_ratio, na.rm = TRUE),
    se_ratio=sd(forb_ratio, na.rm=TRUE)/ sqrt(n())) 

###### native ratio ######
ratio_native <- cov_long %>%
  filter(planting == "T",weeding=="C") %>%
  group_by(LegacySpp, year, native_status,Block) %>%
  summarise(sum_cover = sum(cover, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = native_status, values_from = sum_cover, values_fill = 0) # %>%
  mutate(native_ratio = native / (exotic+1)) %>%
  group_by(LegacySpp, year)%>%
  summarise(avg_nat=mean(native),
            avg_ex=mean(exotic),
    avg_ratio=mean(native_ratio, na.rm = TRUE),
    se_ratio=sd(native_ratio, na.rm=TRUE)/ sqrt(n())) 

###### legacy cover -----
str(cover)
weeds <- cover %>% 
  mutate(native_cover = rowSums(across(ACHMIL:PSESPE), na.rm = TRUE)) %>% 
  select(Block, LegacySpp, year, planting, weeding, BROTEC, CENSTO, EUPESU, POTREC,native_cover,
         original_legacy_cover, exotic_grass_other, exotic_forb_other, exotic_other_total, exotic_total) %>% 
  filter(weeding=="C", planting =="T") %>% 
  group_by(LegacySpp, year) %>% 
  summarise(avg_tot_exotic=mean(exotic_total, na.rm = TRUE),
            avg_legacy=mean(original_legacy_cover, na.rm= TRUE),
            avg_native=mean(native_cover, na.rm=TRUE))

legacy_cov<-cover %>% 
  filter(weeding=="C", planting =="T") %>% 
  group_by(LegacySpp, year, Block) %>% 
  summarise(exotic_cover=mean(exotic_total-original_legacy_cover
                              , na.rm = TRUE),
            legacy_cov=mean(original_legacy_cover))

legacy_cov_long <- legacy_cov %>%
  pivot_longer(cols = c(exotic_cover, legacy_cov), 
               names_to = "cover_type", 
               values_to = "cover_value") %>% 
  group_by(year, LegacySpp, cover_type) %>% 
  summarise(avg_cover=mean(cover_value))

## error bars or even hypothesis testing with exotic + legacy stack bars----

##### nutrients #####

nutrient_sum<-nutrients %>% 
  group_by(LegacySpp, Year) %>% 
  summarise(avg_n=mean(Nitrate),
              se_n=sd(Nitrate) / sqrt(n()))

#### Hypothesis testing one way ANOVA ####
##### native ratio ######
ratio_native_df <- cov_long %>%
  filter(planting == "T",weeding=="C", year==2023) %>%
  group_by(LegacySpp, year, native_status) %>%
  summarise(sum_cover = mean(cover, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = native_status, values_from = sum_cover, values_fill = 0) %>%
  mutate(native_ratio = native / (exotic+1))

native_anova<-aov(native_ratio ~ LegacySpp, data= ratio_native_df)
summary(native_anova)

native_tukeys<-TukeyHSD(native_anova)
print(native_tukeys)

native_letters<-multcompLetters4(native_anova, native_tukeys)
print(native_letters)

native_letter_df <- as.data.frame.list(native_letters$LegacySpp)
colnames(native_letter_df) <- c("LegacySpp", "Significance")

##### native ratio w/out PENSTR######
ratio_native_nopen <- cov_long %>%
  filter(planting == "T",weeding=="C", year==2023, species != "PENSTR") %>%
  group_by(LegacySpp, year, native_status, Block) %>%
  summarise(sum_cover = sum(cover, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = native_status, values_from = sum_cover, values_fill = 0) %>%
  mutate(native_ratio = native / (exotic+1))

nopen_anova<-aov(native_ratio ~ LegacySpp, data= ratio_native_nopen)
summary(nopen_anova)

nopen_tukeys<-TukeyHSD(nopen_anova)
print(nopen_tukeys)

nopen_letters<-multcompLetters4(nopen_anova, nopen_tukeys)
print(nopen_letters)

nopen_letter_df <- as.data.frame.list(nopen_letters$LegacySpp)
colnames(nopen_letter_df) <- c("LegacySpp", "Significance")

##### nutrient anova #####

nutrient_anova<-nutrients %>% 
 { aov(Nitrate ~ Year, data = .)}
summary(nutrient_anova)

#### Plots ####
##### count 2024 ####
# Box plot with facets by LegacySpp
species_box<- freq_long %>% 
  filter(year=="2024") %>% 
  ggplot(aes(x = reorder(species, as.numeric(life_form)), y = count)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ LegacySpp, scales = "free_y") +
  labs(
    x = "",
    y = "Average Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

species_box

##### survival over time (count) #####
# bar plot of survival over time
survival <- sum_count %>%
  ggplot(aes(x = life_form, y = avg_count, fill = factor(year))) +  # Fill by life form
  geom_bar(stat = "identity", position = position_dodge(width=1), na.rm = TRUE) +  # Use dodge for side-by-side bars
  facet_grid(~LegacySpp) +
  labs(
    x = "",
    y = "mean count",
    title = "survival by functional group"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

survival

##### cover ####
# point plot mean functional group cover
fg_cover_native <- sum_cover %>%
  filter(year %in% c("2023", "2024")) %>% 
  ggplot(aes(x = year, y = avg_cover, shape = life_form)) +
  geom_point(size = 3, position = position_dodge(width = 0.5),
             na.rm = TRUE) +
  geom_errorbar(aes(ymin = avg_cover - se_cover, ymax = avg_cover + se_cover),
                width = 0.2,                    # Width of the error bars
                position = position_dodge(width = 0.5),  # Align with the points
                na.rm = TRUE) +
  facet_grid(~LegacySpp)+
  labs(
    x="",
    y = "mean % cover",
    title = "Functional Group Cover"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks = c(2023, 2024))

fg_cover_native

##### native vs. exotic #####
# bar plot mean cover by native status
prov_cover <- sum_prov %>%
  filter(year %in% c("2023")) %>% 
  ggplot(aes(x = factor(year), y = avg_cover, fill = native_status)) +  # Change to factor for x
  geom_bar(stat = "identity", position = position_dodge(width = 1), na.rm = TRUE) +  # Bar plot
  geom_errorbar(aes(ymin = avg_cover - se_cover, ymax = avg_cover + se_cover),
                width = .2, 
                position = position_dodge(width = 1),  # Align error bars with the bars
                na.rm = TRUE) +
  facet_grid(~ LegacySpp) +
  labs(
    x = "",
    y = "Mean % Cover",
    title = "Native Status Cover"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prov_cover

###### point plot forb ratio ######
forb_ratio_plot <- ratio_forb %>%
  filter(year %in% c("2023")) %>% 
  ggplot(aes(x = reorder(LegacySpp, avg_ratio), y = avg_ratio,
             shape = LegacySpp)) +
  geom_point(size = 3, position = position_dodge(width = 0.5),
             na.rm = TRUE) +
  geom_errorbar(aes(ymin = avg_ratio - se_ratio, ymax = avg_ratio + se_ratio),
                width = 0.2,                  
                position = position_dodge(width = 0.5),  
                na.rm = TRUE) +
  scale_shape_manual(values=c(18,15 ,16, 17,25))+
  labs(
    x="",
    y = "Native:Exotic",
    title = ""
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

forb_ratio_plot

###### point plot native ratio ######
plot_data <- ratio_native_df %>%
  left_join(native_letter_df, by = "LegacySpp")

native_ratio_plot <- ratio_native %>%
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
  ylim(0,6)

native_ratio_plot

##### legacy cover #####
# Stacked bar plot
legacy_cov_plot <- legacy_cov_long %>%
  filter(year==2023, LegacySpp != "Native") %>% 
  ggplot(aes(x = reorder(LegacySpp, avg_cover), y = avg_cover, fill = cover_type)) +
  geom_bar(stat = "identity") +
  labs(
    x = "",
    y = "average % cover",
    title = "Total exotic cover"
  ) +
  scale_x_discrete(labels=leg_labels)+
  scale_fill_manual(values = c("exotic_cover" = "darkgray", "legacy_cov" = "darkslategray"),  # Custom colors
                    labels = c("total Exotic Cover", "Legacy Cover"),
                    guide= guide_legend(title = NULL)) +
  theme_minimal()

legacy_cov_plot

##### nitrate #####
y_limits <- range(c(nutrient_sum$avg_n - nutrient_sum$se_n, 
                    nutrient_sum$avg_n + nutrient_sum$se_n), 
                  na.rm = TRUE)

nitrate_22<- nutrient_sum %>% 
  filter(Year==2022) %>% 
  ggplot(aes(x=LegacySpp, y=avg_n))+
  geom_bar(stat="identity")+
  geom_errorbar( aes(x=LegacySpp, y=avg_n, ymin=avg_n-se_n, ymax=avg_n+se_n), 
                 width = 0.2)+
  scale_x_discrete(labels=leg_labels)+
  labs(
    x = "",
    y = expression("mean NO"[3]^"-" ~ "mg kg"^"-1"),
    title = expression("2 month post-plant NO"[3]^-"")
  )+
  coord_cartesian(ylim=y_limits)+
  theme_minimal()

nitrate_22

nitrate_23<- nutrient_sum %>% 
  filter(Year==2023) %>% 
  ggplot(aes(x=LegacySpp, y=avg_n))+
  geom_bar(stat="identity")+
  geom_errorbar( aes(x=LegacySpp, y=avg_n, ymin=avg_n-se_n, ymax=avg_n+se_n), 
                 width = 0.2)+
  labs(
    x = "",
    y = expression("mean NO"[3]^"-" ~ "mg kg"^"-1"),
    title = expression("14 month post-plant NO"[3]^-"")
  )+
  coord_cartesian(ylim=y_limits)+
  theme_minimal()

nitrate_23

##### pH summary #####
ph_sum <- nutrients %>% 
  group_by(LegacySpp, Year) %>% 
  summarise(avg_ph = mean(pH, na.rm = TRUE),
            se_ph = sd(pH, na.rm = TRUE) / sqrt(n()))

##### pH plot #####
y_limits_ph <- range(c(ph_sum$avg_ph - ph_sum$se_ph, 
                       ph_sum$avg_ph + ph_sum$se_ph), 
                     na.rm = TRUE)

ph_plot <- ph_sum %>% 
  ggplot(aes(x = LegacySpp, y = avg_ph, fill = factor(Year))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = avg_ph - se_ph, ymax = avg_ph + se_ph),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  scale_x_discrete(labels = leg_labels) +
  scale_fill_brewer(palette = "Set2", name = "Year") +
  labs(
    x = "",
    y = "mean pH",
    title = "Soil pH across legacy species treatments"
  ) +
  theme_minimal()

ph_plot

# Save the plot
ggsave("figures/ph_plot.png", ph_plot, width = 8, height = 6, dpi = 300)
str(nutrients


