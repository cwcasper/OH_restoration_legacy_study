# load packages and data ----
library(tidyverse)
library(here)

counts<-read.csv("raw_data/Master_CountsOHEP.csv")
cover<-read.csv("raw_data/Master_CoverOHEP.csv")
nutrients<-read.csv("raw_data/OHEP_NO3_SOM.csv")

# wrangle ----
## recode w/ metadata ----
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
  native_forb_other="native"
)

leg_labels<-c(  BROTEC = "cheatgrass",
                CENSTO = "knapweed",
                EUPESU = "leafy spurge",
                POTREC = "cinquefoil",
                Native= "native")

## summarise and group ----
cov_long<-cover %>% 
  pivot_longer(cols = ACHMIL:POTREC, names_to = "species", values_to = "cover") %>% 
  mutate(cover=as.numeric(cover),
         LegacySpp = factor(LegacySpp, levels = c("CENSTO", "EUPESU", "POTREC", "BROTEC", "Native")),
         life_form=factor(recode(species, !!!life_form), levels=c("forb","grass")),
         native_status = factor(recode(species, !!!native_status), levels = c("native", "exotic")))

write.csv(cov_long, "processed_data/cov_long.csv")

## native, exotic, legacy ----
## group_cov dataframe represents unweeded, transplanted quadrants with the following functional groups totaled for cover
## native, original_legacy, exotic_other, and exotic_total
group_cov <- cover %>% 
  mutate(native_cover = rowSums(across(ACHMIL:PSESPE), na.rm = TRUE)) %>% 
  select(Block, Subplot, LegacySpp, year, planting, weeding, native_cover,
         original_legacy_cover, exotic_other_total, exotic_total) %>% 
  filter(weeding=="C", planting =="T") %>% 
  group_by(LegacySpp, year) %>% 
  select(Block, Subplot, LegacySpp, year, native_cover, original_legacy_cover, exotic_other_total, exotic_total) %>% 
  mutate(native_ratio = native_cover / (exotic_total+1))
str(group_cov)

write.csv(group_cov, "processed_data/group_cov.csv")



