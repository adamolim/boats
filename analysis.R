# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Rowing
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Author: Michele Adamoli 14.03.2021

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 0. Setup ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Palette de couleurs
# 
# color_club_official <- "#DA322B"
# color_club <- c("#e8847f", "#e15a55", "#da322b", "#98231e", "#571411", "#150504", rep("black", 30))
# color_club_light <- c("#f0adaa", "#f3c1bf", "#f7d6d4", "#fbeae9")
# color_club_dark <- c("#821e19", "#6d1915", "#571411", "#2b0a08")

# Packages

library(tidyverse)
library(lubridate)

# Load data

d_RCB_DATA_IMP <- read_csv("input/rcb_data_anonym.csv")
d_RCB_BOATS_IMP <- read_csv("input/rcb_boats_anonym.csv")

# Join data on boats and tweak some terminology

d_RCB_DATA_00 <- d_RCB_DATA_IMP %>% 
  filter(SaisonYear > 2010) %>% 
  select(Startzeit, Boot, SaisonYear, Laenge, MitgliedID) %>% 
  left_join(d_RCB_BOATS_IMP) %>% 
  filter(TypeRigging != "andere") %>%
  mutate(Seats = as.character(Seats)) %>% 
  mutate(Seats = case_when(Seats == "1" ~ "1x",
                           Seats == "2" ~ "2x/2-",
                           Seats == "3" ~ "3x",
                           Seats == "4" ~ "4x/4-",
                           Seats == "5" ~ "4x/4-",
                           Seats == "9" ~ "8+")) %>% 
  mutate(CGig = case_when(CGig == TRUE ~ "C-Gig boat",
                          TRUE ~ "Racing boat"))

print("clarify 'andere'")
  
# Aggregate per boat

d_RCB_DATA_01 <- d_RCB_DATA_00 %>% 
  select(-MitgliedID) %>% 
  unique() %>% 
  group_by(Startzeit, Boot, SaisonYear, TypeRigging, Seats, CGig, AllowedGroupIdList, Laenge) %>% 
  summarise(Laenge = sum(Laenge, na.rm = TRUE)) %>% 
  ungroup() 

# Parameters

y_min <- 2010
y_max <- 2023

# Vectors of boats

d_RCB_BOATS_00 <- d_RCB_BOATS_IMP  %>% 
  mutate(Seats = case_when(Seats == "1" ~ "1x",
                           Seats == "2" ~ "2x/2-",
                           Seats == "3" ~ "3x",
                           Seats == "4" ~ "4x/4-",
                           Seats == "5" ~ "4x/4-",
                           Seats == "9" ~ "8+"))

# Single (club)

BOAT_NAME_1x_CLUB <- d_RCB_BOATS_00 %>%
  filter(AllowedGroupIdList != "Privat") %>% 
  filter(Seats == "1x") %>% 
  select(Boot) %>% 
  pull()

# Single (privat)

BOAT_NAME_1x_PRIVATE <- d_RCB_BOATS_00 %>%
  filter(AllowedGroupIdList == "Privat") %>% 
  filter(Seats == "1x") %>% 
  select(Boot) %>% 
  pull()

# Zweier

BOAT_NAME_2x <- d_RCB_BOATS_00 %>%
  filter(AllowedGroupIdList != "Privat") %>%
  filter(Seats == "2x/2-") %>% 
  select(Boot) %>% 
  pull()

# Vierer

BOAT_NAME_4x <- d_RCB_BOATS_00 %>%
  filter(Seats == "4x/4-") %>% 
  select(Boot) %>% 
  pull()

# Achter

BOAT_NAME_8x <- d_RCB_BOATS_00 %>%
  filter(Seats == "8+") %>% 
  select(Boot) %>% 
  pull()



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# 3. Analysis ----
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3.3 Main categories ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3.3.1 Sweep or scull ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Number of trips

d_ANA_08_1 <- d_RCB_DATA_00 %>%
  count(SaisonYear, TypeRigging) %>%
  arrange(SaisonYear) %>% 
  rename("NUMBER_TRIP_PERS" = "n")

# Number of kilometers des personnes

d_ANA_08_2 <- d_RCB_DATA_00 %>% # par personne
  group_by(SaisonYear, TypeRigging) %>% 
  summarise(Laenge_PERS = sum(Laenge)) %>% 
  ungroup()

# Nombre sorties des bateaux

d_ANA_08_3 <- d_RCB_DATA_01 %>%
  count(SaisonYear, TypeRigging) %>%
  arrange(SaisonYear) %>% 
  rename("NUMBER_TRIP_BOAT" = "n")

# Number of kilometers des bateaux

d_ANA_08_4 <- d_RCB_DATA_01 %>% # par bateau
  group_by(SaisonYear, TypeRigging) %>% 
  summarise(Laenge_BOAT = sum(Laenge)) %>% 
  ungroup()

# Union

d_ANA_08_5 <- full_join(d_ANA_08_1, d_ANA_08_2, by = c("SaisonYear", "TypeRigging"))
d_ANA_08_6 <- full_join(d_ANA_08_3, d_ANA_08_4, by = c("SaisonYear", "TypeRigging"))
d_ANA_08_7 <- full_join(d_ANA_08_5, d_ANA_08_6, by = c("SaisonYear", "TypeRigging"))

# Tidy and unit

d_ANA_08_8 <- d_ANA_08_7 %>% 
  pivot_longer(cols = c(Laenge_BOAT, NUMBER_TRIP_BOAT, Laenge_PERS, NUMBER_TRIP_PERS), names_to = "DIMENSION", values_to = "Value") %>% 
  mutate(UNIT = case_when(DIMENSION == "NUMBER_TRIP_PERS" ~ " persons' trips",
                          DIMENSION == "Laenge_PERS" ~ " persons' km",
                          DIMENSION == "NUMBER_TRIP_BOAT" ~ " boats' trips",
                          DIMENSION == "Laenge_BOAT" ~ " boats' km"))

# In percentage

d_ANA_08_8_pct <- d_ANA_08_8 %>% 
  group_by(SaisonYear, UNIT) %>% 
  mutate(Value = round(Value / sum(Value), digits = 3)) %>% 
  ungroup()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3.3.2 Boats category ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Number of trips

d_ANA_09_1 <- d_RCB_DATA_00 %>%
  count(SaisonYear, Seats) %>%
  arrange(SaisonYear) %>% 
  rename("NUMBER_TRIP_PERS" = "n")

# Number of kilometers des personnes

d_ANA_09_2 <- d_RCB_DATA_00 %>% # par personne
  group_by(SaisonYear, Seats) %>% 
  summarise(Laenge_PERS = sum(Laenge)) %>% 
  ungroup()

# Nombre sorties des bateaux

d_ANA_09_3 <- d_RCB_DATA_01 %>%
  count(SaisonYear, Seats) %>%
  arrange(SaisonYear) %>% 
  rename("NUMBER_TRIP_BOAT" = "n")

# Number of kilometers des bateaux

d_ANA_09_4 <- d_RCB_DATA_01 %>% # par personne
  group_by(SaisonYear, Seats) %>% 
  summarise(Laenge_BOAT = sum(Laenge)) %>% 
  ungroup()

# Union

d_ANA_09_5 <- full_join(d_ANA_09_1, d_ANA_09_2, by = c("SaisonYear", "Seats"))
d_ANA_09_6 <- full_join(d_ANA_09_3, d_ANA_09_4, by = c("SaisonYear", "Seats"))
d_ANA_09_7 <- full_join(d_ANA_09_5, d_ANA_09_6, by = c("SaisonYear", "Seats"))

# Tidy and unit

d_ANA_09_8 <- d_ANA_09_7 %>% 
  pivot_longer(cols = c(Laenge_BOAT, NUMBER_TRIP_BOAT, Laenge_PERS, NUMBER_TRIP_PERS), names_to = "DIMENSION", values_to = "Value") %>% 
  mutate(UNIT = case_when(DIMENSION == "NUMBER_TRIP_PERS" ~ " persons' trips",
                          DIMENSION == "Laenge_PERS" ~ " persons' km",
                          DIMENSION == "NUMBER_TRIP_BOAT" ~ " boats' trips",
                          DIMENSION == "Laenge_BOAT" ~ " boats' km"))

# In percentage

d_ANA_09_8_pct <- d_ANA_09_8 %>% 
  group_by(SaisonYear, UNIT) %>% 
  mutate(Value = round(Value / sum(Value), digits = 3)) %>% 
  ungroup()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3.3.3 C-Gigs ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Number of trips

d_ANA_10_1 <- d_RCB_DATA_00 %>%
  count(SaisonYear, CGig) %>%
  arrange(SaisonYear) %>% 
  rename("NUMBER_TRIP_PERS" = "n")

# Number of kilometers des personnes

d_ANA_10_2 <- d_RCB_DATA_00 %>% # par personne
  group_by(SaisonYear, CGig) %>% 
  summarise(Laenge_PERS = sum(Laenge)) %>% 
  ungroup()

# Nombre sorties des bateaux

d_ANA_10_3 <- d_RCB_DATA_01 %>%
  count(SaisonYear, CGig) %>%
  arrange(SaisonYear) %>% 
  rename("NUMBER_TRIP_BOAT" = "n")

# Number of kilometers des bateaux

d_ANA_10_4 <- d_RCB_DATA_01 %>% # par personne
  group_by(SaisonYear, CGig) %>% 
  summarise(Laenge_BOAT = sum(Laenge)) %>% 
  ungroup()

# Union

d_ANA_10_5 <- full_join(d_ANA_10_1, d_ANA_10_2, by = c("SaisonYear", "CGig"))
d_ANA_10_6 <- full_join(d_ANA_10_3, d_ANA_10_4, by = c("SaisonYear", "CGig"))
d_ANA_10_7 <- full_join(d_ANA_10_5, d_ANA_10_6, by = c("SaisonYear", "CGig"))

# Tidy and unit

d_ANA_10_8 <- d_ANA_10_7 %>% 
  pivot_longer(cols = c(Laenge_BOAT, NUMBER_TRIP_BOAT, Laenge_PERS, NUMBER_TRIP_PERS), names_to = "DIMENSION", values_to = "Value") %>% 
  mutate(UNIT = case_when(DIMENSION == "NUMBER_TRIP_PERS" ~ " persons' trips",
                          DIMENSION == "Laenge_PERS" ~ " persons' km",
                          DIMENSION == "NUMBER_TRIP_BOAT" ~ " boats' trips",
                          DIMENSION == "Laenge_BOAT" ~ " boats' km"))

# In percentage

d_ANA_10_8_pct <- d_ANA_10_8 %>% 
  group_by(SaisonYear, UNIT) %>% 
  mutate(Value = round(Value / sum(Value), digits = 3)) %>% 
  ungroup()



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3.4 Boats ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3.4.1 Skiff ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Number of kilometers

d_ANA_SKIFF <- d_RCB_DATA_01 %>%
  filter(Seats == "1x") %>% 
  group_by(Boot, SaisonYear, AllowedGroupIdList) %>% 
  summarise(Laenge = sum(Laenge)) %>% 
  ungroup()

d_ANA_SKIFF_CLUB <- d_ANA_SKIFF %>%
  filter(AllowedGroupIdList != "Privat" | is.na(AllowedGroupIdList))

d_ANA_SKIFF_PRIVATE <- d_ANA_SKIFF %>%
  filter(AllowedGroupIdList == "Privat")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3.4.2 Doubles and pairs ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Number of kilometers

d_ANA_DOUBLE <- d_RCB_DATA_01 %>%
  filter(Seats == "2x/2-") %>% 
  filter(AllowedGroupIdList != "Privat"| is.na(AllowedGroupIdList)) %>% 
  group_by(Boot, SaisonYear, AllowedGroupIdList) %>% 
  summarise(Laenge = sum(Laenge, na.rm = FALSE)) %>% 
  ungroup()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3.4.2 Quads and fours ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Number of kilometers

d_ANA_QUADS <- d_RCB_DATA_01 %>%
  filter(Seats == "4x/4-") %>% 
  filter(AllowedGroupIdList != "Privat" | is.na(AllowedGroupIdList)) %>% 
  group_by(Boot, SaisonYear, AllowedGroupIdList) %>% 
  summarise(Laenge = sum(Laenge, na.rm = FALSE)) %>% 
  ungroup()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3.4.2 Eeights ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Number of kilometers

d_ANA_EIGHTS <- d_RCB_DATA_01 %>%
  filter(Seats == "8+") %>% 
  group_by(Boot, SaisonYear, AllowedGroupIdList) %>% 
  summarise(Laenge = sum(Laenge, na.rm = FALSE)) %>% 
  ungroup()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3.5 Overview ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Actual club's boats

vec_boot_ymax <- d_RCB_DATA_01 %>%
  filter(!str_detect(Boot, "Private")) %>% 
  filter(SaisonYear == y_max) %>% 
  filter(!is.na(Laenge)) %>% 
  select(Boot) %>% 
  # corrections
  filter(!Boot %in% c("noname", "zerlina")) %>%
  # no liteboats
  filter(!str_detect(Boot, "liteboat|fremdes boot")) %>% 
  unique() %>% 
  pull()
  







