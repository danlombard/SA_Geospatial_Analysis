library(tidyverse)
library(googlesheets)
library(rmapshaper)
library(rvest)
library(sf)


my_sf <- st_read("gadm36_ZAF_shp/gadm36_ZAF_1.shp")


sa_sheets <- gs_title('SA_provinces')
pop <- sa_sheets %>% gs_read(ws = 'sa_pop')
gdp <- sa_sheets %>% gs_read(ws = 'sa_gdp')
crime <- sa_sheets %>% gs_read(ws = 'sa_crime_stats')

# Prepare the population data
tidy_pop <- pop %>%
  select(-1) %>%
  slice(1:10) %>% 
  rename_at(vars(names(.)), ~ c("prov", "pop_2011")) %>%
  mutate(
    pop_2011 = sub("\n.*", "", pop_2011) %>% 
      str_replace_all(",", "") %>% 
      as.numeric(),
  ) %>% 
  arrange(prov)
tidy_pop

# wrangle gdp data
tidy_gdp <- gdp %>% 
  select(-1) %>% 
  slice(1:nrow(.)) %>% 
  rename_at(vars(names(.)), ~ c("prov", "total_gdp", 
                                "gdp_per_capita", "gdp_per_capita_usd")) %>% 
  mutate(
    total_gdp = sub("\n.*", "", total_gdp) %>% 
      str_replace_all(",", "") %>% 
      as.numeric(),
    gdp_per_capita = sub("\n.*", "", gdp_per_capita) %>% 
      str_replace_all(",", "") %>% 
      as.numeric(),
    gdp_per_capita_usd = sub("\n.*", "", gdp_per_capita_usd) %>% 
      str_replace_all(",", "") %>% 
      as.numeric(),
  ) %>%
  arrange(prov)

tidy_crime <- crime %>% 
  select(-1) %>% 
  slice(1:nrow(.)) %>% 
  rename_at(vars(names(.)), ~ c("prov", "Murder",	"Sexual_Offences",	
                                "Attempted_murder",	"Assault_with_the_intent_to_inflict_grievous_bodily_harm",	
                                "Common_assault",	"Common_robbery",	'Robbery_with_aggravating_circumstances',	
                                "Arson", "Malicious_damage_to_property",	"Burglary_at_nonresidential_premises",	
                                "Burglary_at_residential_premises",	"Theft_of_motor_vehicle_and_motorcycle",	
                                "Theft_out_of_or_from_motor_vehicle",	"Stock_theft",	"Illegal_possession_of_firearms_and_ammunition",	
                                "Drug_related_crime",	"Driving_under_the_influence_of_alcohol_or_drugs",	
                                "Sexual_offences_detected_as_a_result_of_police_action",	"All_theft_not_mentioned_elsewhere",	
                                "Commercial_crime",	"Shoplifting", "Community_reported_serious_crimes",	"Carjacking",	"Truck_hijacking",	
                                "Robbery_at_residential_premises",	"Robbery_at_non_residential_premises",	"Bank_robbery",	
                                "Robbery_of_cash_in_transit",	"TRIO_Crimes",	"Rape",	"Sexual_assault",	"Attempted_sexual_offences",	
                                "Contact_sexual_offences"	)) %>% 
  arrange(prov)


# Joining the attribute data
attributes_df <- tidy_gdp %>% 
  left_join(tidy_pop) %>% 
  left_join(tidy_crime)

attributes_df <- saveRDS(attributes_df ,'attributes.rds')




