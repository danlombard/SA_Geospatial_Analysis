
library(tidyverse)
library(googlesheets)
library(rmapshaper)
library(rvest)
library(sf)
library(sp)
library(readRDS)
library(GADMTools)

my_sf <- st_read("gadm36_ZAF_shp/gadm36_ZAF_1.shp")


prov <- c("Free State", "Gauteng", "Western Cape", "Northern Cape", "Mpumalanga", "KwaZulu-Natal", "Eastern Cape", "Limpopo")



#sa_wrapper = gadm_sf.loadCountries("ZAF", level = 1, basefile = "./")

my_spdf = as(my_sf, "Spatial")
class(my_spdf)

str(my_spdf, max.level = 2)

glimpse(my_spdf@data)


