set.seed(42)

###################################
################
#######
# FIGHTING CLIMATE CHANGE
#######
################
###################################
library(haven)
library(tidyverse)
library(sf)
library(here)

#Importing fighting climate change data
fighting_climate_data <- read_dta(here("data", "fighting_climate_change", "all.dta"))

table(fighting_climate_data$country)

#Checking what country specific zip codes look like
fighting_climate_data_country_subset <- fighting_climate_data %>%
  filter(country == "JP")

fighting_climate_data_country_subset$zipcode

#Correcting incorrect country codes in fighting_climate_data based on standard ISO codes
fighting_climate_data <- fighting_climate_data %>%
  mutate(
    country = case_when(
      country == "IA" ~ "IN",   #India
      country == "SP" ~ "ES",   #Spain
      country == "SA" ~ "ZA",   #South Africa
      country == "SK" ~ "KR",   #South Korea
      TRUE ~ country
    )
  )

#Importing GeoNames ZIP code data (data is from: https://download.geonames.org/export/zip/allCountries.zip)
geonames_zips <- read_delim(here("data", "fighting_climate_change", "geonames", "allCountries.txt"),             
  delim = "\t",
  col_names = c(
    "country_code",     
    "postal_code",       
    "place_name",
    "admin1_name",
    "admin1_code",
    "admin2_name",
    "admin2_code",
    "admin3_name",
    "admin3_code",
    "lat",
    "lon",
    "accuracy"
  ),
  col_types = cols(
    country_code = col_character(),
    postal_code  = col_character(),
    lat          = col_double(),
    lon          = col_double()
  )
)

table(geonames_zips$country_code)

#Collapse geonames_zips to unique country+postal_code combos
geonames_zip_unique <- geonames_zips %>%
  dplyr::distinct(country_code, postal_code, .keep_all = TRUE)

#Joining GeoNames lat/lon twith fighting_climate_data
fighting_climate_data_geo <- fighting_climate_data %>%
  mutate(
    zipcode = as.character(zipcode),
    country = as.character(country)
  ) %>%
  left_join(
    geonames_zips %>%
      dplyr::select(country_code, postal_code, lat, lon, place_name,
                    admin1_name, admin2_name),
    by = c("country" = "country_code",
           "zipcode" = "postal_code")
  )

# Error: In left_join(., geonames_zips %>% dplyr::select(country_code, postal_code,  :
# Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 2925 of `x` matches multiple rows in `y`.
# ℹ Row 1805796 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
#Investigating the many-to-many relationship
#Check what is in row 1805796 of geonames_zips
geonames_zips[1805796, ]
#11 Municipalities are assigned to row 2925 with postal code 70360 in our geonames_zips data, location does not seem far off from each other, so retaining only one of them and removing the other ones from geonames_zips
#Infact, let us remove duplicate postal codes within each country in geonames_zip_unique
geonames_zip_unique <- geonames_zips %>%
  dplyr::distinct(country_code, postal_code, .keep_all = TRUE)

#Rejoining
#Joining GeoNames lat/lon twith fighting_climate_data
fighting_climate_data_geo <- fighting_climate_data %>%
  mutate(
    zipcode = as.character(zipcode),
    country = as.character(country)
  ) %>%
  left_join(
    geonames_zip_unique %>%
      dplyr::select(country_code, postal_code, lat, lon, place_name,
                    admin1_name, admin2_name),
    by = c("country" = "country_code",
           "zipcode" = "postal_code")
  )

table(geonames_zip_unique$country_code)

#Convert to sf POINTS ----
# 1. Split into matched vs unmatched ZIPs
fighting_climate_geo_split <- fighting_climate_data_geo %>%
  mutate(has_coords = !is.na(lat) & !is.na(lon))

fighting_climate_geo_matched <- fighting_climate_geo_split %>%
  filter(has_coords)

fighting_climate_geo_unmatched <- fighting_climate_geo_split %>%
  filter(!has_coords)

table(fighting_climate_geo_unmatched$country)

#How many didn't match?
table(fighting_climate_geo_split$has_coords)

#Subsetting unmatched to CN only
fighting_climate_geo_unmatched_cn <- fighting_climate_geo_unmatched %>%
  filter(country == "CN")

fighting_climate_geo_unmatched_cn$zipcode

#Checking zipcodes of countries with high no matches  
geonames_zip_country_subset <- geonames_zip_unique %>%
  filter(country_code == "JP")

geonames_zip_country_subset$postal_code

#Correcting the reasons for mismatches 
#For Brazil and Poland, remove everything after the hyphen in the zip code

#For UK, geonames uses country specific ISO, but fighting climate change uses UK as a category for United Kingdom. The zipcodes for UK are part of a single national system, so I cna just change England, Scotland, Wales, and Northern Ireland to UK in geonames

#For South Korea, geonames uses KR as the country code, but fighting climate change uses SK. So I will change SK to KR in fighting climate change data (see the above code where we change the 2 letter ISO)

#Japan is weird. The standard postal codes are 7 digits (XXX-XXXX), but in fighting climate change data, there are 5 digit codes. Not sure what this maps on to. 
#"Japanese postal codes have seven digits, with the first three digits and the last four digits separated by a hyphen (e.g., 123-4567). The first two digits represent a prefecture and multiple digits can be assigned to a given prefecture. The third digit indicates a city or a group of cities. The fourth and fifth digits represent a delivery area with the city or town, such as a neighborhood or district." (From: https://www.japanmanifest.com/japanese-address-format/). Based on this, what we can do is to remove the last two digits from the 7-digit postal codes in geonames to see if that helps with matching and also remove the hyphen
#Updating geonames_zip_unique accordingly

geonames_zip_unique_cleaned <- geonames_zip_unique %>%
  mutate(
    postal_code = case_when(
      country_code == "BR" & str_detect(postal_code, "-") ~ sub("-.*$", "", postal_code),
      country_code == "PL" & str_detect(postal_code, "-") ~ sub("-.*$", "", postal_code),
      country_code == "JP" & str_detect(postal_code, "-") ~ substr(gsub("-", "", postal_code), 1, 5),
      country_code == "JP" & nchar(postal_code) == 7      ~ substr(postal_code, 1, 5),
      TRUE ~ postal_code
    ),
    country_code = case_when(
      country_code == "GB" ~ "UK",
      TRUE ~ country_code
    )
  ) %>%
  distinct(country_code, postal_code, .keep_all = TRUE)

table(geonames_zip_unique_cleaned$country_code)

#Rejoining after cleaning
fighting_climate_data_geo <- fighting_climate_data %>%
  mutate(
    zipcode = as.character(zipcode),
    country = as.character(country)
  ) %>%
  left_join(
    geonames_zip_unique_cleaned %>%
      dplyr::select(country_code, postal_code, lat, lon, place_name,
                    admin1_name, admin2_name),
    by = c("country" = "country_code",
           "zipcode" = "postal_code")
  )

#Convert to sf POINTS ----
# 1. Split into matched vs unmatched ZIPs
fighting_climate_geo_split <- fighting_climate_data_geo %>%
  mutate(has_coords = !is.na(lat) & !is.na(lon))

fighting_climate_geo_matched <- fighting_climate_geo_split %>%
  filter(has_coords)

fighting_climate_geo_unmatched <- fighting_climate_geo_split %>%
  filter(!has_coords)

table(fighting_climate_geo_unmatched$country)
table(fighting_climate_data$country)

#Calculating proportion of unmatched by country
fighting_climate_data_prop_unmatched <- fighting_climate_geo_split %>%
  group_by(country) %>%
  summarise(
    total = n(),
    unmatched = sum(!has_coords),
    prop_unmatched = unmatched / total
  )

#Brazil still has a lot of missing zip codes. Checking issues manually. 

#Check all Brazil zip codes in unmatched
fighting_climate_geo_unmatched_br <- fighting_climate_geo_unmatched %>%
  filter(country == "BR")
fighting_climate_geo_unmatched_br$zipcode

#Check all Brazil zip codes in geonames
geonames_zip_country_subset <- geonames_zip_unique_cleaned %>%
  filter(country_code == "BR")
geonames_zip_country_subset$postal_code

#This seems to be lack of matching data in geonames rather than because of how they are written. Manually cross-checked these codes. 
#Checking what zipcodes from India did not find a match 

#Check all Brazil zip codes in unmatched
fighting_climate_geo_unmatched_in <- fighting_climate_geo_unmatched %>%
  filter(country == "IN")
fighting_climate_geo_unmatched_in$zipcode

#780001 consistently does not find a match. geonames does not have this code, but it appears that this is a valid zip code in Assam. 