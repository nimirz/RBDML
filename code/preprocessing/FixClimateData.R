
### Alter names in climate dataset to match case data
library(dplyr)

# Load dataset
clim = read.csv('~/Projects/RBDML/data/processed/climate_data.csv')

clim = clim%>%
  rename(country = ADM0_NAME,
         adm1 = ADM1_NAME)

### Fix Swedish Names ###
sw_clim = clim%>%
  subset(country == 'Sweden')%>%
  rename(adm1_clim = adm1)

names = read.csv('data/location_codes/conversions/sweden_adm1.csv')

sw_clim = left_join(sw_clim, names)

# combine with full dataset
sw_clim = sw_clim%>%
  select(-c(adm1_case, adm1_sw2, adm1_clim))

clim = clim%>%
  subset(country != 'Sweden')

clim = rbind(clim, sw_clim)

# FIX USA
clim = clim%>%
  mutate(country = ifelse(country == 'United States of America', 'USA', country))

# Fix Nigeria
clim$adm1 = ifelse(clim$adm1 == 'Nassarawa', 'Nasarawa', clim$adm1)
clim$adm1 = ifelse(clim$adm1 == 'Abuja', 'Federal Capital Territory', clim$adm1)

# summarize countries for country-level data
countries = c("China", "Austria", "Norway", "Slovenia", "Slovakia", "France")

clim_summary = clim%>%
  subset(country %in% countries)%>%
  group_by(country, ADM0_CODE, year, month, measure, unit)%>%
  summarize(value = mean(value))%>%
  ungroup()

clim_summary$adm1 = 'all'
clim_summary$ADM1_CODE = NA
clim_removed = clim%>%subset(!country %in% countries)

clim = rbind(clim_removed, clim_summary)

clim_nas = clim%>%
  subset(is.na(country))

write.csv(clim, '~/Projects/RBDML/data/processed/climate_data.csv', row.names = F)

