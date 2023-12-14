library(tidyverse)
library(sf)
library(lubridate)
library(scales)
library(patchwork)

#-------------------- Process Hantavirus Data -------------------- 

#----- AMERICAS -----
### USA ###
# data source: https://www.cdc.gov/hantavirus/surveillance/index.html
usa = read.csv('data/human/raw/hanta/USA_hantavirus_monthly.csv', skip=1)
usa = usa[1:nrow(usa)-1,] # remove last row, contains citation info
usa$country = 'USA'

# State recorded as FIPS - need to convert to state name
# FIPS codes downloaded from: 
fips = read.csv('data/location_codes/usa_state_fips.csv')
fips = fips%>%
  rename(StateFIPS = FIPS)
usa = left_join(usa, fips)

usa = usa%>%
  # remove undefined cases
  subset(IllnessOnsetDate != 'Before 1993-undefined')%>%
  rename(adm1 = state, 
         date = IllnessOnsetDate)%>%
  mutate(year = year(my(date)),
         month = month(my(date)),
         num_cases = 1)

# remove unknown date
usa = usa%>%
  subset(!is.na(year))

# now aggregate
usa = usa%>%
  group_by(country, adm1, year, month)%>%
  summarize(num_cases = sum(num_cases))%>%
  ungroup()

usa$disease = 'HPS'
usa$continent = 'America'

write.csv(usa, 'data/human/processed/hanta/US_hanta.csv', row.names = F)

### Brazil ### 
br = read.csv('data/human/raw/hanta/Brazil_hantavirus_monthly.csv')
br$country = 'Brazil'
br$disease = 'HPS'

# add state names
sf = st_read('data/location_codes/bra_shp/Brazil_shp_harm_2022.shp')
sf = sf%>%
  rename(adm1 = nam_stt, adm2 = name_mn)%>%
  select(c(adm1, IBGE6))%>%
  st_drop_geometry()

br$IBGE6 = as.character(br$IBGE6)
br = left_join(br, sf, by='IBGE6')
br = br%>%
  select(c(country, adm1, adm2, year, month, num_cases))

br$continent = 'America'

write.csv(br, 'data/human/processed/hanta/BR_hanta.csv', row.names = F)

### CHILE ###
ch = read.csv('data/human/raw/hanta/Chile_hantavirus.csv')
ch$disease = 'HPS'
ch$country = 'Chile'
ch = ch%>%
  rename(adm1_notification = adm1,
         adm1 = region_probable_contagion)%>%
  mutate(date_first_symptoms = as.Date(date_first_symptoms, format="%d/%m/%Y"),
         month=month(date_first_symptoms),
         year = year(date_first_symptoms),
         isoweek = isoweek(date_first_symptoms))%>%
  rename(date = date_first_symptoms)

ch = ch%>%
  group_by(year, month, isoweek, date, country, adm1, adm3, disease)%>%
  summarize(num_cases = n())

# add in adm2
sf = st_read('data/location_codes/chl_shp/chl_admbndp_admALL_bcn_itos_20211008.shp')
sf = sf%>%
  rename(adm3 = ADM3_ES,
         adm2 = ADM2_ES,
         adm1 = ADM1_ES)%>%
  select(c(adm2, adm3))%>%
  st_drop_geometry()

# one has english spelling, other spanish
ch$adm3_es = gtranslate::translate(ch$adm3, from="en", to="es")
ch = ch%>%
  rename(adm3_eng = adm3,
         adm3 = adm3_es)%>%
  mutate(adm3 = str_to_title(adm3))

ch = left_join(ch, sf)

# some missing but will sort later
nas = ch%>%
  subset(is.na(adm2))%>%
  distinct(adm3)

ch$continent = 'America'
write.csv(ch, 'data/human/processed/hanta/CH_hanta.csv', row.names = F)

#----- EUROPE ----- 
### Sweden ### 
sw = read.csv('data/human/raw/hanta/Sweden_Hantavirus.csv')
sw = sw%>%
  mutate(across(January:December, ~stringr::str_split_i(., '/', 1)))%>%
  rename(adm1 = County.Regions, year = Year)
sw$country = 'Sweden'
sw = sw%>%
  pivot_longer(January:December, names_to = "month", values_to = "num_cases")
sw  = sw%>%
  mutate(month = ifelse(month == 'Sept', 'September', month),
         num_cases = str_trim(num_cases), # remove white space
         date = as.Date(paste0(year, '-', month, '-01'), format = '%Y-%B-%d'),
         month = month(date)) 

sw$num_cases = as.integer(sw$num_cases)
sw = sw%>%
  subset(num_cases > -1) # remove cases not measured yet
sw$disease = 'HFRS'
sw$continent = 'Europe'
write.csv(sw, 'data/human/processed/hanta/SE_hanta.csv', row.names = F)

### Germany ### 
de_list = lapply(list.files('data/human/raw/hanta/germany', full.names = T), function(x){
  y = read.csv(x)
  y = y%>%
    pivot_longer(-Week.of.notification, names_to = 'region', values_to = 'num_cases')
  y$year = str_split_i(str_split_i(x, '/', -1), '\\.', 1)
  return(y)
}
)
de = do.call(rbind, de_list)
de = de%>%
  mutate(num_cases = ifelse(is.na(num_cases), 0, num_cases),
         date = ISOweek::ISOweek2date(paste0(year, '-W', sprintf("%02d", Week.of.notification), '-', "1")),
         month = month(date)
  )%>%
  rename(isoweek = Week.of.notification)

# change region names
de = de%>%
  mutate(county = ifelse(grepl('County', region), str_split_i(region, '\\.County', 1),
                         ifelse(grepl('City', region), str_split_i(region, 'City.of.', 2),
                                region)),
         county = str_replace_all(county, '\\.', ' ')
  )

# fixes
de$county = ifelse(de$county == 'Sankt Wendel', 'St. Wendel', de$county)
de$county = str_replace_all(de$county, " i d ", " in der ")
de$county = str_replace_all(de$county, " i ", " im ")
de$county = str_replace_all(de$county, " a d ", " an der ")
de$county = str_replace_all(de$county, " a ", " am ")
de$county = str_replace_all(de$county, "OPf ", "Oberpfalz")
de$county = str_replace_all(de$county, "Greater Aachen", "Städteregion Aachen")
de$county = str_replace_all(de$county, "Haale", "Haale Saale")
de$county = str_replace_all(de$county, "Kempten", "Kempten  Allgäu")
de$county = str_replace_all(de$county, "Lindau", "Lindau  Bodensee")
de$county = str_replace_all(de$county, "Greater Hannover", "Region Hannover")
de$county = str_replace_all(de$county, "Altenkirchen", "Altenkirchen  Westerwald ")
de$county = str_replace_all(de$county, "Altenkirchen", "Frankenthal  Pfalz ")
de$county = str_replace_all(de$county, "Ludwigshafen", "Ludwigshafen am Rhein")
de$county = str_replace_all(de$county, "Stadtverband Saarbrücken", "Regionalverband Saarbrücken")
de$county = str_replace_all(de$county, "Bitburg Prüm", "Eifelkreis Bitburg Prüm")


sf = st_read('data/location_codes/ger_shp/DEU_adm2.shp')
sf = sf%>%
  mutate(county = str_replace_all(NAME_2, '-', ' '))%>%
  select(c(NAME_1, NAME_2, county))%>%
  st_drop_geometry()%>%
  distinct()

sf$county = str_replace_all(sf$county, '\\(', ' ')
sf$county = str_replace_all(sf$county, '\\)', ' ')

de = left_join(de, sf)
de = de%>%
  mutate(NAME_1 = ifelse(grepl(county, 'Berlin'), 'Berlin', NAME_1))

# which don't match - should be 0
nas = de%>%
  subset(is.na(NAME_1))%>%
  distinct(region, county)

de = de%>%
  rename(adm1 = NAME_1, adm2 = NAME_2)%>%
  select(c(year, month, isoweek, adm1, adm2, num_cases))

de$country = 'Germany'
de$continent = 'Europe'

write.csv(de, 'data/human/processed/hanta/DE_hanta.csv', row.names = F)

### FINLAND ###
fi = read.csv('data/human/raw/hanta/Finland_hantavirus.csv')
fi = fi%>%
  mutate(num_cases = ifelse(is.na(num_cases), 0, num_cases))

# region names are in Swedish fyi
fi = fi%>%
  subset(Region != "Hela landet")%>% # means throughout the country
  rename(adm1 = Region,
         date = Date)%>%
  mutate(date = as.Date(paste0(date, "-01"), format="%Y-%m-%d"))

# get year and month
fi = fi%>%
  mutate(month = month(date),
         year = year(date),
         country = 'Finland')

fi$continent = 'Europe'  
fi$disease = 'HFRS'
write.csv(fi, 'data/human/processed/hanta/FI_hanta.csv', row.names = F)

# simplify Germany, Finland, and Sweden and combine
de$disease = 'HFRS'
de = de%>%
  group_by(continent, country, year, month, disease)%>%
  summarize(num_cases=sum(num_cases))
de$year = as.numeric(de$year)
 
sw = sw%>%
  group_by(continent, country, year, month, disease)%>%
  summarize(num_cases=sum(num_cases)) 

fi = fi%>%
  group_by(continent, country, year, month, disease)%>%
  summarize(num_cases=sum(num_cases)) 

# checked that they each match EU numbers before this
# maybe input test though
### EUROPE DATA ###
eu = read.csv('data/human/raw/hanta/Europe_hantavirus.csv')
eu = eu%>%
  rename(country = RegionName,
         num_cases = NumValue)%>%
  mutate(year = as.numeric(str_split_i(Time, '-', 1)),
         month = as.numeric(str_split_i(Time, '-', 2)),
         num_cases = as.numeric(num_cases),
         disease = 'HFRS',
         continent = 'Europe')%>%
  select(c(year, month, continent, country, num_cases, disease))

# remove NAs (no data reported)
eu = eu%>%
  subset(!is.na(num_cases))

eu = do.call(rbind, c(eu, de, sw, fi))

# remove repeated dates
eu = eu%>%distinct()

write.csv(eu, 'data/human/processed/hanta/EU_hanta.csv', row.names = F)

#----- ASIA ----- 

### CHINA ###
dir = 'data/human/raw/hanta/china/'
cn = lapply(list.files(dir), function(x){
   y = readxl::read_excel(paste0(dir, x))
   y$year = str_split_i(str_split_i(x, '-', -1), '\\.', 1)
   y$month = str_split_i(str_split_i(x, '-', 1), '\\.', 1)
   return(y)
})

cn = do.call(rbind, cn)
cn$month = as.numeric(cn$month)
cn$year = as.numeric(cn$year)
cn$country = 'China'

t = cn%>%
  subset(year == 2023)
cn = cn%>%
  subset(Diseases == 'HFRS' | Diseases == 'Epidemic hemorrhagic fever')


cn$disease = 'HFRS'
cn$num_cases = as.numeric(gsub(",","",cn$Cases))

cn = cn%>%
  select(c(year, month, country, num_cases, disease))

cn2 = readxl::read_excel('data/human/raw/hanta/China_Hantavirus_2019.xlsx', skip=1)
cn2$disease = 'Hantavirus'
cn2$country = 'China'
cn2 = cn2%>%rename(num_cases = cases)

cn = rbind(cn, cn2)

# add NAS for break line
na = data.frame(year=2019,
                month =c(1:10),
                num_cases=NA,
                country='China',
                disease ='HFRS')

cn = rbind(cn, na)
cn$continent = 'Asia'
write.csv(cn, 'data/human/processed/hanta/CN_hanta.csv', row.names = F)

#-------------------- Combine Hantavirus Data -------------------- 
df = do.call(bind_rows, 
             lapply(list.files('data/human/processed/hanta', full.names = T), 
                    read.csv))

df = df%>%
  mutate(dummy_date = as.Date(paste0(year, '-', month, '-01'), format="%Y-%m-%d"))

eu_removals = df%>%
  subset(continent == 'Europe')%>%
  group_by(country)%>%
  summarize(cases = sum(num_cases))%>%
  subset(cases < 300)

eu_removals = unique(eu_removals$country)
eu_removals = c(eu_removals, 'EU', 'EU/EEA')

df = df%>%
  subset(!country %in% eu_removals)

# create country factor ordering
order = df%>%
  select(c(continent, country))%>%
  distinct()%>%
  arrange(continent)

df$country = factor(df$country, order$country)

#-------------------- Plot Hantavirus Data --------------------
e1 = df%>%
  #subset(continent == 'Europe')%>%
  group_by(continent, country, dummy_date)%>%
  summarize(num_cases = sum(num_cases))%>%
  ggplot()+
  geom_line(aes(dummy_date, num_cases, col=continent))+
  scale_x_date(name="Date",
               breaks = seq(as.Date('1995-01-01'), as.Date('2025-01-01'), by="5 years"),
               date_labels="%b-%Y",
               date_minor_breaks = 'year')+
  facet_grid(country~., scales="free_y")+
  scale_color_discrete(name='Region')+
  labs(y = 'Total Cases')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        legend.position="bottom")

e2 = df%>%
  #subset(continent == 'Europe')%>%
  mutate(dummy_date2 = as.Date(paste0(month, '-01-2000'), format="%m-%d-%Y"))%>%
  group_by(country, year, month, dummy_date2)%>%
  summarize(num_cases = sum(num_cases, na.rm=T))%>%
  ggplot()+
  geom_line(aes(dummy_date2, num_cases, col=year, group=year))+
  scale_colour_viridis_c(name = 'Year')+
  scale_x_date(name="Month", date_labels="%b",
               date_minor_breaks = 'month')+
  #scale_color_discrete(name="Year")+
  facet_grid(country~., scales="free_y")+
  labs(y = 'Total Cases')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        legend.position="bottom")


fig1 = e1 + e2 

ggsave(plot=fig1, filename='viz/Hantavirus_Timeseries_v2.png', width=12, height=10, units="in")

#-------------------- Process Lassa Data -------------------- 
las = read.csv('data/human/raw/lassa/lassafever_confirmed_admin1.csv')

las = las%>%
  rename(adm1 = adminName)%>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         month = month(date))%>%
  group_by(year, month, epiWeek, adm1)%>%
  summarize(num_cases = sum(value))%>%
  ungroup()

las$disease = 'Lassa fever'
las$country = 'Nigeria'

las = las%>%
  mutate(dummy_date = as.Date(paste0(year, '-', month, '-01'), format="%Y-%m-%d"))

p1 = las%>%
  group_by(country, dummy_date)%>%
  summarize(num_cases = sum(num_cases, na.rm=T))%>%
  ggplot()+
  geom_line(aes(dummy_date, num_cases))+
  # scale_x_date(name="month",
  #              breaks = "1 year",
  #              date_labels="%b-%Y",
  #              date_minor_breaks = '6 months')+
  # facet_grid(country~., scales="free_y")+
  labs(x = 'Year', y = 'Total Cases')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(colour="black", fill="white"))

p2 = las%>%
  subset(!is.na(year))%>%
  mutate(dummy_date2 = as.Date(paste0(month, '-01-2000'), format="%m-%d-%Y"))%>%
  group_by(country, year, month, dummy_date2)%>%
  summarize(num_cases = sum(num_cases, na.rm=T))%>%
  ggplot()+
  geom_line(aes(dummy_date2, num_cases, col=year, group=year))+
  scale_x_date(name="Month", date_labels="%b",
               date_minor_breaks = 'month')+
  scale_colour_viridis_c(name = 'Year', breaks=seq(2012, 2022, by=2))+
  facet_grid(country~., scales="free_y")+
  labs(y = 'Total Cases')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(colour="black", fill="white"))

fig = p1 + p2
ggsave(plot=fig, filename='viz/lassa_timeseries.png', width=8, height=3, units="in", dpi=300) 

#-------------------- Combine lassa and hanta  -------------------- 
df = df%>%
  select(c(country, adm1, year, month, num_cases, continent))

df$disease = 'Hantavirus'
las = las%>%
  group_by(year, month, adm1, disease, country)%>%
  summarize(num_cases=sum(num_cases))
las$continent = 'Africa'

out = rbind(df, las)
write.csv(out, 'data/case_data.csv', row.names = F)

#-------------------- Add geo information and create summary dataset  -------------------- 
df = read.csv('data/case_data.csv')

df_sum = df%>%
  group_by(country, year, month)%>%
  mutate(monthcases = sum(num_cases, na.rm=T))%>%
  group_by(country)%>%
  summarize(minyear = min(year),
            maxyear = max(year),
            max_monthly_cases = max(monthcases, na.rm=T),
            total_cases = sum(monthcases,na.rm=T))%>%
  mutate(country = ifelse(country == 'USA', 'United States of America', country))

write.csv(df_sum, 'data/human/processed/country_list.csv', row.names = F)

earth = st_read('data/location_codes/world-administrative-boundaries.geojson')
earth = earth%>%
  select(c(continent, name, iso3, geo_point_2d, geometry))%>%
  rename(country = name)

# add shapefile of country
df_sum = df_sum%>%
  left_join(earth, by='country')

st_write(df_sum, 'data/location_codes/countries.geojson')



