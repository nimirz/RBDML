# Visualization of case data and climate data

# Packages:
library(dplyr)
library(ggplot2)
library(patchwork)

#-------------------- Load datasets --------------------
df = read.csv('~/Projects/RBDML/data/processed/case_data.csv')
clim = read.csv('~/Projects/RBDML/data/processed/climate_data.csv')



### CHECK FOR MATCHING NAMES ###
# fix germany
df$adm1 = ifelse(df$adm1 == 'Baden-Württemberg', 'Baden-Wuerttemberg', df$adm1)
df$adm1 = ifelse(df$adm1 == 'Thüringen', 'Thueringen', df$adm1)
df$adm1 = ifelse(df$adm1 == 'Nuble', 'Biobio', df$adm1)
df$continent = ifelse(df$country == 'Germany', 'Europe', df$continent)

# check for matching adm1 names
df_adm1 = df%>%
  select(c('country', 'adm1'))%>%
#  subset(country == 'Finland')%>%
  distinct()

clim_adm1 = clim%>%
  select(c('country', 'adm1'))%>%
  mutate(name = 'clim')%>%
  #subset(country == 'Germany')%>%
  distinct()

ma = left_join(df_adm1, clim_adm1)

# SHOULD BE 0
nas = ma%>%
  subset(is.na(name))%>%
  #select(c(adm1, ))%>%
  distinct()

#-------------------- Case data  --------------------
# create date for easier plotting
df = df%>%
  mutate(dummy_date = as.Date(paste0(year, '-', month, '-01'), format="%Y-%m-%d"))

# create country factor ordering
order = df%>%
  select(c(continent, country))%>%
  distinct()%>%
  arrange(continent)

df$country = factor(df$country, order$country)

e1 = df%>%
  #subset(continent == 'Europe')%>%
  #subset(disease == 'Hantavirus')%>%
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

#ggsave(plot=fig1, filename='viz/Hantavirus_Timeseries_v2.png', width=12, height=10, units="in")


#-------------------- Plot climate data -------------------- 
# add continent
conts = df%>%
  select(c(country, continent))%>%
  distinct()

clim = left_join(clim, conts)
clim$country = factor(clim$country, order$country)

clim = clim%>%
  mutate(dummy_date = as.Date(paste0(year, '-', month, '-01'), format="%Y-%m-%d"))

# temperature
clim%>%
  group_by(continent, country, adm1, dummy_date)%>%
  subset(measure == 'mean_lst')%>%
  summarize(mean_temp = mean(value))%>%
  ggplot()+
  geom_line(aes(dummy_date, mean_temp, col=continent, group=adm1))+
  scale_x_date(name="Date",
               #breaks = seq(as.Date('1995-01-01'), as.Date('2025-01-01'), by="5 years"),
               date_labels="%b-%Y",
               date_minor_breaks = 'year')+
  facet_grid(country~.)+
  scale_color_discrete(name='Region')+
  labs(y = 'Mean Land Surface Temperature (oC)')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        legend.position="bottom")


# precip
clim%>%
  group_by(continent, country, dummy_date, adm1)%>%
  subset(measure == 'precipitation')%>%
  summarize(precip = sum(value))%>%
  ggplot()+
  geom_line(aes(dummy_date, precip, group=adm1, col=continent))+
  scale_x_date(name="Date",
               #breaks = seq(as.Date('1995-01-01'), as.Date('2025-01-01'), by="5 years"),
               date_labels="%b-%Y",
               date_minor_breaks = 'year')+
  facet_grid(country~., scales="free_y")+
  scale_color_discrete(name='Region')+
  labs(y = 'Precipatation (mm)')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        legend.position="bottom")

out = clim%>%
  subset(country == 'Finland')%>%
  subset(measure == 'precipitation')


# EVI
df%>%
  subset(country != 'China')%>%
  #group_by(continent, country, dm1)%>%
  #subset(measure == 'precipitation')%>%
  #summarize(precip = sum(value))%>%
  ggplot()+
  geom_histogram(aes(num_cases, fill=continent))+
  facet_grid(country~., scales="free")+
  scale_fill_discrete(name='Region')+
 #labs(y = 'Precipatation (mm)')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        legend.position="bottom")

df%>%
  subset(country == 'China')%>%
  #group_by(continent, country, dm1)%>%
  #subset(measure == 'precipitation')%>%
  #summarize(precip = sum(value))%>%
  ggplot()+
  geom_histogram(aes(num_cases, fill=continent))+
  facet_grid(country~., scales="free")+
  scale_fill_discrete(name='Region')+
  #labs(y = 'Precipatation (mm)')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        legend.position="bottom")


ng = df%>%subset(country == 'Nigeria')
ng%>%
  ggplot()+
  geom_histogram(aes(num_cases))

df%>%
  ggplot()+
  geom_histogram(aes(num_cases))
