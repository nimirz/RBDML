# Visualization of case data and climate data

# Packages:
library(dplyr)
library(ggplot2)

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

#-------------------- Plot Lassa Data -------------------- 
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

#-------------------- Plot climate data -------------------- 



#-------------------- Overall data  -------------------- 



