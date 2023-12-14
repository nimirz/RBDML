# load packages
library(tidyverse)
library(sf)
library(terra)

#-------------------- Load host/virus association -------------------- 
# Using clover database available at: https://doi.org/10.5281/zenodo.5167655
df = read.csv("data/host/CLOVER_0.1_MammalViruses_AssociationsFlatFile.csv")

# subset out hantaviruses
df = df%>%
  #subset(HostOrder == 'rodentia')%>% # only use rodents
  subset(VirusFamily == 'hantaviridae' | Virus == 'lassa mammarenavirus')%>%
  select(c(Host, Virus, HostClass, HostOrder, HostFamily, HostGenus, VirusClass, VirusOrder, VirusFamily, VirusGenus))%>%
  distinct()

# remove nas
df = df%>%
  subset(!is.na(Virus))

# associate virus with a disease
# if knwon to case HPS or HFRS included along with abbrev
dz = read.csv('data/host/virus_disease_assoc.csv')%>%
  select(-c(Source))

df = left_join(df, dz, by="Virus")

# select only those known to cause disease and focus on rodents
df = df%>%
  subset(Disease != 'UNK')%>%
  subset(HostOrder == 'rodentia')

#-------------------- Load species range and habitat prefs -------------------- 
ranges = st_read('data/host/MAMMALS_TERRESTRIAL_ONLY.shp')
ranges = ranges%>%
  subset(order_ == 'RODENTIA')%>%
  rename(Host = sci_name)%>%
  mutate(Host = str_to_lower(Host))%>%
  subset(presence == 1)%>%
  select(c(Host, category, SHAPE_Leng, SHAPE_Area))

out = merge(ranges, df)

hanta = out%>%
  subset(VirusFamily == 'hantaviridae')

# create raster to see what it looks like?
ext          = ext(-180, 180, -90, 90) # whole world
gridsize     = 1/120 #0.083 degrees
plain_raster = rast(ext, res=gridsize)

r = rasterize(hanta, plain_raster, fun=sum)

 plot(st_geometry(out))
