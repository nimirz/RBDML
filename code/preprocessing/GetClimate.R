library(ncdf4)



nc_data = nc_open('~/Downloads/precip.2023.nc')
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
par = ncvar_get(ncdata, 'precip')
fillvalue <- ncatt_get(ncdata, "precip", "_FillValue")
nc_close(ncdata)

par[par == fillvalue$value] = NA

par_slice = par[,,1]

precip = rast(t(par_slice), crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
plot(precip)
r <- flip(precip, direction='y')



library(pastclim)

set_data_path(path_to_nc = "~/Projects/HumAnimOverlap/pastclim")
download_dataset(dataset="Beyer2020", bio_variables="biome")
download_dataset(dataset="")

# only goes up to 0
get_time_steps("Beyer2020")

clim = region_slice(
  time_bp=0,
  bio_variables="biome",
  dataset="Beyer2020"
)
plot(clim)
get_biome_classes("Beyer2020")

plot(ecorast)

