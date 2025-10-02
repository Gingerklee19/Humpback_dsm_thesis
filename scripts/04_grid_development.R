# 04_grid_development.R
# This code was provided by Angus Henderson Helping me make a grid
# This code creates a grid for my study area 

# Load in packages
library(sf)
library(dplyr)
library(basf)
library(tidyverse)
library(Distance)
library(dsm)
library(tidyr)

# Load in segmented data 
load("segmentdata.Rdata")

# Spatial projection. 
p  <- "+proj=laea +lat_0=-90 +lon_0=-57 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
p <- st_crs(p)

# Below is for plotting. 
# getting the bathy and coastlines. 
# you can use this method for any plots. 

w <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", country = "Antarctica") |> 
  st_transform(st_crs(segs)) 

bathy <- marmap::getNOAA.bathy(lon1 = -60, lon2 = -65,
                               lat1 = -64, lat2 = -65.5,
                               resolution = 1) #change up or down.
b <- marmap::as.xyz(bathy)

bsf <- b |> st_as_sf(coords = c("V1","V2" ), remove = F) |>
  st_set_crs(4326) |>
  st_transform(p) |>
  mutate(V1 = st_coordinates(geometry)[,'X'],
         V2 = st_coordinates(geometry)[,'Y'])

bstars <- marmap::as.SpatialGridDataFrame(bathy) |> stars::st_as_stars() |>
  st_transform(p)

coast <- marmap::as.SpatialGridDataFrame(bathy) |> stars::st_as_stars() |>
  stars::st_contour(breaks =  c(0), contour_lines = F) |> st_transform(p)

bcon <- marmap::as.SpatialGridDataFrame(bathy) |> stars::st_as_stars() |>
  stars::st_contour(breaks =  c(-4000,-3000,-2000, -1000,-500,-100), contour_lines = F) |> st_transform(p)

blines <- marmap::as.SpatialGridDataFrame(bathy) |> stars::st_as_stars() |>
  stars::st_contour(breaks =  c(-4000,-3000,-2000, -1000,-500,-100), contour_lines = T) |> st_transform(p)

cols = RColorBrewer::brewer.pal(3, "RdYlGn")


# Make a grid

grid <- sf::st_make_grid(st_bbox(coast), what = "polygons",cellsize = 2500 , crs = p) |> 
  st_intersection( coast) |> 
  st_difference(coast[2,]) |> 
  st_as_sf()

grid$area <- st_area(grid) |> as.numeric()
grid$area_km2 <- as.numeric(st_area(grid)/10^6)
grid.poly <- grid

grid <- st_centroid(grid)

grid <- grid |> 
  rename(geometry = x) |> 
  mutate(x= st_coordinates(geometry)[,1], y= st_coordinates(geometry)[,2]) |> 
  st_transform(st_crs(4326)) |> 
  mutate(lon= st_coordinates(geometry)[,1], lat= st_coordinates(geometry)[,2])|> 
  st_transform(p ) |> 
  dplyr::select(everything(), geometry) |> 
  mutate(poly = grid.poly$x)

plot(grid |> filter(area_km2 > 0.5))

grid <- grid |> filter(area_km2 > 1)

plot(grid, axes = T)
points(x= -165000,y= 2850000 , col = "red")
points(x= -300000,y= 2700000 , col = "red")
points(x= -130000,y= 2700000 , col = "red")
points(x= -130000,y= 2850000 , col = "red")
points(x= -165000,y= 2850000 , col = "red")


coords <- matrix(c(
  -165000, 2850000,
  -300000, 2700000,
  -130000, 2700000,
  -130000, 2850000,
  -165000, 2850000  # Closing the polygon
), ncol = 2, byrow = TRUE)

polygon <- st_polygon(list(coords))
polygon_sf <- st_sfc(polygon, crs = p) 
grid <- grid |> st_difference( polygon_sf) 
plot(grid) # this is the center points of the grid
plot(grid.poly, add=T) # these are the squares. I have cut out the weddell. 
grid

save(grid, grid.poly, file = "4.proj_grid.Rdata")
