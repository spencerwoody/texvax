
get_vector_tiles <- function(bbox){
  mz_box=mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)
  mz_vector_tiles(mz_box)
}

vax_sub <- vax %>% filter(PO_NAME == "Austin")

travis <- st_union(vax_sub$geometry)
travis_bbox <- st_bbox(travis)

travis_vector_tiles <- get_vector_tiles(travis_bbox)
names(travis_vector_tiles)

travis_water <- as_sf(travis_vector_tiles$water) ## %>%
  ## mutate(geometry = st_intersection(geometry, travis))

travis_roads <- as_sf(travis_vector_tiles$roads) ## %>%
  ## mutate(geometry = st_transform(geometry, st_crs(travis)))
## %>% 
  ## mutate(geometry = st_crop(geometry, travis))

travis_roads1 <- travis_roads %>%
  mutate(st_transform(geometry, st_crs(travis))) %>% 
  filter(kind == "highway") %>%
  pull(geometry) %>%
  st_union() %>%
  st_transform(st_crs(travis)) %>% 
  st_intersection(travis)

travis_water1 <- travis_water %>%
  mutate(geometry = st_transform(geometry, st_crs(travis))) %>% 
  ## filter(kind == "highway") %>%
  pull(geometry) %>%
  st_union() %>%
  st_transform(st_crs(travis)) %>% 
  st_intersection(travis)

travis_water3 <- travis_water %>% mutate(geometry2 = travis_water1)
