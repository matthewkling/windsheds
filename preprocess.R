
library(raster)
library(tidyverse)
library(windscape)

unwrap <- function(x, width=20){
      x <- x %>% crop(extent(180-width, 180, -90, 90)) %>% shift(-360) %>% merge(x)
      x <- x %>% crop(extent(-180, -180+width, -90, 90)) %>% shift(360) %>% merge(x)
      x
}

wind <- stack("data/windrose_p1_1980_2009.tif") %>% rotate() %>% unwrap(180) %>% add_coords()

downwind <- wind %>% transition_stack(windflow, directions=8, symm=F, direction="downwind")
upwind <- wind %>% transition_stack(windflow, directions=8, symm=F, direction="upwind")

saveRDS(downwind, "data/downwind.rds")
saveRDS(upwind, "data/upwind.rds")

plot(wind[[1]])



s <- data.frame(lon=-122.262277, lat=37.871444) %>% as.matrix()
ac <- accCost(downwind, s) %>% "/"(3600)
acont <- rasterToContour(ac)
