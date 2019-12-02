
library(raster)
library(tidyverse)
library(windscape)
library(gdistance)

unwrap <- function(x, width=20){
      x <- x %>% crop(extent(180-width, 180, -90, 90)) %>% shift(-360) %>% merge(x)
      x <- x %>% crop(extent(-180, -180+width, -90, 90)) %>% shift(360) %>% merge(x)
      x
}


for(i in c("annual", "DJF", "MAM", "JJA", "SON")){
      
      wind <- stack(paste0("data/windrose_p1_wnd10m_", i, ".tif")) %>% rotate() %>% unwrap(180) %>% add_coords()
      
      wind %>% transition_stack(windflow, directions=8, symm=F, direction="downwind") %>%
            saveRDS(paste0("data/downwind_", i, ".rds"))
      
      wind %>% transition_stack(windflow, directions=8, symm=F, direction="upwind") %>%
            saveRDS(paste0("data/upwind_", i, ".rds"))
}