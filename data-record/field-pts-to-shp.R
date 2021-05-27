library(tidyverse)
library(sf)

wdir <- "Z:\\DEC\\Fire_regimes_2018-074\\DATA\\Working\\fireSeverity_v2\\data-record\\"
csv <- read.csv(paste0(wdir, "field-data_2021-02-02.csv"))
colnames(csv)[8] <- "OzCBI"
csv <- dplyr::select(csv, ID, Burn.ID, Site, Lat, Long, drop.outlier, OzCBI)

csvi <- mutate(csv, id = paste0(Burn.ID, "-", Site))
grp <- group_by(csvi, id) %>%
  summarise(n = n()) %>%
  filter(n != 1)

pt <- st_as_sf(csv ,coords=c("Long", "Lat"), crs=4326)
pt <- filter(pt, Site != "CJL7")
plot(pt[,2])
st_write(pt, paste0(wdir, "fieldata_", Sys.Date(), "_LL.shp"), append=FALSE)
