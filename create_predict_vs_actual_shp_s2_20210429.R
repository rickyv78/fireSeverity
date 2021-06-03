library(sf)
library(tidyverse)
library(here)

#######################################
### Sentinel ###

list <- list.files(here("predict"), pattern = "predict_ibra_s2_", full.names = TRUE)
df <- lapply(list, read.csv) %>% bind_rows()
length(unique(df$id))

shp1 <- st_read(paste0(here("data-record"),"\\fieldata_2021-02-02_LL.shp"), 
                stringsAsFactors = FALSE) %>% dplyr::select(Burn_ID, Site) %>% rename(BURNID = Burn_ID)
shp2 <- st_read(paste0(here("data-record"),"\\fieldata_2021-03-17_LL.shp"), 
                stringsAsFactors = FALSE) %>% dplyr::select(BURNID, Site) 

shp3 <- st_read(paste0(here("data-record"),"\\fieldata_2021-04-07_LL.shp"), 
                stringsAsFactors = FALSE) %>% dplyr::select(BURNID, ID) %>% rename(Site = ID)

shp <- shp1 %>% rbind(shp2) %>% rbind(shp3) %>% 
  mutate(id = paste0(BURNID, "-", Site)) %>%
  left_join(df, by = c("id", "BURNID")) 
  
st_write(shp, paste0(here("predict"), "\\predict.s2.ibra.", Sys.Date(), ".shp"), delete_dsn=TRUE)

