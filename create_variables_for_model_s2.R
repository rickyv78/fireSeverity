#load libraries
library(tidyverse)
library(caret)
library(broom)
library(kableExtra)
library(lubridate)
library(sf)
library(raster)

### Joe's data 

# set paths and load data
fdir <- "Z:\\DEC\\Fire_regimes_2018-074\\DATA\\Working\\fireSeverity\\rds.files\\"
vdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\"

# read in and clean up
df <- st_read(paste0(vdir, "set_2021jf_s2\\fieldata_2021-04-07_LL.shp"), stringsAsFactors = FALSE) %>%
  st_drop_geometry() %>%  mutate(id = paste0(BURNID, "-", ID)) %>% 
  dplyr::select(id, CBI_Plt, BURNID) 
colnames(df) <- c("id", "OzCBI", "BURNID")
df$OzCBI <- as.numeric(df$OzCBI)
df <- na.omit(df)

csv.lst <- list.files(paste0(vdir, "set_2021jf_s2\\tmp_10m"), pattern = "10m", full.names = TRUE)
csv.lst2 <- list.files(paste0(vdir, "set_2021jf_s2\\tmp_10m"), pattern = "20m", full.names = TRUE)

ts.j1 <- lapply(csv.lst, read.csv) %>% bind_rows()
colnames(ts.j1)[1:4] <- c("b2", "b3", "b4", "b8")
ts.j2 <- lapply(csv.lst2, read.csv) %>% bind_rows()

ts.j <- left_join(ts.j1, ts.j2, by = c("site", "date", "name"))

ts.j <- ts.j %>% mutate(BURNID = str_split_fixed(name, "_", 2)[,1]) %>%
  rename(id = site)

tmp <- filter(df, BURNID == "DON006")

df.joe <- df %>% left_join(ts.j, by = c("id", "BURNID")) %>%
  na.omit()

vdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\fireSeverity\\"
fire.dates.joe <- read.csv(paste0(vdir, "data-record\\burn-dates-20210407_jf.csv"), stringsAsFactors = FALSE) 
fire.dates.joe$start <- dmy(fire.dates.joe$start)
fire.dates.joe$end <- dmy(fire.dates.joe$end)
fire.dates.joe$BURNID <- str_replace(fire.dates.joe$BURNID, "_", "")
fire.ibra.joe <- dplyr::select(fire.dates.joe, BURNID, ibra)
fire.dates.joe <- dplyr::select(fire.dates.joe, BURNID, start, end)

  ################################ Val 1 #################################
  dropOutliers <- F
  
  # set paths and load data
  fdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\rds.files\\"
  vdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\fireSeverity\\"
  
  # script inputs
  df1 <- read_csv(paste0(vdir, "data-record\\data-record_2021-05-26.csv"))

  #if (dropOutliers == T){
  #  df1 <- filter(df1, drop_outli == "No")
  #}else{
  #  df1 <- filter(df1, drop_outli == "No" |  Severity_class == "U")
  #}

  colnames(df1)[c(2,7)]<- c("BURNID", "OzCBI") 
  
  df <- df1 %>% mutate(id = paste0(BURNID, "-", Site)) %>%
    dplyr::select(id, BURNID, OzCBI)
  
 #### band values
 csv.lst <- list.files("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\plots.s2a\\tmp_10m", full.names = TRUE, pattern = "10m")
 ts.10m <- lapply(csv.lst, read.csv) %>% bind_rows()
 colnames(ts.10m)[1:5] <- c("b2", "b3", "b4", "b8", "id")
 
 csv.lst <- list.files("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\plots.s2a\\tmp_10m", full.names = TRUE, pattern = "20m")
 ts.20m <- lapply(csv.lst, read.csv) %>% bind_rows()
 colnames(ts.20m)[7] <- "id"
 
 ts.v1 <- left_join(ts.10m, ts.20m, by = c("id", "date", "name"))
  
  
 csv.lst <- list.files("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\set_20210311\\tmp_10m", full.names = TRUE, pattern = "10m")
 ts.10m <- lapply(csv.lst, read.csv) %>% bind_rows()
 colnames(ts.10m)[1:5] <- c("b2", "b3", "b4", "b8", "id")
 
 csv.lst <- list.files("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\set_20210311\\tmp_10m", full.names = TRUE, pattern = "20m")
 ts.20m <- lapply(csv.lst, read.csv) %>% bind_rows()
 colnames(ts.20m)[7] <- "id"
 
 ts.v2 <- left_join(ts.10m, ts.20m, by = c("id", "date", "name"))
 
 ts.v <- bind_rows(ts.v1, ts.v2)
 ts.v$name <- str_replace_all(ts.v$name, "_", "-")
 ts.v <- mutate(ts.v, BURNID = str_split_fixed(name, "-", 2)[,1])
 
 tmp <- filter(ts.v, BURNID == "DON006")
 unique(ts.v$BURNID)
 
 ts <- bind_rows(ts.v, ts.j)
 
 df.val <- left_join(df, ts, by = c("id", "BURNID"))
 
 tmp <- filter(ts, BURNID == "DON006")
  
  

# fire dates and ibra
  fire.dates <- read.csv(paste0(vdir, "\\data-record\\burn-dates_20210510_val.csv"), stringsAsFactors = FALSE) 
  fire.ibra.val <- dplyr::select(fire.dates, Burn.ID, ibra)
  colnames(fire.ibra.val)[1] <- "BURNID"
  
  fire.dates <- fire.dates %>% gather(action, date,  3:10)%>% 
    dplyr::select(Burn.ID, Burn.name, action, date) 
    
  fire.dates$date <- dmy(fire.dates$date)
  fire.dates <- na.omit(fire.dates)
  
  dateBurns <- unique(fire.dates$Burn.ID)
  fire.dates.val <- data.frame(BURNID = as.character(), start = as.Date(character()), 
                              end = as.Date(character()), stringsAsFactors = FALSE)[1:length(dateBurns), ]
  i <-5
  for (i in 1:length(dateBurns)){
    r <- filter(fire.dates, Burn.ID == dateBurns[i])
    fire.dates.val$BURNID[i] <- dateBurns[i]
    fire.dates.val$start[i] <- as.Date(min(r$date, na.rm = TRUE))
    fire.dates.val$end[i] <- as.Date(max(r$date, na.rm = TRUE))
  }
  
  #fire.dates.val <- filter(fire.dates.val, BURNID != "SWC057")
#################
### unburnt pts ###

  csv.lst1 <- list.files("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\unburntPts\\tmp_s2", 
                        full.names = TRUE, pattern = "10m")
  csv.lst2 <- list.files("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\unburntPts\\tmp_s2", 
                        full.names = TRUE, pattern = "20m")
  
  ub.ts1 <- lapply(csv.lst1, read.csv) %>% bind_rows()
  colnames(ub.ts1)[1:4] <- c("b2", "b3", "b4", "b8")
  ub.ts2 <- lapply(csv.lst2, read.csv) %>% bind_rows()
  
  ub.ts <- left_join(ub.ts1, ub.ts2, c("site", "date", "name"))
  ub.ts <- mutate(ub.ts, BURNID = str_split_fixed(name, "-", 2)[,1])
  
  colnames(ub.ts)[5] <- "id"
  ub.ts <- mutate(ub.ts, OzCBI =0)
  tmp <- filter(ub.ts, BURNID == "SWC046")
  tmp <- ub.ts %>% group_by(BURNID, id) %>%
    summarise(n = n())
  
#################
### join data sets
  
  fire.ibras <- bind_rows(fire.ibra.joe, fire.ibra.val)
  fire.dates.all <- bind_rows(fire.dates.val, fire.dates.joe)
  fire.dates.all <- fire.dates.all %>% group_by(BURNID) %>%
    summarise(start = min(start), end = max(end))
  fire.dates.all <- left_join(fire.dates.all, distinct(fire.ibras), by = "BURNID")
  fdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\rds.files\\"
  
  saveRDS(fire.dates.all, paste0(vdir, "rds.files\\site.date.s2.", Sys.Date()))
  
  df.all <- bind_rows(df.val, df.joe, ub.ts)
  saveRDS(df.all, paste0(vdir, "rds.files\\site.allData.s2.", Sys.Date()))
  
  #df.all <- filter(df.all, BURNID == "SWC046" & OzCBI == 0)
  
#################
### calculate indicies
  df.all <- mutate(df.all, ndvi = (b8-b4)/(b8+b4),
                   nbr = (b8-b12)/(b8+b12),
                   i35 = (b4+b11)/2,
                   ndwi = (b8-b11)/(b8+b11))
  #tmp <- filter(df.all, BURNID == "DON006")

  fire.date.names <- unique(fire.dates.all$BURNID)
  df.all <- filter(df.all, BURNID %in% fire.date.names)

  ids <- unique(df.all$id)
  
  burn.df <- data.frame(id = as.character(), BURNID = as.character(), OzCBI = as.character(), 
                        preNBR = as.numeric(), preNDVI = as.numeric(), preNIR = as.numeric(), 
                        prei35 = as.numeric(), preNDWI = as.numeric(), postNBRmin = as.numeric(), 
                        postNIRmin = as.numeric(), postNDVImin = as.numeric(), posti35max = as.numeric(),
                        postNDWImin = as.numeric(), stringsAsFactors = FALSE)[1:length(ids), ]
  
  burn.df$id <- ids
  burn.df$BURNID <- str_split_fixed(ids, "-", 2)[,1]
  unique(burn.df$BURNID)

  #tmp <- filter(burn.df, BURNID == "DON006")
  i <- 2
  for(i in 1:nrow(burn.df)){
    fire.date.i <- unique(fire.dates.all$start[which(fire.dates.all$BURNID == burn.df$BURNID[i])])
    fire.end.i <- unique(fire.dates.all$end[which(fire.dates.all$BURNID == burn.df$BURNID[i])])
    
    ind1 <- filter(df.all, date < fire.date.i &  id == burn.df$id[i])
  
    if (nrow(ind1) == 0){
      ind1 <- filter(df.all, id == burn.df$id[i])
      ind1 <- ind1[1,]
    }
    
    burn.df$OzCBI[i] <- ind1$OzCBI[1]
    
    if (nrow(ind1) !=0){
      burn.df$preNBR[i] <- ind1$nbr[nrow(ind1)]
      burn.df$preNDVI[i] <- ind1$ndvi[nrow(ind1)]
      burn.df$prei35[i] <- ind1$i35[nrow(ind1)]
      burn.df$preNIR[i] <- ind1$b4[nrow(ind1)]
      burn.df$preNDWI[i] <- ind1$ndwi[nrow(ind1)]
      
      ind2 <- filter(df.all, date > fire.date.i & id == burn.df$id[i])
      
      ind3 <- filter(ind2, date < fire.end.i)
      if (nrow(ind3) !=0){
        burn.df$postNBRmin[i] <- min(ind3$nbr)
        burn.df$postNDVImin[i] <- min(ind3$ndvi)
        burn.df$postNIRmin[i] <- min(ind3$b4)
        burn.df$posti35max[i] <- max(ind3$i35)
        burn.df$postNDWImin[i] <- min(ind3$ndwi)
      }else{
        burn.df$postNBRmin[i] <- ind2$nbr[1]
        burn.df$postNDVImin[i] <- ind2$ndvi[1]
        burn.df$postNIRmin[i] <- ind2$b4[1]
        burn.df$posti35max[i] <- ind2$i35[1]
        burn.df$postNDWImin[i] <- ind2$ndwi[1]
        
      }
    }
  }
  
  burn.dfv <- burn.df %>% na.omit() %>% 
    mutate(dNBRmax = preNBR - postNBRmin,
           dNIRmax = preNIR - postNIRmin, 
           di35max = prei35 - posti35max,
           dNDVImax = preNDVI - postNDVImin, 
           dNDWImax = preNDWI - postNDWImin,
           RBRmax = (preNBR - postNBRmin) / (preNBR + 1.001))
  tmp <- filter(burn.dfv, BURNID == "SWC046")
  fdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\fireSeverity\\rds.files\\"
  saveRDS(burn.dfv, paste0(fdir, "site.var.s2.", Sys.Date()))
  
  tmp <- filter(burn.dfv, BURNID == "DON006")
  
  plot(burn.dfv$OzCBI, burn.dfv$dNBRmax)
  plot(burn.dfv$OzCBI, burn.dfv$RBRmax)
  
  
  