#load libraries
library(tidyverse)
library(caret)
library(broom)
library(kableExtra)
library(lubridate)
library(sf)
library(raster)

### Joe 

# set paths and load data
fdir <- "Z:\\DEC\\Fire_regimes_2018-074\\DATA\\Working\\fireSeverity\\rds.files\\"
vdir <- "Z:\\DEC\\Fire_regimes_2018-074\\DATA\\Working\\fireSeverity\\"
wdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\fireSeverity\\"

# read in and clean up
df <- st_read(paste0(vdir, "plots.L8\\fieldata_2021-04-07_LL.shp"), stringsAsFactors = FALSE) %>%
  st_drop_geometry() %>%  mutate(id = paste0(BURNID, "-", ID)) %>% 
  dplyr::select(id, CBI_Plt, BURNID) 
colnames(df) <- c("id", "OzCBI", "BURNID")
df$OzCBI <- as.numeric(df$OzCBI)

csv.lst <- list.files(paste0(vdir, "plots.L8\\tmp"), full.names = TRUE)

ts <- lapply(csv.lst, read.csv) %>% bind_rows()
colnames(ts)[c(7, 10)] <- c("id", "BURNID")

df.joe <- df %>% left_join(ts, by = c("id", "BURNID")) %>%
  na.omit()
df.joe$date <- parse_date_time(df.joe$date, orders = c('ymd', 'dmy'))

vdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\"
fire.dates.joe <- read.csv(paste0(wdir, "data-record\\burn-dates-20210407_jf.csv"), stringsAsFactors = FALSE) 
fire.dates.joe$start <- dmy(fire.dates.joe$start)
fire.dates.joe$end <- dmy(fire.dates.joe$end)
fire.dates.joe$BURNID <- str_replace(fire.dates.joe$BURNID, "_", "")
fire.ibra.joe <- dplyr::select(fire.dates.joe, BURNID, ibra)
fire.dates.joe <- dplyr::select(fire.dates.joe, BURNID, start, end)
  ################################ Val 1 #################################
  dropOutliers <- F
  
  # set paths and load data
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
  csv.lst <- list.files("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\plots2yr\\tmp", 
                        full.names = TRUE)
  ts1 <- lapply(csv.lst, read.csv) %>% bind_rows()
  
  csv.lst <- list.files("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\set_20210311_L8\\tmp", full.names = TRUE)
  ts2 <- lapply(csv.lst, read.csv) %>% bind_rows()
  
  ts <- bind_rows(ts1, ts2)
  colnames(ts)[c(7, 10)] <- c("id", "BURNID")
  
  ts$id <- str_replace_all(ts$id, "_", "")
  
df.val <- df %>% left_join(ts, by = c("id", "BURNID")) %>%
   na.omit()
df.val$date <- parse_date_time(df.val$date, orders = c('ymd', 'dmy'))

# fire dates and ibra
  fire.date1 <- read.csv(paste0(wdir, "data-record\\burn-dates_20210510_val.csv"), stringsAsFactors = FALSE) 
  fire.ibra1 <- dplyr::select(fire.date1, Burn.ID, ibra)
  fire.date1 <- fire.date1 %>% gather(action, date,  3:10)%>% 
    dplyr::select(Burn.ID, Burn.name, action, date) 
    
  #fire.date2 <- read.csv(paste0(vdir, "set_20210311_L8\\burn-dates.csv"), stringsAsFactors = FALSE) 
  #fire.ibra2 <- dplyr::select(fire.date2, Burn.ID, ibra)
  #fire.date2 <- fire.date2 %>% gather(action, date,  3:8 ) %>% 
  #  dplyr::select(Burn.ID, Burn.name, action, date) 
  
  #fire.ibra.val <- bind_rows(fire.ibra1, fire.ibra2)
  fire.ibra.val <- fire.ibra1
  colnames(fire.ibra.val)[1] <- "BURNID"
  
  #fire.dates <- bind_rows(fire.date1, fire.date2) 
  fire.dates <- fire.date1
  
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
  
 # fire.dates.val <- filter(fire.dates.val, BURNID != "SWC057")
#################
### unburnt pts ###

  csv.lst <- list.files("Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\unburntPts\\tmp_L8", full.names = TRUE)
  
  ub.ts <- lapply(csv.lst, read.csv) %>% bind_rows()
  colnames(ub.ts)[c(7, 10)] <- c("id", "BURNID")
  ub.ts <- mutate(ub.ts, OzCBI =0)
  ub.ts$date <- parse_date_time(ub.ts$date, orders = c('ymd', 'dmy')) 
  
#################
### join data sets
  
  fire.ibras <- bind_rows(fire.ibra.joe, fire.ibra.val)
  fire.dates.all <- bind_rows(fire.dates.val, fire.dates.joe)
  fire.dates.all <- left_join(fire.dates.all, distinct(fire.ibras), by = "BURNID")
  fdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\fireSeverity\\rds.files\\"
  
  saveRDS(fire.dates.all, paste0(fdir, "site.date.L8.", Sys.Date()))
  
  df.all <- bind_rows(df.val, df.joe, ub.ts)
  saveRDS(df.all, paste0(fdir, "site.allData.L8.", Sys.Date()))
  
  t.df <- filter(df.joe, BURNID == "SWC046")
  
############################################################################################3
### calculate indicies
  df.all <- mutate(df.all, nbr = (b4-b6)/(b4+b6), 
                    i35 = (b3 +b5)/2, 
                    ndvi = (b4-b3)/(b4+b3),
                    ndwi = (b4-b5)/(b4+b5))
  
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

  tmp <- filter(burn.df, BURNID == "SWC046")
  #burn.df <- filter(burn.df, id == "PHS136-ChuB3")
  i <- 1
  for(i in 1:nrow(burn.df)){
    fire.date.i <- unique(fire.dates.all$start[which(fire.dates.all$BURNID == burn.df$BURNID[i])])
    fire.end.i <- unique(fire.dates.all$end[which(fire.dates.all$BURNID == burn.df$BURNID[i])])
    
    ind1 <- filter(df.all, date < fire.date.i &  id == burn.df$id[i])
    #ind1 <- filter(csv.all, name == burn.df$id[i])
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
  
  fdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\fireSeverity\\rds.files\\"
  saveRDS(burn.dfv, paste0(fdir, "site.var.L8.", Sys.Date()))
  
  tmp <- filter(burn.dfv, BURNID == "SWC046")
  
  plot(burn.dfv$OzCBI, burn.dfv$RBRmax)
  
  #####################################################################################333
  
  