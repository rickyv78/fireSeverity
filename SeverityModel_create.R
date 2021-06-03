library(tidyverse)
library(caret)
library(broom)
library(kableExtra)
library(lubridate)
library(sf)
library(raster)
library(rpart)
library(rpart.plot)
library(doParallel)
library(here)

options(scipen=999) # remove scientific notation
sat <- "s2a" #"L8" #"s2a

t1 <- 0.5
t2 <- 1.4
t3 <- 2
t4 <- 2.5

# location for graphs
#gdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\plots.s2a\\graphs\\"
gdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\plots2yr\\graphs\\"

df <- readRDS(here("rds.files/site.var.s2.2021-05-26"))

df$OzCBI <- as.numeric(df$OzCBI)
df.rm <- read.csv(here("data-record\\outliers.csv"))
df.rm <- mutate(df.rm, id = paste0(BURNID, "-", site))
remove <- df.rm$id

df <- filter(df, (id %in% remove) == FALSE)

df <- mutate(df, Severity_class = case_when(OzCBI <= t1 ~ "U", 
                                            OzCBI <= t2 ~ "L", 
                                            OzCBI <= t3 ~ "M",
                                            OzCBI <= t4 ~ "H",
                                            TRUE ~ "VH")) 
df$Severity_class <- factor(df$Severity_class, levels = c("U", "L", "M", "H", "VH"))

fire.dates.df <- readRDS(here("rds.files/site.date.s2.2021-05-24")) %>% na.omit()
#fire.dates.df <- fire.dates.df %>% dplyr::select(BURNID, ibra) %>% distinct() 

select.index <- function(x){
  dplyr::select(x, -id, BURNID,  -Veg.Type, OzCBI,
                -posti35max, -postNDVImin, -postNIRmin, -postNBRmin, -postNDWImin, 
                -preNIR, -prei35, -preNDVI, -preNBR, -preNDWI,
                di35max,  dNIRmax, dNBRmax, dNDVImax, dNDWImax, RBRmax)
  #LD1, LD2, -rain)
}

dfi <- df %>% na.omit() %>%
  left_join(fire.dates.df, by = "BURNID") %>%
  mutate(Veg.Type = ibra) %>%
  dplyr::select(-ibra, -start, -end)

#types <- unique(dfi$Veg.Type)
types <- c("Southern Jarrah Forest", "Warren", "Northern Jarrah Forest", "Perth", "Dandaragan Plateau") 
labs <- c("sjf", "war", "njf", "pth", "dgn")
ck.all <- data.frame()  

#######################################################################################
### Create models for application

t <- 1
#Define how many cores (memory is limiting factor here)
UseCores <- length(types)
#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)
foreach(t= 1:length(types)) %dopar% {
  library(tidyverse)
  library(caret)
  library(broom)
  library(kableExtra)
  library(lubridate)
  library(sf)
  library(raster)
  library(rpart)
  library(here)
#for(t in 1:length(types)){
  
  df.t <- dfi %>% filter(Veg.Type == types[t]) %>% 
    select.index()
   
  df.t <- filter(df.t, (BURNID %in% c("ALB040", "PHS128")) == FALSE)
  
  df.t$Severity_class <- factor(df.t$Severity_class, levels = c("U", "L", "M", "H", "VH"))

  ### random forest model with all variables
  df.rf <- dplyr::select(df.t, -BURNID, -OzCBI) %>% 
    droplevels()
  rf_model <- train(Severity_class ~.,
                      data = df.rf, 
                      #tuneGrid = tunegrid,
                      method = "rf", # random forest model from the ranger package
                      importance = TRUE,
                      trControl = trainControl(method = "repeatedcv", 
                                               number = 10, # number of folds in the training data
                                               repeats = 5, 
                                               verboseIter = FALSE)) # do not print progress updates
  saveRDS(rf_model, paste0(here("models"), "/rf.ibra.", sat, ".class.",types[t],".", Sys.Date()))
    
  ### weighted rf model
  
  ctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       summaryFunction = multiClassSummary,
                       classProbs = TRUE)
  
  # Create model weights (they sum to one)
  
  model_weights <- case_when(df.rf$Severity_class == "U" ~ (1/table(df.rf$Severity_class)[1]) * 0.2,
                             df.rf$Severity_class == "L" ~ (1/table(df.rf$Severity_class)[2]) * 0.2, 
                             df.rf$Severity_class == "M" ~ (1/table(df.rf$Severity_class)[3]) * 0.2,
                             df.rf$Severity_class == "H" ~ (1/table(df.rf$Severity_class)[4]) * 0.2,
                             df.rf$Severity_class == "VH" ~ (1/table(df.rf$Severity_class)[5]) * 0.2)
  #set.seed(5627)
  
  # Build weighted model
  weighted.model <- train(Severity_class ~ .,
                        data = df.rf,
                        method = "gbm",
                        verbose = FALSE,
                        weights = model_weights,
                        metric = "ROC",
                        trControl = ctrl)
  saveRDS(weighted.model, paste0(here("models"), "/rw.ibra.", sat, ".class.",types[t],".", Sys.Date()))
  
  # model with up sampling
  ctrl$sampling <- "up"
  up_fit <- train(Severity_class ~ .,
                     data = df.rf,
                     method = "gbm",
                     verbose = FALSE,
                     metric = "ROC",
                     trControl = ctrl)
  saveRDS(weighted.model, paste0(here("models"), "/ru.ibra.", sat, ".class.",types[t],".", Sys.Date()))
  
  # model with down sampling
  #ctrl$sampling <- "down"
  #down_fit <- train(Severity_class ~ .,
  #                data = df.rf,
  #                method = "gbm",
  #                verbose = FALSE,
  #                metric = "ROC",
  #                trControl = ctrl)
  #saveRDS(weighted.model, paste0(here("models"), "/rd.ibra.s2a.class.",types[t],".", Sys.Date()))
  
  ### rpart model with single index
  df.rp <- dplyr::select(df.t, Severity_class, dNBRmax)
  rp.model <- rpart(Severity_class ~., data = df.rp, method="class")  
  saveRDS(rp.model, paste0(here("models"), "/rp.ibra.", sat, ".class.",types[t],".", Sys.Date()))
    
  ### Quadratic model
  df.ln <- dplyr::select(df.t, OzCBI, dNBRmax)
  ln.model <- lm(OzCBI ~ dNBRmax + I(dNBRmax^2), data = df.ln)
  saveRDS(ln.model, paste0(here("models"), "/ln.ibra.", sat, ".class.",types[t],".", Sys.Date()))
}
stopCluster(cl)
