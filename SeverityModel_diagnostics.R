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


t1 <- 0.5
t2 <- 1.4
t3 <- 2
t4 <- 2.5


sat <- "L8" #"s2a
df <- readRDS(here("rds.files/site.var.L8.2021-05-26"))
#df <- readRDS(here("rds.files/site.var.s2.2021-05-26"))

# location for graphs
gdir <- "Z:\\DEC\\Prescribed_Bushfire_Outcomes_2018-134\\DATA\\Working\\ts.Model\\plots2yr\\graphs\\"

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

select.index <- function(x){
  dplyr::select(x, -id, BURNID,  -Veg.Type, OzCBI,
                -posti35max, -postNDVImin, -postNIRmin, -postNBRmin, -postNDWImin, 
                -preNIR, -prei35, -preNDVI, -preNBR, -preNDWI,
                di35max,  dNIRmax, dNBRmax, dNDVImax, dNDWImax, RBRmax)
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
### test models for report
t <- 1
#Define how many cores (memory is limiting factor here)
UseCores <- length(types)
#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)
i <- 2
foreach(t= 1:length(types)) %dopar% {
  library(tidyverse)
  library(caret)
  library(broom)
  library(lubridate)
  library(sf)
  library(raster)
  library(rpart)
  library(rpart.plot)
  library(here)
  library(DMwR2) # for smote implementation
  library(purrr) # for functional programming (map)
  library(pROC) # for AUC calculations
  #library(MLmetrics)
  #install.packages("zoib")
  #library(rjags)
  #library(zoib)
  
# for(t in 1:length(types)){
  
  df.i <- filter(dfi, Veg.Type == types[t])
  df.i <- filter(df.i, (BURNID %in% c("ALB040", "PHS128")) == FALSE)
  
  df.rf <- select.index(df.i)
  df.rf$Severity_class <- factor(df.rf$Severity_class, levels = c("U", "L", "M", "H", "VH"))
  
  burns <- unique(df.rf$BURNID)   
  
  acc <- data.frame(BURNID = as.character(), rmse = as.numeric(), n = as.numeric(),
                    nSamples = as.numeric(), rf.acc = as.numeric(),  rp.acc = as.numeric(),
                    stringsAsFactors = FALSE)[1:length(burns), ]
  
  cm.df.all <- data.frame()
  j <- 1 
  
  for (j in 1:length(burns)){
    burn.i <- burns[-j]
    ck <- filter(df.i, BURNID == burns[j])
    df.f <- filter(df.rf, BURNID %in% burn.i)
    df.j <- dplyr::select(df.f, -BURNID, -OzCBI)
    
    ### random forest model with all variables
    df.j <- droplevels(df.j)
    rf_model <- train(Severity_class ~.,
                      data = df.j, 
                      #tuneGrid = tunegrid,
                      method = "rf", # random forest model from the ranger package
                      importance = TRUE,
                      trControl = trainControl(method = "repeatedcv", 
                                               number = 10, # number of folds in the training data
                                               repeats = 5, 
                                               verboseIter = FALSE)) # do not print progress updates
    
    
    df.w <- filter(df.rf, (BURNID %in% burn.i)==FALSE )
    df.w$predict <- predict(rf_model, df.w)
    
    ck <- ck %>% dplyr::select(id, Veg.Type) %>% bind_cols(df.w)
    ck.all <- bind_rows(ck, ck.all)
    
    cm <- confusionMatrix(df.w$predict, df.w$Severity_class)
    
    ggplot(as.data.frame(cm$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
      geom_tile() + geom_text(aes(label=Freq)) +
      scale_fill_gradient(low="white", high="#009194") +
      labs(title = paste0(sat," ,", types[t], ": ", df.w$BURNID[1], 
                          ", acc = ", round(cm$overall[1], digits = 3)), y = "Reference",x = "Prediction") +
      scale_x_discrete(labels=c("unburnt","no scorch","partial scorch","canopy scorch", "canopy burnt")) +
      scale_y_discrete(labels=c("canopy burnt", "canopy scorch","partial scorch","no scorch", "unburnt"))
    dir.create(paste0(gdir, "cm"), showWarnings = FALSE)
    ggsave(paste0(gdir, "cm\\cm_", labs[t], "_",df.w$BURNID[1], "_" ,sat,"_rf.jpg" ), width = 7, height = 4)
    
    cm.rf <- as.data.frame(cm$table)
    cm.rf$acc <- cm$overall[1]
    cm.rf$model <- "rf"
    
    ### weighted rf model
    ctrl <- trainControl(method = "repeatedcv",
                         number = 5,
                         repeats = 5,
                         summaryFunction = multiClassSummary,
                         classProbs = TRUE)
    
    # Create model weights (they sum to one)
    # model with up sampling
    w.model = "rf"# #gbm"
    if (types[t] == "Dandaragan Plateau"){w.model = "rf"}
    ctrl$sampling <- "up"
    up_fit <- train(Severity_class ~ .,
                    data = df.j,
                    method = w.model,
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = ctrl)
    
    df.w$predict <- predict(up_fit, df.w)
    
    cm <- confusionMatrix(df.w$predict, df.w$Severity_class)
    
    cm.rw <- as.data.frame(cm$table)
    cm.rw$acc <- cm.rw$overall[1]
    cm.rw$model <- "rw"
    
    ### rpart model with single index
    df.rp <- dplyr::select(df.j, Severity_class, dNBRmax)
    rp.model <- rpart(Severity_class ~., data = df.rp, method="class")  
    
    df.w$predict <- predict(rp.model, df.w, type = "class")
    cm <- confusionMatrix(df.w$predict, df.w$Severity_class)
    cm.rp <- as.data.frame(cm$table)
    cm.rp$acc <- cm$overall[1]
    cm.rp$model <- "rpart"
    
    ### beta regression
    #df.bt <- dplyr::select(df.f, OzCBI, RBRmax)
    #df.bt$OzT <- df.bt$OzCBI / 3
    
    #df.br <- betareg(OzT ~ RBRmax, df.bt)
    
    
    ### Quadratic model
    df.ln <- dplyr::select(df.f, OzCBI, RBRmax)

    fm <- lm(OzCBI ~ RBRmax + I(RBRmax^2), data = df.ln)
    
    df.w$predict.cbi <- predict(fm, df.w)
    
    df.w <- mutate(df.w, predict = case_when(predict.cbi <= t1 ~ "U", 
                                             predict.cbi <= t2 ~ "L", 
                                             predict.cbi <= t3 ~ "M",
                                             predict.cbi <= t4 ~ "H",
                                             TRUE ~ "VH")) 

    df.w$predict <- factor(df.w$predict, levels = c("U", "L", "M", "H", "VH"))
    #plot(df.w$predict, df.w$OzCBI)
    cm <- confusionMatrix(df.w$predict, df.w$Severity_class)
    cm.ln <- as.data.frame(cm$table)
    cm.ln$acc <- cm$overall[1]
    cm.ln$model <- "quadratic"
    
    cm.df <- bind_rows(cm.rp, cm.rf, cm.ln, cm.rw)
    
    cm.df$burn <- df.w$BURNID[1]
    cm.df$n <- nrow(df.w)
    cm.df$ibra <- types[t]
    cm.df.all <- bind_rows(cm.df.all, cm.df)
  }
  ck.all <- mutate(ck.all, dif = as.numeric(predict) - as.numeric(Severity_class))
  write.csv(ck.all, paste0(here("predict"), "/predict_ibra_",sat,"_", labs[t],".csv"))
  saveRDS(cm.df.all, paste0(here("rds.files"), "/cm.type.", sat, ".",labs[t],".", Sys.Date()))
}
stopCluster(cl)