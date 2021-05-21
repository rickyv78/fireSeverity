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

rf.num <- 10# number of folds in the training data
rf.rep <- 5
here("ts.model/models")
# set paths and load data
mdir <- here("models")
fdir <- here("rds.files")
gdir <- here("plots.s2a/graphs/")
vdir <- here()

df <- readRDS(here( "site.var.s2.2021-05-14"))
df$OzCBI <- as.numeric(df$OzCBI)

df.rm <- read.csv(paste0(vdir, "data-record\\outliers.csv"))
df.rm <- mutate(df.rm, id = paste0(BURNID, "-", site))
remove <- df.rm$id

df <- filter(df, (id %in% remove) == FALSE)

df <- mutate(df, Severity_class = case_when(OzCBI == 0 ~ "UB", 
                                            OzCBI <= 0.9 ~ "NS", 
                                            OzCBI <= 1.6 ~ "PS", 
                                            OzCBI <= 2.1 ~ "CS",
                                            TRUE ~ "CB")) 
df$Severity_class <- factor(df$Severity_class, levels = c("UB", "NS", "PS", "CS", "CB"))

fire.dates.df <- readRDS(paste0(fdir, "site.date.s2.2021-05-14")) %>% na.omit()
#fire.dates.df <- fire.dates.df %>% dplyr::select(BURNID, ibra) %>% distinct() 

select.index <- function(x){
  dplyr::select(x, -id, BURNID,  -Veg.Type, OzCBI,
                -posti35max, -postNDVImin, -postNIRmin, -postNBRmin, -postNDWImin, 
                #preNIR, prei35, preNDVI, preNBR, preNDWI,
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

t <- 5
#Define how many cores (memory is limiting factor here)
UseCores <- length(types)
#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)
i <- 1
foreach(t= 1:length(types)) %dopar% {
  library(tidyverse)
  library(caret)
  library(broom)
  library(kableExtra)
  library(lubridate)
  library(sf)
  library(raster)
  library(rpart)
  library(rpart.plot)
  #for(t in 1:length(types)){
  
  df.i <- filter(dfi, Veg.Type == types[t])
  df.i <- filter(df.i, (BURNID %in% c("ALB040", "PHS128")) == FALSE)
  
  ggplot(df.i, aes(Severity_class, dNBRmax, color = BURNID))+
    geom_boxplot() + 
    theme_bw()
  ggsave(paste0(gdir, "boxplot\\boxplt_",labs[t] ,".jpg" ), width = 7, height = 4)
  
  ggplot(df.i, aes(OzCBI, dNBRmax, colour = BURNID))+
    geom_point() +
    stat_smooth(method = lm, se = FALSE)+
    theme_bw()
  ggsave(paste0(gdir, "scatter\\boxplt_",labs[t] ,".jpg" ), width = 7, height = 4)
  
  
  df.rf <- select.index(df.i)
  df.rf$Severity_class <- factor(df.rf$Severity_class, levels = c("UB", "NS", "PS", "CS", "CB"))
  
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
      labs(title = paste0(types[t], ": ", df.w$BURNID[1], 
                          ", acc = ", round(cm$overall[1], digits = 3)), y = "Reference",x = "Prediction") +
      scale_x_discrete(labels=c("unburnt","no scorch","partial scorch","canopy scorch", "canopy burnt")) +
      scale_y_discrete(labels=c("canopy burnt", "canopy scorch","partial scorch","no scorch", "unburnt"))
    ggsave(paste0(gdir, "cm\\ibra\\cm_", labs[t], "_",df.w$BURNID[1], "_rf.jpg" ), width = 7, height = 4)
    
    cm.rf <- as.data.frame(cm$table)
    cm.rf$acc <- cm$overall[1]
    cm.rf$model <- "rf"
    
    ### rpart model with single index
    df.rp <- dplyr::select(df.j, Severity_class, RBRmax)
    rp.model <- rpart(Severity_class ~., data = df.rp, method="class")  
    #rpart.plot(rp.model) 
    df.w$predict <- predict(rp.model, df.w, type = "class")
    cm <- confusionMatrix(df.w$predict, df.w$Severity_class)
    cm.rp <- as.data.frame(cm$table)
    cm.rp$acc <- cm$overall[1]
    cm.rp$model <- "rpart"
    
    ### linear model
    df.ln <- dplyr::select(df.f, OzCBI, RBRmax)
    exp(coef(lm()))
    
    library(drc)
    library(nlme)
    library(aomisc)
    model <- drm(OzCBI~. , fct = DRC.powerCurve(), data = df.ln)
    
    
    ln.model <- lm(OzCBI ~ exp(RBRmax), data = df.ln)
    ln.model <- nls(OzCBI ~ a*exp(r*RBRmax), data = df.ln)
    
    summary(ln.model)
    
    
    ggplot(df.ln, aes(y = OzCBI, x = RBRmax))+
      geom_point(alpha = 0.4)+
      stat_smooth(method = lm, formula= (y ~ exp(x)), se = TRUE, linetype = 1, colour = "forestgreen")+
      theme_bw()
    
    df.ln$predict <- 0.34 * exp(df.ln$RBRmax * 4.3)
    
    plot(df.ln$predict, df.ln$OzCBI)
    
    cm.df <- bind_rows(cm.rp, cm.rf)
    
    cm.df$burn <- df.w$BURNID[1]
    cm.df$n <- nrow(df.w)
    cm.df$ibra <- types[t]
    cm.df.all <- bind_rows(cm.df.all, cm.df)
  }
  ck.all <- mutate(ck.all, dif = as.numeric(predict) - as.numeric(Severity_class))
  write.csv(ck.all, paste0(vdir, "plots.s2a\\predict\\predict_ibra_s2_", labs[t],".csv"))
  saveRDS(cm.df.all, paste0(fdir, "cm.type.s2.",labs[t],".", Sys.Date()))
}
stopCluster(cl)