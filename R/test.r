# https://towardsdatascience.com/is-a-trawler-fishing-modelling-the-global-fishing-watch-dataset-d1ffb3e7624a


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

#Levantamos archivo.
trollers = read.table("C:/Users/nico_/Desktop/ITBA/TFI/global fishing watch/dataset/trollers.csv", sep=",", header=TRUE)

#cols.dont.want <- c("mmsi", "source","distance_from_port", "distance_from_shore") # if you want to remove multiple columns
#trollers <- trollers[, ! names(trollers) %in% cols.dont.want, drop = F]

trollers <- trollers[trollers$is_fishing == -1.0 | trollers$is_fishing == 1.0 ,]

#Renombramos las columnas de la variable a predecir de si esta pescando o no.
trollers$is_fishing[trollers$is_fishing == -1.0] <- 0
trollers$is_fishing[trollers$is_fishing == 1.0] <- 1


################ RE SAMPLING ########################
#trollers$dif <- c(0, cumsum(as.numeric(diff(trollers$timestamp))))                                                                                                                                                                                     
#trollers <- trollers[trollers$dif %% 60*5 == 0,] 
#https://stackoverflow.com/questions/59341819/sample-every-nth-minute-in-a-minute-based-datetime-column-in-python


library(lubridate)
library(Rcpp)
trollers$Time <- as.POSIXct(trollers$timestamp, origin="1970-01-01")
library(dplyr); library(padr)
#trollers$Time <- anytime::anytime(trollers$Time)


#https://stackoverflow.com/questions/44155615/how-to-perform-r-time-based-resampling-with-a-given-time-period-equivalently-to
trollers <- trollers %>% thicken("5 min") %>% group_by(Time_5_min)

trollers <- trollers[!duplicated(trollers[,c('Time_5_min','Time_5_min')]),]
trollers <- trollers[trollers$distance_from_shore>=5556 ,] #5556metros = 3 nautical miles, Pagina 1 The GFW FISHING SCORE.

#### GENERAMOS FEATURE ENGENIERRING.

#Normalizamos la variable Course.
trollers$course <- trollers$course/360.0

library(geosphere)
library(dplyr)
#No esta funcando ahora, la de abajo si y da igual.
#trollers <- mutate(trollers,Distance = distHaversine(cbind(lon, lat),cbind(lag(lon), lag(lat))))
trollers$Distance[2:nrow(trollers)] <- sapply(2:nrow(trollers), function(x) distm(trollers[x-1,c('lon', 'lat')], trollers[x,c('lon', 'lat')], fun = distHaversine))
#trollers <- mutate(trollers,Distance = distHaversine(cbind(lon, lat),cbind(lag(lon), lag(lat))))
trollers$delta_T<-c(NA,diff(trollers$timestamp))
trollers$Speed_Change<-c(NA,diff(trollers$speed))
trollers$Course_Change<-c(NA,diff(trollers$course))

#AGREGAR AVERAGE SPEED.
trollers$SOG <- trollers$Distance/trollers$delta_T #Speed Over Ground
trollers$COG <- trollers$Course_Change/trollers$delta_T
trollers$RA <- trollers$SOG/trollers$delta_T #Rectilinear acceleration
trollers$RJ <- trollers$RA/trollers$delta_T #Rectilinear Jerk
trollers$Delta_Course <- trollers$Course_Change/trollers$delta_T

head(trollers)
cols.dont.want <- c("timestamp", "source","course","mmsi","dif", "Time_5_min","Time") # if you want to remove multiple columns

aux <- trollers


trollers <- aux
trollers <- trollers[, ! names(trollers) %in% cols.dont.want, drop = F]
asd <- trollers

#Generamos Lags. ventana=6.
#Tener en cuenta que puedo generar lags agrupados.

primes_list <- list("Course_Change", "Speed_Change")
for (i in 1:length(primes_list)) {
  lags <- seq(6)
  lag_names <- paste(primes_list[[i]], formatC(lags, width = nchar(max(lags)), flag = "0"), sep = "_")
  lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
  
  df = cbind(trollers, trollers %>% mutate_at(vars(primes_list[[i]]), funs_(lag_functions)))[ ,lag_names]
  trollers <- cbind(trollers, df)

}
head(trollers)



###
library(plyr)
count(trollers, "is_fishing")

#Observamos el desbalanceo de clases.
## 75% of the sample size
set.seed(101) 
require(caTools)
sample = sample.split(trollers$is_fishing, SplitRatio = .80)
original_train = subset(trollers, sample == TRUE)
original_test  = subset(trollers, sample == FALSE)
count(original_train, "is_fishing")
count(original_test, "is_fishing")


### R tress
library(rpart)

train = original_train
test = original_test
train$is_fishing = factor(train$is_fishing)
test$is_fishing = factor(test$is_fishing)

#modelo_DT <-  rpart(is_fishing ~ . ,data= train,xval= 0,cp= 0.0,maxdepth=20)
modelo_DT <-  rpart(is_fishing ~ . ,data= train,xval= 0,cp= 0.0)

library("rpart.plot")
#rpart.plot(modelo_DT)

prediccion_DT <- predict(modelo_DT, test, type = "class")

library(caret)
confusionMatrix(prediccion_DT,as.factor(test$is_fishing), positive = "1")
#printcp(modelo_DT) # display the results 
#plotcp(modelo_DT) # visualize cross-validation results 
#summary(modelo_DT) # detailed summary of splits


######################################################
#Random forest. ######################################
######################################################
require("ranger")
train = original_train
test = original_test
train$is_fishing = factor(train$is_fishing)
test$is_fishing = factor(test$is_fishing)

params  <- list( "num.trees"=      500,  #cantidad de arboles
                 "mtry"=             3,  #cantidad de variables que evalua para hacer un split
                 "min.node.size"=    1,  #hoja mas chica
                 "max.depth"=        0   #0 significa profundidad infinita
)
modelo_RF  <- ranger( formula= is_fishing ~ .,
                      train[complete.cases(train),] ,
                      probability=   FALSE,  
                      num.trees=     params$num.trees,
                      mtry=          params$mtry,
                      min.node.size= params$min.node.size,
                      max.depth=     params$max.depth
)
prediccion_RF <- predict(modelo_RF, test[complete.cases(test),])
confusionMatrix(prediccion_RF$predictions,as.factor(test$is_fishing), positive = "1")















