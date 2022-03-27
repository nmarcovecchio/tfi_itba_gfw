#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

#Levantamos archivo.
trollers = read.table("C:/Users/nico_/Desktop/ITBA/TFI/global fishing watch/dataset/trollers.csv", sep=",", header=TRUE)

cols.dont.want <- c("mmsi", "source") # if you want to remove multiple columns
trollers <- trollers[, ! names(trollers) %in% cols.dont.want, drop = F]

trollers <- trollers[trollers$is_fishing == -1.0 | trollers$is_fishing == 1.0 ,]

#Renombramos las columnas de la variable a predecir de si esta pescando o no.
trollers$is_fishing[trollers$is_fishing == -1.0] <- 0
trollers$is_fishing[trollers$is_fishing == 1.0] <- 1


library(zoo)

data= trollers

#Mean 1
width = 2
means = rollmean(data$course,width)
data$Mean = c(rep(NA,width),ave[1:(nrow(data)-width)])
head(data)

#Mean 2
custom_function = function(vec){
  return(mean(vec))
}
width = 2
customvals = rollapply(data$course,width = 2, FUN = custom_function) 
data$Mean2 = c(rep(NA,width),customvals[1:(nrow(data)-width)])
head(data)

#X[2]-X[1]
custom_function = function(x){
  #yourresult = (x[2] - x[1])
  yourresult= x[window,2] - x[1,2]
  return (yourresult)
}
library(zoo)

data= trollers
window = 2


change = rollapply(data$course, window, custom_function, by.column = FALSE)
data$course_change= c(rep(NA,window), change[1:(nrow(data)-width)])
head(data)



#DeltaT
#AvgSpeed
#Variacion velocidad
#Curso_medio
#Varicion_curso
#Variacion_Lat/Lon


#A - - - B - - - - - - - C
#Generar puntos artificiales con AVG?
#Como tratar puntos con mucha diferencia de tiempo?
#agregar_distancia_entre_puntos <- function() {
  
#  Function body 
#} 

rm( list=ls() )  #remove all objects
gc()             #garbage collection
library(geosphere)
library(dplyr)
trollers = read.table("C:/Users/nico_/Desktop/ITBA/TFI/global fishing watch/dataset/trollers.csv", sep=",", header=TRUE)

data= trollers
data <- data[data$is_fishing == -1.0 | data$is_fishing == 1.0 ,]
data$is_fishing[data$is_fishing == -1.0] <- 0
data$is_fishing[data$is_fishing == 1.0] <- 1

data <- mutate(data,Distance = distHaversine(cbind(lon, lat),cbind(lag(lon), lag(lat))))
data$delta_T<-c(NA,diff(data$timestamp))
data$Speed_Change<-c(NA,diff(data$speed))
data$Course_Change<-c(NA,diff(data$course))
head(data)
cols.dont.want <- c("timestamp", "source", "lat", "lon","mmsi","distance_from_port", "distance_from_shore") # if you want to remove multiple columns
data <- data[, ! names(data) %in% cols.dont.want, drop = F]
#data <- data[data$distance_from_shore>=5556 ,] #5556metros = 3 nautical miles, Pagina 1 The GFW FISHING SCORE.


count(data, "is_fishing")

head(data)

set.seed(101) 
require(caTools)
sample = sample.split(data$is_fishing, SplitRatio = .80)
original_train = subset(data, sample == TRUE)
original_test  = subset(data, sample == FALSE)

#### ARBOLES DE DESICION #####
library(tree)
train = original_train
test = original_test

train$is_fishing = factor(train$is_fishing)
test$is_fishing = factor(test$is_fishing)

tree <- rpart(is_fishing ~ . ,data= train)
rpart.plot(tree)
p <- predict(tree, test, type="class")
confusionMatrix(p, as.factor(test$is_fishing), positive = "1")

