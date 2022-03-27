#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

library(geosphere)
library(dplyr)


#Levantamos archivo.
#trollers = read.table("C:/Users/nico_/Desktop/ITBA/TFI/global fishing watch/dataset/trollers.csv", sep=",", header=TRUE)
trollers = read.table("C:/Users/marco/Desktop/Itba/TFI/dataset/trollers.csv", sep=",", header=TRUE)

trollers <- trollers[trollers$is_fishing == -1.0 | trollers$is_fishing == 1.0 ,]

#Renombramos las columnas de la variable a predecir de si esta pescando o no.
trollers$is_fishing[trollers$is_fishing == -1.0] <- 0
trollers$is_fishing[trollers$is_fishing == 1.0] <- 1

data.frame(table(trollers$is_fishing))

data= trollers[1:10000,]
data.frame(table(data$is_fishing))



data <- mutate(data,Distance = distHaversine(cbind(lon, lat),cbind(lag(lon), lag(lat))))
data$delta_T<-c(NA,diff(data$timestamp))
data$Speed_Change<-c(NA,diff(data$speed))
data$Course_Change<-c(NA,diff(data$course))
head(data)
cols.dont.want <- c("timestamp", "source", "lat", "lon","mmsi","distance_from_port", "distance_from_shore") # if you want to remove multiple columns
data <- data[, ! names(data) %in% cols.dont.want, drop = F]
#data <- data[data$distance_from_shore>=5556 ,] #5556metros = 3 nautical miles, Pagina 1 The GFW FISHING SCORE.

data.frame(table(data$delta_T))

library(tidyr)
data %>% drop_na()
hist(data$delta_T)

library(ggplot2)
p <- ggplot(data=subset(data, !is.na(data$delta_T)), aes(x=delta_T)) + geom_histogram()
p <- p + geom_point(uy=co)
print(p)

set.seed(101) 
require(caTools)
sample = sample.split(data$is_fishing, SplitRatio = .80)
original_train = subset(data, sample == TRUE)
original_test  = subset(data, sample == FALSE)

#### ARBOLES DE DESICION #####
library(rpart)
library("rpart.plot")
library(caret) #confusionMatrix
train = original_train
test = original_test
train$is_fishing = factor(train$is_fishing)
test$is_fishing = factor(test$is_fishing)
tree <- rpart(is_fishing ~ . ,data= train)
rpart.plot(tree)
p <- predict(tree, test, type="class")

##### No llega a nada.


##### SVM ???? #####
library(e1071)
# Pagina 50 explica todos los parametros de SVM.
#https://cran.r-project.org/web/packages/e1071/e1071.pdf
#https://www.rdocumentation.org/packages/e1071/versions/1.7-5/topics/tune
train = original_train
test = original_test
train$is_fishing[train$is_fishing == 0] <- '0'
train$is_fishing[train$is_fishing == 1] <- '1'
test$is_fishing[test$is_fishing == 0] <- '0'
test$is_fishing[test$is_fishing == 1] <- '1'

train$is_fishing <- as.factor(train$is_fishing)
test$is_fishing <- as.factor(test$is_fishing)

head(train)
#Corre 4.40 PM.
#svm_tune <- tune(svm, train.x= train, train.y=train$is_fishing, 
#                 type="C-classification",kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
#print(svm_tune)


#### Clasificador lineal ##############

linear <- tune.svm(is_fishing ~ . ,
                data = train,
                kernel="linear", 
                type="C-classification",
                cost  = c(10),
                max_iter=100
                )

svm.linear_model <- svm(is_fishing ~ .,
                 data = train, 
                 )

p <- predict(svm.linear_model, test, type="class")
confusionMatrix(p, test$is_fishing, positive = "1")
                 kernel = "linear", 
                 cost =linear$best.model$cost

##### Clasificador radial ###########

radial <- tune.svm(is_fishing ~ . ,
                   data = train,
                   kernel="radial", 
                   type="C-classification",
                   cost  = c(10,15,20),
                   gamma=c(0.1,1,10)
)



########## sigmoid ##############

sigmoid <- tune.svm(is_fishing ~ . ,
                   data = train,
                   kernel="sigmoid", 
                   type="C-classification",
                   cost  = c(10,15,20),
                   gamma=c(0.1,1,10),
                   coef0 = c(0.1,1,10)
  
  
#Ver de ordenar por TIMESTAMP, y ver que sea la diferencia.
O sea que el 1..2...3 se correlacionen con el timestamp en forma ascendente.


Hacer histogram del tiempo promedio entre 2 transmisiones.

A partir de ahi seguir.

Ver tolerancia de SVM, y ver de reducir el dataset para que empiece
a entrenar algo ...


