#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection
#library(plyr)
library(plyr)
library(caret)
require(caTools)
library(rpart)
library("rpart.plot")

#Levantamos archivo.
trollers = read.table("C:/Users/nico_/Desktop/ITBA/TFI/global fishing watch/dataset/trollers.csv", sep=",", header=TRUE)
count(trollers, "is_fishing")
#Observamos si fueron levantados correctamente.
head(trollers)

#Veo que tengo la informacion de 5 buques.
count(trollers, "mmsi")
#mmsi  freq
#1 7.652701e+13 16175
#2 1.129409e+14 64871
#3 1.259544e+14 17763
#4 1.670724e+14 37788
#5 2.740638e+14 24767


#Remuevo las columnas MMSI, y Source que no me deberian estar brindando informacion para hacer la prediccion. 
cols.dont.want <- c("mmsi", "source") # if you want to remove multiple columns
trollers <- trollers[, ! names(trollers) %in% cols.dont.want, drop = F]
head(trollers)

#Removemos los registros que estan con duda de si estan o no pescando.
#Nos quedamos solo con los que is_fishing == -1.0 o 1.0
trollers <- trollers[trollers$is_fishing == -1.0 | trollers$is_fishing == 1.0 ,]
#Remuevo la columna 'source'. No nos brinda informacion
#trollers <- select(trollers, -source)

#Renombramos las columnas de la variable a predecir de si esta pescando o no.
trollers$is_fishing[trollers$is_fishing == -1.0] <- 0
trollers$is_fishing[trollers$is_fishing == 1.0] <- 1

count(trollers, "is_fishing")
#is_fishing   freq
#FALSE        158398
#TRUE         2966

#Observamos el desbalanceo de clases.
## 75% of the sample size
set.seed(101) 
sample = sample.split(trollers$is_fishing, SplitRatio = .80)
original_train = subset(trollers, sample == TRUE)
original_test  = subset(trollers, sample == FALSE)
count(original_train, "is_fishing")
count(original_test, "is_fishing")

#### ARBOLES DE DESICION #####
#[OK]
library(tree)
train = original_train
test = original_test

train$is_fishing = factor(train$is_fishing)
test$is_fishing = factor(test$is_fishing)

tree <- rpart(is_fishing ~ . ,data= train)
rpart.plot(tree)
p <- predict(tree, test, type="class")
confusionMatrix(p, as.factor(test$is_fishing), positive = "1")

######################################################
#Arboles de decision(Decision Trees, DT): ############
######################################################
#[OK]
train = original_train
test = original_test
train$is_fishing = factor(train$is_fishing)
test$is_fishing = factor(test$is_fishing)

modelo_DT <-  rpart(is_fishing ~ . ,data= train,xval= 0,cp= 0.0, maxdepth=  6)
rpart.plot(modelo_DT)

prediccion_DT <- predict(modelo_DT, test, type = "class")
confusionMatrix(prediccion_DT,as.factor(test$is_fishing), positive = "1")
printcp(modelo_DT) # display the results 
plotcp(modelo_DT) # visualize cross-validation results 
summary(modelo_DT) # detailed summary of splits


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
                   data= train ,
                   probability=   FALSE,  
                   num.trees=     params$num.trees,
                   mtry=          params$mtry,
                   min.node.size= params$min.node.size,
                   max.depth=     params$max.depth
)
prediccion_RF <- predict(modelo_RF, test)
confusionMatrix(prediccion_RF$predictions,as.factor(test$is_fishing), positive = "1")


# Random Forest y crossvalidation #####
#https://www.guru99.com/r-random-forest-tutorial.html 
library(randomForest)
train = original_train
test = original_test
train$is_fishing = factor(train$is_fishing)
test$is_fishing = factor(test$is_fishing)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
#train(formula, data= train, method = "rf", trControl = trainControl(), tuneGrid = NULL)
set.seed(1234)
rf_default <- train(is_fishing ~ .,
                    data = train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)
# Print the results
print(rf_default)



#########################################################
#Lightgbm ###############################################
#########################################################
require("data.table")
require("lightgbm")
library(lightgbm)
train = original_train
test = original_test

#dejo los datos en el formato que necesita LightGBM
campos_buenos  <- setdiff(  colnames(trollers) ,  c("is_fishing" ))
dgeneracion  <- lgb.Dataset( data= data.matrix(  train[ , campos_buenos]),label= train$is_fishing, free_raw_data= FALSE )
modelo  <- lgb.train( data= dgeneracion,
                      objective= "binary",
                      boost_from_average= TRUE,
                      max_bin= 31,
                      num_iterations= 100
)
dapply <-  test
prediccion_LightGBM  <- predict( modelo, data.matrix( dapply[, campos_buenos ]))
test$predicted = ifelse(prediccion_LightGBM > 0.3,'1','0')
confusionMatrix(factor(test$predicted), factor(test$is_fishing), positive = "1")

#Para ver que variables son importantes.
tree_imp <- lgb.importance(modelo, percentage = TRUE)


######################################################
### Estudiamos las variables #########################
######################################################

## Feature engeering en R o pandas dataframe?

#plot(trollers$speed,trollers$is_fishing,main="is_fishing",pch=1,col=rainbow(2))

#https://medium.com/@mariekekortsmit/feature-engineering-for-time-series-data-ad4f52548a16
#Change between previous measurement:
#  S(m-1)-S(m-2);
#Change over a time window of size w:
#  [S(m-1)-S(m-1-w)]/[t(m-1)-t(m-1-w)];
#Note that if measurements are regular, you can also just divide by w;
#Average over a time window
#[S(m-1)+...+S(m-1-w)]/[t(m-1)-t(m-1-w)];
#Relative growth over a time window:
#[S(m-1)-S(m-1-w)]/S(m-1-w);
library(zoo)

data= trollers

#Mean 1
width = 2
means = rollmean(data$course,width)
data$Mean = c(rep(NA,width),means[1:(nrow(data)-width)])
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


A - - - B - - - - - - - C
#Generar puntos artificiales con AVG?
#Como tratar puntos con mucha diferencia de tiempo?
agregar_distancia_entre_puntos <- function() {
  
  Function body 
}


library(geosphere)
distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)


