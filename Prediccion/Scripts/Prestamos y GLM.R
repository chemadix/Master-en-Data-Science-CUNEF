########################################################################################################
## Start Date: 31/10/2019
## End Date: 
## Author: José María Álvarez Silva
## School: CUNEF
## Class: Predicción
## Language: Spanish
##
########################################################################################################
## Predicción
########################################################################################################
## Préstamos y GLM
##
########################################################################################################
## Caso: Préstamos y GLM
##
## Propósito ###########################################################################################
## - Utilizar modelos GLM para determinar si se otorga un préstamo o no

########################################################################################################
## Paquetes ############################################################################################
##
library(readr)     ##  cargar data sets
library(ISLR)      ##  paqueteria de Introduction to statistical learning in R
library(skimr)     ##  Beautiful Summarize
library(ggplot2)   ##  Ploting
library(dplyr)     ##  programación tidy
library(corrplot)  ##  gráficas de correlación
library(mltools)   ##  one hot encoding tools
library(onehot)    ##  same
library(cleandata) ##  same
library(vcd)       ##  Association Statistics
library(ROCR)      ##  Curva ROC
library(boot)      ##  Bootsrapping

########################################################################################################
## Datos ###############################################################################################
##
datos <- na.omit(read_csv("DATOS.csv"))

## Clean Dataset (numeric variables incluidas al principio)
datosClean <- datos[, c(1,5,6)]
datosClean$int_rate <- as.double(gsub("%", "", datosClean$int_rate))/100

## Default
Default <- (datos$loan_status == "Default")*1
datosClean$Default <- Default

## factor columns
##
## Ordinal encoding
datos$emp_length <- factor(datos$emp_length, levels = c("n/a", "< 1 year", "1 year", "2 years",
                                                        "3 years", "4 years", "5 years", 
                                                        "6 years", "7 years", "8 years",  
                                                        "9 years", "10+ years" ), ordered = TRUE)
employmentFunct <- function(x){
  switch(x, 
         "n/a" = {
           return(1)
         },
         "< 1 year" = {
           return(2)
         },
         "1 year" = {
           return(3)
         },
         "2 years" = {
           return(4)
         },
         "3 years" = {
           return(5)
         },
         "4 years" = {
           return(6)
         },
         "5 years" = {
           return(7)
         },
         "6 years" = {
           return(8)
         },
         "7 years" = {
           return(9)
         },
         "8 years" = {
           return(10)
         },
         "9 years" = {
           return(11)
         },
         {
           return(12)
         }
  )
}

datosClean$emp_length <- sapply(datos$emp_length,employmentFunct)

###
datosClean$grade <- factor(datos$grade)

gradeFunct <- function(x){
  switch(x, 
         "A" = {
           return(1)
         },
         "B" = {
           return(2)
         },
         "C" = {
           return(3)
         },
         "D" = {
           return(4)
         },
         "E" = {
           return(5)
         },
         "F" = {
           return(6)
         },
         {
           return(7)
         }
  )
}

datosClean$grade <- sapply(datos$grade, gradeFunct)

##
## Nominal - Onehot Encoding
datos$home_ownership <- factor(datos$home_ownership)
datos$purpose        <- factor(datos$purpose)
datos$term           <- factor(datos$term)

home_ownership        <- as.data.frame(datos$home_ownership)
encoder               <- onehot(home_ownership)
home_ownership_onehot <- predict(encoder, home_ownership, stringsAsFactors = TRUE)
datosClean            <- cbind(datosClean, home_ownership_onehot)

purpose        <- as.data.frame(datos$purpose)
encoder        <- onehot(purpose, max_levels = 15)
purpose_onehot <- predict(encoder, purpose, stringsAsFactors = TRUE)
datosClean     <- cbind(datosClean, purpose_onehot)

term        <- as.data.frame(datos$term)
encoder     <- onehot(term, max_levels = 15)
term_onehot <- predict(encoder, term, stringsAsFactors = TRUE)
datosClean  <- cbind(datosClean, term_onehot)

## Escalamos

escalar <- function(x) {
  minD <- mean(x)
  maxD <- sd(x)
  res <- (x - minD) / (maxD)
  return(res)
}
datosClean2 <- datosClean

datosClean2$annual_inc <- escalar(datosClean$annual_inc)
datosClean2$int_rate   <- escalar(datosClean$int_rate)
datosClean2$emp_length <- escalar(datosClean$emp_length)


########################################################################################################
## Análisis exploratorio de Datos ######################################################################
##
sum(datosClean$Default) ## casos de dafault

## Annual income
ggplot(data = datosClean) + geom_histogram(aes(annual_inc), binwidth = 100000, colour = "white")

ggplot(data = datosClean, aes(x = as.factor(Default), y = annual_inc)) + geom_boxplot() +
  scale_y_continuous(trans = 'log10') + ggtitle("Ingreso Anual (Escala Logarítmica)") + xlab("Default")

## interest rate
ggplot(data = datosClean) + geom_histogram(aes(int_rate), colour = "white")

ggplot(data = datosClean, aes(x = as.factor(Default), y = int_rate)) + geom_boxplot() +
  ggtitle("Tasa de Interés") + xlab("Default")

## loan amount
ggplot(data = datosClean) + geom_histogram(aes(loan_amnt), colour = "white")

ggplot(data = datosClean, aes(x = as.factor(Default), y = loan_amnt)) + geom_boxplot() +
  ggtitle("Monto del Préstamo") + xlab("Default")

## Employment Length

ggplot(data = datosClean) + geom_histogram(aes((emp_length)), colour = "white") + xlab("")

xtabs(~Default + emp_length, data = datosClean)

nemp_length <- xtabs(~Default + emp_length, data = datosClean)[2,] +
  xtabs(~Default + emp_length, data = datosClean)[1,]

round(xtabs(~Default + emp_length, data = datosClean)[1,] / nemp_length, 7)
round(xtabs(~Default + emp_length, data = datosClean)[2,] / nemp_length, 7)

## Variable Grade
ggplot(data = datosClean) + geom_histogram(aes((grade)), colour = "white") + xlab("")

xtabs(~Default + grade, data = datosClean)

ngrade <- xtabs(~Default + grade, data = datosClean)[2,] +
  xtabs(~Default + grade, data = datosClean)[1,]

round(xtabs(~Default + grade, data = datosClean)[1,] / ngrade, 7)
round(xtabs(~Default + grade, data = datosClean)[2,] / ngrade, 7)

## Variable Mortgage

xtabs(~Default + `datos$home_ownership=MORTGAGE`, data = datosClean)

## Variable Mortgage

xtabs(~Default + `datos$home_ownership=OWN`, data = datosClean)

## Variable Mortgage

xtabs(~Default + `datos$home_ownership=RENT`, data = datosClean)


########################################################################################################
## Modelo ##############################################################################################
##
modelo <- glm(data = datosClean, Default ~ ., family = "binomial")
summary(modelo)

modelo2 <- glm(data = datosClean[,-c(7, 9, 10, 13:26)], Default ~ ., family = "binomial")
summary(modelo2)

modelo3 <- glm(data = datosClean[,-c(7:26)], Default ~ ., family = "binomial")
summary(modelo3)

modelo4 <- glm(data = datosClean[,-c(7, 9, 10, 13:28)], Default ~ ., family = "binomial")
summary(modelo4) ## Ganador

## Interpretación de los Coeficientes

exp(cbind(OR = coef(modelo4), confint(modelo4)))

## Individuos Tipo

newdata1 <- with(datosClean,
                 data.frame(annual_inc = mean(annual_inc),
                            int_rate   = mean(int_rate),
                            loan_amnt  = mean(loan_amnt),
                            emp_length = 12,
                            grade      = 1:7,
                            `datos$home_ownership=MORTGAGE` = 1,
                            `datos$home_ownership=OWN`      = 0,
                            `datos$home_ownership=RENT`     = 0)
                 )
colnames(newdata1) <- c("annual_inc",
                        "int_rate",
                        "loan_amnt",
                        "emp_length",
                        "grade",
                        "datos$home_ownership=MORTGAGE",
                        "datos$home_ownership=OWN",
                        "datos$home_ownership=RENT")

predict(modelo4, newdata = newdata1, type = "response")

## Clasificación Binaria ##############################################################################
##
mod4.pred <- predict(modelo4,type = "response")
glm.pred  <- rep(0, dim(datosClean))
glm.pred[mod4.pred > .5] <- 1 ## Muy ALto el cutoff
mean(glm.pred == datosClean$Default) ## No hay varores iguales, por lo tanto no hay Default

## Funcion para determinar el accuracy
fAccuracy <- function(cut_off, mod01.pred = mod4.pred, datos = datosClean){
  glm.pred <- rep(0 , nrow(datos))
  glm.pred[mod01.pred  > cut_off] = 1 #default
  return(mean(glm.pred == datos$Default))
}

cutoff <- seq(0,.2,0.0001)
accuracy <- sapply(seq(0,.2,0.0001), fAccuracy)
plot(cutoff, accuracy, xlim = c(0,1))
abline(h = 1, col = "red")

## 
set.seed(131822)
train <- sample(nrow(datosClean), nrow(datosClean)/2) ## la mitad de los datos para training
test  <- datosClean[-train,]
train <- datosClean[train,]

## Estimamos el modelo con las mismas variables que en el modelo4  
glm.model <- glm(data = datosClean[,-c(7, 9, 10, 13:28)],
                 Default ~ . ,
                 family = "binomial",
                 subset = train)
## Predecir con test.  
glm.model.pred <- predict(glm.model, test, type = "response")

fAccuracy(0.001, glm.model.pred, test)
fAccuracy(0.002, glm.model.pred, test)
fAccuracy(0.003, glm.model.pred, test)
fAccuracy(0.006, glm.model.pred, test)
fAccuracy(0.007, glm.model.pred, test)
fAccuracy(0.008, glm.model.pred, test)
fAccuracy(0.009, glm.model.pred, test)
fAccuracy(0.01, glm.model.pred, test)
fAccuracy(0.02, glm.model.pred, test)
fAccuracy(0.03, glm.model.pred, test)

cutoff <- seq(0,.2,0.0001)
accuracy <- sapply(seq(0,.2,0.0001), fAccuracy)
plot(cutoff, accuracy, xlim = c(0,1))
abline(h = 1, col = "red")

## Matriz de confusión y curva ROC ####################################################################
##
summary(modelo4)

ggplot(data = as.data.frame(predict(modelo4))) +
  geom_histogram(aes(x = predict(modelo4)), binwidth = .5, color = "white") +
  xlim(-10,0) + xlab("Valores de Prediccion")

hist(predict(modelo4,type = "response"))

ggplot(data = as.data.frame(predict(modelo4,type = "response"))) +
  geom_histogram(aes(x = predict(modelo4,type = "response")), binwidth = .0001, color = "white") +
  xlim(0,.014) + xlab("Valores de Prediccion")

table(predict(modelo4, type = "response") > 0.001)
table(predict(modelo4, type = "response") > 0.005)
table(predict(modelo4, type = "response") > 0.0075)
table(predict(modelo4, type = "response") > 0.01)
table(predict(modelo4, type = "response") > 0.02)
table(predict(modelo4, type = "response") > 0.03)
table(predict(modelo4, type = "response") > 0.05)

cut_off <- 0.0075

## In Sample
prob.modelo4.insample      <- predict(modelo4,train,type = "response")
predicted.modelo4.insample <- (prob.modelo4.insample > cut_off)
predicted.modelo4.insample <- as.numeric(predicted.modelo4.insample)

## Matriz de confusión
##
table(train$Default, predicted.modelo4.insample, dnn = c("Truth","Predicted"))

## ERROR

mean(ifelse(train$Default !=  predicted.modelo4.insample, 1, 0))

## OUT OF SAMPLE
prob.modelo4.outsample      <- predict(modelo4, test, type = "response")
predicted.modelo4.outsample <-  prob.modelo4.outsample > cut_off
predicted.modelo4.outsample <- as.numeric(predicted.modelo4.outsample)
table(test$Default, predicted.modelo4.outsample, dnn = c("Truth","Predicted"))

mean(ifelse(test$Default != predicted.modelo4.outsample, 1, 0))

## Curva ROC 
##
pred <- prediction(prob.modelo4.outsample, test$Default)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

## Encontrar cut off deacuerdo a funciones de costo
pcut <- cut_off
# Symmetric cost
costoSim <- function(r, pi){
  mean(((r == 0) & (pi > pcut)) | ((r == 1) & (pi < pcut)))
}
#Asymmetric cost
costoAsim <- function(r, pi){
  weight1 <- 2
  weight0 <- 1
  c1 <- (r == 1) & (pi < pcut) #logical vector - true if actual 1 but predict 0
  c0 <- (r == 0) & (pi > pcut) #logical vector - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}


## Cutoff óptimo dado una funcion de costos  


searchgrid = seq(0.001,0.07, 0.001)
#
result = cbind(searchgrid, NA)
#in the cost function, both r and pi are vectors, r=truth, pi=predicted probability
costF <- function(r, pi){
  weight1 = 100
  weight0 = 1
  c1 = (r == 1) & (pi < pcut) #logical vector - true if actual 1 but predict 0 false positive
  c0 = (r == 0) & (pi > pcut) #logical vector - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}

prob <- predict(glm.model,type = "response")

for (i in 1:length(searchgrid))
{
  pcut <- result[i,1]
  #assign the cost to the 2nd col
  result[i,2] <- costF(train$Default, prob)
}


plot(result, ylab = "Cost in Training Set")
result[which.min(result[,2]),]
