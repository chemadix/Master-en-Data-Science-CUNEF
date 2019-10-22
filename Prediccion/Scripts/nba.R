#######################################################################################################
## Prediccion
#######################################################################################################
## Salario de jugadores NBA
##
#######################################################################################################
## - Propósito
##      determinar el mejor modelo, dadas las variabes, para predecir el salario de los jugadores.
##      Partiremos de seleccionar un grupo de modelos que tengan buen poder explicativo. A partir
##      de este grupo de modelos, tomaremos el que mayor poder de prediccion tenga.
##
#######################################################################################################
##
##  Forward Stepwise para calcular el mejor modelo partiendo de pocas variables a muchas variables
##
##  Paquetes:
library(MASS)
library(dplyr)
library(readr)
library(leaps)
library(car)
library(fBasics)
library(akima)
library(ISLR)
##
datos <- na.omit(read_csv("nba.csv"))
##
## Con regsubset method = Forward, genera modelos agragando variables que mejoren el criterio de
##  información.
##   - He empezado con todas las variables menos Player y NBA_Country porque complican el proceso
##
regfit.fwd     <- regsubsets(data = datos, Salary ~ . - Player - NBA_Country, method ="forward")
regfit.summary <- summary(regfit.fwd )
##
## Ha genarado 8 modelos y se detiene.
##
## - Residual sum of squares for each model
regfit.summary$rss
##
## - The r-squared for each model
regfit.summary$rsq
##
## - Adjusted r-squared
regfit.summary$adjr2
##
## - Schwartz's information criterion, BIC
regfit.summary$bic
##
## Variables en modelos
variables <- colnames(regfit.summary$which)
##
## De los 8 modelos, nos quedaremos con los 4 que mayor poder explicativo tenga
## - Min BIC (cuatro modelos de los 8)
numModelo <- c()
for(i in 1:4){
  numModelo <- c(numModelo, which(regfit.summary$bic == sort(regfit.summary$bic)[i]))
}
numModelo
## Usaremos los modelos 6, 7, 8 y 5 
##   - modelos con minimo criterio de informacion BIC
mod6Names <- variables[regfit.summary$which[6,]][-1]
mod7Names <- variables[regfit.summary$which[7,]][-1]
mod8Names <- variables[regfit.summary$which[8,]][-1]
mod5Names <- variables[regfit.summary$which[5,]][-1]
## i.e. nombre de las variables sin intercepto
##
## Parece que HOU es significativo
##  - agregaré una columna con HOU (0 no pertenece y 1 pertenece)
datos$HOU <- 0
for(i in 1:nrow(datos)) {
  if(datos$Tm[i] == "HOU"){
    datos$HOU[i] <- 1
  }
}
#######################################################################################################
##
##  Analizaremos los modelos por separado para probar los supuestos y determinar si son adecuados o no.
##  Despues, de los modelos a utilizar escogeremos el que mejor prediga el salario.
##
mod6 <- lm( data = datos, Salary ~ NBA_DraftNumber + Age + G + MP + `USG%` + WS)
round(mean(mod6$residuals),2) == 0 ## E[res] = 0
qqPlot(mod6$residuals)             ## Normalidad: comparamos graficamente
jbTest(resid(mod6))                ## Normalidad: Jarque Bera (No norm)
crPlots(mod6)                      ## Linealidad: componentes - Residuales
ncvTest(mod6)                      ## Prueba de Heterocedasticidad *AJUSTE*
sqrt(vif(mod6)) > 2                ## Multicolinealidad (Prob. con G y MP)
influencePlot(mod6)
##
mod7 <- lm( data = datos, Salary ~ NBA_DraftNumber + Age + G + MP + `DRB%` + `USG%` + WS)
round(mean(mod7$residuals),2) == 0 ## E[res] = 0
qqPlot(mod7$residuals)             ## Normalidad: comparamos graficamente
jbTest(resid(mod7))                ## Normalidad: Jarque Bera (No norm)
crPlots(mod7)                      ## Linealidad: componentes - Residuales
ncvTest(mod7)                      ## Prueba de Heterocedasticidad *AJUSTE*
sqrt(vif(mod7)) > 2                ## Multicolinealidad (Prob. con G y MP)
##
mod8 <- lm( data = datos, Salary ~ NBA_DraftNumber + Age + HOU + G + MP + `DRB%` + `USG%` + WS)
round(mean(mod8$residuals),2) == 0 ## E[res] = 0
qqPlot(mod8$residuals)             ## Normalidad: comparamos graficamente
jbTest(resid(mod8))                ## Normalidad: Jarque Bera (No norm)
crPlots(mod8)                      ## Linealidad: componentes - Residuales
ncvTest(mod8)                      ## Prueba de Heterocedasticidad *AJUSTE*
sqrt(vif(mod8)) > 2                ## Multicolinealidad (Prob. con G y MP)
##
mod5 <- lm( data = datos, Salary ~  NBA_DraftNumber + Age + G + `USG%` + WS)
round(mean(mod5$residuals),2) == 0 ## E[res] = 0
qqPlot(mod5$residuals)             ## Normalidad: comparamos graficamente
jbTest(resid(mod5))                ## Normalidad: Jarque Bera (No norm)
crPlots(mod5)                      ## Linealidad: componentes - Residuales
ncvTest(mod5)                      ## Prueba de Heterocedasticidad *AJUSTE*
sqrt(vif(mod5)) > 2                ## Multicolinealidad (Sin Problemas)
##
#######################################################################################################
##
## Cross Validation
##  - Objetivo: encontrar el modelo que mejor prediga
##
MSE <- c()
##
##
## Modelo 6
set.seed(6)
numData  <- nrow(datos)
training <- sample(numData, numData/2)
mod6_T <- lm( data = datos, Salary ~ NBA_DraftNumber + Age + G + MP + `USG%` + WS, subset = training)
attach(datos)
MSE <- c(MSE,mean((datos$Salary-predict(mod6_T ,Auto))[-training]^2))
detach(datos)
##
## Modelo 7
set.seed(7)
numData  <- nrow(datos)
training <- sample(numData, numData/2)
mod7_T <- lm( data = datos, Salary ~ NBA_DraftNumber + Age + G + MP + `DRB%` + `USG%` + WS,
              subset = training)
attach(datos)
MSE <- c(MSE,mean((datos$Salary-predict(mod7_T ,Auto))[-training]^2))
detach(datos)
##
## Modelo 8
set.seed(8)
numData  <- nrow(datos)
training <- sample(numData, numData/2)
mod8_T <- lm( data = datos, Salary ~ NBA_DraftNumber + Age + HOU + G + MP + `DRB%` + `USG%` + WS,
              subset = training)
attach(datos)
MSE <- c(MSE,mean((datos$Salary-predict(mod8_T ,Auto))[-training]^2))
detach(datos)
##
## Modelo 5
set.seed(5)
numData  <- nrow(datos)
training <- sample(numData, numData/2)
mod5_T <- lm( data = datos, Salary ~ NBA_DraftNumber + Age + G + `USG%` + WS, subset = training)
attach(datos)
MSE <- c(MSE,mean((datos$Salary-predict(mod5_T ,Auto))[-training]^2))
detach(datos)
##
##
MSE
## El modelo con menor MSE es el modelo 8