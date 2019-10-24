########################################################################################################
## Start Date: 20/10/2019
## End Date: 23/10/2019
## Author: José María Álvarez Silva
## School: CUNEF
## Class: Predicción
## Language: Spanish
##
########################################################################################################
## Predicción
########################################################################################################
## GAM Bikes
##
########################################################################################################
## Caso: GAM Bikes
##
## Propósito ###########################################################################################
## - encontrar un modelo predictivo para el número de bicicletas alquiladas por día

## Paqueteria ##########################################################################################
##
library(readr)    ##  cargar data sets
library(ISLR)     ##  paqueteria de Introduction to statistical learning in R
library(skimr)    ##  Beautiful Summarize
library(ggplot2)  ##  Ploting
library(dplyr)    ##  programación tidy
library(corrplot) ##  gráficas de correlación
library(splines)  ##  Ajustes via splines
library(boot)     ##  Bootstraping
library(gam)
## Analisis Exploratorio de Datos ######################################################################
##

## Dataset: trabajaremos unicamente por días
day <- read_csv("day.csv")[,-1]      ## eliminamos la columna "instant"

skim(day)

## Variable cnt

ggplot(data = day, aes(x = cnt)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, colour = "black", fill = "steelblue4") +
  geom_density(alpha = .2, fill = "#FF6666") +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Frecuency") )

## Relación con las demás variables

ggplot(data = day) + geom_boxplot(aes(y = temp, group = season, colour = as.factor(season))) 

ggplot(data = day, aes(factor(mnth), cnt, colour = factor(mnth))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))

ggplot(data = day, aes(factor(season), cnt, colour = factor(season))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))

ggplot(data = day, aes(factor(weekday), cnt, colour = factor(weekday))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))
## parace que un grupo esta contenido en el otro y sera dificil diferenciarlos

ggplot(data = day, aes(factor(weathersit), cnt, colour = factor(weathersit))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))

ggplot(data = day, aes(factor(holiday), cnt, colour = factor(holiday))) + geom_boxplot()
## parace que un grupo esta contenido en el otro y sera dificil diferenciarlos

ggplot(data = day, aes(factor(workingday), cnt, colour = factor(workingday))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))
## parace que un grupo esta contenido en el otro y sera dificil diferenciarlos



## variables continuas

ggplot(data = day) +
  geom_point(aes(x = temp, y = cnt, colour = as.factor(season),
                 size = as.factor(weekday), alpha = as.factor(1/weekday))) + 
  geom_smooth(aes(x = temp, y = cnt))

ggplot(data = day) +
  geom_point(aes(x = temp, y = cnt, colour = as.factor(season))) + 
  geom_smooth(aes(x = temp, y = cnt))
  
ggplot(data = day) +
  geom_point(aes(x = hum, y = cnt, colour = as.factor(season))) + 
  geom_smooth(aes(x = hum, y = cnt))

ggplot(data = day) +
  geom_point(aes(x = windspeed, y = cnt, colour = as.factor(season))) + 
  geom_smooth(aes(x = windspeed, y = cnt)) +
  geom_smooth(aes(x = windspeed, y = cnt), method = "lm", col = "red")


corrplot(cor(day[,-c(1,3)])) ## correlaciones de las variables sin fecha ni año

## Selección de Modelos ################################################################################
##

## Polinomial:
## - Para cada variable buscaremos el grado del polynomio óptimo (el que tenga error mínimo)
##   que mejor se ajuste a la realción entre la variable a explicar y el regresor. Generaremos
##   el error a través de k-fold cross-validation.

ajustePoly <- function(data = day, columna, nombre, semilla = 131822){
  
  df <- as.data.frame(data[,c(15,columna)]) ## variable a explicar y número de columna del regresor
  colnames(df) <- c("cnt", "Var")
  
  set.seed(semilla)
  cv.errors <- data.frame(grado = 1:min(5,length(unique(df$Var)) - 1), 
                          error = rep(NA, min(5,length(unique(df$Var)) - 1)))
  for (i in 1:min(5,length(unique(df$Var)) - 1)) {  # iteramos para variar los grados del polinomio
    glm.fit <- glm(data = df, cnt ~ poly(Var, i))
    cv.errors$error[i] <- cv.glm(df, glm.fit, K = 10)$delta[1]
  }
  
  minG <- which(min(cv.errors$error) == cv.errors$error)
  glm.fit <- glm(data = df, cnt ~ poly(Var, minG))
  
  varlims <- range(df$Var)
  pred    <- predict(glm.fit, newdata = list(Var = seq(varlims[1], varlims[2], length.out = 1000)),
                     se = TRUE)
  
  ggplot() +
    geom_point(data = df, aes(x = Var, y = cnt))  +
    geom_smooth(data = df, aes(x = Var, y = cnt), method = "lm", col = "red") +
    geom_line(aes(x = seq(varlims[1], varlims[2], length.out = 1000),
                  y = pred$fit),
              col = "steelblue", size = 1.5) + 
    ggtitle(paste("Grado del Polinomio = ", minG)) + 
    xlab(nombre)

}

## season
ajustePoly(data = day, columna = 2, nombre = "season")
## grado del polinomio: 2

## month
ajustePoly(data = day, columna = 4, nombre = "month")
## Grado del polinomio: 3

## holiday
ajustePoly(data = day, columna = 5, nombre = "holiday")
## grado del polinomio: 1

## weekday
ajustePoly(data = day, columna = 6, nombre = "weekday")
## Grado del polinomio: 1

## workingday
ajustePoly(data = day, columna = 7, nombre = "workingday")
## grado del polinomio: 1

## weathersit
ajustePoly(data = day, columna = 8, nombre = "weathersit")
## Grado del polinomio: 2

## temp
ajustePoly(data = day, columna = 9, nombre = "Temperature")
## grado del polinomio: 3

## atemp
ajustePoly(data = day, columna = 10, nombre = "atemp")
## Grado del polinomio: 3

## humidity
ajustePoly(data = day, columna = 11, nombre = "humidity")
## Grado del polinomio: 3

## windspeed
ajustePoly(data = day, columna = 12, nombre = "windspeed")
## Grado del polinomio: 3

## Splines:
## - Adicional al analisis de polinomios, buscamos el spline optimo variando los grados de libertad.
##   Asi evitamos la complicacion de elegir el número de nodos, através de cross validation. El
##   siguiente algoritmos nos muestra los grados de liberad del spline tal que este sea óptimo via 
##   cross-validation.

## Función para encontrar el óptimo grado de libertad para el Spline y graficar ajuste
##  comparando con ajuste lineal. Para variables continuas:
splineDFyGCont <- function(data = day, columna, nombre){
  
  df <- as.data.frame(data[,c(15,columna)]) ## variable a explicar y número de columna del regresor
  colnames(df) <- c("cnt", "Var")
  
  x <- smooth.spline(df$Var, df$cnt, cv = TRUE)$df
  
  ggplot() +
    geom_point(data = df, aes(x = Var, y = cnt))  +
    geom_smooth(data = df, aes(x = Var, y = cnt), method = "lm", col = "red") +
    geom_line(aes(x = smooth.spline(df$Var, df$cnt, cv = TRUE)$x,
                  y = smooth.spline(df$Var, df$cnt, cv = TRUE)$y),
              col = "forestgreen", size = 1.5) + 
    ggtitle(paste("Modelo con DF = ", x)) + 
    xlab(nombre) 
}

## Función para encontrar el óptimo grado de libertad para el Spline y graficar ajuste
##  comparando con ajuste lineal y loess. Para variables discretas:
splineDFyGDisc <- function(data = day, columna, nombre){
  
  df <- as.data.frame(data[,c(15,columna)]) ## variable a explicar y número de columna del regresor
  colnames(df) <- c("cnt", "Var")
  
  x <- smooth.spline(df$Var, df$cnt, cv = TRUE)$df
  
  ggplot() +
    geom_boxplot(data = df, aes(x = factor(Var), y = cnt, colour = factor(Var)))  +
    geom_smooth(data = df, aes(x = Var, y = cnt), method = "lm", col = "red") +
    geom_line(aes(x = smooth.spline(df$Var, df$cnt, cv = TRUE)$x,
                  y = smooth.spline(df$Var, df$cnt, cv = TRUE)$y),
              col = "forestgreen", size = 1.5) + 
    ggtitle(paste("Modelo con DF = ", x)) + 
    xlab(nombre) + labs(colour = nombre)
}

dfMatrix <- list()

## Variables

## Season
splineDFyGDisc(data = day, 2, "season")
dfMatrix$season <- smooth.spline(day$season, day$cnt, cv = TRUE)$df


## mnth
splineDFyGDisc(data = day, 4, "Months")
dfMatrix$months <- smooth.spline(day$mnth, day$cnt, cv = TRUE)$df

## holiday <- se incluirá de manera lineal en el modelo

## weekday <- se incluirá de manera lineal en el modelo

## workingday <- se incluirá de manera lineal en el modelo

## weathersit <- se incluirá de manera lineal en el modelo

## temp
splineDFyGCont(data = day, 9, "Temperature")
dfMatrix$temp <- smooth.spline(day$temp, day$cnt, cv = TRUE)$df

## atemp
splineDFyGCont(data = day, 10, "atemp")
dfMatrix$atemp <- smooth.spline(day$atemp, day$cnt, cv = TRUE)$df

## hum
splineDFyGCont(data = day, 11, "Humidity")
dfMatrix$hum <- smooth.spline(day$hum, day$cnt, cv = TRUE)$df

## windspeed
splineDFyGCont(data = day, 12, "windspeed")
dfMatrix$windspeed <- smooth.spline(day$windspeed, day$cnt, cv = TRUE)$df

## Modelos ############################################################################################
##

## Lineal

modelo1 <- gam(data = day, cnt ~ season + mnth + holiday + weekday + workingday +
                 weathersit + temp + atemp + hum + windspeed)

## Solamente polinomios

modelo2 <- glm(data = day, cnt ~ poly(season, 2) + poly(mnth, 3) + holiday + weekday + workingday +
                 poly(weathersit, 2) + poly(temp, 3) + poly(atemp, 3) + poly(hum, 3) +
                 poly(windspeed, 3))

## Solamente Spline

modelo3 <- gam(data = day, cnt ~ s(season, df = dfMatrix$season) + s(mnth, df = dfMatrix$months) +
                 holiday + weekday + workingday + weathersit + s(temp, df = dfMatrix$temp) +
                 s(atemp, df = dfMatrix$atemp) + s(hum, df = dfMatrix$hum) +
                 s(windspeed, df = dfMatrix$windspeed))

## Cross- validatiom ##################################################################################
##

set.seed(131822)

cv.glm(day, modelo1, K = 10)$delta[1] %>% print() %>% sqrt()
##

set.seed(131822)

cv.glm(day, modelo2, K = 10)$delta[1] %>% print() %>% sqrt()
##

set.seed(131822)

cv.glm(day, modelo3, K = 10)$delta[1] %>% print() %>% sqrt()

########################################################################################################
## Analisis Alternativo (casual y registered por separado)  ###############################################
##
## Analisis Exploratorio de Datos ######################################################################
##

##########################################################
## Variable registered

ggplot(data = day, aes(x = registered)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, colour = "black", fill = "steelblue4") +
  geom_density(alpha = .2, fill = "#FF6666") +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Frecuency") )

## Relación con las demás variables

ggplot(data = day, aes(factor(mnth), registered, colour = factor(mnth))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))

ggplot(data = day, aes(factor(season), registered, colour = factor(season))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))

ggplot(data = day, aes(factor(weekday), registered, colour = factor(weekday))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))
## a diferencia del resultado obtenido en cnt, para registered parecen influenciar más

ggplot(data = day, aes(factor(weathersit), registered, colour = factor(weathersit))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))

ggplot(data = day, aes(factor(holiday), registered, colour = factor(holiday))) + geom_boxplot()
## a diferencia del resultado obtenido en cnt, para registered parecen influenciar más

ggplot(data = day, aes(factor(workingday), registered, colour = factor(workingday))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))
## a diferencia del resultado obtenido en cnt, para registered parecen influenciar más


## variables continuas

ggplot(data = day) +
  geom_point(aes(x = temp, y = registered, colour = as.factor(season),
                 size = as.factor(weekday), alpha = as.factor(1/weekday))) + 
  geom_smooth(aes(x = temp, y = registered))

ggplot(data = day) +
  geom_point(aes(x = temp, y = registered, colour = as.factor(season))) + 
  geom_smooth(aes(x = temp, y = registered))

ggplot(data = day) +
  geom_point(aes(x = hum, y = registered, colour = as.factor(season))) + 
  geom_smooth(aes(x = hum, y = registered))

ggplot(data = day) +
  geom_point(aes(x = windspeed, y = registered, colour = as.factor(season))) + 
  geom_smooth(aes(x = windspeed, y = registered)) +
  geom_smooth(aes(x = windspeed, y = registered), method = "lm", col = "red")

##########################################################
## Variable casual

ggplot(data = day, aes(x = casual)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, colour = "black", fill = "steelblue4") +
  geom_density(alpha = .2, fill = "#FF6666") +
  scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Frecuency") )

## Relación con las demás variables

ggplot(data = day, aes(factor(mnth), casual, colour = factor(mnth))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))

ggplot(data = day, aes(factor(season), casual, colour = factor(season))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))

ggplot(data = day, aes(factor(weekday), casual, colour = factor(weekday))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))
## a diferencia del resultado obtenido en cnt, para casual parecen influenciar más

ggplot(data = day, aes(factor(weathersit), casual, colour = factor(weathersit))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))

ggplot(data = day, aes(factor(holiday), casual, colour = factor(holiday))) + geom_boxplot()
## a diferencia del resultado obtenido en cnt, para casual parecen influenciar más

ggplot(data = day, aes(factor(workingday), casual, colour = factor(workingday))) + geom_boxplot() +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1))
## a diferencia del resultado obtenido en cnt, para casual parecen influenciar más


## variables continuas

ggplot(data = day) +
  geom_point(aes(x = temp, y = casual, colour = as.factor(season),
                 size = as.factor(weekday), alpha = as.factor(1/weekday))) + 
  geom_smooth(aes(x = temp, y = casual))

ggplot(data = day) +
  geom_point(aes(x = temp, y = casual, colour = as.factor(season))) + 
  geom_smooth(aes(x = temp, y = casual))

ggplot(data = day) +
  geom_point(aes(x = hum, y = casual, colour = as.factor(season))) + 
  geom_smooth(aes(x = hum, y = casual))

ggplot(data = day) +
  geom_point(aes(x = windspeed, y = casual, colour = as.factor(season))) + 
  geom_smooth(aes(x = windspeed, y = casual)) +
  geom_smooth(aes(x = windspeed, y = casual), method = "lm", col = "red")


## Selección de Modelos ################################################################################
##

## Polinomial:
## - Para cada variable buscaremos el grado del polynomio óptimo (el que tenga error mínimo)
##   que mejor se ajuste a la realción entre la variable a explicar y el regresor. Generaremos
##   el error a través de k-fold cross-validation.

ajustePolyRegCas <- function(data = day, columna, nombre, semilla = 131822, y){
  
  df <- as.data.frame(data[,c(y,columna)]) ## variable a explicar y número de columna del regresor
  colnames(df) <- c("y", "Var")
  
  if (y == 14) {
    yChar <- " para Registered"
  }
  if (y == 13) {
    yChar <- " para Casual"
  }
  
  set.seed(semilla)
  cv.errors <- data.frame(grado = 1:min(5,length(unique(df$Var)) - 1), 
                          error = rep(NA, min(5,length(unique(df$Var)) - 1)))
  for (i in 1:min(5,length(unique(df$Var)) - 1)) {  # iteramos para variar los grados del polinomio
    glm.fit <- glm(data = df, y ~ poly(Var, i))
    cv.errors$error[i] <- cv.glm(df, glm.fit, K = 10)$delta[1]
  }
  
  minG <- which(min(cv.errors$error) == cv.errors$error)
  glm.fit <- glm(data = df, y ~ poly(Var, minG))
  
  varlims <- range(df$Var)
  pred    <- predict(glm.fit, newdata = list(Var = seq(varlims[1], varlims[2], length.out = 1000)),
                     se = TRUE)
  
  ggplot() +
    geom_point(data = df, aes(x = Var, y = y))  +
    geom_smooth(data = df, aes(x = Var, y = y), method = "lm", col = "red") +
    geom_line(aes(x = seq(varlims[1], varlims[2], length.out = 1000),
                  y = pred$fit),
              col = "steelblue", size = 1.5) + 
    ggtitle(paste(paste("Grado del Polinomio = ", minG), yChar)) + 
    xlab(nombre)
  
}

##################################################################################
## Para registered

## season
ajustePolyRegCas(data = day, columna = 2, nombre = "season", y = 14)
## grado del polinomio: 2

## month
ajustePolyRegCas(data = day, columna = 4, nombre = "month", y = 14)
## Grado del polinomio: 3

## holiday
ajustePolyRegCas(data = day, columna = 5, nombre = "holiday", y = 14)
## grado del polinomio: 1

## weekday
ajustePolyRegCas(data = day, columna = 6, nombre = "weekday", y = 14)
## Grado del polinomio: 2 ## OJO

## workingday
ajustePolyRegCas(data = day, columna = 7, nombre = "workingday", y = 14)
## grado del polinomio: 1

## weathersit
ajustePolyRegCas(data = day, columna = 8, nombre = "weathersit", y = 14)
## Grado del polinomio: 2

## temp
ajustePolyRegCas(data = day, columna = 9, nombre = "Temperature", y = 14)
## grado del polinomio: 3

## atemp
ajustePolyRegCas(data = day, columna = 10, nombre = "atemp", y = 14)
## Grado del polinomio: 3

## humidity
ajustePolyRegCas(data = day, columna = 11, nombre = "humidity", y = 14)
## Grado del polinomio: 3

## windspeed
ajustePolyRegCas(data = day, columna = 12, nombre = "windspeed", y = 14)
## Grado del polinomio: 4 ## OJO

##################################################################################
## Para casual

## season
ajustePolyRegCas(data = day, columna = 2, nombre = "season", y = 13)
## grado del polinomio: 2

## month
ajustePolyRegCas(data = day, columna = 4, nombre = "month", y = 13)
## Grado del polinomio: 4 ## OJO

## holiday
ajustePolyRegCas(data = day, columna = 5, nombre = "holiday", y = 13)
## grado del polinomio: 1

## weekday
ajustePolyRegCas(data = day, columna = 6, nombre = "weekday", y = 13)
## Grado del polinomio: 5 ## OJO

## workingday
ajustePolyRegCas(data = day, columna = 7, nombre = "workingday", y = 13)
## grado del polinomio: 1

## weathersit
ajustePolyRegCas(data = day, columna = 8, nombre = "weathersit", y = 13)
## Grado del polinomio: 1

## temp
ajustePolyRegCas(data = day, columna = 9, nombre = "Temperature", y = 13)
## grado del polinomio: 3

## atemp
ajustePolyRegCas(data = day, columna = 10, nombre = "atemp", y = 13)
## Grado del polinomio: 3

## humidity
ajustePolyRegCas(data = day, columna = 11, nombre = "humidity", y = 13)
## Grado del polinomio: 2 ## OJO

## windspeed
ajustePolyRegCas(data = day, columna = 12, nombre = "windspeed", y = 13)
## Grado del polinomio: 4 ## OJO

## Splines:
## - Adicional al análisis de polinomios, buscamos el spline óptimo variando los grados de libertad.
##   Asi evitamos la complicacion de elegir el número de nodos, através de cross validation. El
##   siguiente algoritmos nos muestra los grados de liberad del spline tal que este sea óptimo via 
##   cross-validation.

## Función para encontrar el óptimo grado de libertad para el Spline y graficar ajuste
##  comparando con ajuste lineal. Para variables continuas:
splineDFyGContRegCas <- function(data = day, columna, nombre, y){
  
  df <- as.data.frame(data[,c(y,columna)]) ## variable a explicar y número de columna del regresor
  colnames(df) <- c("y", "Var")
  
  if (y == 14) {
    yChar <- " para Registered"
  }
  if (y == 13) {
    yChar <- " para Casual"
  }
  
  x <- smooth.spline(df$Var, df$y, cv = TRUE)$df
  
  ggplot() +
    geom_point(data = df, aes(x = Var, y = y))  +
    geom_smooth(data = df, aes(x = Var, y = y), method = "lm", col = "red") +
    geom_line(aes(x = smooth.spline(df$Var, df$y, cv = TRUE)$x,
                  y = smooth.spline(df$Var, df$y, cv = TRUE)$y),
              col = "forestgreen", size = 1.5) + 
    ggtitle(paste(paste("Modelo con DF = ", x), yChar)) + 
    xlab(nombre) 
}

## Función para encontrar el óptimo grado de libertad para el Spline y graficar ajuste
##  comparando con ajuste lineal y loess. Para variables discretas:
splineDFyGDiscRegCas <- function(data = day, columna, nombre, y){
  
  df <- as.data.frame(data[,c(y,columna)]) ## variable a explicar y número de columna del regresor
  colnames(df) <- c("y", "Var")
  
  if (y == 14) {
    yChar <- " para Registered"
  }
  if (y == 13) {
    yChar <- " para Casual"
  }
  
  x <- smooth.spline(df$Var, df$y, cv = TRUE)$df
  
  ggplot() +
    geom_boxplot(data = df, aes(x = factor(Var), y = y, colour = factor(Var)))  +
    geom_smooth(data = df, aes(x = Var, y = y), method = "lm", col = "red") +
    geom_line(aes(x = smooth.spline(df$Var, df$y, cv = TRUE)$x,
                  y = smooth.spline(df$Var, df$y, cv = TRUE)$y),
              col = "forestgreen", size = 1.5) + 
    ggtitle(paste(paste("Modelo con DF = ", x), yChar)) + 
    xlab(nombre) + labs(colour = nombre)
}

dfMatrixReg <- list()

## Variables para analizar registered

## Season
splineDFyGDiscRegCas(data = day, 2, "season", y = 14)
dfMatrixReg$season <- smooth.spline(day$season, day$registered, cv = TRUE)$df


## mnth
splineDFyGDiscRegCas(data = day, 4, "Months", y = 14)
dfMatrixReg$months <- smooth.spline(day$mnth, day$registered, cv = TRUE)$df

## holiday <- se incluirá de manera lineal en el modelo

## weekday 
splineDFyGDiscRegCas(data = day, 6, "weekday", y = 14)
dfMatrixReg$weekdays <- smooth.spline(day$mnth, day$registered, cv = TRUE)$df

## workingday <- se incluirá de manera lineal en el modelo

## weathersit <- se incluirá de manera lineal en el modelo

## temp
splineDFyGContRegCas(data = day, 9, "Temperature", y = 14)
dfMatrixReg$temp <- smooth.spline(day$temp, day$registered, cv = TRUE)$df

## atemp
splineDFyGContRegCas(data = day, 10, "atemp", y = 14)
dfMatrixReg$atemp <- smooth.spline(day$atemp, day$registered, cv = TRUE)$df

## hum
splineDFyGContRegCas(data = day, 11, "Humidity", y = 14)
dfMatrixReg$hum <- smooth.spline(day$hum, day$registered, cv = TRUE)$df

## windspeed
splineDFyGContRegCas(data = day, 12, "windspeed", y = 14)
dfMatrixReg$windspeed <- smooth.spline(day$windspeed, day$registered, cv = TRUE)$df

###########################################################################
## Variables para analizar casual

dfMatrixCas <- list()

## Season
splineDFyGDiscRegCas(data = day, 2, "season", y = 13)
dfMatrixCas$season <- smooth.spline(day$season, day$casual, cv = TRUE)$df

## mnth
splineDFyGDiscRegCas(data = day, 4, "Months", y = 13)
dfMatrixCas$months <- smooth.spline(day$mnth, day$casual, cv = TRUE)$df

## holiday <- se incluirá de manera lineal en el modelo

## weekday 
splineDFyGDiscRegCas(data = day, 6, "weekday", y = 13)
dfMatrixCas$months <- smooth.spline(day$weekday, day$casual, cv = TRUE)$df

## workingday <- se incluirá de manera lineal en el modelo

## weathersit <- se incluirá de manera lineal en el modelo

## temp
splineDFyGContRegCas(data = day, 9, "Temperature", y = 13)
dfMatrixCas$temp <- smooth.spline(day$temp, day$casual, cv = TRUE)$df

## atemp
splineDFyGContRegCas(data = day, 10, "atemp", y = 13)
dfMatrixCas$atemp <- smooth.spline(day$atemp, day$casual, cv = TRUE)$df

## hum
splineDFyGContRegCas(data = day, 11, "Humidity", y = 13)
dfMatrixCas$hum <- smooth.spline(day$hum, day$casual, cv = TRUE)$df

## windspeed
splineDFyGContRegCas(data = day, 12, "windspeed", y = 13)
dfMatrixCas$windspeed <- smooth.spline(day$windspeed, day$casual, cv = TRUE)$df

## Modelos ############################################################################################
##

## Lineal

modelo1Reg <- gam(data = day, registered ~ season + mnth + holiday + weekday + workingday +
                 weathersit + temp + atemp + hum + windspeed)

modelo1Cas <- gam(data = day, casual ~ season + mnth + holiday + weekday + workingday +
                    weathersit + temp + atemp + hum + windspeed)

## Solamente polinomios

modelo2Reg <- glm(data = day, registered ~ poly(season, 2) + poly(mnth, 3) + holiday +
                    poly(weekday, 2) + workingday + poly(weathersit,2) + poly(temp, 3) +
                    poly(atemp, 3) + poly(hum, 3) + poly(windspeed, 4))

modelo2Cas <- glm(data = day, casual ~ poly(season, 2) + poly(mnth, 4) + holiday +
                    poly(weekday, 5) + workingday + weathersit +
                    poly(temp, 3) + poly(atemp, 3) + poly(hum, 2) + poly(windspeed, 4))

## Solamente Spline

modelo3Reg <- gam(data = day, registered ~ s(season, df = dfMatrixReg$season) + 
                    s(mnth, df = dfMatrixReg$months) + holiday +
                    s(weekday, df = dfMatrixReg$weekdays) + workingday +
                    weathersit + s(temp, df = dfMatrixReg$temp) +
                    s(atemp, df = dfMatrixReg$atemp) + s(hum, df = dfMatrixReg$hum) +
                    poly(windspeed, df = dfMatrixReg$windspeed))

modelo3Cas <- gam(data = day, casual ~ s(season, df = dfMatrixCas$season) + 
                    s(mnth, df = dfMatrixCas$months) + holiday + 
                    s(weekday, df = dfMatrixCas$months) + workingday + weathersit +
                    s(temp, df = dfMatrixCas$temp) +
                    s(atemp, df = dfMatrixCas$atemp) + s(hum, df = dfMatrixCas$hum) +
                    poly(windspeed, df = dfMatrixCas$windspeed))

## Cross- validatiom ##################################################################################
##

set.seed(131822)

cv.glm(day, modelo1Reg, K = 10)$delta[1] %>% print() %>% sqrt()
cv.glm(day, modelo1Cas, K = 10)$delta[1] %>% print() %>% sqrt()
##

set.seed(131822)

cv.glm(day, modelo2Reg, K = 10)$delta[1] %>% print() %>% sqrt()
cv.glm(day, modelo1Cas, K = 10)$delta[1] %>% print() %>% sqrt()
##

set.seed(131822)

cv.glm(day, modelo3Reg, K = 10)$delta[1] %>% print() %>% sqrt()
cv.glm(day, modelo1Cas, K = 10)$delta[1] %>% print() %>% sqrt()
