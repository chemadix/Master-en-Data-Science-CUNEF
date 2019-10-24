########################################################################################################
## Start Date: 12/10/2019
## End Date: 15/10/2019
## Author: José María Álvarez Silva
## School: CUNEF
## Class: Predicción
## Language: Spanish
##
########################################################################################################
## Clase de fundamentos para el analisis de datos y la investigacion
#############################################################################################
## La Cesta
#############################################################################################
## Paquetes
##
library(MASS)
##
datos <- read.csv("LA CESTA.csv")
#############################################################################################
## Primer acercamiento a los datos
##
## Media
mean(datos$x)
## Varianza 
var(datos$x)
## Dimension de los datos
length(datos$x)
## Desviacion Estandar
sd(datos$x)
## Min y Max
min(datos$x)
max(datos$x)
## Visualizacion de los datos
table(datos)
plot(table(datos))
summary(datos)
##
## Dado que la media y la varianza son similares, proponemos un modelo Poisson
##
fit <- fitdistr(datos$x, densfun = "Poisson")
fit
lambda <- fit$estimate
#############################################################################################
## Se necesita conocer el modelo que recoge el comportamiento, 
## asi como algunas de las principales probabilidades de ocurrencia:
## 
##  a) Que en un minuto cualquiera no acceda ningun cliente.
##
ppois(0,mean(datos$x))
##
##  - donde lambda del modelo la estimamos con mean(datos$x)
## 
##  b) Que en un minuto accedan entre 2 y 5 clientes.
##
ppois(5,mean(datos$x)) - ppois(1,mean(datos$x))
##
(dpois(2, mean(datos$x)) + dpois(3, mean(datos$x)) +
    dpois(4, mean(datos$x)) + dpois(5, mean(datos$x)))
##  
##  - para comprobar y estar seguro del reslutado he utilizzdo dos metodos distintos
##    1) restando las probabilidades acumalas y 2) sumando la probabilidad puntual para 
##    cada uno de los casos
## 
##  c) Que en 5 minutos accedan mas de 10 clientes.
##
##     - Tenemos que cambiar el modelo para que en lugar de por minuto sea por cada 5 minutos
##     - Sabemos que la suma de n poissons iid es una poisson con lambda = n*lambda 
##       usaremos una distribucion poisson con lambda2 =  5*lambda1
##
1 - ppois(10, 5*mean(datos$x))
##
## Adicionalmente, será necesario conocer también entre que valores se podría encontrar 
## el número medio de clientes accediendo a la cola única, con un nivel de confianza del 95%
##
test <- t.test(datos$x)
## - Intervalo
test$conf.int
##
## Uno de los miembros del equipo consigue datos relativos al acceso a caja de una
## empresa competidora del sector, la cual contiene 500 registros y muestra 0,69 como
## valor medio y desviacion tipica 0,96. Los miembros del equipo discuten entre la
## similitud de ambos comercios, pero deberán buscar una respuesta. Analice el caso y
## haga una propuesta.
##
## - lo primero a comentar, el segundo grupo de datos tiene media y desviacion estandar
##   distintas por lo que no deberia distribuirse poisson como la anterior.
##
## - al no contar con la muestra conseguida por nuestros compañeros, usaremos los estimadores
##   que nos han dado y generaremos una muestra simulando.
##
## - al tener estimadores distintos para la media y la sd, es complicado utilizar alguna
##   distribucion discreta para la simulacion, por lo tanto simularemos con una normal.
##
## - Fijamos una semilla para que las conclusiones no varien
##
set.seed(132822)
muestraSim <- rnorm(500,mean = 0.69, sd = 0.96)
## 
difMedias <- t.test(muestraSim , datos$x)
difMedias
## 
## - Observando los resultados del test:
##    - el p-value es grande (0.5391) por lo que no podemos rechazar la hipotesis nula
##         i.e. H0 las diferencia de las medias es cero (las medias son iguales)
##    - el intervalo de confianza (-0.14465965, 0.07567221) contiene al cero
##         - esto es un buen indicador de que las madias pueden ser iguales
##         - no podemos aceptar que la diferencia de medias sea ditinta a cero
##    - no podemos concluir que las muestras provengan de poblaciones distintas con 95%
##         de confianza.

