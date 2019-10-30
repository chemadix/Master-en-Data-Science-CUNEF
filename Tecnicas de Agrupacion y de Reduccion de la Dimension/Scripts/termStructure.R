######################################################################################################
## Start Date: 25/10/2019
## End Date: 30/10/2019
## Author: José María Álvarez Silva
## School: CUNEF
## Class: TAyRD
## Language: Spanish
##
######################################################################################################
## Tecnica de Agrupación y de Reducción de la Dimensión
######################################################################################################
## Estructura temporal subyacente tipos de interés ###################################################
##
######################################################################################################
## Propósito  ########################################################################################
##
## El objetivo que perseguimos en el presente trabajo es, simplemente, efectuar una comprobación 
## empírica mediante la aplicación del ACP a un conjunto de 978 observaciones de los rendimientos de
## 10 bonos norteamericanos a distintos plazos entre el 2 de enero de 1995 y 30 de septiembre de 1998.
## No pretendemos nada más que verificar si, tal y como plantean los estudios teóricos, puede 
## establecerse una estructura subyecente que sintetice y agrupe los distintos plazos en virtud de sus
## características comunes. Para ello, deberá trabajar con el archivo ACPTIUSD.csv Vista previa del
## documento, disponible en la plataforma, del que deberá utilizar las 949 primeras observaciones 
## (denominadas observaciones activas) y las 9 primeras variables (las variables activas); uno de los 
## objetivos será emplear las observaciones 950 a 978 (llamadas observaciones suplementarias) para
## predecir el valor del bono a 10 años (IRS.10Y, variable suplementaria). Aparte de cubrir este 
## objetivo, queremos asimismo tener respuesta a las siguientes preguntas:
##
## 1. Tiene sentido llevar a cabo, en este caso, un análisis de componentes principales? Para 
## justificarlo, deberá llevar a cabo las pruebas que estime oportunas, como, por ejemplo el 
## análisis de la matriz de correlaciones, el del determinante de dicha matriz, la prueba de 
## esfericidad de Bartlett, el KMO o el MSA;
## 
## 2. ¿Cuántos componentes permitirían explicar, adecuadamente, la estructura subycente de los
## tipos de interés aquí analizados? 
##
## Justifique su respuesta empleando, por ejemplo, las pruebas de la varianza explicada o del
## gráfico de sedimentación;
## 
## 3. Finalmente, ¿tiene sentido llevar a cabo una rotación de las variables subyacentes? Para
## responder, lleva a cabo una rotación Varimax, por ejemplo.
## 
## Por último, deberá elaborar las oportunas conclusiones.
######################################################################################################
## Paqueterias:
##
library(readr)
library(skimr)                 # Beautiful Summarize
library(corrplot)              # Correlations
library(corrplot)              # Correlations
library(tidyverse)             # tidy
library(factoextra)
library(FactoMineR)
library(psych)
library(rpart)

## Datos:

datosInt <- read.csv("ACPTIUSD.csv", sep = ";")
ACPTIUSD <- read.csv("ACPTIUSD.csv", sep = ";")

## ACP
## entre el 2 de enero de 1995 y el 30 de septiembre de 1998
## 978 observaciones

## USAR:
##   OBSERVACIONES: 
##     949 primeras observaciones (denominadas observaciones ACTIVAS) 
##   VARIABLES:
##     9  primeras variables (las variables activas)
##   OBSERVACIONES:
##     950 a 978 (llamadas observaciones SUPLEMENTARIAS)
##   PREDICCIÓN:  predecir el valor del bono a 10 años (IRS.10Y, variable suplementaria)

activas <- na.omit(datosInt[1:949, -c(1, 11)])

xseq <- c(1/12, 3/12, 6/12, 1, 2, 3, 4, 5, 7)

ggplot() + geom_point(aes(x = xseq, y = as.double(activas[1,])), col = palette(rainbow(15))[1]) + 
  geom_point(aes(x = xseq, y = as.double(activas[2,])), col = palette(rainbow(15))[2]) + 
  geom_point(aes(x = xseq, y = as.double(activas[52,])), col = palette(rainbow(15))[3]) + 
  geom_point(aes(x = xseq, y = as.double(activas[102,])), col = palette(rainbow(15))[4]) + 
  geom_point(aes(x = xseq, y = as.double(activas[152,])), col = palette(rainbow(15))[5]) + 
  geom_point(aes(x = xseq, y = as.double(activas[202,])), col = palette(rainbow(15))[6]) + 
  geom_point(aes(x = xseq, y = as.double(activas[252,])), col = palette(rainbow(15))[7]) + 
  geom_point(aes(x = xseq, y = as.double(activas[302,])), col = palette(rainbow(15))[8]) + 
  geom_point(aes(x = xseq, y = as.double(activas[352,])), col = palette(rainbow(15))[9]) + 
  geom_point(aes(x = xseq, y = as.double(activas[452,])), col = palette(rainbow(15))[10]) + 
  geom_point(aes(x = xseq, y = as.double(activas[402,])), col = palette(rainbow(15))[11]) + 
  geom_point(aes(x = xseq, y = as.double(activas[552,])), col = palette(rainbow(15))[12]) + 
  geom_point(aes(x = xseq, y = as.double(activas[602,])), col = palette(rainbow(15))[13]) + 
  geom_point(aes(x = xseq, y = as.double(activas[752,])), col = palette(rainbow(15))[14]) + 
  geom_point(aes(x = xseq, y = as.double(activas[702,])), col = palette(rainbow(15))[15]) + 
  ylab("Interets Rate %") + xlab("Time")

ggplot(data = activas) + geom_boxplot(aes(x = xseq[1], DEPO.1M), col = palette(rainbow(9))[1]) +
  geom_boxplot(aes(x = xseq[2], DEPO.3M), col = palette(rainbow(9))[2]) + 
  geom_boxplot(aes(x = xseq[3], DEPO.6M), col = palette(rainbow(9))[3]) + 
  geom_boxplot(aes(x = xseq[4], DEPO.12M), col = palette(rainbow(9))[4]) +
  geom_boxplot(aes(x = xseq[5], IRS.2Y), col = palette(rainbow(9))[5]) + 
  geom_boxplot(aes(x = xseq[6], IRS.3Y), col = palette(rainbow(9))[6]) +
  geom_boxplot(aes(x = xseq[7], IRS.4Y), col = palette(rainbow(9))[7]) +
  geom_boxplot(aes(x = xseq[8], IRS.5Y), col = palette(rainbow(9))[8]) + 
  geom_boxplot(aes(x = xseq[9], IRS.7Y), col = palette(rainbow(9))[9]) + 
  ylab("Interets Rate %") + xlab("Time")

corActivas <- cor(activas)
corrplot.mixed(corActivas, )
eigen(corActivas)

det(corActivas) == 0       ## matriz no singular, invertible


## La medida de la adecuación muestral de Kaiser-Meyer-Olkin contrasta si las correlaciones
## parciales entre las variables son pequeñas.

KMO(corActivas)

## La prueba de esfericidad de Bartlett contrasta la hipótesis nula de que la matriz de
## correlaciones es una matriz identidad, en cuyo caso no existirían correlaciones 
## significativas entre las variables y el modelo factorial no sería pertinente.
## La prueba de esfericidad de Bartlett contrasta si la matriz de correlaciones es una
## matriz de identidad, que indicaría que el modelo factorial es inadecuado.

cortest.bartlett(activas)

## 1. Tiene sentido llevar a cabo, en este caso, un análisis de componentes principales? Para 
## justificarlo, deberá llevar a cabo las pruebas que estime oportunas, como, por ejemplo el 
## análisis de la matriz de correlaciones, el del determinante de dicha matriz, la prueba de 
## esfericidad de Bartlett, el KMO o el MSA;

## Respuesta:
## Si tiene sentido llevar a cabo un análisis de componentes principales. El determinante de la 
## matriz de correlaciones es cercano pero distinto de cero debido a que las variables están muy
## correlacionadas. La prueba de la esfericidad de Bartlett nos devuelve un p-value de cero, por
## lo cual podemos concluir lo siguiente: la matriz de correlaciones es distinta a la matriz identidad,
## nuestras variables están correlacionadas y es tiene sentido realizar el análisis. Por último,
## con la prueba KMO obtuvimos un MSA de 0.87 en general y MSA's individuales entre 0.79 y 0.93.
## La regla establece que para valores de MSA entre cero y uno, el análisis es adecuado.

## Pendiente Exlicar la formula del KMO y la logica.


## 2. ¿Cuántos componentes permitirían explicar, adecuadamente, la estructura subycente de los
## tipos de interés aquí analizados? 
##
## Justifique su respuesta empleando, por ejemplo, las pruebas de la varianza explicada o del
## gráfico de sedimentación;


acpActivas <- PCA(activas) 
fviz_eig(acpActivas, addlabels = TRUE, hjust = -0.3) +
  labs(title = "Scree plot / Gráfico de sedimentación",
       x = "Dimensiones", y = "% Varianza explicada") +
  theme_minimal() + geom_hline(yintercept = 100/9, linetype = "dashed", color = "red")

acpActivas$var$cos2

## La primera comoponente explica el 81.29% de la varianza total y la segunda componente explica 
## otro 17.58%; es decir, con las primeras dos componentes explicamos el 98.87% de la varianza.
## Al observar el grafico de sedimentación (scree plot), con un threshold de 11.11% (1/9; tenemos
## 9 variables), solo es superado por las primeras dos componentes. Las subsecuentes componentes
## solo logran explicar muy poco de la variabilidad restante. Por otra parte, se denomina "comunalidad" 
## a la proporción de la varianza explicada por los factores comunes en una variable. Al observar 
## los cos2 (comunalidad) resultantes del análisis de componentes principales, se logra apreciar 
## que la primera dimensión se explica gran parte de las tasas de interés de largo plazo y la 
## segunda explica más de las tasas de corto plazo.

fviz_pca_biplot(acpActivas, addEllipses = TRUE, ellipse.level = .95)

## 3. Finalmente, ¿tiene sentido llevar a cabo una rotación de las variables subyacentes? Para
## responder, lleva a cabo una rotación Varimax, por ejemplo.

#####################################################################################################################################
#3. Finalmente, ¿tiene sentido llevar a cabo una rotación de las variables subyacentes? Para responder, lleva a cabo una rotación 
# Varimax, por ejemplo.
######################################################################################################################################
# https://www.youtube.com/watch?v=oZ2nfIPdvjY
# https://www.r-bloggers.com/naive-principal-component-analysis-using-r/
# https://methods.sagepub.com/reference/the-sage-encyclopedia-of-communication-research-methods/i5417.xml
# https://www.theanalysisfactor.com/rotations-factor-analysis/
# The first PCA, a heuristic one, worked essentially on the inter-correlations. The definitive PCA, in contrast, 
# will implement a prior shuffling known as ‘rotation’, to ensure that the result is robust enough (just like cards are
# shuffled).

# The go-to rotation method is the orthogonal, or ‘varimax’, a statistical technique used at one level of factor analysis as 
# an attempt to clarify the relationship among factors.

# Explained variance is captured better this way. The adjustment, or rotation, is intended to maximize the variance shared 
# among items. By maximizing the shared variance, results more discretely represent how data correlate with each principal
# component. To maximize the variance generally means to increase the squared correlation of items related to one factor, 
# while decreasing the correlation on any other factor. In other words, the varimax rotation simplifies the loadings 
# of items by removing the middle ground and more specifically identifying the factor upon which data load.

# Generally, the process involves adjusting the coordinates of data that result from a principal components 
# analysis.


###################
# Varimax method  #
###################
# Now with varimax rotation, Kaiser-normalized 
# by default:

acpVarimax <- psych::principal(cor(activas), nfactors = 2, 
                       rotate = "varimax", scores = TRUE)
acpVarimax
# In pc2 we see how each variable correlates with the rotated components. Which reveals how variables load on each component,
# or in other words, to which component a variable belongs. 

acpVarimax$loadings
acpActivas$var$coord

# After doing the rotation, comparing the loadings (the correlation between a variable and a PC) of Dim1 and Dim2 of 
# the inicial pca and the second pca2, we keep seeing that Dim1 and Dim2 keep explaining the same structure below the
# interest rates studied. So to the result of our PC1 and PC2 is robust enough even when we rotate the variables.


# We would want:
# - Less than half of residuals with absolute values > 0.05
# - Model fit > .9
# - All communalities > .7

# Healthcheck
abs(acpVarimax$residual) > .05
acpVarimax$fit > .9
acpVarimax$communality > .7


# After doing the rotation, comparing the loadings (the correlation between a variable and a PC) of Dim1 and Dim2 of 
# the inicial pca and the second pca2, we keep seeing that Dim1 and Dim2 keep explaining the same structure below the
# interest rates studied. So to the result of our PC1 and PC2 is robust enough even when we rotate the variables.




#####################################################################################################
## USAR:
##   OBSERVACIONES: 
##     949 primeras observaciones (denominadas observaciones ACTIVAS) 
##   VARIABLES:
##     9  primeras variables (las variables activas)
##   OBSERVACIONES:
##     950 a 978 (llamadas observaciones SUPLEMENTARIAS)
##   PREDICCIÓN:  predecir el valor del bono a 10 años (IRS.10Y, variable suplementaria)
suplementarias <- na.omit(datosInt[950:nrow(datosInt), -c(1, 2, 11)])

corSuplementarias <- cor(suplementarias)
det(corSuplementarias)


# split data into 2 parts for pca training (70%) and prediction (30%)
set.seed(131822)
muestra <- sample(nrow(suplementarias), nrow(suplementarias)*0.75)
suplementarias.train <- suplementarias[muestra,]
suplementarias.valid <- suplementarias[-muestra,]

#PCA 
prin_comp <- PCA(suplementarias.train, scale.unit = TRUE)

train.data <- data.frame(IRS_10 = data.train$`IRS 10Y`, prin_comp$x)
train.data
#First 10 PCAs
train.data <- train.data[ , 1:10]

#Train decision tree on IRS_10 again 10 components of train data
rpart.model <- rpart(IRS_10 ~ . , data = train.data, method = "anova")

#Transform test into PCA by using princomp from train to pca.test. newdata looks for which variable to predict
test.data <- predict(prin_comp, newdata = data.valid)

#predicited PCA with test one hot encoded dataset
test.data <- as.data.frame(test.data)

#First 30 components
test.data <- test.data[,1:10]

#Prediction on test data. Predicts the IRS10based on the components of the test data
rpart.prediction <- predict(rpart.model, test.data)
rpart.prediction

###########################################################################
training <- ACPTIUSD[1:949, ]
test     <- ACPTIUSD[950:nrow(datosInt), ]



modelo_pcr <- pcr(formula = `IRS.10Y` ~ . -`X` -`DEPO.1M`,
                  data = training,
                  ncomp = 2)
# Test-MSE
predicciones <- predict(modelo_pcr, newdata = test, ncomp = 2)
test_mse <- mean((predicciones - test$`IRS 10Y`)^2)
test_mse


