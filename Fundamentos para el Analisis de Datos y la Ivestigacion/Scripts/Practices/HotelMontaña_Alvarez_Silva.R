########################################################################################################
## Start Date: 18/10/2019
## End Date: 20/10/2019
## Author: José María Álvarez Silva
## School: CUNEF
## Class: Fundamentos
## Language: Spanish
##
########################################################################################################
## Fundamentos
########################################################################################################
## Procesos de nacimiento y muerte
##
########################################################################################################
## Caso: Hotel Montaña
##

## Encunciado del Problema #############################################################################

## El parking exterior de un hotel de montaña está limitado a cinco plazas.
## Los conductores que lo usan llegan siguiendo una distribución de
## Poisson con frecuencia de 6 por hora. El tiempo de estacionamiento
## tiene distribución exponencial con 30 minutos de promedio. Los
## conductores que no pueden encontrar un hueco vacío inmediatamente
## cuando llegan pueden esperar dentro del estacionamiento hasta que
## salga un automóvil, pero sólo pueden permanecer en espera 3
## vehículos. Los vehículos que no pueden aparcar ni tampoco quedan
## huecos provisionales se deben ir. Determinar:

## a) La probabilidad de que haya “n" autos en el sistema.
## b) La frecuencia efectiva de llegada para autos que usen el parking.
## c) La cantidad media de autos en el parking.
## d) El tiempo medio que espera un auto para estacionar, estando en una plaza provisional
## e) La cantidad promedio de plazas de estacionamiento ocupadas.

## Propuesta de solución ###############################################################################

## La llegada de autos es poisson con lambda 6 (por hora)
##   - la llameremos lambdaP
lambdaP <- 6
## El tiempo en el estacionamiento es exponencial con lambda 2 (por hora) <- 1/lambda = .5 horas
##   - la llamareos lambdaE
lambdaE <- 2

## El maximo de coches en el sistema es 8 (5 estacionados y tres esperando)
##  - Espacio de Estados: S = {0,1,...,8}

## Es un proceso de nacimiento y muerte con tasas constantes ya que las tasas no dependen del número de 
##   individuos en el sistema. Se aplica en fenómenos con evolución dinámica de poblaciones 
##   y en líneas de espera.
## - los individuos entran al sistema con tasa lambdaP y salen del sistema con tasa lambdaE

## Ecuaciones de Equilibrio: (Entradas = salidas)
##
##   lambdaP  lambdaP  lambdaP          lambdaP  lambdaP  lambdaP  
##       ->      ->       ->               ->      ->       ->
##   (0)   (1)      (2)      (3) ...   (5)   (6)      (7)      (8)
##      <-       <-       <-              <-       <-       <-
##   lambdaE  lambdaE  lambdaE          lambdaE  lambdaE  lambdaE 


## - 0) P1(lambdaE) = P0(lambdaP)
## - 1) P0(lambdaP) + P2(lambdaE) = P1(lambdaP + lambdaE)
## - 2) P1(lambdaP) + P3(lambdaE) = P2(lambdaP + lambdaE)
## - 3) P2(lambdaP) + P4(lambdaE) = P3(lambdaP + lambdaE)
## ...
## - 7) P6(lambdaP) + P8(lambdaE) = P7(lambdaP + lambdaE)
## - 8) P7(lambdaP) = P8(lambdaE)
## - G) sum(P0, P1, ..., P8) = 1

## Despejando:
##
##   P1 = P0(lamdaP/lambdaE)
##   P2 = (P1(lambdaP + lambdaE) - P0(lambdaP)) / (lambdaE)
##      = (P0(lamdaP/lambdaE) x (lambdaP + lambdaE) - P0(lambdaP)) / (lambdaE)
##      = P0(lamdaP/lambdaE)^2
##   ...
##   
##  En General:
##  Pn = P0(lamdaP/lambdaE)^n con n elemento de S

##  Por lo tanto:
##  - P0 = 1/suma((lamdaP/lambdaE)^n, desde 0 hasta 8)

## P0
VS <- 0
for (i in 0:8) {
  VS <- VS + (lambdaP / lambdaE)^i
}
P0 <- 1/VS

## Respuesta de inciso a:  ####
pn <- function(n, p0){
  return(P0*(lambdaP / lambdaE)^n)
}
## P0
pn(0)
## P1
pn(1)
## P2
pn(2)
## P3
pn(3)
## P4
pn(4)
## P5
pn(5)
## P6
pn(6)
## P7
pn(7)
## P8
pn(8)

## Probabilidad total
PT <- 0
for (i in 0:8) {
  PT <- PT + pn(i)
}
PT

## Respuesta inciso b:   ####
##  - frecuancia efectiva de llegada al parking:
##         - Que cuando un coche nuevo llegue al parking pueda acceder (aparcar/esperar)
##         - El complemento de cuando llegue un nuevo coche y no pueda aparcar o esperar
##         - Respuesta: 1 - P8
1 - pn(8)

## Respuesta inciso C:   ####
##  - E[x] = sum{ n x Pn ; n tomando valores desde 0 hasta 8}

valorEsperadoCoches <- 0
for (i in 0:8) {
  valorEsperadoCoches <- valorEsperadoCoches + i*pn(i)
}
valorEsperadoCoches

## Respuesta inciso d:   ####
##  - los coches dejan el sistema con una tasa de 30 minutos por coche.
##  - si hay un coche esperando el parking, esperara 30 minutos (promedio) a que se desocupe alguno.
##  - si hay dos, cada uno espera media hora a que se desocupe su lugar correspondiente.
##  - si hay tres, cada uno espera media hora a que se desocupe su lugar correspondiente.
##  - pn puede ser interpretado como la proporción de tiempo que un estado ocurre.
##  Por lo tanto, el tiempo medio que espera un auto para estacionar, estando en una plaza provisional
##  es la suma del producto entre número de coches esperando, el tiempo de espera por coche y 
##  la proporción del tiempo en ese estado (pn). Obteniendo así solo unidades de tiempo!!!

## Los estados donde hay almenos un coche esparando son: 6, 7 y 8.
## Ya que el numero de plazas de estacionamiento es 5.
valorEsperadoTiempoEspera <- 0
for (i in 6:8) {
  valorEsperadoTiempoEspera <- valorEsperadoTiempoEspera + (i - 5)*pn(i)*.5
}
valorEsperadoTiempoEspera

## 1.25 horas

## Respuesta inciso e:   ####
##  - La cantidad promedio de plazas de estacionamiento ocupadas es 5. 
##  Razonamiento:
##   El promedio de coches en el sistema es 7.5 (para ejemplificar digamos 7), supongamos que hay 
##   dos coches esperando a que se desocupen los lugares. En promedio en una hora se desocuparan dos 
##   lugares y seran ocpuados. Pero, en esa hora han llegado 6 autos nuevos al sistema que lo llenan,
##   dejando las plazas ocupadas al 100%. Por otro lado, si el sistema empiza vacio en una hora se 
##   llena el estacionamiento y epera el ultimo vehiculo en llegar (5 lugares y uno espera) y a 
##   apartir de ese momento no se vuelve a permanecer desocupado ningun espacio.

pn(6) + pn(7) + pn(8)
## Como la tasa de entrada de autos al sistema (6) es mayor a la tasa de salida (6), el sistema 
## tenderá a permanecer cercano a lleno la gran parte del tiempo (66.66% del tiempo p8). Y con mas de
## 5 coches (todas las plazas de estacionamiento están ocupadas) el 96.3% del tiempo (p6 + p7 + p8)




