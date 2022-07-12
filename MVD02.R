#####
#mvfracsampling
#Descripcion
#samplea dataframes que son elementos de una lista con probabilidades ajustadas a la proporcion que las filas de cada
#df ocupan con respecto al numero de filas totales
#Argumento
#distlist ---- lista de x cantidad de dataframes 
#####


fracsampling <- function(distlist){
  
  #libreria para correr la funcion
  library(dplyr)
  
  #se toman la cantidad de filas de los df, que son los elementos de la lista
  x <- lapply(seq_along(distlist), function(x) nrow(distlist[[x]]))
  
  #el resultado se coerce a vector 
  #vamos a tener un vector con x elementos que van a ser la cantidad de filas de cada df dentro de 
  #la lista pasada como argumento
  x <- unlist(x)
  
  #se suman todas las filas
  n <- sum(x)
  
  #se calcula la proporcion de filas de todos los df en relacion al total n
  #con lapply utilizamos sample_frac de dplyr
  #la funcion va a samplear cada elemento del dataframe de acuerdo a la proporcion que ocupa en el total
  #ej. si tenemos 3 df cuyos n son 200, 200 y 600 respectivamente, se multiplica n * prop: 
  #((200 * 0.2 ) * 2) + 600 * 0.6 = 440
  props <-x/n
  res <- lapply(seq_along(props), function(x) sample_frac(distlist[[x]], props [x])) 
  
  #coercion a dataframe
  res <- do.call("rbind", res)
  
  return(res)
  
}

library(MASS)
resmv<- fracsampling(list("1" = data.frame(mvrnorm(n = 200, c(2, 2, 2),diag(1,3,3))),
                          "2" = data.frame(mvrnorm(n = 200, c(-2, -2, -2),diag(1,3,3))),
                          "3" = data.frame(mvrnorm(n = 600, c(0, 0, 0),diag(1,3,3)))))

nrow(resmv)

library(ggplot2)
ggplot(resmv, aes(x=X2, y=X3))+
  geom_point(alpha = .2) +
  geom_density_2d()+
  theme_bw()

library(MVN)
mvn(resmv, mvnTest = "energy", univariatePlot = "scatter")


     