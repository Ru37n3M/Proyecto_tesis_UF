
#ACA ESTOY TRATANDO DE CONSTRUIR UN CODIGO QUE ME PERMITA PROCESAR LAS VISUALIZACIONES
#Y LOS VOTOS DE MAS DE UN PARTICIPANTE EN SIMULTANEO

#LO QUE FALTA:
#RESTAR EL VALOR DE UN K DISTINTO A CADA PARTICIPANTE
#CONTAR LAS VISUALIZACIONES DE CADA K 
#CONTAR LOS VOTOS DE CADA PARTICIPANTE EN SIMULTANEO

par <- sample(participantes, 5)
par - k

dist_mod <- function(){
  for (value in par){
    result <- (value - k)
    par_iteration[[length(par_iteration)+1]] <- which.min(abs(c(result)))
  }
  par_iteration
}

dist_mod()
list_dist<- dist_mod()
list_dist

a <- function(X){
  return(which.min(abs(X)))
}


c <- lapply(list_dist, a)
c
lapply(list_dist, `[[`, 1)

b <- unlist(lapply(list_dist, a))
list_dist[[1]][5]

list_dist[[c(1,4)]][c(2,1)]

sapply(list_dist, function(x) which.min(abs(x)))
Filter(function(x) which.min(abs(x)), list_dist) 

help("select.list")

library("dplyr")

which.min(c(10:19))

#PROBLEMA: FALLA ORDEN DE ENTRADA QUE NO SEA SECUENCIAL