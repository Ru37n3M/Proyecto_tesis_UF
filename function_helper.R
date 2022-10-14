#####


mixingfun <- function(dislist, beta, total_votos){
  #La funcion devuelve un tibble con los valores para las 3 dimensiones y la distribucion original de donde proviene cada valor
  
  #dislist: Tiene que ser una lista de 3 elementos nombrada como "n","means" y "cov_mat" con un vector de longitud K, una lista de longitud K con vectores de longitud 3 y una lista de longitud K con matrices 3 x 3. El primer elemento marca la cantidad de sujetos a extraer de cada distribucion, el segundo elemento la media de cada distribucion y el ultimo la matriz de covarianza de cada distribucion.  
  
  library(MASS)
  library(tidyverse)
  
  pmap(
    dislist,
    function(n, means, cov_mat) mvrnorm(n, means, cov_mat) #Genero los muestreos de la distribucion multivariada
  ) %>% 
    lapply(as.data.frame) %>%  #Convierte cada matriz de la lista en dataframe
    bind_rows() %>% #Une la lista de matrices en un solo dataframe
    rename(Dim1 = V1,Dim2 = V2,Dim3 = V3) %>% #Cambia los nombres de las 3 columnas a algo mas representativo
    mutate( #Creo una nueva variable para indicar de que distribucion proviene originalmente cada valor
      Dist = rep( 
        1:length(dislist$n), dislist$n #Pongo un numero para cada distribucion y lo repito segun cuantos datos hay provenientes de esa distribucion
      ),
      Beta = beta,
      Votos_positivos = rbinom(sum(dislist$n), c(1:total_votos), prob = beta)
    ) %>%
    tibble() #Convierto el resultado final en un tibble
  
}


dist <- mixingfun(list("n" = c(200,300,100), # n de cada distribucion
                       "means" = list(c(2,2,2),c(1,1,1),c(0,0,0)), # lista con vectores de medias para cada dist
                       "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                  rbeta(600,4,2), #Distribucion beta, n == sum(list$n)
                  2) #Size de distribucion binomial 


#####
shuffle_dist <- function(distr){
  
  #distr: Espera el resultado de mixingfun
  
  library(tidyverse)
  
  tbl_df_O <- distr %>%
    mutate(
      #Se agrega la columna id al tibble distr
      ID = rep( 
        1:nrow(distr)
      )) %>%
    sample_n(nrow(distr)) #Se samplea aleatoriamente por el numero de filas de distr para reordenarlo
  
  #Devuelve tibble reordenado y con columna ID
  tbl_df_O
}

#####

Opinion_pool <-function(dist, k, prop = 0.5, 
                        k_method = "random"){
  
  #shuffle_dist: espera los resultados de shuffle_dist
  #k: numero de ideas que cada participante va a ver (numero entero)
  #total_votos: numero entero, cantidad de votos disponibles para el participante
  #par_num_iteration: cantidad de participantes por iteracion [FALTA ACLARAR QUE PASA CON ESTE ARGUMENTO]
  #k_method: criterio de seleccion de k, puede ser "random" (default), "A" o "B"
  
  library(tidyverse)
  
  
  
  #nuevo dataframe con las dimensiones de shuffled_dist
  par_pool <- dist[,which(grepl("Dim", colnames(dist)) | grepl("Votos", colnames(dist)))]
  
  #Se obtiene la cantidad total de votos para computar cantidad de votos negativos
  total_votos <- max(dist$Votos_positivos)
  
  #dataframe vacio para incorporar resultados del repeat loop
  O_pool <- tibble(NULL)
  
  repeat{
    
    #se selecciona primera fila 
    par_n <- par_pool[c(1:1),]
    
    #se selecciona solo los valores de la opinion del participante (Dim)
    par_dim_only <- par_n[, which(grepl("Dim", colnames(dist)))]
    
    #se arma un tibble con las dimensiones de par_n
    looping_tbl <- tibble(
      "ID" = sum(nrow(O_pool), 1),
      par_dim_only,
      "visualizaciones"=0, 
      "V_pos"=0,
      "V_neg" = 0,
      "ratio_votos_vis"= 0) 
    
    #incorpora la fila armada a O_pool
    O_pool <- bind_rows(O_pool,looping_tbl)
    
    #Se restan los votos positivos del total de votos para obtener la cantidad de votos negativos
    Votos_negativos <- abs(par_n$Votos_positivos - total_votos)
    
    #se guardan los resultados de la funcion Voting
    Voting_loop <- Voting(O_pool, par_dim_only, k, par_n$Votos_positivos, Votos_negativos, prop, k_method)
    
    #se sobrescribe O_pool con los resultados de la votacion
    #se actualizan visualizaciones, votos positivos y negativos y el grado de conseso
    O_pool <- Voting_loop
    
    
    #se elimina la primera fila del pool de participantes
    par_pool <- par_pool[-c(1:1),]
    
    #el loop se rompe cuando no quedan mas filas en el pool de participantes
    if(nrow(par_pool) == 0)
      break
  }
  
  return(O_pool)
}

#####
#Esta funcion se encuentra dentro de Opinion_pool
#Genera votacion, incorpora resultados al repeat loop de Opinion_pool
#argumentos de voting que se setean en Opinion_pool: k, vneg, vpos, k_method, cons_method
#argumentos definidos dentro de Opinion_pool: O_pool, par (seteados a partir de shuffled_distr)


#####

Voting <- function(pool_ideas, par, k, vpos, vneg, prop = 0.5, k_method = "random"){
  
  #Chequea fila por fila si se encuentra la opinion del participante
  check <- function(x){
    x%in%par
  }
  
  #Si se encuentra la idea del participante, se la elimina del pool de seleccion
  O_pool <- pool_ideas[-which(sapply(pool_ideas, check))[1],] %>%
    as_tibble
  
  #Condicional para chequear si el pool de ideas tiene por lo menos k filas
  if(nrow(O_pool) >= k)
  {
    
    #Condicionales para elegir algoritmos de seleccion
    if(k_method == "A"){
      
      k_opinion <- algoritmo_seleccion_f1x(O_pool, k)
      
    } else if(k_method == "Ab"){
      
      k_opinion <- algoritmo_seleccion_f1xb(O_pool, k)
      
    } else if(k_method == "B"){
      
      k_opinion <- algoritmo_seleccion_f2x(O_pool, k, prop)
      
    } else if(k_method == "Bd"){
      
      k_opinion <- algoritmo_seleccion_f2xd(O_pool, k, prop)
      
    } 
    
    else{
      #Sampleo aleatorio
      k_opinion <- sample_n(O_pool, k, replace = F)
    }
    
    O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)] <- O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
    # en 
    
    #k vectores con las dimensiones de k_opinion
    k2_noid <- k_opinion[,which(grepl("Dim", colnames(O_pool)))]
    
    #Matriz de k filas con el resultado de la diferencia
    Dij <- apply(k2_noid,1, function(x) abs(par) - abs(x)) %>%
      bind_rows %>%
      abs
    
    #Inversa de la distancia, se elevan todos los valores a la -1
    sumDij_inverse <- sum(Dij**-1)
    
    #Probabilidad de cada vector de valores de ser elegido en base a la sumatoria de todas las distancias
    
    Poij_pos<- sapply(Dij, function(x) (x**-1) / sumDij_inverse)
    
    #Condicional para votos positivos, chequea si el participante tiene votos positivos
    if(vpos > 0){
      
      #se samlea vpos con las probabilidades de cada fila/vector
      Voted_pos <- sample_n(k_opinion, vpos, prob = c(Poij_pos), replace = T)
      
      #se suma 1 punto a la idea que se corresponde con el valor minimo
      O_pool$V_pos[which(O_pool$ID%in%Voted_pos$ID)] <- O_pool$V_pos[which(O_pool$ID%in%Voted_pos$ID)] + 1 
      
    }
    
    
    #Sumatoria de todas las distancias
    sumDij <- sum(Dij)
    
    #Probabilidad de voto negativo
    Poij_neg<- sapply(Dij, function(x) x / sumDij )
    
    #Condicional para votos negativos, chequea si el participante tiene votos negativos
    if(vneg > 0){
      
      #Se samplea vneg de k_opinion con probabilidades de voto negativo para cada vector
      Voted_neg <- sample_n(k_opinion,vneg, prob = c(Poij_neg), replace = T)
      
      #se suma 1 punto a la idea que se corresponde con el valor minimo
      O_pool$V_neg[which(O_pool$ID%in%Voted_neg$ID)] <- O_pool$V_neg[which(O_pool$ID%in%Voted_neg$ID)] + 1 
      
    }
    
    #Ratio Votos/visualizaciones
    O_pool$ratio_votos_vis[which(O_pool$ID%in%k_opinion$ID)] <- (O_pool$V_pos[which(O_pool$ID%in%k_opinion$ID)]-
                                                                   O_pool$V_neg[which(O_pool$ID%in%k_opinion$ID)])/O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)]
    
    return(O_pool)
    
  }
  
  else{
    return(pool_ideas)
  }
  
}

#####
#Algoritmo de seleccion f1x
#se samplean k ideas por > cantidad de visualizaciones
algoritmo_seleccion_f1x <- function(O_pool, k){
  
  #Se reordena aleatoriamente el dataframe
  #Se hace para evitar que, en caso de empate de visualizaciones,
  #todos los valores minimos tengan iguales probabilidades de ser seleccionados
  #al subsetear las últimas k filas del dataset
  O_pool <- sample_n(O_pool, nrow(O_pool))
  
  #Se obtienen k valores de los ultimos puestos del dataframe
  k_opinion<- O_pool[order(O_pool$visualizaciones, decreasing = T),] %>%
    slice_tail(n = k)
  
  return(k_opinion)
  
}

#se samplean k ideas por > ratio
algoritmo_seleccion_f1xb <- function(O_pool, k){
  
  #Se reordena aleatoriamente el dataframe
  #Se hace para evitar que, en caso de empate de visualizaciones,
  #todos los valores minimos tengan iguales probabilidades de ser seleccionados
  #al subsetear las últimas k filas del dataset
  O_pool <- sample_n(O_pool, nrow(O_pool))
  
  #Se obtienen k valores de los ultimos puestos del dataframe
  k_opinion<- O_pool[order(O_pool$ratio_votos_vis, decreasing = T),] %>%
    slice_tail(n = k)
  
  return(k_opinion)
  
}

#####
#Algoritmo de seleccion f2x
#se samplean la proporcion prop de k ideas segun f1x y k-(prop*k) ideas segun < ratio votos-visualizaciones
algoritmo_seleccion_f2x <- function(O_pool, k, prop){
  
  g1_prop <- round(prop*k)
  g2_prop <- round(k - (prop * k))
  
  #Se reordena aleatoriamente el dataframe
  #Se hace para evitar que, en caso de empate de visualizaciones,
  #todos los valores minimos tengan iguales probabilidades de ser seleccionados
  #al subsetear las últimas k filas del dataset
  O_pool <- sample_n(O_pool, nrow(O_pool))
  
  #Se subsetean las últimas k/2 filas del dataset ordenado de forma decreciente segun visualizaciones
  k_opinion_low <- O_pool[order(O_pool$visualizaciones, decreasing = T),] %>%
    slice_tail(n = g1_prop)
  
  #Se subsetean las primeras k/2 filas del dataset ordenado de forma decreciente segun ratio votos-visualizaciones
  k_opinion_high <- O_pool[order(O_pool$ratio_votos_vis, decreasing = T),] %>%
    slice_head(n = g2_prop)
  
  #Se combinan las filas de ambos subsets para formar k opinones que seran presentadas al participante
  k_opinion <- bind_rows(k_opinion_low, k_opinion_high)
  
  return(k_opinion)
  
}

#se samplean la proporcion prop de k ideas segun > ratio-votos-visualizaciones y k-(prop*k) ideas segun < ratio votos-visualizaciones
algoritmo_seleccion_f2xd <- function(O_pool, k, prop){
  
  g1_prop <- round(prop*k)
  g2_prop <- round(k - (prop * k))
  
  #Se reordena aleatoriamente el dataframe
  #Se hace para evitar que, en caso de empate de visualizaciones,
  #todos los valores minimos tengan iguales probabilidades de ser seleccionados
  #al subsetear las últimas k filas del dataset
  O_pool <- sample_n(O_pool, nrow(O_pool))
  
  #Se subsetean las últimas k/2 filas del dataset ordenado de forma decreciente segun visualizaciones
  k_opinion_low<- O_pool[order(O_pool$ratio_votos_vis, decreasing = T),] %>%
    slice_tail(n = g1_prop)
  
  #Se subsetean las primeras k/2 filas del dataset ordenado de forma decreciente segun ratio votos-visualizaciones
  k_opinion_high <- O_pool[order(O_pool$ratio_votos_vis, decreasing = T),] %>%
    slice_head(n = g2_prop)
  
  #Se combinan las filas de ambos subsets para formar k opinones que seran presentadas al participante
  k_opinion <- bind_rows(k_opinion_low, k_opinion_high)
  
  return(k_opinion)
  
}


#funcion general de la simulacion, incorpora mixingfun y Opinion_pool en una funcion
#Argumentos
#list: lista con vector de n de c/distribucion, lista de vectores de medias de c/distribucion y lista de matrices de covarianza
#beta: distribucion beta para probabilidad de distribucion binomial dentro de mixingfun
#votos_totales: cantidad de votos total para cada participante
simulacion_plataforma <- function(list, beta, votos_totales, k, prop, k_method = "random"){
  dist <- mixingfun(list, beta, votos_totales) #mixingfun
  resultado_simulacion <- Opinion_pool(dist, k, k_method = "random") #Opinion_pool
  return(resultado_simulacion) #devuelve resultado Opinion_pool
}

simulacion_plataforma(list("n" = c(200,300,100), # n de cada distribucion
                           "means" = list(c(2,2,2),c(1,1,1),c(0,0,0)), # lista con vectores de medias para cada dist
                           "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                      rbeta(600,4,2), #Distribucion beta, n == sum(list$n)
                      2,#Size de distribucion binomial 
                      6,#numero de k
                      prop = 0.5, 
                      k_method = "O_B") #algoritmo de seleccion