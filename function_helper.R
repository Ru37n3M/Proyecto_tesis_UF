#####


mixingfun <- function(dislist){
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
      )
    ) %>%
    tibble() #Convierto el resultado final en un tibble
  
}


dist <- mixingfun(list("n" = c(200,300,100), # n de cada distribucion
               "means" = list(c(2,2,2),c(1,1,1),c(0,0,0)), # lista con vectores de medias para cada dist
               "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3)))) # lista con matrices de covarianza

######


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


Opinion_pool <-function(shuffled_distr, k, par_num_iteration,
                        nv, pv, ego = 0.01, k_method = "random"){
  
  #shuffle_dist: espera los resultados de shuffle_dist
  #k: numero de ideas que cada participante va a ver (numero entero)
  #par_num_iteration: cantidad de participantes por iteracion [FALTA ACLARAR QUE PASA CON ESTE ARGUMENTO]
  #nv: la cantidad de votos negativos (numero entero)
  #pv: la cantidad de votos positivos (numero entero)
  #ego: valor > 0 o <= 1, definido como la probabilidad que tiene el participante de votar su propia idea (Dij == 0), valores mas cercanos a 0 implican mayor prob. de votar idea propia
  #k_method: criterio de seleccion de k, puede ser "random" (default), "A" o "B"
  
  
  library(tidyverse)
  
  #nuevo dataframe con las dimensiones de shuffled_dist
  par_pool <- shuffled_distr[, c(1:sum(grepl("Dim", colnames(shuffled_distr))))]
  
  #dataframe vacio para incorporar resultados del repeat loop
  O_pool <- tibble(NULL)
  
  repeat{
    
    #se selecciona primera fila 
    par_n <- par_pool[c(1:par_num_iteration),]
    
    #se arma un tibble con las dimensiones de par_n
    looping_tbl <- tibble(
      "ID" = sum(nrow(O_pool), 1), 
      par_n,
      "visualizaciones"=0, 
      "V_pos"=0,
      "V_neg" = 0,
      "grado_consenso"= 0) 
    
    #incorpora la fila armada a O_pool
    O_pool <- rbind(O_pool,looping_tbl)
    
    #se guardan los resultados de la funcion Voting
    Voting_loop <- Voting(O_pool, par_n, k, nv, pv, ego, k_method)
    
    #se sobrescribe O_pool con los resultados de la votacion
    #se actualizan visualizaciones, votos positivos y negativos y el grado de conseso
    O_pool <- Voting_loop
    
    
    #se elimina la primera fila del pool de participantes
    par_pool <- par_pool[-c(1:par_num_iteration),]
    
    #el loop se rompe cuando no quedan mas filas en el pool de participantes
    if(nrow(par_pool) == 0)
      break
  }
  
  return(O_pool)
}

#Esta funcion se encuentra dentro de Opinion_pool
#Genera votacion, incorpora resultados al repeat loop de Opinion_pool
#argumentos de voting que se setean en Opinion_pool: k, vneg, vpos, k_method, cons_method
#argumentos definidos dentro de Opinion_pool: O_pool, par (seteados a partir de shuffled_distr)

Voting <- function(O_pool, par, k, vneg, vpos, ego = 0.01, k_method = "random"){
  
  
  #Si el metodo de seleccion de ideas es "A", se samplean las ultimas filas de O_pool para que todas las ideas
  #sean visualizadas el mismo numero de veces
  #Si "B" se samplean las primeras filas de O_pool, es decir, las que mayor consenso generaron,
  #es decir, que fueron las que fueron más vistas y votadas
  #Si "random" o cualquier otra cosa que no sea ni "A" ni "B" se samplean filas de O_pool de manera aleatoria
  
  if(k_method == "A"){
    
    #Se reordena el tibble de mayor a menor visualizaciones
    rearranged_df_vis<-order(O_pool$visualizaciones, na.last = F, decreasing = T) 
    
    O_pool_vis <- O_pool[rearranged_df_vis,]
    
    #Se samplea k entre las últimas filas del dataframe
    k_opinion <- sample_n(tail(O_pool_vis, k), ifelse(nrow(O_pool_vis) >= k, yes = k, no = nrow(O_pool_vis)), replace = F)
  } else if(k_method == "B"){
    
    #Se reordena el tibble de mayor a menor visualizaciones
    rearranged_df_vis<-order(O_pool$visualizaciones, na.last = F, decreasing = T) 
    
    O_pool_vis <- O_pool[rearranged_df_vis,]
    
    #Se samplea k/2 entre las últimas filas del dataframe
    sample_bottomvis <- sample_n(tail(O_pool_vis, k/2), ifelse(nrow(O_pool_vis) >= k, yes = k, no = nrow(O_pool_vis))/2, replace = F)
    
    #Se reordena el tibble de mayor a menor ratio votos/visualizaciones
    rearranged_df_ratiovm<-order(O_pool$grado_consenso, na.last = F, decreasing = T) 
    
    O_pool_cons <- O_pool[rearranged_df_ratiovm,]
    
    #Se samplea k/2 entre las últimas filas del dataframe
    sample_topcons <- sample_n(head(O_pool_cons, k), ifelse(nrow(O_pool) >= k/2, yes = k/2, no = nrow(O_pool))/2, replace = F)
    
    #Se unen los resultados de ambos samplings
    k_opinion <- rbind(sample_bottomvis, sample_topcons)
    
  }
  
  else{
    #Sampleo aleatorio
    k_opinion <- sample_n(O_pool, ifelse(nrow(O_pool) >= k, yes = k, no = nrow(O_pool)), replace = F)
  }
  
  O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)] <- O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
  # en 
  
  #k vectores con las dimensiones de k_opinion
  k2_noid <- k_opinion[, c(2:sum(grepl("Dim", colnames(O_pool)),1))]
  
  #Distancia entre participante y k
  distance_i_j <- function(x){
    abs(par) - abs(x)
  }
  
  #Matriz de k filas con el resultado de la diferencia
  Dij <- apply(k2_noid,1, distance_i_j)
  
  Dij <- bind_rows(Dij)
  
  #Se pasan a valores absolutos todos los numeros
  Dij <- abs(Dij)
  
  #Comprobamos que no haya 0 como valor de algún vector, en caso de ser 0, se reemplaza por un valor distinto a 0 para
  #evitar division por 0. Vectores de valor 0 ocurren en las primeras iteraciones del loop, donde 
  #es mas probable que el participante vea su opinion en las k ideas seleccionadas
  Dij[Dij == 0] <- ego
  
  #Inversa de la distancia, se elevan todos los valores a la -1
  sumDij_inverse <- sum(Dij**-1)
  
  #Probabilidad de cada vector de valores de ser elegido en base a la sumatoria de todas las distancias
  Probs_vpos <- function(x){
    (x**-1) / sumDij_inverse
  }
  
  Poij_pos<- sapply(Dij, Probs_vpos)
  
  #Poij_pos<- rowSums(Poij_pos)
  
  #se samlea vpos con las probabilidades de cada fila/vector
  Voted_pos <- sample_n(k_opinion, vpos, prob = c(Poij_pos), replace = T)
  
  #se suma 1 punto a la idea que se corresponde con el valor minimo
  O_pool$V_pos[which(O_pool$ID%in%Voted_pos$ID)] <- O_pool$V_pos[which(O_pool$ID%in%Voted_pos$ID)] + 1 
  
  #Sumatoria de todas las distancias
  sumDij <- sum(Dij)
  
  #Probabilidad de voto negativo
  Probs_vneg <- function(x){
    x / sumDij
  }
  
  Poij_neg<- sapply(Dij, Probs_vneg)
  
  #Se samplea vneg de k_opinion con probabilidades de voto negativo para cada vector
  Voted_neg <- sample_n(k_opinion,vneg, prob = c(Poij_neg), replace = T)
  
  #se suma 1 punto a la idea que se corresponde con el valor minimo
  O_pool$V_neg[which(O_pool$ID%in%Voted_neg$ID)] <- O_pool$V_neg[which(O_pool$ID%in%Voted_neg$ID)] + 1 
  
  #Ratio Votos/visualizaciones
  O_pool$grado_consenso[which(O_pool$ID%in%k_opinion$ID)] <- (O_pool$V_pos[which(O_pool$ID%in%k_opinion$ID)]-
                                                                O_pool$V_neg[which(O_pool$ID%in%k_opinion$ID)])/O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)]
  
  return(O_pool)
}

#Incorpora shuffle_dist() Y Opinion_pool() en una sola funcion
#poner el resultado de mixingfun en mixeddistr
#setear argumentos de Voting(): k, vneg, vpos
#el resultado es ordenado en funcion de grado_consenso descendiente
OV_loop <- function(mixeddistr, k, par_num_iteration, vneg, vpos,
                    ego=0.01, k_method = "random"){
  
  shuffled_dist <- shuffle_dist(mixeddistr)
  
  OV_res <- Opinion_pool(shuffled_dist, k, par_num_iteration, vneg, vpos, ego, k_method)
  
  return(OV_res)
  
}