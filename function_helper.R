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


mixingfun(list("n" = c(200,300,100), # n de cada distribucion
               "means" = list(c(2,2,2),c(1,1,1),c(0,0,0)), # lista con vectores de medias para cada dist
               "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3)))) # lista con matrices de covarianza

######

#Poner el resultado de mixingfun en distr
shuffle_dist <- function(distr){
  
  library(tidyverse)
  
  tbl_df_O <- distr %>%
    mutate( 
      ID = rep( 
        1:nrow(distr)
      )) %>%
    sample_n(nrow(distr)) 
  
  tbl_df_O
}

#Poner el resultado de shuffle_dist en shuffled_dist
#poner el numero de ideas que cada participante va a ver (k)
#poner la cantidad de votos negativos (nv)
#poner la cantidad de votos positivos (pv)
#k_method criterio de seleccion de k, puesto en random por default: criterios posibles "A" o "B"
#con_method "B" como criterio alternativo posible
#el resultado es ordenado en funcion de grado_consenso descendiente
Opinion_pool <-function(shuffled_distr, k, nv, pv, k_method = "random", con_method = "A"){
  
  library(tidyverse)
  
  #nuevo dataframe con las dimensiones de shuffled_dist
  par_pool <- shuffled_distr[, c(1:sum(grepl("Dim", colnames(shuffled_dist))))]
  
  #dataframe vacio para incorporar resultados del repeat loop
  O_pool <- tibble(NULL)
  
  repeat{
    
    #se selecciona primera fila 
    par_n <- par_pool[c(1:1),]
    
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
    Voting_loop <- Voting(O_pool, par_n, k, nv, pv, k_method, con_method )
    
    #se sobrescribe O_pool con los resultados de la votacion
    #se actualizan visualizaciones, votos positivos y negativos y el grado de conseso
    O_pool <- Voting_loop
    
    #se ordena O_pool en orden decreciente por grado de consenso
    rearranged_df_cons<-order(O_pool$grado_consenso, na.last = F, decreasing = T) 
    
    O_pool <- O_pool[rearranged_df_cons,]
    
    #se elimina la primera fila del pool de participantes
    par_pool <- par_pool[-c(1:1),]
    
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
Voting <- function(O_pool, par, k, vneg, vpos, k_method = "random", cons_method = "A" ){
  
  #Si el metodo de seleccion de ideas es "A", se samplean las ultimas filas de O_pool para que todas las ideas
  #sean visualizadas el mismo numero de veces
  #Si "B" se samplean las primeras filas de O_pool, es decir, las que mayor consenso generaron,
  #es decir, que fueron las que fueron más vistas y votadas
  #Si "random" o cualquier otra cosa que no sea ni "A" ni "B" se samplean filas de O_pool de manera aleatoria
  if(k_method == "A"){
    k_opinion <- sample_n(tail(O_pool, k), ifelse(nrow(O_pool) >= k, yes = k, no = nrow(O_pool)), replace = F)
  } else if(k_method == "B"){
    k_opinion <- sample_n(head(O_pool, k), ifelse(nrow(O_pool) >= k, yes = k, no = nrow(O_pool)), replace = F)}
  else{
    k_opinion <- sample_n(O_pool, ifelse(nrow(O_pool) >= k, yes = k, no = nrow(O_pool)), replace = F)
  }
  
  #k vectores con las dimensiones de k_opinion
  k2_noid <- k_opinion[, c(2:sum(grepl("Dim", colnames(O_pool)),1))]
  
  distance <- function(x){
    abs(par) - abs(x)
  }
  
  kpar_distdf <- apply(k2_noid,1,distance)
  
  #deshacemos la lista y coercemos como matriz
  kpar_distdf <- matrix(unlist(kpar_distdf), nrow = nrow(k_opinion), byrow = T)
  
  #coercemos la matriz en dataframe
  kpar_distdf <- as.data.frame(kpar_distdf)
  
  #se agregan las medias por cada fila
  kpar_distdf$means <- abs(rowMeans(kpar_distdf))
  
  #se agrega el id
  kpar_distdf$ID <- k_opinion$ID
  
  #se reordenan las filas 
  rearranged <- order(kpar_distdf$means)
  
  kpar_distdf_top <- kpar_distdf[rearranged,]
  
  #se reordenan en orden inverso
  rearranged_inv <- order(kpar_distdf$means, decreasing = T)
  
  kpar_distdf_bottom <- kpar_distdf[rearranged_inv,]
  
  O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)] <- O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
  # en k
  
  #se utiliza el df reordenado para filtrar las filas a las que se les va asignar voto positivo
  kpar_distdf_top<- kpar_distdf_top[c(1:vpos),]
  
  #se utiliza el df inverso para filtrar las filas a las que se les va asignar voto negativo
  kpar_distdf_bottom<- kpar_distdf_bottom[c(1:vneg),]
  
  O_pool$V_pos[which(O_pool$ID%in%kpar_distdf_top$ID)] <- O_pool$V_pos[which(O_pool$ID%in%kpar_distdf_top$ID)] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
  
  O_pool$V_neg[which(O_pool$ID%in%kpar_distdf_bottom$ID)] <- O_pool$V_neg[which(O_pool$ID%in%kpar_distdf_bottom$ID)] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
  
  #eleccion de criterio de consenso "B" o "A"
  if(cons_method == "B"){
    O_pool$grado_consenso[which(O_pool$ID%in%k_opinion$ID)] <- (O_pool$V_pos[which(O_pool$ID%in%k_opinion$ID)]-
                                                                  O_pool$V_neg[which(O_pool$ID%in%k_opinion$ID)])/O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)]
  }else{
    O_pool$grado_consenso[which(O_pool$ID%in%k_opinion$ID)] <- O_pool$V_pos[which(O_pool$ID%in%k_opinion$ID)]/O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)]
  }
  
  return(O_pool)
}

#Incorpora shuffle_dist() Y Opinion_pool() en una sola funcion
#poner el resultado de mixingfun en mixeddistr
#setear argumentos de Voting(): k, vneg, vpos
#el resultado es ordenado en funcion de grado_consenso descendiente
OV_loop <- function(mixeddistr, k, vneg, vpos, k_method = "random", con_method = "A"){
  
  shuffled_dist <- shuffle_dist(mixeddistr)
  
  OV_res <- Opinion_pool(shuffled_dist, k, vneg, vpos, k_method, con_method )
  
  return(OV_res)
  
}
