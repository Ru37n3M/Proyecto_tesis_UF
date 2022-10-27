#####


mixingfun <- function(dislist, beta, total_votos){
  #La funcion devuelve un tibble con los valores para las 3 dimensiones y la distribucion original de donde proviene cada valor
  
  #dislist: Tiene que ser una lista de elementos nombrada como "n","means" y "cov_mat" con un vector de longitud K, una lista de longitud K con vectores de longitud 3 y una lista de longitud K con matrices 3 x 3. El primer elemento marca la cantidad de sujetos a extraer de cada distribucion, el segundo elemento la media de cada distribucion y el ultimo la matriz de covarianza de cada distribucion.  
  
  library(MASS)
  library(tidyverse)
  mix_df <- pmap(
    dislist,
    function(n, means, cov_mat) mvrnorm(n, means, cov_mat) #Genero los muestreos de la distribucion multivariada
  ) %>% 
    lapply(as.data.frame) %>%  #Convierte cada matriz de la lista en dataframe
    bind_rows() #Une la lista de matrices en un solo dataframe
 
  colnames(mix_df) <- paste('Dim', 1:ncol(mix_df), sep = '') #Cambia los nombres de las columnas a algo mas representativo
  
  mix_df %>% mutate( #Creo una nueva variable para indicar de que distribucion proviene originalmente cada valor
      Dist = rep( 
        1:length(dislist$n), dislist$n #Pongo un numero para cada distribucion y lo repito segun cuantos datos hay provenientes de esa distribucion
      ),
      Beta = beta,
      Votos_positivos = rbinom(sum(dislist$n), c(1:total_votos), prob = beta)
    ) %>%
    tibble() #Convierto el resultado final en un tibble
  
}


#####
shuffle_dist <- function(distr){
  #La funcion devuelve el mismo dataframe que como input pero con una columna de ID para cada fila y reordenado aleatoriamente
  #distr: Espera el resultado de mixingfun
  
  library(tidyverse)
  
  tbl_df_O <- distr %>%
    mutate(
      #Se agrega la columna id al tibble distr, el identificador de cada sujeto.
      ID = rep( 
        1:nrow(distr)
      )) %>%
    sample_n(nrow(distr)) #Se samplea aleatoriamente por el numero de filas de distr para reordenarlo
  
  #Devuelve tibble mezclado (shuffled) y con columna ID
  tbl_df_O
}

#####

Opinion_pool <-function(dist, k, prop, votos_negativos = TRUE,
                        k_method = "random"){
  
  #shuffle_dist: espera los resultados de shuffle_dist
  #k: numero de ideas que cada participante va a ver (numero entero)
  #props: proporcion de ideas seleccionadas en base al algoritmo f1x
  #votos_negativos: booleano, determina si el participante dispone o no de votos negativos
  #TRUE : los participantes tienen votos negativos
  #FALSE : los participantes NO tienen votos negativos
  #k_method: criterio de seleccion de k, puede ser "random" (default), "A" o "B"
  
  library(tidyverse)
  
  
  #nuevo dataframe con las dimensiones de shuffled_dist
  par_pool <- dist[,which(grepl("Dim", colnames(dist)) | grepl("Votos", colnames(dist)))]
  
  #Se obtiene la cantidad total de votos para computar cantidad de votos negativos
  total_votos <- max(dist$Votos_positivos)
  
  #dataframe vacio para incorporar resultados del repeat loop
  O_pool <- tibble(NULL)
  
  participant_vis_tibble <- tibble(NULL)
  
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
    if(votos_negativos){
      Votos_negativos <- abs(par_n$Votos_positivos - total_votos)
    }
    else{
      Votos_negativos <- 0 #Si no se permiten votos negativos, se setea votos_negativos en 0
    }
    
    
    #se guardan los resultados de la funcion Voting
    Voting_loop <- Voting(O_pool, par_dim_only, k, par_n$Votos_positivos, Votos_negativos, prop, k_method)
    
    Voting_res <- as.data.frame(Voting_loop[1]) %>%
      tibble()
    
    #se sobrescribe O_pool con los resultados de la votacion
    #se actualizan visualizaciones, votos positivos y negativos y el grado de conseso
    O_pool <- Voting_res
    
    if(nrow(O_pool > k)){
      Voting_participant_choice <- as.vector(Voting_loop[2])
      
      looping_participant_vis_tibble <- tibble(
        "ID_participante" = rep(nrow(O_pool), length(Voting_participant_choice)), 
        "Ideas_visualizadas" = Voting_participant_choice
      )
      
      participant_vis_tibble <- bind_rows(participant_vis_tibble,looping_participant_vis_tibble)
    }
    
    #se elimina la primera fila del pool de participantes
    par_pool <- par_pool[-c(1:1),]
    
    #el loop se rompe cuando no quedan mas filas en el pool de participantes
    if(nrow(par_pool) == 0)
      break
  }
  
  return(list(O_pool, 
              participant_vis_tibble
  ))
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
      
    } else if(k_method == "B"){
      
      k_opinion <- algoritmo_seleccion_f2x(O_pool, k, prop)
      
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
    
    return(list(O_pool, k_opinion$ID))
    
  }
  
  else{
    return(list(pool_ideas))
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


#####
#Algoritmo de seleccion f2x
#se samplean la proporcion prop de k ideas segun f1x y k-(prop*k) ideas segun < ratio votos-visualizaciones
algoritmo_seleccion_f2x <- function(O_pool, k, prop){
  
  g1_prop <- round(prop*k)
  g2_prop <- k - g1_prop
  
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
  
  if(g1_prop == 0){
    k_opinion <- k_opinion_high
  } else if(g2_prop == 0){
    k_opinion <- k_opinion_low
  } else{
    #Se combinan las filas de ambos subsets para formar k opinones que seran presentadas al participante
    k_opinion <- bind_rows(k_opinion_low, k_opinion_high)
  }
  
  return(k_opinion)
  
}



#funcion general de la simulacion, incorpora mixingfun y Opinion_pool en una funcion
#devuelve una lista que contiene dos dataframes: uno contiene las dimensiones, visualizaciones, votos y rates de c/idea
#y el otro contiene los ID de las ideas que visualiza cada usuario
#Argumentos
#list: lista con vector de n de c/distribucion, lista de vectores de medias de c/distribucion y lista de matrices de covarianza
#beta: distribucion beta para probabilidad de distribucion binomial dentro de mixingfun
#votos_totales: cantidad de votos total para cada participante
simulacion_plataforma <- function(list, beta, votos_totales, k, prop, 
                                  votos_negativos = TRUE, k_method = "random"){
  parametro_alfa <- as.integer(stringr::str_extract_all(beta, '\\d')[[1]][1])
  parametro_beta <- as.integer(stringr::str_extract_all(beta, '\\d')[[1]][2])
  
  beta <- rbeta(
    sum(list$n),
    parametro_alfa,
    parametro_beta
  )
  
  
  dist <- mixingfun(list, beta, votos_totales) #mixingfun
  resultado_simulacion <- Opinion_pool(dist, k, prop, votos_negativos, k_method) #Opinion_pool
  resultado_tbl_opiniones <- resultado_simulacion[1] %>%
    unclass() %>%
    as.data.frame() %>%
    tibble()
  
  covisualizaciones <- resultado_simulacion[2] %>%
    unclass() %>%
    as.data.frame()
  
  return(list(resultado_tbl_opiniones, covisualizaciones)) #devuelve una lista con un  dataframe con las visualizaciones y votos de c/idea y otro con las ideas que visualizo cada usuario
}


#####
#la funcion genera graficos de distribucion de visualizaciones, votos y rates
#devuelve una lista que contiene listas de graficos
#argumentos
#distlist = lista de distribuciones (ej. 1 distribucion por algoritmo)
#k = numerico, cantidad de visualizaciones necesarias para pasar un filtro de visualizaciones (relevante solo para 
#el grafico de distribucion de ideas con mas de k vistas)
#top = numerico, cantidad de ideas seleccionadas por mayor rate 

generador_graficos <- function(distlist){
  
  library(tidyverse)
  library(ggplot2)
  
  combined_df <-distlist %>%
    unclass() %>%
    as.data.frame()
  
  #1 Distribucion de cantidad de visualizaciones
  plot_1a <- ggplot(combined_df, aes(x = visualizaciones)) + 
    geom_bar(size = 1) +
    labs(title = "Distribucion de cantidad de visualizaciones", 
         x= "Cantidad de visualizaciones", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_1b <-  ggplot(combined_df, aes(x = visualizaciones, y = ..count..)) + 
    geom_density() +
    labs(title = "Distribucion de cantidad de visualizaciones", 
         x= "Cantidad de visualizaciones", 
         y = "Frecuencia") +
    theme_minimal()
  
  
  plot_list_1 <- list(plot_1a, plot_1b)
  
  #2 Distribucion de cantidad de votos positivos
  plot_2a <- ggplot(combined_df, aes(x = V_pos)) + 
    geom_bar(size = 1) +
    labs(title = "Distribucion de cantidad de votos positivos", 
         x= "Cantidad de votos positivos", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_2b <-  ggplot(combined_df, aes(x = V_pos, y = ..count..)) + 
    geom_density() +
    labs(title = "Distribucion de cantidad de votos positivos", 
         x= "Cantidad de votos positivos", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_list_2 <- list(plot_2a, plot_2b)
  
  #3 Distribucion de cantidad de votos negativos
  plot_3a <- ggplot(combined_df, aes(x = V_neg)) + 
    geom_bar(size = 1) +
    labs(title = "Distribucion de cantidad de votos negativos", 
         x= "Cantidad de votos negativos", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_3b <- ggplot(combined_df, aes(x = V_neg, y = ..count..)) + 
    geom_density() +
    labs(title = "Distribucion de cantidad de votos negativos", 
         x= "Cantidad de votos negativos", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_list_3 <- list(plot_3a, plot_3b)
  
  #4 Distribucion de cantidad de visualizaciones de ideas que tengan más de k visualizaciones
  
  combined_df_kvis <- combined_df[which(combined_df$visualizaciones >= 6),]
  
  plot_4a <- ggplot(combined_df_kvis, aes(x = visualizaciones)) + 
    geom_bar(size = 1) +
    labs(title = "Distribucion de ideas con más de 6 visualizaciones", 
         x= "Cantidad de visualizaciones", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_4b <- ggplot(combined_df_kvis, aes(x = visualizaciones, y = ..count..)) + 
    geom_density() +
    labs(title = "Distribucion de ideas con más de 6 visualizaciones", 
         x= "Cantidad de visualizaciones", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_list_4<- list(plot_4a, plot_4b)
  
  #5 Distribucion de votos positivos de ideas que tengan mas de 0 votos positivos
  
  combined_df_Vposfilt <- combined_df[which(combined_df$V_pos > 0),]
  
  plot_5a <- ggplot(combined_df_Vposfilt, aes(x = V_pos)) + 
    geom_bar(size = 1) +
    labs(title = "Distribucion de ideas con al menos 1 voto positivo", 
         x= "Cantidad de votos positivos", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_5b <- ggplot(combined_df_Vposfilt, aes(x = V_pos, y = ..count..)) + 
    geom_density() +
    labs(title = "Distribucion de ideas con al menos 1 voto negativo", 
         x= "Cantidad de votos negativos", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_list_5<- list(plot_5a, plot_5b)
  
  #6 Distribucion de votos negativos de ideas que tengan mas de 0 votos negativos
  
  combined_df_Vnegfilt <- combined_df[which(combined_df$V_neg > 0),]
  
  plot_6a <- ggplot(combined_df_Vnegfilt, aes(x = V_neg)) + 
    geom_bar(size = 1) +
    labs(title = "Distribucion de ideas con al menos 1 voto negativo", 
         x= "Cantidad de votos negativos", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_6b <- ggplot(combined_df_Vnegfilt, aes(x = V_neg, y = ..count..)) + 
    geom_density() +
    labs(title = "Distribucion de ideas con al menos 1 voto negativo", 
         x= "Cantidad de votos negativos", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_list_6 <- list(plot_6a, plot_6b)
  
  #7 Distribucion de rates
  
  plot_7a <- ggplot(combined_df, aes(x = ratio_votos_vis)) + 
    geom_bar(size = 1) +
    labs(title = "Distribucion de rates", 
         x= "Ratio votos/visualizaciones", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_7b <- ggplot(combined_df, aes(x = ratio_votos_vis, y = ..count..)) + 
    geom_density() +
    labs(title = "Distribucion de rates", 
         x= "Ratio votos/visualizaciones", 
         y = "Frecuencia") +
    theme_minimal()
  
  
  plot_list_7 <- list(plot_7a, plot_7b)
  
  #8 Distribucion de rates de ideas que tengan al menos 1 voto
  
  combined_df_Votefilt <- combined_df[which(combined_df$V_neg > 0 |
                                              combined_df$V_pos > 0),]
  
  plot_8a <- ggplot(combined_df_Votefilt, aes(x = ratio_votos_vis)) + 
    geom_bar(size = 1) +
    labs(title = "Distribucion de rates de ideas con al menos 1 voto", 
         x= "Ratio votos/visualizaciones", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_8b <- ggplot(combined_df_Votefilt, aes(x = ratio_votos_vis, y = ..count..)) + 
    geom_density() +
    labs(title = "Distribucion de rates de ideas con al menos 1 voto", 
         x= "Ratio votos/visualizaciones", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_list_8<- list(plot_8a, plot_8b)
  
  #9 Distribucion de rates x visualizaciones
  
  plot_9a <- ggplot(combined_df, aes(ratio_votos_vis, visualizaciones)) + 
    geom_point() + 
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio/Visualizaciones") + 
    theme_minimal() 
  
  plot_list_9 <- list(plot_9a)
  #10 Distribucion de rates de ideas x visualizaciones que tengan al menos 1 voto
  
  plot_10b <- ggplot(combined_df_Votefilt, aes(ratio_votos_vis, visualizaciones)) + 
    geom_point() + 
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio/Visualizaciones minimo 1 voto") + 
    theme_minimal() 
  
  plot_list_10 <- list(plot_10b)
  
  #Creacion de dataframe con top 25 de cada algoritmo
  
  combined_df_top <- combined_df[order(combined_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(. , visualizaciones >= 6) %>%
    slice_head(. , n = 25) 
  
  #11 Distribucion de rates de las 25 ideas con mejor rate
  
  plot_11a <- ggplot(combined_df_top, aes(x = ratio_votos_vis)) + 
    geom_bar(size = 1) +
    labs(title = "Distribucion de rates del top 25", 
         x= "Ratio votos/visualizaciones", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_11b <- ggplot(combined_df_top, aes(x = ratio_votos_vis, y = ..count..)) + 
    geom_density() +
    labs(title = "Distribucion de rates del top 25", 
         x= "Ratio votos/visualizaciones", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_list_11 <- list(plot_11a, plot_11b)
  
  #12 Distribucion de visualizaciones de las 25 ideas con mejor rate
  
  plot_12a <- ggplot(combined_df_top, aes(x = visualizaciones)) + 
    geom_bar(size = 1) +
    labs(title = "Distribucion de cantidad de visualizaciones del top 25", 
         x= "Cantidad de visualizaciones", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_12b <- ggplot(combined_df_top, aes(x = visualizaciones, y = ..count..)) + 
    geom_density() +
    labs(title = "Distribucion de cantidad de visualizaciones del top 25", 
         x= "Cantidad de visualizaciones", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_list_12 <- list(plot_12a, plot_12b)
  
  #13 Distribucion de votos de las 25 ideas con mejor rate
  
  plot_13a <- ggplot(combined_df_top, aes(x = V_pos)) + 
    geom_bar(size = 1) +
    labs(title = "Distribucion votos positivos del top 25", 
         x= "Cantidad de votos positivos", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_13b <- ggplot(combined_df_top, aes(x = V_pos, y = ..count..)) + 
    geom_density() +
    labs(title = "Distribucion votos positivos del top 25", 
         x= "Cantidad de votos positivos", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_13c <- ggplot(combined_df_top, aes(x = V_neg)) + 
    geom_bar(size = 1) +
    labs(title = "Distribucion de votos negativos del top 25", 
         x= "Cantidad de votos negativos", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_13d <- ggplot(combined_df_top, aes(x = V_neg, y = ..count..)) + 
    geom_density() +
    labs(title = "Distribucion de votos negativos del top 25", 
         x= "Cantidad de votos negativos", 
         y = "Frecuencia") +
    theme_minimal()
  
  plot_list_13 <- list(plot_13a, plot_13b, plot_13c, plot_13d)
  ##################
  
  #devuelve una lista con listas de graficos
  return(list(
    "visualizaciones" = plot_list_1,
    "votos_positivos" = plot_list_2,
    "votos_negativos" = plot_list_3,
    "visualizaciones_k" = plot_list_4,
    "votos_positivos_atleast1vote" = plot_list_5, 
    "votos_negativos_atleast1vote" = plot_list_6,
    "rates" = plot_list_7,
    "rates_atleast1vote" = plot_list_8,
    "ratesxvisualizaciones" = plot_list_9,
    "ratesxvisualizaciones_atleast1vote" = plot_list_10,
    "rates_top" = plot_list_11, 
    "visualizaciones_top" = plot_list_12, 
    "votos_top" = plot_list_13
  ))
}

######
