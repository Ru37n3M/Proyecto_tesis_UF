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

Opinion_pool <-function(dist, k, prop, votos_negativos = 1,
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
    
    #Se restan los votos positivos del total de votos para obtener la cantidad de votos negativos
    if(as.logical(votos_negativos)){
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
  
  #Condicional para chequear si el pool de ideas tiene por lo menos k filas
  if(nrow(pool_ideas) >= k)
  {
    
    #Condicionales para elegir algoritmos de seleccion
    if(k_method == "A"){
      
      k_opinion <- algoritmo_seleccion_f1x(pool_ideas, k)
      
    } else if(k_method == "B"){
      
      k_opinion <- algoritmo_seleccion_f2x(pool_ideas, k, prop)
      
    }
    
    else{
      #Sampleo aleatorio
      k_opinion <- sample_n(pool_ideas, k, replace = F)
    }
    
    pool_ideas$visualizaciones[which(pool_ideas$ID%in%k_opinion$ID)] <- pool_ideas$visualizaciones[which(pool_ideas$ID%in%k_opinion$ID)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
    # en 
    
    #k vectores con las dimensiones de k_opinion
    k2_noid <- k_opinion[,which(grepl("Dim", colnames(pool_ideas)))]
    
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
      Voted_pos <- sample_n(k_opinion, vpos, prob = c(Poij_pos), replace = F)
      
      #se suma 1 punto a la idea que se corresponde con el valor minimo
      pool_ideas$V_pos[which(pool_ideas$ID%in%Voted_pos$ID)] <- pool_ideas$V_pos[which(pool_ideas$ID%in%Voted_pos$ID)] + 1 
      
    }
    
    
    #Sumatoria de todas las distancias
    sumDij <- sum(Dij)
    
    #Probabilidad de voto negativo
    Poij_neg<- sapply(Dij, function(x) x / sumDij )
    
    #Condicional para votos negativos, chequea si el participante tiene votos negativos
    if(vneg > 0){
      
      #Se samplea vneg de k_opinion con probabilidades de voto negativo para cada vector
      Voted_neg <- sample_n(k_opinion,vneg, prob = c(Poij_neg), replace = F)
      
      #se suma 1 punto a la idea que se corresponde con el valor minimo
      pool_ideas$V_neg[which(pool_ideas$ID%in%Voted_neg$ID)] <- pool_ideas$V_neg[which(pool_ideas$ID%in%Voted_neg$ID)] + 1 
      
    }
    
    #Ratio Votos/visualizaciones
    pool_ideas$ratio_votos_vis[which(pool_ideas$ID%in%k_opinion$ID)] <- (pool_ideas$V_pos[which(pool_ideas$ID%in%k_opinion$ID)]-
                                                                           pool_ideas$V_neg[which(pool_ideas$ID%in%k_opinion$ID)])/pool_ideas$visualizaciones[which(pool_ideas$ID%in%k_opinion$ID)]
    
    return(list(pool_ideas, k_opinion$ID))
    
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
  
  if(g1_prop == 0){
    k_opinion <- O_pool[order(O_pool$ratio_votos_vis, decreasing = T),] %>%
      slice_head(n = k)
  } else if(g2_prop == 0){
    k_opinion <- O_pool[order(O_pool$visualizaciones, decreasing = T),] %>%
      slice_tail(n = k)
  } else{
    
    #Se subsetean las últimas k/2 filas del dataset ordenado de forma decreciente segun visualizaciones
    k_opinion_low <- O_pool[order(O_pool$visualizaciones, decreasing = T),] %>%
      slice_tail(n = g1_prop)
    
    O_pool <- O_pool[-which(O_pool$ID%in%k_opinion_low$ID),]
    
    #Se subsetean las primeras k/2 filas del dataset ordenado de forma decreciente segun ratio votos-visualizaciones
    k_opinion_high <- O_pool[order(O_pool$ratio_votos_vis, decreasing = T),] %>%
      slice_head(n = g2_prop)
    
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
                                  votos_negativos = 1, k_method = "random"){
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
#parametros_simulacion_df : dataframe con los parametros de la simulacion

generador_graficos <- function(dislist, parametros_simulacion_df){
  
  library(tidyverse)
  library(ggplot2)
  library(ggpubr)
  library(MASS)
  
  combined_df <- test_only_opinion %>%
    bind_rows() %>%
    mutate(
      'N' = rep(as.factor(parametros_simulacion_df$N), parametros_simulacion_df$N),
      'cantidad_votos' = rep(as.factor(parametros_simulacion_df$cant_votos), parametros_simulacion_df$N),
      'cantidad_ideas' = rep(as.factor(parametros_simulacion_df$cant_ideas), parametros_simulacion_df$N),
      'Negativos' = rep(as.factor(parametros_simulacion_df$Negativos), parametros_simulacion_df$N),
      'Beta' = rep(parametros_simulacion_df$Beta, parametros_simulacion_df$N),
      'Algoritmo' = rep(as.factor(parametros_simulacion_df$Algoritmo), parametros_simulacion_df$N)
    )
  
  combined_df[,5] <- combined_df[,5] + 1
  
  N_df <- combined_df %>% 
    filter(Beta == "6-2" & Algoritmo == 0.5 & 
             cantidad_ideas == 10 & Negativos == 1 & cantidad_votos == 3)
  
  
  ideas_df <- combined_df %>%
    filter(N == 500  & Beta == "6-2" & Algoritmo == 0.5 & Negativos == 1 & cantidad_votos == 3)
  
  
  votos_df <- combined_df %>%
    filter(N == 500  & Beta == "6-2" & Algoritmo == 0.5 & Negativos == 1 & cantidad_ideas == 10)
  
  
  Negativos_df_part1 <- combined_df %>%
    filter(N == 500  & Beta == "6-2" & Algoritmo == 0.5 & cantidad_votos == 3 & cantidad_ideas == 10)
  
  Negativos_df_part2 <- combined_df %>%
    filter(N == 500  & Beta == "1-0" & Algoritmo == 0.5 & cantidad_votos == 3 & cantidad_ideas == 10)
  
  Negativos_df_part1$Negativos <- 'Si'
  Negativos_df_part2$Negativos <- 'No' 
  
  Negativos_df <- bind_rows(Negativos_df_part1, Negativos_df_part2)
  
  
  Beta_df <- combined_df %>%
    filter(N == 500  &  Algoritmo == 0.5 & cantidad_votos == 3 & cantidad_ideas == 10)
  
  
  Algoritmo_df <- combined_df %>%
    filter(N == 500  &  Negativos == 1 & Beta == "6-2" & cantidad_votos == 3 & cantidad_ideas == 10)
  
  
  #1 Distribucion de Cantidad de visualizaciones (log10)
  plot_1a <- ggplot(votos_df, aes(x = log10(visualizaciones), y = after_stat(density), col =cantidad_votos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos disponibles")) +
    labs(x= "Cantidad de visualizaciones (log10)", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_1b <- ggplot(N_df, aes(x = log10(visualizaciones), y = after_stat(density) , col =N)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="n")) +
    labs( 
      x= "Cantidad de visualizaciones (log10)", 
      y = "Densidad") +
    
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_1c <- ggplot(ideas_df, aes(x = log10(visualizaciones), y = after_stat(density) ,col =cantidad_ideas)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Ideas visualizadas (p/par.)")) +
    labs( 
      x= "Cantidad de visualizaciones (log10)", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_1d <- ggplot(Negativos_df, aes(x = log10(visualizaciones), y = after_stat(density) , col =Negativos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos negativos")) +
    labs( 
      x= "Cantidad de visualizaciones (log10)", 
      y = "Densidad", fill = 'Votos Negativos') +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_1e <- ggplot(Beta_df, aes(x = log10(visualizaciones), y = after_stat(density) , col =Beta)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs( 
      x= "Cantidad de visualizaciones (log10)", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_1f <- ggplot(Algoritmo_df, aes(x = log10(visualizaciones), y = after_stat(density) , col =Algoritmo)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs( 
      x= "Cantidad de visualizaciones (log10)", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  #2 Distribucion de cantidad de votos positivos
  plot_2a <- ggplot(votos_df, aes(x = V_pos, y = after_stat(density) , col =cantidad_votos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos disponibles")) +
    labs( 
      x= "Cantidad de votos positivos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_2b <- ggplot(N_df, aes(x = V_pos, y = after_stat(density) , col =N)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="n")) +
    labs( 
      x= "Cantidad de votos positivos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_2c <- ggplot(ideas_df, aes(x = V_pos, y = after_stat(density) , col =cantidad_ideas)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Ideas visualizadas (p/par.)")) +
    labs( 
      x= "Cantidad de votos positivos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_2d <- ggplot(Negativos_df, aes(x = V_pos, y = after_stat(density) , col =Negativos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos negativos")) +
    labs( 
      x= "Cantidad de votos positivos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_2e <- ggplot(Beta_df, aes(x = V_pos, y = after_stat(density) , col =Beta)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs( 
      x= "Cantidad de votos positivos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_2f <- ggplot(Algoritmo_df, aes(x = V_pos, y = after_stat(density) , col =Algoritmo)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs( 
      x= "Cantidad de votos positivos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  #3 Distribucion de cantidad de votos negativos
  plot_3a <- ggplot(votos_df, aes(x = V_neg, y = after_stat(density) , col =cantidad_votos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos disponibles")) +
    labs( 
      x= "Cantidad de votos negativos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_3b <- ggplot(N_df, aes(x = V_neg, y = after_stat(density) , col =N)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="n")) +
    labs( 
      x= "Cantidad de votos negativos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_3c <- ggplot(ideas_df, aes(x = V_neg, y = after_stat(density) , col =cantidad_ideas)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Ideas visualizadas (p/par.)")) +
    labs( 
      x= "Cantidad de votos negativos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_3d <- ggplot(Negativos_df, aes(x = V_neg, y = after_stat(density) , col =Negativos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos negativos")) +
    labs( 
      x= "Cantidad de votos negativos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  
  plot_3e <- ggplot(Beta_df, aes(x = V_neg, y = after_stat(density) , col =Beta)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs( 
      x= "Cantidad de votos negativos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_3f <- ggplot(Algoritmo_df, aes(x = V_neg, y = after_stat(density) , col =Algoritmo)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs( 
      x= "Cantidad de votos negativos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  #4 Distribucion de Cantidad de visualizaciones (log10) de ideas que tengan más de k visualizaciones
  
  #cantidad_votos
  votos_df_kvis <- votos_df[which(votos_df$visualizaciones >= 7),]
  
  #N
  N_df_kvis <- N_df[which(N_df$visualizaciones >= 7),]
  
  #cantidad_ideas
  ideas_df_kvis <- ideas_df[which(ideas_df$visualizaciones >= 7),]
  
  #Negativos
  Negativos_df_kvis <- Negativos_df[which(Negativos_df$visualizaciones >= 7),]
  
  #Beta
  Beta_df_kvis <- Beta_df[which(Beta_df$visualizaciones >= 7),]
  
  #Algoritmo
  Algoritmo_df_kvis <- Algoritmo_df[which(Algoritmo_df$visualizaciones >= 7),]
  
  plot_4a <- ggplot(votos_df_kvis, aes(x = log10(visualizaciones), y = after_stat(density) , col =cantidad_votos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos disponibles")) +
    labs(title = "Distribucion de ideas con más de 6 visualizaciones", 
         x= "Cantidad de visualizaciones (log10)", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_4b <- ggplot(N_df_kvis, aes(x = log10(visualizaciones), y = after_stat(density) , col =N)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="n")) +
    labs(title = "Distribucion de ideas con más de 6 visualizaciones", 
         x= "Cantidad de visualizaciones (log10)", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_4c <- ggplot(ideas_df_kvis, aes(x = log10(visualizaciones), y = after_stat(density) , col =cantidad_ideas)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Ideas visualizadas (p/par.)")) +
    labs(title = "Distribucion de ideas con más de 6 visualizaciones", 
         x= "Cantidad de visualizaciones (log10)", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_4d <- ggplot(Negativos_df_kvis, aes(x = log10(visualizaciones), y = after_stat(density) , col =Negativos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos negativos")) +
    labs(title = "Distribucion de ideas con más de 6 visualizaciones", 
         x= "Cantidad de visualizaciones (log10)", 
         y = "Densidad", fill = 'Votos Negativos') +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_4e <- ggplot(Beta_df_kvis, aes(x = log10(visualizaciones), y = after_stat(density) , col =Beta)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs(title = "Distribucion de ideas con más de 6 visualizaciones", 
         x= "Cantidad de visualizaciones (log10)", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_4f <- ggplot(Algoritmo_df_kvis, aes(x = log10(visualizaciones), y = after_stat(density) , col =Algoritmo)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs(title = "Distribucion de ideas con más de 6 visualizaciones", 
         x= "Cantidad de visualizaciones (log10)", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  #5 Distribucion de votos positivos de ideas que tengan mas de 0 votos positivos
  
  #cantidad_votos
  votos_df_Vposfilt <- votos_df[which(votos_df$V_pos > 0),]
  
  #N
  N_df_Vposfilt <- N_df[which(N_df$V_pos > 0),]
  
  #cantidad_ideas
  ideas_df_Vposfilt <- ideas_df[which(ideas_df$V_pos > 0),]
  
  #Negativos
  Negativos_df_Vposfilt <- Negativos_df[which(Negativos_df$V_pos > 0),]
  
  #Beta
  Beta_df_Vposfilt <- Beta_df[which(Beta_df$V_pos > 0),]
  
  #Algoritmo
  Algoritmo_df_Vposfilt <- Algoritmo_df[which(Algoritmo_df$V_pos > 0),]
  
  plot_5a <- ggplot(votos_df_Vposfilt, aes(x = V_pos, y = after_stat(density) , col =cantidad_votos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos disponibles")) +
    labs(title = "Distribucion de ideas con al menos 1 voto positivo", 
         x= "Cantidad de votos positivos", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_5b <- ggplot(N_df_Vposfilt, aes(x = V_pos, y = after_stat(density) , col =N)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="n")) +
    labs(title = "Distribucion de ideas con al menos 1 voto positivo", 
         x= "Cantidad de votos positivos", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_5c <- ggplot(ideas_df_Vposfilt, aes(x = V_pos, y = after_stat(density) , col =cantidad_ideas)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Ideas visualizadas (p/par.)")) +
    labs(title = "Distribucion de ideas con al menos 1 voto positivo", 
         x= "Cantidad de votos positivos", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_5d <- ggplot(Negativos_df_Vposfilt, aes(x = V_pos, y = after_stat(density) , col =Negativos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos negativos")) +
    labs(title = "Distribucion de ideas con al menos 1 voto positivo", 
         x= "Cantidad de votos positivos", 
         y = "Densidad", fill = 'Votos Negativos') +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_5e <- ggplot(Beta_df_Vposfilt, aes(x = V_pos, y = after_stat(density) , col =Beta)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs(title = "Distribucion de ideas con al menos 1 voto positivo", 
         x= "Cantidad de votos positivos", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_5f <- ggplot(Algoritmo_df_Vposfilt, aes(x = V_pos, y = after_stat(density) , col =Algoritmo)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs(title = "Distribucion de ideas con al menos 1 voto positivo", 
         x= "Cantidad de votos positivos", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  #6 Distribucion de votos negativos de ideas que tengan mas de 0 votos negativos
  
  #cantidad_votos
  votos_df_Vnegfilt <- votos_df[which(votos_df$V_neg > 0),]
  
  #N
  N_df_Vnegfilt <- N_df[which(N_df$V_neg > 0),]
  
  #cantidad_ideas
  ideas_df_Vnegfilt <- ideas_df[which(ideas_df$V_neg > 0),]
  
  #Negativos
  Negativos_df_Vnegfilt <- Negativos_df[which(Negativos_df$V_neg > 0),]
  
  #Beta
  Beta_df_Vnegfilt <- Beta_df[which(Beta_df$V_neg > 0),]
  
  #Algoritmo
  Algoritmo_df_Vnegfilt <- Algoritmo_df[which(Algoritmo_df$V_neg > 0),]
  
  plot_6a <- ggplot(votos_df_Vnegfilt, aes(x = V_neg, y = after_stat(density) , col =cantidad_votos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos disponibles")) +
    labs(title = "Distribucion de ideas con al menos 1 voto negativo", 
         x= "Cantidad de votos negativos", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_6b <- ggplot(N_df_Vnegfilt, aes(x = V_neg, y = after_stat(density) , col =N)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="n")) +
    labs(title = "Distribucion de ideas con al menos 1 voto negativo", 
         x= "Cantidad de votos negativos", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_6c <- ggplot(ideas_df_Vnegfilt, aes(x = V_neg, y = after_stat(density) , col =cantidad_ideas)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Ideas visualizadas (p/par.)")) +
    labs(title = "Distribucion de ideas con al menos 1 voto negativo", 
         x= "Cantidad de votos negativos", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_6d <- ggplot(Negativos_df_Vnegfilt, aes(x = V_neg, y = after_stat(density) , col =Negativos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos negativos")) +
    labs(title = "Distribucion de ideas con al menos 1 voto negativo", 
         x= "Cantidad de votos negativos", 
         y = "Densidad", fill = 'Votos Negativos') +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_6e <- ggplot(Beta_df_Vnegfilt, aes(x = V_neg, y = after_stat(density) , col =Beta)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs(title = "Distribucion de ideas con al menos 1 voto negativo", 
         x= "Cantidad de votos negativos", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_6f <- ggplot(Algoritmo_df_Vnegfilt, aes(x = V_neg, y = after_stat(density) , col =Algoritmo)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs(title = "Distribucion de ideas con al menos 1 voto negativo", 
         x= "Cantidad de votos negativos", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  
  #7 Distribucion de rates
  
  plot_7a <- ggplot(votos_df, aes(x = ratio_votos_vis, y = after_stat(density) ,col =cantidad_votos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos disponibles")) +
    labs( 
      x= "Ratio votos/visualizaciones", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_7b <- ggplot(N_df, aes(x = ratio_votos_vis, y = after_stat(density) ,col =N)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="n")) +
    labs( 
      x= "Ratio votos/visualizaciones", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_7c <- ggplot(ideas_df, aes(x = ratio_votos_vis, y = after_stat(density) ,col =cantidad_ideas)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Ideas visualizadas (p/par.)")) +
    labs( 
      x= "Ratio votos/visualizaciones", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_7d <- ggplot(Negativos_df, aes(x = ratio_votos_vis, y = after_stat(density) ,col =Negativos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos negativos")) +
    labs( 
      x= "Ratio votos/visualizaciones", 
      y = "Densidad", fill = 'Votos Negativos') +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.2,0.7), legend.title.align = 0.5) 
  
  plot_7e <- ggplot(Beta_df, aes(x = ratio_votos_vis, y = after_stat(density) ,col =Beta)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs( 
      x= "Ratio votos/visualizaciones", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.2,0.7), legend.title.align = 0.5)
  
  plot_7f <- ggplot(Algoritmo_df, aes(x = ratio_votos_vis, y = after_stat(density) ,col =Algoritmo)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs( 
      x= "Ratio votos/visualizaciones", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  
  #8 Distribucion de rates de ideas que tengan al menos 1 voto
  
  #cantidad_votos
  votos_df_Votefilt <- votos_df[which(votos_df$V_neg > 0 |
                                        votos_df$V_pos > 0),]
  #N
  N_df_Votefilt <- N_df[which(N_df$V_neg > 0 |
                                N_df$V_pos > 0),]
  #cantidad_ideas
  ideas_df_Votefilt <- ideas_df[which(ideas_df$V_neg > 0 |
                                        ideas_df$V_pos > 0),]
  #Negativos
  Negativos_df_Votefilt <- Negativos_df[which(Negativos_df$V_neg > 0 |
                                                Negativos_df$V_pos > 0),]
  #Beta
  Beta_df_Votefilt <- Beta_df[which(Beta_df$V_neg > 0 |
                                      Beta_df$V_pos > 0),]
  #Algoritmo
  Algoritmo_df_Votefilt <- Algoritmo_df[which(Algoritmo_df$V_neg > 0 |
                                                Algoritmo_df$V_pos > 0),]
  
  plot_8a <- ggplot(votos_df_Votefilt, aes(x = ratio_votos_vis, y = after_stat(density) ,col =cantidad_votos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos disponibles")) +
    labs(title = "Distribucion de rates de ideas con al menos 1 voto", 
         x= "Ratio votos/visualizaciones", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_8b <- ggplot(N_df_Votefilt, aes(x = ratio_votos_vis, y = after_stat(density) ,col =N)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="n")) +
    labs(title = "Distribucion de rates de ideas con al menos 1 voto", 
         x= "Ratio votos/visualizaciones", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5) 
  
  plot_8c <- ggplot(ideas_df_Votefilt, aes(x = ratio_votos_vis, y = after_stat(density) ,col =cantidad_ideas)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Ideas visualizadas (p/par.)")) +
    labs(title = "Distribucion de rates de ideas con al menos 1 voto", 
         x= "Ratio votos/visualizaciones", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_8d <- ggplot(Negativos_df_Votefilt, aes(x = ratio_votos_vis, y = after_stat(density) ,col =Negativos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    guides(color=guide_legend(title="Votos negativos")) +
    labs(title = "Distribucion de rates de ideas con al menos 1 voto", 
         x= "Ratio votos/visualizaciones", 
         y = "Densidad", fill = 'Votos Negativos') +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.2,0.7), legend.title.align = 0.5) 
  
  plot_8e <- ggplot(Beta_df_Votefilt, aes(x = ratio_votos_vis, y = after_stat(density) ,col =Beta)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs(title = "Distribucion de rates de ideas con al menos 1 voto", 
         x= "Ratio votos/visualizaciones", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.2,0.7), legend.title.align = 0.5)
  
  plot_8f <- ggplot(Algoritmo_df_Votefilt, aes(x = ratio_votos_vis, y = after_stat(density) ,col =Algoritmo)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70, adjust = 3.75) +
    labs(title = "Distribucion de rates de ideas con al menos 1 voto", 
         x= "Ratio votos/visualizaciones", 
         y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5) 
  
  
  #9 Distribucion de rates x visualizaciones
  
  plot_9a <- ggplot(votos_df, aes(x =ratio_votos_vis, y = log10(visualizaciones))) + 
    stat_density_2d_filled( bins = 250,   n = 120, contour_var = 'ndensity', 
                            h = c(ifelse(bandwidth.nrd(votos_df$ratio_votos_vis) == 0, 0.1, 
                                         bandwidth.nrd(votos_df$ratio_votos_vis)),           
                                  ifelse(bandwidth.nrd(log10(votos_df$visualizaciones)) == 0, 0.1, 
                                         bandwidth.nrd(log10(votos_df$visualizaciones) ) ) ) ) +   
    coord_cartesian(xlim = c(-1, 1)) +
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones (log10)", subtitle = 'Votos disponibles') + 
    theme(legend.position = 'none', panel.background = element_rect(fill = 'transparent', color = NA)) +
    facet_wrap(~cantidad_votos)
  
  plot_9b <- ggplot(N_df, aes(x =ratio_votos_vis, y = log10(visualizaciones))) + 
    stat_density_2d_filled( bins = 250,   n = 120, contour_var = 'ndensity',
                            h = c(ifelse(bandwidth.nrd(N_df$ratio_votos_vis) == 0, 0.1, 
                                         bandwidth.nrd(N_df$ratio_votos_vis)),           
                                  ifelse(bandwidth.nrd(log10(N_df$visualizaciones)) == 0, 0.1, 
                                         bandwidth.nrd(log10(N_df$visualizaciones)))))+   coord_cartesian(xlim = c(-1, 1)) +
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones (log10)", subtitle = 'n') + 
    theme(legend.position = 'none', panel.background = element_rect(fill = 'transparent', color = NA)) +
    facet_wrap(~N)
  
  plot_9c <- ggplot(ideas_df, aes(x =ratio_votos_vis, y = log10(visualizaciones))) + 
    stat_density_2d_filled( bins = 250,   n = 120, contour_var = 'ndensity',
                            h = c(ifelse(bandwidth.nrd(ideas_df$ratio_votos_vis) == 0, 0.1, 
                                         bandwidth.nrd(ideas_df$ratio_votos_vis)),          
                                  ifelse(bandwidth.nrd(log10(ideas_df$visualizaciones)) == 0, 0.1, 
                                         bandwidth.nrd(log10(ideas_df$visualizaciones))))) +   
    coord_cartesian(xlim = c(-1, 1)) +
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones (log10)", subtitle = 'Ideas visualizadas (p/par.)') + 
    theme(legend.position = 'none', panel.background = element_rect(fill = 'transparent', color = NA)) +
    facet_wrap(~cantidad_ideas)
  
  plot_9d <- ggplot(Negativos_df, aes(x =ratio_votos_vis, y = log10(visualizaciones))) + 
    stat_density_2d_filled( bins = 250,   n = 120, contour_var = 'ndensity',
                            h = c(ifelse(bandwidth.nrd(Negativos_df$ratio_votos_vis) == 0, 0.1, 
                                         bandwidth.nrd(Negativos_df$ratio_votos_vis)),           
                                  ifelse(bandwidth.nrd(log10(Negativos_df$visualizaciones)) == 0, 0.1, 
                                         bandwidth.nrd(log10(Negativos_df$visualizaciones))))) +   
    coord_cartesian(xlim = c(-1, 1)) +
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones (log10)", subtitle = 'Votos negativos disponibles') + 
    theme(legend.position = 'none', panel.background = element_rect(fill = 'transparent', color = NA)) +
    facet_wrap(~Negativos)
  
  plot_9e <- ggplot(Beta_df, aes(x =ratio_votos_vis, y = log10(visualizaciones))) + 
    stat_density_2d_filled( bins = 250,   n = 120, contour_var = 'ndensity',
                            h = c(ifelse(bandwidth.nrd(Beta_df$ratio_votos_vis) == 0, 0.1, 
                                         bandwidth.nrd(Beta_df$ratio_votos_vis)),           
                                  ifelse(bandwidth.nrd(log10(Beta_df$visualizaciones)) == 0, 0.1, 
                                         bandwidth.nrd(log10(Beta_df$visualizaciones))))) +   
    coord_cartesian(xlim = c(-1, 1)) +
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones (log10)", subtitle = 'Beta (prob. de voto positivo)') + 
    theme(legend.position = 'none', panel.background = element_rect(fill = 'transparent', color = NA)) +
    facet_wrap(~Beta)
  
  plot_9f <- ggplot(Algoritmo_df, aes(x =ratio_votos_vis, y = log10(visualizaciones))) + 
    stat_density_2d_filled( bins = 250,   n = 120, contour_var = 'ndensity',
                            h = c(ifelse(bandwidth.nrd(Algoritmo_df$ratio_votos_vis) == 0, 0.1, 
                                         bandwidth.nrd(Algoritmo_df$ratio_votos_vis)),           
                                  ifelse(bandwidth.nrd(log10(Algoritmo_df$visualizaciones)) == 0, 0.1, 
                                         bandwidth.nrd(log10(Algoritmo_df$visualizaciones))))) +   
    coord_cartesian(xlim = c(-1, 1)) +
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones (log10)", subtitle = 'Algoritmos de selección de ideas') + 
    theme(legend.position = 'none', panel.background = element_rect(fill = 'transparent', color = NA)) +
    facet_wrap(~Algoritmo)
  
  #10 Distribucion de rates de ideas x visualizaciones que tengan al menos 1 voto
  
  plot_10a <- ggplot(votos_df_Votefilt, aes(ratio_votos_vis, log10(visualizaciones))) + 
    stat_density_2d_filled( bins = 250,   n = 120, contour_var = 'ndensity',
                            h = c(ifelse(bandwidth.nrd(votos_df$ratio_votos_vis) == 0, 0.1, 
                                         bandwidth.nrd(votos_df$ratio_votos_vis)),           
                                  ifelse(bandwidth.nrd(log10(votos_df$visualizaciones)) == 0, 0.1, 
                                         bandwidth.nrd(log10(votos_df$visualizaciones))))) +   
    coord_cartesian(xlim = c(-1, 1)) +
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones (log10)", title = "Distribución Ratio/Visualizaciones minimo 1 voto", 
         col = 'Obs.') + 
    theme(legend.position = 'none', panel.background = element_rect(fill = 'transparent', color = NA)) +
    facet_wrap(~cantidad_votos)
  
  plot_10b <- ggplot(N_df_Votefilt, aes(ratio_votos_vis, log10(visualizaciones))) + 
    stat_density_2d_filled( bins = 250,   n = 120, contour_var = 'ndensity',
                            h = c(ifelse(bandwidth.nrd(N_df$ratio_votos_vis) == 0, 0.1, 
                                         bandwidth.nrd(N_df$ratio_votos_vis)),           
                                  ifelse(bandwidth.nrd(log10(N_df$visualizaciones)) == 0, 0.1, 
                                         bandwidth.nrd(log10(N_df$visualizaciones))))) +   
    coord_cartesian(xlim = c(-1, 1)) +
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones (log10)", title = "Distribución Ratio/Visualizaciones minimo 1 voto", 
         col = 'Obs.') + 
    theme(legend.position = 'none', panel.background = element_rect(fill = 'transparent', color = NA)) +
    facet_wrap(~N)
  
  plot_10c <- ggplot(ideas_df_Votefilt, aes(ratio_votos_vis, log10(visualizaciones))) + 
    stat_density_2d_filled( bins = 250,   n = 120, contour_var = 'ndensity', 
                            h = c(ifelse(bandwidth.nrd(ideas_df$ratio_votos_vis) == 0, 0.1, 
                                         bandwidth.nrd(ideas_df$ratio_votos_vis)),           
                                  ifelse(bandwidth.nrd(log10(ideas_df$visualizaciones)) == 0, 0.1, 
                                         bandwidth.nrd(log10(ideas_df$visualizaciones) ) ) ) ) +   
    coord_cartesian(xlim = c(-1, 1)) +
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones (log10)", title = "Distribución Ratio/Visualizaciones minimo 1 voto", 
         col = 'Obs.') + 
    theme(legend.position = 'none', panel.background = element_rect(fill = 'transparent', color = NA)) +
    facet_wrap(~cantidad_ideas)
  
  plot_10d <- ggplot(Negativos_df_Votefilt, aes(ratio_votos_vis, log10(visualizaciones))) + 
    stat_density_2d_filled( bins = 250,   n = 120, contour_var = 'ndensity', 
                            h = c(ifelse(bandwidth.nrd(Negativos_df$ratio_votos_vis) == 0, 0.1, 
                                         bandwidth.nrd(Negativos_df$ratio_votos_vis)),           
                                  ifelse(bandwidth.nrd(log10(Negativos_df$visualizaciones)) == 0, 0.1, 
                                         bandwidth.nrd(log10(Negativos_df$visualizaciones))))) +   
    coord_cartesian(xlim = c(-1, 1)) +
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones (log10)", title = "Distribución Ratio/Visualizaciones minimo 1 voto", 
         col = 'Obs.') + 
    theme(legend.position = 'none', panel.background = element_rect(fill = 'transparent', color = NA)) +
    facet_wrap(~Negativos)
  
  plot_10e <- ggplot(Beta_df_Votefilt, aes(ratio_votos_vis, log10(visualizaciones))) + 
    stat_density_2d_filled( bins = 250,   n = 120, contour_var = 'ndensity',
                            h = c(ifelse(bandwidth.nrd(Beta_df$ratio_votos_vis) == 0, 0.1, 
                                         bandwidth.nrd(Beta_df$ratio_votos_vis)),           
                                  ifelse(bandwidth.nrd(log10(Beta_df$visualizaciones)) == 0, 0.1, 
                                         bandwidth.nrd(log10(Beta_df$visualizaciones))))) +   
    coord_cartesian(xlim = c(-1, 1)) +
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones (log10)", title = "Distribución Ratio/Visualizaciones minimo 1 voto", 
         col = 'Obs.') + 
    theme(legend.position = 'none', panel.background = element_rect(fill = 'transparent', color = NA)) +
    facet_wrap(~Beta)
  
  plot_10f <- ggplot(Algoritmo_df_Votefilt, aes(ratio_votos_vis, log10(visualizaciones))) + 
    stat_density_2d_filled( bins = 250,   n = 120, contour_var = 'ndensity', 
                            h = c(ifelse(bandwidth.nrd(Algoritmo_df$ratio_votos_vis) == 0, 0.1, 
                                         bandwidth.nrd(Algoritmo_df$ratio_votos_vis)),           
                                  ifelse(bandwidth.nrd(log10(Algoritmo_df$visualizaciones) ) == 0, 0.1, 
                                         bandwidth.nrd(log10(Algoritmo_df$visualizaciones) ) ) ) ) +   
    coord_cartesian(xlim = c(-1, 1)) +
    labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones (log10)", title = "Distribución Ratio/Visualizaciones minimo 1 voto", 
         col = 'Obs.') + 
    theme(legend.position = 'none', panel.background = element_rect(fill = 'transparent', color = NA)) +
    facet_wrap(~Algoritmo)
  
  
  #Creacion de dataframe con top 25 de cada algoritmo
  
  #TOP 25 Parametro N
  
  n100 <- N_df[order(N_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(N == 100) %>%
    slice_head(. , n = 25) 
  
  n200 <- N_df[order(N_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(N == 200) %>%
    slice_head(. , n = 25) 
  
  n500 <- N_df[order(N_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(N == 500) %>%
    slice_head(. , n = 25) 
  
  n1000 <- N_df[order(N_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(N == 1000) %>%
    slice_head(. , n = 25) 
  
  n10000 <- N_df[order(N_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(N == 10000) %>%
    slice_head(. , n = 25) 
  
  top25_n <- bind_rows(n100, n200, n500, n1000, n10000)
  
  #TOP 25 Parametro Cantidad_Ideas
  
  I5 <- ideas_df[order(ideas_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(cantidad_ideas == 5) %>%
    slice_head(. , n = 25) 
  
  I10 <- ideas_df[order(ideas_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(cantidad_ideas == 10) %>%
    slice_head(. , n = 25) 
  
  I15 <- ideas_df[order(ideas_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(cantidad_ideas == 15) %>%
    slice_head(. , n = 25) 
  
  top25_cantidad_ideas <- bind_rows(I5, I10, I15)
  #TOP 25 Parametro Cantidad_Votos
  
  V1 <- votos_df[order(votos_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(cantidad_votos == 1) %>%
    slice_head(. , n = 25) 
  
  V3 <- votos_df[order(votos_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(cantidad_votos == 3) %>%
    slice_head(. , n = 25) 
  
  V5 <- votos_df[order(votos_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(cantidad_votos == 5) %>%
    slice_head(. , n = 25) 
  
  top25_cantidad_votos <- bind_rows(V1, V3, V5)
  
  #Top 25 Negativos 
  neg1 <- Negativos_df_part1[order(Negativos_df_part1$ratio_votos_vis, decreasing = T), ] %>%
    slice_head(. , n = 25) 
  
  neg0 <- Negativos_df_part2[order(Negativos_df_part2$ratio_votos_vis, decreasing = T), ] %>%
    slice_head(. , n = 25) 
  
  top25_negativos <- bind_rows(neg1, neg0)
  
  #Top 25 Beta
  B62 <- Beta_df[order(Beta_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(Beta == "6-2") %>%
    slice_head(. , n = 25) 
  
  B44 <- Beta_df[order(Beta_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(Beta == "4-4") %>%
    slice_head(. , n = 25) 
  
  B26 <- Beta_df[order(Beta_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(Beta == "2-6") %>%
    slice_head(. , n = 25) 
  
  B10 <- Beta_df[order(Beta_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(Beta == "1-0") %>%
    slice_head(. , n = 25) 
  
  top25_beta <- bind_rows(B62, B44, B26, B10)
  
  #Top 25 Algoritmo
  
  A50 <- Algoritmo_df[order(Algoritmo_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(Algoritmo == 0.5) %>%
    slice_head(. , n = 25) 
  
  A25 <- Algoritmo_df[order(Algoritmo_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(Algoritmo == 0.25) %>%
    slice_head(. , n = 25) 
  
  A75 <- Algoritmo_df[order(Algoritmo_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(Algoritmo == 0.75) %>%
    slice_head(. , n = 25) 
  
  A1 <- Algoritmo_df[order(Algoritmo_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(Algoritmo == 1) %>%
    slice_head(. , n = 25) 
  
  A0 <- Algoritmo_df[order(Algoritmo_df$ratio_votos_vis, decreasing = T), ] %>%
    filter(Algoritmo == 0) %>%
    slice_head(. , n = 25) 
  
  
  top25_algoritmo <- bind_rows(A50, A25, A75, A1, A0)
  
  #11 Distribucion de rates de las 25 ideas con mejor rate
  
  plot_11a <- ggplot(top25_cantidad_votos, aes(x = ratio_votos_vis, y = after_stat(density) ,col =cantidad_votos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="Votos disponibles")) +
    labs( 
      x= "Ratio votos/visualizaciones", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_11b <- ggplot(top25_n, aes(x = ratio_votos_vis, y = after_stat(density) ,col =N)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="n")) +
    labs( 
      x= "Ratio votos/visualizaciones", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_11c <- ggplot(top25_cantidad_ideas, aes(x = ratio_votos_vis, y = after_stat(density) ,col =cantidad_ideas)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="Ideas visualizadas (p/par.)")) +
    labs( 
      x= "Ratio votos/visualizaciones", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_11d <- ggplot(top25_negativos, aes(x = ratio_votos_vis, y = after_stat(density) ,col =Negativos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="Votos negativos")) +
    labs( 
      x= "Ratio votos/visualizaciones", 
      y = "Densidad", fill = 'Votos Negativos') +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_11e <- ggplot(top25_beta, aes(x = ratio_votos_vis, y = after_stat(density) ,col =Beta)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    labs( 
      x= "Ratio votos/visualizaciones", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_11f <- ggplot(top25_algoritmo, aes(x = ratio_votos_vis, y = after_stat(density) ,col =Algoritmo)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    labs( 
      x= "Ratio votos/visualizaciones", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  
  #12 Distribucion de visualizaciones de las 25 ideas con mejor rate
  
  plot_12a <- ggplot(top25_cantidad_votos, aes(x = log10(visualizaciones), y = after_stat(density) , col =cantidad_votos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="Votos disponibles")) +
    labs( 
      x= "visualizaciones (log10)", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_12b <- ggplot(top25_n, aes(x = log10(visualizaciones), y = after_stat(density) , col =N)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="n")) +
    labs( 
      x= "visualizaciones (log10)", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_12c <- ggplot(top25_cantidad_ideas, aes(x = log10(visualizaciones), y = after_stat(density) , col =cantidad_ideas)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="Ideas visualizadas (p/par.)")) +
    labs( 
      x= "visualizaciones (log10)", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_12d <- ggplot(top25_negativos, aes(x = log10(visualizaciones), y = after_stat(density) , col =Negativos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="Votos negativos")) +
    labs( 
      x= "visualizaciones (log10)", 
      y = "Densidad", fill = 'Votos Negativos') +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_12e <- ggplot(top25_beta, aes(x = log10(visualizaciones), y = after_stat(density) , col =Beta)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    labs( 
      x= "visualizaciones (log10)", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_12f <- ggplot(top25_algoritmo, aes(x = log10(visualizaciones), y = after_stat(density) , col =Algoritmo)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    labs( 
      x= "visualizaciones (log10)", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  #13 Distribucion de votos de las 25 ideas con mejor rate
  
  plot_13a <- ggplot(top25_cantidad_votos, aes(x = V_pos, y = after_stat(density) , col =cantidad_votos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="Votos disponibles")) +
    labs( 
      x= "Cantidad de votos positivos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_13b <- ggplot(top25_n, aes(x = V_pos, y = after_stat(density) , col =N)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="n")) +
    labs( 
      x= "Cantidad de votos positivos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_13c <- ggplot(top25_cantidad_ideas, aes(x = V_pos, y = after_stat(density) , col =cantidad_ideas)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="Ideas visualizadas (p/par.)")) +
    labs( 
      x= "Cantidad de votos positivos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_13d <- ggplot(top25_negativos, aes(x = V_pos, y = after_stat(density) , col =Negativos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="Votos negativos")) +
    labs( 
      x= "Cantidad de votos positivos", 
      y = "Densidad", fill = 'Votos Negativos') +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_13e <- ggplot(top25_beta, aes(x = V_pos, y = after_stat(density) , col =Beta)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    labs( 
      x= "Cantidad de votos positivos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_13f <- ggplot(top25_algoritmo, aes(x = V_pos, y = after_stat(density) , col =Algoritmo)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    labs( 
      x= "Cantidad de votos positivos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_13g <- ggplot(top25_cantidad_votos, aes(x = V_neg, y = after_stat(density) , col =cantidad_votos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    labs(
      x= "Cantidad de votos negativos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_13h <- ggplot(top25_n, aes(x = V_neg, y = after_stat(density) , col =N)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="n")) +
    labs( 
      x= "Cantidad de votos negativos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_13i <- ggplot(top25_cantidad_ideas, aes(x = V_neg, y = after_stat(density) , col =cantidad_ideas)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="Ideas visualizadas (p/par.)")) +
    labs(
      x= "Cantidad de votos negativos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_13j <- ggplot(top25_negativos, aes(x = V_neg, y = after_stat(density) , col =Negativos)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    guides(color=guide_legend(title="Votos negativos")) +
    labs(
      x= "Cantidad de votos negativos", 
      y = "Densidad", fill = 'Votos Negativos') +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_13k <- ggplot(top25_beta, aes(x = V_neg, y = after_stat(density) , col =Beta)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    labs(
      x= "Cantidad de votos negativos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  
  plot_13l <- ggplot(top25_algoritmo, aes(x = V_neg, y = after_stat(density) , col =Algoritmo)) + 
    geom_density(position = 'identity', linetype = 1, lwd = 0.70) +
    labs(
      x= "Cantidad de votos negativos", 
      y = "Densidad") +
    ggthemes::theme_clean() + theme(legend.title = element_text(size = 10), legend.position = c(0.9,0.7), legend.title.align = 0.5)
  ##################
  
  plot_list_1 <- list(plot_1b, plot_2b, plot_3b, plot_4b, plot_5b, plot_6b, plot_7b, plot_8b, plot_9b, plot_10b,
                      plot_11b, plot_12b, plot_13b, plot_13h)
  plot_list_2 <- list(plot_1a, plot_2a, plot_3a, plot_4a, plot_5a, plot_6a, plot_7a, plot_8a, plot_9a, plot_10a,
                      plot_11a, plot_12a, plot_13a, plot_13g)
  plot_list_3 <- list(plot_1c, plot_2c, plot_3c, plot_4c, plot_5c, plot_6c, plot_7c, plot_8c, plot_9c, plot_10c,
                      plot_11c, plot_12c, plot_13c, plot_13i)
  plot_list_4 <- list(plot_1d, plot_2d, plot_3d, plot_4d, plot_5d, plot_6d, plot_7d, plot_8d, plot_9d, plot_10d,
                      plot_11d, plot_12d, plot_13d, plot_13j)
  plot_list_5 <- list(plot_1e, plot_2e, plot_3e, plot_4e, plot_5e, plot_6e, plot_7e, plot_8e, plot_9e, plot_10e,
                      plot_11e, plot_12e, plot_13e, plot_13k)
  plot_list_6 <- list(plot_1f, plot_2f, plot_3f, plot_4f, plot_5f, plot_6f, plot_7f, plot_8f, plot_9f, plot_10f,
                      plot_11f, plot_12f, plot_13f, plot_13l)
  
  ########
  #Ordenamiento de los graficos en una pagina con ggpubr 
  
  ggarrange(plot_1b, plot_2b, plot_3b, plot_4b, plot_5b, plot_6b, plot_7b, plot_8b, plot_9b, plot_10b,
            plot_11b, plot_12b, plot_13b, plot_13h, nrow = 7, ncol = 2, labels = 'auto')
  
  ggsave("Graficos_conjunto_N.png",  height = 80, width = 30, units = 'cm')
  
  ggarrange(plot_1a, plot_2a, plot_3a, plot_4a, plot_5a, plot_6a, plot_7a, plot_8a, plot_9a, plot_10a,
            plot_11a, plot_12a, plot_13a, plot_13g, nrow = 7, ncol = 2, labels = 'auto')
  
  ggsave("Graficos_conjunto_cantidad_votos.png",  height = 80, width = 30, units = 'cm')
  
  ggarrange(plot_1c, plot_2c, plot_3c, plot_4c, plot_5c, plot_6c, plot_7c, plot_8c, plot_9c, plot_10c,
            plot_11c, plot_12c, plot_13c, plot_13i, nrow = 7, ncol = 2, labels = 'auto')
  
  ggsave("Graficos_conjunto_cantidad_ideas.png",  height = 80, width = 30, units = 'cm')
  
  ggarrange(plot_1d, plot_2d, plot_3d, plot_4d, plot_5d, plot_6d, plot_7d, plot_8d, plot_9d, plot_10d,
            plot_11d, plot_12d, plot_13d, plot_13j, nrow = 7, ncol = 2, labels = 'auto')
  
  ggsave("Graficos_conjunto_Negativos.png",  height = 80, width = 30, units = 'cm')
  
  ggarrange(plot_1e, plot_2e, plot_3e, plot_4e, plot_5e, plot_6e, plot_7e, plot_8e, plot_9e, plot_10e,
            plot_11e, plot_12e, plot_13e, plot_13k, nrow = 7, ncol = 2, labels = 'auto')
  
  ggsave("Graficos_conjunto_Beta.png",  height = 80, width = 30, units = 'cm')
  
  ggarrange(plot_1f, plot_2f, plot_3f, plot_4f, plot_5f, plot_6f, plot_7f, plot_8f, plot_9f, plot_10f,
            plot_11f, plot_12f, plot_13f, plot_13l, nrow = 7, ncol = 2, labels = 'auto') 
  
  ggsave("Graficos_conjunto_Algoritmo.png",  height = 80, width = 30, units = 'cm')
  
  
  #####Ordenamiento presentacion paper
  
  #Figura 1
  #Figura 1 DIST VIS.
  
  caption_1 <- paste('Figura 1. Variaciones en la distribución de la cantidad de visualizaciones en los distintos conjuntos de simulaciones.',
                     'Los valores del eje x fueron transformados a logaritmo base 10 (log10) para obtener mayor una resolución de la distribución.',
                     'Dado que en la base de datos original se observaron filas con valor 0 (en visualizaciones y/o votos), se aumento en 1 el valor original de',
                     'los valores de las filas de visualizaciones, votos positivos y votos negativos, a fin de transformar el eje x a log10.',
                     'Los diferentes colores se corresponden con los diferentes conjuntos de simulaciones, habiéndose variado diferentes parámetros en cada uno.'
                     ,sep = '\n')
  
  ggarrange(plot_1a, plot_1b, plot_1c, plot_1d, plot_1e, plot_1f, nrow= 3, ncol = 2, labels = 'auto') %>%
    annotate_figure(fig.lab = "Figura 1", fig.lab.face = "italic", fig.lab.size = 20,
                    top = text_grob('Distribución de visualizaciones', size = 24, face = 'italic'),
                    bottom = text_grob(caption_1, just = 'center'))
  
  ggsave("Figura1.png",  height = 30, width = 30, units = 'cm', bg = 'white')
  
  #Figura 2 DIST VPOS
  
  caption_2 <- paste('Figura 2. Distribuciones de densidad en la distribución de la cantidad de votos positivos en los distintos conjuntos de simulaciones.',
                     'A diferencia de la figura 1, se utiliza la densidad sobre la frecuencia de observaciones.',
                     'A excepción del gráfico f, no se observan variaciones entre los diferentes conjuntos.',
                     sep = '\n')
  
  ggarrange(plot_2a, plot_2b, plot_2c, plot_2d,
            plot_2e, plot_2f, nrow= 3, ncol = 2, labels = 'auto') %>%
    annotate_figure(fig.lab = "Figura 2", fig.lab.face = "italic", fig.lab.size = 20,
                    top = text_grob('Distribución de votos positivos', size = 24, face = 'italic'),
                    bottom = text_grob(caption_2, just = 'center'))
  
  ggsave("Figura2.png", height = 30, width = 30, units = 'cm', bg = 'white')
  
  #Figura 3 DIST. VNEG
  
  caption_3 <- paste('Figura 3. Distribuciones de densidad utilizada para graficar la distribución de la cantidad de votos negativos en los distintos conjuntos de simulaciones.')
  
  ggarrange(plot_3a, plot_3b, plot_3c, plot_3d, plot_3e, plot_3f, nrow= 3, ncol = 2, labels = 'auto') %>%
    annotate_figure(fig.lab = "Figura 3", fig.lab.face = "italic", fig.lab.size = 20,
                    top = text_grob('Distribución de votos negativos', size = 24, face = 'italic'),
                    bottom = text_grob(caption_3, just = 'center'))
  
  ggsave("Figura3.png", height = 30, width = 30, units = 'cm', bg = 'white')
  
  #Figura 4
  #Figura 4 DIST. RATES
  
  caption_4 <- paste('Figura 4. Distribuciones de densidad donde se observan variaciones en los ratios de los distintos conjuntos de simulaciones.',
                     'Los valores del eje y fueron transformados a log10 para obtener mayor una resolución de la distribución.', sep = '\n')
  
  ggarrange(plot_7a, plot_7b, plot_7c, plot_7d, plot_7e, plot_7f, nrow = 3, ncol = 2, labels = 'auto') %>%
    annotate_figure(fig.lab = "Figura 4", fig.lab.face = "italic", fig.lab.size = 20,
                    top = text_grob('Distribución de ratio', size = 24, face = 'italic'),
                    bottom = text_grob(caption_4, just = 'center'))
  
  ggsave("Figura4.png",  height = 30, width = 30, units = 'cm', bg = 'white')
  
  #Figura 5 RATES X VISUALIZACIONES
  
  caption_5 <- paste('Figura 5. Heatmap donde se mide la frecuencia de los valores de las visualizaciones a través de los distintos conjuntos de simulaciones.',
                     'Los puntos con colores más brillantes indican una mayor concentración de observaciones.',
                     sep = '\n')
  
  ggarrange(plot_9a, plot_9b, plot_9c, plot_9d,
            plot_9e, plot_9f, nrow = 3, ncol = 2, labels = 'auto') %>%
    annotate_figure(fig.lab = "Figura 5", fig.lab.face = "italic", fig.lab.size = 20,
                    top = text_grob('Densidad de visualizaciones por ratio', size = 24, face = 'italic'),
                    bottom = text_grob(caption_5, just = 'center'))
  
  ggsave("Figura5.png", height = 30, width = 30, units = 'cm', bg = 'white')
  
  
  
  #Figura 6 TOP VIS
  
  caption_6 <- paste('Figura 6. Distribuciones de densidad de las visualizaciones de las mejores 25 ideas de cada conjunto de simulaciones.')
  
  ggarrange(plot_12a, plot_12b, plot_12c, plot_12d,
            plot_12e, plot_12f, nrow = 3, ncol = 2, labels = 'auto')%>%
    annotate_figure(fig.lab ="Figura 6", fig.lab.face = "italic", fig.lab.size = 20,
                    top = text_grob('Distribución de visualizaciones de las mejores 25 ideas', size = 24, face = 'italic'),
                    bottom = text_grob(caption_6, just = 'center'))
  
  ggsave("Figura6.png",  height = 30, width = 30, units = 'cm', bg = 'white')
  
  
  #Figura 7 TOP VPOS
  
  caption_7 <- paste('Figura 7. Distribuciones de densidad de los votos positivos de las mejores 25 ideas de cada conjunto de simulaciones.')
  
  ggarrange(plot_13a, plot_13b, plot_13c, plot_13d, plot_13e, plot_13f, nrow = 3, ncol = 2, labels = 'auto')%>%
    annotate_figure(fig.lab = "Figura 7", fig.lab.face = "italic", fig.lab.size = 20,
                    top = text_grob('Distribución de votos positivos de las mejores 25 opiniones', size = 24),
                    bottom = text_grob(caption_7, just = 'center'))
  
  ggsave("Figura7.png",  height = 30, width = 30, units = 'cm', bg = 'white')
  
  #Figura 8 TOP VNEG
  
  caption_8 <- paste('Figura 8. Distribuciones de densidad los votos negativos de las mejores 25 ideas de cada conjunto de simulaciones.')
  
  ggarrange(plot_13g, plot_13h, plot_13i, plot_13j, plot_13k, plot_13l, nrow = 3, ncol = 2, labels = 'auto')%>%
    annotate_figure(fig.lab = "Figura 8", fig.lab.face = "italic", fig.lab.size = 20,
                    top = text_grob('Distribución de votos negativos de las mejores 25 opiniones', size = 24),
                    bottom = text_grob(caption_8, just = 'center'))
  
  ggsave("Figura8.png",  height = 30, width = 30, units = 'cm', bg = 'white')
  
  #Figura 9 TOP RATES
  
  caption_9 <- paste('Figura 9. Distribuciones de densidad de las variaciones de ratio contra visualizaciones de las mejores 25 ideas de los distintos parámetros',sep = '\n')
  
  ggarrange(plot_11a, plot_11b, plot_11c, plot_11d, plot_11e, plot_11f, nrow = 3, ncol = 2, labels = 'auto')%>%
    annotate_figure(fig.lab = "Figura 9", fig.lab.face = "italic", fig.lab.size = 20,
                    top = text_grob('Distribución de ratio de las mejores 25 ideas', size = 24, face = 'italic'),
                    bottom = text_grob(caption_9, just = 'center'))
  
  ggsave("Figura9.png",  height = 30, width = 30, units = 'cm', bg = 'white')
  
  #devuelve una lista con listas de graficos
  return(list(
    "N" = plot_list_1,
    "cantidad_votos" = plot_list_2,
    "cantidad_ideas" = plot_list_3,
    "Negativos" = plot_list_4,
    "Beta" = plot_list_5, 
    "Algoritmo" = plot_list_6
  ))
}


######
