

#voting_loop_Oi_random
#####
#Descripcion
#funcion que utliza X para generar un df (df_n), luego utiliza los valores de df_n para generar otro 
#df (df_I), al cual se le agregan filas a medida que pasan repeticiones dentro del repeat loop.

#USO
#voting_loop_Oi_random(distr,row_size)

#ARGUMENTOS
#distr       funcion de distribucion 
#row_size   numero de observaciones que seran sampleadas del df de ideas (df_I) detro de la funcion
#####
voting_loop_Oi_random <- function(distr, row_size){
  
  df_n <- data.frame(
    "ID" = c(1:nrow(distr)), #ID del participante
    "valor" = distr, #valor de la idea del participante
    "vote_status" = F, #booleano que indica si voto u opino
    "opinion_status" = F
  )
  
  df_I <- data.frame(NULL)
  
  repeat {  
    
    #orden de entrada secuencial de participantes (1 en 1)
    par <- sample_n(df_n[which(df_n$opinion_status == F),], 1) 
    
    #si el participante no opino (opinion status = F) se crea un nuevo df de una fila
    #opinion propia del participante = valor participante
    Oi <- data.frame("ID" = df_n$ID[df_n$ID%in%par$ID], 
                     #grepl es una funcion que permite detectar patrones
                     #pido que detecte las columnas que contengan el string "valor" en par
                     #devuelve booleanos True o False
                     #al poner sum, se convierten en 1 o 0
                     #como sabemos que ID siempre va a ir en la posicion 1, partimos de la posicion 2
                     #y se suma 1 al resultado de sum para mantener las posiciones de las columnas de par
                     "valor"= par[, c(2:sum(grepl("valor", colnames(par)),1))],
                     "visualizaciones"=0, 
                     "V_pos"=0,
                     "V_neg" = 0,
                     "grado_consenso"= 0) 
    
    #se une Oi al data frame df_I
    df_I <- rbind(df_I,Oi) #se arma el df a partir de las Oi
    
    #se ordenan las filas de df_I por grado de consenso en orden decreciente
    rearranged_df_v_pos<-order(df_I$grado_consenso, na.last = F, decreasing = T) 
    
    #se reemplaza el df original por el df reordenado
    df_I <- df_I[rearranged_df_v_pos,]
    
    #en este punto, el participante ya opino, entonces
    #se cambia a TRUE la var opinion_status en df_n
    df_n$opinion_status[df_n$ID%in%par$ID] <- T
    
    #sampleo por filas de df_I, si df_I tiene menor numero de filas que el especificado por row_size (ej. 5), se samplea
    #por la cantidad de filas de df_I. ej. yes = 5, no = nrow(df_I) = 1-4. 
    k_opinion <- sample_n(df_I, ifelse(nrow(df_I) >= row_size, yes = row_size, no = nrow(df_I)), replace = F) 
    
    #se crea un df que contenga solamente los valores de k_opinion
    k2_noid <- k_opinion[, c(2:sum(grepl("valor", colnames(par)),1))]
    
    #con apply restamos la fila de par con cada una de las filas de k2_noid
    distance <- function(x){
      abs(par[, c(2:sum(grepl("valor", colnames(par)),1))]) - abs(x)
    }
    
    #devuelve una lista
    kpar_distdf <- apply(k2_noid,1,distance)
    
    #deshacemos la lista y coercemos como matriz
    kpar_distdf <- matrix(unlist(kpar_distdf), nrow = nrow(k_opinion), byrow = T)
    
    #coercemos la matriz en dataframe
    kpar_distdf <- as.data.frame(kpar_distdf)
    
    #se agregan las medias por cada fila
    kpar_distdf$means <- abs(rowMeans(kpar_distdf))
    
    df_I$visualizaciones[which(df_I$ID%in%k_opinion$ID)] <- df_I$visualizaciones[which(df_I$ID%in%k_opinion$ID)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
    # en k
    
    
    v_pos <- k_opinion$ID[which.min(kpar_distdf$means)] #se toma el valor mas cercano a 0 
    #en relacion a la distancia con el valor participante
    
    v_neg <- k_opinion$ID[which.max(kpar_distdf$means)] #se toma el valor maximo 
    #en relacion a la distancia con el valor participante
    #se usa abs para evitar confusion con numeros negativos
    
    
    df_I$V_pos[which(df_I$ID == v_pos)] <- df_I$V_pos[which(df_I$ID == v_pos)] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
    
    df_I$V_neg[which(df_I$ID == v_neg)] <- df_I$V_neg[which(df_I$ID == v_neg)] + 1 #se suma 1 punto a la idea que se corresponde con el valor maximo
    
    
    
    df_n$vote_status[df_n$ID%in%par$ID] <-  T #se cambia a TRUE la var vote_status en df_n
    
    #consenso criterio A
    #los votos positivos de todos los valores de df_I sampleados en k 
    #son divididos por la cantidad de visualizaciones
    
    
    df_I$grado_consenso[which(df_I$ID%in%k_opinion$ID)] <- df_I$V_pos[which(df_I$ID%in%k_opinion$ID)]/df_I$visualizaciones[which(df_I$ID%in%k_opinion$ID)]
    
    #el loop va a frenar si todos los booleanos de df_n$vote_status son TRUE
    
    if(all(df_n$vote_status) == T)
      break
  }
  
  return(list(df_I,df_n))
}

#Ejemplo
voting_loop_Oi_random(mvdist,30)
#voting_loop_Oi_A
#####
#Descripcion
#Idem voting_loop_Oi pero utilizando el criterio A

#USO
#voting_loop_Oi_A(X, sam_k, tail_n)

#Argumentos
#distr       funcion de distribucion
#row_size   numero de observaciones que seran sampleadas del dataframe de ideas(df_I) detro de la funcion
#tail_n  numero de observaciones que seran extraidas de los ultimos valores de df_I reordenado 
#       en orden decreciente para visualizaciones. 
#####
#NOTA IMPORTANTE: asegurarse que la DISTRIBUCION sea un DATAFRAME
#####
voting_loop_Oi_A<- function(distr, row_size, tail_n){
  
  library(dplyr)
  
  
  #Dataframe para cargar datos sobre los participantes
  df_n <- data.frame(
    "ID" = c(1:nrow(distr)),  #ID del participante
    "valor" = distr, #valor de la idea del participante
    "vote_status" = F, #booleanos que indican si voto u opino
    "opinion_status" = F
  )
  df_I <- data.frame(NULL)
  
  repeat {  
    
    #orden de entrada secuencial de participantes (1 en 1)
    par <- sample_n(df_n[which(df_n$opinion_status == F),], 1) 

    #opinion propia del participante = valor participante
    Oi <- data.frame("ID" = df_n$ID[df_n$ID%in%par$ID], 
                     #grepl es una funcion que permite detectar patrones
                     #pido que detecte las columnas que contengan el string "valor" en par
                     #devuelve booleanos True o False
                     #al poner sum, se convierten en 1 o 0
                     #como sabemos que ID siempre va a ir en la posicion 1, partimos de la posicion 2
                     #y se suma 1 al resultado de sum para mantener las posiciones de las columnas de par
                     "valor"= par[, c(2:sum(grepl("valor", colnames(par)),1))], 
                     "visualizaciones"=0, 
                     "V_pos"=0,
                     "V_neg" = 0,
                     "grado_consenso"= 0) 
    
    #se une Oi al data frame df_I
    df_I <- rbind(df_I,Oi) #se arma el df a partir de las Oi
    
    #se ordenan las filas de df_I por grado de consenso en orden decreciente
    rearranged_df_v_pos<-order(df_I$grado_consenso, na.last = F, decreasing = T) 
    
    #se reemplaza el df original por el df reordenado
    df_I <- df_I[rearranged_df_v_pos,]
    
    #en este punto, el participante ya opino, entonces
    #se cambia a TRUE la var opinion_status en df_n
    df_n$opinion_status[df_n$ID%in%par$ID] <- T
    
    #sampleo por filas de df_I, si df_I tiene menor numero de filas que el especificado por row_size (ej. 5), se samplea
    #por la cantidad de filas de df_I. ej. yes = 5, no = nrow(df_I) = 1-4. 
    k_opinion <- sample_n(tail(df_I, tail_n), ifelse(nrow(df_I) >= tail_n, yes = row_size, no = nrow(df_I)), replace = F) 
    
    #se crea un df que contenga solamente los valores de k_opinion
    k2_noid <- k_opinion[, c(2:sum(grepl("valor", colnames(par)),1))]
    
    #con apply restamos la fila de par con cada una de las filas de k2_noid
    distance <- function(x){
      abs(par[, c(2:sum(grepl("valor", colnames(par)),1))]) - abs(x)
    }
    
    #devuelve una lista
    kpar_distdf <- apply(k2_noid,1,distance)
    
    #deshacemos la lista y coercemos como matriz
    kpar_distdf <- matrix(unlist(kpar_distdf), nrow = nrow(k_opinion), byrow = T)
    
    #coercemos la matriz en dataframe
    kpar_distdf <- as.data.frame(kpar_distdf)
    
    #se agregan las medias por cada fila
    kpar_distdf$means <- abs(rowMeans(kpar_distdf))
    
    df_I$visualizaciones[which(df_I$ID%in%k_opinion$ID)] <- df_I$visualizaciones[which(df_I$ID%in%k_opinion$ID)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
    # en k
    
    
    v_pos <- k_opinion$ID[which.min(kpar_distdf$means)] #se toma el valor mas cercano a 0 
    #en relacion a la distancia con el valor participante
    
    v_neg <- k_opinion$ID[which.max(kpar_distdf$means)] #se toma el valor maximo 
    #en relacion a la distancia con el valor participante
    #se usa abs para evitar confusion con numeros negativos
    
    
    df_I$V_pos[which(df_I$ID == v_pos)] <- df_I$V_pos[which(df_I$ID == v_pos)] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
    
    df_I$V_neg[which(df_I$ID == v_neg)] <- df_I$V_neg[which(df_I$ID == v_neg)] + 1 #se suma 1 punto a la idea que se corresponde con el valor maximo
    
    
    
    df_n$vote_status[df_n$ID%in%par$ID] <-  T #se cambia a TRUE la var vote_status en df_n
    
    #consenso criterio A
    #los votos positivos de todos los valores de df_I sampleados en k 
    #son divididos por la cantidad de visualizaciones
    
    
    df_I$grado_consenso[which(df_I$ID%in%k_opinion$ID)] <- df_I$V_pos[which(df_I$ID%in%k_opinion$ID)]/df_I$visualizaciones[which(df_I$ID%in%k_opinion$ID)]
    
    #el loop va a frenar si todos los booleanos de df_n$vote_status son TRUE
    
    if(all(df_n$vote_status) == T)
      break
  }
  
  return(list(df_I,df_n))
}
#Ejemplo
voting_loop_Oi_A(mvdist,5,10)
#####

#####
#voting_loop_Oi_BA
######
#Descripcion
#Idem voting_loop_Oi pero utilizando el criterio B de presentacion de ideas 
#y con criterio de consenso A

#USO
#voting_loop_Oi_BA(X, sam_k, head_n)

#Argumentos
#X       funcion de distribucion
#sam_k   numero de observaciones que seran sampleadas del dataframe de ideas(df_I) detro de la funcion
#head_n  numero de observaciones que seran extraidas de los primeros valores de df_I reordenado 
#        en orden decreciente para grado de consenso. 
#####
voting_loop_Oi_BA<- function(distr, row_size, head_n){
  
  library(dplyr)
  
  
  #Dataframe para cargar datos sobre los participantes
  df_n <- data.frame(
    "ID" = c(1:nrow(distr)),  #ID del participante
    "valor" = distr, #valor de la idea del participante
    "vote_status" = F, #booleano que indica si voto u opino
    "opinion_status" = F
  )
  df_I <- data.frame(NULL)
  
  repeat {  
    
    #orden de entrada secuencial de participantes (1 en 1)
    par <- sample_n(df_n[which(df_n$opinion_status == F),], 1) 
    
    #opinion propia del participante = valor participante
    Oi <- data.frame("ID" = df_n$ID[df_n$ID%in%par$ID], 
                     #grepl es una funcion que permite detectar patrones
                     #pido que detecte las columnas que contengan el string "valor" en par
                     #devuelve booleanos True o False
                     #al poner sum, se convierten en 1 o 0
                     #como sabemos que ID siempre va a ir en la posicion 1, partimos de la posicion 2
                     #y se suma 1 al resultado de sum para mantener las posiciones de las columnas de par
                     "valor"= par[, c(2:sum(grepl("valor", colnames(par)),1))],
                     "visualizaciones"=0, 
                     "V_pos"=0,
                     "V_neg" = 0,
                     "grado_consenso"= 0) 
    
    #se une Oi al data frame df_I
    df_I <- rbind(df_I,Oi) #se arma el df a partir de las Oi
    
    #se ordenan las filas de df_I por grado de consenso en orden decreciente
    rearranged_df_v_pos<-order(df_I$grado_consenso, na.last = F, decreasing = T) 
    
    #se reemplaza el df original por el df reordenado
    df_I <- df_I[rearranged_df_v_pos,]
    
    #en este punto, el participante ya opino, entonces
    #se cambia a TRUE la var opinion_status en df_n
    df_n$opinion_status[df_n$ID%in%par$ID] <- T
    
    #sampleo por filas de df_I, si df_I tiene menor numero de filas que el especificado por row_size (ej. 5), se samplea
    #por la cantidad de filas de df_I. ej. yes = 5, no = nrow(df_I) = 1-4. 
    k_opinion <- sample_n(head(df_I, head_n), ifelse(nrow(df_I) >= head_n, yes = row_size, no = nrow(df_I)), replace = F) 
    
    #se crea un df que contenga solamente los valores de k_opinion
    k2_noid <- k_opinion[, c(2:sum(grepl("valor", colnames(par)),1))]
    
    #con apply restamos la fila de par con cada una de las filas de k2_noid
    distance <- function(x){
      abs(par[, c(2:sum(grepl("valor", colnames(par)),1))]) - abs(x)
    }
    
    #devuelve una lista
    kpar_distdf <- apply(k2_noid,1,distance)
    
    #deshacemos la lista y coercemos como matriz
    kpar_distdf <- matrix(unlist(kpar_distdf), nrow = nrow(k_opinion), byrow = T)
    
    #coercemos la matriz en dataframe
    kpar_distdf <- as.data.frame(kpar_distdf)
    
    #se agregan las medias por cada fila
    kpar_distdf$means <- abs(rowMeans(kpar_distdf))
    
    df_I$visualizaciones[which(df_I$ID%in%k_opinion$ID)] <- df_I$visualizaciones[which(df_I$ID%in%k_opinion$ID)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
    # en k
    
    
    v_pos <- k_opinion$ID[which.min(kpar_distdf$means)] #se toma el valor mas cercano a 0 
    #en relacion a la distancia con el valor participante
    
    v_neg <- k_opinion$ID[which.max(kpar_distdf$means)] #se toma el valor maximo 
    #en relacion a la distancia con el valor participante
    #se usa abs para evitar confusion con numeros negativos
    
    
    df_I$V_pos[which(df_I$ID == v_pos)] <- df_I$V_pos[which(df_I$ID == v_pos)] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
    
    df_I$V_neg[which(df_I$ID == v_neg)] <- df_I$V_neg[which(df_I$ID == v_neg)] + 1 #se suma 1 punto a la idea que se corresponde con el valor maximo
    
    
    
    df_n$vote_status[df_n$ID%in%par$ID] <-  T #se cambia a TRUE la var vote_status en df_n
    
    #consenso criterio A
    #los votos positivos de todos los valores de df_I sampleados en k 
    #son divididos por la cantidad de visualizaciones
    
    
    df_I$grado_consenso[which(df_I$ID%in%k_opinion$ID)] <- df_I$V_pos[which(df_I$ID%in%k_opinion$ID)]/df_I$visualizaciones[which(df_I$ID%in%k_opinion$ID)]
    
    #el loop va a frenar si todos los booleanos de df_n$vote_status son TRUE
    
    if(all(df_n$vote_status) == T)
      break
  }
  
  return(list(df_I,df_n))
}
#ejemplo
voting_loop_Oi_BA(mvdist,5,10)

#voting_loop_Oi_BB
######
#Descripcion
#Idem voting_loop_Oi pero utilizando el criterio B de presentacion de ideas 
#y con criterio de consenso B

#USO
#voting_loop_Oi_BB(X, sam_k, head_n)

#Argumentos
#X       funcion de distribucion
#sam_k   numero de observaciones que seran sampleadas del dataframe de ideas(df_I) detro de la funcion
#head_n  numero de observaciones que seran extraidas de los primeros valores de df_I reordenado 
#        en orden decreciente para grado de consenso. 
#####
voting_loop_Oi_BB<- function(distr, row_size, head_n){
  
  library(dplyr)
  
  
  #Dataframe para cargar datos sobre los participantes
  df_n <- data.frame(
    "ID" = c(1:nrow(distr)),  #ID del participante
    "valor" = distr, #valor de la idea del participante
    "vote_status" = F, #booleano que indica si voto u opino
    "opinion_status" = F
  )
  df_I <- data.frame(NULL)
  
  repeat {  
    
    #orden de entrada secuencial de participantes (1 en 1)
    par <- sample_n(df_n[which(df_n$opinion_status == F),], 1) 
    
    #opinion propia del participante = valor participante
    Oi <- data.frame("ID" = df_n$ID[df_n$ID%in%par$ID], 
                     #grepl es una funcion que permite detectar patrones
                     #pido que detecte las columnas que contengan el string "valor" en par
                     #devuelve booleanos True o False
                     #al poner sum, se convierten en 1 o 0
                     #como sabemos que ID siempre va a ir en la posicion 1, partimos de la posicion 2
                     #y se suma 1 al resultado de sum para mantener las posiciones de las columnas de par
                     "valor"= par[, c(2:sum(grepl("valor", colnames(par)),1))],
                     "visualizaciones"=0, 
                     "V_pos"=0,
                     "V_neg" = 0,
                     "grado_consenso"= 0) 
    
    #se une Oi al data frame df_I
    df_I <- rbind(df_I,Oi) #se arma el df a partir de las Oi
    
    #se ordenan las filas de df_I por grado de consenso en orden decreciente
    rearranged_df_v_pos<-order(df_I$grado_consenso, na.last = F, decreasing = T) 
    
    #se reemplaza el df original por el df reordenado
    df_I <- df_I[rearranged_df_v_pos,]
    
    #en este punto, el participante ya opino, entonces
    #se cambia a TRUE la var opinion_status en df_n
    df_n$opinion_status[df_n$ID%in%par$ID] <- T
    
    #sampleo por filas de df_I, si df_I tiene menor numero de filas que el especificado por row_size (ej. 5), se samplea
    #por la cantidad de filas de df_I. ej. yes = 5, no = nrow(df_I) = 1-4. 
    k_opinion <- sample_n(head(df_I, head_n), ifelse(nrow(df_I) >= head_n, yes = row_size, no = nrow(df_I)), replace = F) 
    
    #se crea un df que contenga solamente los valores de k_opinion
    k2_noid <- k_opinion[, c(2:sum(grepl("valor", colnames(par)),1))]
    
    #con apply restamos la fila de par con cada una de las filas de k2_noid
    distance <- function(x){
      abs(par[, c(2:sum(grepl("valor", colnames(par)),1))]) - abs(x)
    }
    
    #devuelve una lista
    kpar_distdf <- apply(k2_noid,1,distance)
    
    #deshacemos la lista y coercemos como matriz
    kpar_distdf <- matrix(unlist(kpar_distdf), nrow = nrow(k_opinion), byrow = T)
    
    #coercemos la matriz en dataframe
    kpar_distdf <- as.data.frame(kpar_distdf)
    
    #se agregan las medias por cada fila
    kpar_distdf$means <- abs(rowMeans(kpar_distdf))
    
    df_I$visualizaciones[which(df_I$ID%in%k_opinion$ID)] <- df_I$visualizaciones[which(df_I$ID%in%k_opinion$ID)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
    # en k
    
    
    v_pos <- k_opinion$ID[which.min(kpar_distdf$means)] #se toma el valor mas cercano a 0 
    #en relacion a la distancia con el valor participante
    
    v_neg <- k_opinion$ID[which.max(kpar_distdf$means)] #se toma el valor maximo 
    #en relacion a la distancia con el valor participante
    #se usa abs para evitar confusion con numeros negativos
    
    
    df_I$V_pos[which(df_I$ID == v_pos)] <- df_I$V_pos[which(df_I$ID == v_pos)] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
    
    df_I$V_neg[which(df_I$ID == v_neg)] <- df_I$V_neg[which(df_I$ID == v_neg)] + 1 #se suma 1 punto a la idea que se corresponde con el valor maximo
    
    
    
    df_n$vote_status[df_n$ID%in%par$ID] <-  T #se cambia a TRUE la var vote_status en df_n
    
    #consenso criterio A
    #los votos positivos de todos los valores de df_I sampleados en k 
    #son divididos por la cantidad de visualizaciones
    
    
    df_I$grado_consenso[which(df_I$ID%in%k_opinion$ID)] <- (df_I$V_pos[which(df_I$ID%in%k_opinion$ID)]-
      df_I$V_neg[which(df_I$ID%in%k_opinion$ID)])/df_I$visualizaciones[which(df_I$ID%in%k_opinion$ID)]
    
    #el loop va a frenar si todos los booleanos de df_n$vote_status son TRUE
    
    if(all(df_n$vote_status) == T)
      break
  }
  
  
  
  return(list(df_I,df_n))
}
#ejemplo
voting_loop_Oi_BB(mvdist,5,10)

#####
#mvfun
#Descripcion
#Mezcla distribuciones univariadas/multivariadas
#ARGUMENTOS 
#sam_size --- cantidad de filas que se samplean de cada ditribución
#stop_at --- sirve como condicional para el repeat loop que tiene incorporado la funcion
#--- marca cuantas filas queremos que la distribucion resultante de la mezcla tenga
#####
mvfun <- function(sam_size, stop_at){
  
  #librerias necesarias para correr el codigo
  library(dplyr)
  
  #dataframe local vacio para incorporar el sampleo de las dist
  s <- NULL
  
  #funcion anonima para correr la funcion con cualquier distribucion
  function(x,y){
    
    #repeat loop para producir un "cara o cruz" y samplear sam_size de cada distribucion
    repeat{
      
      #"cara o cruz" entre 0 y 1
      samres <- sample(c(0,1), 1)
      
      #condicional para 0
      if(samres == 0){
        
        #si es 0, se samplea al azar la distribucion x por filas 
        #las filas sampleadas se agregan al dataframe local s
        s <- rbind.data.frame(s, sample_n(as.data.frame(x),sam_size))
        
      }
      
      #condicional para 1
      else{
        
        #si es 1 se samplea por filas la distribucion y 
        s <- rbind.data.frame(s, sample_n(as.data.frame(y),sam_size))
        
      }
      
      #si el dataframe s llega a un numero determinado de filas (stop_at) se corta el loop
      if (max(row(s)) >= stop_at)
        
        break
      
      
    }
    
    return(s)
  }
  
}

#definir argumentos iniciales (sam_size y stop_at)
mvfun2 <- mvfun(10,500) 
#definir argumentos para la funcion anonima (distribuciones x e y)
library(MASS)
mvdist <- mvfun2(mvrnorm(n = 1000, c(-3, -1, 1.3),diag(1,3,3)), 
                 mvrnorm(n = 1000, c(1, -1.2, -1.9),diag(1,3,3)) )
######

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

#ejemplo
fracsampling(list("1" = data.frame(mvrnorm(n = 200, c(2, 2, 2),diag(1,3,3))),
                  "2" = data.frame(mvrnorm(n = 200, c(-2, -2, -2),diag(1,3,3))),
                  "3" = data.frame(mvrnorm(n = 600, c(0, 0, 0),diag(1,3,3)))))

#####