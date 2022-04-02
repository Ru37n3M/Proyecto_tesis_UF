#df_I_n_list() 
#####
#DESCRIPCION
#Crea lista con dos dataframes (df_I y df_n) 

#USO
#df_I_n_list(X)

#Argumentos
#X    funcion de distribucion
#####
df_I_n_list <- function(X){
  
  I_n <- X
  
  #Dataframe para cargar datos sobre visualizaciones y votos de ideas
  df_I <- data.frame (
    "ID" = c(1:length(I_n)),
    "valor" = I_n, #valor de la idea
    "visualizaciones" = 0, #numero de visualizaciones
    "V_pos" = 0, #numero de votos positivos
    "V_neg" = 0  #numero de votos negativos
  )  
  #Dataframe para cargar datos sobre los participantes
  df_n <- data.frame(
    "ID" = c(1:length(I_n)), #ID del participante
    "valor" = I_n, #valor de la idea del participante
    "vote_status" = F #booleano que indica si voto 
  )
  
  list_df_In<- list(df_I, df_n) #lista con ambos dataframes
  
  return(list_df_In)
}
#Ejemplo
df_I_n_list(rnorm(100,0,1))


#voting_loop_base() 
#####
#DESCRIPCION
#loop de repeticion que modifica los dfs producidos por df_I_n_list()

#USO
#voting_loop_base(sam_k)[]

#Argumentos
#sam_k  numero de observaciones de df_I$ID que seran sampleadas detro de la funcion
#X      funcion df_I_n_list(X)
#####
voting_loop_base <- function(X, sam_k){
  
  vl_df <- X 
  
  #se separan los dos dfs de la lista
  df_I <- as.data.frame(vl_df[1]) 
  df_n <- as.data.frame(vl_df[2])
  
  repeat {
    
    k <- sample( df_I$ID, sam_k ) #cantidad de I que el participante puede ver
    
    par <- sample(which(df_n$vote_status == F), 1) #orden de entrada secuencial de participantes (1 en 1)
    
    df_n$vote_status[df_n$ID%in%par] <-  T #vote_Status cambia a TRUE para el ID que salio sampleado en par
    
    df_I$visualizaciones[which(df_I$ID%in%k)] <- df_I$visualizaciones[which(df_I$ID%in%k)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
    # en k
    
    dist <- ( df_n$valor[par] - df_I$valor[k])  #Se calcula la distancia de K con respecto al valor del participante
    #se suma 1 cada valor del vector para evitar la aparicion de valores negativos y usar
    #min como evaluador logico 
    #agente ve k. evaluacion logica para ver cual k tiene menor distancia con On
    
    v_pos <-  which( df_I$ID%in%k[which.min ( abs ( dist ) )] ) #se toma el valor mas cercano a 0 
    #en relacion a la distancia con el valor participante
    v_neg <- which ( df_I$ID%in%k[which.max ( abs ( dist ) )] ) #se toma el valor maximo 
    #en relacion a la distancia con el valor participante
    #se usa abs para evitar confusion con numeros negativos
    
    df_I$V_pos[v_pos] <- df_I$V_pos[v_pos] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
    df_I$V_neg[v_neg] <- df_I$V_neg[v_neg] + 1 #se suma 1 punto a la idea que se corresponde con el valor maximo
    
    #el loop va a frenar si todos los booleanos de df_n$vote_status son TRUE
    if(all(df_n$vote_status) == T)
      
      break
    
  }
  
  mod_list_In <- list(df_I, df_n) #ambos dfs modificados se vuelven a colocar dentro de una lista
  
  return(mod_list_In)
}

#Ejemplo
voting_loop_base(df_I_n_list(rnorm(100,0,1)),5)[1]

#voting_loop_A() 
######
#DESCRIPCION
#loop de repeticion que modifica los dfs producidos por df_I_n_list() en base al criterio A
#Criterio A: Se van a cargar las ideas que menos "visualizaciones" tengan.  
#El fin de esto es que todas las ideas se muestran la misma cantidad de veces.

#USO
#voting_loop_A(X, sam_k, tail_n)[]
#sam_k debe ser menor a tail_k

#ARGUMENTOS
#sam_k  numero de observaciones de df_I$ID que seran sampleadas detro de la funcion
#tail_n numero de observaciones que seran extraidas de los ultimos valores de df_I reordenado 
#       en orden decreciente para visualizaciones. 
#X      funcion df_I_n_list(X)
#####
voting_loop_A <- function(X, sam_k, tail_n){
  
  vl_df <- X 
  
  #se separan los dos dfs de la lista
  df_I <- as.data.frame(vl_df[1]) 
  df_n <- as.data.frame(vl_df[2])
  
  counter <- 0 #seteo de contador de repeticiones
  
  repeat {
    
    counter <- counter + 1 #contador de repeticiones para indicar cambio en if else
    
    if (counter == 1){
      k <- sample( df_I$ID, sam_k ) 
    }
    #aca quiero que las k que no tengan visualizaciones luego de 1 ronda, tengan más prob de ser
    #sampleadas en k
    else { 
      #creo df ordenado por visualizaciones en orden decreciente
      rearranged_df_vis<- order(df_I$visualizaciones, na.last = F, decreasing = T)
      #guardo un vector con los ultimos 10 valores del df, son los que menos vis. tienen
      last_I <- tail(rearranged_df_vis, tail_n)
      #elijo 5 valores al azar de los 10 ultimos valores del df
      k <- sample(last_I, sam_k)
    } #cantidad de I que el participante puede ver
    
    par <- sample(which(df_n$vote_status == F), 1) #orden de entrada secuencial de participantes (1 en 1)
    
    df_n$vote_status[df_n$ID%in%par] <-  T #vote_Status cambia a TRUE para el ID que salio sampleado en par
    
    df_I$visualizaciones[which(df_I$ID%in%k)] <- df_I$visualizaciones[which(df_I$ID%in%k)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
    # en k
    
    dist <- ( df_n$valor[par] - df_I$valor[k])  #Se calcula la distancia de K con respecto al valor del participante
    #se suma 1 cada valor del vector para evitar la aparicion de valores negativos y usar
    #min como evaluador logico 
    #agente ve k. evaluacion logica para ver cual k tiene menor distancia con On
    
    v_pos <-  which( df_I$ID%in%k[which.min ( abs ( dist ) )] ) #se toma el valor más cercano a 0 
    #en relacion a la distancia con el valor participante
    v_neg <- which ( df_I$ID%in%k[which.max ( abs ( dist ) )] ) #se toma el valor maximo 
    #en relacion a la distancia con el valor participante
    #se usa abs para evitar confusion con numeros negativos
    
    df_I$V_pos[v_pos] <- df_I$V_pos[v_pos] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
    df_I$V_neg[v_neg] <- df_I$V_neg[v_neg] + 1 #se suma 1 punto a la idea que se corresponde con el valor maximo
    
    #el loop va a frenar si todos los booleanos de df_n$vote_status son TRUE
    if(all(df_n$vote_status) == T)
      
      break
    
  }
  
  mod_list_In <- list(df_I, df_n)
  
  return(mod_list_In)
}

#Ejemplo 
voting_loop_A(df_I_n_list(rnorm(100,0,1)),5,10)[1]

#voting_loop_Oi
#####
#Descripcion
#funcion que utliza X para generar un df (df_n), luego utiliza los valores de df_n para generar otro 
#df (df_I), al cual se le agregan filas a medida que pasan repeticiones dentro del repeat loop.

#USO
#voting_loop_Oi(X,sam_k)

#ARGUMENTOS
#X       funcion de distribucion 
#sam_k   numero de observaciones que seran sampleadas del df de ideas (df_I) detro de la funcion
#####
voting_loop_Oi<- function(X,sam_k){
  
  I_n <- X #se guarda la dist. en una variable
  
  #Dataframe para cargar datos sobre los participantes
  df_n <- data.frame(
    "ID" = c(1:length(I_n)), #ID del participante
    "valor" = I_n, #valor de la idea del participante
    "vote_status" = F, #booleano que indica si voto u opino
    "opinion_status" = F
  )
  df_I <- data.frame(NULL)
  
  repeat {
    
    par <- sample(which(df_n$vote_status == F), 1) #orden de entrada secuencial de participantes (1 en 1)
      
    if (df_n$opinion_status[df_n$ID%in%par] == F){
      Oi <- data.frame("ID" = df_n$ID[df_n$ID%in%par], 
                       "valor"=df_n$valor[df_n$ID%in%par],
                       "visualizaciones"=0, 
                       "V_pos"=0,
                       "V_neg" = 0) #opinion propia del participante = valor participante
      
      
      df_I <- rbind(df_I,Oi) #se arma el df a partir de las Oi
      
      df_n$opinion_status[df_n$ID%in%par] <- T #se cambia a TRUE la var opinion_status en df_n
    }
    
    
   if(length(df_I$ID) >= sam_k && df_n$opinion_status[df_n$ID%in%par] == T){
      
      k <- sample( df_I$ID, sam_k ) #cantidad de I que el participante puede ver
      
      df_n$vote_status[df_n$ID%in%par] <-  T #se cambia a TRUE la var vote_status en df_n
      
      df_I$visualizaciones[which(df_I$ID%in%k)] <- df_I$visualizaciones[which(df_I$ID%in%k)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
      # en k
      
      dist <- ( df_n$valor[par] - df_I$valor[k])  #Se calcula la distancia de K con respecto al valor del participante
      #se suma 1 cada valor del vector para evitar la aparicion de valores negativos y usar
      #min como evaluador logico 
      #agente ve k. evaluacion logica para ver cual k tiene menor distancia con On
      
      v_pos <-  which( df_I$ID%in%k[which.min ( abs ( dist ) )] ) #se toma el valor mas cercano a 0 
      #en relacion a la distancia con el valor participante
      v_neg <- which ( df_I$ID%in%k[which.max ( abs ( dist ) )] ) #se toma el valor maximo 
      #en relacion a la distancia con el valor participante
      #se usa abs para evitar confusion con numeros negativos
      
      df_I$V_pos[v_pos] <- df_I$V_pos[v_pos] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
      df_I$V_neg[v_neg] <- df_I$V_neg[v_neg] + 1 #se suma 1 punto a la idea que se corresponde con el valor maximo
      
      
    }
    #el loop va a frenar si todos los booleanos de df_n$vote_status son TRUE
    if(all(df_n$vote_status) == T)
      
      break
    
  }
  mod_list_In <- list(df_I, df_n) #ambos dfs modificados se vuelven a colocar dentro de una lista
  
  return(mod_list_In)#devuelve una lista compuesta dos dfs modificados
}

#Ejemplo
voting_loop_Oi(rnorm(100,0,1),5)[1]

#voting_loop_Oi_A
#####
#Descripcion
#Idem voting_loop_Oi pero utilizando el criterio A

#USO
#voting_loop_Oi_A(X, sam_k, tail_n)

#Argumentos
#X       funcion de distribucion
#sam_k   numero de observaciones que seran sampleadas del dataframe de ideas(df_I) detro de la funcion
#tail_n  numero de observaciones que seran extraidas de los ultimos valores de df_I reordenado 
#       en orden decreciente para visualizaciones. 
#####
voting_loop_Oi_A<- function(X, sam_k, tail_n){
  
  I_n <- X #se guarda la dist. en una variable
  
  #Dataframe para cargar datos sobre los participantes
  df_n <- data.frame(
    "ID" = c(1:length(I_n)), #ID del participante
    "valor" = I_n, #valor de la idea del participante
    "vote_status" = F, #booleano que indica si voto u opino
    "opinion_status" = F
  )
  df_I <- data.frame(NULL)

  repeat {
    
    par <- sample(which(df_n$vote_status == F), 1) #orden de entrada secuencial de participantes (1 en 1)
    
    if (df_n$opinion_status[df_n$ID%in%par] == F){
      Oi <- data.frame("ID" = df_n$ID[df_n$ID%in%par], 
                       "valor"=df_n$valor[df_n$ID%in%par],
                       "visualizaciones"=0, 
                       "V_pos"=0,
                       "V_neg" = 0) #opinion propia del participante = valor participante
      
      
      df_I <- rbind(df_I,Oi) #se arma el df a partir de las Oi
      
      rearranged_df_vis<- order(df_I$visualizaciones, na.last = F, decreasing = T)
      
      df_I <- df_I[rearranged_df_vis,]
      
      
      df_n$opinion_status[df_n$ID%in%par] <- T #se cambia a TRUE la var opinion_status en df_n
    }

      
    #se tienen en cuenta dos condiciones: si el participante ya opino (opinion_status = T)
    #y si la cantidad de valores en el df es mayor o igual a la cantidad de valores que se deben extraer
    #para realizar el sampleo entre los tail_n valores con menor cantidad de visualizaciones
    if(length(df_I$ID) >= tail_n && df_n$opinion_status[df_n$ID%in%par] == T){
      
      
      k <- sample(tail(df_I$ID,tail_n), sam_k)
      
      df_n$vote_status[df_n$ID%in%par] <-  T #se cambia a TRUE la var vote_status en df_n
      
      df_I$visualizaciones[which(df_I$ID%in%k)] <- df_I$visualizaciones[which(df_I$ID%in%k)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
      # en k
      
      dist <- ( df_n$valor[par] - df_I$valor[k])  #Se calcula la distancia de K con respecto al valor del participante
      #se suma 1 cada valor del vector para evitar la aparicion de valores negativos y usar
      #min como evaluador logico 
      #agente ve k. evaluacion logica para ver cual k tiene menor distancia con On
      
      v_pos <-  which( df_I$ID%in%k[which.min ( abs ( dist ) )] ) #se toma el valor mas cercano a 0 
      #en relacion a la distancia con el valor participante
      v_neg <- which ( df_I$ID%in%k[which.max ( abs ( dist ) )] ) #se toma el valor maximo 
      #en relacion a la distancia con el valor participante
      #se usa abs para evitar confusion con numeros negativos
      
      df_I$V_pos[v_pos] <- df_I$V_pos[v_pos] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
      df_I$V_neg[v_neg] <- df_I$V_neg[v_neg] + 1 #se suma 1 punto a la idea que se corresponde con el valor maximo
      
      
    }
    #el loop va a frenar si todos los booleanos de df_n$vote_status son TRUE
    if(all(df_n$vote_status) == T)
      
      break
    
  }
  mod_list_In <- list(df_I, df_n) #ambos dfs modificados se vuelven a colocar dentro de una lista
  
  return(mod_list_In)#devuelve una lista compuesta dos dfs modificados
}
#Ejemplo
voting_loop_Oi_A(rnorm(100,0,1),5,10)[1]

#voting_loop_B
#####
#DESCRIPCION
#loop de repeticion que modifica los dfs producidos por df_I_n_list() en base al criterio A
#Criterio A: Se van a cargar las ideas que menos "visualizaciones" tengan.  
#El fin de esto es que todas las ideas se muestran la misma cantidad de veces.

#USO
#voting_loop_A(X, sam_k, tail_n)[]
#sam_k debe ser menor a tail_k

#ARGUMENTOS
#sam_k  numero de observaciones de df_I$ID que seran sampleadas detro de la funcion
#head_n numero de observaciones que seran extraidas de los ultimos valores de df_I reordenado 
#       en orden decreciente para votos positivos. 
#X      funcion df_I_n_list(X)
#set_counter    numero de repeticiones que deben pasar antes de que se aplique el criterio
#####
voting_loop_B <- function(X, sam_k, head_n, set_counter){
  
  #se guarda la funcion X como variable
  vl_df <- X 
  
  #se separan los dos dfs de la lista dada por X
  df_I <- as.data.frame(vl_df[1]) 
  df_n <- as.data.frame(vl_df[2])
  
  counter <- 0 #seteo de contador de repeticiones
  
  repeat {
    
    counter <- counter + 1 #contador de repeticiones para indicar cambio en if else
    
    if (counter < set_counter){
      k <- sample( df_I$ID, sam_k ) 
    }
    #aca quiero que las k que tengan mas v_pos luego de la set_counter loop, sean
    #sampleadas en k
    else { 
      #creo df ordenado por votos positivos en orden decreciente
      rearranged_df_v_pos<- order(df_I$V_pos, na.last = F, decreasing = T)
      #guardo un vector con los primeros valores del df, son los que mas v_pos. tienen
      top_I <- head(rearranged_df_vis, head_n)
      #elijo 5 valores al azar de los 10 primeros valores del df
      k <- sample(top_I, sam_k)
    } #cantidad de I que el participante puede ver
    
    par <- sample(which(df_n$vote_status == F), 1) #orden de entrada secuencial de participantes (1 en 1)
    
    df_n$vote_status[df_n$ID%in%par] <-  T #vote_Status cambia a TRUE para el ID que salio sampleado en par
    
    df_I$visualizaciones[which(df_I$ID%in%k)] <- df_I$visualizaciones[which(df_I$ID%in%k)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
    # en k
    
    dist <- ( df_n$valor[par] - df_I$valor[k])  #Se calcula la distancia de K con respecto al valor del participante
    #se suma 1 cada valor del vector para evitar la aparicion de valores negativos y usar
    #min como evaluador logico 
    #agente ve k. evaluacion logica para ver cual k tiene menor distancia con On
    
    v_pos <-  which( df_I$ID%in%k[which.min ( abs ( dist ) )] ) #se toma el valor más cercano a 0 
    #en relacion a la distancia con el valor participante
    v_neg <- which ( df_I$ID%in%k[which.max ( abs ( dist ) )] ) #se toma el valor maximo 
    #en relacion a la distancia con el valor participante
    #se usa abs para evitar confusion con numeros negativos
    
    df_I$V_pos[v_pos] <- df_I$V_pos[v_pos] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
    df_I$V_neg[v_neg] <- df_I$V_neg[v_neg] + 1 #se suma 1 punto a la idea que se corresponde con el valor maximo
    
    #el loop va a frenar si todos los booleanos de df_n$vote_status son TRUE
    if(all(df_n$vote_status) == T)
      
      break
    
  }
  
  mod_list_In <- list(df_I, df_n)
  
  return(mod_list_In)
}
#ejemplo
voting_loop_B(df_I_n_list(rnorm(200,0,1)),5,10,80)[1]

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
voting_loop_Oi_BA<- function(X, sam_k, head_n){
  
  I_n <- X #se guarda la dist. en una variable
  
  #Dataframe para cargar datos sobre los participantes
  df_n <- data.frame(
    "ID" = c(1:length(I_n)), #ID del participante
    "valor" = I_n, #valor de la idea del participante
    "vote_status" = F, #booleano que indica si voto u opino
    "opinion_status" = F
  )
  
  df_I <- data.frame(NULL)
  
  repeat {
    
    par <- sample(which(df_n$vote_status == F), 1) #orden de entrada secuencial de participantes (1 en 1)
    
    #si el participante no opino (opinion status = F) se crea un nuevo df de una fila
    if (df_n$opinion_status[df_n$ID%in%par] == F){
      Oi <- data.frame("ID" = df_n$ID[df_n$ID%in%par], 
                       "valor"=df_n$valor[df_n$ID%in%par],
                       "visualizaciones"=0, 
                       "V_pos"=0,
                       "V_neg" = 0,
                       "grado_consenso"= 0) #opinion propia del participante = valor participante
      
      #se une Oi al data frame df_I
      df_I <- rbind(df_I,Oi) #se arma el df a partir de las Oi
      
      #se ordenan las filas de df_I por grado de consenso en orden decreciente
      rearranged_df_v_pos<-order(df_I$grado_consenso, na.last = F, decreasing = T) 
      
      #se reemplaza el df original por el df reordenado
      df_I <- df_I[rearranged_df_v_pos,]
      
      #en este punto, el participante ya opino, entonces
      #se cambia a TRUE la var opinion_status en df_n
      df_n$opinion_status[df_n$ID%in%par] <- T 
    }
    
    
    #se tienen en cuenta dos condiciones: si el participante ya opino (opinion_status = T)
    #y si la cantidad de valores en el df es mayor o igual a la cantidad de valores que se deben extraer
    #para realizar el sampleo entre los head_n valores con mayor cantidad de votos positivos
    if(length(df_I$ID) >= head_n && df_n$opinion_status[df_n$ID%in%par] == T){
      
      k <- sample(head(df_I$ID,head_n), sam_k) #se elijen sam_k valores al azar entre los head_n primeros valores del df
      
      df_n$vote_status[df_n$ID%in%par] <-  T #se cambia a TRUE la var vote_status en df_n
      
      df_I$visualizaciones[which(df_I$ID%in%k)] <- df_I$visualizaciones[which(df_I$ID%in%k)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
      # en k
      
      dist <- ( df_n$valor[par] - df_I$valor[k])  #Se calcula la distancia de K con respecto al valor del participante
      #se suma 1 cada valor del vector para evitar la aparicion de valores negativos y usar
      #min como evaluador logico 
      #agente ve k. evaluacion logica para ver cual k tiene menor distancia con On
      
      v_pos <-  which( df_I$ID%in%k[which.min ( abs ( dist ) )] ) #se toma el valor mas cercano a 0 
      #en relacion a la distancia con el valor participante
      v_neg <- which ( df_I$ID%in%k[which.max ( abs ( dist ) )] ) #se toma el valor maximo 
      #en relacion a la distancia con el valor participante
      #se usa abs para evitar confusion con numeros negativos
      
      df_I$V_pos[v_pos] <- df_I$V_pos[v_pos] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
      df_I$V_neg[v_neg] <- df_I$V_neg[v_neg] + 1 #se suma 1 punto a la idea que se corresponde con el valor maximo
      
      #consenso criterio A
      #los votos positivos de todos los valores de df_I sampleados en k 
      #son divididos por la cantidad de visualizaciones
      df_I$grado_consenso[which(df_I$ID%in%k)] <- df_I$V_pos[which(df_I$ID%in%k)]/df_I$visualizaciones[which(df_I$ID%in%k)]
    }
    #el loop va a frenar si todos los booleanos de df_n$vote_status son TRUE
    if(all(df_n$vote_status) == T)
      
      break
    
  }
  mod_list_In <- list(df_I, df_n) #ambos dfs modificados se vuelven a colocar dentro de una lista
  
  return(mod_list_In)#devuelve una lista compuesta dos dfs modificados
}
#ejemplo
voting_loop_Oi_BA(rnorm(100,0,1),5,10)[1]

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
voting_loop_Oi_BB<- function(X, sam_k, head_n){
  
  I_n <- X #se guarda la dist. en una variable
  
  #Dataframe para cargar datos sobre los participantes
  df_n <- data.frame(
    "ID" = c(1:length(I_n)), #ID del participante
    "valor" = I_n, #valor de la idea del participante
    "vote_status" = F, #booleano que indica si voto u opino
    "opinion_status" = F
  )
  
  df_I <- data.frame(NULL)
  
  repeat {
    
    par <- sample(which(df_n$vote_status == F), 1) #orden de entrada secuencial de participantes (1 en 1)
    
    #si el participante no opino (opinion status = F) se crea un nuevo df de una fila
    if (df_n$opinion_status[df_n$ID%in%par] == F){
      Oi <- data.frame("ID" = df_n$ID[df_n$ID%in%par], 
                       "valor"=df_n$valor[df_n$ID%in%par],
                       "visualizaciones"=0, 
                       "V_pos"=0,
                       "V_neg" = 0,
                       "grado_consenso"= 0) #opinion propia del participante = valor participante
      
      #se une Oi al data frame df_I
      
      df_I <- rbind(df_I,Oi) #se arma el df a partir de las Oi
      
      #se ordenan las filas de df_I por grado de consenso en orden decreciente
      rearranged_df_v_pos<-order(df_I$grado_consenso, na.last = F, decreasing = T) 
      
      #se reemplaza el df original por el df reordenado
      df_I <- df_I[rearranged_df_v_pos,]
      
      #en este punto, el participante ya opino, entonces
      #se cambia a TRUE la var opinion_status en df_n
      df_n$opinion_status[df_n$ID%in%par] <- T 
    }
    
    
    #se tienen en cuenta dos condiciones: si el participante ya opino (opinion_status = T)
    #y si la cantidad de valores en el df es mayor o igual a la cantidad de valores que se deben extraer
    #para realizar el sampleo entre los head_n valores con mayor cantidad de votos positivos
    if(length(df_I$ID) >= head_n && df_n$opinion_status[df_n$ID%in%par] == T){
      
      k <- sample(head(df_I$ID,head_n), sam_k) #se elijen sam_k valores al azar entre los head_n primeros valores del df
      
      df_n$vote_status[df_n$ID%in%par] <-  T #se cambia a TRUE la var vote_status en df_n
      
      df_I$visualizaciones[which(df_I$ID%in%k)] <- df_I$visualizaciones[which(df_I$ID%in%k)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
      # en k
      
      dist <- ( df_n$valor[par] - df_I$valor[k])  #Se calcula la distancia de K con respecto al valor del participante
      #se suma 1 cada valor del vector para evitar la aparicion de valores negativos y usar
      #min como evaluador logico 
      #agente ve k. evaluacion logica para ver cual k tiene menor distancia con On
      
      v_pos <-  which( df_I$ID%in%k[which.min ( abs ( dist ) )] ) #se toma el valor mas cercano a 0 
      #en relacion a la distancia con el valor participante
      v_neg <- which ( df_I$ID%in%k[which.max ( abs ( dist ) )] ) #se toma el valor maximo 
      #en relacion a la distancia con el valor participante
      #se usa abs para evitar confusion con numeros negativos
      
      df_I$V_pos[v_pos] <- df_I$V_pos[v_pos] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
      df_I$V_neg[v_neg] <- df_I$V_neg[v_neg] + 1 #se suma 1 punto a la idea que se corresponde con el valor maximo
      
      #consenso criterio B
      #se restan los votos positivos de todos los valores de df_I sampleados en k con los votos negativos
      #el resultado es dividido por la cantidad de visualizaciones
      df_I$grado_consenso[which(df_I$ID%in%k)] <- (df_I$V_pos[which(df_I$ID%in%k)]-
                                                     df_I$V_neg[which(df_I$ID%in%k)])/df_I$visualizaciones[which(df_I$ID%in%k)]
    }
    #el loop va a frenar si todos los booleanos de df_n$vote_status son TRUE
    if(all(df_n$vote_status) == T)
      
      break
    
  }
  mod_list_In <- list(df_I, df_n) #ambos dfs modificados se vuelven a colocar dentro de una lista
  
  return(mod_list_In)#devuelve una lista compuesta dos dfs modificados
}
#ejemplo
voting_loop_Oi_BB(rnorm(100,0,1),5,10)[1]











