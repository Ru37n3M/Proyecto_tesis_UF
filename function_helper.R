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

#voting_loop_Oi_A(X, sam_k, tail_n)
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





