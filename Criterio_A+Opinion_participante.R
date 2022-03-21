n <- rnorm(100, 0, 1) #cantidad de participantes

I <- rnorm(30, 0, 1) #pool de ideas

participantes <- n #copia de n para ser modificada en runtime env de deal()
PARTICIPANTES <- n #copia para restaurar vector "participantes" luego de que fue modificada por deal ()
#se usa en la funcion reset()



#Criterio A + Oi
deal_A_c_Oi <- function() {
  
  df_k <- data.frame (
    "valor" = c(I),
    "visualizaciones" = 0,
    "V_pos" = 0,
    "V_neg" = 0
  )               #dataframe para cargar datos sobre visualizaciones y votos de ideas
  counter <- 0
  repeat { #loop de repeticion
    
    counter <- counter + 1 #contador de rep
    
    par <- participantes[1] #orden de entrada secuencial de participantes (1 en 1)
    
    Oi <- c(participantes[1], 0, 0, 0) #opinion propia del participante = valor participante
    
    df_k <- rbind(Oi, df_k) #incorporamos opinion nueva al df
    
    #Criterio A
    #quiero que en la primera vuelta, k sea elegido al azar
    if (counter == 1){
      k <- sample( I, 5 ) 
    }
    #aca quiero que las k que no tengan visualizaciones luego de 1 ronda, tengan más prob de ser
    #sampleadas en k
    else { 
      #ordeno dataframe de en orden decreciente en rel a la cantidad de visualizaciones
      rearranged_df_vis<- order(df_k$visualizaciones, na.last = F, decreasing = T)
      #guardo un vector con los ultimos 10 valores del df, son los que menos vis. tienen
      last_I <- tail(rearranged_df_vis,10)
      #guardo el dataframe reordenado
      df_k_rearr<- df_k[rearranged_df_vis,]
      #sampleo aleatorio de todos los valores excepto los guardados en el vector last_I
      k_2 <- sample(df_k_rearr$valor[-c(last_I)], 5)
      #creo nuevo vector con valores sampleados aleatoriamente + los ultimos 10 valores del df
      k_2 <- c(k_2, df_k_rearr$valor[last_I])
      #k va a ser producto de un sampleo del vector k_2, el cual contiene 10 valores que 
      #tienen la menor cantidad de visualizaciones contra 5 aleatorios
      k <- sample(k_2, 5)
    }
    
    
    assign("participantes", participantes[-1], envir = globalenv()) #se modifica el vector participantes creado a partir
    #de n, se quita al primer participante del vector para 
    #permitir que entre otro participante
    
    df_k$visualizaciones[which(I%in%k)] <- df_k$visualizaciones[which(I%in%k)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
    # en k
    
    dist <- ( par - k )  #Se calcula la distancia de K con respecto al valor del participante
    #se suma 1 cada valor del vector para evitar la aparicion de valores negativos y usar
    #min como evaluador logico 
    #agente ve k. evaluacion logica para ver cual k tiene menor distancia con On
    
    v_pos <-  which( I%in%k[which.min ( abs ( dist ) )] ) #se toma el valor más cercano a 0 
    #en relacion a la distancia con el valor participante
    v_neg <- which ( I%in%k[which.max ( abs ( dist ) )] ) #se toma el valor maximo 
    #en relacion a la distancia con el valor participante
    #se usa abs para evitar confusion con numeros negativos
    
    prueba <- abs(participantes[1] - k)
    
    prueba2 <- prueba[-which.min(prueba)]
    
    prueba3 <- which.min(prueba2)
    
    df_k$V_pos[v_pos] <- df_k$V_pos[v_pos] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
    df_k$V_pos[prueba3] <- df_k$V_pos[prueba3] + 1
    df_k$V_neg[v_neg] <- df_k$V_neg[v_neg] + 1 #se suma 1 punto a la idea que se corresponde con el valor maximo
    
    if ( is.na(par) ) #el loop va a repetir el codigo de arriba hasta que la condicion is.na sea TRUE
      #va a modificar el dataframe a medida que se descartan participantes
      
      break
    
  }
  
  df_k #retorna dataframe modificado
}

reset <- function() {
  assign( "participantes", PARTICIPANTES[], envir = globalenv() )
}

deal_A_c_Oi()
reset()