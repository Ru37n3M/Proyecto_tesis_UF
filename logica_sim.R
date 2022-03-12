

n <- rnorm(100, 0, 1) #cantidad de participantes

I <- rnorm(30, 0, 1) #pool de ideas

participantes <- n #copia de n para ser modificada en runtime env de deal()
PARTICIPANTES <- n #copia para restaurar vector "participantes" luego de que fue modificada por deal ()
                   #se usa en la funcion reset()

deal <- function() {
  
  df_k <- data.frame (
    "valor" = c(I),
    "visualizaciones" = 0,
    "V_pos" = 0,
    "V_neg" = 0
  )               #dataframe para cargar datos sobre visualizaciones y votos de ideas
  
  repeat { #loop de repeticion
    
    k <- sample( I, 5 ) #cantidad de I que el participante puede ver
    
    par <- participantes[1] #orden de entrada secuencial de participantes (1 en 1)
    
    assign("participantes", participantes[-1], envir = globalenv()) #se modifica el vector participantes creado a partir
    #de n, se quita al primer participante del vector para 
    #permitir que entre otro participante
    
    df_k$visualizaciones[which(I%in%k)] <- df_k$visualizaciones[which(I%in%k)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
    # en k
    
    dist <- ( par - k ) + 1 #Se calcula la distancia de K con respecto al valor del participante
                          #se suma 1 cada valor del vector para evitar la aparicion de valores negativos y usar
                          #min como evaluador logico 
                          #agente ve k. evaluacion logica para ver cual k tiene menor distancia con On
    
    v_pos <-  which( I%in%k[which.min(dist)] ) #se toma el valor minimo 
    #en relacion a la distancia con el valor participante
    v_neg <- which ( I%in%k[which.max(dist)] ) #se toma el valor maximo 
    #en relacion a la distancia con el valor participante
    
    df_k$V_pos[v_pos] <- df_k$V_pos[v_pos] + 1 #se suma 1 punto a la idea que se corresponde con el valor minimo
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

reset () #se resetea el vector participantes 
deal() #retorna dataframe con visualizaciones y votos del total de participantes

