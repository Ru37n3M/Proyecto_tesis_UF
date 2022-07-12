
#ARGUMENTOS 
#sam_size --- cantidad de filas que se samplean de cada ditribución
#stop_at --- sirve como condicional para el repeat loop que tiene incorporado la funcion
#--- marca cuantas filas queremos que la distribucion resultante de la mezcla tenga
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

library(ggplot2)
ggplot(mvdist, aes(x=V1, y=V2, z= V3))+
  geom_point(alpha = .2) +
  geom_density_2d()+
  theme_bw()

library(MVN)
mvn(mvdist, mvnTest = "energy", univariatePlot = "scatter")
