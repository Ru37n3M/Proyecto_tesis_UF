
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
