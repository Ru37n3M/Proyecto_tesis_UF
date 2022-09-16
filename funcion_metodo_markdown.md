Match metodo-codigo
================

## Mezcla de Multivariadas y generacion de agentes

En esta simulación, los agentes son generados a partir de una mezcla de
3 distribuciones multivariadas. En uno de los casos la distribución va a
estar definida como *N*(0, Σ) En otro de los casos la distribución va a
estar dada por una mezcla de gaussianas multivariadas con 2 componentes
representando 2 grupos de opiniones definidas según *N* (μ<sub>1</sub>,
Σ) y *N*(μ<sub>2</sub>, Σ) donde μ<sub>1</sub> = 0 y μ<sub>1</sub> -
μ<sub>2</sub> indica el grado de polarización de las opiniones.

La funcion mixingfun fue diseñada para generar una mezcla de
distribuciones multivariadas, siendo dislist una lista que cuente con n,
medias y matrices de covarianza de cada distribucion. Los argumentos
beta y total_votos son utilizados en la generacion de la cantidad de
votos positivos y negativos de cada participante. En particular, se
utilzan la distribucion beta como probabilidad de en la generacion de
una distribucion binomial, la cual determina la cantidad de votos
positivos.

``` r
mixingfun <- function(dislist, beta, total_votos){
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
      ),
      Beta = beta,
      Votos_positivos = rbinom(sum(dislist$n), c(1:total_votos), prob = beta)
    ) %>%
    tibble() #Convierto el resultado final en un tibble
  
}


dist <- mixingfun(list("n" = c(200,300,100), # n de cada distribucion
                       "means" = list(c(2,2,2),c(1,1,1),c(0,0,0)), # lista con vectores de medias para cada dist
                       "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))),
                  rbeta(600,4,2),
                  2) # lista con matrices de covarianza
```

Vamos a explicar con mayor detalle cada parte de la funcion…

### Definición de agentes

Los agentes son generados a partir de las funciones pmap y mvrnorm. En
particular, pmap aplica la funcion mvrnorm a los tres elementos que
componen la lista dislist: n, means y cov_mat, generando distribuciones
multivariadas de n cantidad de agentes cada una. El resultado de la
funcion se coerce en dataframe, dado que pmap devuelve una lista, y se
unen las filas de los tres dataset.

``` r
pmap(
    dislist,
    function(n, means, cov_mat) mvrnorm(n, means, cov_mat) #Genero los muestreos de la distribucion multivariada
  ) %>% 
    lapply(as.data.frame) %>%  #Convierte cada matriz de la lista en dataframe
    bind_rows()
```

En el siguiente segmento se observa la creacion de 3 nuevas variables
que seran agregadas al dataset. Siendo Dist una secuencia de 1 a 3
utilizada para identificar la distribucion proveniente de cada vector de
valores (en este caso la longitud de dislist$n, el cual es un vector
compuesto de 3 elementos), siendo cada numero repetido n cantidad de
veces.

Beta son valores extraidos a partir de una distribucion beta.

Finalmente, Votos_positivos esta compuesto por valores extraidos de una
distribucion binomial, definida por n cantidad de participantes, la
cantidad de votos totales disponibles y Beta.

Al final, la funcion devuelve un tibble con n participantes con x
dimensiones cada uno, un valor beta asignado y un numero entero
designando la cantidad de votos positivos disponibles para el
participante.

``` r
mutate( #Creo una nueva variable para indicar de que distribucion proviene originalmente cada valor
      Dist = rep( 
        1:length(dislist$n), dislist$n #Pongo un numero para cada distribucion y lo repito segun cuantos datos hay provenientes de esa distribucion
      ),
      Beta = beta,
      Votos_positivos = rbinom(sum(dislist$n), c(1:total_votos), prob = beta)
    )%>%
    tibble() 
```

Entonces, mixingfun requiere una lista compuesta por n cantidad de
valores de cada distribucion, means siendo una lista con vectores de
medias para cada distribucion y cov_mat siendo una lista compuesta por
matrices de covarianza. Además, requiere una distribucion beta y un
valor entero definiendo la cantidad de votos totales.

``` r
dist <- mixingfun(list("n" = c(200,300,100), # n de cada distribucion
                       "means" = list(c(2,2,2),c(1,1,1),c(0,0,0)), # lista con vectores de medias para cada dist
                       "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                  rbeta(600,4,2),#Distribucion beta, n == sum(list$n)
                  2) #Size de distribucion binomial 
```

## Definiciones de la plataforma

### Algoritmos de seleccion

La selección de opiniones que se le van a mostrar al agente i-ésimo en
una determinada ronda estarán dadas por dos algoritmos de selección de
opiniones que serán manipulados.

#### Primer algoritmo de selección

el primer algoritmo ƒ<sub>1</sub>(*x*) elige *k* ideas seleccionando
*O*<sub>j</sub> cuando *v*(*O*<sub>i</sub>) \< *v*(*O*<sub>j</sub>) o
seleccionando *O*<sub>i</sub> con probabilidad 1/\|*V*\| cuando
*v*(*O*<sub>i</sub>) = *v*(<i>O</i><sub>j</sub>).

Se samplea el pool de ideas por el numero de filas para cambiar de
posición las filas de todo el dataset, de manera que todas las ideas que
poseen la misma cantidad de visualizaciones tengan la misma probabilidad
de ser seleccionadas, es decir, de ser reordenadas dentro de las últimas
k filas ordenadas decrecientemente por visualizaciones.

Como argumentos, se requiere un dataframe, en este caso, el pool de
ideas, y un número entero representando la cantidad de ideas a
seleccionar del dataset.

``` r
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
```

#### Segundo algoritmo de selección

El segundo algoritmo ƒ<sub>2</sub>(<i>x</i>) selecciona *k* / 2
opiniones de *G*<sub>i,r</sub> según el algoritmo anteriormente descrito
y el resto de las opiniones son seleccionadas tomando las *k* / 2
opiniones con el mayor ratio de votos sobre visualizaciones del conjunto
*M*, eligiendo *O*<sub>i</sub> cuando *m*(*O*<sub>i</sub>) \<
*m*(*O*<sub>j</sub>) y eligiendo *O*<sub>i</sub> con probabilidad
1/<i>n</i> cuando *m*(<i>O</i><sub>i</sub>) = *m*(*O*<sub>j</sub>).

Al igual que en el primer algoritmo, el dataframe es “barajado” para
aleatorizar la posicion de las filas. Luego se seleccionan k/2 filas
ubicadas últimas según orden de visualizaciones y k/2 filas ubicadas
primeras según ratio de votos/visualizaciones. Finalmente se unen las
filas de ambos subdataset.

Respecto a los argumentos, al igual que en el primer algoritmo, requiere
un dataframe (O_pool) y un número entero (k).

``` r
algoritmo_seleccion_f2x<- function(O_pool, k){
  
  #Se reordena aleatoriamente el dataframe
  #Se hace para evitar que, en caso de empate de visualizaciones,
  #todos los valores minimos tengan iguales probabilidades de ser seleccionados
  #al subsetear las últimas k filas del dataset
  O_pool <- sample_n(O_pool, nrow(O_pool))
  
  #Se subsetean las últimas k/2 filas del dataset ordenado de forma decreciente segun visualizaciones
  k_opinion_low<- O_pool[order(O_pool$visualizaciones, decreasing = T),] %>%
    slice_tail(n = k/2)
  
  #Se subsetean las primeras k/2 filas del dataset ordenado de forma decreciente segun ratio votos-visualizaciones
  k_opinion_high <- O_pool[order(O_pool$ratio_votos_vis, decreasing = T),] %>%
    slice_head(n = k/2)
  
  #Se combinan las filas de ambos subsets para formar k opinones que seran presentadas al participante
  k_opinion <- bind_rows(k_opinion_low, k_opinion_high)
  
  return(k_opinion)
  
}
```

## Generacion de votacion

La funcion Voting fue diseñada para llevar a cabo el proceso de votacion
por cada agente. Dispone de 6 argumentos: pool_ideas siendo un dataframe
de n cantidad de filas, par siendo un dataframe de una fila (en caso de
ser un solo agente), k siendo un número entero representando la cantidad
de ideas a seleccionar, vpos siendo la cantidad de votos positivos, vneg
siendo la cantidad de votos negativos y k_method siendo el algoritmo de
selección de ideas, pudiendo tomar los valores “A”, “B” o “random”.

``` r
Voting <- function(pool_ideas, par, k, vpos, vneg, k_method = "random"){
  
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
      
      k_opinion <- algoritmo_seleccion_f2x(O_pool, k)
      
    }
    
    else{
      #Sampleo aleatorio
      k_opinion <- sample_n(O_pool, k, replace = F)
    }
    
    O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)] <- O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
    # en 
    
    #k vectores con las dimensiones de k_opinion
    k2_noid <- k_opinion[,which(grepl("Dim", colnames(O_pool)))]
    
    #Distancia entre participante y k
    distance_i_j <- function(x){
      abs(par) - abs(x)
    }
    
    #Matriz de k filas con el resultado de la diferencia
    Dij <- apply(k2_noid,1, distance_i_j)
    
    Dij <- bind_rows(Dij)
    
    #Se pasan a valores absolutos todos los numeros
    Dij <- abs(Dij)
    
    #Inversa de la distancia, se elevan todos los valores a la -1
    sumDij_inverse <- sum(Dij**-1)
    
    #Probabilidad de cada vector de valores de ser elegido en base a la sumatoria de todas las distancias
    Probs_vpos <- function(x){
      (x**-1) / sumDij_inverse
    }
    
    Poij_pos<- sapply(Dij, Probs_vpos)
    
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
    Probs_vneg <- function(x){
      x / sumDij
    }
    
    Poij_neg<- sapply(Dij, Probs_vneg)
    
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
    
    return(O_pool)
    
  }
  
  else{
    return(pool_ideas)
  }
  
}
```

Vamos a explicar la funcion parte por parte…

La funcion check itera sobre cada fila del dataset buscando una fila
cuyos valores sean iguales a los del participante.

Si estos valores estan presentes en el dataset, se obtiene el indice de
la fila en la cual estan ubicados y se elimina del dataset utilizando
idexacion negativa.

``` r
  #Chequea fila por fila si se encuentra la opinion del participante
  check <- function(x){
    x%in%par
  }

  #Si se encuentra la idea del participante, se la elimina del pool de seleccion
  O_pool <- pool_ideas[-which(sapply(pool_ideas, check))[1],] %>%
    as_tibble
```

Este primer condicional permite asegurarnos de que el dataset tiene la
cantidad necesaria de filas para iniciar un sampleo por k filas.

De tener mayor o igual numero de filas que k, se procede a samplear k
filas segun alguno de los dos algoritmos de seleccion disponibles.

Caso contrario, devuelve el dataset sin modificaciones.

``` r
#Condicional para chequear si el pool de ideas tiene por lo menos k filas
  if(nrow(O_pool) >= k){
    ...
  }else
  {
    return(O_pool)
  }
```

Dentro del primer condicional, encontramos el siguiente segmento de
codigo, el cual corresponde a la seleccion de algoritmos de votacion.

El condicional para k_method es seleccionado en los argumentos de la
funcion. El argumento elegido determina el algoritmo de seleccion de
ideas para la votacion subsiguiente.

En caso de no setear un argumento, o de poner una opcion diferente a “A”
o “B”, se realiza un sampleo aleatorio.

``` r
#Condicionales para elegir algoritmos de seleccion
    if(k_method == "A"){
      
      k_opinion <- algoritmo_seleccion_f1x(O_pool, k)
      
    } else if(k_method == "B"){
      
      k_opinion <- algoritmo_seleccion_f2x(O_pool, k)
      
    }else{
      #Sampleo aleatorio
      k_opinion <- sample_n(O_pool, k, replace = F)
    }
```

Se suma 1 a la columna de visualizaciones en el dataset general de ideas
(O_pool) a las k ideas que fueron seleccionadas.

``` r
O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)] <- O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)] + 1 #se suma 1 a la dimension "visualizacion" del df a las ideas I presentes 
```

De las k ideas sorteadas, se extrae solo su valor (Dim1, Dim2 y Dim3).

``` r
#k vectores con las dimensiones de k_opinion
    k2_noid <- k_opinion[,which(grepl("Dim", colnames(O_pool)))]
```

Luego se genera un cálculo de distancias entre la opinion del
participante y los valores de k ideas, correspondiendose a
*D*<sub>i,j</sub>, definido como la distancia absoluta entre dos
opiniones según *D*<sub>i,j</sub> = \|*O*<sub>i</sub> -
*O*<sub>j</sub>\| donde *O*<sub>j</sub> ∈ <i>G</i><sub>i</sub>.

``` r
#Distancia entre participante y k
    distance_i_j <- function(x){
      abs(par) - abs(x)
    }
    
    #Matriz de k filas con el resultado de la diferencia
    Dij <- apply(k2_noid,1, distance_i_j)
    
    Dij <- bind_rows(Dij)
    
    #Se pasan a valores absolutos todos los numeros
    Dij <- abs(Dij)
```

A partir del calculo de la distancia de cada idea en relacion al
participante, definimos la conducta de votación positiva como la
probabilidad de que el agente *a*<sub>i</sub> vote positivamente a la
idea *O*<sub>i,j</sub> lo cual está dado por P(*O*<sub>i,*j*</sub>) =
*D*<sub>i,j</sub><sup>-1</sup> / ∑<sub>j</sub><sup>k</sup>
*D*<sub>i,j</sub><sup>-1</sup>.

Luego de realizar el cálculo de probabilidad, se ejecuta un condicional,
si la cantidad de votos positivos es mayor a 0, se samplean vpos ideas
de las k ideas presentadas con las probabilidades calculadas.

Una vez sampleadas, se obtiene el indice de las ideas sampleadas de k en
el dataset de ideas y se suma 1 al valor de la columna votos_positivos
de la/s fila/s correspondientes.

``` r
    #Inversa de la distancia, se elevan todos los valores a la -1
    sumDij_inverse <- sum(Dij**-1)
    
    #Probabilidad de cada vector de valores de ser elegido en base a la sumatoria de todas las distancias
    Probs_vpos <- function(x){
      (x**-1) / sumDij_inverse
    }
    
    Poij_pos<- sapply(Dij, Probs_vpos)
    
    #Condicional para votos positivos, chequea si el participante tiene votos positivos
    if(vpos > 0){
      
      #se samlea vpos con las probabilidades de cada fila/vector
      Voted_pos <- sample_n(k_opinion, vpos, prob = c(Poij_pos), replace = T)
      
      #se suma 1 punto a la idea que se corresponde con el valor minimo
      O_pool$V_pos[which(O_pool$ID%in%Voted_pos$ID)] <- O_pool$V_pos[which(O_pool$ID%in%Voted_pos$ID)] + 1 
      
    }
```

La conducta de votación negativa funciona de manera análoga con la única
diferencia que no se toma la inversa de la distancia
P(*O*<sub>i,j</sub>) = *D*<sub>i,j</sub> / ∑<sub>j</sub><sup>k</sup>
*D*<sub>i,j</sub>.

Por lo tanto la probabilidad de votar negativamente la idea es
proporcional a la distancia.

De igual manera que con los votos positivos, que se produzca una
votación negativa depende de la cantidad vneg de votos negativos
disponibles. De ser vneg \> 0, se samplean vneg ideas de acuerdo al
cálculo de probabilidad ejecutado para conducta de votación negativa.

Luego, se obtiene el índice de las vneg ideas sampleadas para sumar 1 a
la columna de votos negativos de las filas correspondientes.

``` r
#Sumatoria de todas las distancias
    sumDij <- sum(Dij)
    
    #Probabilidad de voto negativo
    Probs_vneg <- function(x){
      x / sumDij
    }
    
    Poij_neg<- sapply(Dij, Probs_vneg)
    
    #Condicional para votos negativos, chequea si el participante tiene votos negativos
    if(vneg > 0){
      
      #Se samplea vneg de k_opinion con probabilidades de voto negativo para cada vector
      Voted_neg <- sample_n(k_opinion,vneg, prob = c(Poij_neg), replace = T)
      
      #se suma 1 punto a la idea que se corresponde con el valor minimo
      O_pool$V_neg[which(O_pool$ID%in%Voted_neg$ID)] <- O_pool$V_neg[which(O_pool$ID%in%Voted_neg$ID)] + 1 
      
    }
```

El ratio votos/visualizaciones es calculado según *m*(*O*<sub>i</sub>) /
*v*(*O*<sub>i</sub>), siendo *m*(*O*<sub>i</sub>) la cantidad de votos
positivos de la i-ésima opinión o la diferencia entre los votos
positivos y los votos negativos dependiendo de si en una determinada
iteración la plataforma posee votos negativos.

``` r
#Ratio Votos/visualizaciones
    O_pool$ratio_votos_vis[which(O_pool$ID%in%k_opinion$ID)] <- (O_pool$V_pos[which(O_pool$ID%in%k_opinion$ID)]-
                                                                   O_pool$V_neg[which(O_pool$ID%in%k_opinion$ID)])/O_pool$visualizaciones[which(O_pool$ID%in%k_opinion$ID)]
```

### Algoritmo de generacion de opiniones

Opinion_pool es una funcion diseñada para generar el pool de ideas que
posteriormente será utilizado en la función Voting, previamente
descrita. Según el diseño de la plataforma, el participante ingresa su
opinion y luego vota sobre k ideas presentadas. Opinion_pool representa
el proceso en el cual el participante ingresa a la plataforma y sube su
opinión al pool de ideas.

Respecto a los argumentos de la funcion, Opinion_pool requiere un
dataframe dist y un número entero k. Además tiene otros dos argumentos
que poseen valores por default, siendo par_num_iteration la cantidad de
participantes por iteracion y k_method el método de selección de ideas
elegido. El argumento par_num_iteration determina la cantidad de filas
que pueden ser seleccionadas del dataframe dist por iteración para
generar el dataset O_pool. Por el momento, se utilizó solo con el valor
por default.

``` r
Opinion_pool <-function(dist, k, par_num_iteration = 1, 
                        k_method = "random"){
  
  #shuffle_dist: espera los resultados de shuffle_dist
  #k: numero de ideas que cada participante va a ver (numero entero)
  #total_votos: numero entero, cantidad de votos disponibles para el participante
  #par_num_iteration: cantidad de participantes por iteracion
  #k_method: criterio de seleccion de k, puede ser "random" (default), "A" o "B"
  
  library(tidyverse)
  
  
  
  #nuevo dataframe con las dimensiones de shuffled_dist
  par_pool <- dist[,which(grepl("Dim", colnames(dist)) | grepl("Votos", colnames(dist)))]
  
  #Se obtiene la cantidad total de votos para computar cantidad de votos negativos
  total_votos <- max(dist$Votos_positivos)
  
  #dataframe vacio para incorporar resultados del repeat loop
  O_pool <- tibble(NULL)
  
  repeat{
    
    #se selecciona primera fila 
    par_n <- par_pool[c(1:par_num_iteration),]
    
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
    Votos_negativos <- abs(par_n$Votos_positivos - total_votos)
    
    #se guardan los resultados de la funcion Voting
    Voting_loop <- Voting(O_pool, par_dim_only, k, par_n$Votos_positivos, Votos_negativos, k_method)
    
    #se sobrescribe O_pool con los resultados de la votacion
    #se actualizan visualizaciones, votos positivos y negativos y el grado de conseso
    O_pool <- Voting_loop
    
    
    #se elimina la primera fila del pool de participantes
    par_pool <- par_pool[-c(1:par_num_iteration),]
    
    #el loop se rompe cuando no quedan mas filas en el pool de participantes
    if(nrow(par_pool) == 0)
      break
  }
  
  return(O_pool)
}
```

Vamos a ver el código de la función en detalle…

En este primer segmento, se crea un nuevo dataframe con las dimensiones
de dist y la cantidad de votos disponibles, es decir, se seleccionan
unicamente las columnas Dim1, Dim2, Dim3 y Votos_positivos. Una vez
asignados a la variable par_pool, representan el pool total de
participantes.

La variable total_votos es obtenida a partir del máximo valor de la
columna Votos_positivos en el dataframe dist, representando el total de
votos disponibles por participante.

Además, se genera un tibble con valor NULL, siendo el pool de ideas en
el tiempo t = 0, dónde no se encuentran ideas disponibles.

``` r
#nuevo dataframe con las dimensiones de dist
  par_pool <- dist[,which(grepl("Dim", colnames(dist)) | grepl("Votos", colnames(dist)))]
  
  #Se obtiene la cantidad total de votos para computar cantidad de votos negativos
  total_votos <- max(dist$Votos_positivos)
  
  #dataframe vacio para incorporar resultados del repeat loop
  O_pool <- tibble(NULL)
```

Luego se observa un repeat loop, el cual va a frenar una vez que el
número de filas de par_pool sea igual a 0. Cómo veremos posteriormente,
una vez que una fila del dataset par_pool, representando un
participante, ingresó su opinión al pool de opiniones y realizó el
proceso de votación, es decir, ejecutó la función Voting, es eliminada
del dataset par_pool. De modo que se selecciona la siguiente, y así
sucesivamente hasta agotar la cantidad de filas en el dataset.

``` r
repeat{
  ...
  if(nrow(par_pool) == 0)
      break
}
```

En este fragmento se observa la selección de participantes y armado del
dataset de opiniones. par_n es una variable que contiene una o más filas
(dependiendo el valor de par_num_iteration), seleccionadas del dataset
par_pool, definido anteriormente.

La variable par_dim_only contiene solamente los valores de Dim1, Dim2 y
Dim3 de par_n. Estos valores son luego utilizados en looping_tbl para
generar los valores del dataframe de ideas.

Looping_tbl es un dataframe que cuya función es agregar las columnas ID,
visualizaciones, V_pos, V_neg y ratio_votos_vis junto a la opinion del
participante (par_dim_only). Luego, este tibble une la fila generada a
las filas del dataset de ideas O_pool.

En este sentido, O_pool permite acumular las ideas de cada participante
por iteracion, junto con sus visualizaciones, votos y ratio, dado que
looping_tbl se resetea por cada iteración del repeat loop.

``` r
#se selecciona primera fila 
    par_n <- par_pool[c(1:par_num_iteration),]
    
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
```

La variable Votos_negativos es generada a partir de la resta del valor
de la columna Votos_positivos de par_n con el número asignado a la
variable total_votos, definida anteriormente como el máximo valor de la
columna Votos_positivos del dataframe dist.

``` r
#Se restan los votos positivos del total de votos para obtener la cantidad de votos negativos
    Votos_negativos <- abs(par_n$Votos_positivos - total_votos)
```

Una vez definidos la cantidad de votos negativos, se pasan las variables
definidas como argumentos a la función Voting, la cual llevará a cabo el
proceso de votación. El resultado de la función es guardado dentro de la
variable O_pool, sobrescribiendo los valores de visualizaciones, V_pos,
V_neg y ratio_votos_vis anteriores.

``` r
 #se guardan los resultados de la funcion Voting
    Voting_loop <- Voting(O_pool, par_dim_only, k, par_n$Votos_positivos, Votos_negativos, k_method)
    
    #se sobrescribe O_pool con los resultados de la votacion
    #se actualizan visualizaciones, votos positivos y negativos y el grado de conseso
    O_pool <- Voting_loop
```

Por último, se elimina al participante del pool de participantes, como
se adelantó anteriormente.

``` r
par_pool <- par_pool[-c(1:par_num_iteration),]
```
