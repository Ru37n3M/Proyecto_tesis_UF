
#####GENERACION DE DATASET#####

#Generamos 3 datasets, uno por cada algoritmo
#Se pueden variar los parámetros para observar luego en los gráficos

random <- simulacion_plataforma(list("n" = c(166,167,167), # n de cada distribucion
                                                          "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                          "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                     "4,2", #Distribucion beta, n == sum(list$n)
                                                     3,#Size de distribucion binomial 
                                                     4,#numero de k
                                                     prop = 0.5, 
                                                     k_method = "random") #algoritmo de seleccion

A <- simulacion_plataforma(list("n" = c(166,167,167), # n de cada distribucion
                                                     "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                     "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                "4,2", #Distribucion beta, n == sum(list$n)
                                                3,#Size de distribucion binomial 
                                                4,#numero de k
                                                prop = 0.5, 
                                                k_method = "A") #algoritmo de seleccion

B <- simulacion_plataforma(list("n" = c(166,167,167), # n de cada distribucion
                                                     "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                     "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                "4,2", #Distribucion beta, n == sum(list$n)
                                                3,#Size de distribucion binomial 
                                                4,#numero de k
                                                prop = 0.5, 
                                                k_method = "B") #algoritmo de seleccion

############

#creamos una lista para correrla en la funcion
dist_list <- list(random, A, B)

#la funcion devuelve una lista
resultado_generador_graficos <- generador_graficos(dist_list, k= 4, top = 25)

#1 visualizamos los graficos de distribucion de visualizaciones
visualizaciones_graficos <- resultado_generador_graficos$visualizaciones

#2 visualizamos la dist de votos positivos
votos_positivos_graficos <- resultado_generador_graficos$votos_positivos

#3 visualizamos la dist de votos negativos
votos_negativos_graficos <- resultado_generador_graficos$votos_negativos

#4 visualizamos nuevamente la dist de visualizaciones, pero solamente de ideas con k visualizaciones
visualizacionesk_graficos <- resultado_generador_graficos$visualizaciones_k

#5 visualizamos la dist de votos positivos solo de ideas con mas de 0 votos positivos
votos_positivos_atleast1vote_graficos <- resultado_generador_graficos$votos_positivos_atleast1vote

#6 visualizamos la dist de votos negativos solo de ideas con mas de 0 votos negativos
votos_negativos_atleast1vote_graficos <- resultado_generador_graficos$votos_negativos_atleast1vote

#7 visualizamos la dist de los rates
rates_graficos <- resultado_generador_graficos$rates

#8 visualizamos la dist de los rates de las ideas con al menos 1 voto
rates_atleast1vote_graficos <- resultado_generador_graficos$rates_atleast1vote

#9 visualizamos la dist del rate x las visualizaciones
ratesxvisualizaciones_graficos <- resultado_generador_graficos$ratesxvisualizaciones

#10 visualizamos la dist del rate x las visualizaciones de las ideas con al menos 1 voto
ratesxvisualizaciones_atleast1vote_graficos <- resultado_generador_graficos$ratesxvisualizaciones_atleast1vote

#11 visualizamos la dist de rates de las n ideas con mayor rate
rates_top_graficos <- resultado_generador_graficos$rates_top

#12 visualizamos la dist de visualizaciones de las n ideas con mayor rate
visualizaciones_top_graficos <- resultado_generador_graficos$visualizaciones_top

#13 visualizamos la dist de votos de las n ideas con mayor rate
votos_top_graficos <- resultado_generador_graficos$votos_top





