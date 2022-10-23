
#####GENERACION DE DATASET#####
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

dist_list <- list(random, A, B)

resultado_generador_graficos <- generador_graficos(dist_list, k= 4, top = 25)

#1
visualizaciones_graficos <- resultado_generador_graficos$visualizaciones

#2
votos_positivos_graficos <- resultado_generador_graficos$votos_positivos

#3
votos_negativos_graficos <- resultado_generador_graficos$votos_negativos

#4
visualizacionesk_graficos <- resultado_generador_graficos$visualizaciones_k

#5
votos_positivos_atleast1vote_graficos <- resultado_generador_graficos$votos_positivos_atleast1vote

#6
votos_negativos_atleast1vote_graficos <- resultado_generador_graficos$votos_negativos_atleast1vote

#7
rates_graficos <- resultado_generador_graficos$rates

#8
rates_atleast1vote_graficos <- resultado_generador_graficos$rates_atleast1vote

#9
ratesxvisualizaciones_graficos <- resultado_generador_graficos$ratesxvisualizaciones

#10
ratesxvisualizaciones_atleast1vote_graficos <- resultado_generador_graficos$ratesxvisualizaciones_atleast1vote

#11
rates_top_graficos <- resultado_generador_graficos$rates_top

#12
visualizaciones_top_graficos <- resultado_generador_graficos$visualizaciones_top

#13
votos_top_graficos <- resultado_generador_graficos$votos_top





