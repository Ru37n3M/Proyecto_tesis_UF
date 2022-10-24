#GRAFICOS PARA LOS 6 CONJUNTOS DE SIMULACIONES VARIANDO LOS PARAMETROS DISPONIBLES

#SETEAR PARAMETROS
n <- c(166,167,167)
vt <- 3
k <- 10
beta <- "2,4"
props <- 0.5
votos_neg <- TRUE

#GENERACION DE DATASETS
random <- simulacion_plataforma(list("n" = n, # n de cada distribucion
                                     "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                     "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                beta, #Distribucion beta, n == sum(list$n)
                                vt,#Size de distribucion binomial 
                                k,#numero de k
                                props,
                                votos_neg,
                                k_method = "random") #algoritmo de seleccion

A <- simulacion_plataforma(list("n" = n, # n de cada distribucion
                                "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                           beta, #Distribucion beta, n == sum(list$n)
                           vt,#Size de distribucion binomial 
                           k,#numero de k
                           props, 
                           votos_neg,
                           k_method = "A") #algoritmo de seleccion

B <- simulacion_plataforma(list("n" = n, # n de cada distribucion
                                "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                           beta, #Distribucion beta, n == sum(list$n)
                           vt,#Size de distribucion binomial 
                           k,#numero de k
                           props,
                           votos_neg,
                           k_method = "B") #algoritmo de seleccion


#Guardar los 3 datasets dentro de una lista
dist_list <- list(random, A, B)

#Luego guardar los graficos de cada parametro antes de generar nuevos dataset

#Parametro variado: n (100, 200, 500, 1000, 10000)
#n100
resultado_n100 <- generador_graficos(dist_list, k, top = 25)
#n200
resultado_n200 <- generador_graficos(dist_list, k, top = 25)
#n500
resultado_n500 <- generador_graficos(dist_list, k, top = 25)
#n1000
resultado_n1000 <- generador_graficos(dist_list, k, top = 25)
#n1000
resultado_n10000 <- generador_graficos(dist_list, k, top = 25)

#1
resultado_n100$visualizaciones
resultado_n200$visualizaciones
resultado_n500$visualizaciones
resultado_n1000$visualizaciones
resultado_n10000$visualizaciones
#2
resultado_n100$votos_positivos
resultado_n200$votos_positivos
resultado_n500$votos_positivos
resultado_n1000$votos_positivos
resultado_n10000$votos_positivos
#3
resultado_n100$votos_negativos
resultado_n200$votos_negativos
resultado_n500$votos_negativos
resultado_n100$votos_negativos
resultado_n10000$votos_negativos
#4
resultado_n100$visualizaciones_k
resultado_n200$visualizaciones_k
resultado_n500$visualizaciones_k
resultado_n1000$visualizaciones_k
resultado_n10000$visualizaciones_k
#5
resultado_n100$votos_positivos_atleast1vote
resultado_n200$votos_positivos_atleast1vote
resultado_n500$votos_positivos_atleast1vote
resultado_n1000$votos_positivos_atleast1vote
resultado_n10000$votos_positivos_atleast1vote
#6
resultado_n100$votos_negativos_atleast1vote
resultado_n200$votos_negativos_atleast1vote
resultado_n500$votos_negativos_atleast1vote
resultado_n1000$votos_negativos_atleast1vote
resultado_n10000$votos_negativos_atleast1vote
#7
resultado_n100$rates
resultado_n200$rates
resultado_n500$rates
resultado_n1000$rates
resultado_n10000$rates
#8
resultado_n100$rates_atleast1vote
resultado_n200$rates_atleast1vote
resultado_n500$rates_atleast1vote
resultado_n1000$rates_atleast1vote
resultado_n10000$rates_atleast1vote
#9
resultado_n100$ratesxvisualizaciones
resultado_n200$ratesxvisualizaciones
resultado_n300$ratesxvisualizaciones
resultado_n400$ratesxvisualizaciones
resultado_n500$ratesxvisualizaciones
#10
resultado_n100$ratesxvisualizaciones_atleast1vote
resultado_n200$ratesxvisualizaciones_atleast1vote
resultado_n500$ratesxvisualizaciones_atleast1vote
resultado_n1000$ratesxvisualizaciones_atleast1vote
resultado_n10000$ratesxvisualizaciones_atleast1vote
#11
resultado_n100$rates_top
resultado_n200$rates_top
resultado_n500$rates_top
resultado_n1000$rates_top
resultado_n10000$rates_top
#12
resultado_n100$visualizaciones_top
resultado_n200$visualizaciones_top
resultado_n500$visualizaciones_top
resultado_n1000$visualizaciones_top
resultado_n10000$visualizaciones_top
#13
resultado_n100$votos_top
resultado_n200$votos_top
resultado_n500$votos_top
resultado_n1000$votos_top
resultado_n10000$votos_top


######
#parametro variado k (5,10,15)
#k5
resultado_k5 <- generador_graficos(dist_list, k, top = 25)
#k10
resultado_k10 <- generador_graficos(dist_list, k, top = 25)
#k15
resultado_k15 <- generador_graficos(dist_list, k, top = 25)


#1
resultado_k5$visualizaciones
resultado_k10$visualizaciones
resultado_k15$visualizaciones
#2
resultado_k5$votos_positivos
resultado_k10$votos_positivos
resultado_k15$votos_positivos
#3
resultado_k5$votos_negativos
resultado_k10$votos_negativos
resultado_k15$votos_negativos
#4
resultado_k5$visualizaciones_k
resultado_k10$visualizaciones_k
resultado_k15$visualizaciones_k
#5
resultado_k5$votos_positivos_atleast1vote
resultado_k10$votos_positivos_atleast1vote
resultado_k15$votos_positivos_atleast1vote
#6
resultado_k5$votos_negativos_atleast1vote
resultado_k10$votos_negativos_atleast1vote
resultado_k15$votos_negativos_atleast1vote
#7
resultado_k5$rates
resultado_k10$rates
resultado_k15$rates
#8
resultado_k5$rates_atleast1vote
resultado_k10$rates_atleast1vote
resultado_k15$rates_atleast1vote
#9
resultado_k5$ratesxvisualizaciones
resultado_k10$ratesxvisualizaciones
resultado_k15$ratesxvisualizaciones
#10
resultado_k5$ratesxvisualizaciones_atleast1vote
resultado_k10$ratesxvisualizaciones_atleast1vote
resultado_k15$ratesxvisualizaciones_atleast1vote
#11
resultado_k5$rates_top
resultado_k10$rates_top
resultado_k15$rates_top
#12
resultado_k5$visualizaciones_top
resultado_k10$visualizaciones_top
resultado_k15$visualizaciones_top
#13
resultado_k5$votos_top
resultado_k10$votos_top
resultado_k15$votos_top

######
#Parametro variado: cantidad de votos totales (vt) (1, 3, 5)

#vt1
resultado_vt1 <- generador_graficos(dist_list, k, top = 25)
#vt3
resultado_vt3 <- generador_graficos(dist_list, k, top = 25)
#vt5
resultado_vt5 <- generador_graficos(dist_list, k, top = 25)

#1
resultado_vt1$visualizaciones
resultado_vt3$visualizaciones
resultado_vt5$visualizaciones
#2
resultado_vt1$votos_positivos
resultado_vt3$votos_positivos
resultado_vt5$votos_positivos
#3
resultado_vt1$votos_negativos
resultado_vt3$votos_negativos
resultado_vt5$votos_negativos
#4
resultado_vt1$visualizaciones_k
resultado_vt3$visualizaciones_k
resultado_vt5$visualizaciones_k
#5
resultado_vt1$votos_positivos_atleast1vote
resultado_vt3$votos_positivos_atleast1vote
resultado_vt5$votos_positivos_atleast1vote
#6
resultado_vt1$votos_negativos_atleast1vote
resultado_vt3$votos_negativos_atleast1vote
resultado_vt5$votos_negativos_atleast1vote
#7
resultado_vt1$rates
resultado_vt3$rates
resultado_vt5$rates
#8
resultado_vt1$rates_atleast1vote
resultado_vt3$rates_atleast1vote
resultado_vt5$rates_atleast1vote
#9
resultado_vt1$ratesxvisualizaciones
resultado_vt3$ratesxvisualizaciones
resultado_vt5$ratesxvisualizaciones
#10
resultado_vt1$ratesxvisualizaciones_atleast1vote
resultado_vt3$ratesxvisualizaciones_atleast1vote
resultado_vt5$ratesxvisualizaciones_atleast1vote
#11
resultado_vt1$rates_top
resultado_vt3$rates_top
resultado_vt5$rates_top
#12
resultado_vt1$visualizaciones_top
resultado_vt3$visualizaciones_top
resultado_vt5$visualizaciones_top
#13
resultado_vt1$votos_top
resultado_vt3$votos_top
resultado_vt5$votos_top


######
#Parametro variado: votos negativos (TRUE, FALSE)

#TRUE
resultado_TRUE <- generador_graficos(dist_list, k, top = 25)
#FALSE
resultado_FALSE <- generador_graficos(dist_list, k, top = 25)
#1
resultado_TRUE$visualizaciones
resultado_FALSE$visualizaciones
#2
resultado_TRUE$votos_positivos
resultado_FALSE$votos_positivos
#3
resultado_TRUE$votos_negativos
resultado_FALSE$votos_negativos
#4
resultado_TRUE$visualizaciones_k
resultado_FALSE$visualizaciones_k
#5
resultado_TRUE$votos_positivos_atleast1vote
resultado_FALSE$votos_positivos_atleast1vote
#6
resultado_TRUE$votos_negativos_atleast1vote
resultado_FALSE$votos_negativos_atleast1vote
#7
resultado_TRUE$rates
resultado_FALSE$rates
#8
resultado_TRUE$rates_atleast1vote
resultado_FALSE$rates_atleast1vote
#9
resultado_TRUE$ratesxvisualizaciones
resultado_FALSE$ratesxvisualizaciones
#10
resultado_TRUE$ratesxvisualizaciones_atleast1vote
resultado_FALSE$ratesxvisualizaciones_atleast1vote
#11
resultado_TRUE$rates_top
resultado_FALSE$rates_top
#12
resultado_TRUE$visualizaciones_top
resultado_FALSE$visualizaciones_top
#13
resultado_TRUE$votos_top
resultado_FALSE$votos_top

######
#Parametro variado: proporciones (prop) ((0,1), (0.25,0.75), (0.5, 0.5), (0.25, 0.75), (1, 0))

#1, 0
resultado_prop1 <- generador_graficos(dist_list, k, top = 25)
#0.75, 0.25
resultado_prop075 <- generador_graficos(dist_list, k, top = 25)
#0.5, 0.5
resultado_prop05 <- generador_graficos(dist_list, k, top = 25)
#0.25, 0,75
resultado_prop025 <- generador_graficos(dist_list, k, top = 25)
#0, 1
resultado_prop0 <- generador_graficos(dist_list, k, top = 25)

#1
resultado_prop1$visualizaciones
resultado_prop075$visualizaciones
resultado_prop05$visualizaciones
resultado_prop025$visualizaciones
resultado_prop0$visualizaciones
#2
resultado_prop1$votos_positivos
resultado_prop075$votos_positivos
resultado_prop05$votos_positivos
resultado_prop025$votos_positivos
resultado_prop0$votos_positivos

#3
resultado_prop1$votos_negativos
resultado_prop075$votos_negativos
resultado_prop05$votos_negativos
resultado_prop025$votos_negativos
resultado_prop0$votos_negativos
#4
resultado_prop1$visualizaciones_k
resultado_prop075$visualizaciones_k
resultado_prop05$visualizaciones_k
resultado_prop025$visualizaciones_k
resultado_prop0$visualizaciones_k
#5
resultado_prop1$votos_positivos_atleast1vote
resultado_prop075$votos_positivos_atleast1vote
resultado_prop05$votos_positivos_atleast1vote
resultado_prop025$votos_positivos_atleast1vote
resultado_prop0$votos_positivos_atleast1vote
#6
resultado_prop1$votos_negativos_atleast1vote
resultado_prop075$votos_negativos_atleast1vote
resultado_prop05$votos_negativos_atleast1vote
resultado_prop025$votos_negativos_atleast1vote
resultado_prop0$votos_negativos_atleast1vote
#7
resultado_prop1$rates
resultado_prop075$rates
resultado_prop05$rates
resultado_prop025$rates
resultado_prop0$rates
#8
resultado_prop1$rates_atleast1vote
resultado_prop075$rates_atleast1vote
resultado_prop05$rates_atleast1vote
resultado_prop025$rates_atleast1vote
resultado_prop0$rates_atleast1vote
#9

resultado_prop1$ratesxvisualizaciones
resultado_prop075$ratesxvisualizaciones
resultado_prop05$ratesxvisualizaciones
resultado_prop025$ratesxvisualizaciones
resultado_prop0$ratesxvisualizaciones
#10
resultado_prop1$ratesxvisualizaciones_atleast1vote
resultado_prop075$ratesxvisualizaciones_atleast1vote
resultado_prop05$ratesxvisualizaciones_atleast1vote
resultado_prop025$ratesxvisualizaciones_atleast1vote
resultado_prop0$ratesxvisualizaciones_atleast1vote
#11
resultado_prop1$rates_top
resultado_prop075$rates_top
resultado_prop05$rates_top
resultado_prop025$rates_top
resultado_prop0$rates_top
#12
resultado_prop1$visualizaciones_top
resultado_prop075$visualizaciones_top
resultado_prop05$visualizaciones_top
resultado_prop025$visualizaciones_top
resultado_prop0$visualizaciones_top
#13
resultado_prop1$votos_top
resultado_prop075$votos_top
resultado_prop05$votos_top
resultado_prop025$votos_top
resultado_prop0$votos_top


######
#Parametro variado: dist valencia de los votos (beta) ((4,4),(4,2),(2,4))

#beta44
resultado_beta44 <- generador_graficos(dist_list, k, top = 25)
#beta42
resultado_beta42 <- generador_graficos(dist_list, k, top = 25)
#beta24
resultado_beta24 <- generador_graficos(dist_list, k, top = 25)

#1
resultado_beta44$visualizaciones
resultado_beta42$visualizaciones
resultado_beta24$visualizaciones
#2
resultado_beta44$votos_positivos
resultado_beta42$votos_positivos
resultado_beta24$votos_positivos
#3
resultado_beta44$votos_negativos
resultado_beta42$votos_negativos
resultado_beta24$votos_negativos
#4
resultado_beta44$visualizaciones_k
resultado_beta42$visualizaciones_k
resultado_beta24$visualizaciones_k
#5
resultado_beta44$votos_positivos_atleast1vote
resultado_beta42$votos_positivos_atleast1vote
resultado_beta24$votos_positivos_atleast1vote
#6
resultado_beta44$votos_negativos_atleast1vote
resultado_beta42$votos_negativos_atleast1vote
resultado_beta24$votos_negativos_atleast1vote
#7
resultado_beta44$rates
resultado_beta42$rates
resultado_beta24$rates
#8
resultado_beta44$rates_atleast1vote
resultado_beta42$rates_atleast1vote
resultado_beta24$rates_atleast1vote
#9
resultado_beta44$ratesxvisualizaciones
resultado_beta42$ratesxvisualizaciones
resultado_beta24$ratesxvisualizaciones
#10
resultado_beta44$ratesxvisualizaciones_atleast1vote
resultado_beta42$ratesxvisualizaciones_atleast1vote
resultado_beta24$ratesxvisualizaciones_atleast1vote
#11
resultado_beta44$rates_top
resultado_beta42$rates_top
resultado_beta24$rates_top
#12
resultado_beta44$visualizaciones_top
resultado_beta42$visualizaciones_top
resultado_beta24$visualizaciones_top
#13
resultado_beta44$votos_top
resultado_beta42$votos_top
resultado_beta24$votos_top
