

#CONJUNTO DE SIMULACIONES 1

#Parametros
#N(100, 200, 500, 1000, 10000)
#k = 10
#cantidad de votos = 3
#votos negativos SI
#Props = (0.5, 0.5)
#Dist valencia de votos = (4,2)
#algoritmos = random, A, B

library(ggplot2)

#####GENERACION DE DATASET#####
#N100
random_n100_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(33,33,34), # n de cada distribucion
                                                           "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                           "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                      "4,2", #Distribucion beta, n == sum(list$n)
                                                      3,#Size de distribucion binomial 
                                                      10,#numero de k
                                                      prop = 0.5, 
                                                      k_method = "random") #algoritmo de seleccion

A_n100_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(33,33,34), # n de cada distribucion
                                                      "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                      "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                 "4,2", #Distribucion beta, n == sum(list$n)
                                                 3,#Size de distribucion binomial 
                                                 10,#numero de k
                                                 prop = 0.5, 
                                                 k_method = "A") #algoritmo de seleccion

B_n100_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(33,33,34), # n de cada distribucion
                                                      "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                      "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                 "4,2", #Distribucion beta, n == sum(list$n)
                                                 3,#Size de distribucion binomial 
                                                 10,#numero de k
                                                 prop = 0.5, 
                                                 k_method = "B") #algoritmo de seleccion


#N200

random_n200_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(66,67,67), # n de cada distribucion
                                                           "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                           "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                      "4,2", #Distribucion beta, n == sum(list$n)
                                                      3,#Size de distribucion binomial 
                                                      10,#numero de k
                                                      prop = 0.5, 
                                                      k_method = "random") #algoritmo de seleccion

A_n200_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(66,67,67), # n de cada distribucion
                                                      "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                      "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                 "4,2", #Distribucion beta, n == sum(list$n)
                                                 3,#Size de distribucion binomial 
                                                 10,#numero de k
                                                 prop = 0.5, 
                                                 k_method = "A") #algoritmo de seleccion

B_n200_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(66,67,67), # n de cada distribucion
                                                      "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                      "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                 "4,2", #Distribucion beta, n == sum(list$n)
                                                 3,#Size de distribucion binomial 
                                                 10,#numero de k
                                                 prop = 0.5, 
                                                 k_method = "B") #algoritmo de seleccion



#N500

random_n500_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(166,167,167), # n de cada distribucion
                                                           "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                           "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                      "4,2", #Distribucion beta, n == sum(list$n)
                                                      3,#Size de distribucion binomial 
                                                      10,#numero de k
                                                      prop = 0.5, 
                                                      k_method = "random") #algoritmo de seleccion

A_n500_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(166,167,167), # n de cada distribucion
                                                      "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                      "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                 "4,2", #Distribucion beta, n == sum(list$n)
                                                 3,#Size de distribucion binomial 
                                                 10,#numero de k
                                                 prop = 0.5, 
                                                 k_method = "A") #algoritmo de seleccion

B_n500_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(166,167,167), # n de cada distribucion
                                                      "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                      "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                 "4,2", #Distribucion beta, n == sum(list$n)
                                                 3,#Size de distribucion binomial 
                                                 10,#numero de k
                                                 prop = 0.5, 
                                                 k_method = "B") #algoritmo de seleccion




#k1000

random_n1000_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(333,333,334), # n de cada distribucion
                                                            "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                            "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                       "4,2", #Distribucion beta, n == sum(list$n)
                                                       3,#Size de distribucion binomial 
                                                       10,#numero de k
                                                       prop = 0.5, 
                                                       k_method = "random") #algoritmo de seleccion

A_n1000_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(333,333,334), # n de cada distribucion
                                                       "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                       "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                  "4,2", #Distribucion beta, n == sum(list$n)
                                                  3,#Size de distribucion binomial 
                                                  10,#numero de k
                                                  prop = 0.5, 
                                                  k_method = "A") #algoritmo de seleccion

B_n1000_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(333,333,334), # n de cada distribucion
                                                       "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                       "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                  "4,2", #Distribucion beta, n == sum(list$n)
                                                  3,#Size de distribucion binomial 
                                                  10,#numero de k
                                                  prop = 0.5, 
                                                  k_method = "B") #algoritmo de seleccion






#k10000

random_n10000_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(3333,3333,3334), # n de cada distribucion
                                                             "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                             "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                        "4,2", #Distribucion beta, n == sum(list$n)
                                                        3,#Size de distribucion binomial 
                                                        10,#numero de k
                                                        prop = 0.5, 
                                                        k_method = "random") #algoritmo de seleccion

A_n10000_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(3333,3333,3334), # n de cada distribucion
                                                        "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                        "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                   "4,2", #Distribucion beta, n == sum(list$n)
                                                   3,#Size de distribucion binomial 
                                                   10,#numero de k
                                                   prop = 0.5, 
                                                   k_method = "A") #algoritmo de seleccion

B_n10000_k1005_vt3_beta42 <- simulacion_plataforma(list("n" = c(3333,3333,3334), # n de cada distribucion
                                                        "means" = list(c(1,1,1),c(0,0,0),c(-1,-1,-1)), # lista con vectores de medias para cada dist
                                                        "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                                                   "4,2", #Distribucion beta, n == sum(list$n)
                                                   3,#Size de distribucion binomial 
                                                   10,#numero de k
                                                   prop = 0.5, 
                                                   k_method = "B") #algoritmo de seleccion

############




#####GRAFICOS N100#####
#dataframe con los 3 algoritmos de selección
superdf_n100_k1005_vt3_beta42 <- bind_rows(random_n100_k1005_vt3_beta42, A_n100_k1005_vt3_beta42 ,B_n100_k1005_vt3_beta42) %>%
  mutate(
    algoritmo = rep(c("random", "A", "B"), c(100,100,100))
  )



#1 Distribucion de cantidad de visualizaciones
ggplot(superdf_n100_k1005_vt3_beta42, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal() +
  facet_wrap(algoritmo)

ggplot(superdf_n100_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion de cantidad de votos positivos
ggplot(superdf_n100_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)  +
  facet_grid(V_pos~algoritmo)

ggplot(superdf_n100_k1005_vt3_beta42, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal() +
  facet_grid(V_pos~algoritmo)
#Distribucion de cantidad de votos negativos
ggplot(superdf_n100_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)  +
  facet_grid(V_neg~algoritmo)


#2 Distribucion de cantidad de visualizaciones de ideas que tengan más de k visualizaciones

superdf_n100_k1005_vt3_beta42_kvis <- superdf_n100_k1005_vt3_beta42[which(superdf_n100_k1005_vt3_beta42$visualizaciones >= 10),]

ggplot(superdf_n100_k1005_vt3_beta42_kvis, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal()

ggplot(superdf_n100_k1005_vt3_beta42_kvis, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n100_k1005_vt3_beta42_kvis, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)


#3 Distribucion de votos positivos

ggplot(superdf_n100_k1005_vt3_beta42, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos positivos") +  
  theme_minimal()

ggplot(superdf_n100_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n100_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

ggplot(superdf_n100_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_pos~algoritmo)

#DISTRIBUCION DE VOTOS POSITIVOS EN RELACION A VOTOS NEGATIVOS
ggplot(superdf_n100_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_neg~algoritmo)



#4 Distribucion de votos positivos de ideas que tengan mas de 0 votos positivos

superdf_n100_k1005_vt3_beta42_Vposfilt <- superdf_n100_k1005_vt3_beta42[which(superdf_n100_k1005_vt3_beta42$V_pos > 0),]

ggplot(superdf_n100_k1005_vt3_beta42_Vposfilt, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos positivos") +  
  theme_minimal()

ggplot(superdf_n100_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n100_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

ggplot(superdf_n100_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_pos~algoritmo)



#5 Distribucion de votos negativos

ggplot(superdf_n100_k1005_vt3_beta42, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos negativos", title = "Distribución Votos negativos") +  
  theme_minimal()

ggplot(superdf_n100_k1005_vt3_beta42, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n100_k1005_vt3_beta42, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

ggplot(superdf_n100_k1005_vt3_beta42, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_neg~algoritmo)


#6 Distribucion de votos negativos de ideas que tengan mas de 0 votos negativos

superdf_n100_k1005_vt3_beta42_Vnegfilt <- superdf_n100_k1005_vt3_beta42[which(superdf_n100_k1005_vt3_beta42$V_neg > 0),]

ggplot(superdf_n100_k1005_vt3_beta42_Vnegfilt, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos negativos") +  
  theme_minimal()

ggplot(superdf_n100_k1005_vt3_beta42_Vnegfilt, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n100_k1005_vt3_beta42_Vposfilt, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

ggplot(superdf_n100_k1005_vt3_beta42_Vposfilt, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_neg~algoritmo)





#7 Distribucion de rates

ggplot(superdf_n100_k1005_vt3_beta42, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rate", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n100_k1005_vt3_beta42, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n100_k1005_vt3_beta42, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#8 Distribucion de rates de ideas que tengan al menos 1 voto

superdf_n100_k1005_vt3_beta42_Votefilt <- superdf_n100_k1005_vt3_beta42[which(superdf_n100_k1005_vt3_beta42$V_neg > 0 |
                                                                                superdf_n100_k1005_vt3_beta42$V_pos > 0),]

ggplot(superdf_n100_k1005_vt3_beta42_Votefilt, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rates", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n100_k1005_vt3_beta42_Votefilt, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n100_k1005_vt3_beta42_Vposfilt, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#9 Distribucion de rates x visualizaciones

ggplot(superdf_n100_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n100_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n100_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

ggplot(superdf_n100_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 


#10 Distribucion de rates de ideas x visualizaciones que tengan al menos 1 voto

ggplot(superdf_n100_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n100_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n100_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

ggplot(superdf_n100_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

#Creacion de dataframe con top 25 de cada algoritmo

random_n100_k1005_vt3_beta42_top25 <- random_n100_k1005_vt3_beta42[order(random_n100_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  slice_head(. , n = 25) 

A_n100_k1005_vt3_beta42_top25 <- A_n100_k1005_vt3_beta42[order(A_n100_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  slice_head(. , n = 25) 

B_n100_k1005_vt3_beta42_top25 <- B_n100_k1005_vt3_beta42[order(B_n100_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  slice_head(. , n = 25) 

superdf_n100_k1005_vt3_beta42_top25 <- bind_rows(random_n100_k1005_vt3_beta42_top25, A_n100_k1005_vt3_beta42_top25 ,
                                                 B_n100_k1005_vt3_beta42_top25) %>%
  mutate(
    algoritmo = rep(c("random", "A", "B"), c(25,25,25))
  )



#11 Distribucion de rates de las 25 ideas con mejor rate

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rates", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion rates x visualizaciones
ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal()

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)


ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() +
  facet_grid(V_pos~algoritmo)

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() +
  facet_grid(V_neg~algoritmo)

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 



#12 Distribucion de visualizaciones de las 25 ideas con mejor rate

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución visualizaciones") +  
  theme_minimal()

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)


#13 Distribucion de votos de las 25 ideas con mejor rate

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos", title = "Distribución votos positivos") +  
  theme_minimal()

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#votos negativos
ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos", title = "Distribución votos negativos") +  
  theme_minimal()

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n100_k1005_vt3_beta42_top25, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)
##################

#####GRAFICOS N200#####
#dataframe con los 3 algoritmos de selección
superdf_n200_k1005_vt3_beta42 <- bind_rows(random_n200_k1005_vt3_beta42, A_n200_k1005_vt3_beta42 ,B_n200_k1005_vt3_beta42) %>%
  mutate(
    algoritmo = rep(c("random", "A", "B"), c(200,200,200))
  )



#1 Distribucion de cantidad de visualizaciones
ggplot(superdf_n200_k1005_vt3_beta42, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal() +
  facet_wrap(~algoritmo)

ggplot(superdf_n200_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion de cantidad de votos positivos
ggplot(superdf_n200_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)  +
  facet_grid(V_pos~algoritmo)

ggplot(superdf_n200_k1005_vt3_beta42, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal() +
  facet_grid(V_pos~algoritmo)
#Distribucion de cantidad de votos negativos
ggplot(superdf_n200_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)  +
  facet_grid(V_neg~algoritmo)


#2 Distribucion de cantidad de visualizaciones de ideas que tengan más de k visualizaciones

superdf_n200_k1005_vt3_beta42_kvis <- superdf_n200_k1005_vt3_beta42[which(superdf_n200_k1005_vt3_beta42$visualizaciones >= 10),]

ggplot(superdf_n200_k1005_vt3_beta42_kvis, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal()

ggplot(superdf_n200_k1005_vt3_beta42_kvis, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n200_k1005_vt3_beta42_kvis, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)


#3 Distribucion de votos positivos

ggplot(superdf_n200_k1005_vt3_beta42, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos positivos") +  
  theme_minimal()

ggplot(superdf_n200_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n200_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

ggplot(superdf_n200_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_pos~algoritmo)


#4 Distribucion de votos positivos de ideas que tengan mas de 0 votos positivos

superdf_n200_k1005_vt3_beta42_Vposfilt <- superdf_n200_k1005_vt3_beta42[which(superdf_n200_k1005_vt3_beta42$V_pos > 0),]

ggplot(superdf_n200_k1005_vt3_beta42_Vposfilt, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos positivos") +  
  theme_minimal()

ggplot(superdf_n200_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n200_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

ggplot(superdf_n200_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_pos~algoritmo)



#5 Distribucion de votos negativos

ggplot(superdf_n200_k1005_vt3_beta42, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos negativos", title = "Distribución Votos negativos") +  
  theme_minimal()

ggplot(superdf_n200_k1005_vt3_beta42, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n200_k1005_vt3_beta42, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion separada por cantidad de votos negativos
ggplot(superdf_n200_k1005_vt3_beta42, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_neg~algoritmo)



#6 Distribucion de votos negativos de ideas que tengan mas de 0 votos negativos

superdf_n200_k1005_vt3_beta42_Vnegfilt <- superdf_n200_k1005_vt3_beta42[which(superdf_n200_k1005_vt3_beta42$V_neg > 0),]

ggplot(superdf_n200_k1005_vt3_beta42_Vnegfilt, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos negativos") +  
  theme_minimal()

ggplot(superdf_n200_k1005_vt3_beta42_Vnegfilt, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n200_k1005_vt3_beta42_Vposfilt, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion separada por cantidad de votos negativos
ggplot(superdf_n200_k1005_vt3_beta42_Vposfilt, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_neg~algoritmo)


#7 Distribucion de rates

ggplot(superdf_n200_k1005_vt3_beta42, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rate", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n200_k1005_vt3_beta42, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n200_k1005_vt3_beta42, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#8 Distribucion de rates de ideas que tengan al menos 1 voto

superdf_n200_k1005_vt3_beta42_Votefilt <- superdf_n200_k1005_vt3_beta42[which(superdf_n200_k1005_vt3_beta42$V_neg > 0 |
                                                                                superdf_n200_k1005_vt3_beta42$V_pos > 0),]

ggplot(superdf_n200_k1005_vt3_beta42_Votefilt, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rates", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n200_k1005_vt3_beta42_Votefilt, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n200_k1005_vt3_beta42_Vposfilt, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#9 Distribucion de rates x visualizaciones

ggplot(superdf_n200_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n200_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n200_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

ggplot(superdf_n200_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

#Distribucion de rate separada por cantidad de votos positivos

ggplot(superdf_n200_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_grid(V_pos~algoritmo)


#Distribucion de rate separada por cantidad de votos negativos

ggplot(superdf_n200_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_grid(V_neg~algoritmo)



#10 Distribucion de rates de ideas x visualizaciones que tengan al menos 1 voto

ggplot(superdf_n200_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n200_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n200_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

ggplot(superdf_n200_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

#Distribucion de rate separada por cantidad de votos positivos

ggplot(superdf_n200_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_grid(V_pos~algoritmo)


#Distribucion de rate separada por cantidad de votos negativos

ggplot(superdf_n200_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_grid(V_neg~algoritmo)

#Creacion de dataframe con top 25 de cada algoritmo

random_n200_k1005_vt3_beta42_top25 <- random_n200_k1005_vt3_beta42[order(random_n200_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  slice_head(. , n = 25) 

A_n200_k1005_vt3_beta42_top25 <- A_n200_k1005_vt3_beta42[order(A_n200_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  slice_head(. , n = 25) 

B_n200_k1005_vt3_beta42_top25 <- B_n200_k1005_vt3_beta42[order(B_n200_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  slice_head(. , n = 25) 

superdf_n200_k1005_vt3_beta42_top25 <- bind_rows(random_n200_k1005_vt3_beta42_top25, A_n200_k1005_vt3_beta42_top25 ,
                                                 B_n200_k1005_vt3_beta42_top25) %>%
  mutate(
    algoritmo = rep(c("random", "A", "B"), c(25,25,25))
  )



#11 Distribucion de rates de las 25 ideas con mejor rate

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rates", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion rates x visualizaciones
ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal()

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)


#Distribucion de rate por cantidad de votos positivos
ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() +
  facet_grid(V_pos~algoritmo)

#Distribucion de rate por cantidad de votos negativos
ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() +
  facet_grid(V_neg~algoritmo)



#12 Distribucion de visualizaciones de las 25 ideas con mejor rate

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución visualizaciones") +  
  theme_minimal()

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)


#13 Distribucion de votos de las 25 ideas con mejor rate

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos", title = "Distribución votos positivos") +  
  theme_minimal()

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion de votos positivos separados por cantidad de votos negativos

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_neg~algoritmo)

#votos negativos
ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos", title = "Distribución votos negativos") +  
  theme_minimal()

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n200_k1005_vt3_beta42_top25, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)
##################

#####GRAFICOS N500#####
#dataframe con los 3 algoritmos de selección
superdf_n500_k1005_vt3_beta42 <- bind_rows(random_n500_k1005_vt3_beta42, A_n500_k1005_vt3_beta42 ,B_n500_k1005_vt3_beta42) %>%
  mutate(
    algoritmo = rep(c("random", "A", "B"), c(500,500,500))
  )



#1 Distribucion de cantidad de visualizaciones
ggplot(superdf_n500_k1005_vt3_beta42, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal() +
  facet_wrap(~algoritmo)

ggplot(superdf_n500_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion de cantidad de votos positivos
ggplot(superdf_n500_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)  +
  facet_grid(V_pos~algoritmo)

ggplot(superdf_n500_k1005_vt3_beta42, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal() +
  facet_grid(V_pos~algoritmo)
#Distribucion de cantidad de votos positivos
ggplot(superdf_n500_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)  +
  facet_grid(V_neg~algoritmo)


#2 Distribucion de cantidad de visualizaciones de ideas que tengan más de k visualizaciones

superdf_n500_k1005_vt3_beta42_kvis <- superdf_n500_k1005_vt3_beta42[which(superdf_n500_k1005_vt3_beta42$visualizaciones >= 10),]

ggplot(superdf_n500_k1005_vt3_beta42_kvis, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal()

ggplot(superdf_n500_k1005_vt3_beta42_kvis, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n500_k1005_vt3_beta42_kvis, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)


#3 Distribucion de votos positivos

ggplot(superdf_n500_k1005_vt3_beta42, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos positivos") +  
  theme_minimal()

ggplot(superdf_n500_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n500_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion de votos positivos separada por votos negativos
ggplot(superdf_n500_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_neg~algoritmo)


#4 Distribucion de votos positivos de ideas que tengan mas de 0 votos positivos

superdf_n500_k1005_vt3_beta42_Vposfilt <- superdf_n500_k1005_vt3_beta42[which(superdf_n500_k1005_vt3_beta42$V_pos > 0),]

ggplot(superdf_n500_k1005_vt3_beta42_Vposfilt, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos positivos") +  
  theme_minimal()

ggplot(superdf_n500_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n500_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion de Votos positivos separada por cantidad de votos negativos
ggplot(superdf_n500_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_neg~algoritmo)

#5 Distribucion de votos negativos

ggplot(superdf_n500_k1005_vt3_beta42, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos negativos", title = "Distribución Votos negativos") +  
  theme_minimal()

ggplot(superdf_n500_k1005_vt3_beta42, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n500_k1005_vt3_beta42, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#6 Distribucion de votos negativos de ideas que tengan mas de 0 votos negativos

superdf_n500_k1005_vt3_beta42_Vnegfilt <- superdf_n500_k1005_vt3_beta42[which(superdf_n500_k1005_vt3_beta42$V_neg > 0),]

ggplot(superdf_n500_k1005_vt3_beta42_Vnegfilt, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos negativos") +  
  theme_minimal()

ggplot(superdf_n500_k1005_vt3_beta42_Vnegfilt, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n500_k1005_vt3_beta42_Vposfilt, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion de votos negativos separada por votos positivos
ggplot(superdf_n500_k1005_vt3_beta42_Vposfilt, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_pos~algoritmo)


#7 Distribucion de rates

ggplot(superdf_n500_k1005_vt3_beta42, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rate", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n500_k1005_vt3_beta42, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n500_k1005_vt3_beta42, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#8 Distribucion de rates de ideas que tengan al menos 1 voto

superdf_n500_k1005_vt3_beta42_Votefilt <- superdf_n500_k1005_vt3_beta42[which(superdf_n500_k1005_vt3_beta42$V_neg > 0 |
                                                                                superdf_n500_k1005_vt3_beta42$V_pos > 0),]

ggplot(superdf_n500_k1005_vt3_beta42_Votefilt, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rates", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n500_k1005_vt3_beta42_Votefilt, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n500_k1005_vt3_beta42_Vposfilt, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#9 Distribucion de rates x visualizaciones

ggplot(superdf_n500_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n500_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n500_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

ggplot(superdf_n500_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 


#10 Distribucion de rates de ideas x visualizaciones que tengan al menos 1 voto

ggplot(superdf_n500_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n500_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n500_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

ggplot(superdf_n500_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

#Creacion de dataframe con top 25 de cada algoritmo

random_n500_k1005_vt3_beta42_top25 <- random_n500_k1005_vt3_beta42[order(random_n500_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  filter(. , visualizaciones >= 10) %>%
  slice_head(. , n = 25) 

A_n500_k1005_vt3_beta42_top25 <- A_n500_k1005_vt3_beta42[order(A_n500_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  slice_head(. , n = 25) 

B_n500_k1005_vt3_beta42_top25 <- B_n500_k1005_vt3_beta42[order(B_n500_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  filter(. , visualizaciones >= 10) %>%
  slice_head(. , n = 25) 

superdf_n500_k1005_vt3_beta42_top25 <- bind_rows(random_n500_k1005_vt3_beta42_top25, A_n500_k1005_vt3_beta42_top25 ,
                                                 B_n500_k1005_vt3_beta42_top25) %>%
  mutate(
    algoritmo = rep(c("random", "A", "B"), c(25,25,25))
  )



#11 Distribucion de rates de las 25 ideas con mejor rate

ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rates", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion rates x visualizaciones
ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal()

ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

#Rate x visualizaciones separado por cantidad de votos positivos
ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() +
  facet_grid(V_pos~algoritmo)

#Rate x visualizaciones separado por cantidad de votos positivos
ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() +
  facet_grid(V_neg~algoritmo)

ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 



#12 Distribucion de visualizaciones de las 25 ideas con mejor rate

ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución visualizaciones") +  
  theme_minimal()

ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)


#13 Distribucion de votos de las 25 ideas con mejor rate

ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos", title = "Distribución votos positivos") +  
  theme_minimal()

ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#votos negativos
ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos", title = "Distribución votos negativos") +  
  theme_minimal()

ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n500_k1005_vt3_beta42_top25, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)
##################

#####GRAFICOS N1000#####
#dataframe con los 3 algoritmos de selección
superdf_n1000_k1005_vt3_beta42 <- bind_rows(random_n1000_k1005_vt3_beta42, A_n1000_k1005_vt3_beta42 ,B_n1000_k1005_vt3_beta42) %>%
  mutate(
    algoritmo = rep(c("random", "A", "B"), c(1000,1000,1000))
  )



#1 Distribucion de cantidad de visualizaciones
ggplot(superdf_n1000_k1005_vt3_beta42, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal() +
  facet_wrap(~algoritmo)

ggplot(superdf_n1000_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion de cantidad de visuaslizaciones separada por votos positivos
ggplot(superdf_n1000_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)  +
  facet_grid(V_pos~algoritmo)

ggplot(superdf_n1000_k1005_vt3_beta42, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal() +
  facet_grid(V_pos~algoritmo)

ggplot(superdf_n1000_k1005_vt3_beta42, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_jitter() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal() +
  facet_grid(V_pos~algoritmo)

#Distribucion de cantidad de visualizaciones separada por cantidad de votos negativos
ggplot(superdf_n1000_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)  +
  facet_grid(V_neg~algoritmo)

#2 Distribucion de cantidad de visualizaciones de ideas que tengan más de k visualizaciones

superdf_n1000_k1005_vt3_beta42_kvis <- superdf_n1000_k1005_vt3_beta42[which(superdf_n1000_k1005_vt3_beta42$visualizaciones >= 10),]

ggplot(superdf_n1000_k1005_vt3_beta42_kvis, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal()

ggplot(superdf_n1000_k1005_vt3_beta42_kvis, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n1000_k1005_vt3_beta42_kvis, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)


#Distribucion de cantidad de visuaslizaciones separada por votos positivos
ggplot(superdf_n1000_k1005_vt3_beta42_kvis, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)  +
  facet_grid(V_pos~algoritmo)

ggplot(superdf_n1000_k1005_vt3_beta42_kvis, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal() +
  facet_grid(V_pos~algoritmo)

ggplot(superdf_n1000_k1005_vt3_beta42_kvis, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_jitter() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal() +
  facet_grid(V_pos~algoritmo)

#Distribucion de cantidad de visualizaciones separada por cantidad de votos negativos
ggplot(superdf_n1000_k1005_vt3_beta42_kvis, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)  +
  facet_grid(V_neg~algoritmo)


#3 Distribucion de votos positivos

ggplot(superdf_n1000_k1005_vt3_beta42, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos positivos") +  
  theme_minimal()

ggplot(superdf_n1000_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n1000_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion de votos positivos separada por votos negativos
ggplot(superdf_n1000_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_neg~algoritmo)

#4 Distribucion de votos positivos de ideas que tengan mas de 0 votos positivos

superdf_n1000_k1005_vt3_beta42_Vposfilt <- superdf_n1000_k1005_vt3_beta42[which(superdf_n1000_k1005_vt3_beta42$V_pos > 0),]

ggplot(superdf_n1000_k1005_vt3_beta42_Vposfilt, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos positivos") +  
  theme_minimal()

ggplot(superdf_n1000_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n1000_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)


#Distribucion de votos positivos separada por cantidad de votos negativos
ggplot(superdf_n1000_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_neg~algoritmo)


#5 Distribucion de votos negativos

ggplot(superdf_n1000_k1005_vt3_beta42, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos negativos", title = "Distribución Votos negativos") +  
  theme_minimal()

ggplot(superdf_n1000_k1005_vt3_beta42, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n1000_k1005_vt3_beta42, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#6 Distribucion de votos negativos de ideas que tengan mas de 0 votos negativos

superdf_n1000_k1005_vt3_beta42_Vnegfilt <- superdf_n1000_k1005_vt3_beta42[which(superdf_n1000_k1005_vt3_beta42$V_neg > 0),]

ggplot(superdf_n1000_k1005_vt3_beta42_Vnegfilt, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos negativos") +  
  theme_minimal()

ggplot(superdf_n1000_k1005_vt3_beta42_Vnegfilt, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n1000_k1005_vt3_beta42_Vposfilt, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#7 Distribucion de rates

ggplot(superdf_n1000_k1005_vt3_beta42, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rate", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n1000_k1005_vt3_beta42, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n1000_k1005_vt3_beta42, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion de rates separada por cantidad de votos positivos
ggplot(superdf_n1000_k1005_vt3_beta42, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_pos~algoritmo)

#Distribucion de rates separada por cantidad de votos negativos
ggplot(superdf_n1000_k1005_vt3_beta42, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_neg~algoritmo)

#8 Distribucion de rates de ideas que tengan al menos 1 voto

superdf_n1000_k1005_vt3_beta42_Votefilt <- superdf_n1000_k1005_vt3_beta42[which(superdf_n1000_k1005_vt3_beta42$V_neg > 0 |
                                                                                  superdf_n1000_k1005_vt3_beta42$V_pos > 0),]

ggplot(superdf_n1000_k1005_vt3_beta42_Votefilt, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rates", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n1000_k1005_vt3_beta42_Votefilt, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n1000_k1005_vt3_beta42_Votefilt, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#9 Distribucion de rates x visualizaciones

ggplot(superdf_n1000_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n1000_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n1000_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

ggplot(superdf_n1000_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 


#10 Distribucion de rates de ideas x visualizaciones que tengan al menos 1 voto

ggplot(superdf_n1000_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n1000_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n1000_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

ggplot(superdf_n1000_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

#Distribucion de rates x visualizaciones separada por votos positivos

ggplot(superdf_n1000_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_grid(V_pos~algoritmo)

#Distribucion de rates x visualizaciones separada por votos negativos

ggplot(superdf_n1000_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_grid(V_neg~algoritmo)

#Creacion de dataframe con top 25 de cada algoritmo

random_n1000_k1005_vt3_beta42_top25 <- random_n1000_k1005_vt3_beta42[order(random_n1000_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  filter(. , visualizaciones >= 10) %>%
  slice_head(. , n = 25) 


A_n1000_k1005_vt3_beta42_top25 <- A_n1000_k1005_vt3_beta42[order(A_n1000_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  filter(. , visualizaciones >= 10) %>%
  slice_head(. , n = 25) 


B_n1000_k1005_vt3_beta42_top25 <- B_n1000_k1005_vt3_beta42[order(B_n1000_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  filter(. , visualizaciones >= 10) %>%
  slice_head(. , n = 25) 


superdf_n1000_k1005_vt3_beta42_top25 <- bind_rows(random_n1000_k1005_vt3_beta42_top25, A_n1000_k1005_vt3_beta42_top25 ,
                                                  B_n1000_k1005_vt3_beta42_top25) %>%
  mutate(
    algoritmo = rep(c("random", "A", "B"), c(25,25,25))
  )



#11 Distribucion de rates de las 25 ideas con mejor rate

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rates", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion de rates separadas por votos positivos

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Frecuencia", title = "Distribución Rate") + 
  theme_minimal() + 
  facet_grid(V_pos~algoritmo)


#Distribucion rates x visualizaciones
ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal()

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

#Distribucion de rate x visualizaciones separado por cantidad de votos positivos
ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() +
  facet_grid(V_pos~algoritmo)


#Distribucion de rate x visualizaciones separado por cantidad de votos negativos
ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() +
  facet_grid(V_neg~algoritmo)



#12 Distribucion de visualizaciones de las 25 ideas con mejor rate

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución visualizaciones") +  
  theme_minimal()

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)


#13 Distribucion de votos de las 25 ideas con mejor rate

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos", title = "Distribución votos positivos") +  
  theme_minimal()

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#votos negativos
ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos", title = "Distribución votos negativos") +  
  theme_minimal()

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n1000_k1005_vt3_beta42_top25, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)
##################

#####GRAFICOS N10000#####
#dataframe con los 3 algoritmos de selección
superdf_n10000_k1005_vt3_beta42 <- bind_rows(random_n10000_k1005_vt3_beta42, A_n10000_k1005_vt3_beta42 ,B_n10000_k1005_vt3_beta42) %>%
  mutate(
    algoritmo = rep(c("random", "A", "B"), c(10000,10000,10000))
  )



#1 Distribucion de cantidad de visualizaciones
ggplot(superdf_n10000_k1005_vt3_beta42, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal() +
  facet_wrap(~algoritmo)

ggplot(superdf_n10000_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion de visualizaciones separado por cantidad de votos positivos
ggplot(superdf_n10000_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)  +
  facet_grid(V_pos~algoritmo)

#Distribucion de visualizaciones separado por cantidad de votos positivos
ggplot(superdf_n10000_k1005_vt3_beta42, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal() +
  facet_grid(V_pos~algoritmo)

#Distribucion de visualizaciones separado por cantidad de votos negativos
ggplot(superdf_n10000_k1005_vt3_beta42, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)  +
  facet_grid(V_neg~algoritmo)


#2 Distribucion de cantidad de visualizaciones de ideas que tengan más de k visualizaciones

superdf_n10000_k1005_vt3_beta42_kvis <- superdf_n10000_k1005_vt3_beta42[which(superdf_n10000_k1005_vt3_beta42$visualizaciones >= 10),]

ggplot(superdf_n10000_k1005_vt3_beta42_kvis, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución Visualizaciones") +  
  theme_minimal()

ggplot(superdf_n10000_k1005_vt3_beta42_kvis, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n10000_k1005_vt3_beta42_kvis, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)


#3 Distribucion de votos positivos

ggplot(superdf_n10000_k1005_vt3_beta42, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos positivos") +  
  theme_minimal()

ggplot(superdf_n10000_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n10000_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion de votos positivos separado por votos negativos
gplot(superdf_n10000_k1005_vt3_beta42, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_neg~algoritmo)


#4 Distribucion de votos positivos de ideas que tengan mas de 0 votos positivos
superdf_n10000_k1005_vt3_beta42_Vposfilt <- superdf_n10000_k1005_vt3_beta42[which(superdf_n10000_k1005_vt3_beta42$V_pos > 0),]

ggplot(superdf_n10000_k1005_vt3_beta42_Vposfilt, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos positivos") +  
  theme_minimal()

ggplot(superdf_n10000_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n10000_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)


#Distribucion de votos positivos separado por votos negativos
ggplot(superdf_n10000_k1005_vt3_beta42_Vposfilt, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_grid(V_neg~algoritmo)

#5 Distribucion de votos negativos

ggplot(superdf_n10000_k1005_vt3_beta42, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos negativos", title = "Distribución Votos negativos") +  
  theme_minimal()

ggplot(superdf_n10000_k1005_vt3_beta42, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n10000_k1005_vt3_beta42, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#6 Distribucion de votos negativos de ideas que tengan mas de 0 votos negativos

superdf_n10000_k1005_vt3_beta42_Vnegfilt <- superdf_n10000_k1005_vt3_beta42[which(superdf_n10000_k1005_vt3_beta42$V_neg > 0),]

ggplot(superdf_n10000_k1005_vt3_beta42_Vnegfilt, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos positivos", title = "Distribución Votos negativos") +  
  theme_minimal()

ggplot(superdf_n10000_k1005_vt3_beta42_Vnegfilt, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n10000_k1005_vt3_beta42_Vposfilt, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#7 Distribucion de rates

ggplot(superdf_n10000_k1005_vt3_beta42, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rate", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n10000_k1005_vt3_beta42, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n10000_k1005_vt3_beta42, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#8 Distribucion de rates de ideas que tengan al menos 1 voto

superdf_n10000_k1005_vt3_beta42_Votefilt <- superdf_n10000_k1005_vt3_beta42[which(superdf_n10000_k1005_vt3_beta42$V_neg > 0 |
                                                                                    superdf_n10000_k1005_vt3_beta42$V_pos > 0),]

ggplot(superdf_n10000_k1005_vt3_beta42_Votefilt, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rates", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n10000_k1005_vt3_beta42_Votefilt, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n10000_k1005_vt3_beta42_Vposfilt, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#9 Distribucion de rates x visualizaciones

ggplot(superdf_n10000_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n10000_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n10000_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

ggplot(superdf_n10000_k1005_vt3_beta42, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 


#10 Distribucion de rates de ideas x visualizaciones que tengan al menos 1 voto

ggplot(superdf_n10000_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n10000_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n10000_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

ggplot(superdf_n10000_k1005_vt3_beta42_Votefilt, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 

#Creacion de dataframe con top 25 de cada algoritmo


#
#random_n10000_k1005_vt3_beta42[which(random_n10000_k1005_vt3_beta42$visualizaciones >= 10),]
#order(random_n10000_k1005_vt3_beta42$ratio_votos_vis, decreasing = T)
random_n10000_k1005_vt3_beta42_top25 <- random_n10000_k1005_vt3_beta42[order(random_n10000_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  filter(. , visualizaciones >= 10) %>%
  slice_head(. , n = 25) 

A_n10000_k1005_vt3_beta42_top25 <- A_n10000_k1005_vt3_beta42[order(A_n10000_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  filter(. , visualizaciones >= 10) %>%
  slice_head(. , n = 25) 

B_n10000_k1005_vt3_beta42_top25 <- B_n10000_k1005_vt3_beta42[order(B_n10000_k1005_vt3_beta42$ratio_votos_vis, decreasing = T), ] %>%
  filter(. , visualizaciones >= 10) %>%
  slice_head(. , n = 25) 

superdf_n10000_k1005_vt3_beta42_top25 <- bind_rows(random_n10000_k1005_vt3_beta42_top25, A_n10000_k1005_vt3_beta42_top25 ,
                                                   B_n10000_k1005_vt3_beta42_top25) %>%
  mutate(
    algoritmo = rep(c("random", "A", "B"), c(25,25,25))
  )

#11 Distribucion de rates de las 25 ideas con mejor rate

ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(algoritmo, ratio_votos_vis, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "rates", title = "Distribución rates") +  
  theme_minimal()

ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(x = ratio_votos_vis, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#Distribucion rates x visualizaciones
ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal()

ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() + 
  facet_wrap(~algoritmo)

ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_count() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() 
#Distribucion visualizaciones por votos positivos
ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() +
  facet_grid(V_pos~algoritmo)

#Distribucion visualizaciones por votos negativos
ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(ratio_votos_vis, visualizaciones, col = algoritmo)) + 
  geom_jitter() + 
  labs(x = "Ratio votos/visualizaciones", y = "Visualizaciones", title = "Distribución Ratio - Visualizaciones") + 
  theme_minimal() +
  facet_grid(V_neg~algoritmo)





#12 Distribucion de visualizaciones de las 25 ideas con mejor rate

ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(algoritmo, visualizaciones, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Visualizaciones", title = "Distribución visualizaciones") +  
  theme_minimal()

ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(x = visualizaciones, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)


#13 Distribucion de votos de las 25 ideas con mejor rate

ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(algoritmo, V_pos, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos", title = "Distribución votos positivos") +  
  theme_minimal()

ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(x = V_pos, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)

#votos negativos
ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(algoritmo, V_neg, col = algoritmo)) +
  geom_count() +
  labs(x = "Algoritmo", y = "Votos", title = "Distribución votos negativos") +  
  theme_minimal()

ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2)

ggplot(superdf_n10000_k1005_vt3_beta42_top25, aes(x = V_neg, fill = algoritmo, color = algoritmo)) + 
  geom_bar(size = 2) +
  facet_wrap(~algoritmo)
##################

