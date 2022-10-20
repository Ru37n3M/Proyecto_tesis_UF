dist <- mixingfun(list("n" = c(50,50), # n de cada distribucion
                       "means" = list(c(1,1,1),c(0,0,0)), # lista con vectores de medias para cada dist
                       "cov_mat" = list(diag(1,3,3),diag(1,3,3))), # lista con matrices de covarianza
                  rbeta(100,6,2),#Distribucion beta, n == sum(list$n)
                  3)
dist <- shuffle_dist(dist)

#Genera pool de opiniones
pool_opiniones <- dist %>% select(Dim1:Dim3, ID) %>%
  mutate(
    "visualizaciones"=0, 
    "V_pos"=0,
    "V_neg" = 0,
    "ratio_votos_vis"= 0
  )

contador <- 1

k <- 5

sujetos_iniciales <- T

for(i in 1:nrow(dist)){
  
  sujeto_actual_ID <- dist[[i, 'ID']]
  indice_sujeto_actual <- which(dist$ID == sujeto_actual_ID)
  
  if(sujetos_iniciales == T){
    if(indice_sujeto_actual <= k + 1){
      next
    }else{
      sujetos_iniciales <-  F
    }
  }else{
    
    #pool minimo
    pool_aleatorio <- slice_min(pool_opiniones[1:(indice_sujeto_actual-1),], order_by = visualizaciones, n = k)
    #pool_aleatorio <- slice_sample(pool_opiniones[1:(indice_sujeto_actual-1),], n = k) %>% select(Dim1:Dim3, ID)
    current_ids <- pool_aleatorio %>% pull(ID)
    
    #visualizaciones
    pool_opiniones[pool_opiniones$ID %in% current_ids, 'visualizaciones'] <- pool_opiniones[
      pool_opiniones$ID %in% current_ids, 'visualizaciones'] + 1
    
    #voto aleatorio
    voto <- slice_sample(pool_aleatorio, n =1) %>% pull(ID)
    pool_opiniones[pool_opiniones$ID == voto, 'V_pos'] <- pool_opiniones[
      pool_opiniones$ID == voto, 'V_pos'] + 1
    
  }
  
}
