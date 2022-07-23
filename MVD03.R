
mixingfun <- function(dislist){
  
  dislist
  
}


mixingfun(list("n" = c(200,300,100), # n de cada distribucion
               "means" = list(c(2,2,2),c(1,1,1),c(0,0,0)), # lista con vectores de medias para cada dist
               "cov_mat" = list(diag(1,3,3),diag(1,3,3),diag(1,3,3)))) # lista con matrices de covarianza
