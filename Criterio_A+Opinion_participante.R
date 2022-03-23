
#Criterio A + Oi 
#funcion explicada en "function_helper"
vl_list_Oi_A <- voting_loop_Oi_A(rnorm(100,0,1),5,10)
vl_list_Oi_A


#Se producen 4 duplicados, por el momento no se me ocurre como solucionarlo
vl <- as.data.frame(vl_list_Oi_A[1])
n_occur<- data.frame(table(vl$ID))
n_occur[n_occur$Freq > 1,]



#Oi s/criterio A
voting_loop_Oi(rnorm(100,0,1),5)


