
#Criterio A + Oi
#funcion explicada en "function_helper"
vl_list_A_Oi<- voting_loop_Oi(rnorm(100,0,1),5)
vl_list_A_Oi


vl <- as.data.frame(vl_list_A_Oi[1])

n_occur<- data.frame(table(vl$ID))
n_occur[n_occur$Freq > 1,]


