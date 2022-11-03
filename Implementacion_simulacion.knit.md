
<!-- rnb-text-begin -->

---
title: "R Notebook"
output: html_notebook
---


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYGBgclxubGlicmFyeSh0aWR5dmVyc2UpXG5gYGBcbmBgYCJ9 -->

```r
```r
library(tidyverse)
```
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# Parametros


## Variacion de N

En este caso quedan todos los parametros fijos con la excepcion del N.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYGBgclxudmFyaWFjaW9uX24gPC0gZXhwYW5kLmdyaWQoXG4gIGMoMTAwLDIwMCw1MDAsMTAwMCwgMTAwMDApLCAjTlxuICAxMCwgI2NhbnRpZGFkIGRlIGlkZWFzXG4gIDMsICNDYW50aWRhZCBkZSB2b3Rvc1xuICAxLCAjUHJlc2VuY2lhIGRlIHZvdG9zIG5lZ2F0aXZvcyAoYm9vbClcbiAgMC41LCAjUHJvcG9yY2lvbmVzIGRlbCBhbGdvcml0bW8gZGUgdmlzdWFsaXphY2lvblxuICAnNi0yJyAjUGFyYW1ldHJvcyBkZSBsYSBkaXN0cmlidWNpb24gYmV0YVxuKVxuYGBgXG5gYGAifQ== -->

```r
```r
variacion_n <- expand.grid(
  c(100,200,500,1000, 10000), #N
  10, #cantidad de ideas
  3, #Cantidad de votos
  1, #Presencia de votos negativos (bool)
  0.5, #Proporciones del algoritmo de visualizacion
  '6-2' #Parametros de la distribucion beta
)
```
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Variacion de la cantidad de ideas

En este caso quedan todos los parametros fijos con la excepcion de la cantidad de ideas, k.


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYGBgclxudmFyaWFjaW9uX2sgPC0gZXhwYW5kLmdyaWQoXG4gIDUwMCwgI05cbiAgYyg1LDEwLDE1KSwgI2NhbnRpZGFkIGRlIGlkZWFzXG4gIDMsICNDYW50aWRhZCBkZSB2b3Rvc1xuICAxLCAjUHJlc2VuY2lhIGRlIHZvdG9zIG5lZ2F0aXZvcyAoYm9vbClcbiAgMC41LCAjUHJvcG9yY2lvbmVzIGRlbCBhbGdvcml0bW8gZGUgdmlzdWFsaXphY2lvblxuICAnNi0yJyAjUGFyYW1ldHJvcyBkZSBsYSBkaXN0cmlidWNpb24gYmV0YVxuKVxuYGBgXG5gYGAifQ== -->

```r
```r
variacion_k <- expand.grid(
  500, #N
  c(5,10,15), #cantidad de ideas
  3, #Cantidad de votos
  1, #Presencia de votos negativos (bool)
  0.5, #Proporciones del algoritmo de visualizacion
  '6-2' #Parametros de la distribucion beta
)
```
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Variacion de la cantidad de votos

Como siempre, quedan todos los parametros fijos y se varia unicamente la cantidad de votos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYGBgclxudmFyaWFjaW9uX3ZvdG9zIDwtIGV4cGFuZC5ncmlkKFxuICA1MDAsICNOXG4gIDEwLCAjY2FudGlkYWQgZGUgaWRlYXNcbiAgYygxLDMsNSksICNDYW50aWRhZCBkZSB2b3Rvc1xuICAxLCAjUHJlc2VuY2lhIGRlIHZvdG9zIG5lZ2F0aXZvcyAoYm9vbClcbiAgMC41LCAjUHJvcG9yY2lvbmVzIGRlbCBhbGdvcml0bW8gZGUgdmlzdWFsaXphY2lvblxuICAnNi0yJyAjUGFyYW1ldHJvcyBkZSBsYSBkaXN0cmlidWNpb24gYmV0YVxuKVxuYGBgXG5gYGAifQ== -->

```r
```r
variacion_votos <- expand.grid(
  500, #N
  10, #cantidad de ideas
  c(1,3,5), #Cantidad de votos
  1, #Presencia de votos negativos (bool)
  0.5, #Proporciones del algoritmo de visualizacion
  '6-2' #Parametros de la distribucion beta
)
```
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Variacion del tipo de votos


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYGBgclxudmFyaWFjaW9uX3RpcG8gPC0gZXhwYW5kLmdyaWQoXG4gIDUwMCwgI05cbiAgMTAsICNjYW50aWRhZCBkZSBpZGVhc1xuICAzLCAjQ2FudGlkYWQgZGUgdm90b3NcbiAgYygxLDApLCAjUHJlc2VuY2lhIGRlIHZvdG9zIG5lZ2F0aXZvcyAoYm9vbClcbiAgMC41LCAjUHJvcG9yY2lvbmVzIGRlbCBhbGdvcml0bW8gZGUgdmlzdWFsaXphY2lvblxuICAnNi0yJyAjUGFyYW1ldHJvcyBkZSBsYSBkaXN0cmlidWNpb24gYmV0YVxuKVxuYGBgXG5gYGAifQ== -->

```r
```r
variacion_tipo <- expand.grid(
  500, #N
  10, #cantidad de ideas
  3, #Cantidad de votos
  c(1,0), #Presencia de votos negativos (bool)
  0.5, #Proporciones del algoritmo de visualizacion
  '6-2' #Parametros de la distribucion beta
)
```
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


El tipo de votos no esta implementado dentro de la funcion `simulacion_plataforma` por lo que es necesario adaptarlo para lo que esta implementado. La mejor manera de hacerlo seria aprovechar los parametros de la distribucion beta. Siendo que esta distribucion controla la probabilidad de que ocurran votos positivos, si lo llevamos a 1 nos aseguramos que nunca ocurran votos negativos. Para eso podemos usar una distribucion $Beta(1,0)$. 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYGBgclxucmJldGEoMTAsIDEsIDApXG5gYGBcbmBgYCJ9 -->

```r
```r
rbeta(10, 1, 0)
```
```

<!-- rnb-source-end -->

<!-- rnb-output-begin eyJkYXRhIjoiIFsxXSAxIDEgMSAxIDEgMSAxIDEgMSAxXG4ifQ== -->

```
 [1] 1 1 1 1 1 1 1 1 1 1
```



<!-- rnb-output-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

De esta manera implementamos una modificacion sobre el dataframe alterando los parametros de la distribucion para que sean '1-0'


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYGBgclxudmFyaWFjaW9uX3RpcG8kVmFyNiA8LSBjKCc2LTInLCAnMS0wJylcbmBgYFxuYGBgIn0= -->

```r
```r
variacion_tipo$Var6 <- c('6-2', '1-0')
```
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Variacion de algoritmo de visualizacion


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYGBgclxudmFyaWFjaW9uX3Zpc3VhbGl6YWNpb24gPC0gZXhwYW5kLmdyaWQoXG4gIDUwMCwgI05cbiAgMTAsICNjYW50aWRhZCBkZSBpZGVhc1xuICAzLCAjQ2FudGlkYWQgZGUgdm90b3NcbiAgMSwgI1ByZXNlbmNpYSBkZSB2b3RvcyBuZWdhdGl2b3MgKGJvb2wpXG4gIGMoMCwgMC4yNSwgMC41LCAwLjc1LCAxKSwgI1Byb3BvcmNpb25lcyBkZWwgYWxnb3JpdG1vIGRlIHZpc3VhbGl6YWNpb25cbiAgJzYtMicgI1BhcmFtZXRyb3MgZGUgbGEgZGlzdHJpYnVjaW9uIGJldGFcbilcbmBgYFxuYGBgIn0= -->

```r
```r
variacion_visualizacion <- expand.grid(
  500, #N
  10, #cantidad de ideas
  3, #Cantidad de votos
  1, #Presencia de votos negativos (bool)
  c(0, 0.25, 0.5, 0.75, 1), #Proporciones del algoritmo de visualizacion
  '6-2' #Parametros de la distribucion beta
)
```
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Variacion de distribucion beta

Se varian los parametros que controlan la distribucion beta


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYGBgclxudmFyaWFjaW9uX2JldGEgPC0gZXhwYW5kLmdyaWQoXG4gIDUwMCwgI05cbiAgMTAsICNjYW50aWRhZCBkZSBpZGVhc1xuICAzLCAjQ2FudGlkYWQgZGUgdm90b3NcbiAgMSwgI1ByZXNlbmNpYSBkZSB2b3RvcyBuZWdhdGl2b3MgKGJvb2wpXG4gIDAuNSwgI1Byb3BvcmNpb25lcyBkZWwgYWxnb3JpdG1vIGRlIHZpc3VhbGl6YWNpb25cbiAgYygnNC00JywgJzYtMicsICcyLTYnKSAjUGFyYW1ldHJvcyBkZSBsYSBkaXN0cmlidWNpb24gYmV0YVxuKVxuYGBgXG5gYGAifQ== -->

```r
```r
variacion_beta <- expand.grid(
  500, #N
  10, #cantidad de ideas
  3, #Cantidad de votos
  1, #Presencia de votos negativos (bool)
  0.5, #Proporciones del algoritmo de visualizacion
  c('4-4', '6-2', '2-6') #Parametros de la distribucion beta
)
```
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Integracion

Para integrar todos los dataframes generados anteriormente en uno solo vamos a usar `rbind`. 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYGBgclxucGFyYW1ldHJvc19zaW11bGFjaW9uX2RmIDwtIHJiaW5kKFxuICB2YXJpYWNpb25fbiwgXG4gIHZhcmlhY2lvbl9rLFxuICB2YXJpYWNpb25fdm90b3MsXG4gIHZhcmlhY2lvbl90aXBvLFxuICB2YXJpYWNpb25fdmlzdWFsaXphY2lvbixcbiAgdmFyaWFjaW9uX2JldGFcbilcbmBgYFxuYGBgIn0= -->

```r
```r
parametros_simulacion_df <- rbind(
  variacion_n, 
  variacion_k,
  variacion_votos,
  variacion_tipo,
  variacion_visualizacion,
  variacion_beta
)
```
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


A continuacion es necesario cambiar los nombres de las columnas a algo mas representativo


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYGBgclxuY29sbmFtZXMocGFyYW1ldHJvc19zaW11bGFjaW9uX2RmKSA8LSBjKFxuICAnTicsXG4gICdjYW50X2lkZWFzJyxcbiAgJ2NhbnRfdm90b3MnLFxuICAnTmVnYXRpdm9zJyxcbiAgJ0FsZ29yaXRtbycsXG4gICdCZXRhJ1xuKVxuYGBgXG5gYGAifQ== -->

```r
```r
colnames(parametros_simulacion_df) <- c(
  'N',
  'cant_ideas',
  'cant_votos',
  'Negativos',
  'Algoritmo',
  'Beta'
)
```
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Por ultimo tenemos que eliminar filas repetidas. 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuYGBgclxucGFyYW1ldHJvc19zaW11bGFjaW9uX2RmIDwtIHBhcmFtZXRyb3Nfc2ltdWxhY2lvbl9kZiAlPiUgZGlzdGluY3QoKVxuYGBgXG5gYGAifQ== -->

```r
```r
parametros_simulacion_df <- parametros_simulacion_df %>% distinct()
```
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Asi finalmente quedan 16 situaciones unicas.



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxudGVzdCA8LSBwbWFwKHBhcmFtZXRyb3Nfc2ltdWxhY2lvbl9kZixcbiAgICAgfiBzaW11bGFjaW9uX3BsYXRhZm9ybWEoXG4gICAgICBsaXN0ID0gbGlzdChcbiAgICAgICAgICAgICduJyA9IGMoLi4xLzIsIC4uMS8yKSxcbiAgICAgICAgICAgICdtZWFucycgPSBsaXN0KHJlcCgxLDMpLCByZXAoLTEsMykpLFxuICAgICAgICAgICAgJ2Nvdl9tYXQnID0gbGlzdChkaWFnKDMpLCBkaWFnKDMpKVxuICAgICAgICAgICksXG4gICAgICBiZXRhID0gLi42LFxuICAgICAgdm90b3NfdG90YWxlcyA9IC4uMyxcbiAgICAgIGsgPSAuLjIsXG4gICAgICBwcm9wID0gLi41LFxuICAgICAga19tZXRob2QgPSAnQidcbiAgICAgICAgKVxuICAgICApXG5cbmBgYCJ9 -->

```r
test <- pmap(parametros_simulacion_df,
     ~ simulacion_plataforma(
      list = list(
            'n' = c(..1/2, ..1/2),
            'means' = list(rep(1,3), rep(-1,3)),
            'cov_mat' = list(diag(3), diag(3))
          ),
      beta = ..6,
      votos_totales = ..3,
      k = ..2,
      prop = ..5,
      k_method = 'B'
        )
     )

```

<!-- rnb-source-end -->

<!-- rnb-output-begin eyJkYXRhIjoiXG5BdHRhY2hpbmcgcGFja2FnZTog4oCYTUFTU+KAmVxuXG5UaGUgZm9sbG93aW5nIG9iamVjdCBpcyBtYXNrZWQgZnJvbSDigJhwYWNrYWdlOmRwbHly4oCZOlxuXG4gICAgc2VsZWN0XG4ifQ== -->

```

Attaching package: ‘MASS’

The following object is masked from ‘package:dplyr’:

    select
```



<!-- rnb-output-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

Finalmente, se separan los datasets de opiniones y de covisualizaciones. Se utiliza la funcion `generador_graficos` para generar los 6 conjuntos de graficos de los datasets de opiniones.

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG50ZXN0X29ubHlfb3BpbmlvbiA8LSBtYXAodGVzdCwgMSlcbnRlc3RfY292aXN1YWxpemFjaW9uZXMgPC0gbWFwKHRlc3QsIDIpXG5cbmdyYWZpY29zX29waW5pb25lcyA8LSBnZW5lcmFkb3JfZ3JhZmljb3ModGVzdF9vbmx5X29waW5pb24sIHBhcmFtZXRyb3Nfc2ltdWxhY2lvbl9kZilcblxuXG5gYGAifQ== -->

```r

test_only_opinion <- map(test, 1)
test_covisualizaciones <- map(test, 2)

graficos_opiniones <- generador_graficos(test_only_opinion, parametros_simulacion_df)

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->

