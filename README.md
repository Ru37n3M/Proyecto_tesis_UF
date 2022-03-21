<!DOCTYPE html>

<html lang= "esp-ar">
  <head>
    <meta charset="UTF-8">
  </head>
<main>
  <h1> Proyecto tesis UF </h1>
 </main>
 <body>
  <article>
    <section>
    <h2>Objetivo</h2>
  <p> El objetivo de este proyecto es explorar cuáles son los parámetros requeridos para obtener el mejor consenso (definir mejor consenso) a partir del modelado de distintas distribuciones siguiendo la macroestructura de algunos modelos de influencia social (definir cuales, influencia social asimilativa, influencia social por bias de similitud, influencia social repulsiva(indagar evidencia empirica), modelos híbridos). Por lo tanto, se pretende observar cómo se comportan los agentes en base a distintos modelos.Una vez obtenidos los resultados y distribuciones modeladas, estos se analizarán y se compararán con una muestra obtenida de forma empírica para analizarla en relación a las distribuciones modeladas </p>
      </section>
      
<section>
  <h2>Circuito de la plataforma</h2>
    <p> El modelado se realizará en base a una plataforma, la misma que posteriormente será utilizada para obtener la muestra empírica, cuyo <b>circuito</b> es el siguiente: </p>
    <ol>
      <li>Un usuario entra a la plataforma. Se direcciona a un “desafío”. El desafío plantea una consigna sobre la que se deberán presentar aportes/ideas (un texto de menos de 1000 caracteres en principio) Se le pide al usuario que ingrese una idea o aporte sobre ese desafío. </li>
      <li>Se le muestran al usuario en una misma pantalla un conjunto de R ideas </li>
      <li>El usuario puede leer esas ideas y votar: A. La idea que le parece más relevante/más lo representa B. La segunda idea que le parece más relevante/más lo representa C. La idea que te genera más rechazo</li>
      <li>Los usuarios acumularán puntos al terminar el desafìo segùn 2 criterios: A. Si las ideas propuestas generaron un consenso (en este caso un consenso positivo). B. Si las ideas de otros usuarios que cada usuario votó, lograron obtener un consenso alto. En base a esos puntajes pueden darse premios.</li>
    </ol>
    </section>
    <section>
    <header><h2>Parámetros utilizados en el modelado del circuito:</h2></header>
  <ul>
    <li><b>N</b> cantidad de usuarios</li>
    <li><b>I</b> cantidad de ideas</li>
    <li><b>R</b> cantidad de ideas por conjunto de ideas</li>
    <li><b>V_pos</b> cantidad de votos positivos</li>
    <li><b>V_neg</b> cantidad de votos negativos</li>
  </ul>
    </section>
  <section>
    <header><h2>Algoritmo de presentación</h2></header>
    <p>La presentación de ideas se va a seleccionar en base a un criterio determinado: </p>
<ul>
<li><b>Criterio A:</b> Se van a cargar las ideas que menos “visualizaciones” tengan. Es decir, cada vez que una idea es cargada y se le muestra a un usuario, se le agrega una visualización. Cuando otro usuario carga un conjunto de ideas, estas se van a elegir en base a las que tengan menos cantidad de visualizaciones. El fin de esto es que todas las ideas se muestran la misma cantidad de veces.</li>
<li><b>Criterio B:</b> Se cargan las ideas según el grado de consenso que generen en base a las votaciones previas. La idea de este criterio es que las “mejores” ideas se muestren màs para poder encontrar las mejores ideas (en este caso el orden de apariciòn es muy relevante)</li>
<li><b>Criterio C:</b> Este criterio usa una mezcla entre el criterio A y el B. Por ejemplo, la mitad de las ideas mostradas del conjunto, serán cargadas en base a las visualizaciones que tengan previamente. La otra mitad de las ideas cargadas, serán seleccionadas según el consenso que hayan logrado previamente. La idea es lograr un equilibrio entre que todas las ideas tengan oportunidad de ser calificadas (esto puede no pasar en el criterio B) pero a su vez, que las “mejores” ideas puedan ser visualizadas por mucha gente para facilitar la elección de las mejores ideas.</li>
    </ul>
    </section>
    <section>
      <header><h2>Criterios para obtención de consenso </h2></header> 
<ul>
  <li><b>Criterio A:</b> Cantidad de votos positivos sobre visualizaciones</li>
<li><b>Criterio B:</b> (Cantidad de votos positivos - cantidad de votos negativos) sobre visualizaciones.</li>
  </ul>
  </section>
    </body>
</html>
  
