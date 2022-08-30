<!DOCTYPE html>
<html lang="esp">
    <head>
        <meta charset="UTF-8" />
        <link href="styles.css" rel="stylesheet" />
    </head>
    <main>
        <body>
            <header>
                <h1 class="title">Efectos contextuales sobre la dinámica de opinión en una plataforma de Crowdsoursing</h1>
                <h2>Tesista: Yabur Carranza, Enio A.  </h2>
                <h2>Directores: Ramos Usaj, Alejandro; Dottori, Martín</h2>
            </header>
            <section class="intro">
                <h2 class="intro-header">Introducción</h2>
                <p>La masividad del intercambio de opiniones a través de redes sociales
                    y el impacto que genera en la sociedad actual ha puesto de manifiesto
                    la necesidad de analizar y comprender estos fenómenos con mayor profundidad.</p>
                <p>Debido a la alta complejidad social involucrada en este proceso,
                    una forma muy útil de aproximarse a su comprensión y generar 
                    predicciones cuantitativas y cualitativas es mediante las simulaciones computacionales.</p>
                <p>En este estudio se generará un conjunto de simulaciones basadas en agentes sobre la estructura
                    de una plataforma de votación con el objetivo de estudiar un sistema de facilitación y búsqueda 
                    de consensos.</p>
                <h3>¿Por qué simular?</h3>
                <p>Mediante estas simulaciones, se busca explorar el comportamiento de distintos modelos de dinámicas de opiniones en relación a la dinámica de formación de consensos,
                    la distribución de votos y visualizaciones de las opiniones, la estructura de similitud entre opiniones según votos recibidos por agentes y el efecto de influencia social para la generación de opiniones.</p>
            </section>
            <section class="OD">
                <h2 class="OD-header">¿Qué es Dinamicas de Opinión?</h2>
                <p>La forma en la cual los individuos en una red social interactúan e intercambian opiniones 
                    y cómo la opinión propia de un individuo cambia como consecuencia de dicha interacción 
                    se conoce como dinámicas de opinión.</p>
                <p>El estudio de las dinámicas de opinión (DO) apunta a
                    comprender y caracterizar cómo los individuos forman sus opiniones 
                    y cómo estas evolucionan a través del tiempo y a partir de la interacción 
                    con otros agentes.</p>
            </section>
            <section class="WoC">
                <h2 class="WoC-header">¿Qué es Sabiduría de las masas?</h2>
                <p> El efecto Sabiduría de las Masas (SM) aplica a problemas donde se asume una respuesta correcta, 
                    dado que enuncia que el promedio de todas las respuestas es un valor cercano al valor real en 
                    tanto las estimaciones de cada individuo se realicen de forma simétrica e independiente alrededor 
                    del valor. </p>
                <p>Dicho efecto es utilizado para exporar las dinámicas de opinión en problemas con respuestas correctas a priori</p>
            </section>
            <section class="Crowdsourcing">
                <h2 class="Crowdsourcing-header">¿Qué es Crowdsourcing?</h2>
                <p>Crowdsourcing  es un método en el cual no necesariamente se asume una respuesta correcta, 
                    sino que implica que poseer a un gran número de personas trabajando en una misma tarea, 
                    y luego compartiendo las soluciones, impulsa la innovación y selección de ideas más votadas 
                    o “mejores”.</p>
            </section>
            <section class="ABM">
                <h2 class="ABM-header">¿Qué es el Modelado Basado en Agentes?</h2>
                <p>Agent-Based Modelling (ABM) es un tipo de simulación basada en objetos, en el cual cada individuo 
                    es representado por un agente y sus opiniones son representadas por un valor que evoluciona 
                    en el tiempo.</p>
            </section>
            <section class="relevance">
                <h2 class="relevance-header">Relevancia</h2>
                <p>Se ha ubicado una falta de estudios que analicen la dinámica de opiniones en
                    plataformas de crowdsourcing online, asumiendo influencia unidireccional 
                    bajo distintas condiciones contextuales, como alternativa para explorar SM en problemas 
                    sin respuesta a priori.</p>
            </section>
            <section class="objectives">
                <h2 class="objectives-header">Objetivos</h2>
                <p>El objetivo de este estudio es explorar el comportamiento de un modelo de dinámica de opiniones e 
                    identificación de consensos bajo distintas condiciones contextuales de la plataforma donde 
                    interactúan los agentes para generar predicciones cuantitativas y cualitativas que puedan ser 
                    exploradas empíricamente.</p>
                <p>Específicamente se explorará la dinámica de formación de consensos, la distribución de votos 
                    y visualizaciones de las opiniones, la estructura de similitud entre opiniones según votos 
                    recibidos por agentes y el efecto de influencia social para la generación de opiniones.</p>
            </section>
            <section class="method">
                <h2 class="method-header">Metodología</h2>
                <h3>Funcionamiento de la plataforma</h3>
                <p>La simulación pretende representar la dinámica generada por una plataforma 
                    de crowdsourcing de opiniones, que funciona de la siguiente manera:
                </p>
                <ol>
                    <li>Un usuario entra a la plataforma. Se direcciona a un “desafío”. 
                        El desafío plantea una consigna sobre la que se deberán presentar sus opiniones.</li>
                    <li>Dependiendo de la implementación se le pide al usuario que 
                        primero ingrese su opinión sobre ese desafío o se le muestran una cantidad 
                        determinada de ideas del resto de los usuarios sobre ese desafío 
                        (según el criterio de un algoritmo de selección) para que vote sobre las mismas.</li>
                    <li>Independientemente de en qué momento el usuario dio su opinión, 
                        una vez que esta fue emitida entra en el conjunto del total de ideas que pueden 
                        llegar a ser visualizadas por los otros usuarios. De igual manera, 
                        independientemente de en qué momento se realice la votación, 
                        los usuarios visualizan una cantidad fija de ideas del resto 
                        sobre las cuales pueden emitir uno o más votos individuales 
                        (sobre cada idea) de valencia positiva o negativa. </li>
                </ol>
                <h3>Definiciones de Agente</h3>
                <p>La cantidad de agentes (usuarios) está dado por el parámetro <i>N</i> siendo
                   <i>a</i><sub>i</sub>, con <i>i</i>=1,2,...,<i>N</i> el agente i-ésimo de la simulación.
                </p>
                <p>A su vez cada agente tiene asociada una opinión en un espacio ideológico definido según</p>
                <p><i>O</i>(<i>a<sub>i</sub></i>) = [<i>O</i><sub>1</sub>, <i>O</i><sub>2</sub>, <i>O</i><sub>3</sub>], <i>i</i> = 1, 2,..., <i>N</i>; <i>O</i><sub>i</sub></i> &isin; &#x211D<sup>3</sup></p>
                <p>donde <i>O</i><sub>i</sub> es la opinión del agente i-ésimo en el espacio tridimensional.</p>
                <p>Las opiniones del agente i-ésimo se obtienen por muestreo a partir de una distribución gaussiana multivariada 
                    <i>O</i><sub>i</sub> &sim; <i>N</i>(&mu;, &Sigma;).</p>
                <p>En uno de los casos la distribución va a estar definida como <i>N</i>(0, &Sigma;).</p>
                <p>
                    En otro de los casos la distribución va a estar dada por una mezcla de gaussianas multivariadas 
                    con 2 componentes representando 2 grupos de opiniones definidas según <i>N</i>(&mu;<sub>1</sub>, &Sigma;)
                    y <i>N</i>(&mu;<sub>2</sub>, &Sigma;) donde &mu;<sub>1</sub> = 0 y &mu;<sub>1</sub> - &mu;<sub>2</sub> indica el grado de polarización de las opiniones.
                </p>
               <p>Para todos los casos la estructura de la matriz de covarianza (&Sigma;) es esférica.</p>
               <p>
                <i>O</i><sub>i</sub>es entonces una realización de alguna de estas distribuciones multivariadas y 
                expresa la idea que el agente <i>a</i><sub>i</sub> ingresa a la plataforma y expresa su opinión siendo
                &Theta;<sub>t</sub> = {<i>O</i><sub>1</sub>, <i>O</i><sub>2</sub>, ..., <i>O</i><sub>t</sub>}, &forall;<i>t</i> &le; <i>N</i> 
                el conjunto de todas las opiniones presentes en la plataforma hasta el momento <i>t</i>.
               </p>
               <p>
                Dada una cantidad <i>k</i> de opiniones disponibles en cada ronda de valoración, <i>G</i><sub>i</sub> &sub; &Theta;<sub>t</sub> \ {<i>O</i
                   <sub>i</sub>} 
                es el subconjunto de <i>k</i> opiniones vistas por el i-ésimo agente. Si un caso particular se le muestran al agente <i>a</i><sub>i</sub> el conjunto
                   de opiniones
                <i>G</i><sub>i</sub> , <i>O</i><sub>i,j</sub>  refiere a la j-esima idea que se le muestran al i-esimo agente donde
                <i>O</i><sub>i,j</sub> &isin; <i>G</i><sub>i</sub> , <i>j</i> &isin; <i>N</i> - {<i>i</i>}.
                Definimos la conducta de votación positiva como la probabilidad de que el agente <i>a</i><sub>i</sub> vote positivamente a la idea
                   <i>O</i><sub>i,j</sub> lo cual está dado por </p>
                   <p>P(O<sub>i,j</sub>) = <i>D</i><sub>i,j</sub><sup>-1</sup> / &sum;<sub>j</sub><sup>k</sup> <i>D</i><sub>i,j</sub><sup>-1</sup></p>
        <p>estando <i>D</i><sub>i,j</sub> definido como la distancia absoluta entre dos opiniones según <i>D</i><sub>i,j</sub> = |<i>O</i><sub>i</sub> - <i>O</i><sub>j</sub>| donde <i>O</i><sub>j</sub> &isin; <i>G</i><sub>i</sub>.
               </p>
                <p>
                     Se define entonces la probabilidad de que el agente <i>a</i><sub>i</sub> vote positivamente a la idea <i>O</i><sub>j</sub> como proporcional a la inversa de la distancia entre la opinión del agente (<i>O</i><sub>i</sub>) y la idea <i>O</i><sub>j</sub>.
                </p>
                <p>La conducta de votación negativa funciona de manera análoga con la única diferencia que no se toma la inversa de la distancia </p>
                    <p>P(O<sub>i,j</sub>) = <i>D</i><sub>i,j</sub> / &sum;<sub>j</sub><sup>k</sup> <i>D</i><sub>i,j</sub></p>  
                <p>
                    Por tanto la probabilidad de votar negativamente la idea es proporcional a la distancia.
                </p>
                <h3>Definiciones de la plataforma</h3>
                <p>La selección de opiniones que se le van a mostrar al agente i-ésimo en una determinada ronda estarán dadas por dos algoritmos de selección de opiniones que serán manipulados.</p>
                <p>Sean <i>v</i>(<i>O</i><sub>j</sub>) la cantidad de visualizaciones de la j-ésima opinión y <i>V</i> &sube; &theta;<sub>t</sub> el subconjunto de opiniones con el mínimo valor de visualizaciones donde |<i>V</i>| &isin; [k, N], el primer algoritmo &fnof;<sub>1</sub>(<i>x</i>) elige <i>k</i> ideas seleccionando <i>O</i><sub>j</sub> cuando <i>v</i>(<i>O</i><sub>i</sub>) < <i>v</i>(<i>O</i><sub>j</sub>) o seleccionando <i>O</i><sub>i</sub> con probabilidad 1/|<i>V</i>| cuando <i>v</i>(<i>O</i><sub>i</sub>) = <i>v</i>(<i>O</i><sub>j</sub>) </p>
                <p>Sea <i>M</i> &sube; &theta; el subconjunto de <i>n</i> opiniones con el máximo ratio de votos sobre visualizaciones <i>m</i>(<i>O</i><sub>i</sub>) /  <i>v</i>(<i>O</i><sub>i</sub>), siendo <i>m</i>(<i>O</i><sub>i</sub>) la cantidad de votos positivos de la i-ésima opinión o la diferencia entre los votos positivos y los votos negativos dependiendo de si en una determinada iteración la plataforma posee votos negativos, el segundo algoritmo &fnof;<sub>2</sub>(<i>x</i>) selecciona <i>k</i> / 2 opiniones de <i>G</i><sub>i,r</sub> según el algoritmo anteriormente descrito y el resto de las opiniones son seleccionadas tomando las <i>k</i> / 2 opiniones con el mayor ratio de votos sobre visualizaciones del conjunto <i>M</i>,  eligiendo <i>O</i><sub>i</sub> cuando <i>m</i>(<i>O</i><sub>i</sub>) < <i>m</i>(<i>O</i><sub>j</sub>) y eligiendo <i>O</i><sub>i</sub> con probabilidad 1/<i>n</i> cuando <i>m</i>(<i>O</i><sub>i</sub>) = <i>m</i>(<i>O</i><sub>j</sub>). </p>
            </section>    
        </body>
    </main>    
</html>




