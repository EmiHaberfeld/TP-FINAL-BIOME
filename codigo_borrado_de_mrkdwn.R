# Introduccion
Los animales se encuentran constantemente tomando decisiones respecto a cuando alimentarse, aparearse, dormir, y demas acciones (1). A traves de aprendizajes y experiencias previas, son capaces de comparar dos o mas escenarios probables antes de realizar cualquier accion (2). La generacion de una expectativa respecto a dichos escenarios es un proceso que permite a los animales predecir la aparicion de estimulos (tanto aversivos como apetitivos) y de este modo, adaptar su comportamiento (3). Esta expectativa incide directamente en sus capacidades mnesicas, debido a que el aprendizaje depende de asociaciones entre claves externas y representaciones internas de dichas claves (4). Este proceso ha sido ampliamente estudiado en vertebrados, pero hay menos informacion disponible en invertebrados.

El objetivo de este trabajo es estudiar la modulacion de memorias a largo termino a partir de cambios en la expectativa de la recompensa. El modelo experimental es la abeja Apis mellifera y los experimentos fueron realizados en un contexto controlado dentro del laboratorio.

# Materiales y metodos
Abejas Apis mellifera fueron entrenadas bajo un condicionamiento clasico del reflejo de extension de proboscide (PER, por sus siglas en ingles) (5,6): se administra un odorante a la vez que se tocan las antenas con una gota de sacarosa. La abeja extiende su proboscide como reflejo de este estimulo, y en ese momento se la alimenta con una solucion azucarada. De este entrenamiento, recibieron 4 ensayos. Terminada la etapa de entrenamiento, se realizaron tres testeos donde se presento solo el odorante a 3, 24 y 48 hs posteriores al ultimo ensayo de entrenamiento.

Las abejas se dividieron en 4 grupos experimentales dependiendo de la sacarosa recibida en las antenas y en la probóoscide. Los grupos "constante alto" y "constante bajo" recibieron tanto en las antenas como en la proboscide azucar de concentracion 1,5 M y 0,5 M respectivamente. Por otro lado, los grupos "contraste positivo" y "contraste negativo" recibieron azucar de distinta concentracion en cada pieza sensorial: los animales del grupo contraste positivo recibieron sacarosa 0,5 M en las antenas y 1,5 M en su proboscide. Por otro lado, los animales del contraste negativo recibieron azucar 1,5 M en las antenas para luego ser alimentadas con sacarosa 0,5 M.

Como VR se midio la extension de la proboscide frente al olor (si-no). Al ser una variable dicotomica, la distribucion de probabilidades esperada es una Bernoulli. El diseño es de medidas repetidas ya que cada abeja fue medida 7 veces (4 ensayos de entrenamiento + 3 testeos). Se realizo estadistica descriptiva de la etapa de entrenamiento y un modelo estadistico para la etapa de evaluacion ya que el mayor interes del analisis esta depositado en las diferencias observables durante esta etapa. Como variables explicatorias se incluyeron:
  
  VE1: Tiempo de testeo → cualitativa fija de 3 niveles (3, 24, 48 hs).
VE2: Tratamiento → cualitativa fija de 4 niveles (constante alto, constante bajo, contraste positivo, contraste negativo)
VE3: ID de abeja → cualitativa aleatoria de 132 niveles (abeja 1 a 132).
VE4: Semana de trabajo → cualitativa aleatoria de 7 niveles (semanas 1 a 7). Covariable.

Se implemento un modelo lineal generalizado condicional con la funcion glmmTMB de la librería glmmTMB. Se opto por un modelo condicional ya que se compararon modelos marginales con distintas matrices de correlacion y, a partir de un ranking de QIC (el cual compara modelos según su verosimilitud y cantidad de parametros estimados), el mas conveniente resulto un modelo marginal con matriz de simetria compuesta. Como los modelos condicionales tienen implicita una matriz de simetria compuesta y resultan mas familiares para su implementacion en R, se eligio esta opcion. 

¿¿Acá habría que explicitar el modelo como lo escribimos en la teórica??
  
  Contrastes a priori:
  Se espera que el grupo contraste positivo aprenda la asociacion olor-azucar mas fuertemente que el constante alto, debido a un mayor estado motivacional gracias a la "sorpresa" recibida en la proboscide (azucar 1,5 M) en contraste con el azucar esperada que toco las antenas segundos antes (0,5 M). Caso opuesto, se espera que la proporcion de animales de contraste negativo que aprendan la asociacion sea menor que la proporcion de constante bajo, debido a un estado motivacional degradado por la "decepcion" de recibir azucar 0,5 M cuando esperaban 1,5 M.

# Resultados
Presentar graficos y/o Tablas. Editar lo que sea necesario de formato para que el lector comprenda. Informar medias, magnitud del efecto, letras de significacion cuando corresponda. Supuestos: salvo excepciones, solo mencionarlos y mencionar su cumplimiento. Incluir decisiones metodologicas.

En el primer tiempo de evaluación (3 hs) no se observaron diferencias significativas en los contrastes (p>0,05). 
A 24 hs se observaron diferencias significativas entre los grupos CONSTANTE ALTO y CONTRASTE POSITIVO. Se estima que la chance de extension de proboscide para el grupo CONTRASTE POSITIVO aumenta entre un 2,96% y un 84,2% respecto al grupo CONSTANTE ALTO, con un 95% de confianza (p<0,05). No se observaron diferencias significativas en la comparacion CONSTANTE BAJO vs CONTRASTE NEGATIVO a 24 hs (p>0,05). 
En la evaluacion a 48 hs, se observaron diferencias significativas entre los grupos CONSTANTE BAJO y CONTRASTE NEGATIVO. Se estima que la chance de extension de proboscide para el grupo CONTRASTE NEGATIVO disminuye entre un 27,26% y un 4.720,20% respecto al grupo CONSTANTE BAJO, con un 95% de confianza (p<0,05). No se observaron diferencias significativas en la comparacion CONSTANTE ALTO vs CONTRASTE POSITIVO a 48 hs (p>0,05), aunque la tendencia de las estimaciones coincide con lo observado a 24 hs.

# Discusión o conclusión
Debido a que la interaccion tratamiento*tiempo resultó significativa, se realizaron contrastes ortogonales teniendo en cuenta ambas variables. Si nos situamos en primer lugar en las comparaciones en t = 3 hs, se observa que ninguno de los dos contrastes propuestos mostro diferencias significativas. Lo que es mas curioso aun es que la tendencia de la respuesta parece ser opuesta a la esperada por los contrastes a priori: los grupos contraste negativo y constante bajo son aquellos que mayor proporcion de PER presentaron. Debido a que la memoria observada a las 3 hs posteriores de finalizado el ultimo ensayo de entrenamiento es una memoria de corto término, puede estar influida por diversos fenomenos ajenos al tratamiento aplicado. En particular, se propone que en este punto temporal hay un conflicto en relacion a la expresion de la memoria generada. Los animales de los grupos contraste negativo y constante bajo son los que menos azucar ingirieron (en terminos nutricionales), ya que siempre consumieron azucar de concentración 0,5 M. Por lo tanto, es muy probable que a 3 hs estos animales esten mas motivados que los otros dos grupos y que por ende, lo que parece ser una mayor retencion de la memoria (que solo es posible de observar a traves de la extensión de la proboscide, en este experimento) sea un reflejo de la motivacion de estos animales por seguir ingiriendo azucar. En contraste, las abejas de los grupos contraste positivo y constante alto habrian alcanzado un nivel de saciedad mas alto, respondiendo menos al estimulo (odorante). 
[agregar algunas citas a esto, después busco].

[discusion 24 hs]

El dia siguiente al aprendizaje, se buscó estudiar la consolidación de memoria de largo termino en las abejas. Al hacer las comparaciones a t = 24 hs se observaron diferencias significativas entre los grupos constante alto y contraste positivo. Esto sugiere que un mismatch positivo entre lo que el animal sensa con las antenas y lo que ingiere genera una consolidacion de memoria de largo termino mas robusta, la cual tiene un efecto directo en el comportamiento. Creemos que el animal al sensar con las antenas genera expectativas de lo que va a ingerir y es la sorpresa positiva que siente lo que generaria un estado motivacional que predispone a una mayor retención de la experiencia. Por otro lado al comparar constante bajo con contraste negativo no observamos diferencias significativas. Sin embargo se pudo observar una tendencia que encaja con lo teorizado a priori. Aquellos animales pertenecientes al grupo contraste negativo presentaron una menor proporción de PER que el grupo constante bajo. Esto sugiere que puede haber un efecto en la consolidacion de la memoria de largo termino por mismatch negativo. La abeja al sentir frustrada su expectativa le resta importancia a la experiencia y en consecuencia la memoria no perdura tanto en el tiempo. Es interesante haber observado que no solo es importante que haya contraste o mismatch para generar un efecto diferencial en el comportamiento, sino tambien la valencia del mismatch, siendo la valencia positiva una que genera un mayor efecto diferencial a las 24 hs.

[discusion 48 hs, aca para pos-alto podemos sumar lo de la varianza de las bernoulli y que por eso seria difícil ver diferencias]

48 hs luego de la etapa de aprendizaje se volvio a medir el PER, con el objetivo de analizar como se desarrollaba la memoria de largo termino en los distintos grupos. Contrariamente a lo observado a las 24 hs, no se observaron diferencias significativas entre los grupos constante alto y contraste positivo. Se puede deber a que el grupo contraste positivo habria llegado a un estado asintótico a las 24 hs mientras que el grupo constante positivo, con mayor margen, aumenta  respecto a las 24 hs, reduciendo asi la brecha entre los grupos a las 48 hs. Por otro lado, los grupos constante bajo y contraste negativo presentaron diferencias significativas. Se pudo observar que la sorpresa negativa que sufre el animal tiene un efecto fuerte en la retencion o evocacion de la memoria a las 48 hs. Si bien la memoria del grupo constante bajo decae en el tiempo, la memoria del grupo contraste negativo decae a un ritmo mayor. A diferencia de lo observado a las 24 hs, es la valencia negativa la que genera un mayor efecto diferencial. 

# Bibliografia
1. McFarland, D. J. Decision making in animals. Nature 269, 15–21 (1977).
2. Menzel, R. & Giurfa, M. Dimensions of Cognition in an Insect, the Honeybee. Behav. Cogn. Neurosci. Rev. 5, 24–40 (2006).
3. Gil, M., De Marco, R. J. & Menzel, R. Learning reward expectations in honeybees. Learn. Mem. 14, 491–6 (2007).
4. Rescorla, R. A. A Pavlovian analysis of goal-directed behavior. Am. Psychol. 42, 119–129 (1987).
5. Bitterman, M. E., Menzel, R., Fietz, A. & Schäfer, S. Classical conditioning of proboscis extension in honeybees (Apis mellifera). J. Comp. Psychol. 97, 107–119 (1983).
6. Takeda, K. Classical conditioned response in the honey bee. J. Insect Physiol. 6, 168–179 (1961).

### A) triple interaccion
m1 <- geeglm(formula=rta~ANT*PROB*tiempo_testeo+SEMANA,family=binomial,data=long_testeo,id=ID,
             corstr="independence")
anova(m1)
m2 <- geeglm(formula=rta~ANT*PROB*tiempo_testeo+SEMANA,family=binomial,data=long_testeo,id=ID,
             corstr="exchangeable")
anova(m2)
m3 <- geeglm(formula=rta~ANT*PROB*tiempo_testeo+SEMANA,family=binomial,data=long_testeo,id=ID,
             corstr="ar1")
anova(m3)
m4 <- geeglm(formula=rta~ANT*PROB*tiempo_testeo+SEMANA,family=binomial,data=long_testeo,id=ID,
             corstr="unstructured")
anova(m4)

##### PRIMERA PROPUESTA: BONFERRONI (TENEMOS COMPARACIONES A PRIORI Y BONFERRONI SE PUEDE PONER EN EL EMMEANS)

bonferroni<-emmeans(m9,~TRATAMIENTO*tiempo_testeo,type="response",
                    contr=list("3hs_alto_pos"=c(1,0,0,-1,0,0,0,0,0,0,0,0), 
                               "3hs_bajo_neg"=c(0,1,-1,0,0,0,0,0,0,0,0,0), 
                               "24hs_alto_pos"=c(0,0,0,0,1,0,0,-1,0,0,0,0), 
                               "24hs_bajo_neg"=c(0,0,0,0,0,1,-1,0,0,0,0,0), 
                               "48hs_alto_pos"=c(0,0,0,0,0,0,0,0,1,0,0,-1), 
                               "48hs_bajo_neg"=c(0,0,0,0,0,0,0,0,0,1,-1,0)),
                    adjust="bonferroni")

bonferroni # Nos da la probabilidad para cada grupo y las comparaciones (en odds ratio) porque lo seteamos en options más arriba en el código.
# No da nada significativo pero puede ser porque son muchas comparaciones para Bonferroni (multiplica los p-valores por 6 en este caso).
# Hicimos las comparaciones a mano y las cuentas están bien.

# Podemos graficar los IC y las flechas rojas para comparar.
plot(bonferroni, comparisons=T)
# No sabemos si está comparando bien porque el gráfico no está dividido en las 6 comparaciones.

# FORMA DE JOSÉ:
emm <- emmeans(m9,specs=~TRATAMIENTO*tiempo_testeo)
confint(contrast(emm, method = list("3hs_alto_pos"=c(1,0,0,-1,0,0,0,0,0,0,0,0), 
                                    "3hs_bajo_neg"=c(0,1,-1,0,0,0,0,0,0,0,0,0), 
                                    "24hs_alto_pos"=c(0,0,0,0,1,0,0,-1,0,0,0,0), 
                                    "24hs_bajo_neg"=c(0,0,0,0,0,1,-1,0,0,0,0,0), 
                                    "48hs_alto_pos"=c(0,0,0,0,0,0,0,0,1,0,0,-1), 
                                    "48hs_bajo_neg"=c(0,0,0,0,0,0,0,0,0,1,-1,0))))


##### TERCERA PROPUESTA: EFECTOS SIMPLES (COMPARACIÓN DE TODOS LOS TRATAMIENTOS PARA CADA TIEMPO)

ef_simples<-emmeans(m9,pairwise~TRATAMIENTO|tiempo_testeo,type="response")

ef_simples # A las 3hs no da nada significativo. A las 24hs tampoco. A las 48hs dan significativas las comparaciones constante alto vs contraste negativo y contraste negativo vs contraste positivo. No nos interesan mucho esas comparaciones.

plot(ef_simples, comparisons=T) 


##### CUARTA PROPUESTA: TUKEY (TODOS CONTRA TODOS, NO NOS INTERESA EN REALIDAD PERO PARA PROBAR. SI DA ALGO SIGNIFICATIVO ACÁ ES PORQUE ESTAMOS HACIENDO MAL LOS OTROS CONTRASTES)

tukey<-emmeans(m9,pairwise~TRATAMIENTO*tiempo_testeo,type="response")

tukey # Dan significativas solo comparaciones entre tiempos y contraste negativo vs contraste positivo a las 48hs.

plot(tukey, comparisons=T) 

#*******************************************************************************
############################ SIN 3HS ###########################################
#*******************************************************************************

# Dado que 3hs no presenta diferencias, vamos a probar hacer el modelo sin los datos de las 3hs para ver si aumentamos la potencia y obtenemos nuevas comparaciones significativas. Esto es porque creíamos que a las 3hs había "ruido". Sin embargo, luego de una charla con el director de Mili llegamos a la conclusión de que la info a las 3hs sí nos interesa porque es para ver si hay memoria a corto término. Que dé NS es información.

# Eliminamos las columnas de 3hs
wide_testeo_sin3 <- datos[,c(1,2,3,4,5,11,12)]

# Pasamos a formato long la base de datos
long_testeo_sin3 <- melt(wide_testeo_sin3,
                         id.vars = c("SEMANA", "ID", "ANT", "PROB", "TRATAMIENTO"),
                         variable.name = "tiempo_testeo",
                         value.name = "rta")

# Modelamos los datos sin 3hs con el mismo modelo
m9_sin3 <- glmmTMB(rta ~ TRATAMIENTO*tiempo_testeo + SEMANA + (1|ID), data=long_testeo_sin3, family="binomial")

# Estimación e inferencia
Anova(m9_sin3) # La interacción tratamiento*tiempo no da significativa así que podemos evaluar los tratamientos y el tiempo por separado. El efecto del tiempo no dio significativo. El efecto de los tratamientos sí sio significativo.

######################### COMPARACIONES

#### Efectos principales para tratamientos

ef_ppales_trat_sin3<-emmeans(m9_sin3,pairwise~TRATAMIENTO,type="response")

ef_ppales_trat_sin3 # Da solo significativa la comparación de contraste negativo vs contraste positivo.

plot(ef_ppales_trat_sin3, comparisons=T)


#### Contrastes ortogonales

ortogonales_sin3<-emmeans(m9_sin3,~TRATAMIENTO*tiempo_testeo,type="response",
                          contr=list("24hs_alto_pos"=c(1,0,0,-1,0,0,0,0), 
                                     "24hs_bajo_neg"=c(0,1,-1,0,0,0,0,0), 
                                     "48hs_alto_pos"=c(0,0,0,0,1,0,0,-1), 
                                     "48hs_bajo_neg"=c(0,0,0,0,0,1,-1,0)))

ortogonales_sin3 # Dan significativas las comparaciones constante alto vs contraste positivo a las 24 hs y constante bajo vs contraste negativo a las 48 hs. Igual que cuando hacíamos comparaciones ortogonales conservando los datos de las 3hs.


#### Tukey

tukey_sin3<-emmeans(m9_sin3,pairwise~TRATAMIENTO*tiempo_testeo,type="link")

tukey_sin3 # Dan significativas solo las comparaciones contraste positivo 24 hs vs contraste negativo 48 hs y contraste negativo vs contraste positivo ambos a 48 hs.

plot(tukey_sin3, comparisons=T)


#### Efectos simples

ef_simples_sin3<-emmeans(m9_sin3,pairwise~TRATAMIENTO|tiempo_testeo,type="link")

ef_simples_sin3 # A las 24 hs no dan significativas las comparaciones y las 48 hs dan significativas las comparaciones constante alto vs contraste negativo y contraste negativo vs contraste positivo. Ninguna nos interesa mucho.


#**************************** EN CONCLUSIÓN ************************************

# Sacar los datos de 3hs no ayudó. En los contrastes ortogonales dieron significativas las mismas comparaciones que cuando dejábamos las 3hs. Hasta sucede que cuando dejamos las 3hs, los p-valores de las comparaciones son menores, lo que nos lleva a pensar que en realidad sacando las 3hs perdemos potencia porque eliminamos muchas observaciones y la disminución de parámetros estimados no llega a compensarlas. Entonces, decidimos quedarnos con los datos de las 3hs que encima nos aportan información de que a las 3hs (memoria a corto término) no hay diferencias significativas entre los grupos.

# Pensamos en quedarnos con los contrastes ortogonales (con los datos que incluyen las 3hs) porque son los contrastes que queríamos hacer a priori y los resultados podrían tener una explicación biológica. Habría que confirmar que la línea de los contrastes ortogonales está bien hecha.

# Son los contrastes que queríamos hacer porque en otros experimentos que hizo Mili observó que las abejas aprenden más en el tiempo si son alimentadas con mayor concentración de azúcar, hasta llegar a un techo (también hay un piso). Entonces, tiene sentido comparar a los grupos que fueron alimentados con la misma concentración de azúcar en el entrenamiento.

# Pensamos que la comparación constante alto vs contraste positivo a las 48 hs no está dando significativa porque el control (constante alto a las 48 hs) tiene probabilidad muy cercana a 0,5, lo que hace que su varianza sea enorme y haya menor potencia en la comparación.

