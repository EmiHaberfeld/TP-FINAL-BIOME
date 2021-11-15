
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

