# RESOLUCION GUIA 1

# ---- Setup ----

data_dir = "~/MateriasExactas/2c2025/Estadistica/Datos"

# ---- Ejercicio 1 ----

file_name = "Debernardi.csv"
file_path = paste(data_dir, file_name, sep="/")

debernardi_df = read.csv(file_path, header=TRUE)
colnames(debernardi_df)

# a - Crear una tabla con los valores observados para "diagnosis"y su frecuencia relativa

debernardi_df$diagnosis <- factor(debernardi_df$diagnosis,
                                              levels = c(1, 2, 3),                # los valores originales
                                              labels = c("Control", "Benigno", "ACPD"))  # los nombres que le pongo

#tabla
table(debernardi_df["diagnosis"])
#tabla con frecuencia relativa
proporcion_diagnosis = prop.table(table(debernardi_df$diagnosis))

# b - realizar un grafico de barra con lo anterior
barplot(proporcion_diagnosis, main="Proporcion de por diagnostico",
        ylab="proporcion",
        xlab="diagnostico")

# ---- Ejercicio 2 ----

file_name = "datos_titanic.csv"
file_path = paste(data_dir, file_name, sep="/")
titanic_df = read.csv(file_path)
colnames(titanic_df)

# a - Estimar la probabilidad de ser mujer sabiendo que sobrevivió y comparar con la estimación de ser mujer a bordo del Titanic.

# una proba condicional para el caso finito

titanic_survived <- titanic_df[titanic_df$Survived==1,]
titanic_survived_prop <- prop.table(table(titanic_survived$Sex))
barplot(titanic_survived_prop, main="Proporcion por sexo de las personas que sobrevivieron",
        xlab="Sexo",
        ylab="Proporcion")

# cuantas mujeres estaba a bordo?

prop.table(table(titanic_df$Sex))

# algo asi como 0.35 proba de ser mujer, pero 0.68 de sobrevivir si se lo era: "mujeres y ninios primero"

# b - Hacer una tabla de contingencia entre las variables categóricas Survived y Pclass. A
# partir de esta tabla estimar la probabilidad de sobrevivir dada la clase para los distintos
# valores de la variable Pclass.

con_table = prop.table(table(titanic_df[,c("Survived", "Pclass")]))
con_table

# proba de sobrevivir dado una clase? P(A|B) = P(A y B) / P(B)
p_survive_given_class <- function(t_class) {
  p <- con_table[2, t_class] / sum(con_table[, t_class]) #fila de survived y col clase dividio la propa total de ser de tal clase
  return(p)
}

p_survive_given_class(1)
p_survive_given_class(2)
p_survive_given_class(3)

# c - hacer un grafico de barras que vincule Survived y Pclass
barplot(con_table,
        beside = FALSE,           # barras apiladas
        col = c("skyblue", "orange"), # colores para Survived
        legend = TRUE,
        xlab = "Clase de pasajero",
        ylab = "Proporción",
        main = "Proporción de sobrevivientes por clase")


# ---- Ejercicio 3 ----

iridio_df <- read.csv(paste(data_dir, "iridio.txt", sep="/"), sep=" ", header=TRUE)
rodio_df <- read.csv(paste(data_dir, "rodio.txt", sep="/"), sep=" ", header=TRUE)

colnames(iridio_df)
head(iridio_df)
colnames(rodio_df)
head(rodio_df)

# a - Comparar los dos conjuntos de datos mediante histogramas y boxplots, graficando los boxplots en paralelo.
boxplot(iridio_df[[1]], rodio_df[[1]],
        name=c("Iridio", "Rodio"),
        main="Boxplots de temperatura de sublimacion observada",
        xlab="Temperatura",
        horizontal=TRUE)

# b - Hallar las medias, las medianas y las medias podadas al 10 % y 20 % muestrales. Comparar.

summary(iridio_df)
summary(rodio_df)

# c - Hallar los desvı́os estándares, las distancias intercuartiles y las MAD muestrales como medidas de dispersión.
IQD_iridio = quantile(iridio_df[[1]], 0.75) - quantile(iridio_df[[1]], 0.25)
IQD_rodio = quantile(rodio_df[[1]], 0.75) - quantile(rodio_df[[1]], 0.25)

std_iridio = sqrt(var(iridio_df[[1]]))
std_rodio = sqrt(var(rodio_df[[1]]))

MAD_iridio = mad(iridio_df[[1]])
MAD_rodio = mad(rodio_df[[1]])

# d - Hallar los cuantiles 0.90, 0.75, 0.50, 0.25 y 0.10.

print("rodio")
for(q in c(0.90, 0.75, 0.60, 0.25, 0.10)) {
  print(paste("Cuantil", q, sep=" "))
  print(quantile(rodio_df[[1]], q))
}

print("iridio")
for(q in c(0.90, 0.75, 0.60, 0.25, 0.10)) {
  print(paste("Cuantil", q, sep=" "))
  print(quantile(iridio_df[[1]], q))
}

# ---- Ejercicio 4 ----

salchichas_A_df = read.table(paste(data_dir, "salchichas_A.txt", sep="/"), sep="\t", header=TRUE)
salchichas_B_df = read.csv(paste(data_dir, "salchichas_B.txt", sep="/"), sep="\t", header=TRUE)
salchichas_C_df = read.csv(paste(data_dir, "salchichas_C.txt", sep="/"), sep="\t", header=TRUE)

head(salchichas_A_df)

# a - armar un archivo con los datos de los tres a la vez

salchichas_A_df$tipo = 'A'
salchichas_B_df$tipo = 'B'
salchichas_C_df$tipo = 'C'

colnames(salchichas_A_df) = c("CALORIAS", "SODIO", "TIPO")
colnames(salchichas_B_df) = c("CALORIAS", "SODIO", "TIPO")
colnames(salchichas_C_df) = c("CALORIAS", "SODIO", "TIPO")

salchichas_df = merge(salchichas_A_df, salchichas_B_df, by=c("CALORIAS", "SODIO", "TIPO"), all.x=TRUE, all.y=TRUE)
salchichas_df = merge(salchichas_df, salchichas_C_df, by=c("CALORIAS", "SODIO", "TIPO"), all.x=TRUE, all.y=TRUE)

rownames(salchichas_df)

write.csv(salchichas_df, paste(data_dir, "salchichas.csv", sep="/"), row.names=FALSE)

# b - Realizar un histograma para las calorı́as de cada tipo de salchichas. ¿Observa grupos
# en algún gráfico? ¿Cuántos grupos observa? ¿Observa algún candidato a dato atı́pico?
# ¿Alguno de los histogramas tiene una caracterı́stica particular?

library(ggplot2)

ggplot(salchichas_df, aes(x = CALORIAS, fill = TIPO)) +
  geom_histogram(position = "dodge", color = "black", bins = 20) +
  theme_minimal() +
  labs(title = "Distribución de Calorías por tipo de salchicha",
       x = "Calorías",
       y = "Frecuencia")

# Parece que los tres tipos tienen su media en torno a las 130-140 calorias pero las salchichas
# de tipo A Y B tienden a tomar valores mas altos cuando no estan centrados, mientras que las de tipo C se van a mas bajos
# de cualquier manera, parecen haber tres grupos diferenciados:
# unas salchichas con calorias menores a 125
# unas con calorias entre 125 y 170
# unas con calorias entre 170 y 200
# No parecen haber datos atipicos en ese sentido. Los ultimos 2 grupos parecen tener una distribucion aproximadamente normal, mientras
# que el primero es mas extranio

# c - Realizar los boxplots paralelos para las calorı́as. ¿Observa la misma cantidad de grupos
# que antes? ¿A cuál conclusión llega? De acuerdo con los boxplots graficados, ¿cómo
# caracterizarı́a la diferencia entre los tres tipos de salchichas desde el punto de vista de
# las calorı́as?

boxplot(salchichas_df[salchichas_df$TIPO == "A","CALORIAS"],
        salchichas_df[salchichas_df$TIPO == "B","CALORIAS"],
        salchichas_df[salchichas_df$TIPO == "C","CALORIAS"],
        names=c("A", "B", "C"),
        main="Boxplots de las calorias por tipo de salchicha",
        xlab="Calorias",
        ylab="Tipo",
        horizontal=TRUE)

boxplot(CALORIAS ~ TIPO, 
        data = salchichas_df,
        main="Boxplots de las calorías por tipo de salchicha",
        xlab = "Calorias",
        ylab = "Tipo",
        horizontal=TRUE)

#Usando los boxplots no se ve la separacion en tres grupos de antes
#Aunque nuevamente, los tipos A Y B tienden a valores mas altos, mientras que las C a mas bajos
#Si nos basamos solo en eso, en verdad A y B podrian ser un unico grupo, pero si nos basamos en los histogramas,
#Hay tres grupos bien diferenciados que no se condicen con las etiquetas A, B y C 

# d - Repetir con la cantidad de sodio.

ggplot(salchichas_df, aes(x = SODIO, fill = TIPO)) +
  geom_histogram(position = "dodge", color = "black", bins = 20) +
  theme_minimal() +
  labs(title = "Distribución de Calorías por tipo de salchicha",
       x = "Calorías",
       y = "Frecuencia")

#En el histograma se ve que estan bastante superpuestos, todos entre 200 y 650 calorias, parece tener una distribucion normal en ese rango.
#No obstante, hay una salchicha de tipo B que tiene menos de 150gm de sodio, un outlier.

boxplot(SODIO ~ TIPO, 
        data = salchichas_df,
        main="Boxplots del sodio por tipo de salchicha",
        xlab = "Sodio",
        ylab = "Tipo",
        horizontal=TRUE)

# los boxplots nos muestra una imagen similar: los tres tipos de salchicha toman valores parecidos y B tiene un outlier.
# No obstante, en este caso se ve mas claramente que mientras que B y C son bastante parecidos, los bigotes
# del boxplot de A son mucho mas grandes que los del resto, tiene una mayor varianza.

# ---- EJERCICIO 5 ----

estudiantes_df = read.csv(paste(data_dir, "estudiantes.txt", sep="/"), header=TRUE, sep="\t")
head(estudiantes_df)
# 
# hist(estudiantes_df$GRUPO1, main="Distribucion de las mediciones realizadas por el grupo 1", xlab="valor medido", ylab="Frecuencia")
# hist(estudiantes_df$GRUPO2, main="Distribucion de las mediciones realizadas por el grupo 2", xlab="valor medido", ylab="Frecuencia")

# 1 fila, 2 columnas → dos gráficos uno al lado del otro
par(mfrow = c(1, 2))

hist(estudiantes_df$GRUPO1,
     main = "Distribución grupo 1",
     xlab = "Valor medido",
     ylab = "Frecuencia")

hist(estudiantes_df$GRUPO2,
     main = "Distribución grupo 2",
     xlab = "Valor medido",
     ylab = "Frecuencia")

# ambas distribuciones se ven normales
summary(estudiantes_df)

# la del grupo 2 tiende a valores mas altos que la del grupo 1

# Dos gráficos lado a lado
par(mfrow = c(1, 2))

# QQ-plot grupo 1
qqnorm(estudiantes_df$GRUPO1,
       main = "QQ-plot Grupo 1")
qqline(estudiantes_df$GRUPO1, col = "red")

# QQ-plot grupo 2
qqnorm(estudiantes_df$GRUPO2,
       main = "QQ-plot Grupo 2")
qqline(estudiantes_df$GRUPO2, col = "red")

# Podemos ver que los datos del grupo 2 se ajustan bien al hacer un qqplot a una recta, mientras
# que los datos del grupo 1 parecen ser escalonados: crecients y constantes en grandes intervalos
# Pareciese que los datos del grupo 1 no son naturales o estan midiendo cosas distintas que el grupo 2

# volver a layout normal
par(mfrow = c(1, 1))

# b - ¿Le parece a partir de estos datos que ambos grupos están midiendo lo mismo? Respon-
# der comparando medidas de centralidad y de dispersión de los datos. Hacer boxplots
# paralelos.

print(paste("Media del grupo 1: ", mean(estudiantes_df$GRUPO1)))
print(paste("Desvio estandar del grupo 1: ", sd(estudiantes_df$GRUPO1)))
      
print(paste("Media del grupo 2: ", mean(estudiantes_df$GRUPO2)))
print(paste("Desvio estandar del grupo 2: ", sd(estudiantes_df$GRUPO2)))

#La media del grupo 1 y su desvio estandar son ambos menores que los corresppondientes del grupo 2

boxplot(estudiantes_df$GRUPO1,
        estudiantes_df$GRUPO2,
        names=c("1","2"),
        main="Boxplots de las mediciones por grupo",
        xlab="Medicion",
        ylab="Grupo",
        horizontal=TRUE)

# Los boxplots confirman lo visto en las medidas resumen, la media de la medicion del grupo 2 es mucho mayor que las del 1 y ademas tiene mucha mas varianza
# Los datos del grupo 1 varian muy poco

# ---- EJERCICIO 6 ----

nubes_df = read.csv(paste(data_dir, "nubes.txt", sep="/"), header=TRUE, sep="\t")
head(nubes_df)

# a - hacer boxplots paralelos, parece que hay efectos con este metodo?
boxplot(nubes_df$CONTROLES,
        nubes_df$TRATADAS,
        names=c("CONTROL","TRATADA"),
        main="Boxplots de la cantidad de agua caida por cada nube",
        xlab="Tipo de nube",
        ylab="Cantidad de agua caida",
        horizontal=TRUE)

# Ambos grupos cuentan con datos atipicos con valores bastante superiores a la media, pero aun asi,
# La mayoria de las nubes tratadas tienden a tener valores mas altos que los de control.
# De hecho, la mediana de las nubes tratadas es superior al tercer cuantil de las nubes de control
# O sea que el 50% de las nubes tratadas emitieron mas lluvia que el 75% de las de control

# b 

# Dos gráficos lado a lado
par(mfrow = c(1, 2))

# QQ-plot grupo 1
qqnorm(nubes_df$CONTROLES,
       main = "QQ-plot Grupo de control")
qqline(nubes_df$CONTROLES, col = "red")

# QQ-plot grupo 2
qqnorm(nubes_df$TRATADAS,
       main = "QQ-plot Grupo de Tratadas")
qqline(nubes_df$TRATADAS, col = "red")

# Ninguna parece seguir una distribucion normal

# c

nubes_df_ln = log(nubes_df)

# Dos gráficos lado a lado
par(mfrow = c(1, 2))

# QQ-plot grupo 1
qqnorm(nubes_df_ln$CONTROLES,
       main = "QQ-plot Grupo de control Ln")
qqline(nubes_df_ln$CONTROLES, col = "red")

# QQ-plot grupo 2
qqnorm(nubes_df_ln$TRATADAS,
       main = "QQ-plot Grupo de Tratadas Ln")
qqline(nubes_df_ln$TRATADAS, col = "red")

#Si tomamos logaritmo, se ve que parecen seguir una distribucion normal

#d 
par(mfrow=c(1,2))
boxplot(nubes_df$CONTROLES,
        nubes_df$TRATADAS,
        names=c("CONTROL","TRATADA"),
        main="datos originales",
        xlab="Tipo de nube",
        ylab="Cantidad de agua caida",
        horizontal=TRUE)

boxplot(nubes_df_ln$CONTROLES,
        nubes_df_ln$TRATADAS,
        names=c("CONTROL","TRATADA"),
        main="datos transformados",
        xlab="Tipo de nube",
        ylab="Cantidad de agua caida",
        horizontal=TRUE)

#se veq que los outliers pasaron a ser aquellos que toman valors mas bajos

# ---- EJERCICIO 7 ----

credit_card_df = read.csv(paste(data_dir, "data_credit_card.csv", sep="/"), header=TRUE)
head(credit_card_df)
# a - graficar la distribucion empirica para cada variable

par(mfrow=c(2, 2))

for(variable in colnames(credit_card_df)) { #son 4 btw
  plot(density(credit_card_df[, variable]), 
               main = paste("KDE empirica de ", variable),
               xlab = "Valor medido",
               ylab = "Densidad")
}

# tenure parece seguir una clara normal con media en el 12
par(mfrow=c(1,1))
hist(credit_card_df$tenure, bin=6)
# Pero si vemos el histograma (son valores discretos), no parece ser el caso
# Podria llegar a modelarse como una geometrica: muy alta proba en torno al 12, pero decrece rapido a medida que se aleja

# credit_limit tambien pare ser aproximable por una normal, aunque 
# a la derecha vemos que no baja de manera "suave", no es simetrica
par(mfrow=c(1,1))
hist(credit_card_df$credit_limit)

# viendo el limite de credito, se ve clarament que esa es una exponencial (tomemos valores continuos)
# aunque hay varios valores en torno al 15000

# purchases concentra casi todos sus valores en torno al 0, con KDE pareciese que se puede aproximar con una normal en ese entorno
par(mfrow=c(1,1))
hist(credit_card_df$purchases, breaks=100)
# Pero con el histograma se ve claramente exponencial

# finalmente, purchases_freq parece tener ocultas dos normales, una en torno al 0 y otra en torno al 1
par(mfrow=c(1,1))
hist(credit_card_df$purchases_freq, breaks=30)

# Con el histograma se ve que concentra mucha probabilidad en los extremos, pero en
# el medio es mas o menos uniforme. No conozco una distribucion generica para modelarlo.
# Nada, seria escribir una f de densidad a mano con esta pinta y chau.
plot(density(credit_card_df$purchases_freq, kernel="rectangular"))

# b - Para la variable credit limit hacer un histograma y un gráfico de densidad usando
#la funcion density, ¿Qué observa? ¿Le parece adecuado realizar estos gráficos para las
#variables purchases y tenure?

# ya lo hice antes: hay algunas variables que parece que toman cierta distribucion viendo
# el KDE, pero si vemos el histograma nos damos cuenta que en verdad convendria
# modelarlas con otras

# c - Para la variable tenure hacer un barplot con las frecuencias relativas de cada valor.
# ¿Qué observa?
par(mfrow=c(1,1))
barplot(prop.table(table(credit_card_df$tenure)))

# casi toda la probabilidad (aprox 80%) se concentra en 12 meses

# d - Para todas las variables, calcular la media, la mediana y la media α−podada (con
# α =0.1). Comparar los resultados y justificar. ¿Qué medida de posición del centro de
# los datos le parece más adecuada en cada caso?

par(mfrow = c(2, 2))

for (variable in colnames(credit_card_df)) {
  x <- credit_card_df[[variable]]
  
  # medidas
  m <- mean(x, na.rm = TRUE)
  med <- median(x, na.rm = TRUE)
  mt <- mean(x, trim = 0.1, na.rm = TRUE)  # media 10% podada
  
  # KDE
  plot(density(x, na.rm = TRUE),
       main = paste("KDE empírica de", variable),
       xlab = "Valor medido",
       ylab = "Densidad")
  
  # líneas
  abline(v = m,   col = "red",  lwd = 2, lty = 2) # media
  abline(v = med, col = "blue", lwd = 2, lty = 3) # mediana
  abline(v = mt,  col = "darkgreen", lwd = 2, lty = 4) # media 0.1-podada
  
  # leyenda
  legend("topright",
         legend = c("Media", "Mediana", "Media 0.1-podada"),
         col = c("red", "blue", "darkgreen"),
         lty = c(2, 3, 4),
         lwd = 2,
         bty = "n", cex = 0.8)
}


# Dado las observaciones que hicimos antes para cada variable, podriamos tomar:
# Para purchases, cualquiera, dan muy similar, cerquita de 0. Quiza la mediana porque es la mas pequenia.
# Para credit_limit, parece exponencial, por LGN la media deberia ser parecida a la esperanza,
# aunque las otras medidas de centralidad son similares tambien
# Para purchases_freq, la probabilidad se centraba en los bordes, mientras que en el medio era aproximadamente uniforme.
# Su centro esta en el medio, la media o la media 0.1 podada son buenas, aunque
# la mediana refleja que hay una tendencia ligera a tirar mas para el 0
# Para tenure, vimos que el 80% de la proba cae en el 12, el resto se distribuye mas o menos uniforme.
# la media podada y la mediana dan al 12 como valor central.

# e - Para todas las variables, obtener los cuantiles de nivel 0.25 y 0.75 de los datos. Calcular
# el rango inter-cuartı́lico y la MAD muestrales. Graficar boxplots. ¿Qué observa?

par(mfrow = c(2, 2))

for (variable in colnames(credit_card_df)) {
  x <- credit_card_df[[variable]]
  
  
  # KDE
  boxplot(x,
       main = paste("Boxplot de", variable),
       xlab = "Valor medido", horizontal=TRUE)
  

}

# - purchases se encuentra mega concentrado hacia valores bajos y tiene 
# muchos outliers con valores altos (cierra si lo aproximamos con una exponencial)
# - credit_limit tambien es similar, pero no tiene outliers tan ruidosos
# como los de purchases
# - purchases_freq tiene un rango intercuantil muy grande, muchos datos caen ahi,
# vimos en los histogramas que se distribuian aproximadamente uniformemente entre el 0 y el 1,
# pero cerca de ellos, la probabilidad aumenta mucho. Se refleja en que los bigotes del boxplot 
# son cortitos
# - tenure es horrible. Y si, tiene el 80% de proba en un bin y el resto valen re poco.

# f - Calcular el desvı́o estándar, el coeficiente de asimetrı́a y el coeficiente de curtosis mues-
# trales. Interpretar los resultados en relación a las distribuciones vistas.

# q conio es eso?
# 
# 📌 Coeficiente de asimetría (skewness)
# 
# Mide si la distribución está sesgada hacia la izquierda o hacia la derecha respecto de la media.
# 
# Valor ≈ 0 → simétrica (como la normal).
# 
# Valor > 0 → cola más larga a la derecha (asimetría positiva).
# 
# Valor < 0 → cola más larga a la izquierda (asimetría negativa).


# 📌 Coeficiente de curtosis (kurtosis)
# 
# Mide el apuntamiento o achatamiento de la distribución en comparación con la normal.
# 
# Normal → curtosis ≈ 3 (esto se llama mesocúrtica).
# 
# > 3 → más “picuda” y con colas más pesadas (leptocúrtica).
# 
# < 3 → más “aplanada” (platicúrtica).
# 
# A veces se reporta exceso de curtosis = curtosis − 3 (para que la normal dé 0).

library(moments)

for (variable in colnames(credit_card_df)) {
  x <- credit_card_df[[variable]]
  print(paste("Medidas para", variable))
  print(paste("El desvio estandar es", sd(x)))                # desvío estándar
  print(paste("El coeficiente de asimetria s", skewness(x)))           # coeficiente de asimetría
  print(paste("El coeficiente de curtosis es", kurtosis(x)))          # coeficiente de curtosis
  print("")
}

# y bueno, siguiendo las interpretaciones de los comentarios de arriba,
# se condice con lo observado en items anteriores

# g) Identificar datos atı́picos. ¿Deberı́an excluirse? ¿Cómo se modifican las medidas obte-
# nidas anteriormente si se los excluye?

# los boxplot dan muchos datos atipicos en purchases y credit_limit, pero si lo modelamos como
# una exponencial, es posible que aparezan. Excluirlos no cambiaria cualitativamente la eleccion del modelo en ese caso
# las medidas de asimetria y curtosis serian mas pequenias, eso si.

# respecto de tenure, la cantidad de meses que le quedan al cliente para cancelar el credito,
# no tendria mucho sentido excluir esos dtos.


par(mfrow=c(1,1))

# ---- EJERCICIO 8 ----

library(readxl)

ciclo_combinado_df <- read_excel(paste(data_dir, "ciclocombinado.xlsx", sep="/"))
head(ciclo_combinado_df)

# a - Realizar un histograma y un gráfico density con los datos de PE, ¿Qué se observa?

par(mfrow=c(1,2))
hist(ciclo_combinado_df$PE)
plot(density(ciclo_combinado_df$PE))

# A simple vista, el histograma parece seguir una distribucion normal,
# pero viendo el KDE, se nota que parecen haber dos rangos que concentran bastante probabildiad
# Uno en torno al 440 y otro en torno al 470

# b - Clasificar los datos en dos vectores según la variable HighTemp y realizar gráficos
# density separados. Visualizar simultáneamente los gráficos en la misma escala. ¿Qué
# se observa?

high_temp_df = ciclo_combinado_df[ciclo_combinado_df$HighTemp==1, "PE"]
low_temp_df = ciclo_combinado_df[ciclo_combinado_df$HighTemp==0, "PE"]
plot(density(high_temp_df[[1]]),
     main="KDE para observacions con alta temperatura")
plot(density(low_temp_df[[1]]),
     main="KDE para observacions con baja temperatura")

# Los dias con alta temperatura terminaron con registros centrados
# en torno al 440, mientras que los de baja en torno al 470.
# Por eso el KDE original mostraba dos picos

# c - Estimar P (PE < 450|HighTemp = 0) y P (PE < 300|HighTemp = 1).

#el caso menor a 300 para high temp = 1 da 0

#para el caso de high temp = 0, intengro 

dens_low_temp <- density(low_temp_df[[1]])

fun_dens_low_temp <- approxfun(dens_low_temp$x, dens_low_temp$y)  # función interpolada de la densidad

#aproximo entre 430 y 450, total antes da 0
integrate(fun_dens_low_temp, lower = 430, upper =450)$value
#algo as como 4%

# d - Estimar P (PE < 450).
# Nada, proba total
# P (PE < 450) = P (PE < 450 | highTemp) P(highTemp) + P (PE < 450 | lowTemp) P(lowTemp)

dist_temp = prop.table(table(ciclo_combinado_df$HighTemp))
p_high_temp <- dist_temp["1"][[1]]
p_low_temp <- dist_temp["0"][[1]]
p_given_low_temp <- 0.04 #era algo asi

dens_high_temp <- density(high_temp_df[[1]])
fun_dens_high_temp <- approxfun(dens_high_temp$x, dens_high_temp$y)  # función interpolada de la densidad
#aproximo entre 410 y 450, total antes da 0
p_given_high_temp <- integrate(fun_dens_high_temp, lower = 420, upper =450)$value

p = p_given_high_temp * p_high_temp + p_given_low_temp * p_low_temp

p # algo asi como 0.47

#e - Estimar la potencia mı́nima garantizada con probabilidad 0.9 para un cierto dı́a con
# Hightemp = 1

# Busoc X tal que
# P(PE > a  | highTemp) >= 0.9

a=470
b=470
p <- 0
while(p < 0.9) {
  a <- a - 1
  p <- integrate(fun_dens_high_temp, lower = a, upper = b)$value
}
p
a
print(paste("Con probabilidad", p, "se obtendra un PE mayor a", a))

#f - Estimar la potencia mı́nima garantizada con probabilidad 0.9 para un cierto dı́a.
# Nada, para un dia en gneal hay que usar bayes/proba condicional. Es definir una funcion y hacer lo mismo que recine, chau

# ---- EJERCICIO 9 ----


file_name = "Debernardi.csv"
file_path = paste(data_dir, file_name, sep="/")

debernardi_df = read.csv(file_path, header=TRUE)

head(debernardi_df)

# a - hacer histograma de LYVE1 discriminando por Diagnosis

par(mfrow=c(1,1))

debernardi_df$diagnosis <- as.factor(debernardi_df$diagnosis)

library(ggplot2) 
ggplot(debernardi_df, 
       aes(x = LYVE1, fill = diagnosis)) + 
  geom_histogram(position = "stack", color = "black", bins = 20) + 
  theme_minimal() + labs(title = "Distribución de LYVE1 por tipo de diagnostico", x = "LYVE1", y = "Frecuencia")

# Graficar, en distintos colores y superpuestas, las funciones de distribución empı́ricas
# de la variable LYVE1 según los niveles de la variable factor diagnosis. Decidir si la
# siguiente afirmación es verdadera o falsa y justificar: “los valores de la variable LYVE1
# tienden a ser más altos entre quienes tienen cáncer de páncreas que entre quienes sufren
# otras enfermedades asociadas al páncreas”.


ggplot(debernardi_df, aes(x = LYVE1, color = diagnosis)) +
  geom_density(size = 1) +
  theme_minimal() +
  labs(title = "Densidad de LYVE1 por diagnóstico",
       x = "LYVE1", y = "Densidad")

# 1 no tiene cancer de pancreas - linea roja
# 2 tiene otra enfermedad - linea verde
# 3 tiene cancer de pancreas - linea azul

# es correcta, para valores "altos", los tipo 1 decaen rapido, los tipo 2 decaen lento pero los tipo 3 decaen aun mas lento

# c - Realizar boxplots paralelos para la variable LYVE1 según los niveles de la variable
#factor diagnosis, considerando el sexo de los pacientes (variable sex). Decidir si la
#siguiente afirmación es verdadera o falsa y justificar: “en términos generales, el sexo del
#paciente no afecta los niveles de la proteı́na que se mide en la variable LYVE1”. 

par(mfrow=c(3,1))

boxplot(LYVE1 ~ sex, 
        data = debernardi_df[debernardi_df$diagnosis==1, ],
        main="Boxplots de los niveles de LYVE1 con diganostico 1 por sexo",
        xlab = "Nivel medido",
        ylab = "Sexo",
        horizontal=TRUE)


boxplot(LYVE1 ~ sex, 
        data = debernardi_df[debernardi_df$diagnosis==2, ],
        main="Boxplots de los niveles de LYVE1 con diganostico 2 por sexo",
        xlab = "Nivel medido",
        ylab = "Sexo",
        horizontal=TRUE)


boxplot(LYVE1 ~ sex, 
        data = debernardi_df[debernardi_df$diagnosis==3, ],
        main="Boxplots de los niveles de LYVE1 con diganostico 3 por sexo",
        xlab = "Nivel medido",
        ylab = "Sexo",
        horizontal=TRUE)

# para diagnosticos de tipo 3 (cancer de pancreas), no hay grandes diferencias
# pero para diagnosticos de tipo 1 (no tienen cancer de pancreas), las mujeres tienen
# tienden a tener valores menores que los hombres
# en el tipo 2 ocurre similar, aunque de manera menos abrupta

# la afirmacion es falsa.

