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