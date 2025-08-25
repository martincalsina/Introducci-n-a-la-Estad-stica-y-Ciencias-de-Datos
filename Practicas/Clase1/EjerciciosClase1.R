#Ejercicios de la clase 1 de estadística

#Ejercicio 1

# a

t <- 0
for(i in 1:1000) {
  t <- t+i
}
t

#b

sumaHastaN <- function(n) {
  suma <- 0
  for(i in 1:n) {
    suma <- suma + i
  }
  return(suma)
}

n <- 1
while(TRUE) {
  if (sumaHastaN(n) > 1000) {
    break
  }
  n <- n+1
}
n

#c

sumaCoordsPositivas <- function(arr) {
  sumaPositivos <- 0
  for(i in 1:length(arr)) {
    if (arr[i] > 0) {
      sumaPositivos <- sumaPositivos + arr[i]
    }
  }
  return(sumaPositivos)
}


sumaCoordsPositivas(myArr)

#EJERCICIO 2

funcionAGraficar <- function(p) {
  return(p * (1-p))
}

dominio = seq(0, 1, 0.05)

plot(dominio, funcionAGraficar(dominio))

#EJERCICIO 3

dominio = seq(0, 2*pi, 2*pi/100)

plot(dominio, sin(dominio), col="blue", type="l")
lines(dominio, cos(dominio), col="red", lwd=2)
lines(dominio, cos(dominio^2), col="green", lwd=2)

legend("topright",
       legend = c("sin(x)", "cos(x)", "cos(x^2)"),
       col = c("blue", "red", "green"),
       lty = 1,
       lwd = 2)

# #EJERCICIO 4

#setup
dir <- "~/MateriasExactas/2c2025/Estadistica/Practicas/Clase1"
autos <- read.csv(file.path(dir, "autos.txt"), sep=" ", header=TRUE)

#a - encontrar los valores de la tercera fila

autos[3, ]

#b - encontrar los valores de la segunda columna

autos[, 2]

#c - encontrar el valor de calidad del auto mas barato

precioMasBarato = min(autos["precio"])
calidadAutoMasBarato = autos[autos["precio"]==precioMasBarato][2] #es una unica fila, un arreglo
calidadAutoMasBarato

#d - encontrar la suma de los precios de las primeras 4 filas

sum(autos[1:4,]["precio"])

#e - usar apply() para calcular las suma de cada fila, y para cada columna?

sumaPorFila = apply(autos, 1, sum)
sumaPorFila

sumaPorCol = apply(autos, 2, sum)
sumaPorCol

#f - usar un scatterplot para graficar estas cositas
plot(autos$precio, autos$calidad, type="p", xlab="precio", ylab="calidad")

#g - ordenar los datos en funcion de su precio
autos_ordenados <- autos[order(autos$precio), ]
autos_ordenados

# EJERCICIO 

View(mtcars)

head(mtcars)

# a - los nombres de los autos con 4 en la variable gear
rownames(mtcars[mtcars$gear == 4, ])

# b - los nombres de 4 autos con engranajes delanteros y transmision manual
# NOTA: gear es cant de engranajes delanteros y transmision manual tiene cuando am == 1
rownames(mtcars[mtcars$gear==4 & mtcars$am == 1, ])

# c - la cantidad de autos que tienen 4 engranajes delanteros o transmision manual.
length(mtcars[mtcars$gear==4 | mtcars$am==1, ])

# d - convertir la variable am en un factor 
transmission = factor(c(FALSE, TRUE))

mtcars$am <- factor(mtcars$am, labels = c("automatic", "manual"))
mtcars

