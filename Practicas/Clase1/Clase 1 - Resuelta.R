############ Ejemplo con un archivo
autos <- read.csv('autos.txt', sep=' ')
print(head(autos, 10))
names(autos)


View(mtcars)
names(mtcars)
mtcars[ ,c("mpg", "am")]


# Ejercicio 1 ##################################################################

#a
sum <- 0
for(i in 1:1000){
  sum <- sum + i
}
print(sum)

#b
sum <- 0
i <- 0
while(sum <= 10000){
  i <- i+1
  sum <- sum + i
}
print(i)

#c
suma_positivas <- function(v){
  sum(v[v>0])
}
v = c(1,2,3,4,5,-1,-2)
print (suma_positivas(v))


# Ejercicio 2 ##################################################################
xs <- seq(0, 1, by=0.01)
cuadratica <- function(x){
  x*(1-x)
}
ys <- cuadratica(xs)
plot(xs, ys) # Tengo un scatter plot :(
plot(xs, ys, type = "l")


# Para curiosos, grafico usando la libreria ggplot2
library(ggplot2)
f <- function(p) p*(1 - p)
ggplot() +
  stat_function(fun = f, geom = "line", xlim=c(0,1))

# Ejercicio 3 ##################################################################
xs <- seq(0, 2*pi, length.out=100)
ys_1 <- sin(xs)
ys_2 <- cos(xs)
ys_3 <- cos(xs^2)
plot(xs, ys_1, , type = "l", xlim=c(0, 2*pi))
lines(xs, ys_2)
lines(xs, ys_3)


# Para curiosos, grafico usando la libreria ggplot2
ggplot() +
  stat_function(fun = function(x) sin(x), geom = "line") +
  stat_function(fun = function(x) cos(x), geom = "line") +
  stat_function(fun = function(x) cos(x^2), geom = "line") +
  xlim(0, 2*pi)

# Ejercicio 4 ##################################################################
autos <- read.csv('autos.txt', sep=' ')
print(head(autos, 10))

#a
print(autos[3,])

#b
print(autos[,2]) # esta es una opción
autos$calidad # esta opción es mejor

#c
autos$calidad[autos$precio == min(autos$precio)]

#d
sum(autos$precio[1:4])

#e
apply(autos,2,sum) # columnas

apply(autos,1,sum) # filas

#f
plot(autos$precio, autos$calidad)

ggplot(data = autos) +
  geom_point(aes(x = precio, y = calidad))


#g
head(autos[order(autos$precio),], 10)



# Ejercicio 5 ##################################################################
head(mtcars, 10)

#a
rownames(mtcars[mtcars$gear == 4, ])

#b
rownames(mtcars[(mtcars$gear == 4) & (mtcars$am == 1), ])

#c
rownames(mtcars[(mtcars$gear == 4) | (mtcars$am == 1), ])

#d
print(class(mtcars$am))

mtcars$am <- as.factor(mtcars$am)

print(class(mtcars$am))


# Ejercicio 6 ##################################################################
x <- rnorm(1000) #  rnorm(1000, mean = 0, scale = 1)

hist(x) # Primera opción

# O con ggplot
ggplot() +
  geom_histogram(aes(x = x, y = after_stat(density)))




boxplot(x) # o con ggplot

ggplot() +
  geom_boxplot(aes(x = x))


qqnorm(x)
qqline(x)

# Otra opción
# ggplot(data.frame(x = x), aes(sample = x)) +
# stat_qq() +
# stat_qq_line()

