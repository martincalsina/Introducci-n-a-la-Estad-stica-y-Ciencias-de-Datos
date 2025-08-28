# 1  - indicar cuantas variable tiene el siguiente dataset

install.packages("Lock5Data")
library(Lock5Data)
data("FloridaLakes")
help("FloridaLakes")   # da informacion sobre los datos

print(paste("El dataset tiene", length(colnames(FloridaLakes)), "variables"))

# 2 - indicar cuantas observaciones hay

print(paste("El dataset tiene", length(rownames(FloridaLakes)), "observaciones"))

# 3 - Alkalinity es una variable cuantitativa o cualitativa?

head(FloridaLakes$Alkalinity) #es cuantitativa, son numeritos reales sueltos y ya

# 4 - hacer un histograma de densidad
aa <- hist(FloridaLakes$Alkalinity, freq=FALSE)
































































names(aa)
print(paste("el mayor valor que alcanza la densidad es", round(max(aa$density),4)))
      
      

# 5, 6, 7 choice...

# 8 - media muestral

print(paste("la media muestral es", round(mean(FloridaLakes$Alkalinity),4)))


# 9 - la mediana muestral


print(paste("la mediaana muestral es", round(median(FloridaLakes$Alkalinity),4)))
median(FloridaLakes$Alkalinity)

# 10 - la media 0.2 podada

print(paste("la media 0.2 podada es", round(mean(FloridaLakes$Alkalinity, trim=0.2),4)))

# 11 - choice

# 12 - estimarla proba de que la alcalinidad sea menor o igual a 40
proba_0_20 = 20*aa$density[1]
proba_2_40 = 20*aa$density[2]
print(paste("la probabilidad de que la alcalinidad sea menor o iguala 40 es", round(proba_0_20+proba_2_40,4)))

anchos <- diff(aa$breaks)

# Áreas = densidad * ancho
areas <- aa$density * anchos

sum(areas[1:2])

# Ver los cortes
aa$breaks

# 13 - boxplot y ver si se observa la asimetria del histograma

boxplot(FloridaLakes$Alkalinity, horizontal=TRUE)

# 14 - grafico de densidad, ver que toma valores negativos
plot(density(FloridaLakes$Alkalinity))
#esta mal? es un subproducto de la estimacionS