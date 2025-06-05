# Emanuel Molina Marchan
# 10/02/2025
# Matricula: 2134498
# Clase 3


# Importar datosa R provenientes de diferetnes fuentes 


# Importar datos ----------------------------------------------------------

ocampo <- read.csv("datos_ocampo.csv", header = T)

# getdw() se utiliza para conocer en que directorio estamos trabajando 
# setwd() se emplea para determinar una carpeta 

ocampo

names(ocampo)

mean(ocampo$TEMP)

mean(ocampo$DIRR)

# Las gráficas de barras son datos acumulados para una categoría y los histogramas son frecuencias 

boxplot(ocampo$TEMP)

# Si hay valores que se salen de los quantiles son outlayer, son datos que se salen del rango 
# Los boxplot dice el valor maximo y mino, los rangos de cuatiles, la mediana
# En este caso se tiene un sesgo hacia la izquiera 
# La caja gris es el 50% de los datos 

boxplot(ocampo$TEMP, col = "lightgreen",
        main = "Temperatura de Ocampo")

hist(ocampo$TEMP,  col = "blue", main = "Sitio Ocampo",
     xlab = "Temperatura (°C)",
     ylab = "Frecuencia")  

# El histograma tiene una distribucion bionomial (dos grupos de datos)

# Anova (parametrico) y su igualdad es Kruskall Walis (no parametrica)

# El experimento valanciado es cuando se tiene la misma cantidad de observaciones en los tratamientos 
# Los experimentos valanciados es cuando no presentan la misma cantidad de observaciones 
# Bonferroni es para comopar medias de experimentos no valanciados 
# Si tengo dos muestras son pruebas de T y si tengo 3 o mas grupos el ANOVA

stem(ocampo$TEMP)

# Si se acuestan los datos presenta un resultado similar a un histograma 

# Datos vivero ------------------------------------------------------------

vivero <- read.csv("vivero.csv", header = T)

vivero$Tratamiento <- as.factor(vivero$Tratamiento)

# Se separan dos variables por tratamiento en cada analisis 

boxplot(vivero$IE  ~  vivero$Tratamiento, col = "Blue",
        xlab = "Tratamientos",
        ylab = "Índice de esveltez")


# Revsar la normalidad de los datos

shapiro.test(vivero$IE)

# Revisar homogeneidad de varianzas

ctrl <- subset(vivero$IE, vivero$Tratamiento == "Ctrl") 
Fert <- subset(vivero$IE, vivero$Tratamiento != "Ctrl") 

var(ctrl)
var(Fert)

bartlett.test(vivero$IE ~  vivero$Tratamiento)

homo_var <- bartlett.test(vivero$IE ~  vivero$Tratamiento) 

# Interpretación de los resultados
if (homo_var$p.value < 0.05) {
  cat("Los datos no presentan homogeneidad de varainzas (p < 0.05)\n")
} else {
  cat("Los datos presentan homogeneidad de varainzas y se acepta H0 (p >= 0.05)\n")
}

# Ya que se cumplieron las dos pruebas de normalidad se peuden comparar las dos muestars

# Prueba de t de dos muestras 
# Se utiliza prueba de datos independientes 

t.test(vivero$IE ~  vivero$Tratamiento, var.equal = TRUE)
comp_medias <- t.test(vivero$IE ~  vivero$Tratamiento, var.equal = TRUE)
# Si fueran muestras dependientes se utilizaría paired = TRUE en lugar de var.equal = TRUE

# En esta prueba se obtuvo un valor de p < a 0.025 son diferntes por lo que se acepta H1 (Hip alternativa)
# Se obtuvo un valor de p tan bajo que son diferentes (p-value = probabilidad que sean iguales)
# Si es recomedable comprar el fertilizante 

# Interpretación de los resultados
if (comp_medias$p.value < 0.05) {
  cat("Las medias presentan diferencias significativa y se acepta H1 hipotesis 
      alternativa (p < 0.025)\n")
} else {
  cat("Las medias no presentan diferencias significativa y se acepta Ho hipotesis nula
      que son iguales (p >= 0.025)\n")
}


# Pruebas de t de  una muestra 

mean(vivero$IE)
# Si CONAFOR tiene una media de 0.85 (mu = 0.85) de Índice de Esveltez (IE)

t.test(vivero$IE, mu = 0.85)

# p-value = 0.6163 al ser mayor a 0.05 y 0.25 se obtiene que las medias son iguales y no hay diferencia 
# SI solo es un grupo no se aplica la homogeneidad de varianzas, solo se aplica la normalidad 




# Grados de libertad son las observaciones menos los tratamientos 

# Las pruebas de t en R son dos colas y ahí el valor de p a comparar es de p= 0.025

# Existe la teoría del límite central  dice que todos los datos en la naturaleza parten de una distribución normal 

# Se debe revisar si el programa está en una cola o dos colas ya que eso incide en aceptar H0 o H1

# La hipotesis nula dice (H0) dice que los datos siguen una distribución normal y la hipotesis alterna (H1) dice que no son normales

# El índice de esveltez entre mas cercano esé a 1 es más posible que sobreviva a campo 

# El índice de esveltez es la proporción de tallo - raíz 

# Las pruebas de t solo comparan dos tratamientos, pueden ser de tres tipos, las de 1 una muestra, independientes y dependientes 

# En las de una muestra ya existe una media definida. Se hace un experimento y se compara con la muestra 

# Las independientes son dos grupos diferentes pero se analiza una variable en común. Como rodal B y rodal B 

# Las dependientes se realiza la prueba en un mismo grupo en un antes y un después. Como en las plantas que se realiza para evaluar un tratamiento

# En el ejemplo son datos independientes, ya que son plantas de distintas procendencias 

# Los datos deben de tener homocedasticidad de varianzas ya que si no cumplen con esta condición, ya presentan diferencias 

# Debe cumplir la normalidad y homocedasticidad, si no cumple una o las dos no son normales 

# La hpotesis nula indica que NO hay diferencias en lo que voy a comparar y la hipotesis alternativa indica que SI hay diferencias

# alpha es el limite en el que yo obtendré diferencias, normalmente alpha vale 0.05, lo que nos da una confianza del 95%

# p-value nos indica si existe diferencia significativa, si es mayor a 0.05 no son diferentes, ya que si 0.75 es TAN alto, que son igual

# p value es la probabilidad que sean iguales











