# Emanuel Molina Marchan
# 17/02/2025
# Matricula: 2134498
# Clase 4


# Determinar la normalidad, homocedasticidad y pruebas de t

# Datos vivero ------------------------------------------------------------

vivero <- read.csv("vivero.csv", header = T)

# Se convierte Tratamiento a factor para que no lo lea como variable númerica sino como factor

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

# La hipotesis nula dice que no hay diferencias significativa 
# alpha se toma regularmente de 0.05 lo que nos da una confianza del 95%
# Los datos para ser normales deben presentar varianzas homogeneas 
# Las medidas de tendencia central son la media, mediana y moda
# Las medidas de dispersión mide que tan amplio son los rangos de dispersión de datos, entre las medias de dispersión son el coeficiente de error, error estandar y desvición estandar 
# Para la homogeneidad se puede usar shapiro o kolmogorov
# Barret se utiliza para la homocedasticidad de varianzas 
# Cada prueba tiene un p-value, oscila entre 1 a 0.0000001
# El Ho ocupa la mayor parte de la campana de Gauss
# Segun el programa o la prueba se divide la campana de Gauss en una o dos colas, en caso de ser dos colas es alpha = 0.025
# Si solo quiero conocer si hay diferencia aplico d
# Si solo quiero ver si hay diferencia en dos muestras se usan dos colas
# Las pruebas de t son de tres tipos, una muestra, dos muestras, dependientes e independientes 
# En una muestra se compara un valor con otro ya establecido
# La de dos muestras se comparan dos valores 
# Las dependientes es cuando se compara en una misma localidad y las independientes son distintas procedencias 
# la prueba de t es: t.test() 
















































