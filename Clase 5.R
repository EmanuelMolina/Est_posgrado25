# Emanuel Molina Marchan
# 24/02/2025
# Matricula: 2134498
# Clase 5



# Pruebas de t ------------------------------------------------------------

# ANOVA sirve para comparar la media de 3 tratamientos o más
# Los datos deben tener una distribució normal y presentar homogeneidad de varianzas 
# Los análisis de varianza nos dice si existe diferencia en al menos alguno de los tratamientos
# El ANOVA descompone la varianza que genera el tratamiento o la varianza intrinseca del grupo
# Si hay varianza entre los grupos está reaccionando a mi tratamiento (riego, fertilizante, etc)
# El nivel de factor es por ejemplo el periodo de riego 
# Si hay diferencia se puede realizar una prueba de Tukey pero debe ser el experimento balanceado (misma cantidad de observaciones)
# Las repeticiones es cuando se elige otro grupo de individuos para repetir el experimento 
# Hay ANOVAS de una vía y dos vía, en el de una vía se toma tratamiento
# En el ANOVA de dos vías se analiza como interaccionan los dos tratamientos 
# El ANOVA de 2 o 3 vías es complicado de explicar por las interacciones
# Experimentos balanceados = mismo numero de observaciones 
# Si el experimento no es balanceado se utiliza una transformación como Bonferroni 
# La transformación que se usa depende del área que se está investigando 
# Se dice robusta a una prueba que percibe más fácil los cambios pequeños


# Para llamar datos de una librería en internet se instala el paque de repmis y se corre la libreria 

library(repmis) 

localidad <- source_data("https://www.dropbox.com/s/fbrwxypacjgeayj/Datos_Rascon_Anova.csv?dl=1")

# El tratamiento es el sitio y el número de factor son 4 por los 4 parajes

# Grafico de cajas 

boxplot(localidad$DAP ~  localidad$Paraje, col= "green")

boxplot(localidad$DAP ~  localidad$Paraje, col= "green",
        xlab = "Paraje",
        ylab = "DAP (cm)")

# Determinar la normalidad 

shapiro.test(localidad$DAP)

# Ya que los datos presentaron un valor menor a 0.5 no siguen una distribucion normal

# Prueba de homogeneidad de var

bartlett.test(localidad$DAP ~  localidad$Paraje)

homo_var <- bartlett.test(localidad$DAP ~  localidad$Paraje)

# Interpretación de los resultados
if (homo_var$p.value < 0.05) {
  cat("Los datos no presentan homogeneidad de varainzas (p < 0.05)\n")
} else {
  cat("Los datos presentan homogeneidad de varainzas y se acepta H0 (p >= 0.05)\n")
}

# Los datos no siguen una distribución normal, pero si presentan homogeneidad de varainzas. Por lo que no son datos normales y se debe realziar una transformación

# Transformar datos 

localidad$DAP_log <- round(log10(localidad$DAP + 1), 2)

hist(localidad$DAP_log)

shapiro.test(localidad$DAP_log)

# Los datos no presentaron normalidad y se busca otra transformacion

localidad$sqrt <- round(sqrt(localidad$DAP),2)

shapiro.test(localidad$sqrt)

hist(localidad$sqrt)

# Grafico de cajas 

boxplot(localidad$sqrt ~  localidad$Paraje, col= "green")

boxplot(localidad$sqrt ~  localidad$Paraje, col= "green",
        xlab = "Paraje",
        ylab = "DAP (cm)")


# La siguiente librería se utiliza para ver el sesgo de los datos 

library(e1071)

skewness(localidad$DAP)

skewness(localidad$DAP_log)

skewness(localidad$sqrt)

# Si el resultado es 0 es que no teine sesgo, y si teiene un valor mayor a 1 o menor a -1 no se puede hacer mucho


# Histograma

hist(localidad$sqrt)

# ANOVA

# El ANOVA identifica la fuente de variación de datos 
# Los datos provienen de la fuente de información, en este caso el tratamiento es el Paraje
# El ANOVA muestra el error y total en filas y grados de libertidad, suma de cuadrados, cuadrado medio, valor de F y p-value en las columnas

dap.aov <- aov(localidad$sqrt ~  localidad$Paraje)
dap.aov

# Residuales también se le llama error, resultan 3 g.l. porque son 4 parajes

summary(dap.aov)

# * = diferencia significativa, ** = altamente significativo y *** = sumamente significativo

# P-value= 1.5e-15 ***, Por ende, al menos un Paraje presenta diferencia significativa 
# Al menos un variable esta generando variación en el DAP


# Ahora se debe proceder a la comparación y contrastes de medias con Tukey

TukeyHSD(dap.aov)

# Los valores de p adj que sean inferiores a 0.05 presentan diferencia significativa 
# Tule-Chinatu y Tule-Laguna no presentan diferencia significativa en su respectiva cimparación

# Para ver la diferencia en grafica se hace de la siguiente manera

plot(TukeyHSD(dap.aov), las = 1)

# Si la linea horizontal toca con el cero no hay diferencia


















# Ejemplo de como generar un histograma y que aumentar observaciones se logra la normalidad

d50 <- rnorm(50, mean=0, sd=1)
hist(d50)

d1000 <- rnorm(1000, mean= 0, sd=1)
hist(d1000)












