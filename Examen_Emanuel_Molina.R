#Respuestas: a, c, b, b, bc, c, b, e, c, d, d, a, cd, b, c, a, a


#  Código pregunta 12 y 13  -----------------------------------------------

set.seed(42)
n <- 30
altura <- rnorm(n, mean= 170, sd= 10)
altura2 <- rnorm(n, mean= 165, sd= 8) #Antes del tratamiento


# Crear un data.frame para agrupar los datos
datos <- data.frame(Altura = altura, Altura2 = altura2)

View(datos)

# Data.frame para la agrupación de datos
datos <- data.frame(Altura = altura, Altura2 = altura2)

# Prueba de t
t_test <- t.test(datos$Altura, datos$Altura2, paired = TRUE)

# Resultados
print(t_test)

# Código pregunta 14 y 15 -------------------------------------------------

set.seed(42)
n <- 30
altura <- rnorm(n, mean= 170, sd= 10)
peso <- 0.5 * altura + rnorm(n, mean = 0, sd= 5)

# Correlación
cor.test(altura, peso)

# Valores de p-value = 1.044e-05 y cor = 0.7114793 obtenidos


# Código pregunta 16 y 17 -------------------------------------------------

set.seed(123)
suelo <- rep(c("Arcilloso", "Arenoso", "Franco"), each = 10)
crecimiento <- c(rnorm(10, mean =15, sd= 2),
                 rnorm(10, mean =20, sd= 2),
                 rnorm(10, mean =25, sd= 2))
datos2 <- data.frame(Suelo= suelo, Crecimiento= crecimiento)
print(datos2)

# ANOVA
suelos_tipo.aov <- aov(datos2$Crecimiento ~  datos2$Suelo)  
summary(suelos_tipo.aov) 
# Calculo de media por tipo de suelo
promedio_suelo <- tapply(crecimiento, suelo, mean)
print(promedio_suelo)













