#Respuestas: a, c, b, b, bc, c, b, e, c, d, d, a, cd, b, c, a, a

# Codigo pregunta 12 y 13 
set.seed(42)
n <- 30
altura <- rnorm(n, mean= 170, sd= 10)
altura2 <- rnorm(n, mean= 165, sd= 8) #antes del tratamiento


# Crear un data.frame para agrupar los datos
datos <- data.frame(Altura = altura, Altura2 = altura2)

View(datos)

set.seed(42)
n <- 30
altura <- rnorm(n, mean = 170, sd = 10) # Altura después del tratamiento
altura2 <- rnorm(n, mean = 165, sd = 8) # Altura antes del tratamiento

# Crear un data.frame para agrupar los datos
datos <- data.frame(Altura = altura, Altura2 = altura2)

# Realizar la prueba t pareada
resultado_t_test <- t.test(datos$Altura, datos$Altura2, paired = TRUE)

# Ver los resultados
print(resultado_t_test)

# Codigo pregunta 14 y 15

set.seed(42)
n <- 30
altura <- rnorm(n, mean= 170, sd= 10)
peso <- 0.5 * altura + rnorm(n, mean = 0, sd= 5)


cor.test(altura, peso)

# p-value = 1.044e-05 y cor = 0.7114793 


# Codigo pregunta 16


# 1
set.seed(123)

#2
suelo <- rep(c("Arcilloso", "Arenoso", "Franco"), each = 10)

#3
crecimiento <- c(rnorm(10, mean =15, sd= 2),
                 rnorm(10, mean =20, sd= 2),
                 rnorm(10, mean =25, sd= 2))
#4
datos2 <- data.frame(Suelo= suelo, Crecimiento= crecimiento)

#5 
print(datos2)

suelo.aov <- aov(datos2$Crecimiento ~  datos2$Suelo)  
summary(suelo.aov) 

set.seed(123)

# Calculo de media por tipo de suelo
medias <- tapply(crecimiento, suelo, mean)
print(medias)













