# Emanuel Molina Marchan
# 03/03/2025
# Matricula: 2134498
# Clase 6



# Pruebas de t ------------------------------------------------------------


# Las pruebas de t de dos colas  nos dice si hay diferencia
# Se utilizan las pruebas de una cola si utilizamos un contraste con un número de referencia 
# t.test(lo que voy a comparar, ) alternative = "less" o "greater"
# En este caso usaremos "less" para comprar si los costales contienen menos cantidad 
# 80= H nula

costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7,
            82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95,
            73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23,
            78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99,
            81.94, 80.41, 77.7)

mean(costal)

sd(costal)

# Para confirmar la longitud de los datos se usa length
length(costal)

# density se utiliza para obtener una aproximación si los datos siguen una distribución normal
plot(density(costal),
     main = "Gráfica densidad de costales",
     xlab= "Peso costales",
     ylaab= "Densidad",
     col= "blue",
     lwd= 2)

# Agregar valores de la media observada y peso declarado a los contales 
# abline es una mascara que añade una linea vertical 
abline(v = mean(costal), col= "green", lwd = 2, lty = 2)
abline(v = 80, col="red", lwd = 2, lty = 1)

t.test(costal, mu = 80, alternative = "less")
# Se acpeta la hipotesis alterna (H1) ya que presentó diferencias significativas
# Si se quita el alternative se queda la prueba de una cola, pero solo nos dice si hay diferencia sin conciderar que es mayor o menor 
# Si quisieramos saber si el valor es mayor se usa "greater" en lugar de "less"
# Si ponemos: ?nombre de la formula, nos indica como funciona la ecuación 



# Ejemplo # 2 -------------------------------------------------------------

sleep

# Es un experimento balanceado ya que tenemos 10 personas por grupo

boxplot(sleep$extra ~  sleep$group, col= "green")

shapiro.test(sleep$extra)

bartlett.test(sleep$extra  ~   sleep$group)

t.test(sleep$extra  ~   sleep$group, var.equal = TRUE)
 

# Ejemplo # 3 -------------------------------------------------------------

airquality

mean(airquality$Temp)

# Se pone 5 porque es la variable que queremos separar, mayo es el mes 5

mayo <- subset(airquality$Temp, airquality$Month == 5)
mean(mayo)

# Contrastamos la temp promedio de mayo con toda la temperatura en general

t.test(mayo, mu=mean(airquality$Temp), alternative = "l")

# La temperatura de mayo es menor que las otras observaciones 

boxplot(airquality$Temp   ~   airquality$Month, col= "green")

# Aqui hicimos una conversión de grados °F a °C
airquality$Cent <- (airquality$Temp-32)/1.8

boxplot(airquality$Cent   ~   airquality$Month, col= "green")
boxplot(airquality$Wind   ~   airquality$Month, col= "green")
boxplot(airquality$Ozone  ~   airquality$Month, col= "green")

# Me interesa que los datos sean normales de acuerdo a su grupo y no en conjunto

shapiro.test(airquality$Cent)
shapiro.test(mayo)

bartlett.test(airquality$Cent   ~   airquality$Month)

# Analisis de varianza 

# Se debe convertir el mes a factor para que se diferencie 

airquality$Month <- as.factor(airquality$Month)

# Si no hubieramos convertido a factor tendríamos 1 g.l. y deberian de ser 4, ya que n-1 (5-1)

Cent.aov <- aov(airquality$Cent ~  airquality$Month)
Cent.aov
summary(Cent.aov)

Wind.aov <- aov(airquality$Wind ~  airquality$Month)
Wind.aov
summary(Wind.aov)

# El ANOVA al obtener p value = 0.00879 **, indica que si hay diferencia significativa 
# Al obtener diferencia se debe correr la prueba de Tukey

TukeyHSD(Wind.aov)

# Las correlaciones se usan para ver si una variable tiene efecto en otra 

# La correlación mide una correlación lineal, puede ser positiva (cuando ambas variables aumentan)
# Las correlaciones negativas indican cuando una aumenta la siguiente sube 
# Es indistinto aquí cual varible acomode primero 
# Ho dice que no hay correlacion 
# La linea de regresión es alpha+(beta*x)

cor.var <- cor.test(airquality$Temp, airquality$Wind)
cor.var 

cor.temp_rad <- cor.test(airquality$Temp, airquality$Solar.R)
cor.temp_rad

cor.oz_tem <- cor.test(airquality$Ozone, airquality$Temp)
cor.oz_tem 

# Nube de puntos. pc= forma del punto
plot(airquality$Wind  ~   airquality$Temp, pch= 19)

plot(airquality$Ozone  ~   airquality$Temp, pch= 19)


# El resultado de p-value = 2.642e-09 indica que la correlación es significativa

# R es coeficiente de correlación y R2 es el coficiente de determinación, dice el porcentaje que una variable explica la otra

0.4579879^2 

# Por lo que la temperarua explica un 0.2097529*100 = 20.97% la velocidad del viento
# Calcular la ecuación de la linea recta alpha+(beta*x)

var.lm <- lm(airquality$Temp ~   airquality$Wind)
var.lm

# 90.13+(-1.23*x) 

