# Emanuel Molina Marchan
# 10/03/2025
# Matricula: 2134498
# Clase 7



# Correlaciones ------------------------------------------------------------

# Los valores oscilan entre -1 a 1
# Si la correlación es lineal se prosigue con la regresion lineal
# La regresion linean genera una linea de tendencia central 
# La varible indepeinte es "X" y la variable dependiente es "Y"
# El diametro es la varible independiente y la altura es la variable dependiente 
# y'= alpha+beta(x)
# Alpha es lo que vale "Y" cuando equis vale 0
# Beta indica la pendiente cuando x aumenta una unidad 
# La funcion de regresion lineal nos permite obtener el comportamiento de un valor que se encuentre dentro de la nube de datos 
# Las lineas de regresion miden la asocion entre dos variables 
# La regresión lineal nos ayuda predecir el comportamiento 
# La correlacion tiene el coef. de correlación (R) y la regresión lineal es R2 ajustado
# R2 nos dice el porcentaje que la variable "X" predice el comportamiento de la variable "Y"
# En la correlación lineal se usa lm(variable Y ~ variable X)

# Regresion lineal_____________________________________________________________

geyser <- read.csv("erupciones.csv", header= T) 

geyser

plot(geyser$waiting, geyser$eruptions, pch= 19,
     xlab= "Tiempo de espera",
     ylab= ("Duracion (min)"))

# Cor nos indica el valor de correlacion 

cor(geyser$waiting, geyser$eruptions)

# cor.test nos indica el valor de correlacion y si existe diferencia significativa (se prefiere este analisis mejor que cor, ya que es mas completo)

cor.test(geyser$waiting, geyser$eruptions)

# Se obtuvo una alta correlacion positiva de 0.9008112 y un p-value= 2.2e-16

# Modelo de regresion linea, que nos permite obtener la linea de tendencia central 

g.lm <- lm(geyser$waiting ~ geyser$eruptions)
g.lm
#  Intercept es alpha y la otra variable que muestra es beta

summary(g.lm)
# Alpha y beta deben de ser significativos para que el modelo de regresión sea valido 

# Graficar la linea de tendencia central 

plot(geyser$waiting, geyser$eruptions, pch= 19,
     xlab= "Tiempo de espera",
     ylab= ("Duracion (min)"))
abline(g.lm, col= "red")

# Si no aparece la linea de abline es que están alreves las varibles de X y Y
# Se vuelve a correr la informacion con los datos corregidos


g.lm <- lm(geyser$eruptions ~ geyser$waiting)
g.lm
summary(g.lm)

# Ambas variables son siignificativas 

# R-squared ajustada (R2)=  0.8108, lo que indica que el tiempo de espera nos explica el 81.88% de las erupciones

# Se vuelve a graficar y se obtiene la linea de tendencia central 

plot(geyser$waiting, geyser$eruptions, pch= 19,
     xlab= "Tiempo de espera",
     ylab= ("Duracion (min)"))
abline(g.lm, col= "red")

# Ejemplo con un tiempo de espera de 60 minutos
# Los valores que tiene la linea de tendencia central y'= alpha+beta(x)
#  -1.87402+0.07563(60)
# La operación se realiza de la siguiente manera: 
-1.87 + 0.07*60
# El valor estimado para una espera te 60 minutos sería una duracion de 2.33 minutos de erupcion

# Para obtener el valor estimado de cada valor real se realiza: 

g.lm$coefficients[1]+g.lm$coefficients[2]*60

geyser$yprima <- g.lm$fitted.values

# Para observar los valores
geyser

# Los residuales es la diferencia entre los valores observados con la media

geyser$residuales <- g.lm$residuals

# Para observar los valores
geyser

# Para obtener la suma de los residuales se realiza lo siguiente, entre mas cercano a 0 indica un mejor ajuste 
sum(geyser$residuales)

# Para obtener la varianza se eleva al cuadrado el valor de los residuales 

geyser$residuales2 <- geyser$residuales^2

# Para observar los valores
geyser

sum(geyser$residuales2)/270

# Se obtuvo una varianza de 0.2465251

# Se usó 270 porque son los grados de libertad 


# Resumen: valoraciones más importantes

# Para decidir si la regresion es correcta se debe estimar alpha y beta, estimar si son significativos
# Obtener que la linea de tendia central es significativa 


# El anova se hace para descomponer la varianza y nos dice si hay diferencia significativa
# Se realiza para rectificar que el modelo presenta diferencia significativa y es valido
# Se compara la "Y" obtenida con la "Y" estimada
mod.lm <- anova(g.lm)
mod.lm








