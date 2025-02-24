# Parte I R y RStudio-----------------------------------------------------------------

# Gastos totales
300 + 240 + 1527 + 400 + 1500 + 1833

celular <- 300
transporte <- 240
comestibles <- 1527
gimnasio <- 400
alquiler <- 1500
otros <- 1833

gastos_totales <- c(celular, transporte, comestibles, gimnasio, alquiler, otros)

sum(gastos_totales * 5) 

sum(gastos_totales * 10) 


# Autoevaluación ----------------------------------------------------------

# Instrucción: + Toma los objetos creados de la estudiantes (i.e variables) transporte, comestibles, gimnasio,alquiler y otros y escribamos dentro de la función de combinación c() para crear un vector llamado gastos:

gastos <- c(celular, transporte, comestibles, gimnasio, alquiler, otros)

# + Ahora, use la función gráfica barplot() para producir un diagrama de barras de gastos:

barplot(gastos, col="dodgerblue1", ylab= "Cantidad ($)", 
        xlab= "Tipo de gasto", cex.names = (1), ylim= c(0, 2000), 
        names.arg = c("celular", "transporte", "alimentos", "gimnasio", 
                      "alquiler", "otros")) 
# + Descubra cómo utilizar sort() para ordenar los elementos en la variable gastos, con el fin de organizar elementos en gastos en orden decreciente. 
# + Descubra cómo utilizar sort() y barplot() para producir un gráfico de barras con barras en orden decreciente.
# + Opcional: ve si puedes descubrir cómo mostrar los nombres de las variables debajo de cada una de las barras

barplot(sort(gastos, decreasing = TRUE), 
        main = "Gastos totales",
        xlab= "Tipo de gasto", 
        ylab= "Gasto ($)", 
        col="dodgerblue1", 
        ylim= c(0, 2000), 
        cex.names = (1), 
        names.arg = c("otros", "comestibles", "alquiler", "gimnasio", 
                      "cellular", "transporte")) 

# Parte II -----------------------------------------------------------------

# Problema 1:

# + Identifique el tipo de variable (cualitativa o cuantitativa) para la lista de preguntas de una encuesta aplicada a estudiantes universitarios en una clase de estadística:

# Nombre de estudiante. Respuesta: Cualitativa
# Fecha de nacimiento (p. Ej., 21/10/1995). Respuesta: Cualitativa
# Edad (en años). Respuesta: Cuantitativa
# Dirección de casa (por ejemplo, 1234 Ave. Alamo). Respuesta: Cualitativa
# Número de teléfono (por ejemplo, 510-123-4567). Respuesta: Cualitativa
# Área principal de estudio. Respuesta: Cualitativa
# Grado de año universitario: primer año, segundo año, tercer año, 
# último año. Respuesta: Cualitativa
# Puntaje en la prueba de mitad de período (basado en 100 
# puntos posibles). Respuesta: Cualitativa
# Calificación general: A, B, C, D, F. Respuesta: Cualitativa
# Tiempo (en minutos) para completar la prueba final de 
# MCF 202. Respuesta: Cuantitativa
# Numero de hermanos. Respuesta: Cuantitativa

# Problema 2

# + Elija un objeto (cualquier objeto, por ejemplo, animales, plantas, países, instituciones, etc.) y obtenga una lista de 8 variables: 4 cuantitativas y 4 categóricas.

variables <- list(
  cuantitativas = c("Diametro", "Altura", "Volumen", "Cobertura de copa"),
  cualitativas = c("Vigor", "Especie", "Tipo de daño", "Estrato"))

print(variables)
 
# Probelma 3 --------------------------------------------------------------

# + Considere una variable con valores numéricos que describen formas electrónicas de expresar opiniones personales: 1 = Twitter; 2 = correo electrónico; 3 = mensaje de texto; 4 = Facebook; 5 = blog ¿Es esta una variable cuantitativa o cualitativa?  
  
# Respuesta: Son variables cualitativas, ya que se le asigna un número a cada red social para hacer una clasificación, pero no representan un sentido númerico para análisis matemáticos 

# Problema 4 --------------------------------------------------------------

# + Para cada pregunta de investigación, (1) identifique a los individuos de interés (el grupo o grupos que se están estudiando), (2) identifique la (s) variable (s) (la característica sobre la que recopilaríamos datos) y (3) determine si cada variable es categórico o cuantitativo.

# ¿Cual es la cantidad promedio de horas que los estudiantes de universidades públicas trabaja cada semana?
# Respuestas: (1) estudiantes de universidades publicas (2) horas promedio que trabajan por semana (3) variable cuantitativa 

# ¿Que proporcion de todos los estudiantes universitarios de México están inscritos en una universidad pública?
# Respuestas: (1) estudiantes mexicanos universitarios  (2) inscritos en universidades publicas (3) variable cuantitativa

# En las universidades públicas, las estudiantes femeninas tienen un promedio de CENEVAL más alto que los estudiantes varones?
# Respuestas: (1) estudiantes en universidades publicas según su género (masculino y femenino) (2) promedio en la prueba de CENEVAL (3) variable cuantitativa

# Es más probable que los atletas universitarios reciban asesoramiento académico que los atletas no universitarios?
# Respuestas: (1) atletas inscritos o no inscritos en universidades (2) recepción de asesoramiento academico (3) variable categorica 

# Si reunieramos datos para responder a las preguntas de la investigación anterior, qué datos podrían analizarse mediante un histograma? ¿Cómo lo sabes?.
# Respuestas: Los histogramas se pueden aplicar a todas las variables que son cuantitativas, ya sean continuas o discretas (Fávero y Belfiore, 2019).    
  

# Referencia
# Fávero, L. P., & Belfiore, P. (2019). Univariate Descriptive Statistics. In Data Science for Business and Decision Making (pp. 21–91). doi:10.1016/b978-0-12-811216-8.00003-3

















