install.packages('readxl')
library(readxl)

# Leer el conjunto de datos
setwd("/cloud/project")
df <- as.data.frame(read.csv('T09_12 - Hoja 1.csv'))

############################ (1)	Análisis exploratorio de los datos #######################################

# Revise el análisis descriptivo para las variables, 
# estadísticas descriptivas, matriz de correlación, vector(es) de media(s),
# matriz de varianza y covarianza, etc.

# Primeros 10 renglones
View(df[1:10,])

# Descriptivas de todos los renglones
View(summary(df))

# Matriz de correlación
R <- cor(df)
View(R)

# Vector de medias
media <- matrix(colMeans(df), ncol = 1)

# Matriz de Varianza y covarianza
sigma <- cov(df)


#Histogramas
par(mfrow = c(3, 3))

# Histograma de x1
hist(df$x1, main = "Histograma del índice crecimiento de ventas", 
     xlab = "Índice crecimiento de ventas", ylab = "Frecuencia", 
     col = "skyblue", xlim = c(80,120), ylim = c(0, 15), axes = TRUE)

# Histograma de x2
hist(df$x2, main = "Histograma del índice de rentabilidad de ventas", 
     xlab = "Índice de rentabilidad de ventas", ylab = "Frecuencia", 
     col = "skyblue", xlim = c(80,130), ylim = c(0, 15), axes = TRUE)

# Histograma de x3
hist(df$x3, main = "Histograma del índice de ventas a nuevas cuentas", 
     xlab = "Índice de ventas a nuevas cuentas", ylab = "Frecuencia", 
     col = "skyblue", xlim = c(90,120), ylim = c(0, 20), axes = TRUE)

# Histograma de x4
hist(df$x4, main = "Histograma de la prueba de creatividad", 
     xlab = "Prueba de creatividad", ylab = "Frecuencia", 
     col = "skyblue", xlim = c(0,20), ylim = c(0, 15), axes = TRUE)

# Histograma de x5
hist(df$x5, main = "Histograma de la prueba de razonamiento mecánico", 
     xlab = "Prueba de razonamiento mecánico", ylab = "Frecuencia", 
     col = "skyblue", xlim = c(0,20), ylim = c(0, 15), axes = TRUE)

# Histograma de x6
hist(df$x6, main = "Histograma de la prueba de razonamiento abstracto", 
     xlab = "Prueba de razonamiento abstracto", ylab = "Frecuencia", 
     col = "skyblue", xlim = c(0,20), ylim = c(0, 25), axes = TRUE)

# Histograma de x7
hist(df$x7, main = "Histograma de la prueba de matemáticas", 
     xlab = "Prueba de matemáticas", ylab = "Frecuencia", 
     col = "skyblue", xlim = c(0,60), ylim = c(0, 15), axes = TRUE)

# Restablecer la disposición de gráficos
par(mfrow = c(1, 1))


############################ (2)	Pruebas de bondad de ajuste ###############################################
# Investigará la prueba disponible en R, donde se revise si los datos provienen
# o no de distribución normal multivariada, para ello el alumno adjuntará una 
# tabla donde indique las hipótesis correspondientes y el criterio de rechazo, 
# mencionando la librería y la función empleada en R para posteriormente aplicarlo 
# a sus datos (Aquí se deberá construir una tabla de PH como las acostumbradas en clase)
install.packages('MVN')
library(MVN)

# Escalar los datos
df_scaled <- scale(df)
means <- attr(df_scaled, "scaled:center")
sds <- attr(df_scaled, "scaled:scale")

# Prueba de normalidad
mvn(df_scaled, mvnTest = 'mardia') 
mvn(df_scaled)

# Eliminando las variables x1 y x6
df_scaled_filtered <- scale(df[c(-1,-6)])
df_filtered <- df[c(-1,-6)]
mvn(df_scaled_filtered, mvnTest = 'mardia')

# Ho: Los datos provienen de una distribución normal multivariada.
# Ha: Los datos NO provienen de una distribución normal multivariada.

# Rechazo Ho si p < alpha 
# Como 0.645257206200229 > 0.05, no rechazo Ho, SI provienen de dis nomral mult

############################ (3)	Planteamiento de comparativas #############################################

#   •	Realizar la prueba correspondiente a la revisión de 
# independencia de las variables en el conjunto


# Para buscar si las variables son independientes entre sí se realiza la prueba de Bartlett

# Ho: las 5 variables son intependientes entre si
# Ha: Las 5 variables NO son independientes entre si
# R: Matriz de correlaciones muestrales
R <- cor(df_filtered)
R

# Estadístico de Prueba

#EP
p <- ncol(df_scaled_filtered)
n <- nrow(df_scaled_filtered)
-2 * (1-( (2*p+11)/(6*n)) ) * log(det(R)**(n/2))

# Región de rechazo
qchisq(1-0.05,p*(1+p)/2)

# Rechazo Ho si EP>RR, 287.1494 > 24.99579, por lo tanto rechazo Ho
# Las variables NO son independientes
# Comprobación
library(psych)
cortest.bartlett(R, n = nrow(df_scaled_filtered), diag = T)
barlett <- cortest.bartlett(R, n = nrow(df_scaled_filtered), diag = T)

#------------------- Comparativas -----------------------------------
# •	Dado que el material revisado consiste en el análisis de conjuntos con 
# diversas características, complemente la información del análisis exploratorio
# identificando dos comparativas o pruebas de hipótesis a realizarse en el conjunto
# de datos, donde se incluyan PH e IC según corresponda. Para ello deberá primero 
# plantear una pregunta detonante y posterior a realizar el análisis responder 
# dicho cuestionamiento, según los resultados obtenidos. (Aquí se deberá elegir 
# del contenido disponible  en material de  clase referente a pruebas de hipótesis
# de conjuntos multivariados)

# # 3.1. Determinar si es posible considerar, de manera simultanea, que el promedio 
# de calificación por sección no difieren
# Ho las medias de puntaje por sección no difieren
# Ha al menos una sección difiere

# Vector de medias

xi_b <- matrix(colMeans(df_filtered[,3:5]),ncol = 1) #media por muestra
xi_b
x_b <-mean(c(df_filtered[,3],df_filtered[,4],df_filtered[,5])) # Media global
x_b

B <- sum(n*(xi_b-x_b)**2) # suma de cuadrados tratamiento 
B

# suma de cuadrados total
BW <-sum((df_filtered[,3]-x_b)**2) +sum((df_filtered[,4]-x_b)**2) + 
  sum((df_filtered[,5]-x_b)**2)

#suma de cuadrados error
W <- BW-B
W

# Estadístico de prueba

# N: 50+50+50
# k: número de muestras 2 especies de iris, etc
N <-50*3
k <- 3
# p = 1

# Estadístico de prueba
(B/(k-1)) / (W/(N-k))

# Región de rechazo
qf(1-0.05,k-1,N-k)

# rechazo Ho si 86.00494 > 2.655939, por lo tanto al menos una sección difiere.

# 3.2 Determinar si es posible considerar los valores de las medias 
# poblacionales en 100, 100, 15, 15, 30 respectivamente

# Ho: El vector de medias poblacionales está dado por 100, 100, 15, 15, 30
# Ha: El vector de medias difiere a 100, 100, 15, 15, 30
mu_0 <- matrix(c(100, 100, 15, 15, 30), ncol = 1)

#descriptivas
media_filtered <- as.matrix(colMeans(df_filtered), ncol=1)
sigma_filtered <- cov(df_filtered)

n <- nrow(df)
# Encontrar el estadístico de prueba
install.packages("ICSNP")
library(ICSNP)
HT2_2 <- HotellingsT2(df_filtered, mu = mu_0, test = 'chi')
T2_2 <- n * t(media_filtered - mu_0) %*% solve(sigma_filtered) %*% (media_filtered- mu_0)
T2_2 # Estadístico de Prueba
HT2_2$statistic
HT2_2
HT2_2$p.value

# Región de rechazo
qf(1-0.05,p,n-p)

(((n-1)*p) / (n-p)) *  qf(1-0.05,p,n-p)

p_valor <- 1 - pf(((n-p)*T2_2)/((n-1)*p), p, n-p)

# Se rechaza Ho si EP > RR
# 1804.227 >2.422085, por lo tanto se rechaza Ho, el vector de medias difiere


############### (4)	Técnica multivariada:#################################################### 
#   •	Aplicar en análisis de componentes principales y el grafica de codo,
# indicar si es posible disminuir las dimensiones del conjunto según los 
# resultados obtenidos. Aquí deberá indicar los coeficientes asociados a la 
# transformación y el nuevo conjunto. 
install.packages("factoextra")
installed.packages("ggplot2")
library(factoextra)
library(ggplot2)

# Análisis de componentes principales
pca <- prcomp(df_scaled_filtered, scale. = T)

# Análisis del codo
fviz_eig(pca, addlabels = T, main = 'Varianza Explicada por Cada Componente Principal')
summary(pca)

# Se consideran 2 componentes con varianza acumulada de 0.8903

# coeficientes asociados a la transformación
pca2 <- prcomp(df_scaled_filtered, scale. = T, rank. = 2)
pca2$rotation

# Nuevo conjunto
pca2$x

par(mar = c(5.1, 4.1, 4.1, 2.1))
plot(pca2$x,main="Gráfica de PCA")
 


# Análisis de conglomerados
# Calcular la distancia euclidiana
d <- dist(df_scaled_filtered, method = "euclidean")

# Realizar clustering jerárquico usando el método de Ward
hclust_result <- hclust(d, method = "ward.D")

# Plotear el dendrograma
plot(hclust_result, labels = FALSE, main = "Dendrograma de Clustering Jerárquico")
rect.hclust(hclust_result, k = 4, border = "red")

# Cortar el dendrograma en 4 grupos
grupos <- cutree(hclust_result, k = 4)

# Añadir los grupos al dataframe original
df_con_grupos <- cbind(df[c(-1,-6)], Grupo = grupos)

# Mostrar los primeros 10 renglones con los grupos asignados
View(df_con_grupos[1:10,])

# Análisis factorial
num_factors <- 2
fa_result <- principal(R, nfactors = num_factors, rotate = "varimax")
print(fa_result)
# Comunidades (h2) y especificidades (u2)
comunalidades <- fa_result$communality
especificidad <- 1 - comunalidades

# Suma de las comunalidades (SS loadings)
ss_loadings<-sum(comunalidades)
# Proporción de la varianza explicada
prop_var <- fa_result$Vaccounted

# Mostrar resultados
print(comunalidades)
print(especificidad)
print("Suma de las comunalidades (SS loadings):")
print(ss_loadings)
print("Proporción de la varianza explicada:")
print(prop_var)
#### Actualizado Domingo 23:56
