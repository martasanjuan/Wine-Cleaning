#Instalación necesaria:
install.packages("knitr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("nortest")


#Establecemos la carpeta en la que vamos a trabajar y tenemos el conjunto de datos
setwd("C:/Users/editrea/Desktop/Master 1819/TCVD/Practica 2")

#Procedemos a la lectura del fichero csv, obteniendo el data.frame denominado vinos.
vinos <- read.csv("winequality-red.csv", header = TRUE)

#Procedemos a ver las dimensiones del fichero
dim(vinos)


#Veamos el contenido de los primeros cinco registros de la cabecera
head(vinos[,1:12])


# Tipo de dato asignado a cada campo
library(knitr)
t <- sapply(vinos, function(x) class(x))
kable(data.frame(variables=names(t),tipo=as.vector(t)))




#Procedemos a renombrar las columnas
names(vinos)
names(vinos)[names(vinos) == 'fixed.acidity'] <- 'acidez_fija'
names(vinos)[names(vinos) == 'volatile.acidity'] <- 'acidez_volátil'
names(vinos)[names(vinos) == 'citric.acid'] <- 'ácido_cítrico'
names(vinos)[names(vinos) == 'residual.sugar'] <- 'azúcar_residual'
names(vinos)[names(vinos) == 'chlorides'] <- 'cloruros'
names(vinos)[names(vinos) == 'free.sulfur.dioxide'] <- 'libre_dióxido_sulfurico'
names(vinos)[names(vinos) == 'total.sulfur.dioxide'] <- 'total_dióxido_sulfurico'
names(vinos)[names(vinos) == 'density'] <- 'densidad'
names(vinos)[names(vinos) == 'pH'] <- 'pH'
names(vinos)[names(vinos) == 'sulphates'] <- 'sulfatos'
names(vinos)[names(vinos) == 'alcohol'] <- 'alcohol'
names(vinos)[names(vinos) == 'quality'] <- 'calidad'
names(vinos)




#Verificamos si existen valores vacíos
library(knitr)
t <- sapply(vinos, function(x) sum(is.na(x)))
kable(data.frame(variables=names(t),vacíos=as.vector(t)))


#Verificamos cuántos ceros existen 
library(knitr)
t <- sapply(vinos, function(x) sum(x==0))
kable(data.frame(variables=names(t),ceros=as.vector(t)))

#Vemos un resumen del conjunto de datos. Dónde se indica los valores máximo y mínimo de la variable citric.acid.
summary(vinos)


#Generamos los diagtramas de cajas para cada una de las variables 
par(mfrow=c(3,4))
for(i in 1:ncol(vinos)) {
if (is.numeric(vinos[,i])){
boxplot(vinos[,i], main = colnames(vinos)[i], width = 100)
}
}


#Generamos las primeras cuatro cajas
 par(mfrow=c(2,2))
 for(i in 1:4) {
{ boxplot(vinos[,i], main = colnames(vinos)[i], width = 100)}
}
 
 #Generamos las siguientes cuatro cajas
 par(mfrow=c(2,2))
 for(i in 5:8) {
{ boxplot(vinos[,i], main = colnames(vinos)[i], width = 100)}
}

#Generamos las últimas cuatro cajas
 par(mfrow=c(2,2))
 for(i in 9:12) {
{ boxplot(vinos[,i], main = colnames(vinos)[i], width = 100)}
}


 
#Generamos los valores atipicos para cada una de las variables)
boxplot.stats(vinos$acidez_fija)$out
boxplot.stats(vinos$acidez_volátil)$out
boxplot.stats(vinos$ácido_cítrico)$out
boxplot.stats(vinos$azúcar_residual)$out
boxplot.stats(vinos$cloruros)$out
boxplot.stats(vinos$libre_dióxido_sulfurico)$out
boxplot.stats(vinos$total_dióxido_sulfurico)$out
boxplot.stats(vinos$densidad)$out
boxplot.stats(vinos$pH)$out
boxplot.stats(vinos$sulfatos)$out
boxplot.stats(vinos$alcohol)$out
boxplot.stats(vinos$calidad)$out







#Generamos el diagrama de barras de la variable calidad
barplot(prop.table(table(vinos$calidad)), main='calidad')

#Vemos la distribución que existe de los valores dentro de la variable calidad
table(vinos$calidad)

 


#Copia del conjunto de datos con nombre de columnas modificadas
vinos_clean <- vinos


# Exportación de los datos limpios a .csv
write.csv(vinos_clean, "vinos_clean.csv")




#Probamos si las variables siguen una distribución normal
library(nortest)
alpha = 0.05
col.names = colnames(vinos_clean)
for (i in 1:ncol(vinos_clean)) {
	if (i == 1) cat("Variables que no siguen una distribución normal:\n")
		if (is.integer(vinos_clean[,i]) | is.numeric(vinos_clean[,i])) {
				p_val = ad.test(vinos_clean[,i])$p.value
					if (p_val < alpha) {
						cat(col.names[i])
						# Formato de salida
						if (i <= ncol(vinos_clean) - 1) cat(", ")
								if (i %% 3 == 0) cat("\n")
					}
		}
}


#Test de fligner 
plot(alcohol ~ calidad, data = vinos_clean)
fligner.test(alcohol ~ calidad, data = vinos_clean)






#Calculamos la matriz de correlación
matrizcorrelación <- round(cor(vinos),2)
head(matrizcorrelación)

#Convertimos a formato largo de datos.
library(reshape2)
matcor <- melt(matrizcorrelación)
head(matcor)

#Visualizamos la matriz de correlación
library(ggplot2)
ggplot(data = matcor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


#Visualizamos la matriz de correlación
library(ggplot2)
ggheatmap  <- ggplot(data = matcor, aes(Var2, Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Pearson\nCorrelation") +
  theme_minimal()+ 
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 12, hjust = 1))+
 coord_fixed()
 
#Añadimos los coeficientes de correlación por cuadrado
ggheatmap + 
geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)



#Copia del conjunto de datos con variables seleccionadas para el análisis posterior.
vinos_selected <- data.frame("alcohol"=vinos_clean$alcohol, "sulfatos"=vinos_clean$sulfatos, "ácido_cítrico"=vinos_clean$ácido_cítrico, "acidez_fija"=vinos_clean$acidez_fija, "calidad"=vinos_clean$calidad  )
head(vinos_selected)


# Exportación de los datos con variables seleccionadas para su análisis
write.csv(vinos_selected, "vinos_selected.csv")






#creamos un vector de tipo carácter
valoración <- as.vector(vinos_clean$calidad, mode="character") 
valoración <- vinos_clean $calidad


#Lo completamos con los valores malo, bueno, medio y excelente.
for(i in 1:nrow(vinos_clean))
{
	if (valoración[i] <= 5)
	{valoración[i] <- 'malo'}
	else if (valoración[i] >= 6 )
	{valoración[i] <- 'bueno'}
	
}


#Vemos si cuadra el número de cada valor con lo obtenido anteriormente.
table(vinos_clean$calidad)
table(valoración)




#Añadimos la nueva columna al conjunto de datos 
vinos_extended<- data.frame(vinos_clean, valoración)
head(vinos_extended)

# Exportación de los datos con variables seleccionadas para su análisis
write.csv(vinos_extended, "vinos_extended.csv")


#Obtenemos ambas muestras
vinos_extended_malo <-vinos_extended[vinos_extended$valoración == "malo",]$alcohol
vinos_extended_bueno <-vinos_extended[vinos_extended$valoración == "bueno",]$alcohol


#Obtenemos las estadísticas de cada muestras de alcohol
summary(vinos_extended_malo)
summary(vinos_extended_bueno)


#Ejecutamos el test sobre diferencia de medias
t.test(vinos_extended_malo, vinos_extended_bueno, alternative = "less")


 