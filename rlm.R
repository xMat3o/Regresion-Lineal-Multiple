##-------------------------------------------------##
##-----------      Modelos Lineales     -----------##
##-----------        Trabajo Final         -----------##
##-- Nombre: Mateo Larco Álvarez

#2.-  Genere un archivo rlm.R, de tal forma que le permita responder las siguientes preguntas:
#2.1.- Leer los archivos poblacion1.xlsx y poblacion2.xlsx , y analizar sus dimensiones.
library(readxl)
poblacion1<-read_excel("poblacion1.xlsx", sheet=1, na=" ")
poblacion2<-read_excel("poblacion2.xlsx", sheet=1, na=" ")

View(poblacion1)
str(poblacion1)

View(poblacion2)
str(poblacion2)

nrow(poblacion1) 
ncol(poblacion1) 

nrow(poblacion2) 
ncol(poblacion2) 

#2.2.- Una los archivos leídos en un mismo objeto llamado población.
#Uso la funcion merge para unir dos data frames
poblacion <- merge(poblacion1,poblacion2,by="identificador",suffixes=c("",""))
View(poblacion)

#2.3.- Cree un código que identiﬁque la clase de cada variable y genere diagramas
#de cajas para variables continuas y diagramas de barras para variables discretas.

#Para variables continuas
for(j in 2:dim(poblacion)[2]){
  if(is.numeric(poblacion[,j])==TRUE){
    boxplot(poblacion[,j])
    title(names(poblacion)[j])
  }
}

#Para variables discretas
for(j in 2:dim(poblacion)[2]){
  if(is.numeric(poblacion[,j])!=TRUE){
    barplot(table(poblacion[,j]))
    title(names(poblacion)[j])
  }
}

#2.4.- Cree un código que calcule automáticamente el mínimo, media,
#máximo, desviación estándar, primer cuartil de cada variable 
#numérica y la frecuencia en el caso de variables categóricas.

tipo <- 1:dim(poblacion)[2]
maximo <- 1:dim(poblacion)[2]
minimo <- 1:dim(poblacion)[2]
media <- 1:dim(poblacion)[2]
desviacion <- 1:dim(poblacion)[2]
primer_cuartil <- 1:dim(poblacion)[2]



for(j in 2:dim(poblacion)[2]){
  if(is.numeric(poblacion[,j])==TRUE){
    tipo[j] <- class(poblacion[,j])
    maximo[j] <- max(poblacion[,j])
    minimo[j] <- min(poblacion[,j])
    media[j] <- mean(poblacion[,j])
    desviacion[j] <- sd(poblacion[,j])
    primer_cuartil[j] <- quantile(poblacion[,j],probs=seq(0,1,0.25),na.rm = FALSE)[2]
  }else {
    
    tipo[j] <- class(poblacion[,j])
    maximo[j] <- NA
    minimo[j] <- NA
    media[j] <- NA
    desviacion[j] <- NA
    primer_cuartil[j] <- NA 
    table(poblacion[,j])/dim(poblacion)[1]
  }
}

resultados1 <- data.frame(names(poblacion),tipo,maximo,minimo,media,desviacion,primer_cuartil)
View(resultados1)

#frecuencias

frecuencia_region  <- table(poblacion[,9])/dim(poblacion)[1]
frecuencia_ser.bas.compl <- table(poblacion[,10])/dim(poblacion)[1]

frecuencia_region
frecuencia_ser.bas.compl

#2.5.- Calcule la correlación entre la variable dependiente poblacion
#  y cada una de las variables explicativas (numéricas).

correlacion <- 3:dim(poblacion)[2]
correlacion[1] <- NA
c

for(j in 2:dim(poblacion)[2]){
  if(is.numeric(poblacion[,j])==TRUE){
    correlacion[j] <- cor(poblacion[,2], poblacion[,j])
  } else {
    correlacion[j] <- NA
  }
}


res <- data.frame(names(poblacion),correlacion) 
View(res)

#2.6.- Considere la variable categórica serv.bas.compl con una conﬁabilidad 
# del 90%, ¿Puede asumirse que la media de la variable poblacion en el 
# grupo serv.bas.compl: SI es distinta a la media del grupo 
#serv.bas.compl: NO ?


serv.bas.compl_fac <- factor(poblacion[,"serv.bas.compl"],levels=c("SI","NO"),labels=c("si","no"))
region_fac <- factor(poblacion[,"region"],levels=c("A","B"),labels=c("a","b"))
poblacion_fac <- data.frame(poblacion[,1:7],region_fac,serv.bas.compl_fac)

#Diagrama de cajas
plot(poblacion ~ serv.bas.compl_fac , data = poblacion_fac) 

# prueba de hipótesis
hipotesis <- t.test(poblacion ~ serv.bas.compl_fac , data=poblacion_fac, conf.level=0.9)
hipotesis


# Aceptamos que la diferencia de medias es igual a cero

#2.7.- Considerando los cálculos anteriores genere el modelo de regresión 
# lineal múltiple que mejor se ajuste a los datos. 
# Interprete los coeﬁcientes obtenidos.

reg1 <- lm(poblacion~var.pobl.mayor+menores.18+tasa.crimen, data=poblacion_fac)
summary(reg1)

#Cuando la población mayor aumenta en una unidad, la población aumenta en promedio 0,029694 unidades
#Cuando la población menores de 18 aumenta en una unidad, la población aumenta en promedio 0,039498 unidades
#Cuando la tasa de crimen aumenta en una unidad, la población disminuye en promedio 0,012813 unidades

#2.8.- Interprete el R2

R2 <- summary(reg1)[["r.squared"]]
R2

#Esto quiere decir que la regresion explica el: 
porcentaje <- 100*summary(reg1)[["r.squared"]] 
porcentaje

#Explica el 15,33% de variabilidad de la regresión

#2.8.-  Analice la signiﬁcancia de la regresión y de cada uno de los 
# parámetros individuales. 

anova <- aov(reg1)
summary(anova)

#2.9.-  Realice un análisis detallado de los residuos.

residuos<-1:40  
for(i in 1:40){
  residuos[i]<-summary(reg1)[["residuals"]][i]
}
residuos

#poblacion vs residuos
plot(poblacion[,"poblacion"],residuos)

#histograma 
hist(residuos)

qqnorm(residuos)
qqline(residuos,col="red")

