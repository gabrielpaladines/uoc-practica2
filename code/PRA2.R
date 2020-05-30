# Iniciando PRA2
# https://www.kaggle.com/rushirdx/suicide-rates-from-1986-to-2016/data
library(dplyr)
library(knitr)

# 1 DESCRIPCION DEL DATASET

# Lectura de datos
data <- read.csv("../data/suicide.csv", header = TRUE, sep = ",", quote="\"", dec=".",fill = TRUE)
dim(data) 
# Tenemos 27820 observaciones y un total de 12 variables.
summary(data)
# Tipos de datos
str(data)
res <- sapply(data,class)
kable(data.frame(variables=names(res),clase=as.vector(res)))

# 2 INTEGRACION Y SELECCION DE DATOS

# Eliminar columnas
d_suicides <- select(data, -HDI.for.year, -country.year)
str(d_suicides)

# 3 LIMIPIEZA DE DATOS

# 3.1 Los datos que contienen ceros o elementos vacíos
colSums(is.na(d_suicides))
colSums(d_suicides=="")
colSums(d_suicides=="0")

# 3.2 Identificación y tratamiento de valores extremos

# Análisis mediante diagramas de cajas e histogramas
par(mfrow=c(2,2))
for(i in 1:ncol(d_suicides)) {
  if (is.integer(d_suicides[,i])){
    boxplot(d_suicides[,i], main = colnames(d_suicides)[i], width = 100)
    hist(d_suicides[,i], main = colnames(d_suicides)[i])
  }
}

par(mfrow=c(2,2))
cols <- c("sex","age","generation")
for (i in cols){
  if (is.factor(d_suicides[,i])){
    counts <- table(d_suicides[,i])
    barplot(counts,main=paste("Distribution of ", i), 
            xlab=i)
  }
}


# 4 ANALISIS DE LOS DATOS

# 4.1 Seleccion de los grupos de datos que se quieren analizar


# 4.2 Comprobacion de la normalidad y homogeneidad de la varianza
par(mfrow=c(2,2))
cols <- c("suicides_no", "population", "suicides.100k.pop", "gdp_per_capita....")
for (i in cols){
  qqnorm(d_suicides[,i],main = paste("Normal Q-Q Plot for ", i))
  qqline(d_suicides[,i],col="red")
  hist(d_suicides[,i], 
        main=paste("Histogram for ", i), 
        xlab=i, freq = FALSE)
}

# 4.3 Aplicando pruebas estadisticas para comparar grupos