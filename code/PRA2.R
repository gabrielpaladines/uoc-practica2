# Iniciando PRA2
# https://www.kaggle.com/rushirdx/suicide-rates-from-1986-to-2016/data
library(dplyr)
library(knitr)
library(ggplot2)

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
cols <- c("suicides_no","population")
for(i in cols) {
  if (is.integer(d_suicides[,i])){
    boxplot(d_suicides[,i], main = i, width = 100)
    hist(d_suicides[,i], main = i)
  }
}

# Por la naturaleza de las variables population y suicides, no siguen una distribución normal.

par(mfrow=c(2,2))
cols <- c("sex","age","generation")
for (i in cols){
  if (is.factor(d_suicides[,i])){
    counts <- table(d_suicides[,i])
    barplot(counts,main=paste("Distribution of ", i), 
            xlab=i)
  }
}

# La variable sex tiene dos clases: female y male, ambas con la misma cantidad de registros
# La variable age tiene 6 clases: "", ambas con la misma cantidad de registros.
# La variable generation tiene 6 clases con diferente cantidad de registros.


# 4 ANALISIS DE LOS DATOS

# 4.1 Seleccion de los grupos de datos que se quieren analizar

# ¿Es mayor la tasa de suicidios en hombres que en mujeres (en función del porcentaje de población)?
#  - Atributos: sex, suicides_no, population
#  - Calcular la tasa de suicidios por sex.
#  - Gráfica o tabla donde se muestra una comparativa entre la tasa de suicidios entre hombres y mujeres.



#¿Qué generación tiene una tasa más alta de suicidios según el nivel de riqueza de su país en el 2008 durante la crisis económica mundial?
  
#  -  Atributos: suicides_no, population, generation, gdp_per_capita...., country, year
#  -	Clasificar los países según el producto interno bruto gdp_for_capita para el 2008.
#  -	Agregar una columna nueva con la clasificación en 3 grupos (Primer mundo, segundo mundo, tercer mundo), para esto utilizaremos un algoritmo de clasificación no supervisado.
#  -  Calcular la tasa de suicidios por generación / nivel de riqueza.
#  -	Comparativa por generación y tasa de suicidios para el año 2008.


#¿Cómo ha evolucionado la tasa de suicidios desde el 1985 en España? 

# - Atributos: suicides_no, population, year, country=Spain,  
# -	Columna que sume el número por cada año (actualmente por cada anio existen 12 registros, 2 sex x 6 age).
#-	Columna que sume la población por cada año.
#-	Calcular la tasa de suicidios por cada año.
#-	Graficar para identificar de picos o valores altos.

# Proyección del número de suicidios en los siguientes 5 años.

# Nota: Pregunta derivada en función de los resultados de la pregunta anterior.


# 4.2 Comprobacion de la normalidad y homogeneidad de la varianza

# Análisis de Normalidad

par(mfrow=c(2,2))
cols <- c("suicides_no", "population")
for (i in cols){
  qqnorm(d_suicides[,i],main = paste("Normal Q-Q Plot for ", i))
  qqline(d_suicides[,i],col="red")
  hist(d_suicides[,i], 
        main=paste("Histogram for ", i), 
        xlab=i, freq = FALSE)
}
 # No son variables normales...

# Análisis de homogeneidad de la varianza

# 4.3 Aplicando pruebas estadisticas para comparar grupos


# ¿Es mayor la tasa de suicidios en hombres que en mujeres (en función del porcentaje de población)?
#  - Atributos: sex, suicides_no, population
#  - Calcular la tasa de suicidios por sex.
#  - Gráfica o tabla donde se muestra una comparativa entre la tasa de suicidios entre hombres y mujeres.
df  <- d_suicides %>% select(sex, suicides_no, population)
df[2:3] <- lapply(df[2:3], as.numeric)
report <- df %>%
  group_by(sex) %>%
  summarise_all(funs(sum)) %>%
  mutate(suicides_100k_pop = suicides_no / population * 100000)

theme_set(theme_classic())
g <- ggplot(report, aes(sex,suicides_100k_pop))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Bar Chart", 
       subtitle="Tasa de suicidios por genero") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


