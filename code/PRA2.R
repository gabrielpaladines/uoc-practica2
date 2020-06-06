# Tipología y ciclo de vida de los datos - Práctica 2
# Autores: Gabriel Paladines y Jaime Pardo
# Dataset: https://www.kaggle.com/rushirdx/suicide-rates-from-1986-to-2016/data
library(dplyr)
library(knitr)
library(ggplot2)
library(stats)

# 1 DESCRIPCIÓN DEL DATASET
# ¿Por qué es importante y qué pregunta/problema pretende responder?
# Este dataset contiene diversos parámetros con los que se puede establecer correlación/causalidad con el suicidio. 
# Pretende responder a preguntar del tipo: ¿el suicidio afecta más a los hombres o a las mujeres? 
# ¿tiene más impacto entre personas de una determinada edad o generación?
# ¿cómo influye el nuvel de riqueza del país en el que viven?

# Lectura de datos
data <- read.csv("../data/suicide.csv", header = TRUE, sep = ",", quote="\"", dec=".",fill = TRUE)
dim(data) 
# Tenemos 27820 observaciones y un total de 12 variables. Entre otras:
# La variable sex tiene dos clases: female y male, ambas con la misma cantidad de registros.
# La variable age tiene 6 clases, todas ellas con la misma cantidad de registros.
# La variable generation tiene 6 clases con diferente cantidad de registros.
summary(data)
# Tipos de datos
str(data)
res <- sapply(data,class)
kable(data.frame(variables=names(res),clase=as.vector(res)))


# 2 INTEGRACIÓN Y SELECCIÓN DE LOS DATOS DE INTERÉS A ANALIZAR

# Eliminar columnas
# HDI.for.year tiene demasiados vacíos y country.year es un campo derivado
d_suicides <- select(data, -HDI.for.year, -country.year)
str(d_suicides)


# 3 LIMPIEZA DE LOS DATOS

# 3.1 ¿Los datos contienen ceros o elementos vacíos? ¿Cómo gestionarías cada uno de estos casos?
# Suicides y Suicides/100kpop tienen ceros, pero son valores correctos y no requieren una gestión especial
# HDI.for.year contiene un 70% de vacíos. Es difícil completar con valores fiables, por ello se decide descartarlo.
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


# 4 ANÁLISIS DE LOS DATOS

# 4.1 Selección de los grupos de datos que se quieren analizar/comparar

# CASO 1: ¿Es mayor la tasa de suicidios en hombres que en mujeres (en función del porcentaje de población)?
# Seleccionamos atributos: sex, suicides_no, population.
# Calculamos la tasa de suicidios por sexo.
# Representamos gráficamente una comparativa entre la tasa de suicidios de hombres y mujeres.

df  <- d_suicides %>% select(sex, suicides_no, population)
df[2:3] <- lapply(df[2:3], as.numeric)
report_case1 <- df %>%
  group_by(sex) %>%
  summarise_all(funs(sum)) %>%
  mutate(suicides_100k_pop = suicides_no / population * 100000)
head(report_case1)
theme_set(theme_classic())
par(mfrow=c(1,1))
g_case1 <- ggplot(report_case1, aes(sex,suicides_100k_pop)) + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="CASO 1: Comparativo tasa de suicidios entre hombes y mujeres", 
       subtitle="Tasa de suicidios por genero") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

g_case1

# Porcentaje suicidios hombres y mujeres respectivamente:

#percent_male = rate_male / (rate_male + rate_female)
#percent_female = rate_female / (rate_male + rate_female)
#percent_male
#percent_female

# Conclusión: la tasa de suicidios en hombres es mayor que en mujeres, más del triple (20.7 vs. 5.94).
# En total representan el 77,7% de los suicidios.


# CASO 2: ¿Qué generación tiene mayor tasa de suicidios según el nivel de riqueza de su país en 2008 (crisis económica mundial?
# Clasificamos los países según el producto interior bruto (gdp_per_capita) para el 2008.
# Calculamos la tasa de suicidios por generación / nivel de riqueza.
# Comparamos por generación y tasa de suicidios para el año 2008.

# Empezamos clasificando los países en 3 niveles de riqueza, mediante el algoritmo k-means.
# Se genera la columna nivel_riqueza (1=Segundo mundo; 2=Tercer; 3=Primer)
# Atributos: suicides_no, population, generation, gdp_per_capita...., country, year

suicides_2008 <- filter(d_suicides, year=='2008')

modelo <- kmeans(suicides_2008$gdp_per_capita...., centers = 3)
modelo
modelo$cluster
par(mfrow=c(1,1))
plot(modelo$cluster)

suicides_2008$nivel_riqueza <- modelo$cluster
head(suicides_2008)

report_case2 <- tibble()

for (idx in (1:3)) {
  suicides_2008_riqueza <- filter(suicides_2008, nivel_riqueza==idx)
  df  <- suicides_2008_riqueza %>% select(generation, suicides_no, population)
  
  df[2:3] <- lapply(df[2:3], as.numeric)
  report <- df %>%
    group_by(generation) %>%
    summarise_all(funs(sum)) %>%
    mutate(suicides_100k_pop = suicides_no / population * 100000) %>%
    mutate(nivel_riqueza= paste("Nivel ", idx))
  
  print(paste("Nivel de Riqueza ", idx))
  print(report)
  report_case2 <- rbind(report_case2, report)
}

head(report_case2)

g_case2 <- ggplot(report_case2, aes(fill=nivel_riqueza,y=suicides_100k_pop,x=generation)) + geom_bar(position="dodge", stat="identity") + 
  labs(title="CASO 2: Comparativo tasa de suicidios de acuerdo a su generacion y nivel de riqueza")
g_case2

# CASO 3: ¿Cómo ha evolucionado la tasa de suicidios desde 1985 en España? 
# Atributos: suicides_no, population, year, country=Spain  
# Creamos una columna que sume el número de suicidios por año (para cada año existen 12 registros, 2 sex x 6 age).
# Creamos una columna que sume la población por año.
# Calculamos la tasa de suicidios por año.
# Graficamos para identificar picos o valores altos.

suicides_spain <- select(data, country, year, suicides_no, population)
suicides_spain <- filter(d_suicides, country=='Spain')
df  <- suicides_spain %>% select(year, suicides_no, population)
df[2:3] <- lapply(df[2:3], as.numeric)
report_case3 <- df %>%
  group_by(year) %>%
  summarise_all(funs(sum)) %>%
  mutate(suicides_100k_pop = suicides_no / population * 100000)
head(report_case3)

g_case3 <- ggplot(report_case3, aes(x=year,y=suicides_100k_pop)) + geom_line(stat="identity", color="darkgreen", linetype="dotted") + 
  labs(title="CASO 3: Evolución de la tasa de suicidios en España desde 1985 hasta 2015") +
  geom_point(color="darkgreen")
g_case3

# 4.2 Comprobación de la normalidad y homogeneidad de la varianza

# Análisis de normalidad

par(mfrow=c(2,2))
cols <- c("suicides_no", "population")
for (i in cols){
  qqnorm(d_suicides[,i],main = paste("Normal Q-Q Plot for ", i))
  qqline(d_suicides[,i],col="red")
  hist(d_suicides[,i], 
       main=paste("Histogram for ", i), 
       xlab=i, freq = FALSE)
}
# Se concluye que no son variables normales.
# Análisis de homogeneidad de la varianza: no aplica.

# 4.3 Aplicación de pruebas estadisticas para comparar los grupos de datos


# 5 REPRESENTACIÓN DE LOS RESULTADOS A PARTIR DE TABLAS Y GRÁFICAS
par(mfrow=c(2,2))
report_case1
g_case1
report_case2
g_case2
report_case3
g_case3

# 6 RESOLUCIÓN DEL PROBLEMA
# A partir de los resultados obtenidos, ¿cuáles son las conclusiones? ¿los resultados permiten responder al problema?


# 7 CÓDIGO

#  Se adjunta el código 

