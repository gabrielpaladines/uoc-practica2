# Iniciando PRA2
# https://www.kaggle.com/rushirdx/suicide-rates-from-1986-to-2016/data

# Installing R Package Dependencies for R Markdown: 'evaluate', 'glue', 'knitr', 'rmarkdown', 'tinytex', 'xfun'

datos1 <- read.csv("suicide.csv", header = TRUE, sep = ",", quote="\"", dec=".",fill = TRUE)
dim(datos1)

# Tipos de datos. Ejemplo

class(datos1$country)

# Eliminar columnas

datos2 <- select(datos1, -HDI.for.year, -country.year)

# Análisis mediante diagramas de cajas e histogramas

boxplot(datos2$gdp_per_capita....)
hist(datos2$gdp_per_capita....)


