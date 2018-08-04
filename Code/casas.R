library(readr)
kc_house_data <- read_csv("practice_machine_learning/Data/kc_house_data.csv")

casas <- kc_house_data


# estadisticas clasicas de cada variables: min, max, media, median y quatiles
summary(casas)
names(casas)
str(casas) # veo que clase de dato es, si es numerico, factor, etc.


# Anova analysis. La función utilizada es aov(), la estructura es aov(variable a predecir ~ parametros para modelar, data)
# Tarea: construir variable "estado_bin" que signfica 1 cuando es NOROTO y 0 cuando es ROTO
res.aov <- aov( ~   price + bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + , data = casas)

# mostrar resultado del analisis ANOVA
summary(res.aov)  
