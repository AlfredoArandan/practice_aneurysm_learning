library(readr)
kc_house_data <- read_csv("Data/kc_house_data.csv")

casas <- kc_house_data


# estadisticas clasicas de cada variables: min, max, media, median y quatiles
summary(casas)
names(casas)
str(casas) # veo que clase de dato es, si es numerico, factor, etc.


# Anova analysis. La función utilizada es aov(), la estructura es aov(variable a predecir ~ parametros para modelar, data)

res.aov <- aov( price ~  bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + 
                  condition + grade + sqft_above + sqft_basement + yr_built + yr_renovated + sqft_living15
                + sqft_lot15, data = casas)

# mostrar resultado del analisis ANOVA
summary(res.aov)  
nrow(casas)
head(casas)
table(casas$floors) #conteo de cuantas casas tienen esa cantidad de pisos


# Modelando prediccion


install.packages("caret")

library(caret)


set.seed(1)
intrain <- createDataPartition(y = casas$price, p= 0.6, list = FALSE)
training <- casas[intrain,]
testing <- casas[-intrain,]

#trctrl <- trainControl(method = "repeatedcv", number = 10,
                     #  repeats = 3,
                      # classProbs = TRUE,
                       #summaryFunction = twoClassSummary,
                       #savePredictions = TRUE)


set.seed(1)
modelo1 <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + waterfront + view + condition + grade + 
                sqft_above + yr_built + sqft_living15 + sqft_lot15, data=training)
summary(modelo1)

predicciones <- predict(modelo1, testing)

#Error de predicción del modelo. Al tratarse de una variable continua se emplea como medida 
#de error el MSE (mean square error).

modelo2 <- glm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + waterfront + view + condition + grade + 
                 sqft_above + yr_built + sqft_living15 + sqft_lot15, data=casa)

summary(modelo1)
