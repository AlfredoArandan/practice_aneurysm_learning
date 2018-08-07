library(readr)
dataset_aneurisma <- read_delim("Data/dataset_aneurisma.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
 aneurisma <- dataset_aneurisma
 
 # Los valores no numericos hay que trabajarlos como factors
 aneurisma$estado <- as.factor(aneurisma$estado)
 aneurisma$genero <- as.factor(aneurisma$genero)
 
 # Estadisticas clasicas de cada variables: min, max, media, median y quatiles
 summary(aneurisma)
 names(aneurisma)
 
 # veo que clase de dato es, si es numerico, factor, etc.
 str(aneurisma) 
 
 # Anova analysis. La función utilizada es aov(), la estructura es aov(variable a predecir ~ parametros para modelar, data)
 # Tarea: construir variable "estado_bin" que signfica 1 cuando es NOROTO y 0 cuando es ROTO
 res.aov <- aov(estado_bin ~  N_womersley +  wss_mean + wss_max +  osi + rrt + area_aneurisma_mm2 + 
                  vol_aneurisma_mm3 + cuello_mm + ancho_mm + alto_mm + ar + nsi + bf + angle +  mh + 
                  vol_geo_mm3 + sup_total_mm2 + strain_von_mises_real + deformation + vol_aneurisma + 
                  strain_von_mises , data = aneurisma)
 
 # Mostrar resultado del analisis ANOVA
 summary(res.aov)  
 

  # Parte II
 
 library(caret)
 
 #Creación de partición de los datos para entrenamiento
 
 set.seed(1)
 intrain <- createDataPartition(y = aneurisma$estado_bin, p= 0.8, list = FALSE)
 training <- aneurisma[intrain,]
 testing <- aneurisma[-intrain,]
 
 
 #Train control para la prediccion
 
 control_train <- trainControl(method = "repeatedcv", number = 10,
                               repeats = 3)
                               #classProbs = FALSE,
                               #summaryFunction = twoClassSummary,
                               #savePredictions = TRUE)
 
 #Modelo 1
 
 set.seed(1)
 modelo_svm<- train(estado_bin ~ N_womersley +  wss_mean + wss_max +  osi + rrt + area_aneurisma_mm2 + 
                      vol_aneurisma_mm3 + cuello_mm + ancho_mm + alto_mm + ar + nsi + bf + angle +  mh + 
                      vol_geo_mm3 + sup_total_mm2 + strain_von_mises_real + deformation + vol_aneurisma + 
                      strain_von_mises, data = training,
                    method = "svmRadial",
                    #num_trees = 300,
                    trControl = control_train)
 summary(modelo_svm)
 predicciones <- predict(modelo_svm, testing)
 error <- sqrt((sum((predicciones-testing$estado_bin)^2))/nrow(testing))
 plot(testing$estado_bin, predicciones)
 abline(0,1)
 mean(aneurisma$estado_bin)
 cor(testing$estado_bin, predicciones)
 