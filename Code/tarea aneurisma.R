library(readr)
dataset_aneurisma <- read_delim("practice_machine_learning/Data/dataset_aneurisma.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
 aneurisma <- dataset_aneurisma
 # Los valores no numericos hay que trabajarlos como factors
 aneurisma$estado <- as.factor(aneurisma$estado)
 aneurisma$genero <- as.factor(aneurisma$genero)
 
  # estadisticas clasicas de cada variables: min, max, media, median y quatiles
 summary(aneurisma)
 names(aneurisma)
 
 # Anova analysis. La función utilizada es aov(), la estructura es aov(variable a predecir ~ parametros para modelar, data)
 # Tarea: construir variable "estado_bin" que signfica 1 cuando es NOROTO y 0 cuando es ROTO
 res.aov <- aov(estado_bin ~  N_womersley +  wss_mean + wss_max +  osi + rrt + area_aneurisma_mm2 + vol_aneurisma_mm3 + cuello_mm + ancho_mm + alto_mm + ar + nsi + bf + angle +  mh + vol_geo_mm3 + sup_total_mm2 + strain_von_mises_real + deformation + vol_aneurisma + strain_von_mises , data = aneurisma)
 
 # mostrar resultado del analisis ANOVA
 summary(res.aov)  
 str(aneurisma) # veo que clase de dato es, si es numerico, factor, etc.
 