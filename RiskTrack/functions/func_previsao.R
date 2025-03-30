library(rpart)

fazer_previsao <- function(modelo, dataset){
  
  previsoes <- predict(modelo, newdata = dataset, type = "raw")
  
  print (previsoes)
  
}