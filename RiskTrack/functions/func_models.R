library(caret)
library(rpart)


# 
criarModelo<- function(dataset, para_treino){
  
  cat("Boraaa   ")
  
  dataset <- dataset[, -1]
  
  
  datasetF = filtrardados(dataset)
  
  nome_variaveis <- names(datasetF)  # Obter os nomes das colunas
  target <- nome_variaveis[length(nome_variaveis)]  # Última coluna como target
  features <- nome_variaveis[-length(nome_variaveis)]  # Todas menos a última
  
  indices_treino = dividirData(datasetF, para_treino, target)
  
  treino <- datasetF[indices_treino, ]
  teste <- datasetF[-indices_treino, ]
  
  print(levels(datasetF[[target]])) 
  
  #print(head(teste[[target]]))
  
  modelo = treinarModelo(treino, target, features)
  avaliacao = avaliarModelo(modelo, target, teste)
}




filtrardados <- function(dataset) {
  
  
  cat("Filtrando Dataset    ")
  
  # Converter todas as colunas para character (evita problemas com classes diferentes)
  dataset[] <- lapply(dataset, function(x) if (is.character(x)) trimws(x) else x)
  
  # Substituir valores problemáticos por NA
  dataset[dataset %in% c("NULL", "", " ", "null", "Null", "nan", "NAN", "Nan")] <- NA
  
  dataset[dataset == "nan"] <- NA
  dataset[dataset == "NULL"] <- NA
  dataset[dataset == ""] <- NA
  dataset[dataset == " "] <- NA
  dataset[dataset == "null"] <- NA
  dataset[dataset == "Null"] <- NA
  dataset[dataset == "NAN"] <- NA
  dataset[dataset == "Nan"] <- NA
  
  
  # Garantir que o target seja tratado como fator (caso necessário)
  # Aqui estamos assumindo que a última coluna é sempre o target
  target <- names(dataset)[ncol(dataset)]
  
  # Converter o target para fator sem forçar níveis
  dataset[[target]] <- as.factor(dataset[[target]])
  
  
  # Remover linhas com qualquer NA ou NaN
  dataset <- dataset[complete.cases(dataset), ]
  
  cat("Dataset Filtrado     ")
  
  return(dataset)
}




dividirData <- function(dataset, para_treino, target){
  
  cat("dividindo Dataset     ")
  
  set.seed(123)  # permite reprodutibilidade
  
  # Fazer a divisão estratificada apenas se o target categórico
  if (is.factor(dataset[[target]]) || is.character(dataset[[target]])) {
    indices <- createDataPartition(dataset[[target]], p = para_treino, list = FALSE)
  } else {
    indices <- sample(1:nrow(dataset), size = para_treino * nrow(dataset))  # Amostragem aleatória
  }
  
  cat("Dataset dividido      ")
  
  return (indices)
  
}




treinarModelo <- function(dados, target, features){
  
  maxdepth = 15
  cp = 0.05
  
  formula_modelo <- criarFormula(dados, target, features)
  
  cat("Treinando Modelo       ")
  
  
  modelo <- rpart(formula_modelo, 
                  data = dados[1:500,], 
                  method = "class", 
                  control = rpart.control(maxdepth = maxdepth, cp = cp))
  cat("Modelo Treinado       ")
  
  return (modelo)
}




criarFormula <- function(dados, target, features) {

  
  formula_texto <- paste(target, "~", paste(features, collapse = " + "))  # Construir fórmula
  formula <- as.formula(formula_texto)  # Converter para fórmula R
  
  return(formula)
}




avaliarModelo <- function(modelo, target, dados){
  
  cat("Hora de avaliar      ")
  
  # Realizar previsões
  previsoes <- predict(modelo, newdata = dados, type = "class")  # para classificação
  
  #print(unique(dados[[target]]))
  #print(levels(previsoes))  # Veja os níveis das previsões
  #print(levels(dados[[target]]))  # Veja os níveis do target real
  
  # Criar a matriz de confusão
  matriz_confusao <- confusionMatrix(previsoes, dados[[target]])
  
  # Retornar as métricas de avaliação
  print(matriz_confusao)
  
  return(matriz_confusao)
}

