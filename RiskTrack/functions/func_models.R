library(caret)
library(rpart)
library(tidymodels)


getmodel_path <- function(pool, id){
  
  query <- paste0("SELECT caminho FROM modelo where id =", id)
  resultado <- dbGetQuery(pool, query)
  
  return (as.character(resultado))
}



getmodels_list <- function(pool){
  
  query <- "SELECT id, nome FROM modelo"
  resultados <- dbGetQuery(pool, query)
  
  modelos <- setNames(resultados$id, resultados$nome)
  
  return (modelos)
  
}

# 
criarModelo<- function(dataset, percentPara_treino){
  
  cat("Boraaa   ")
  
  #dataset <- dataset[, -1]
  
  
  datasetF = tratardados(dataset)
  
  nome_variaveis <- names(datasetF)  # Obter os nomes das colunas
  target <- nome_variaveis[length(nome_variaveis)]  # Última coluna como target
  features <- nome_variaveis[-length(nome_variaveis)]  # Todas menos a última
  
  indices_treino = dividirData(datasetF, percentPara_treino, target)
  
  treino <- datasetF[indices_treino, ]
  teste <- datasetF[-indices_treino, ]
 
  
  #print(head(teste[[target]]))
  
  #modelo = treinarModeloComParametrosOtimizado(treino, target, features)
  modelo = treinarModelo(treino, target, features)
  avaliacao = avaliarModelo(modelo, target, teste)
  return(list(modelo = modelo, avaliacao = avaliacao))
}




tratardados <- function(dataset) {
  
  
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
  
  
  #substituir os valores em falta pela média ou moda
    dataset[] <- lapply(dataset, function(coluna) {
      
      if (is.numeric(coluna)) {
        media <- mean(coluna, na.rm = TRUE)
        coluna[is.na(coluna)] <- media
        cat("lá dentro")
        
      } else {
        # Substituir NAs pela moda (classe mais frequente)
        tab <- table(coluna, useNA = "no")  # Conta a frequência das categorias
        moda <- names(tab[tab == max(tab)])  # Obtém a classe mais frequente
        coluna[is.na(coluna)] <- moda[1] 
        
        # Se for texto, converte para fator
        coluna <- as.factor(coluna)
      }
      return(coluna)
    })
    
  
  # Remover linhas com qualquer NA ou NaN
  dataset <- dataset[complete.cases(dataset), ]
  
  cat("Dataset Filtrado     ")
  
  return(dataset)
}




dividirData <- function(dataset, para_treino, target){
  
  cat("dividindo Dataset     ")
  
  #set.seed(123)  # permite reprodutibilidade
  
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
  
  str(dados)  # Mostra a estrutura do dataset
  
  maxdepth = 15
  cp = 0.001
  
  formula_modelo <- criarFormula(dados, target, features)
  
  cat("Treinando Modelo       ")
  
  
  modelo <- rpart(formula_modelo, 
                  data = dados, 
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
  previsoes <- predict(modelo, newdata = dados, type = "class")  # "raw" é usado para classificação quando usa-se tidymodels, usando o rpart podemos usar o "class"
  
  #print(unique(dados[[target]]))
  #print(levels(previsoes))  # Veja os níveis das previsões
  #print(levels(dados[[target]]))  # Veja os níveis do target real
  
  # Criar a matriz de confusão
  matriz_confusao <- confusionMatrix(previsoes, dados[[target]])
  
  # Retornar as métricas de avaliação
  print(matriz_confusao)
  
  return(matriz_confusao)
}







treinarModeloComParametrosOtimizado <- function(dados, target, features) {
  
  #dados[[target]] <- factor(dados[[target]], levels = make.names(levels(dados[[target]])))
  
  #str(dados)  # Mostra a estrutura do dataset
  
  # Imprimir os níveis (classes) do target (coluna 'Passed')
  #print(levels(dados[[target]]))
  
  
  #print(sum(is.na(dados)))  # Mostra quantos NAs existem no dataset
  #print(colSums(is.na(dados)))  # Mostra quantos NAs há em cada coluna
  
  
  # Definir a grade de parâmetros apenas para cp
  tuneGrid <- expand.grid(
    cp = c(0.001, 0.005, 0.05, 0.01, 0.1, 0.3) # Parâmetro de complexidade da árvore
  )
  
  # Definir o controle de treinamento com validação cruzada
  trainControlObj <- trainControl(
    method = "cv",                     # Validação cruzada
    number = 5,                        # 10-fold cross-validation
    search = "grid",                    # Usar uma grade de parâmetros
    classProbs = TRUE,                  # Para cálculos de probabilidades em problemas de classificação
    #summaryFunction = twoClassSummary   # Para calcular AUC, precisão, etc.
  )
  
  # Criar a fórmula para o modelo
  formula_modelo <- criarFormula(dados, target, features)
  
  cat("Treinando Modelo com parâmetros otimizados  \n")
  
  
  
  # Treinamento do modelo com ajuste do parâmetro cp
  modelo <- train(
    formula_modelo,                     
    data = dados,                        
    method = "rpart",                    
    trControl = trainControlObj,         
    tuneGrid = tuneGrid,                 
    metric = "Accuracy",                 
    control = rpart.control(maxdepth = 10)  # Controla a profundidade da árvore
  )
  
  print(modelo$bestTune)  # Ver qual foi o melhor valor de cp encontrado
  
  
  cat("Modelo Treinado com parâmetros otimizados  \n")
  
  return(modelo)
}


