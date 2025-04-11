library(caret)
library(rpart)
library(tidymodels)



getModels <- function(pool, userId){
  
  query <- paste0("SELECT * FROM modelo where utilizador_id =", userId)
                   
  resultado <- dbGetQuery(pool, query)
  
  return (resultado)
}



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
criarModelo<- function(dataset, percentPara_treino, useCrossV){
  
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
  
  
  #Usar Cross Validarion ou não? eis a questão
  if(useCrossV){
    
    res <- treinarModeloCrossV(treino, target, features)
    modelo <- res$modelo
    }
  
  else{modelo = treinarModelo(treino, target, features)}
  
  
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
  
  #str(dados)  # Mostra a estrutura do dataset
  
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
  #print(matriz_confusao)
  
  return(matriz_confusao)
}







treinarModeloCrossV <- function(dados, target, features, 
                                cp_values = c(0.001, 0.005, 0.01, 0.05), 
                                maxdepth_values = c(5, 10, 15), 
                                k = 5) {
  
  formula_modelo <- criarFormula(dados, target, features)
  
  
  # Criar grid de parâmetros
  grid <- expand.grid(cp = cp_values, maxdepth = maxdepth_values)
  
  
  # Criar folds
  set.seed(123)
  folds <- createFolds(dados[[target]], k = k, list = TRUE)
  
  resultados <- data.frame(cp = numeric(), maxdepth = numeric(), Accuracy = numeric())
  
  
  
  for (i in 1:nrow(grid)) {
    cp <- grid$cp[i]
    maxd <- grid$maxdepth[i]
    
    accs <- c()
    
    for (fold in folds) {
      treino <- dados[-fold, ]
      teste <- dados[fold, ]
      
      modelo <- rpart(formula_modelo,
                      data = treino,
                      method = "class",
                      control = rpart.control(cp = cp, maxdepth = maxd))
      
      pred <- predict(modelo, teste, type = "class")
      
      f1 <- f1_score(teste[[target]], pred)
      accs <- c(accs, f1)
      
    }

    resultados <- rbind(resultados, data.frame(cp = cp, maxdepth = maxd, f1_score  = mean(accs)))
  }
  
  melhor_linha <- resultados[which.max(resultados$f1_score), ]
  print(melhor_linha)
  
  # Treinar modelo final com os melhores parâmetros
  modelo_final <- rpart(formula_modelo,
                        data = dados,
                        method = "class",
                        control = rpart.control(cp = melhor_linha$cp,
                                                maxdepth = melhor_linha$maxdepth))
  
  cat("Modelo Treinado com parâmetros otimizados  \n")
  
  return(list(modelo = modelo_final, resultados_cv = resultados))
}
  


f1_score <- function(y_true, y_pred, average = "macro") {
  cm <- confusionMatrix(as.factor(y_pred), as.factor(y_true))
  
  precision <- cm$byClass[, "Precision"]
  recall <- cm$byClass[, "Recall"]
  support <- as.numeric(table(y_true))  # Nº de exemplos por classe
  
  f1 <- ifelse(precision + recall == 0, 0,
               2 * precision * recall / (precision + recall))
  
  if (average == "macro") {
    return(mean(f1, na.rm = TRUE))
  } else if (average == "weighted") {
    return(sum(f1 * support, na.rm = TRUE) / sum(support))
  } else if (average == "micro") {
    # TP e FN totais
    total_tp <- sum(diag(cm$table))
    total_fn <- sum(cm$table) - total_tp
    precision_micro <- total_tp / sum(cm$table)
    recall_micro <- total_tp / (total_tp + total_fn)
    return(2 * precision_micro * recall_micro / (precision_micro + recall_micro))
  } else {
    stop("Average inválido. Usa: 'macro', 'weighted' ou 'micro'.")
  }
}


