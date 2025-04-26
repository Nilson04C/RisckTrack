library(rpart)


getPreds <- function(pool, utilizador_id){
  
  query <- paste0("
  SELECT 
    modelo.nome AS modelo_usado,
    previsao.nome as nome,
    previsao.id as id,
    previsao.caminho as caminho,
    previsao.data_criacao
    
  FROM 
    previsao
    
  LEFT JOIN 
    modelo ON modelo.id = previsao.modelo_id
    
  WHERE
    modelo.utilizador_id =",utilizador_id(),"
    
  ORDER BY 
    modelo.nome, previsao.data_criacao DESC;
")
  
  resultado <- dbGetQuery(pool, query)
  
  
  return (resultado)
}


getPredData <- function(pool, path)
{
  resultado <- readRDS(path)
  return(resultado)
}



fazer_previsao <- function(modelo, dataset){
  
  previsao <- predict(modelo, newdata = dataset, type = "class")
  
  dataset$prediction <- previsao
  #print (previsao)
  
  return(dataset)
  
}

savepred <- function(name, dados, pool, model_id){
  
  saveRDS(dados, paste0(name, ".rds"))
  print("previsão Salva nos arquivos")
  
  query = paste0("INSERT INTO previsao (nome, data_criacao, modelo_id, caminho) 
      VALUES ('",name,"', NOW(), ", model_id, ", '",getwd(),"/",name,".rds');")
  
  print("previsão Salva na base de dados")
  
  dbExecute(pool, query)
}
  