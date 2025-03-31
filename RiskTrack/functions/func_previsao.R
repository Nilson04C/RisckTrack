library(rpart)

fazer_previsao <- function(modelo, dataset){
  
  previsao <- predict(modelo, newdata = dataset, type = "raw")
  
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
  