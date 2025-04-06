
source("./functions/func_previsao.R")


utilizador_id <- 2

# Módulo UI
mod_prediction_ui <- function(id) {
  ns <- NS(id)
  
  DTOutput(ns("pred_table"))
}



# Módulo Server
mod_prediction_server <- function(id, estado_pagina, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    previsoes <- getPredFromModels(pool, utilizador_id)
    
    
    previsoes$acoes <- sprintf(
      '<button id="ver_%s" class="btn btn-primary btn-sm">Ver</button>
       <button id="apagar_%s" class="btn btn-danger btn-sm">Apagar</button>',
      previsoes$id, previsoes$id
    )
    
    # Lista de colunas para remover
    cols_to_remove <- c("id", "caminho")
    
    # Filtra as colunas que não estão na lista de remoção
    prev_filtradas <- previsoes[, setdiff(names(previsoes), cols_to_remove)]
    
    
    # tabela que exibe os dados da previsão
    output$pred_table <- renderDT({
      datatable(prev_filtradas, escape = FALSE, selection = 'none',
                options = list(
                  scrollX = TRUE,
                  pageLength = 10 # Número de linhas visíveis por página
                ))
    })
    
  })
}
