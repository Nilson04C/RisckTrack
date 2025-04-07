
source("./functions/func_previsao.R")


utilizador_id <- 2

# Módulo UI
mod_prediction_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    DTOutput(ns("pred_table")),
    
    tags$script(HTML(sprintf("
      $(document).on('click', '.acao-ver', function() {
        var id = this.id.replace('ver_', '');
        Shiny.setInputValue('%s', id, {priority: 'event'});
      });
      
      $(document).on('click', '.acao-apagar', function() {
        var id = this.id.replace('apagar_', '');
        Shiny.setInputValue('%s', id, {priority: 'event'});
      });
    ", ns("ver_pred_id"), ns("apagar_pred_id"))))
  )
}




# Módulo Server
mod_prediction_server <- function(id, estado_pagina, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    utilizador_id <- 2  # <-- ou passar por parâmetro depois
    
    previsoes <- getPredFromModels(pool, utilizador_id)
    
    previsoes$acoes <- sprintf(
      '<button id="ver_%s" class="btn btn-primary btn-sm acao-ver">Ver</button>
       <button id="apagar_%s" class="btn btn-danger btn-sm acao-apagar">Apagar</button>',
      previsoes$id, previsoes$id
    )
    
    cols_to_remove <- c("id", "caminho")
    prev_filtradas <- previsoes[, setdiff(names(previsoes), cols_to_remove)]
    
    output$pred_table <- renderDT({
      datatable(prev_filtradas, escape = FALSE, selection = 'none',
                options = list(scrollX = TRUE, pageLength = 10))
    })
    
    observeEvent(input$ver_pred_id, {
      
      # *função para mostrar os dados da provisão*
      
      showModal(modalDialog(
        title = "Visualizar Previsão",
        paste("Clicaste em ver a previsão com ID:", input$ver_pred_id)
      ))
    })
    
    observeEvent(input$apagar_pred_id, {
      
      # *função para apagra a previsão*
      
      showModal(modalDialog(
        title = "Apagar Previsão",
        paste("Clicaste em apagar a previsão com ID:", input$apagar_pred_id)
      ))
    })
  })
}
