
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
    
    previsoes_rv <- reactiveVal()
    
    # obter as Previsões
    atualizar_tabela_previsoes <- function() {
    
      previsoes <- getPredFromModels(pool, utilizador_id)
      
      previsoes$acoes <- sprintf(
        '<button id="ver_%s" class="btn btn-primary btn-sm acao-ver">Ver</button>
         <button id="apagar_%s" class="btn btn-danger btn-sm acao-apagar">Apagar</button>',
        previsoes$id, previsoes$id
      )
      
      previsoes_rv(previsoes)
      
      cols_to_remove <- c("id", "caminho")
      prev_filtradas <- previsoes[, setdiff(names(previsoes), cols_to_remove)]
      
      output$pred_table <- renderDT({
        datatable(prev_filtradas, escape = FALSE, selection = 'none',
                  options = list(scrollX = TRUE, pageLength = 10))
      })
    }
    
    # Ver previsão
    observeEvent(input$ver_pred_id, {
      
      # *função para mostrar os dados da provisão*
      
      showModal(modalDialog(
        title = "Visualizar Previsão",
        paste("Clicaste em ver a previsão com ID:", input$ver_pred_id)
      ))
    })
    
    
    # Apagar previsão
    observeEvent(input$apagar_pred_id, {
      
      showModal(modalDialog(
        title = "Confirmar Apagamento",
        paste("Tens a certeza que queres apagar a previsão com ID:", input$apagar_pred_id, "?"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("confirmar_apagar_prev"), "Apagar", class = "btn btn-danger")
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirmar_apagar_prev, {
      removeModal()
      
      previsoes <- previsoes_rv()
      
      id_previsao <- input$apagar_pred_id
      prev_escolhida <- previsoes[previsoes$id == id_previsao, ]
      caminho_prev <- prev_escolhida$caminho
      
      print(id_previsao)
      print(caminho_prev)
      
      if (file.exists(caminho_prev)) {
        file.remove(caminho_prev)
      }
      
      dbExecute(pool, paste0("DELETE FROM previsao WHERE id = ", id_previsao))
      
      # Atualiza a tabela depois de apagar
      atualizar_tabela_previsoes()
      
    })
    
    atualizar_tabela_previsoes()
    
  })
}
