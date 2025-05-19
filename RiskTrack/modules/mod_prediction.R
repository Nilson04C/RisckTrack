
source("./functions/func_previsao.R")



# Módulo UI
mod_prediction_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    DTOutput(ns("pred_table")),
    
    tags$script(HTML(sprintf("
      $(document).on('click', '.acao-verP', function() {
        var id = this.id.replace('ver_', '');
        Shiny.setInputValue('%s', id, {priority: 'event'});
      });
      
      $(document).on('click', '.acao-apagarP', function() {
        var id = this.id.replace('apagar_', '');
        Shiny.setInputValue('%s', id, {priority: 'event'});
      });
    ", ns("ver_pred_id"), ns("apagar_pred_id"))))
  )
}




# Módulo Server
mod_prediction_server <- function(id, estado_pagina, pool, user_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    previsoes_rv <- reactiveVal()
    
    # obter as Previsões
    atualizar_tabela_previsoes <- function() {
    
      previsoes <- getPreds(pool, user_id)
      
      previsoes$acoes <- sprintf(
        '<button id="ver_%s" class="btn btn-primary btn-sm acao-verP">Ver</button>
         <button id="apagar_%s" class="btn btn-danger btn-sm acao-apagarP">Apagar</button>',
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
      
      previsao <- previsoes_rv()[previsoes_rv()$id == input$ver_pred_id, ]
      
      dados <- getPredData(pool, previsao$caminho)
      
      showModal(modalDialog(
                            renderDT({
                              datatable(dados, 
                                        escape = FALSE, 
                                        extensions = 'Buttons',
                                        options = list(
                                          scrollX = TRUE, 
                                          pageLength = 10,
                                          dom = 'Bfrtip',  # This positions the Buttons at the top
                                          buttons = c('copy', 'csv', 'excel', 'pdf')
                                        )
                              )  # Tabela interativa
                            }),
                            title = previsao$nome,
                            size = "l",
                            easyClose = TRUE
                ))
    }, ignoreInit = TRUE)
    
    
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
    }, ignoreInit = TRUE)
    
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
      
    return(list(atualizar_tabela = atualizar_tabela_previsoes))
    
  })
}
