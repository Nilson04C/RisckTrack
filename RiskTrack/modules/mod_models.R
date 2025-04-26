
source("./functions/func_models.R")


utilizador_id <- 2

# Módulo UI
# UI
mod_models_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    DTOutput(ns("models_table")),
    
    tags$script(HTML(sprintf("
      $(document).on('click', '.acao-verM', function() {
        var id = this.id.replace('ver_', '');
        Shiny.setInputValue('%s', id, {priority: 'event'});
      });
      
      $(document).on('click', '.acao-apagarM', function() {
        var id = this.id.replace('apagar_', '');
        Shiny.setInputValue('%s', id, {priority: 'event'});
      });
    ", ns("ver_modelo_id"), ns("apagar_modelo_id"))))
  )
}




# Módulo Server
mod_models_server <- function(id, pool, user_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    modelos_rv <- reactiveVal()
    
    
    #Buscar dados para a tabela
    atualizar_tabela_modelos <- function() {
      
      modelos <- getModels(pool, user_id)
      
      modelos$acoes <- sprintf(
        '<button id="ver_%s" class="btn btn-primary btn-sm acao-verM">Avaliação</button>
         <button id="apagar_%s" class="btn btn-danger btn-sm acao-apagarM">Apagar</button>',
        modelos$id, modelos$id
      )
      
      modelos_rv(modelos)
      
      cols_to_remove <- c("id", "caminho", "avaliacao_caminho", "utilizador_id")
      modelos_filtrados <- modelos[, setdiff(names(modelos), cols_to_remove)]
      
      output$models_table <- renderDT({
        datatable(modelos_filtrados,
                  escape = FALSE,
                  selection = 'none',
                  options = list(scrollX = TRUE, pageLength = 10))
      })
    }
    
    
    
    
    # Reage ao clique no botão "ver"
    observeEvent(input$ver_modelo_id, {
      
      modelo <- modelos_rv()[modelos_rv()$id == input$ver_modelo_id, ]
      
      dados <- getAvData(modelo$caminho)
      
      matriz <- as.matrix(dados$table)
      
      showModal(modalDialog(
        
                            renderPlot({
                              req(dados)
                              pheatmap(matriz, 
                                       cluster_rows = FALSE,  # Sem agrupamento nas linhas
                                       cluster_cols = FALSE,  # Sem agrupamento nas colunas
                                       show_colnames = TRUE,
                                       show_rownames = TRUE,
                                       display_numbers = TRUE,  # Mostrar valores dentro das células
                                       color = colorRampPalette(c("white", "red"))(100))  # Gradiente de cor
                            }),
                            
                            # Envolver renderTable com div para rolagem horizontal
                            div(style = "overflow-x: auto;", 
                                renderTable({
                                  metrics <- as.data.frame(dados$byClass)
                                  metrics <- cbind(Métrica = rownames(metrics), metrics)
                                  rownames(metrics) <- NULL
                                  metrics
                                })
                            ),
                            
                            title = modelo$nome,
                            size = "m",
                            easyClose = TRUE
                          ))
    })
    
    
    
    # Reage ao clique no botão "apagar"
    observeEvent(input$apagar_modelo_id, {
      
      showModal(modalDialog(
        title = "Confirmar Apagamento",
        paste("Tens a certeza que queres apagar o modelo com ID:", input$apagar_modelo_id, "?"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("confirmar_apagar_modelo"), "Apagar", class = "btn btn-danger")
        ),
        easyClose = TRUE
      ))
  })
    
    
    observeEvent(input$confirmar_apagar_modelo, {
      removeModal()
      
      modelos <- modelos_rv()
      
      
      id_modelo <- input$apagar_modelo_id
      modelo_escolhido <- modelos[modelos$id == id_modelo, ]
      caminho_modelo <- modelo_escolhido$caminho
      caminho_avaliacao <- modelo_escolhido$avaliacao_caminho
      
      if (file.exists(caminho_modelo)) {
        file.remove(caminho_modelo)
      }
      if (file.exists(caminho_avaliacao)) {
        file.remove(caminho_avaliacao)
      }
      
      dbExecute(pool, paste0("DELETE FROM modelo WHERE id = ", id_modelo))
      
      # Atualiza a tabela depois de apagar
      atualizar_tabela_modelos()
      
    })
    
    
    
    atualizar_tabela_modelos()
    
    return(list(atualizar_tabela = atualizar_tabela_modelos))
})
}

