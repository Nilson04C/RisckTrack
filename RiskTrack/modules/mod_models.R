
source("./functions/func_models.R")


utilizador_id <- 2

# Módulo UI
# UI
mod_models_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    DTOutput(ns("models_table")),
    
    tags$script(HTML(sprintf("
      $(document).on('click', '.acao-ver', function() {
        var id = this.id.replace('ver_', '');
        Shiny.setInputValue('%s', id, {priority: 'event'});
      });
      
      $(document).on('click', '.acao-apagar', function() {
        var id = this.id.replace('apagar_', '');
        Shiny.setInputValue('%s', id, {priority: 'event'});
      });
    ", ns("ver_modelo_id"), ns("apagar_modelo_id"))))
  )
}



observeEvent(input$ver_modelo_id, {
  id_modelo <- input$ver_modelo_id
  print(paste("Clicaste em ver o modelo com id:", id_modelo))
  # Aqui podes abrir um modal, mostrar info, etc.
})



# Módulo Server
mod_models_server <- function(id, estado_pagina, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    utilizador_id <- 2  # <-- ou passa como parâmetro depois
    
    modelos <- getModels(pool, utilizador_id)
    
    modelos$acoes <- sprintf(
      '<button id="ver_%s" class="btn btn-primary btn-sm acao-ver">Avaliação</button>
       <button id="apagar_%s" class="btn btn-danger btn-sm acao-apagar">Apagar</button>',
      modelos$id, modelos$id
    )
    
    # Colunas a esconder
    cols_to_remove <- c("id", "caminho", "avaliacao_caminho", "utilizador_id")
    modelos_filtrados <- modelos[, setdiff(names(modelos), cols_to_remove)]
    
    output$models_table <- renderDT({
      datatable(modelos_filtrados,
                escape = FALSE,
                selection = 'none',
                options = list(scrollX = TRUE, pageLength = 10))
    })
    
    # Reage ao clique no botão "ver"
    observeEvent(input$ver_modelo_id, {
      
      # *função para mostrar dos a avaliação*
      
      showModal(modalDialog(
        title = "Avaliação do Modelo",
        paste("Clicaste em ver o modelo com ID:", input$ver_modelo_id)
      ))
    })
    
    # Reage ao clique no botão "apagar"
    observeEvent(input$apagar_modelo_id, {
      
      # *função para apagar o modelo*
      
      showModal(modalDialog(
        title = "Apagar Modelo",
        paste("Clicaste em apagar o modelo com ID:", input$apagar_modelo_id)
      ))
    })
  })
}
