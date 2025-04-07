# Módulo UI
mod_dashboard_ui<- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "grid",
      
      # Grid de modelos renderizado dinamicamente
      uiOutput(ns("grid_modelos")),
      
      # Botão para trocar os modelos exibidos (inicialmente invisível)
      uiOutput(ns("btn_pagina"))
    )
  )
}

# Módulo Server
mod_dashboard_server <- function(id, estado_pagina, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Buscar a quantidade total de modelos
    models_count <- reactive({
      dbGetQuery(pool, "SELECT COUNT(*) as total FROM modelo;")$total[1]
    })
    
    # Buscar todos os modelos da base de dados
    modelos_df <- reactive({
      dbGetQuery(pool, "SELECT id, nome FROM modelo ORDER BY id;")
    })
    
    pagina_atual <- reactiveVal(1)
    elementos_por_pagina <- 6
    
    # Modelos atualmente visíveis
    modelos_atualizados <- reactive({
      if (nrow(modelos_df()) == 0) return(NULL)  # Se não houver modelos, retorna NULL
      inicio <- (pagina_atual() - 1) * elementos_por_pagina + 1
      fim <- min(inicio + elementos_por_pagina - 1, nrow(modelos_df()))
      modelos_df()[inicio:fim, ]  # Retorna os modelos da página atual
    })
    
    # Renderiza a grid de modelos dinamicamente
    output$grid_modelos <- renderUI({
      if (is.null(modelos_atualizados())) {
        return(h4("Sem Modelos", style = "text-align: center; margin-top: 20px;"))
      }
      
      div(
        lapply(seq(1, nrow(modelos_atualizados()), by = 3), function(i) {
          fluidRow(
            lapply(0:2, function(j) {
              index <- i + j
              if (index <= nrow(modelos_atualizados())) {
                modelo <- modelos_atualizados()[index, ]
                column(4, div(class = "grid-item",
                              h4(modelo$nome),
                              actionButton(ns(paste0("opcoes_", modelo$id)), "Opções", class = "btn-options"))
                )
              }
            })
          )
        })
      )
    })
    
    # Renderiza o botão "Próxima Página" apenas se houver modelos
    output$btn_pagina <- renderUI({
      if (models_count() > elementos_por_pagina) {
        actionButton(ns("proxima_pagina"), "Próxima Página", style = "margin-top: 20px;")
      } else {
        NULL  # Esconde o botão se não for necessário
      }
    })
    
    # Atualiza a página ao clicar no botão
    observeEvent(input$proxima_pagina, {
      if (pagina_atual() * elementos_por_pagina < models_count()) {
        pagina_atual(pagina_atual() + 1)
      } else {
        pagina_atual(1)
      }
    })
  })
}
