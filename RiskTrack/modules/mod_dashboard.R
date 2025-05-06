library(shiny)
library(bslib)
library(DBI)
library(RPostgres)

# Módulo UI
mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_columns(
      card(
        card_header(
          div(style = "text-align: center; font-weight: 600;", "Modelos")
        ),
        card_body(
          div(style = "text-align: center;",
              div(style = "font-size: 36px; font-weight: bold; color: #346cba; margin-bottom: 10px;",
                  textOutput(ns("model_count"))
              ),
              div(style = "font-size: 16px; color: #7f8c8d;",
                  "Total de Modelos"
              )
          )
        ),
        height = 160,
        style = "border: 1px solid #e0e6ed; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.05); background-color: white; margin: 0 10px;"
      ),
      
      card(
        card_header(
          div(style = "text-align: center; font-weight: 600;", "Predições")
        ),
        card_body(
          div(style = "text-align: center;",
              div(style = "font-size: 36px; font-weight: bold; color: #346cba; margin-bottom: 10px;",
                  textOutput(ns("prediction_count"))
              ),
              div(style = "font-size: 16px; color: #7f8c8d;",
                  "Total de Predições"
              )
          )
        ),
        height = 160,
        style = "border: 1px solid #e0e6ed; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.05); background-color: white; margin: 0 10px;"
      ),
      col_widths = c(6, 6),
      gap = "10px"  # Reduce the gap between columns
    )
  )
}




# Módulo Server
mod_dashboard_server <- function(id, pool, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    user_id <- reactive({
      # Access the user_id reactive value
      req(user())
      return(user())
    })
    
    
    # Buscar a quantidade total de modelos e predições em uma única query eficiente
    counts_data <- reactive({
      req(user_id())
      tryCatch({
        query <- "
      SELECT 
        COUNT(DISTINCT m.id) AS model_count,
        COUNT(DISTINCT p.id) AS prediction_count
      FROM modelo m
      LEFT JOIN previsao p ON p.modelo_id = m.id
      WHERE m.utilizador_id = $1
    "
        result <- dbGetQuery(pool, query, params = list(user_id()))
        
        return(list(
          models = result$model_count[1],
          predictions = result$prediction_count[1]
        ))
      }, error = function(e) {
        message("Erro ao buscar quantidade de modelos e predições: ", e$message)
        return(list(models = "Erro", predictions = "Erro"))
      })
    })
    
    # Render outputs para os cards de contagem
    output$model_count <- renderText({
      as.integer(counts_data()$models)
    })
    
    output$prediction_count <- renderText({
      as.integer(counts_data()$predictions)
    })
    
  })
}