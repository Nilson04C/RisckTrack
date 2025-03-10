library(shiny)
library(dplyr)

# UI do aplicativo
ui <- fluidPage(
  titlePanel("Análise de Dataset CSV"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Escolha o arquivo CSV", accept = c(".csv")),
      selectInput("coluna", "Escolha a coluna para análise", choices = NULL),
      actionButton("analisar", "Analisar Dados")
    ),
    
    mainPanel(
      h3("Resultados da Análise"),
      tableOutput("resultados")
    )
  )
)

# Server do aplicativo
server <- function(input, output, session) {
  
  # Lê o arquivo CSV quando o mesmo é carregado
  dados <- reactive({
    req(input$file1)
    df <- read.csv2(input$file1$datapath, stringsAsFactors = FALSE)
    colnames(df) <- make.names(colnames(df))  # Certificar que as colunas têm nomes válidos
    updateSelectInput(session, "coluna", choices = colnames(df))
    return(df)
  })
  
  # Função de análise quando o botão é pressionado
  observeEvent(input$analisar, {
    req(dados())
    df <- dados()
    
    coluna_escolhida <- df[[input$coluna]]
    
    # Calcular as quantidades para cada condição
    resultados <- data.frame(
      Maior_que_100 = sum(as.numeric(coluna_escolhida) > 100, na.rm = TRUE),
      Maior_que_168 = sum(as.numeric(coluna_escolhida) > 168, na.rm = TRUE),
      Menor_que_0 = sum(as.numeric(coluna_escolhida) < 0, na.rm = TRUE),
      NAN = sum(tolower(coluna_escolhida) == "nan", na.rm = TRUE),
      Vazio = sum(coluna_escolhida == "", na.rm = TRUE)
    )
    
    # Exibir os resultados
    output$resultados <- renderTable({
      resultados
    })
  })
}

# Rodar o aplicativo
shinyApp(ui = ui, server = server)
