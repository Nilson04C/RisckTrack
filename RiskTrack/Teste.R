library(shiny)
library(DT)  # Para tabelas interativas
library(ggplot2)  # Para gráficos

# UI - Interface do usuário
ui <- fluidPage(
  titlePanel("Exploração de Dados - RiskTrack"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Carregar Dataset CSV", accept = ".csv"),
      
      uiOutput("column_select"),  # Seleção de colunas dinâmica
      
      numericInput("min_value", "Valor mínimo:", value = NA, step = 100),
      numericInput("max_value", "Valor máximo:", value = NA, step = 100),
      
      checkboxInput("remove_na", "Remover valores em falta (NA)", value = TRUE),
      
      actionButton("apply_filter", "Aplicar Filtros"),
      
      hr(),
      
      h4("Correlação entre duas colunas"),
      uiOutput("corr_column1"),
      uiOutput("corr_column2"),
      verbatimTextOutput("correlation_result"),  # Exibir a correlação
      plotOutput("scatter_plot")  # Gráfico de dispersão
    ),
    
    mainPanel(
      DTOutput("table")  # Tabela interativa
    )
  )
)

# Server - Lógica do app
server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    data <- read.csv(input$file$datapath, sep = ";", header = TRUE, stringsAsFactors = FALSE)
    
    # Substituir "nan", "NULL", "", " " por NA em todo o dataset
    data[data == "nan"] <- NA  
    data[data %in% c("NULL", "")] <- NA
    
    dataset(data)
  })
  
  output$column_select <- renderUI({
    req(dataset())
    selectInput("selected_column", "Selecionar Coluna:", 
                choices = names(dataset()), selected = names(dataset())[1])
  })
  
  filtered_data <- eventReactive(input$apply_filter, {
    req(dataset(), input$selected_column)
    data <- dataset()
    
    if (input$remove_na) {
      data <- na.omit(data)
    }
    
    if (!is.na(input$min_value)) {
      data <- data[data[[input$selected_column]] >= input$min_value, ]
    }
    
    if (!is.na(input$max_value)) {
      data <- data[data[[input$selected_column]] <= input$max_value, ]
    }
    
    return(data)
  })
  
  output$table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  # UI para selecionar as colunas da correlação
  output$corr_column1 <- renderUI({
    req(dataset())
    selectInput("selected_corr1", "Coluna 1:", choices = names(dataset()), selected = names(dataset())[1])
  })
  
  output$corr_column2 <- renderUI({
    req(dataset())
    selectInput("selected_corr2", "Coluna 2:", choices = names(dataset()), selected = names(dataset())[2])
  })
  
  # Calcular correlação
  output$correlation_result <- renderPrint({
    req(dataset(), input$selected_corr1, input$selected_corr2)
    
    data <- dataset()
    
    # Verificar se as colunas são numéricas
    if (!is.numeric(data[[input$selected_corr1]]) || !is.numeric(data[[input$selected_corr2]])) {
      return("Selecione colunas numéricas para calcular a correlação.")
    }
    
    cor_value <- cor(data[[input$selected_corr1]], data[[input$selected_corr2]], use = "complete.obs")
    
    paste("Correlação entre", input$selected_corr1, "e", input$selected_corr2, ":", round(cor_value, 4))
  })
  
  # Gerar gráfico de dispersão
  output$scatter_plot <- renderPlot({
    req(dataset(), input$selected_corr1, input$selected_corr2)
    
    data <- dataset()
    
    # Verificar se as colunas são numéricas
    if (!is.numeric(data[[input$selected_corr1]]) || !is.numeric(data[[input$selected_corr2]])) {
      return(NULL)
    }
    
    ggplot(data, aes_string(x = input$selected_corr1, y = input$selected_corr2)) +
      geom_point(color = "blue") +
      theme_minimal() +
      labs(title = paste("Dispersão entre", input$selected_corr1, "e", input$selected_corr2),
           x = input$selected_corr1,
           y = input$selected_corr2)
  })
}

# Executar o app
shinyApp(ui, server)
