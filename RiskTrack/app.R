library(shiny)
library(DBI)
library(RPostgres)
library(pool)
library(shinyjs)


# Criar um pool de conexão para gerenciar conexões ao banco de dados
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "RiskTrackDB",
  host = "localhost",  # Ou IP do servidor
  port = 5432,         # Porta padrão do PostgreSQL
  user = "postgres",
  password = "123456"
)

# Carregar os módulos
source("modules/mod_login.R")
source("modules/mod_models.R")
source("modules/mod_dashboard.R")

ui <- fluidPage(

  # Menu superior (inicialmente escondido)
  useShinyjs(),  # Ativar shinyjs para manipular visibilidade
  
  # Menu superior (escondido por padrão)
  div(id = "menu_superior", 
      style = "display: none;",  # Esconde o menu logo no carregamento
      class = "navbar navbar-expand-lg navbar-light bg-light",
      div(class = "container-fluid",
          tags$a(class = "navbar-brand", "RiskTrack"),  # Título do app
          div(class = "collapse navbar-collapse",
              tags$ul(class = "navbar-nav navbar-right",
                      tags$li(class = "nav-item", actionLink("btn_logout", "Logout", class = "nav-link"))
              )
          )
      )
  ),
  

  # Layout com menu lateral (inicialmente escondido) e conteúdo principal
  sidebarLayout(
    sidebarPanel(
      id = "menu_lateral",# Menu lateral
      class = "sidebar-custom",
      style = "display: none;",  # Esconde o menu lateral inicialmente
      actionButton("btn_models", "Modelos"),
      actionButton("btn_dashboard", "Dashboard")
    ),
    
    mainPanel(
      uiOutput("pagina")  # Conteúdo dinâmico
    )
  ),
  

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  )
)

server <- function(input, output, session) {
  
  
  estado_login <- reactiveVal(FALSE)  # Estado de login (TRUE = logado, FALSE = não logado)
  estado_pagina <- reactiveVal("login")  # Começa na página de login
  
  # Quando o estado do login muda, mostrar ou esconder o menu lateral
  observe({
    if (estado_login()) {
      shinyjs::show("menu_superior")  # Mostra o menu superior
      shinyjs::show("menu_lateral")   # Mostra o menu lateral
    } else {
      shinyjs::hide("menu_superior")  # Esconde o menu superior
      shinyjs::hide("menu_lateral")   # Esconde o menu lateral
    }
  })
  
  output$pagina <- renderUI({
    if (!estado_login()) {
      # Se não estiver logado, exibe a página de login
      mod_login_ui("login")
    } else {
      # Se estiver logado, mostra o conteúdo da página (Modelos ou Dashboard)
      switch(
        estado_pagina(),
        models = mod_models_ui("models"),
        dashboard = mod_dashboard_ui("dashboard"),
        # Adicionar outros casos aqui se necessário
        mod_dashboard_ui("dashboard")  # Valor padrão, caso nenhuma condição seja atendida
      )
    }
  })
  
  
  # Observa cliques nos botões do menu superior
  observeEvent(input$btn_models, { estado_pagina("models") })
  observeEvent(input$btn_dashboard, { estado_pagina("dashboard") })
  observeEvent(input$btn_logout, {
    poolClose(pool)  # Fecha a conexão com segurança
    estado_login(FALSE)  # Faz logout
    estado_pagina("login")  # Volta para a página de login
  })
  
  
  mod_login_server("login", estado_login)
  mod_models_server("models", estado_pagina, pool)
  mod_dashboard_server("dashboard", estado_pagina)
}

shinyApp(ui, server)
