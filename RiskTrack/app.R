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
  
  # Input oculto para rastrear o redimensionamento
  hidden(
    numericInput("window_width", "Window Width", value = 0)
  ),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  useShinyjs(),  # Ativar shinyjs para manipular visibilidade
  
  # Menu superior (escondido por padrão)
  div(id = "menu_superior", 
      style = "display: none; background-color: black; top: 0; width: 100%; z-index: 1000;",
      class = "navbar",
      
          # Container para o conteúdo da navbar
          tags$a(href = "#", style = "color: white; ", class ="navbar-brand", "RiskTrack"),  # Título do app
          
      div(style = "display: inline-block; position: relative;",
          
          # Botão "new"
          actionButton("add_btn", "New", 
                       style = "color: white; 
                                background-color: black; 
                                border-color: black;"),
          
          #DropDown List
          div(id = "dropdown_new", 
              style = "display: none; position: absolute; top: 100%; left: 0; background-color: #f9f9f9; min-width: 160px; box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2); z-index: 1000;",
              actionLink("new_dataset", "Dataset", style = "color: black; padding: 12px 16px; text-decoration: none; display: block;"),
              actionLink("new_modelo", "Modelo", style = "color: black; padding: 12px 16px; text-decoration: none; display: block;"),
              actionLink("new_previsoes", "Previsões", style = "color: black; padding: 12px 16px; text-decoration: none; display: block;")
          )
      ),
          
          #logout
          actionLink("btn_logout", "Logout", style=" color:white; float: right; ")
  ),
  
            
  # Layout com menu lateral (inicialmente escondido) e conteúdo principal
  sidebarLayout(
    sidebarPanel(
      id = "menu_lateral",
      class = "sidebar-custom",
      
      style = "display: none;",  # Esconde o menu lateral inicialmente
      fluidRow( class= "menu-item", actionButton("btn_models", "Modelos")),
      fluidRow( class= "menu-item", actionButton("btn_dashboard", "Dashboard"))
      
    ),
    
    mainPanel(
      uiOutput("pagina")  # Conteúdo dinâmico
    )
  ),
)

server <- function(input, output, session) {
  
  
  estado_login <- reactiveVal(FALSE)  # Estado de login (TRUE = logado, FALSE = não logado)
  estado_pagina <- reactiveVal("login")  # Começa na página de login
  
  # Quando o estado do login muda, mostrar ou esconder o menu lateral
  observe({
    if (estado_login()) {
      shinyjs::show("menu_superior")
      shinyjs::show("menu_lateral")
      shinyjs::show("add_btn")
      runjs("
        setTimeout(function() {
          var sidebarWidth = document.getElementById('menu_lateral').offsetWidth;
          var newButton = document.getElementById('add_btn');
          newButton.style.position = 'absolute';
          newButton.style.left = (sidebarWidth - newButton.offsetWidth) + 'px';  // Alinha ao final do sidebar
        }, 100);
      ")
      
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
  
  
  # Mostrar/esconder o dropdown ao clicar no botão "New"
  observeEvent(input$add_btn, {
    shinyjs::toggle("dropdown_new")
  })
  
  # Reagir às opções do dropdown
  observeEvent(input$new_dataset, {
    shinyjs::hide("dropdown_new")
  })
  observeEvent(input$new_modelo, {
    shinyjs::hide("dropdown_new")
  })
  observeEvent(input$new_previsoes, {
    shinyjs::hide("dropdown_new")
  })
  
  
  observeEvent(input$btn_logout, {
    poolClose(pool)  # Fecha a conexão com segurança
    estado_login(FALSE)  # Faz logout
    estado_pagina("login")  # Volta para a página de login
  })
  
  
  
  
  
  # Adicionar listener de redimensionamento ao carregar a página
  runjs("
    window.addEventListener('resize', function() {
      var width = window.innerWidth;
      Shiny.setInputValue('window_width', width);
    });
    // Disparar o evento inicial para definir o valor inicial
    Shiny.setInputValue('window_width', window.innerWidth);
  ")
  
  # Observar mudanças no tamanho da janela e ajustar o botão "New"
  observe({
    # Dependência reativa: input$window_width
    req(input$window_width)  # Garante que o valor esteja definido
    
    runjs("
      var sidebarWidth = document.getElementById('menu_lateral').offsetWidth;
      var newButton = document.getElementById('add_btn');
      newButton.style.position = 'absolute';
      newButton.style.left = (sidebarWidth - newButton.offsetWidth) + 'px';  // Alinha ao final do sidebar
    ")
  })
  
  
  mod_login_server("login", estado_login)
  mod_models_server("models", estado_pagina, pool)
  mod_dashboard_server("dashboard", estado_pagina)
}

shinyApp(ui, server)
