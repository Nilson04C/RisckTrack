library(shiny)
library(DBI)
library(RPostgres)
library(pool)
library(shinyjs)
library(bslib)
library(shinyBS)
#heatmap para amatriz de confusão
library(pheatmap)
library(DT)


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
source("functions/func_models.R")
source("functions/func_previsao.R")

ui <- fluidPage(
  
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=2")
  ),
  
  useShinyjs(),  # Ativar shinyjs para manipular visibilidade
  
  # Menu superior (escondido por padrão)
  div(id = "menu_superior", 
      class = "menu",
      
      
      #Titulo do APP
      tags$a(href = "#", style = "color: white;", class ="navbar-brand", "RiskTrack"),  # Título do app
      
      #Conteiner para o botão NEW
      div(style = "position: relative; masgin-left: 100px",
          
          # Botão "new"
          actionLink("add_btn", "New", 
                       style = "color: white;"),
          
          #DropDown List
          div(id = "dropdown_new", class ="dropdown-list",
              actionLink("new_modelo", "Modelo", class = "dropdown-option"),
              actionLink("new_previsao", "Previsão", class = "dropdown-option")
          )
      ),
      
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
  
  
  # formulário que aparaece ao clicar numa das opções do new
  uiOutput("Formulario_modelo"),
  
  bsModal(
    id = "modal_modelo",
    title = "Criar Modelo",
    trigger = "new_modelo",
    size = "large",
    body = tagList(
      fluidRow(
        column(6, 
               textInput("file_name", "Nome do arquivo:"),
               fileInput("file_upload", "Escolha o arquivo", accept = ".csv"),
               sliderInput("slider", "Slider", min = 30, max = 80, value = 60),
               actionButton("submit_dataset", "Enviar"),
               actionButton("save_model","salavr Modelo",disabled = TRUE),
        ),
        column(6, 
               plotOutput("plot_confusao"),
               tableOutput("table_avaliacao")
        )
      )
    )
  ),
  
  bsModal(
    id = "modal_previsao",
    title = "Fazer Previsão",
    trigger = "new_previsao",
    size = "large",
    body = tagList(
      fluidRow(
        column(5, 
               textInput("file_name_pred", "Nome do arquivo:"),
               fileInput("file_upload_pred", "Escolha o arquivo", accept = ".csv"),
               selectInput("select_model_pred", "escolha um modelo", choices = NULL ),
               actionButton("submit_dataset_pred", "Fazer Previsão"),
        ),
        column(7, 
               #plotOutput("fail_rate"),
               DTOutput("pred_table")
        )
      )
    )
  )
)




















server <- function(input, output, session) {
  
  valores <- reactiveValues(modelo = NULL, avaliacao = NULL) #valores globais
  
  
  
  # se o botao para ver o modal de previsão for clicado
  observeEvent(input$new_previsao, {
    
    modelos <- getmodels_list(pool)
    
    # Atualizar a dropdownList das previsões com nome e o id dos modelos
    updateSelectInput(session, "select_model_pred", choices = modelos)
    
    # se o botão de fazer uma previsão for clicado
    observeEvent(input$submit_dataset_pred, {
      
      nome <- input$file_name_pred
      
      
      if (!is.null(input$file_upload_pred) && !is.null(input$select_model_pred) && nzchar(nome)) {
        
        dataset <- read.csv(input$file_upload_pred$datapath, sep = ";")
        print("Dataset carregado:")
        
        modelo_id <- input$select_model_pred
        
        caminho <- getmodel_path(pool, modelo_id)
        
        modelo <- readRDS(caminho)
        
        previsao <- fazer_previsao( modelo, dataset )
        
        
        #output$fail_rate <- renderPlot()
        
        # tabela que exibe os dados da previsão
        output$pred_table <- renderDT({
          datatable(previsao, 
                    extensions = "Buttons", 
                    options = list(
                      scrollX = TRUE,
                      pageLength = 15, # Número de linhas visíveis por página
                      dom = 'Bfrtip', # Ativa botões
                      buttons = c('csv', 'excel', 'pdf') # Exportação
                    ))
           })
        savepred(nome, previsao, pool, modelo_id)
      }
      
    })
  })
  
  observeEvent(input$submit_dataset,{
    
    # Obter o valor do slider
    valor_slider <- input$slider
    print(paste("Valor do slider:", valor_slider))

    # Verificar se o dataset foi carregado
    if (!is.null(input$file_upload)) {
      
      # Carregar o dataset
      dataset <- read.csv(input$file_upload$datapath, sep = ";")
      print("Dataset carregado:")
      
      #criar o modelo e avaliar
      resultado <- criarModelo(dataset, valor_slider/100)
      avaliacao_modelo = resultado$avaliacao
      modelo_treinado = resultado$modelo
      
      #guardar os dados em variaveis globais
      valores$modelo <- modelo_treinado
      valores$avaliacao <- avaliacao_modelo
      
      # Criar matriz numérica
      matriz <- as.matrix(avaliacao_modelo$table)
      
      output$plot_confusao <- renderPlot({
        req(avaliacao_modelo)
        print("Renderizando o heatmap...")  # Verifica se o código está a ser executado
        pheatmap(matriz, 
                 cluster_rows = FALSE,  # Sem agrupamento nas linhas
                 cluster_cols = FALSE,  # Sem agrupamento nas colunas
                 show_colnames = TRUE,
                 show_rownames = TRUE,
                 display_numbers = TRUE,  # Mostrar valores dentro das células
                 color = colorRampPalette(c("white", "red"))(100))  # Gradiente de cor
      })
      output$table_avaliacao <- renderTable({
        metrics <- as.data.frame(avaliacao_modelo$byClass)
        metrics <- cbind(Metricas = rownames(metrics), metrics)
        metrics
      })
      shinyjs::enable("save_model")
      
    } else {
      print("Nenhum dataset foi carregado.")
    }
  })
  
  #Guardar Modelo
  observeEvent(input$save_model,{
    
    name = input$file_name
    if(nzchar(name)){ # se name tiver caracteres
      saveRDS(valores$modelo, paste0(name, ".rds"))
      print("Modelo Salvo")
      
      utilizador_id <- 2 #a ser substituido por valor verdadeiro
      
      query = paste0("INSERT INTO modelo (nome, algoritmo, data_criacao, utilizador_id, caminho) 
      VALUES ('",name,"', 'Árvore de Decisao', NOW(), ", utilizador_id, ", '",getwd(),"/",name,".rds');")
      
      dbExecute(pool, query)
      
    }
  })
  
  
  estado_login <- reactiveVal(FALSE)  # Estado de login (TRUE = logado, FALSE = não logado)
  estado_pagina <- reactiveVal("login")  # Começa na página de login
  
  # Quando o estado do login muda, mostrar ou esconder o menu lateral
  shiny::observe({
    if (estado_login()) {
      shinyjs::show("menu_superior")
      shinyjs::show("menu_lateral")
      shinyjs::show("add_btn")
      shinyjs::show("btn_logout")
      runjs("
        setTimeout(function() {
          var sidebarWidth = document.getElementById('menu_lateral').offsetWidth;
          var newButton = document.getElementById('add_btn');
          newButton.style.position = 'absolute';
          newButton.style.left = (sidebarWidth - newButton.offsetWidth-120) + 'px';  // Alinha ao final do sidebar
        }, 100);
      ")
      
    } else {
      shinyjs::hide("add_btn")  # Esconde o menu superior
      shinyjs::hide("menu_lateral")   # Esconde o menu lateral
      shinyjs::hide("btn_logout")  # Esconde o menu superior
      
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
  shiny::observe({
    # Dependência reativa: input$window_width
    req(input$window_width)  # Garante que o valor esteja definido
    
    runjs("
      var sidebarWidth = document.getElementById('menu_lateral').offsetWidth;
      var newButton = document.getElementById('add_btn');
      newButton.style.position = 'absolute';
      newButton.style.left = (sidebarWidth - newButton.offsetWidth-120) + 'px';  // Alinha ao final do sidebar
    ")
  })
  
  

  
  
  mod_login_server("login", estado_login)
  mod_models_server("models", estado_pagina, pool)
  mod_dashboard_server("dashboard", estado_pagina)
}

shinyApp(ui, server)
