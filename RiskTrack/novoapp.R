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

onStop(function() {
  poolClose(pool)
})


# Carregar os módulos
source("modules/mod_login.R")
source("modules/mod_models.R")
source("modules/mod_dashboard.R")
source("modules/mod_prediction.R")
source("functions/func_models.R")
source("functions/func_previsao.R")


ui <- fluidPage(
  
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=1")
  ),
  
  useShinyjs(),  # Ativar shinyjs para manipular visibilidade
  
  # Menu superior (escondido por padrão)
  div(id = "menu_superior", 
      class = "menu",
      
      
      #Titulo do APP
      tags$a(href = "#", style = "color: white;", class ="navbar-brand", "RiskTrack"),  # Título do app
      
      
      #Conteiner para o botão NEW e o dropdown list
      div(class = "dropdown-container",
          # Botão "new" com ícone
          actionLink("add_btn", 
                     label = span(style = "display: inline-flex; align-items: center;", 
                                  "New ", 
                                  icon("plus", style = "margin-left: 4px;"))),
          
          # DropDown List com ícones
          div(id = "dropdown_new", class = "dropdown-list",
              actionLink("new_modelo", label = span(icon("chart-line"), "Modelo"), class = "dropdown-option"),
              actionLink("new_previsao", label = span(icon("chart-bar"), "Previsão"), class = "dropdown-option")
          )
      ),
      
      # Botão de logout (alinhado à direita)
      div(id = "div_logout",style = "margin-left: auto; display: none",
          actionLink("btn_logout", 
                     label = span(style = "display: none; display: inline-flex; align-items: center; color: white;", 
                                  "Logout ", 
                                  icon("sign-out-alt", style = "margin-left: 4px;")))
      )
      
  ),
  
  
  # Layout com menu lateral (inicialmente escondido) e conteúdo principal
  sidebarLayout(
    sidebarPanel(
      id = "menu_lateral",
      class = "sidebar-custom",
      
      style = "display: none;",  # Esconde o menu lateral inicialmente
      fluidRow(class = "menu-item", 
               actionButton("btn_dashboard", 
                            label = span(style = "display: flex; align-items: center;", 
                                         icon("list-alt", style = "margin-right: 8px;"), "Dashboard"))),
      fluidRow(class = "menu-item", 
               actionButton("btn_models", 
                            label = span(style = "display: flex; align-items: center;", 
                                         icon("chart-line", style = "margin-right: 8px;"), "Modelos"))),
      fluidRow(class = "menu-item", 
               actionButton("btn_prediction", 
                            label = span(style = "display: flex; align-items: center;", 
                                         icon("chart-bar", style = "margin-right: 8px;"), "Previsões")))
      
    ),
    
    mainPanel(
      uiOutput("pagina")  # Conteúdo dinâmico
    )
  ),
  
  
  # formulário que aparaece ao clicar na opção do Modelo <- new
  
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
               sliderInput("sliderTrainPr", "Percentagem de Treino", min = 50, max = 80, value = 60),
               numericInput("cpInput","(parâmetro de complexidade (CP)", value = 0.001, step = 0.001),
               numericInput("maxDInput", "profundidade máxima (Max Depth)", value = 5, max = 30),
               checkboxInput("crossV_box","Tuning"),
               actionButton("submit_dataset", "Enviar"),
               actionButton("save_model","salvar Modelo",disabled = TRUE),
        ),
        column(6, 
               plotOutput("plot_confusao"),
               div( #o div é para a tabela ser scrolable
                 style = "overflow-x: auto; max-width: 100%;",
                 tableOutput("table_avaliacao")
               )
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
  
  valores <- reactiveValues(modelo = NULL, avaliacao = NULL, valor_slider = NULL, crossV = NULL, cp = NULL, maxDepth = NULL) #valores globais
  
  user_id <- reactiveVal(NULL)
  
  MdlReadyToCreate <- reactiveVal(FALSE)
  
  modal_page <- reactiveVal("menu")  # Começa no menu principal
  
  models_module <- NULL
  dashboard_module <- NULL
  prediction_module <- NULL
  
  
  # se o botao para ver o modal de previsão for clicado
  observeEvent(input$new_previsao, {
    
    modelos <- getmodels_list(pool, user_id)
    
    # Atualizar a dropdownList das previsões com nome e o id dos modelos
    updateSelectInput(session, "select_model_pred", choices = modelos)
    

  })
  
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
                    pageLength = 10, # Número de linhas visíveis por página
                    dom = 'Bfrtip', # Ativa botões
                    buttons = c('csv', 'excel', 'pdf') # Exportação
                  ))
      })
      
      #SALVAR A PREVISÃO
      savepred(nome, previsao, pool, modelo_id)
      
    }
    
  })
  
  #botão de criar modelo é criado
  observeEvent(input$submit_dataset,{
    
    
    #obter valor de cp e maxdepth
    valores$cp <- input$cpInput
    valores$maxDepth <- input$maxDInput
    
    # Obter o valor do slider
    valores$valor_slider <- input$sliderTrainPr
    print(paste("Valor do slider:", valores$valor_slider))
    
    valores$crossV <- input$crossV_box
    #print(crossV)
    
    
    
    
    
    # Verificar se o dataset foi carregado
    if (!is.null(input$file_upload)) {
      
      # Carregar o dataset
      dataset <- read.csv(input$file_upload$datapath, sep = ";")
      print("Dataset carregado:")
      
      dataset <<- subsPorNA(dataset)   #passar todos os dados que Null, " ", entre outros em NA
      
      
      # Alerta sobre os missing Values se houver NAs
      if (any(is.na(dataset))) {
      
        # Modal com conteúdo condicional
        showModal(modalDialog(
          title = "Dados Incompletos Detectados",
          uiOutput("modal_conteudo_dinamico"),
          easyClose = FALSE,
          footer = NULL
        ))
        
      } else{MdlReadyToCreate(TRUE)}   #se não houver dados em falta, o modelo pode ser treinado
      
    } else {
      print("Nenhum dataset foi carregado.")
    }
    
  })
  
  
  
  # criar Modelo
  observeEvent(MdlReadyToCreate(),{
    #print(paste("MdlReadyToCreate:", MdlReadyToCreate()))
    
    # se as as condições para criar o modelo foram cumpridas (caso seja encontrado dados vazios do dataset, o dataset deve ser tratado)
    if(MdlReadyToCreate())
    {
      
    toggle("plot_confusao")
    toggle("table_avaliacao")
      
    
    #criar o modelo e avaliar
    withProgress(message = "A gerar modelo...", value = 0, { #barra de progresso
    resultado <- criarModelo(dataset, valores$valor_slider/100, valores$crossV, valores$cp, valores$maxDepth, progress = incProgress, session = session)
    })
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
      metrics <- cbind(Métrica = rownames(metrics), metrics)
      rownames(metrics) <- NULL
      metrics
    })
    
    toggle("plot_confusao")
    toggle("table_avaliacao")
    
    shinyjs::enable("save_model")
    MdlReadyToCreate(FALSE)  # Volta a ser FALSE
    }
  })

  
  
  
  
  # Conteúdo que muda dinamicamente conforme o estado
  output$modal_conteudo_dinamico <- renderUI({
    if (modal_page() == "menu") {
      tagList(
        div("Foram encontrados valores em falta no dataset. O que deseja fazer?"),
        br(),
        fluidRow(
          column(4, actionButton("btn_apagar_missing", "Apagar Dados", class = "btn-danger")),
          column(4, actionButton("btn_substituir_missing", "Substituir por Média/Moda", class = "btn-warning")),
          column(4, actionButton("btn_ver_detalhes_missing", "Ver Detalhes", class = "btn-primary"))
        )
      )
    } else if (modal_page() == "detalhes") {
      tagList(
        h5("Detalhes dos Valores em Falta"),
        tableOutput("tabela_missing"),
        br(),
        actionButton("btn_voltar_menu", "Voltar", class = "btn-secondary")
      )
    }
  })
  
  
  observeEvent(input$btn_apagar_missing, {
    dataset <<- na.omit(dataset)
    removeModal()
    MdlReadyToCreate(TRUE)  # Define como TRUE
    
  })
  
  observeEvent(input$btn_substituir_missing, {
    dataset <<- tratardados(dataset)
    removeModal()
    MdlReadyToCreate(TRUE)  # Define como TRUE
  })
  
  observeEvent(input$btn_ver_detalhes_missing, {
    # Tabela de missing values
    output$tabela_missing <- renderTable({
      verificarMissingValues(dataset)
    })
    modal_page("detalhes")
  })
  
  observeEvent(input$btn_voltar_menu, {
    modal_page("menu")
  })
  
  
  
  
  #Guardar Modelo
  observeEvent(input$save_model,{
    
    name = input$file_name
    if(nzchar(name)){ # se name tiver caracteres
      saveRDS(valores$modelo, paste0(name, ".rds"))
      saveRDS(valores$avaliacao, paste0(name, "_av.rds"))
      print("Modelo Salvo")
      
      
      num_variaveis <- length(attr(valores$modelo$terms, "term.labels"))
      caminho <- paste0(getwd(),"/",name)
      avaliacao_caminho <- paste0(getwd(),"/",name,"_av")
      
      query = paste0("INSERT INTO modelo (nome, algoritmo, data_criacao, utilizador_id, caminho, avaliacao_caminho, n_variaveis) 
                      VALUES ('",name,"', 'Árvore de Decisao', NOW(), ", user_id(), ", '",caminho,".rds','",avaliacao_caminho,".rds', ",num_variaveis,");")
      
      dbExecute(pool, query)

    }
  })
  
  
  logado <- reactiveVal(FALSE)  # Estado de login (TRUE = logado, FALSE = não logado)
  pagina_atual <- reactiveVal("login")  # Começa na página de login
  
  # Quando o estado do login muda, mostrar ou esconder o menu lateral
  shiny::observe({
    if (logado()) {
      shinyjs::show("menu_superior")
      shinyjs::show("menu_lateral")
      shinyjs::show("add_btn")
      shinyjs::show("btn_logout")
      shinyjs::show("div_logout")
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
    if (!logado()) {
      # Se não estiver logado, exibe a página de login
      mod_login_ui("login")
      
    } else {
      # Se estiver logado, mostra o conteúdo da página (Modelos ou Dashboard)
      switch(
        pagina_atual(),
        models = mod_models_ui("models"),
        dashboard = mod_dashboard_ui("dashboard"),
        prediction = mod_prediction_ui("prediction"),
        # Adicionar outros casos aqui se necessário
        mod_dashboard_ui("dashboard")  # Valor padrão, caso nenhuma condição seja atendida
      )
    }
  })
  
  
  # Observa cliques nos botões do menu superior
  observeEvent(input$btn_models, {
    # Verifica se o user_id existe
    req(user_id())
    pagina_atual("models")
    
    # Inicializa o módulo apenas uma vez
    if (is.null(models_module)) {
      models_module <<- mod_models_server("models", pool, user_id)
    } else {
      # Se já existe, apenas atualiza os dados
      models_module$atualizar_tabela()
    }
  })
  
  
  observeEvent(input$btn_dashboard, { 
    pagina_atual("dashboard") 
    
      dashboard_module$atualizar_dashboard()
    })
  
  
  observeEvent(input$btn_prediction, { 
    pagina_atual("prediction")
    
    # Inicializa o módulo apenas uma vez
    if (is.null(prediction_module)) {
      prediction_module <<- mod_prediction_server("prediction", estado_pagina, pool, user_id)
    } else {
      # Se já existe, apenas atualiza os dados
      prediction_module$atualizar_tabela()
    }
    })
  
  
  # Mostrar/esconder o dropdown ao clicar no botão "New"
  observeEvent(input$add_btn, {
    shinyjs::toggle("dropdown_new")
    
    # Make sure dropdown appears below the button
    runjs("
    var newButtonRect = document.getElementById('add_btn').getBoundingClientRect();
    var dropdown = document.getElementById('dropdown_new');
    dropdown.style.top = newButtonRect.height + 'px';
    dropdown.style.left = '125px';
  ")
  })
  
  observeEvent(input$new_modelo, {
    shinyjs::hide("dropdown_new")
  })
  observeEvent(input$new_previsao, {
    shinyjs::hide("dropdown_new")
  })
  
  
  observeEvent(input$btn_logout, {
    user_id(NULL)
    logado(FALSE)  # Faz logout
    pagina_atual("login")  # Volta para a página de login
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
  
  

  
  
  mod_login_server("login", logado, pool, user_id)
 
  dashboard_module <- mod_dashboard_server("dashboard", pool, user_id)
  
}

shinyApp(ui, server)
