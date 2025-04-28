library(shiny)
library(RPostgres)
library(pool)
library(sodium)

# UI
mod_login_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "display: flex; justify-content: center; margin-top: 2rem;",
      card(
        max_width = 400,
        card_header(
          h3("RiskTrack", class = "text-center"),
          p("Sign in to your account", class = "text-muted text-center")
        ),
        card_body(
          textInput(ns("email"), "Email:", placeholder = "Enter your email"),
          passwordInput(ns("senha"), "Password:", placeholder = "Enter your password"),
          div(
            style = "margin-top: 20px;",
            actionButton(ns("entrar"), "Sign In", 
                         class = "btn-primary w-100 mb-2"),
            actionButton(ns("criar_conta"), "Create Account", 
                         class = "btn-outline-secondary w-100")
          ),
          uiOutput(ns("mensagem"))
        ),
        card_footer(
          div(id = ns("mensagem"), class = "text-center")
        )
      )
    )
  )
}

# SERVER
mod_login_server <- function(id, estado_login, pool, user_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$entrar, {
      req(input$email, input$senha)
      
      con <- poolCheckout(pool)
      on.exit(poolReturn(con))
      
      query <- dbSendQuery(con, "SELECT id, senha FROM utilizador WHERE email = $1")
      dbBind(query, list(input$email))
      resultado <- dbFetch(query)
      dbClearResult(query)
      
      if (nrow(resultado) == 1) {
        senha_hash <- resultado$senha[1]
        
        if (password_verify(senha_hash, input$senha)) {
          estado_login(TRUE)
          user_id(resultado$id[1])
        } else {
          output$mensagem <- renderText("Senha incorreta!")
        }
      } else {
        output$mensagem <- renderText("Email não encontrado!")
      }
    })
    
    observeEvent(input$criar_conta, {
      showModal(modalDialog(
        title = NULL,
        card(
          card_header(
            h3("Criar Nova Conta", class = "text-center"),
            p("Preencha os dados para criar sua conta", class = "text-muted text-center")
          ),
          card_body(
            textInput(ns("novo_nome"), "Nome:", placeholder = "Digite seu nome completo"),
            textInput(ns("novo_email"), "Email Institucional:", placeholder = "nome@instituicao.com"),
            passwordInput(ns("nova_senha"), "Senha:", placeholder = "Crie uma senha forte"),
            passwordInput(ns("confirmar_senha"), "Confirmar Senha:", placeholder = "Digite a senha novamente"),
            
            # Adding UI element to display messages
            uiOutput(ns("mensagem_criacao"))
          ),
          card_footer(
            div(
              class = "d-flex justify-content-between",
              actionButton(ns("cancelar_criacao"), "Cancelar", class = "btn-outline-secondary"),
              actionButton(ns("confirmar_criacao"), "Criar Conta", class = "btn-primary")
            )
          )
        ),
        size = "m",
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$confirmar_criacao, {
      req(input$novo_nome, input$novo_email, input$nova_senha, input$confirmar_senha)
      
      if (input$nova_senha != input$confirmar_senha) {
        output$mensagem_criacao <- renderText("As senhas não coincidem!")
        return()
      }
      
      if (!grepl("@ulusofona\\.pt$", input$novo_email)) {
        output$mensagem_criacao <- renderText("É necessário um email institucional da Lusófona.")
        return()
      }
      
      con <- poolCheckout(pool)
      on.exit(poolReturn(con))
      
      # Verificar se o email já existe
      query_check <- dbSendQuery(con, "SELECT id FROM utilizador WHERE email = $1")
      dbBind(query_check, list(input$novo_email))
      existente <- dbFetch(query_check)
      dbClearResult(query_check)
      
      if (nrow(existente) > 0) {
        output$mensagem_criacao <- renderText("Email já registado!")
        return()
      }
      
      # Guardar senha cifrada
      senha_hash <- password_store(input$nova_senha)
      
      # Inserir novo utilizador
      query_insert <- dbSendQuery(con, "INSERT INTO utilizador (nome, email, senha) VALUES ($1, $2, $3)")
      dbBind(query_insert, list(input$novo_nome, input$novo_email, senha_hash))
      dbClearResult(query_insert)
      
      removeModal()
      output$mensagem <- renderText("Conta criada com sucesso! Faça login.")
    })
  })
}
