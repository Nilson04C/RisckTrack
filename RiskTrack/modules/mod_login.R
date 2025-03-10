mod_login_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "width: 300px; margin: auto; text-align: center;",
      h2("RiskTrack - Login"),
      textInput(ns("email"), "Email:"),
      passwordInput(ns("senha"), "Senha:"),
      actionButton(ns("entrar"), "Entrar", class = "btn-primary"),
      verbatimTextOutput(ns("mensagem"))
    )
  )
}

mod_login_server <- function(id, estado_login) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$entrar, {
      # Aqui podes substituir por uma base de dados real
      credenciais_validas <- input$email == "Teste" && input$senha == "1234"
      
      
      
      estado_login(TRUE)
      
      
      
      if (credenciais_validas) {
        estado_login(TRUE)  # Atualiza o estado para "logado"
      } else {
        output$mensagem <- renderText("Credenciais inválidas!")
      }
    })
  })
}
