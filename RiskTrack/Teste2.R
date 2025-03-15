library(shiny)

ui <- fluidPage(
  actionButton("open_modal", "Abrir Formulário"),  # Botão para abrir o modal
  
  # Aqui você define o modal que será exibido
  uiOutput("form_modal")
)

server <- function(input, output, session) {
  
  # Definindo o conteúdo do modal
  observeEvent(input$open_modal, {
    showModal(modalDialog(
      title = "Formulário de Upload",
      textInput("file_name", "Nome do arquivo:"),
      fileInput("file_upload", "Escolha o arquivo", accept = ".csv"),
      actionButton("submit", "Enviar"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Processando o envio do formulário (apenas exemplo)
  observeEvent(input$submit, {
    file <- input$file_upload
    file_name <- input$file_name
    if (!is.null(file)) {
      # Aqui você pode processar o arquivo e o nome dado
      showModal(modalDialog(
        title = "Arquivo Enviado",
        paste("Nome do arquivo: ", file_name),
        paste("Arquivo:", file$name),
        easyClose = TRUE
      ))
    } else {
      showModal(modalDialog(
        title = "Erro",
        "Por favor, escolha um arquivo antes de enviar.",
        easyClose = TRUE
      ))
    }
  })
}

shinyApp(ui, server)
