mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("RiskTrack!"),
    p("Página principal."),
  )
}

mod_dashboard_server <- function(id, estado_login) {
  moduleServer(id, function(input, output, session) {

  })
}
