library(shiny)
port <- Sys.getenv('PORT')
shiny::runApp(
  appdir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)
