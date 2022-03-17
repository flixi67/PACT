#' Server
#'
#' Core server function.
#'
#' @param input,output Input and output list objects
#' containing said registered inputs and outputs.
#' @param session Shiny session.
#'
#' @noRd
#' @keywords internal

require(dplyr)
require(leaflet)

baseMap <- leaflet(options = leafletOptions(minZoom = 2)) %>%
  addTiles()

server <- function(input, output, session){
	output$plot <- renderPlot(plot(mtcars$mpg, mtcars$hp))

	output$ basemap <- renderLeaflet({
	  baseMap
	})
}
