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
require(readr)

### read in data
# data <- read_csv("data/")
# reportdata <- read_csv("data/")

### create datasets for application


### create choicelists, selectors, ...


### create map
activityMap <- leaflet(options = leafletOptions(minZoom = 2)) %>%
  addTiles()




server <- function(input, output, session){
	output$activity <- renderPlot(plot(mtcars$mpg, mtcars$hp))

	output$ec <- renderPlot(plot(mtcars$cyl, mtcars$gear))

	output$map <- renderLeaflet({
	  activityMap
	})
}
