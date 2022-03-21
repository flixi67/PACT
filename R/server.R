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
  ### PLACEHOLDERS
	output$activity <- renderPlot(plot(mtcars$mpg, mtcars$hp))

	output$ec <- renderPlot(plot(mtcars$cyl, mtcars$gear))

	output$map <- renderLeaflet({
	  activityMap
	})

	### Modular plot inputs
	## Groups of missions
	inserted_mission <- c()

	observeEvent(input$act_insert_mission, {
	  btn <- input$act_insert_mission
	  id <- paste0('act_mission', btn)
	  insertUI(
	    selector = '#act_placeholder_mission',
	    ## wrap element in a div with id for ease of removal
	    ui = tags$div(
	      selectizeInput(
	        id,
	        label = NULL,
	        choices = list("Test" = "ts",
	                       "Doesit" = "ds",
	                       "Work?" = "work"),
	        multiple = TRUE
	      ),
	      id = id
	    )
	  )
	  inserted_mission <<- c(inserted_mission, id)
	})

	observeEvent(input$act_remove_mission, {
	  removeUI(
	    ## pass in appropriate div id
	    selector = paste0('#', inserted_mission[length(inserted_mission)])
	  )
	  inserted_mission <<- inserted_mission[-length(inserted_mission)]
	})

	## Groups of activities
	inserted_act <- c()

	observeEvent(input$act_insert_act, {
	  btn <- input$act_insert_act
	  id <- paste0('act_act', btn)
	  insertUI(
	    selector = '#act_placeholder_act',
	    ## wrap element in a div with id for ease of removal
	    ui = tags$div(
	      selectizeInput(
	        id,
	        label = NULL,
	        choices = list("More" = "ts",
	                       "Activities" = "ds",
	                       "Here" = "work"),
	        multiple = TRUE
	      ),
	      id = id
	    )
	  )
	  inserted_act <<- c(inserted_act, id)
	})

	observeEvent(input$act_remove_act, {
	  removeUI(
	    ## pass in appropriate div id
	    selector = paste0('#', inserted_act[length(inserted_act)])
	  )
	  inserted_act <<- inserted_act[-length(inserted_act)]
	})
}
