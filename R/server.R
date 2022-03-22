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
data <- read_csv("data/PACT_mission-month_full.csv")
reportdata <- read_rds("data/transformed-paragraphs.Rds")

### create datasets for application


### create choicelists, selectors, ...
mission_list <- as.list(unique(data$PKO) %>%
                          set_names(unique(data$PKO)))

activity_list_all <- as.list(names(data) %>%
                               str_subset("_All") %>%
                               set_names(names(data) %>%
                                           str_subset("_All") %>%
                                           str_remove("_All")))

activity_list <- as.list(names(data) %>%
                           str_subset("_All") %>%
                           str_remove("_All") %>%
                           set_names(names(data) %>%
                                       str_subset("_All") %>%
                                       str_remove("_All")))

### create map
activityMap <- leaflet(options = leafletOptions(minZoom = 2)) %>%
  addTiles()




server <- function(input, output, session){
  #### PLACEHOLDERS ####
	output$map <- renderLeaflet({
	  activityMap
	})

	#### Modular plot inputs ####
	## Groups of missions (peackeeping activities)
	inserted_mission <- c()

	observeEvent(input$act_insert_mission, {
	  btn <- input$act_insert_mission
	  id <- paste0('act_mission', btn)
	  insertUI(
	    selector = '#act_placeholder_mission',
	    ui = tags$div(
	      selectizeInput(
	        id,
	        label = NULL,
	        choices = mission_list,
	        multiple = TRUE
	      ),
	      id = id
	    )
	  )
	  inserted_mission <<- c(inserted_mission, id)
	})

	observeEvent(input$act_remove_mission, {
	  removeUI(
	    selector = paste0('#', inserted_mission[length(inserted_mission)])
	  )
	  inserted_mission <<- inserted_mission[-length(inserted_mission)]
	})

	## Groups of activities (peacekeeping activities)
	inserted_act <- c()

	observeEvent(input$act_insert_act, {
	  btn <- input$act_insert_act
	  id <- paste0('act_act', btn)
	  insertUI(
	    selector = '#act_placeholder_act',
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
	    selector = paste0('#', inserted_act[length(inserted_act)])
	  )
	  inserted_act <<- inserted_act[-length(inserted_act)]
	})

	## Groups of missions (engagement categories)
	inserted_mission <- c()

	observeEvent(input$ec_insert_mission, {
	  btn <- input$ec_insert_mission
	  id <- paste0('ec_mission', btn)
	  insertUI(
	    selector = '#ec_placeholder_mission',
	    ui = tags$div(
	      selectizeInput(
	        id,
	        label = NULL,
	        choices = mission_list,
	        multiple = TRUE
	      ),
	      id = id
	    )
	  )
	  inserted_mission <<- c(inserted_mission, id)
	})

	observeEvent(input$ec_remove_mission, {
	  removeUI(
	    selector = paste0('#', inserted_mission[length(inserted_mission)])
	  )
	  inserted_mission <<- inserted_mission[-length(inserted_mission)]
	})

	## Groups of activities (engagement categories)
	inserted_act <- c()

	observeEvent(input$ec_insert_act, {
	  btn <- input$ec_insert_act
	  id <- paste0('ec_act', btn)
	  insertUI(
	    selector = '#ec_placeholder_act',
	    ui = tags$div(
	      selectizeInput(
	        id,
	        label = NULL,
	        choices = list("Activities" = "Activities",
	                       "Petting cats" = "Petting cats",
	                       "Feeding stray dogs" = "Feeding stray dogs"),
	        multiple = TRUE
	      ),
	      id = id
	    )
	  )
	  inserted_act <<- c(inserted_act, id)
	})

	observeEvent(input$ec_remove_act, {
	  removeUI(
	    selector = paste0('#', inserted_act[length(inserted_act)])
	  )
	  inserted_act <<- inserted_act[-length(inserted_act)]
	})
}
