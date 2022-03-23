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
require(rlang)
require(zoo)
require(jsonlite)

Sys.setlocale("LC_TIME", "English")

### read in data
data <- read_csv("data/PACT_mission-month_full.csv")
reportdata <- read_rds("data/transformed-paragraphs.Rds")

### create datasets for application


### create choicelists, selectors, ...
mission_list <- as.list(unique(data$PKO) %>%
                          set_names(unique(data$PKO)))

mission_data <- data %>%
  group_by(PKO) %>%
  summarise(dur = n(),
            start = as.character(min(as.yearmon(paste(month, year), "%m %Y"))),
            end = as.character(max(as.yearmon(paste(month, year), "%m %Y"))),
            country = first(Mission_Country),
            continent = first(Mission_Continent),
            continent_i = case_when(continent == "South America"  ~ "americas",
                                    TRUE ~ tolower(continent))) %>%
  left_join(read_rds("data/PKO_start_end.Rds"), by = c("PKO" = "Acronym")) %>%
  rename("name" = "Mission name")
mission_data$name[mission_data$PKO == "UNAVEM"] <- "United Nations Angola Verification Mission"
mission_data$name[mission_data$PKO == "UNSOM"] <- "United Nations Operation in Somalia"
# UNSOM in PACT is officially called UNOSOM, UNSOM is a different mission that started in 2017
# more missions were full mission name is deprecated
# ...


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
	##### Groups of missions (peackeeping activities) #####
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
	        choices = "",
	        multiple = TRUE,
	        options = list(
	          valueField = "PKO",
	          labelField = "PKO",
	          searhField = "PKO",
	          create = FALSE,
	          placeholder = "Select missions to aggregate",
	          options = toJSON(mission_data),
	          render = I("{
      option: function(item, escape) {
        return '<div>' +
               '<strong><img src=\"https://raw.githubusercontent.com/FortAwesome/Font-Awesome/28e297f07af26f148c15e6cbbd12cea3027371d3/svgs/solid/earth-' + escape(item.continent_i) + '.svg\" width=20 />' + escape(item.PKO) + '</strong></br>' +
               escape(item.name) +
               ' <em>' + ' (' + escape(item.start) + ' - ' + escape(item.end) + ')' + '</em>' +
            '<ul>' +
        '</div>';
      }
    }")
	        )
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

	##### Groups of activities (peacekeeping activities) #####
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

	##### Groups of missions (engagement categories) #####
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
	        choices = "",
	        multiple = TRUE,
	        options = list(
	          valueField = "PKO",
	          labelField = "PKO",
	          searhField = "PKO",
	          create = FALSE,
	          placeholder = "Select missions to aggregate",
	          options = toJSON(mission_data),
	          render = I("{
      option: function(item, escape) {
        return '<div>' +
               '<strong><img src=\"https://raw.githubusercontent.com/FortAwesome/Font-Awesome/28e297f07af26f148c15e6cbbd12cea3027371d3/svgs/solid/earth-' + escape(item.continent_i) + '.svg\" width=20 />' + escape(item.PKO) + '</strong></br>' +
               escape(item.name) +
               ' <em>' + ' (' + escape(item.start) + ' - ' + escape(item.end) + ')' + '</em>' +
            '<ul>' +
        '</div>';
      }
    }")
	        )
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

	##### Groups of activities (engagement categories) #####
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

	#### Plot data ####
	##### Peaceeping Activities (Aggregated) #####
	observeEvent(input$act_draw_plot1, {
	  mission_grp <- isolate({
	    out <- list()
	    for (a in inserted_mission) {
	      out[[a]] <- input[[a]]
	    }
	    out
	  })
	  output$inserted_mission <- renderPrint(inserted_mission)
	  output$mission_grp <- renderPrint(mission_grp)
	  activity_grp <- NULL
	})
	##### Peaceeping Activities (Per mission) #####

	##### Engagement Categories (Aggregated) #####
	##### Engagement Categories (Per mission) #####

}
