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

require(tidyverse)
require(leaflet)
require(zoo)
require(jsonlite)

Sys.setlocale("LC_TIME", "English")

### read in data
data <- read_csv("data/PACT_mission-month_full.csv")
reportdata <- read_rds("data/transformed-paragraphs.Rds")

### create datasets for application
data_all <- data %>%
  select(PKO, month_index, month, year, contains("_All"))

data_ec <- data %>%
  select(-contains("_IA"), -contains("Abuse"), -contains("Infra"),
         -contains("AssistPolicies"), -contains("AssistAgents"),
         -contains("AssistOther"), -contains("AssistComm")) %>%
  select(PKO, month_index, month, year,
         contains("_Monitor"), contains("_MaterialSupport"),
         contains("_Meeting"), contains("_Advocate"), contains("_Outreach"),
         contains("_Implement"), contains("_Assist"))

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
mission_data$name[mission_data$PKO == "MONUC"] <- "United Nations Organization Mission in the Democratic Republic of the Congo"
mission_data$name[mission_data$PKO == "MONUSCO"] <- "United Nations Stabilization Mission in the Democratic Republic of the Congo"
mission_data$name[mission_data$PKO == "MINUSCA"] <- "United Nations Multidimensional Integrated Stabilization Mission in the Central African Republic"
mission_data$name[mission_data$PKO == "MINUSMA"] <- "United Nations Multidimensional Integrated Stabilization Mission in Mali"

activity_list <- as.list(names(data) %>%
                           str_subset("_All") %>%
                           str_remove("_All") %>%
                           set_names(names(data) %>%
                                       str_subset("_All") %>%
                                       str_remove("_All") %>%
                                       str_replace("_", ":") %>%
                                       str_replace_all("(?=[A-Z])", " ") %>%
                                       str_trim()))

blair_cat <- list("Security-related" = c("DisarmamentDemobilization",
                                         "Reintegration",
                                         "ControlSALW",
                                         "Demilitarization",
                                         "ArmsEmbargo",
                                         "Ceasefire",
                                         "PeaceProcess",
                                         "CivilianProtection",
                                         "Operations_UseOfForce",
                                         "Operations_PatrolsInterventions"),
                  "Peacbuilding-related" = c("PoliceReform",
                                             "MilitaryReform",
                                             "JusticeSectorReform",
                                             "TransitionalJustice",
                                             "PrisonReform",
                                             "BorderControl",
                                             "Demining",
                                             "Resources",
                                             "StateAuthority",
                                             "StateAdministration",
                                             "DemocraticInstitutions",
                                             "ElectionAssistance",
                                             "ElectoralSecurity",
                                             "VoterEducation",
                                             "PartyAssistance",
                                             "CivilSocietyAssistance",
                                             "Media",
                                             "LocalReconciliation",
                                             "NationalReconciliation",
                                             "EconomicDevelopment",
                                             "HumanitarianRelief",
                                             "PublicHealth",
                                             "RefugeeAssistance",
                                             "LegalReform",
                                             "PowerSharing"),
                  "Cross-cutting" = c("HumanRights",
                                      "ChildRights",
                                      "SexualViolence",
                                      "Gender")
                  )

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
	        choices = activity_list,
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
	        choices = activity_list,
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
	    mission_grp <- list()
	    for (a in inserted_mission) {
	      mission_grp[[a]] <- input[[a]]
	    }
	    mission_grp
	  })

	  activity_grp <- if(length(inserted_act) == 0) {
	    blair_cat
	  } else {
	    isolate({
	      activity_grp <- list()
	      for (a in inserted_act) {
	        activity_grp[[a]] <- input[[a]]
	      }
	      activity_grp
	    })
	  }

	  act_agg_data <- data_all %>%
	    pivot_longer(cols = !c(PKO, month_index, month, year),
	                 names_to = "Activity",
	                 values_to = "number") %>%
	    mutate(Activity = str_remove(Activity, "_All"),
	           Activity =input_aggregate(Activity, activity_grp),
	           PKO = input_aggregate(PKO, mission_grp)) %>%
	    filter(!is.na(Activity), !is.na(PKO)) %>%
	    when(input$act_select_time == "mission_month"
	         ~ group_by(., PKO, month_index, Activity),
	         ~ group_by(., PKO, year, month, Activity)) %>%
	    when(input$act_select_time == "timerange"
	         ~ filter(., year >= input$act_select_time2[1] & year <= input$act_select_time2[2]),
	         ~ .) %>%
	    summarise(number = sum(number, na.rm = TRUE))

	  output$testdata <- renderDataTable(act_agg_data)

	  output$testplot <- renderPlot({
	    when(input$act_smooth1 == FALSE & input$act_select_time == "mission_month"
	         ~ ggplot(data = act_agg_data) +
	           geom_line(aes(y = number, x = month_index, group = Activity)) +
	           facet_wrap(~ PKO, scales = "free_x"),
	         input$act_smooth1 == TRUE & input$act_select_time == "mission_month"
	         ~ ggplot(data = act_agg_data) +
	           geom_smooth(
	             aes(
	               x = as.numeric(month_index),
	               y = as.numeric(number),
	               group = Activity
	             ),
	             se = FALSE
	           ) +
	           facet_wrap(~ PKO))
	  })

	  output$smooth <- renderPrint(input$act_smooth1)

	})
	##### Peaceeping Activities (Per mission) #####

	##### Engagement Categories (Aggregated) #####
	##### Engagement Categories (Per mission) #####

}
