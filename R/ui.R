#' Shiny UI
#'
#' Core UI of package.
#'
#' @param req The request object.
#'
#' @import shiny
#' @importFrom bslib bs_theme
#'
#' @keywords internal

require(shiny)
require(bslib)
require(htmltools)

ui <- function(req) {
  navbarPage(
    theme = bs_theme(version = 5),
    header = list(assets()),
    title = "Peacekeeping ACTivities",
    id = "main-menu",
    #### Landing Page ####
    tabPanelBody(
      "Welcome Page",
      h5("hello I appear upon loading but not in the navbar"),
      h6("also i lowkey fix things"),
      actionButton("lol", "Press me daddy!", icon = icon("hand-paper"))
    ),
    navbarMenu(
      "Plots",
      #### Plot: Peacekeeping Activities ####
      tabPanel(
        "Peacekeeping Activities",
        sidebarLayout(
          ##### Sidebar #####
          sidebarPanel(
            tags$style(HTML(".tabbable > .nav > li > a {background-color: lightgrey; width: 100%; text-align: center}")),
            tags$style(HTML(".tabbable > .nav > li {width: 50%}")),
            tabsetPanel(
              id = "type-act",
              type = "pills",
              ###### Aggregated ######
              tabPanel(
                "Aggregated",
                hr(),
                h6("Timerange"),
                sliderInput(
                  "act_years",
                  label = NULL,
                  min = as.Date("1989", format = "%Y"),
                  max = as.Date("2018", format = "%Y"),
                  value = c(as.Date("1989", format = "%Y"), as.Date("2018", format = "%Y")),
                  timeFormat = "%Y"
                ),
                hr(),
                h6("Missions"),
                div(
                  style = "text-align:center",
                  tags$div(id = 'act_placeholder_mission'),
                  actionButton(
                    "act_insert_mission",
                    label = NULL,
                    icon = icon("plus-circle"),
                    class = "btn-primary",
                    style = "margin: 1%"
                  ),
                  actionButton(
                    "act_remove_mission",
                    label = NULL,
                    icon = icon("minus-circle"),
                    class = "btn-default",
                    style = "margin: 1%"
                  )
                ),
                hr(),
                h6("Activities"),
                div(
                  style = "text-align:center",
                  tags$div(id = 'act_placeholder_act'),
                  actionButton(
                    "act_insert_act",
                    label = NULL,
                    icon = icon("plus-circle"),
                    class = "btn-primary",
                    style = "margin: 1%"
                  ),
                  actionButton(
                    "act_remove_act",
                    label = NULL,
                    icon = icon("minus-circle"),
                    class = "btn-default",
                    style = "margin: 1%"
                  )
                ),
                hr(),
                div(
                  style = "text-align:center",
                  checkboxInput("act_smooth1", label = "Smooth line"),
                  checkboxInput("act_color1", label = "Use colors"),
                  actionButton(
                    "act_draw_plot1",
                    label = "draw plot",
                    icon = icon("paint-brush"),
                    class = "btn-warning",
                    style = "margin: 1%"
                  )
                )
              ),
              ###### Per mission ######
              tabPanel(
                "Mission",
                hr(),
                h6("Missions"),
                radioButtons(
                  "act_select_mission",
                  label = NULL,
                  choices = list("All" = "all",
                                 "Selected" = "select"),
                  inline = TRUE
                ),
                conditionalPanel(
                  "input.act_select_mission == 'select'",
                  div(
                    style = "text-align:center",
                    selectizeInput(
                      "act_select_missions",
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
                    )
                  )
                ),
                hr(),
                h6("Activities"),
                selectizeInput(
                  "act_select_act",
                  label = NULL,
                  choices = list("Activities" = "Activities",
                                 "Petting cats" = "Petting cats",
                                 "Feeding stray dogs" = "Feeding stray dogs"),
                  selected = "Activities",
                  multiple = TRUE
                ),
                hr(),
                div(
                  style = "text-align:center",
                  checkboxInput("act_smooth2", label = "Smooth line"),
                  checkboxInput("act_color2", label = "Use colors"),
                  actionButton(
                    "act_draw_plot2",
                    label = "draw plot",
                    icon = icon("paint-brush"),
                    class = "btn-warning",
                    style = "margin: 1%"
                  )
                )
              )
            )
          ),
          ##### Main panel #####
          mainPanel(
            h4("lul im big and placeholder for a plot"),
            verbatimTextOutput("inserted_mission"),
            verbatimTextOutput("mission_grp")
          )
        )
      ),
      #### Plot: Engagement Categories ####
      tabPanel(
        "Engagement Categories",
        sidebarLayout(
          ##### Sidebar #####
          sidebarPanel(
            tabsetPanel(
              id = "type-ec",
              type = "pills",
              ###### Aggregated ######
              tabPanel(
                "Aggregated",
                hr(),
                h6("Timerange"),
                sliderInput(
                  "ec_years",
                  label = NULL,
                  min = as.Date("1989", format = "%Y"),
                  max = as.Date("2018", format = "%Y"),
                  value = c(as.Date("1989", format = "%Y"), as.Date("2018", format = "%Y")),
                  timeFormat = "%Y"
                ),
                hr(),
                h6("Missions"),
                div(
                  style = "text-align:center",
                  tags$div(id = 'ec_placeholder_mission'),
                  actionButton(
                    "ec_insert_mission",
                    label = NULL,
                    icon = icon("plus-circle"),
                    class = "btn-primary",
                    style = "margin: 1%"
                  ),
                  actionButton(
                    "ec_remove_mission",
                    label = NULL,
                    icon = icon("minus-circle"),
                    class = "btn-default",
                    style = "margin: 1%"
                  )
                ),
                hr(),
                h6("Activities"),
                div(
                  style = "text-align:center",
                  tags$div(id = 'ec_placeholder_act'),
                  actionButton(
                    "ec_insert_act",
                    label = NULL,
                    icon = icon("plus-circle"),
                    class = "btn-primary",
                    style = "margin: 1%"
                  ),
                  actionButton(
                    "ec_remove_act",
                    label = NULL,
                    icon = icon("minus-circle"),
                    class = "btn-default",
                    style = "margin: 1%"
                  )
                ),
                hr(),
                div(
                  style = "text-align:center",
                  checkboxInput("ec_smooth1", label = "Smooth line"),
                  checkboxInput("ec_color1", label = "Use colors"),
                  actionButton(
                    "ec_draw_plot1",
                    label = "draw plot",
                    icon = icon("paint-brush"),
                    class = "btn-warning",
                    style = "margin: 1%"
                  )
                )
              ),
              ###### Per mission ######
              tabPanel(
                "Mission",
                hr(),
                h6("Missions"),
                radioButtons(
                  "ec_select_mission",
                  label = NULL,
                  choices = list("All" = "all",
                                 "Selected" = "select"),
                  inline = TRUE
                ),
                conditionalPanel(
                  "input.ec_select_mission == 'select'",
                  div(
                    style = "text-align:center",
                    selectizeInput(
                      "ec_select_missions",
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
                    )
                  )
                ),
                hr(),
                h6("Activities"),
                selectizeInput(
                  "ec_select_act",
                  label = NULL,
                  choices = list("Activities" = "Activities",
                                 "Petting cats" = "Petting cats",
                                 "Feeding stray dogs" = "Feeding stray dogs"),
                  selected = "Activities",
                  multiple = TRUE
                ),
                hr(),
                div(
                  style = "text-align:center",
                  checkboxInput("ec_smooth2", label = "Smooth line"),
                  checkboxInput("ec_color2", label = "Use colors"),
                  actionButton(
                    "ec_draw_plot2",
                    label = "draw plot",
                    icon = icon("paint-brush"),
                    class = "btn-warning",
                    style = "margin: 1%"
                  )
                )
              )
            )
          ),
          ##### Main panel #####
          mainPanel(
            h2("im smaller and different")
          )
        )
      )
    ),
    navbarMenu(
      "Missions",
      #### Missions: Overview ####
      tabPanel(
        "Mission overview",
        shiny::h3("Here comes overview by continent, timeranges")
      ),
      #### Missions: Data Coverage ####
      tabPanel(
        "Data coverage",
        shiny::h3("SG report coverage per mission (maybe: links to UN Docs)")
      ),
      #### Missions: Activity map ####
      tabPanel(
        "Activity map",
        conditionalPanel(condition = "window.innerWidth < 1000 || window.innerHeight < 720",
                         div(
                           class = "outer",
                           tags$style(type = "text/css", "#map {height: calc(100vh - 110px) !important;}"),
                           leafletOutput("map")
                         ))
      )
    ),
    #### About page/ Impressum ####
    tabPanel(
      "About",
      shiny::h3("Info about the data, collection, funding, documentation")
    )
  )}

#' Assets
#'
#' Includes all assets.
#' This is a convenience function that wraps
#' [serveAssets] and allows easily adding additional
#' remote dependencies (e.g.: CDN) should there be any.
#'
#' @importFrom shiny tags
#'
#' @keywords internal
assets <- function(){
	list(
		serveAssets(), # base assets (assets.R)
		tags$head(
			# Place any additional depdendencies here
			# e.g.: CDN
		)
	)
}
