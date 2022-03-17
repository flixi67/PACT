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
    title = "PACT",
    id = "main-menu",
    navbarMenu(
      "Plots",
      tabPanel(
        "Peacekeeping Activities",
        shiny::h3("Peacekeeping Activities"),
        tabsetPanel(
          id = "type-act",
          type = "pills",
          tabPanel(
            "Aggregated",
            "Here come modular inputs to select and group missions",
            sliderInput(
              "act_years",
              "Timerange",
              min = as.Date("1989", format = "%Y"),
              max = as.Date("2018", format = "%Y"),
              value = c(as.Date("1989", format = "%Y"), as.Date("2018", format = "%Y")),
              timeFormat = "%Y"
            ),
            textInput("example", "Group 1"),
            textInput("example2", "Group 2")
          ),
          tabPanel(
            "Mission",
            "Mission inputs where you can select a specific mission and",
            textInput("examplee", "Group 1"),
            textInput("exampleee", "Group 2"),
            textInput("exampleeee", "Group 3")
          )
        )
      ),
      tabPanel(
        "Engagement Categories",
        shiny::h3("Engagment Types"),
        tabsetPanel(
          id = "type-ec",
          type = "tabs",
          tabPanel(
            "Aggregated",
            "Here come modular inputs to select and group missions",
            sliderInput(
              "act_years",
              "Timerange",
              min = 1989,
              max = 2018,
              value = c(1989, 2018)
            ),
            textInput("example", "Group 1"),
            textInput("example2", "Group 2")
          ),
          tabPanel(
            "Mission",
            "Mission inputs where you can select a specific mission and",
            textInput("examplee", "Group 1"),
            textInput("exampleee", "Group 2"),
            textInput("exampleeee", "Group 3")
          )
        )
      )
    ),
    navbarMenu(
      "Missions",
      tabPanel(
        "Mission overview",
        shiny::h3("Here comes overview by continent, timeranges")
      ),
      tabPanel(
        "Data coverage",
        shiny::h3("SG report coverage per mission (maybe: links to UN Docs)")
      ),
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
    tabPanel(
      "About",
      shiny::h3("Info about the data, collection, funding, documentation")
    )
  )
}

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
