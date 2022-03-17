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
        fluidRow(
          column(
            width = 4,
            "Here come modular inputs to select and group missions",
            textInput("example", "Group 1"),
            textInput("example2", "Group 2")
          ),
          column(width = 6,
                 offset = 2,
                 plotOutput("plot"))
        )
      ),
      tabPanel(
        "Engagement Categories",
        shiny::h3("Engagment Types"),
        fluidRow(
          column(
            width = 4,
            "Here come modular inputs to select and group missions",
            checkboxGroupInput(
              "example",
              "EC",
              choices = list(
                "Assist" = "Assist",
                "Implement" = "Implement",
                "Monitor" = "Monitor"
              )
            ),
            textInput("example2", "Group 2")
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
        "Activity by map",
        conditionalPanel(condition = "window.innerWidth < 1000 || window.innerHeight < 720",
                         div(
                           class = "outer",
                           tags$style(type = "text/css", "#basemap {height: calc(100vh - 110px) !important;}"),
                           leafletOutput("basemap")
                         )
                         )
      )
    ),
    tabPanel(
      "About",
      shiny::h3(
        "Info about the data, collection, funding, later link to data download, documentation"
      )
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
