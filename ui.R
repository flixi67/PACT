library(tidyverse)
library(leaflet)
library(zoo)
library(jsonlite)
library(sf)
# library(shiny)
library(rlang)
library(bslib)
library(htmltools)
library(tippy)
library(ggforce)

shinyUI(
  navbarPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "cosmo"),
    title = "PACT Interactive Visualization",
    id = "main_menu",
    #### Landing Page #### Could be removed later, when this issue is fixed https://github.com/rstudio/shiny/issues/3604
    tabPanelBody(
      "Landing Page",
      style = "width: 80%; margin: auto",
      h4("Peacekeeping Activities Dataset (PACT): Interactive Visualization"),
      p("Welcome to the interactive plotting tool for the peacekeeping activity dataset."),
      div(
        h5("Available tools"),
        tags$ul(
          tags$li(
            strong("Peacekeeping Activities - Aggregated:"),  "Create mission groups, and compare them over time."
          ),
          tags$li(
            strong("Peacekeeping Activities - Mission:"), "Deep dive into certain activities across missions."
          ),
          tags$li(
            strong("Engagement Categories - Aggregated:"), "Create mission groups, and compare types of engagement over time."
          ),
          tags$li(
            strong("Engagement Categories - Activity:"), "How are different activities implemented across missions? Select missions to compare, select activities to aggregate, and go!"
          )
        ),
        h5("Information"),
        p(
          "Since the data underlying the tool is suitable for diverse use cases, we opted to code a plotting tool that allows the user a maximum of flexibility. You can create your own mission and activity groups. The benefit is that this allows for manyfold analysis and a very deep dive into the yet unreleased data. The tradeoff is that it requires a bit of preparation and knowledge of the data to use this tool to its full potential."
        ),
        p(
          "It is therefore recommended to first read the Guide and About pages and check out the project and data description before jumping into the plotting tool."
        ),
        p(
          "If you have any questions regarding this tool, the project or the data, you can find contact information in the About page."
        ),
        p(
          "Have fun and some insights!"
        )
      )
    ),
    navbarMenu(
      "Plotting tool",
      #### Plot: Peacekeeping Activities ####
      tabPanel(
        "Peacekeeping Activities",
        sidebarLayout(
          ##### Sidebar #####
          sidebarPanel(
            tags$style(
              HTML(
                ".tabbable > .nav > li > a {background-color: lightgrey; width: 100%; text-align: center}"
              )
            ),
            tags$style(HTML(".tabbable > .nav > li {width: 50%}")),
            tabsetPanel(
              id = "type_act",
              type = "pills",
              ###### Aggregated ######
              tabPanel(
                "Aggregated",
                value = 1,
                hr(),
                h6("Timeline"),
                radioButtons(
                  "act_select_time",
                  label = NULL,
                  choices = list("Mission month" = "mission_month",
                                 "Calendar month" = "timerange"),
                  inline = TRUE
                ),
                tippy_this(
                  "act_select_time",
                  "Changes the Y-axis between month since mission start or calendar date"
                ),
                conditionalPanel(
                  "input.act_select_time == 'timerange'",
                  sliderInput(
                    "act_select_time2",
                    label = NULL,
                    min = 1989,
                    max = 2018,
                    value = c(1989, 2018),
                    sep = ""
                  )
                ),
                hr(),
                h6("Missions"),
                div(
                  style = "text-align:center",
                  id = "act_div_mission",
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
                tippy_this(
                  "act_div_mission",
                  "Insert and remove fields for mission grouping. Missions can be searched by acronym or full name"
                ),
                hr(),
                h6("Activities"),
                div(
                  style = "text-align:center",
                  id = "act_div_act",
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
                tippy_this(
                  "act_div_act",
                  "Insert and remove fields for activity grouping. The same activity should not be added to different groups. If none are selected, categories according to <a href=\"https://onlinelibrary.wiley.com/doi/full/10.1111/ajps.12650\" target=\"_blank\">Blair et. al (2020)</a> are used",
                  interactive = TRUE
                ),
                hr(),
                div(
                  style = "text-align:center",
                  checkboxInput("act_smooth1", label = "Smooth line"),
                  checkboxGroupInput(
                    "act_color1",
                    label = NULL,
                    choices = c("Use colors" = "Activity")
                  ),
                  actionButton(
                    "act_draw_plot1",
                    label = "draw plot",
                    icon = icon("paint-brush"),
                    class = "btn-warning",
                    style = "margin: 1%"
                  )
                  # hr(),
                  # actionButton(
                  #   "act_download_plot1",
                  #   label = "download",
                  #   icon = icon("download"),
                  #   class = "btn-secondary",
                  #   style = "margin: 1%"
                  # )
                )
              ),
              ###### Per mission ######
              tabPanel(
                "Mission",
                value = 2,
                hr(),
                h6("Missions"),
                tippy_this("act_select_missions",
                           "Missions to display as facets in the plot"),
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
                      searchField = c("PKO", "name"),
                      create = FALSE,
                      placeholder = "Select missions to show",
                      options = toJSON(mission_data),
                      render = I(
                        "{
        option: function(item, escape) {
          return '<div>' +
                 '<strong><img src=\"https://raw.githubusercontent.com/FortAwesome/Font-Awesome/28e297f07af26f148c15e6cbbd12cea3027371d3/svgs/solid/earth-' + escape(item.continent_i) + '.svg\" width=20 />' + escape(item.PKO) + '</strong></br>' +
                 escape(item.name) +
                 ' <em>' + ' (' + escape(item.start) + ' - ' + escape(item.end) + ')' + '</em>' +
              '<ul>' +
          '</div>';
        }
      }"
                      )
                    )
                  )
                ),
                hr(),
                h6("Activities"),
                div(
                  style = "text-align:center",
                  id = "act_div_act2",
                  selectizeInput(
                    "act_select_act",
                    label = NULL,
                    choices = activity_list,
                    multiple = TRUE,
                    options = list(placeholder = "Select activities to aggregate")
                  )
                ),
                tippy_this(
                  "act_div_act2",
                  "Activities to be aggregated within each mission. When multiple are selected, plot shows the share of implemented activities"
                ),
                hr(),
                div(
                  style = "text-align:center",
                  checkboxInput("act_smooth2", label = "Smooth line"),
                  actionButton(
                    "act_draw_plot2",
                    label = "draw plot",
                    icon = icon("paint-brush"),
                    class = "btn-warning",
                    style = "margin: 1%"
                  )
                  # hr(),
                  # actionButton(
                  #   "act_download_plot2",
                  #   label = "download",
                  #   icon = icon("download"),
                  #   class = "btn-secondary",
                  #   style = "margin: 1%"
                  # )
                )
              )
            )
          ),
          ##### Main panel #####
          mainPanel(
            conditionalPanel("input.type_act == 1",
                             plotOutput("act_agg_plot")),
            conditionalPanel("input.type_act == 2",
                             plotOutput("act_mission_plot"))
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
              id = "type_ec",
              type = "pills",
              ###### Aggregated ######
              tabPanel(
                "Aggregated",
                value = 1,
                hr(),
                h6("Timeline"),
                radioButtons(
                  "ec_select_time",
                  label = NULL,
                  choices = list("Mission month" = "mission_month",
                                 "Calendar month" = "timerange"),
                  inline = TRUE
                ),
                conditionalPanel(
                  "input.ec_select_time == 'timerange'",
                  sliderInput(
                    "ec_select_time2",
                    label = NULL,
                    min = 1989,
                    max = 2018,
                    value = c(1989, 2018),
                    sep = ""
                  )
                ),
                hr(),
                h6("Missions"),
                div(
                  style = "text-align:center",
                  id = "ec_div_mission",
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
                tippy_this(
                  "ec_div_mission",
                  "Insert and remove fields for mission grouping. Missions can be searched by acronym or full name"
                ),
                hr(),
                h6("Engagement categories"),
                div(
                  style = "text-align:center",
                  checkboxGroupInput(
                    "ec_select_ec",
                    label = NULL,
                    choices = ec_list,
                    selected = ec_list,
                    inline = TRUE
                  )
                ),
                tippy_this("ec_select_ec", "Engagement categories to show in the plot"),
                hr(),
                div(
                  style = "text-align:center",
                  checkboxInput("ec_smooth1", label = "Smooth line"),
                  checkboxGroupInput(
                    "ec_color1",
                    label = NULL,
                    choices = c("Use colors" = "`Engagement category`")
                  ),
                  actionButton(
                    "ec_draw_plot1",
                    label = "draw plot",
                    icon = icon("paint-brush"),
                    class = "btn-warning",
                    style = "margin: 1%"
                  )
                  # hr(),
                  # actionButton(
                  #   "ec_download_plot1",
                  #   label = "download",
                  #   icon = icon("download"),
                  #   class = "btn-secondary",
                  #   style = "margin: 1%"
                  # )
                )
              ),
              ###### Per mission ######
              tabPanel(
                "Mission",
                value = 2,
                hr(),
                h6("Missions"),
                tippy_this("ec_select_missions",
                           "Missions to display as facets in the plot"),
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
                      searchField = c("PKO", "name"),
                      create = FALSE,
                      placeholder = "Select missions to show",
                      options = toJSON(mission_data),
                      render = I(
                        "{
        option: function(item, escape) {
          return '<div>' +
                 '<strong><img src=\"https://raw.githubusercontent.com/FortAwesome/Font-Awesome/28e297f07af26f148c15e6cbbd12cea3027371d3/svgs/solid/earth-' + escape(item.continent_i) + '.svg\" width=20 />' + escape(item.PKO) + '</strong></br>' +
                 escape(item.name) +
                 ' <em>' + ' (' + escape(item.start) + ' - ' + escape(item.end) + ')' + '</em>' +
              '<ul>' +
          '</div>';
        }
      }"
                      )
                    )
                  )
                ),
                hr(),
                h6("Activities"),
                div(
                  style = "text-align:center",
                  id = "ec_div_act",
                  selectizeInput(
                    "ec_select_act",
                    label = NULL,
                    choices = activity_list,
                    multiple = TRUE,
                    options = list(placeholder = "Select activities to aggregate")
                  )
                ),
                tippy_this(
                  "ec_div_act",
                  "Activities to be aggregated within each mission. When multiple are selected, plot shows the share of implemented activities in each engagement category"
                ),
                hr(),
                h6("Engagement categories"),
                div(
                  style = "text-align:center",
                  checkboxGroupInput(
                    "ec_select_ec2",
                    label = NULL,
                    choices = ec_list,
                    selected = ec_list,
                    inline = TRUE
                  )
                ),
                tippy_this("ec_select_ec2", "Select engagement categories to show"),
                hr(),
                div(
                  style = "text-align:center",
                  checkboxInput("ec_smooth2", label = "Smooth line"),
                  checkboxGroupInput(
                    "ec_color2",
                    label = NULL,
                    choices = c("Use colors" = "`Engagement category`")
                  ),
                  actionButton(
                    "ec_draw_plot2",
                    label = "draw plot",
                    icon = icon("paint-brush"),
                    class = "btn-warning",
                    style = "margin: 1%"
                  )
                  # hr(),
                  # actionButton(
                  #   "ec_download_plot2",
                  #   label = "download",
                  #   icon = icon("download"),
                  #   class = "btn-secondary",
                  #   style = "margin: 1%"
                  # )
                )
              )
            )
          ),
          ##### Main panel #####
          mainPanel(
            conditionalPanel("input.type_ec == 1",
                             plotOutput("ec_agg_plot")),
            conditionalPanel("input.type_ec == 2",
                             plotOutput("ec_mission_plot"))
          )
        )
      ),
      icon = icon("wrench")
    ),
    #### Missions: Activity map ####
    tabPanel(
      "Activity map",
      sidebarLayout(
        ##### Sidebar #####
        sidebarPanel(
          tags$style(
            HTML(
              ".tabbable > .nav > li > a {background-color: lightgrey; width: 100%; text-align: center}"
            )
          ),
          tags$style(HTML(".tabbable > .nav > li {width: 50%}")),
          div(
            h6("Activity map"),
            p(
              "This map helps to get an overview of the spatial and temporal dimension of UN peacekeeping activities. It shows where missions were employed in each year and which activities they implemented."
            ),
            p(
              "You can choose up to three activities, which will be plotted into the map by the symbols next to them."
            )
          ),
          hr(),
          div(
            id = "map_div_time",
            sliderInput(
              inputId = "map_select_time",
              label = NULL,
              min = 1989,
              max = 2018,
              value = 2018,
              step = 1,
              sep = "",
              width = "100%",
              animate = animationOptions(interval = 1000, loop = TRUE)
            )
          ),
          tippy_this(
            "map_div_time",
            "Select year to show in the map. The play button on the bottom right animates the map over time."
          ),
          div(
            id = "map_div_act",
            checkboxInput("map_show_active", "Show active missions"),
            div(
              selectizeInput(
                "map_activity1",
                label = NULL,
                choices = activity_list,
                multiple = TRUE,
                options = list(placeholder = "Select activity", maxItems = 1)
              ),
              style = "width: 80%; float: left"
            ),
            div(
              style = "width: 10%; float: left; padding: 5px",
              icon("hands-helping"),
              tags$style(".fa-hands-helping {color: blue}")
            ),
            div(
              selectizeInput(
                "map_activity2",
                label = NULL,
                choices = activity_list,
                multiple = TRUE,
                options = list(placeholder = "Select activity", maxItems = 1)
              ),
              style = "width: 80%; float: left"
            ),
            div(
              style = "width: 10%; float: left; padding: 5px",
              icon("people-carry"),
              tags$style(".fa-people-carry {color: red}")
            ),
            div(
              selectizeInput(
                "map_activity3",
                label = NULL,
                choices = activity_list,
                multiple = TRUE,
                options = list(placeholder = "Select activity", maxItems = 1)
              ),
              style = "width: 80%; float: left"
            ),
            div(
              style = "width: 10%; float: left; padding: 5px",
              icon("helicopter"),
              tags$style(".fa-helicopter {color: green}")
            ),
          ),
          br(), # these are only here because sidebarPanel ignores floating items and I did not find an option to put elements inline otherwise... please fix
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          tippy_this("map_div_act", "Select activities to be plotted on the map")
        ),
        ##### Main panel #####
        mainPanel(
          div(
            tags$style(type = "text/css", "#map {height: calc(100vh - 110px) !important;}"),
            leafletOutput("map")
          )
        )
      ),
      icon = icon("map")
    ),
    #### Data coverage ####
    tabPanel(
      "Data coverage",
      div(
        style = "width: 80%; height: auto; margin: auto",
        plotOutput("mo_timerange_plot"),
        br(),
        dataTableOutput("coverage")
      ),
      icon = icon("table")
    ),
    #### Guide ####
    tabPanel(
      "Guide",
      div(style = "width: 100%",
          img(src = "banner_guide.jpg", style = "width: 100%; margin-top: 0px; margin-left: 0px; margin-right: 0px; margin-bottom: auto")
      ),
      div(style = "width: 80%; margin: auto",
          includeMarkdown("guide.md")
      ),
      icon = icon("lightbulb")
    ),
    #### About page/ Impressum ####
    tabPanel(
      "About",
      div(style = "width: 100%",
          img(src = "banner_about.jpg", style = "width: 100%")
      ),
      div(style = "width: 80%; margin: auto",
          includeMarkdown("about.md")
      ),
      icon = icon("fingerprint")
    )
  )
)
