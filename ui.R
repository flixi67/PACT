library(tidyverse)
library(leaflet)
library(zoo)
library(jsonlite)
library(sf)
library(shiny)
library(bslib)
library(htmltools)
library(tippy)

shinyUI(
  navbarPage(
    theme = bslib::bs_theme(version = 5),
    title = "PACT Interactive Visualization",
    id = "main_menu",
    #### Landing Page #### Can be removed later, when this issue is fixed https://github.com/rstudio/shiny/issues/3604
    tabPanelBody(
      "Landing Page",
      style = "width: 80%; margin: auto",
      h4("Peacekeeping Activities Dataset (PACT): Interactive visualization"),
      p(
        "This tool aims to let researchers create their own insights from the Peacekeeping Activities (PACT) dataset. To do so, you can find a modular plotting tool, links to our basis of data and a map overview of implementation of certain peackeeping activities across the world."
      ),
      h5("Plotting tool"),
      p(
        "For activities and engagement categories, the tool offers plot creation in \"Aggregated\" and \"Mission\" mode. The difference is, that \"Aggregated\" allows for the grouping of missions and multiple activity groups, while \"Mission\" draws a separate plot for each mission selected in the analysis. The latter is recommended for insights into the implementation of specific actities in missions."),
      h6("Aggregated"),
      div(
        tags$ul(
          tags$li(
            "Timeline: Changes the Y-axis to either mission month, where the months within each mission are indexed or by date. Aggregation per mission month allows to compare implemented activies over the lifespan of missions, whereas aggregation by calendar date can give insights into developments over generations of peacekeeping."
          ),
          tags$li(
            "Missions: Create grouping for missions. Different groups will be displayed in separate plots. Make sure each missions is only added to one mission group."
          ),
          tags$li(
            "Activities: Create groups for activity aggregation. The activities in each group will be added up and displayed as a line in the plot."
          )
        )
      ),
      div(
        style = "border: 3px outset black; background-color: #FF6B6B; margin: auto; text-align: center; width: 410px",
        p(
          "In the current version of the tool, the X-axis in the \"Aggregated\" tab shows absolute numbers and not shares. Since the number of missions and their length in months varies, the graphs need careful interpretation, as a direct comparison of lines is not possible."
        )
      ),
      h6("Mission"),
      div(
        tags$ul(
          tags$li(
            "Timeline: Changes the Y-axis to either mission month, where the months within each mission are indexed or by date. Aggregation per mission month allows to compare implemented activies over the lifespan of missions, whereas aggregation by calendar date can give insights into developments over generations of peacekeeping."
          ),
          tags$li(
            "Missions: Missions to include. Each mission will be represented by a seperate facet."
          ),
          tags$li(
            "Activities: The activities to consider. The line in the graph will show the share of activities implemented, if multiple are selected."
          )
        )
      ),
      h6("Example: What comes first? Security- vs. peacebuilding-related tasks"),
      p(
        "A long-standing debate in peacekeeping research is, when and if security-related tasks are implemented first. The priorization of security-related tasks such as civilian protection, demilitarization, the enforcement of arms embargos or even military operations would hint at a sequentation within missions and therefore also have implications for necessary equipment and operational demands."
      ),
      p(
        "To get a first overview, we use the 'Aggregated' mode to depict the implementation of peacebuilding, security-related and cross-cutting tasks (<a href=\"https://onlinelibrary.wiley.com/doi/full/10.1111/ajps.12650\" target=\"_blank\">Blair et. al (2020)</a>). For the timeline, we choose 'Mission month', as we want to see developments within the missions."
      ),
      img(
        src= "example1.png", align = "center", width = "100%"
      ),
      p(
        "We can see that across all missions, security-related tasks (Activities 1) implementation starts slightly faster. After less than a year, the implemented tasks reach their maximum and sligthly decline in until the 4 year mark. For the peacebuilding activities (Activities 2), the implementation is more dynamic. After a bit more than a year, most tasks are implemented with a comparably stronger decline until the 4 year mark compared to security tasks. It is important to note that at this point, the graphs show absolute numbers and not shares, which is problematic when activity groups have different sizes. In this example, there are 18 peacebuilding tasks and only 10 security-related tasks. Therefore, the interpretation needs to be careful as only the progression of the line can be compared to each other. Also, some missions ends earlier than others, limiting the comparability on the Y-axis."
      ),
      p(
        "To get a better overview, we add mission groups and plot three stabilization missions, a mission type with high military component often deployed in unstable theatres of operation, against a comparison group of three other missions of equal length in Africa. The results show that overall the differences are not that significant, but that stabilization missions implement more of the tasks overall (higher multidimensionality) and that the implementation of security-related tasks is slightly higher and endures longer over the course of the mission."
      ),
      img(
        src= "example2.png", align = "center", width = "100%"
      ),
      br(),
      br(),
      br()
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
                                 "Timerange" = "timerange"),
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
                )
              ),
              ###### Per mission ######
              tabPanel(
                "Mission",
                value = 2,
                hr(),
                h6("Missions"),
                radioButtons(
                  "act_select_mission",
                  label = NULL,
                  choices = list("All" = "all",
                                 "Selected" = "select"),
                  inline = TRUE
                ),
                tippy_this("act_select_mission",
                           "Missions to display as facets in the plot"),
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
          sidebarPanel(tabsetPanel(
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
                               "Timerange" = "timerange"),
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
              )
            ),
            ###### Per mission ######
            tabPanel(
              "Mission",
              value = 2,
              hr(),
              h6("Missions"),
              radioButtons(
                "ec_select_mission",
                label = NULL,
                choices = list("All" = "all",
                               "Selected" = "select"),
                inline = TRUE
              ),
              tippy_this("ec_select_mission",
                         "Missions to display as facets in the plot"),
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
              )
            )
          )),
          ##### Main panel #####
          mainPanel(
            conditionalPanel("input.type_ec == 1",
                             plotOutput("ec_agg_plot")),
            conditionalPanel("input.type_ec == 2",
                             plotOutput("ec_mission_plot"))
          )
        )
      )
    ),
    navbarMenu(
      "Mission overview",
      #### Missions: Data Coverage ####
      tabPanel(
        "Data coverage",
        div(
          style = "width: 50%; margin: auto",
          dataTableOutput("coverage")
        )
      ),
      #### Missions: Activity map ####
      tabPanel(
        "Activity map",
        div(
          class = "outer",
          tags$style(type = "text/css", "#map {height: calc(100vh - 110px) !important;}"),
          leafletOutput("map")
        ),
        absolutePanel(
          class = "panel panel-default",
          top = 70,
          left = 85,
          width = 320,
          height = "auto",
          fixed = TRUE,
          style = "padding: 14px; background:rgba(232, 232, 232, 0.8); bottom:25px",
          div(
            style = "margin-bottom:15px;",
            h6("Activity map"),
            p(
              "This map helps to get an overview of the spatial and temporal dimension of UN peacekeeping activities. It shows where missions were employed in each year and which activities they implemented."
            ),
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
            ),
            tags$style(
              type = "text/css",
              HTML(".irs-single {color:black; background:transparent}")
            ),
            tags$style(type = "text/css", HTML(".irs-grid-text {color:#333333}"))
          ),
          tippy_this(
            "map_div_time",
            "Select year to show in the map. Play button on the bottom right animates the map over time"
          ),
          div(
            id = "map_div_act",
            checkboxInput("map_show_active", "Show active missions"),
            div(
              style = "width: 90%; float: left",
              selectizeInput(
                "map_activity1",
                label = NULL,
                choices = activity_list,
                multiple = TRUE,
                options = list(placeholder = "Select activity",
                               maxItems = 1)
              )
            ),
            div(
              style = "width: 10%; float: left; padding: 5px",
              icon("hands-helping"),
              tags$style(".fa-hands-helping {color: blue}")
            ),
            div(
              style = "width: 90%; float: left",
              selectizeInput(
                "map_activity2",
                label = NULL,
                choices = activity_list,
                multiple = TRUE,
                options = list(placeholder = "Select activity",
                               maxItems = 1)
              ),
            ),
            div(
              style = "width: 10%; float: left; padding: 5px",
              icon("people-carry"),
              tags$style(".fa-people-carry {color: red}")
            ),
            div(
              style = "width: 90%; float: left",
              selectizeInput(
                "map_activity3",
                label = NULL,
                choices = activity_list,
                multiple = TRUE,
                options = list(placeholder = "Select activity",
                               maxItems = 1)
              )
            ),
            div(
              style = "width: 10%; float: left; padding: 5px",
              icon("helicopter"),
              tags$style(".fa-helicopter {color: green}")
            )
          ),
          tippy_this("map_div_act", "Select activities to be plotted on the map"),
          div(
            style = "color:#888888; float: left",
            br(),
            br(),
            br(),
            p(
              "If this input panel does not show correctly, try zooming in or out in the browser window."
            )
          )
        )
      )
    ),
    #### About page/ Impressum ####
    tabPanel(
      "About",
      div(style = "width: 80%; margin: auto",
          includeMarkdown("about.md"))
    )
  )
)
