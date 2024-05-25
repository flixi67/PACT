library(tidyverse)
library(leaflet)
library(zoo)
library(jsonlite)
library(sf)
library(shiny)
library(rlang)
library(bslib)
library(htmltools)
library(tippy)
library(ggforce)

source("R/utils.R")

Sys.setlocale("LC_TIME", "English")

### read in data
data <- read_csv("data/PACT_mission-month_full.csv")
reportdata <- read_rds("data/transformed-paragraphs.Rds")

### create datasets for application
data_all <- data %>%
  select(PKO, month_index, month, year, contains("_All"))

data_ec <- data %>%
  select(
    -contains("_IA"),
    -contains("Abuse"),
    -contains("Infra"),-contains("AssistPolicies"),
    -contains("AssistAgents"),-contains("AssistOther"),
    -contains("AssistComm")
  ) %>%
  select(
    PKO,
    month_index,
    month,
    year,
    contains("_Monitor"),
    contains("_MaterialSupport"),
    contains("_Meeting"),
    contains("_Advocate"),
    contains("_Outreach"),
    contains("_Implement"),
    contains("_Assist")
  )

replace <- names(data_ec) %>% str_subset("_[A-Z]")

data_ec[, replace] <- data_ec[, replace] %>%
  replace(is.na(.), 0)

data_mo <- data %>%
  group_by(PKO) %>%
  summarise(lower = min(as.yearmon(paste(month, year), "%m %Y")),
            upper = max(as.yearmon(paste(month, year), "%m %Y")),
            Continent = unique(Mission_Continent))

### create choicelists, selectors, ...
mission_list <- as.list(unique(data$PKO) %>%
                          set_names(unique(data$PKO)))

mission_data <- data %>%
  group_by(PKO) %>%
  summarise(
    dur = n(),
    start = as.character(min(as.yearmon(
      paste(month, year), "%m %Y"
    ))),
    end = as.character(max(as.yearmon(
      paste(month, year), "%m %Y"
    ))),
    country = first(Mission_Country),
    continent = first(Mission_Continent),
    continent_i = case_when(
      continent == "South America"  ~ "americas",
      TRUE ~ tolower(continent)
    )
  ) %>%
  left_join(read_rds("data/PKO_start_end.Rds"), by = c("PKO" = "Acronym")) %>%
  rename("name" = "Mission name")
mission_data$name[mission_data$PKO == "UNAVEM"] <-
  "United Nations Angola Verification Mission"
mission_data$name[mission_data$PKO == "MONUC"] <-
  "United Nations Organization Mission in the Democratic Republic of the Congo"
mission_data$name[mission_data$PKO == "MONUSCO"] <-
  "United Nations Stabilization Mission in the Democratic Republic of the Congo"
mission_data$name[mission_data$PKO == "MINUSCA"] <-
  "United Nations Multidimensional Integrated Stabilization Mission in the Central African Republic"
mission_data$name[mission_data$PKO == "MINUSMA"] <-
  "United Nations Multidimensional Integrated Stabilization Mission in Mali"

activity_list <- as.list(
  names(data) %>%
    str_subset("_All") %>%
    str_remove("_All") %>%
    set_names(
      names(data) %>%
        str_subset("_All") %>%
        str_remove("_All") %>%
        str_replace("_", ":") %>%
        str_replace_all("(?=[A-Z])", " ") %>%
        str_trim()
    )
)

ec_list <- list(
  "Monitor",
  "Meeting",
  "Advocate",
  "Outreach",
  "Assist",
  "MaterialSupport",
  "Implement"
) %>%
  set_names(
    c(
      "Monitor",
      "Meeting",
      "Advocate",
      "Outreach",
      "Assist",
      "Material Support",
      "Implement"
    )
  )

blair_cat <-
  list(
    "Security-related" = c(
      "DisarmamentDemobilization",
      "Reintegration",
      "ControlSALW",
      "Demilitarization",
      "ArmsEmbargo",
      "Ceasefire",
      "PeaceProcess",
      "CivilianProtection",
      "Operations_UseOfForce",
      "Operations_PatrolsInterventions"
    ),
    "Peacbuilding-related" = c(
      "PoliceReform",
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
      "PowerSharing"
    ),
    "Cross-cutting" = c("HumanRights",
                        "ChildRights",
                        "SexualViolence",
                        "Gender")
  )

### create mapdata and icons
shapes <- st_read("data/countryborders.json") %>%
  select(name_long, geometry) %>%
  mutate(
    name_long = recode(
      name_long,
      "Côte d'Ivoire" = "Cote d Ivoire",
      "Democratic Republic of the Congo" = "DR Congo",
    )
  )
data_map <- left_join(
  data %>%
    select(PKO, Mission_Country, month, year, contains("_All")),
  shapes,
  by = c("Mission_Country" = "name_long")
) %>%
  mutate(points = st_centroid(geometry))
names(data_map) <- names(data_map) %>% str_remove("_All")

map_icons <- iconList(
  "bluehand" = makeIcon(
    iconUrl = "data/icons/handshake-angle.svg",
    iconWidth = 20,
    iconHeight = 20,
    iconAnchorX = 20,
    iconAnchorY = 20
  ),
  "redhelp" = makeIcon(
    iconUrl = "data/icons/people-carry-box.svg",
    iconWidth = 20,
    iconHeight = 20,
    iconAnchorX = 20
  ),
  "greenheli" = makeIcon(
    iconUrl = "data/icons/helicopter.svg",
    iconWidth = 20,
    iconHeight = 20,
    iconAnchorX = 1,
    iconAnchorY = 10
  )
)

### Server

shinyServer(function(input, output, session) {
  #### Modular plot inputs ####
  ##### Groups of missions (peackeeping activities) #####
  act_inserted_mission <- c()

  observeEvent(input$act_insert_mission, {
    btn <- input$act_insert_mission
    id <- paste0('act_mission', btn)
    insertUI(selector = '#act_placeholder_mission',
             ui = tags$div(
               selectizeInput(
                 id,
                 label = NULL,
                 choices = "",
                 multiple = TRUE,
                 options = list(
                   valueField = "PKO",
                   labelField = "PKO",
                   searchField = c("PKO", "name"),
                   create = FALSE,
                   placeholder = "Select missions to aggregate",
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
               ),
               id = id
             ))
    act_inserted_mission <<- c(act_inserted_mission, id)
  })

  observeEvent(input$act_remove_mission, {
    removeUI(selector = paste0('#', act_inserted_mission[length(act_inserted_mission)]))
    act_inserted_mission <<-
      act_inserted_mission[-length(act_inserted_mission)]
  })

  ##### Groups of activities (peacekeeping activities) #####
  act_inserted_act <- c()

  observeEvent(input$act_insert_act, {
    btn <- input$act_insert_act
    id <- paste0('act_act', btn)
    insertUI(selector = '#act_placeholder_act',
             ui = tags$div(
               selectizeInput(
                 id,
                 label = NULL,
                 choices = activity_list,
                 multiple = TRUE,
                 options = list(placeholder = "Select activities to aggregate")
               ),
               id = id
             ))
    act_inserted_act <<- c(act_inserted_act, id)
  })

  observeEvent(input$act_remove_act, {
    removeUI(selector = paste0('#', act_inserted_act[length(act_inserted_act)]))
    act_inserted_act <<- act_inserted_act[-length(act_inserted_act)]
  })

  ##### Groups of missions (engagement categories) #####
  ec_inserted_mission <- c()

  observeEvent(input$ec_insert_mission, {
    btn <- input$ec_insert_mission
    id <- paste0('ec_mission', btn)
    insertUI(selector = '#ec_placeholder_mission',
             ui = tags$div(
               selectizeInput(
                 id,
                 label = NULL,
                 choices = "",
                 multiple = TRUE,
                 options = list(
                   valueField = "PKO",
                   labelField = "PKO",
                   searchField = c("PKO", "name"),
                   create = FALSE,
                   placeholder = "Select missions to aggregate",
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
               ),
               id = id
             ))
    ec_inserted_mission <<- c(ec_inserted_mission, id)
  })

  observeEvent(input$ec_remove_mission, {
    removeUI(selector = paste0('#', ec_inserted_mission[length(ec_inserted_mission)]))
    ec_inserted_mission <<-
      ec_inserted_mission[-length(ec_inserted_mission)]
  })

  ##### Groups of activities (engagement categories) #####
  ec_inserted_act <- c()

  observeEvent(input$ec_insert_act, {
    btn <- input$ec_insert_act
    id <- paste0('ec_act', btn)
    insertUI(selector = '#ec_placeholder_act',
             ui = tags$div(
               selectizeInput(
                 id,
                 label = NULL,
                 choices = activity_list,
                 multiple = TRUE,
                 options = list(placeholder = "Select activities to aggregate")
               ),
               id = id
             ))
    ec_inserted_act <<- c(ec_inserted_act, id)
  })

  observeEvent(input$ec_remove_act, {
    removeUI(selector = paste0('#', ec_inserted_act[length(ec_inserted_act)]))
    ec_inserted_act <<- ec_inserted_act[-length(ec_inserted_act)]
  })

  #### Plot data ####
  ##### Peaceeping Activities (Aggregated) #####
  observeEvent(c(input$act_draw_plot1, input$act_select_time), {
    mission_grp <- if (length(act_inserted_mission) == 0) {
      mission_list %>% unlist() %>% list() %>% set_names("All missions")
    } else {
      isolate({
        mission_grp <- list()
        for (a in act_inserted_mission) {
          mission_grp[[a]] <- input[[a]]
        }
        mission_grp
      })
    }

    activity_grp <- if (length(act_inserted_act) == 0) {
      blair_cat
    } else {
      isolate({
        activity_grp <- list()
        for (a in act_inserted_act) {
          activity_grp[[a]] <- input[[a]]
        }
        activity_grp
      })
    }

    act_agg_data <- data_all %>%
      pivot_longer(
        cols = !c(PKO, month_index, month, year),
        names_to = "Activity",
        values_to = "number"
      ) %>%
      mutate(
        Activity = str_remove(Activity, "_All"),
        Activity = input_aggregate_a(Activity, activity_grp),
        PKO = input_aggregate_m(PKO, mission_grp)
      ) %>%
      filter(!is.na(Activity),!is.na(PKO)) %>%
      when(
        input$act_select_time == "mission_month"
        ~ group_by(., PKO, month_index, Activity),
        ~ .
      ) %>%
      when(
        input$act_select_time == "timerange"
        ~ filter(
          .,
          year >= input$act_select_time2[1] &
            year <= input$act_select_time2[2]
        ) %>%
          mutate(date = as.Date(paste(1, month, year), format = "%d %m %Y")) %>%
          group_by(PKO, date, Activity),
        ~ .
      ) %>%
      summarise(number = sum(number, na.rm = TRUE))

    if (input$act_smooth1 == FALSE &
        input$act_select_time == "mission_month") {
      act_agg_plot <- ggplot(data = act_agg_data) +
        geom_line(
          aes_string(
            x = "month_index",
            y = "number",
            group = "Activity",
            color = input$act_color1,
            linetype = "Activity"
          ),
          size = 1
        ) +
        facet_wrap( ~ PKO, scales = "free_x") +
        labs(x = "Months since mission start", y = "Number of implemented activities")
    } else if (input$act_smooth1 == TRUE &
               input$act_select_time == "mission_month") {
      act_agg_plot <- ggplot(data = act_agg_data %>%
                               mutate(
                                 month_index = as.numeric(month_index),
                                 number = as.numeric(number)
                               )) +
        geom_smooth(
          aes_string(
            x = "month_index",
            y = "number",
            group = "Activity",
            color = input$act_color1,
            linetype = "Activity"
          ),
          se = FALSE,
          size = 1
        ) +
        facet_wrap( ~ PKO, scales = "free_x") +
        labs(x = "Months since mission start", y = "Number of implemented activities")
    } else if (input$act_smooth1 == FALSE &
               input$act_select_time == "timerange") {
      act_agg_plot <- ggplot(data = act_agg_data) +
        geom_line(
          aes_string(
            x = "date",
            y = "number",
            group = "Activity",
            color = input$act_color1,
            linetype = "Activity"
          ),
          size = 1
        ) +
        facet_wrap( ~ PKO, scales = "free_x") +
        labs(x = "", y = "Number of implemented activities")
    } else if (input$act_smooth1 == TRUE &
               input$act_select_time == "timerange") {
      act_agg_plot <- ggplot(data = act_agg_data %>%
                               mutate(number = as.numeric(number))) +
        geom_smooth(
          aes_string(
            x = "date",
            y = "number",
            group = "Activity",
            color = input$act_color1,
            linetype = "Activity"
          ),
          se = FALSE,
          size = 1
        ) +
        facet_wrap( ~ PKO, scales = "free_x") +
        labs(x = "", y = "Number of implemented activities")
    }

    output$act_agg_plot <- renderPlot(act_agg_plot)

  })

  ##### Peaceeping Activities (Per mission) #####
  observeEvent(input$act_draw_plot2, {
    mission_sel <- isolate(input$act_select_missions)

    activity_sel <- if (is.null(input$act_select_act)) {
      unlist(activity_list)
    } else {
      isolate(input$act_select_act)
    }

    act_mission_data <- data_all %>%
      select(-month,-year) %>%
      pivot_longer(
        cols = !c(PKO, month_index),
        names_to = "Activity",
        values_to = "number"
      ) %>%
      mutate(Activity = str_remove(Activity, "_All")) %>%
      filter(Activity %in% activity_sel & PKO %in% mission_sel) %>%
      group_by(PKO, month_index) %>%
      summarise(number = sum(number, na.rm = TRUE),
                perc = number / length(activity_sel))

    output$act_mission_plot <- renderPlot({
      if (input$act_smooth2 == FALSE) {
        ggplot(data = act_mission_data) +
          geom_line(aes(
            x = month_index,
            y = perc,
            group = PKO
          ),
          size = 1) +
          facet_wrap( ~ PKO, scales = "free_x") +
          facet_wrap( ~ PKO, scales = "free_x") +
          labs(x = "Months since mission start", y = "Share of implemented activities")
      } else if (input$act_smooth2 == TRUE) {
        ggplot(data = act_mission_data) +
          geom_smooth(aes(
            x = month_index,
            y = perc,
            group = PKO
          ),
          se = FALSE,
          size = 1) +
          facet_wrap( ~ PKO, scales = "free_x") +
          labs(x = "Months since mission start", y = "Share of implemented activities")
      }
    })
  })

  ##### Engagement Categories (Aggregated) #####
  observeEvent(c(input$ec_draw_plot1, input$ec_select_time), {
    mission_grp <- if (length(ec_inserted_mission) == 0) {
      mission_list %>% unlist() %>% list() %>% set_names("All missions")
    } else {
      isolate({
        mission_grp <- list()
        for (a in ec_inserted_mission) {
          mission_grp[[a]] <- input[[a]]
        }
        mission_grp
      })
    }

    activity_grp <- if (length(ec_inserted_act) == 0) {
      blair_cat
    } else {
      isolate({
        activity_grp <- list()
        for (a in ec_inserted_act) {
          activity_grp[[a]] <- input[[a]]
        }
        activity_grp
      })
    }

    ec_agg_data <- data_ec %>%
      group_by(PKO, year, month, month_index) %>%
      summarise(
        Monitor = !!parse_quo(paste(
          names(data_ec) %>%
            str_subset("Monitor"),
          collapse = " + "
        ),
        env = caller_env()),
        Meeting = !!parse_quo(paste(
          names(data_ec) %>%
            str_subset("Meeting"),
          collapse = " + "
        ),
        env = caller_env()),
        Advocate = !!parse_quo(paste(
          names(data_ec) %>%
            str_subset("Advocate"),
          collapse = " + "
        ),
        env = caller_env()),
        Outreach = !!parse_quo(paste(
          names(data_ec) %>%
            str_subset("Outreach"),
          collapse = " + "
        ),
        env = caller_env()),
        MaterialSupport = !!parse_quo(paste(
          names(data_ec) %>%
            str_subset("MaterialSupport"),
          collapse = " + "
        ),
        env = caller_env()),
        Assist = !!parse_quo(paste(
          names(data_ec) %>%
            str_subset("Assist"),
          collapse = " + "
        ),
        env = caller_env()),
        Implement = !!parse_quo(paste(
          names(data_ec) %>%
            str_subset("Implement"),
          collapse = " + "
        ),
        env = caller_env())
      ) %>%
      mutate(PKO = input_aggregate_m(PKO, mission_grp)) %>%
      filter(!is.na(PKO)) %>%
      pivot_longer(
        cols = !c(PKO, year, month, month_index),
        names_to = "Engagement category",
        values_to = "number"
      ) %>%
      when(
        input$ec_select_time == "mission_month"
        ~ group_by(., PKO, month_index, `Engagement category`),
        ~ .
      ) %>%
      when(
        input$ec_select_time == "timerange"
        ~ filter(
          .,
          year >= input$ec_select_time2[1] &
            year <= input$ec_select_time2[2]
        ) %>%
          mutate(date = as.Date(paste(1, month, year), format = "%d %m %Y")) %>%
          group_by(PKO, date, `Engagement category`),
        ~ .
      ) %>%
      when(
        !is.null(input$ec_select_ec)
        ~ filter(., `Engagement category` %in% input$ec_select_ec),
        ~ .
      ) %>%
      summarise(number = sum(number))

    output$ec_agg_plot <- renderPlot({
      if (input$ec_smooth1 == FALSE &
          input$ec_select_time == "mission_month") {
        ggplot(data = ec_agg_data) +
          geom_line(
            aes_string(
              x = "month_index",
              y = "number",
              group = "`Engagement category`",
              color = input$ec_color1,
              linetype = "`Engagement category`"
            ),
            size = 1
          ) +
          facet_wrap( ~ PKO, scales = "free_x") +
          labs(x = "Months since mission start", y = "Number of implemented activities")
      } else if (input$ec_smooth1 == TRUE &
                 input$ec_select_time == "mission_month") {
        ggplot(data = ec_agg_data %>%
                 mutate(
                   month_index = as.numeric(month_index),
                   number = as.numeric(number)
                 )) +
          geom_smooth(
            aes_string(
              x = "month_index",
              y = "number",
              group = "`Engagement category`",
              color = input$ec_color1,
              linetype = "`Engagement category`"
            ),
            se = FALSE,
            size = 1
          ) +
          facet_wrap( ~ PKO, scales = "free_x") +
          labs(x = "Months since mission start", y = "Number of implemented activities")
      } else if (input$ec_smooth1 == FALSE &
                 input$ec_select_time == "timerange") {
        ggplot(data = ec_agg_data) +
          geom_line(
            aes_string(
              x = "date",
              y = "number",
              group = "`Engagement category`",
              color = input$ec_color1,
              linetype = "`Engagement category`"
            ),
            size = 1
          ) +
          facet_wrap( ~ PKO, scales = "free_x") +
          labs(x = "", y = "Number of implemented activities")
      } else if (input$ec_smooth1 == TRUE &
                 input$ec_select_time == "timerange") {
        ggplot(data = ec_agg_data %>%
                 mutate(number = as.numeric(number))) +
          geom_smooth(
            aes_string(
              x = "date",
              y = "number",
              group = "`Engagement category`",
              color = input$ec_color1,
              linetype = "`Engagement category`"
            ),
            se = FALSE,
            size = 1
          ) +
          facet_wrap( ~ PKO, scales = "free_x") +
          labs(x = "", y = "Number of implemented activities")
      }
    })
  })

  ##### Engagement Categories (Per mission) #####
  observeEvent(input$ec_draw_plot2, {
    mission_sel <- isolate(input$ec_select_missions)

    activity_sel <- if (is.null(input$ec_select_act)) {
      unlist(activity_list)
    } else {
      isolate(input$ec_select_act)
    }

    ec_mission_data <- data_ec %>%
      select(-month,-year) %>%
      pivot_longer(
        cols = !c(PKO, month_index),
        names_to = "Activity",
        values_to = "number"
      ) %>%
      separate(
        Activity,
        c("Activity", "Engagement category"),
        sep = "_(?![PU])",
        extra = "merge"
      ) %>%
      filter(
        Activity %in% activity_sel &
          PKO %in% mission_sel &
          `Engagement category` %in% input$ec_select_ec2
      ) %>%
      group_by(PKO, month_index, `Engagement category`) %>%
      summarise(number = sum(number, na.rm = TRUE),
                perc = number / length(activity_sel))

    output$ec_mission_plot <- renderPlot({
      if (input$ec_smooth2 == FALSE) {
        ggplot(data = ec_mission_data) +
          geom_line(
            aes_string(
              x = "month_index",
              y = "perc",
              group = "`Engagement category`",
              color = input$ec_color2,
              linetype = "`Engagement category`"
            ),
            size = 1
          ) +
          facet_wrap( ~ PKO, scales = "free_x") +
          labs(x = "Months since mission start", y = "Share of implemented activities")
      } else if (input$ec_smooth2 == TRUE) {
        ggplot(data = ec_mission_data) +
          geom_smooth(
            aes_string(
              x = "month_index",
              y = "perc",
              group = "`Engagement category`",
              color = input$ec_color2,
              linetype = "`Engagement category`"
            ),
            se = FALSE,
            size = 1
          ) +
          facet_wrap( ~ PKO, scales = "free_x") +
          labs(x = "Months since mission start", y = "Share of implemented activities")
      }
    })
  })


  #### Data coverage ####
  output$mo_timerange_plot <- renderPlot({
    ggplot(data = data_mo) +
      geom_linerange(aes(xmin = lower, xmax = upper, y = reorder(PKO, lower)),
                     size = 2) +
      scale_x_continuous(n.breaks = 10) +
      facet_col(~ Continent, scales = "free_y", space = "free") +
      ylab("")
  })

  output$coverage <- renderDataTable({
    reportdata %>%
      select(PKO = report_namePKO, reportNumber, reportDate, reportPeriod_start, reportPeriod_end, numberParagraphs) %>%
      unique() %>%
      mutate(reportNumber = paste0("<a href=\"https://undocs.org/en/", reportNumber, "\">", reportNumber, "</a>"),
             PKO = str_extract(PKO, "[A-Z]*"))
  },
  escape = FALSE)

  #### Activity Map ####
  data_map_reactive <- reactive({
    data_map %>%
      filter(year == input$map_select_time)
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -40, lat = 20, zoom = 2) %>%
      addTiles()
  })

  observe({
    leafletProxy("map", data = data_map_reactive()) %>%
      clearShapes() %>%
      when(
        input$map_show_active == TRUE
        ~ addPolygons(
          .,
          data = data_map_reactive() %>% select(PKO, year, Mission_Country, geometry) %>% unique() %>% .$geometry,
          label = data_map_reactive() %>% select(PKO, year) %>% unique() %>% .$PKO,
          color = "red",
          opacity = 0.1,
          stroke = FALSE
        ),
        ~ .
      )
  })

  observe({
    leafletProxy("map", data = data_map_reactive()) %>%
      clearMarkers() %>%
      when(
        !is.null(input$map_activity1)
        ~ addMarkers(
          .,
          data = data_map_reactive() %>% filter(
            !!sym(input$map_activity1) == TRUE
          ) %>% .$points,
          icon = map_icons$bluehand
        ),
        ~ .
      ) %>%
      when(
        !is.null(input$map_activity2)
        ~ addMarkers(
          .,
          data = data_map_reactive() %>% filter(
            !!sym(input$map_activity2) == TRUE
          ) %>% .$points,
          icon = map_icons$redhelp
        ),
        ~ .
      ) %>%
      when(
        !is.null(input$map_activity3)
        ~ addMarkers(
          .,
          data = data_map_reactive() %>% filter(
            !!sym(input$map_activity3) == TRUE
          ) %>% .$points,
          icon = map_icons$greenheli
        ),
        ~ .
      )
  })

})
