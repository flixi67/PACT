library(tidyverse)
library(zoo)
library(rlang)
library(ggforce)

data <- read_csv("../../Data/out/final_data/PACT_mission-month_full.csv")

### Element definitions

# define peace-building, security-related and crosscutting tasks
# according to Blair et al 2021, Appendix p. 22

sec_rel <- c("DisarmamentDemobilization",
             "Reintegration",
             "ControlSALW",
             "Demilitarization",
             "ArmsEmbargo",
             "Ceasefire",
             "PeaceProcess",
             "CivilianProtection",
             "Operations_UseOfForce",
             "Operations_PatrolsInterventions")

peace_rel <- c("PoliceReform",
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
               "ElectoralSecurity", # ???
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
               "PowerSharing")

cross_cut <- c("HumanRights",
               "ChildRights",
               "SexualViolence",
               "Gender") # = WomensRights?

# for comparability pact1 and pact2, only use common EC (no Sanction, no special EC)
relevant_ec <- c("Monitor", "Meeting", "Advocate", "Outreach", "Assist", "MaterialSupport", "Implement")

# PKO time order
PKO_order <- data %>%
  group_by(PKO) %>%
  summarise(lower = min(as.yearmon(paste(month, year), "%m %Y")),
            upper = max(as.yearmon(paste(month, year), "%m %Y")),
            Continent = unique(Mission_Continent)) %>%
  arrange(lower) %>%
  .$PKO

# Active missions indices
active_missions_index <- table(data$month_index)

active_missions_yearmon <- data %>%
  group_by(year, month) %>%
  summarise(active = n()) %>%
  .$active

### Plots

base <- ggplot() +
  theme_bw()
  # thematic::thematic_shiny()

# Activities

data_p8 <- data %>%
  group_by(month_index) %>%
  select(month_index, contains("_All")) %>%
  pivot_longer(cols = !month_index,
               names_to = "Activity",
               values_to = "number") %>%
  mutate(Activity = str_remove(Activity, "_All"),
         Activity = case_when(Activity %in% sec_rel ~ "Security-related",
                              Activity %in% peace_rel ~ "Peace-related",
                              Activity %in% cross_cut ~ "Cross-cutting",
                              TRUE ~ Activity)) %>%
  group_by(month_index, Activity) %>%
  summarise(number = sum(number, na.rm = TRUE), .groups = "drop") %>%
  arrange(Activity) %>%
  mutate(perc = number/c(active_missions_index*length(cross_cut),
                         active_missions_index*length(peace_rel),
                         active_missions_index*length(sec_rel)))

plot_p8 <- base +
  geom_smooth(aes(x = month_index, y = perc, group = Activity, colour = Activity, linetype = Activity), color = "black", data = data_p8, se = FALSE) +
  ylab("Share of activities implemented") +
  xlab("Months since mission start")

data_p9 <- data %>%
  group_by(year, month) %>%
  select(year, month, contains("_All")) %>%
  pivot_longer(cols = !c(year, month),
               names_to = "Activity",
               values_to = "number") %>%
  mutate(Activity = str_remove(Activity, "_All"),
         Activity = case_when(Activity %in% sec_rel ~ "Security-related",
                              Activity %in% peace_rel ~ "Peace-related",
                              Activity %in% cross_cut ~ "Cross-cutting",
                              TRUE ~ Activity)) %>%
  group_by(year, month, Activity) %>%
  summarise(number = sum(as.integer(number), na.rm = TRUE), .groups = "drop") %>%
  arrange(Activity) %>%
  mutate(date = as.Date(paste(1, month, year), format = "%d %m %Y"),
         perc = number/c(active_missions_yearmon*length(cross_cut),
                         active_missions_yearmon*length(peace_rel),
                         active_missions_yearmon*length(sec_rel)))

plot_p9 <- base +
  geom_smooth(aes(x = date, y = perc, linetype = Activity, group = Activity), color = "black", se = FALSE, data = data_p9) +
  scale_x_date(limits = c(as.Date("01/01/1989", "%d/%m/%Y"), as.Date("01/01/2019", "%d/%m/%Y"))) +
  ylab("Share of activities implemented") +
  xlab("")

#### EC

replace <- names(data) %>% str_subset("_[A-Z]")

data[, replace] <- data[, replace] %>%
  replace(is.na(.), 0)

data_ec %>%
  group_by(PKO, year, month, month_index) %>%
  summarise(Monitor = !!parse_quo(paste(names(data_ec) %>%
                                          str_subset("Monitor"),
                                        collapse = " + "),
                                  env = caller_env()),
            Meeting = !!parse_quo(paste(names(data_ec) %>%
                                          str_subset("Meeting"),
                                        collapse = " + "),
                                  env = caller_env()),
            Advocate = !!parse_quo(paste(names(data_ec) %>%
                                           str_subset("Advocate"),
                                         collapse = " + "),
                                   env = caller_env()),
            Outreach = !!parse_quo(paste(names(data_ec) %>%
                                           str_subset("Outreach"),
                                         collapse = " + "),
                                   env = caller_env()),
            MaterialSupport = !!parse_quo(paste(names(data_ec) %>%
                                                  str_subset("MaterialSupport"),
                                                collapse = " + "),
                                          env = caller_env()),
            Assist = !!parse_quo(paste(names(data_ec) %>%
                                         str_subset("Assist"),
                                       collapse = " + "),
                                 env = caller_env()),
            Implement = !!parse_quo(paste(names(data_ec) %>%
                                            str_subset("Implement"),
                                          collapse = " + "),
                                    env = caller_env())) %>%
  pivot_longer(cols = !c(PKO, year, month, month_index), names_to = "Engagement category", values_to = "number") %>%
  group_by(PKO, year, month, month_index, `Engagement category`) %>%
  summarise(number = sum(number))

