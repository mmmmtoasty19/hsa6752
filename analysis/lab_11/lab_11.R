


# ---- load-packages -----------------------------------------------------------
library(magrittr) # pipes
library(dplyr)    # wrangling
library(ggplot2)  # graphing
library(tidyr)    # pivoting
library(stringr)  # strings
library(maps)     # map data

# ---- load-sources ------------------------------------------------------------

source("./scripts/helper_functions.R")


# ---- load-data ---------------------------------------------------------------

# readxl::excel_sheets("./data_public/raw/Tableau 10 Training Practice Data.xlsx")


ds_drug_raw <- readxl::read_excel(
                          "./data_public/raw/Tableau 10 Training Practice Data.xlsx"
                          ,sheet = "17 - Drug Overdose Deaths" 
                        )
ds_motor_raw <- readxl::read_excel(
                          "./data_public/raw/Tableau 10 Training Practice Data.xlsx"
                          ,sheet = "18 - Motor Vehicle Deaths" 
                        )

# ---- inspect-data ------------------------------------------------------------

ds_drug_raw %>% glimpse()
ds_motor_raw %>% glimpse()

# ---- tweak-data --------------------------------------------------------------

#clean col names of raw data to make snake case
colnames(ds_drug_raw) <- sapply(colnames(ds_drug_raw), clean_names) 
colnames(ds_motor_raw) <- sapply(colnames(ds_motor_raw), clean_names) 

ds_drug_1 <- ds_drug_raw %>% 
  mutate_at(vars(drug_overdose_deaths), na_if, "Suppressed") %>% 
  mutate(
    year = lubridate::year(report_date)
    ,drug_overdose_deaths = as.numeric(drug_overdose_deaths)  
  ) %>% 
  group_by(
    state
    ,year
  ) %>% 
  summarise(
    drug_overdose_deaths = sum(drug_overdose_deaths, na.rm = TRUE)
  ) %>% 
  ungroup()


ds_motor_1 <- ds_motor_raw %>% 
  mutate(
    year = lubridate::year(date)
  ) %>% 
  select(-date)
  
ds_combine <- ds_drug_1 %>% 
  left_join(ds_motor_1
            ,by = c(
                "state" = "state_name"
                ,"year" = "year"
                )
            ) %>%
  mutate(
    drug_overdose_death_rate = (drug_overdose_deaths/population)*100000
    ,motor_death_rate        = (motor_vehicle_incident_deaths/population)*100000
    ,year = as.integer(year)
  )
  


# ---- deaths-timeline ---------------------------------------------------------

g1 <- ds_combine %>%
  group_by(year) %>% 
  summarise(
    drug_overdose_deaths = sum(drug_overdose_deaths)
    ,motor_vehicle_incident_deaths = sum(motor_vehicle_incident_deaths)
  ) %>% 
  pivot_longer(
    cols  = drug_overdose_deaths:motor_vehicle_incident_deaths
    ,names_to = "death_type"
  ) %>% 
  # save for later, but not needed for this graph
  # mutate(
  #   label  = if_else(year == min(year), as.character(death_type), NA_character_)
  #   ,label = snakecase::to_title_case(label)
  # ) %>% 
  ggplot(aes(x = year, y = value , group = death_type)) +
  geom_line(aes(color = death_type), show.legend = FALSE) +
  # geom_text(aes(label = label), na.rm = TRUE, hjust = -0.1) +  
  # ggrepel::geom_text_repel(aes(label = label), na.rm = TRUE) +
  annotate("text", x = 2007.5, y = 30000, label = "Drug Overdose Deaths") +
  annotate("text", x = 2007.5, y = 48000, label =  "Motor Vehicle Deaths") +
  scale_x_continuous(breaks = seq(2006,2015,1)) +
  scale_y_continuous(breaks = seq(0,60000,10000), limits = c(0,60000)) +
  labs(
    title = "Total Deaths from Drug Overdoses and Moter Vehicle Incidents"
    ,x    = NULL
    ,y    = "Number of Deaths"
  ) +
  theme_bw()
g1


# ---- 2015-death-rates --------------------------------------------------------

states_map <- map_data("state")

deaths_map <- ds_combine %>% 
  filter(year == 2015) %>% 
  mutate(state = tolower(state)) %>% 
  select(state,drug_overdose_death_rate, motor_death_rate) %>% 
  merge(states_map, by.x = "state", by.y = "region")

#testing maps

# g2 <- deaths_map %>% 
#   ggplot(aes(x = long, y = lat, group = group, fill = drug_overdose_death_rate)) +
#   geom_polygon(colour = "black") +
#   coord_map() +
#   theme(legend.position = "bottom")
# g2


deaths_map_long <- deaths_map %>% 
  pivot_longer(
    drug_overdose_death_rate:motor_death_rate
    ,names_to = "death_type"
  ) %>% 
  mutate(
    death_type = factor(
      death_type
      ,levels = c("motor_death_rate","drug_overdose_death_rate" )
      ,labels = c("Motor Vehicle Deaths", "Drug Overdose Deaths")
      )
    )
  
g3 <- deaths_map_long %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = value)) +
  geom_polygon(colour = "black") +
  coord_map() +
  scale_fill_gradient(
    low   = "#ece2f0"
    ,high = "#1c9099"
  ) +
  facet_wrap(~death_type) +
  theme_bw() +
  theme(axis.title.x = element_blank()
        ,axis.title.y = element_blank()
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,legend.position = "bottom"
        ,legend.title = element_blank())
g3
