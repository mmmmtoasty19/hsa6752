#These first few lines run only when the file is run in RStudio,
# !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages --------------------------------------------------
# Attach these packages so their functions 
#don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(dplyr)    # data wrangling
library(ggplot2)  # graphs
library(tidyr)    # data tidying
library(maps)     # map data

# ---- load-sources ---------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.
source("./scripts/helper_functions.R") # used in multiple reports


# ---- declare-globals ------------------------------------------------

# ---- load-data ------------------------------------------------------

readxl::excel_sheets("./data_public/raw/Tableau 10 Training Practice Data.xlsx")

ds_cancer_raw <-  readxl::read_xlsx(
                            "./data_public/raw/Tableau 10 Training Practice Data.xlsx"
                            ,sheet = "13 - Cancer Deaths by State" 
                          )

ds_per_capita_raw <- readxl::read_xlsx(
  "data_public/raw/us_per_capita.xlsx"
)

colnames(ds_cancer_raw) <- sapply(colnames(ds_cancer_raw), clean_names)

states_map <- map_data('state')


# ---- inspect-data ---------------------------------------------------

# ---- tweak-data -----------------------------------------------------

cancer_map <- ds_cancer_raw %>% 
  select(state,crude_death_rate) %>% 
  mutate(
    state = tolower(state)
  ) %>% 
  merge(states_map, by.x = "state", by.y = "region")

# ---- cancer-map -----------------------------------------------------

g1 <- cancer_map %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = crude_death_rate)) +
  geom_polygon(colour = "black") +
  coord_map() +
  theme_void() +
  annotate("text", x = -111.5, y = 40, label = "102", fontface = 2, size = 4) +
  annotate("text", x = -80.5, y = 39, label = "254", fontface = 2, size = 4) +
  theme(
    legend.position = "bottom"
  ) +
  scale_fill_gradient2(
    low       = "#deebf7"
    ,mid      = "#9ecae1"
    ,high     = "#3182bd"
    ,midpoint = mean(cancer_map$crude_death_rate)
  )

g1

# ---- symbol-map -----------------------------------------------------


state_centers <- data.frame(state.name,state.center) %>% 
  mutate(
    state = tolower(state.name)
  ) %>% 
  select(-state.name)

ds_per_capita1 <-  ds_per_capita_raw %>% 
  mutate(
    state = tolower(state)
  )
  

cancer_map_centers <-  cancer_map %>% 
  inner_join(state_centers) %>% 
  inner_join(ds_per_capita1) 

#remove duplicate numbers
cancer_map_centers$crude_death_rate[duplicated(cancer_map_centers$crude_death_rate)] <- NA


g2 <- cancer_map_centers %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = per_capita_income), color = "black", show.legend = FALSE) +
  coord_map() +
  geom_point(
    aes(x = x, y = y, size = crude_death_rate)
    ,fill   = "#9ecae1"
    ,color  = "black"
    ,shape  = 21
    ,alpha  = 0.6
    ,na.rm  = TRUE
    ) +
  scale_radius(range = c(3,10)) +
  theme_void() +
  theme(
    legend.position = "bottom"
  ) +
  scale_fill_gradient2(
    low       = "#e5f5e0"
    ,mid      = "#74c476"
    ,high     = "#005a32"
    ,midpoint = mean(cancer_map_centers$per_capita_income)
  )



g2
# ---- publish ---------------------------------------
path_report_1 <- "./analysis/*/report_1.Rmd"
# path_report_2 <- "./analysis/*/report_2.Rmd" # when linking multiple .Rmds to a single .R script
path_files_to_build <- c(path_report_1)
# path_files_to_build <- c(path_report_1, path_report_2) # add path_report_n to extend

testit::assert("The knitr Rmd files should exist.", base::file.exists(path_files_to_build))
# Build the reports
for( pathFile in path_files_to_build ) {
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document"
                      # "pdf_document"
                      # "word_document"
                    ),
                    clean=TRUE)
}
