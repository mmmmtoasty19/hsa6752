rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # pipes %>%
library(ggplot2)  # graphs
library(dplyr)    # data wrangling
library(rlang)
requireNamespace("tidyr")  # data tidying

#----- load-sources -------------------------------
# source("./scripts/modeling/model-basic.R")
# source("./scripts/common-functions.R")
# ---- declare-globals ---------------------------------------------------------
# you will need to replace this path to the location where you stored your data file
path_file_input <- "./data_public/raw/florida-population-suicide.csv"

# to help with sorting the levels of the `age_group` factor
lvl_age_groups <-c(
  "less_than_1"
  ,"1_4"
  ,"5_9"
  ,"10_14"
  ,"15_19"
  ,"20_24"
  ,"25_34"
  ,"35_44"
  ,"45_54"
  ,"55_64"
  ,"65_74"
  ,"75_84"
  ,"85_plus"
)
age_groups_10_84 <- lvl_age_groups[4:12]
age_groups_10_24 <- lvl_age_groups[4:6]

#set global ggplot theme and options
theme_set(theme_bw())
update_geom_defaults("point", list(shape = 1
                                   ,size = 3))

facet_labels <- c(
  "less_than_1" = "Less Than 1"
  ,"1_4"        = "1 to 4"                  
  ,"5_9"        = "5 to 9"                
  ,"10_14"      = "10 to 14"           
  ,"15_19"      = "15 to 19"         
  ,"20_24"      = "20 to 24"           
  ,"25_34"      = "25 to 34"         
  ,"35_44"      = "35 to 44"           
  ,"45_54"      = "45 to 54"         
  ,"55_64"      = "55 to 64"           
  ,"65_74"      = "65 to 74"         
  ,"75_84"      = "75 to 84"           
  ,"85_plus"    = "85 &Over"        
)

make_line_graph <- function(ds,x,y, group = NULL, color = NULL){
  # browser()
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- enquo(group)
  color <- enquo(color)
  
  g <- ds %>% 
    ggplot(
      aes(
        x  = !!x
        ,y = !!y
        ,group = !!group
        ,color = !!color
        )
      ) +
    geom_line() +
    geom_point()
  return(g)
}

# ---- load-data ---------------------------------------------------------------
# data from Florida Health Charts
ds_population_suicide <-   readr::read_csv(path_file_input)

# ---- tweak-data-1 -----------------------------------------------------
ds0 <- ds_population_suicide %>%
  dplyr::mutate(
    year            = as.integer(year)
    ,sex            = factor(sex)
    ,race_ethnicity = factor(paste0(race, " + ", ethnicity))
    ,race           = factor(race)
    ,ethnicity      = factor(ethnicity)
    ,age_group      = factor(age_group, levels = lvl_age_groups)
    ,n_population   = as.integer(n_population)
    ,n_suicides     = as.integer(n_suicides)
  )
ds0 %>% dplyr::glimpse(70)



# ---- population_1 -----------------------------
# How did the total population of Florida changed over the years?
g1 <- ds0 %>% 
  group_by(year) %>% 
  summarise(
    n_population = sum(n_population)
  ) %>% 
  make_line_graph(year,n_population) +
  scale_x_continuous(breaks = seq(2007,2020,3)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Florida Total Population 2006 - 2017"
    ,x = NULL
    ,y = "Population"
  )
g1

# ---- population_2 -----------------------------
# What was the trajectory of growth for each age group?

g2 <- ds0 %>% 
  group_by(year,age_group) %>% 
  summarise(
    n_population = sum(n_population)
  ) %>% 
  make_line_graph(year,n_population) +
  scale_x_continuous(breaks = seq(2007,2020,3)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Florida Total Population 2006 - 2017 by Age Group"
    ,x    = NULL
    ,y    = "Population"
  ) +
  facet_wrap(
    ~age_group
    ,scales   = "free_y"
    ,labeller = labeller(
      age_group = facet_labels
      )
  )  
g2



# ---- population_3 -----------------------------
# For residends between 10 and 84 years of age,
# What was the trajectory of growth for each age group by sex?

g3 <- ds0 %>% 
  filter(age_group %in% age_groups_10_84) %>% 
  group_by(year,age_group,sex) %>% 
  summarise(
    n_population = sum(n_population)
  ) %>% 
  make_line_graph(year,n_population, group = sex, color = sex) +
  scale_x_continuous(breaks = seq(2007,2020,3)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Florida Total Population 2006 - 2017 by Age Group and Sex"
    ,x    = NULL
    ,y    = "Population"
  ) +
  facet_wrap(
    ~age_group
    ,scales = "free_y"
    ,labeller = labeller(age_group = facet_labels)
  ) 
g3


# ---- population_4 -----------------------------
# For residends between 10 and 84 years of age,
# What was the trajectory of growth for each ethnic group?
g4 <- ds0 %>% 
  filter(age_group %in% age_groups_10_84) %>% 
  group_by(year,age_group,race_ethnicity) %>% 
  summarise(
    n_population = sum(n_population)
  ) %>% 
  make_line_graph(year,n_population, group = race_ethnicity, color = race_ethnicity) +
  scale_x_continuous(breaks = seq(2007,2020,3)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title  = "Florida Total Population 2006 - 2017 \nby Age Group and Ethnic Group"
    ,x     = NULL
    ,y     = "Population"
    ,color = "Race + Ethnicity"
  ) +
  facet_wrap(~age_group
             ,scales = "free_y"
             ,labeller = labeller(age_group = facet_labels)
  ) 
g4



# ---- suicide_1 ----------------------------
# What is the trajectory of total suicides in FL between 2006 and 2017?
g5 <- ds0 %>% 
  group_by(year) %>% 
  summarise(
    n_suicides = sum(n_suicides,na.rm = TRUE)
  ) %>% 
  make_line_graph(year,n_suicides) +
  scale_x_continuous(breaks = seq(2007,2020,3)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Florida Total Suicides 2006 - 2017"
    ,x    = NULL
    ,y    = "Total Suicides"
  ) +
 scale_color_brewer(palette = "Dark2")
g5


# ---- suicide_2 ----------------------------
# For residends between 10 and 84 years of age,
# How does the trend of total suicides differ between men and women?
g6 <- ds0 %>% 
  group_by(year, sex) %>% 
  summarise(
    n_suicides = sum(n_suicides,na.rm = TRUE)
  ) %>% 
  make_line_graph(year,n_suicides, color = sex) +
  scale_x_continuous(breaks = seq(2007,2020,3)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Florida Total Suicides 2006 - 2017 \nby Sex"
    ,x    = NULL
    ,y    = "Total Suicides"
  ) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~sex, scales = "free_y") +
  theme(legend.position = "none")
g6



# ---- suicide_3 ----------------------------
# For residends between 10 and 84 years of age,
# How does the trend of suicides counts among ethnic groups differ by sex

g7 <- ds0 %>% 
  group_by(year, sex, race_ethnicity) %>% 
  summarise(
    n_suicides = sum(n_suicides,na.rm = TRUE)
  ) %>% 
  make_line_graph(year,n_suicides,group = race_ethnicity, color = race_ethnicity) +
  scale_x_continuous(breaks = seq(2007,2020,3)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title  = "Florida Total Suicides 2006 - 2017 n\by Sex and Ethnic Group"
    ,x     = NULL
    ,y     = "Total Suicides"
    ,color = "Race + Ethnicity"
  ) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~sex, scales = "free_y") 
g7



# ---- suicide_4 ----------------------------
# For residends between 10 and 84 years of age,
# How does the trend of total suicides between men and women differ across ethnic groups?


g8 <- ds0 %>% 
  group_by(year, sex, race_ethnicity) %>% 
  summarise(
    n_suicides = sum(n_suicides,na.rm = TRUE)
  ) %>% 
  make_line_graph(year,n_suicides,group = sex, color = sex) +
  scale_x_continuous(breaks = seq(2007,2020,3)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title  = "Florida Total Suicides 2006 - 2017 \nby Ethnic Groups and Sex "
    ,x     = NULL
    ,y     = "Total Suicides"
    ,color = "Sex"
  ) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~race_ethnicity, scales = "free_y") 
g8


# ----- compute_rate_function --------------------

# Compose the function that computes suicide rates per 100,000
compute_suicide_rate <- function(d_input, group_by_variables){
  # d_input <- ds0
  # group_by_variables <- "year"
  d_output <- d_input %>%
    #group the data by user variables
    group_by(., .dots = group_by_variables) %>%
    #summarise based on variables, this allows flexabilty of the totals
    summarise(
      n_population = sum(n_population)
      ,n_suicides  = sum(n_suicides, na.rm = TRUE)
    ) %>% 
    #add the column with rate calculation
  mutate(
    rate_per100k_suicide = (n_suicides/n_population)*100000 
  ) %>% 
    # remove groups
    ungroup()
  return(d_output)
}


# ---- suicide_rate_1 ----------------------------------
# What is the trend of the total suicide rates in Florida between 2006 and 2017?
g9 <- ds0 %>%
  compute_suicide_rate("year") %>%
  make_line_graph(year,rate_per100k_suicide) +
  scale_x_continuous(breaks = seq(2007,2020,3)) +
  scale_y_continuous(breaks = seq(13,16,0.5)) +
  labs(
    title = "Florida Suicide Rates 2006 - 2017"
    ,x    = NULL
    ,y    = "Rate per 100k"
  )

g9
  



# ---- suicide_rate_2 ----------------------------------
# For residends between 10 and 24 years of age ( as a single group),
# How does the trend of suicide rates among ethnic groups differ by sex?
g10 <- ds0 %>%
  dplyr::filter(age_group %in% age_groups_10_24) %>%
  compute_suicide_rate(c("year","race_ethnicity","sex")) %>%
  make_line_graph(year,rate_per100k_suicide, group = race_ethnicity, color = race_ethnicity) +
  scale_x_continuous(breaks = seq(2007,2020,3)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Florida Youth Suicide Rates 2006 - 2017 \n by Sex and Race"
    ,x    = NULL
    ,y    = "Rate per 100k"
  ) +
  facet_wrap(~sex)

g10
  



# ---- publish ---------------------------------
rmarkdown::render(
  input = "./script.Rmd"
  ,output_format = c(
    "html_document"
    # ,"pdf_document"
    # ,"md_document"
    # "word_document"
  )
  ,clean=TRUE
)



