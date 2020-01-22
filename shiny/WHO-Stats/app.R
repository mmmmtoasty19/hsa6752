
# Load Packages -----------------------------------------------------------

library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(magrittr) # pipes

setwd("C:/Users/belangew/Documents/UCF/HSA6752 - Spring 2020/labs/shiny/WHO-Stats")

# Load Data & Clean Names--------------------------------------------------


ds <- readxl::read_xlsx(path = "data/Tableau 10 Training Practice Data.xlsx"
                        ,sheet = "03 - WHO Life Expect & Mort"
)

varnames <-  c("who_region", "country", "year" , "sex" , "life_expect_birth" , "neo_mort"
               ,"under_five_mort" , "health_expenditure")
names(ds) <- varnames

ds <-  ds %>% 
    select(who_region
           ,country
           ,life_expect_birth
           ,neo_mort
           ,under_five_mort
           ,health_expenditure)



# ui ----------------------------------------------------------------------

ui <- fluidPage(
    titlePanel("WHO Life Expectancy and Mortality")
    ,sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "region"
                ,label = "Region"
                ,choices = c(
                    "Africa"    = "Africa"
                    ,"Europe"   = 	"Europe"
                    ,"Americas" = "Americas"
                    )
                ,selected = "Africa"
                )
            )
        ,mainPanel(
            plotOutput("g1")
        )
    )
)



# server ------------------------------------------------------------------

server <- function(input,output) {
    output$g1 <-  
        renderPlot(
           ggplot(ds[ds$who_region == input$region,],mapping = aes(x = reorder(country, life_expect_birth), y = life_expect_birth)) +
        geom_col(fill = "#2b83ba") +
        labs(x  = NULL
             ,y = "Life Expectancy at Birth \n(years)"
        ) + 
        coord_flip() +
        theme_classic() +
        geom_text(aes(label = format(life_expect_birth, digits = 0))
                  ,nudge_y = 1
                  ,vjust   = 0.25
                  ,size    = 3 
                  ,color   = "black"
                )
        )
}



# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
