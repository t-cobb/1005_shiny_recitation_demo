#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# getwd()
# setwd("rec_04_demo/")

library(tidyverse)
library(primer.data)
library(shinythemes)
library(shiny)

# this is the source file / r script that calls in the data.
# this source() function references that script so I can then do anything
# I need in this as if it were in the same rmd 
source(file = "clean_pov_map.R")


# for plot opt. 2:
# CSV had same tibble force issue with [geometry]

data1 <- read_rds("poverty_map.Rds")

# Define UI for application that draws a histogram
ui <- navbarPage(
    
    tabsetPanel(
        tabPanel("Interactivity",
                 fluidPage(
                     titlePanel("Qscores Data"),
                     sidebarLayout(
                         sidebarPanel(
                            
        # selectInput is where interactivity starts. Choices is the label names
        # when user selects "Voted" returns TRUE or FALSE based on server logic                    
                              selectInput(
                                 "rufio",
                                 "Choose a Response Category",
                                 choices = c("Voted" = 1, 
                                   "Did not vote" = 0)
                             ),
                             width = 300),
                         plotOutput("object",
                                    width = 550,
                                    height = 500)))),

        # this is created live in the server, referencing a static data set

        tabPanel("Map",
                 titlePanel("Map of % HH Below Poverty Line"),
                 plotOutput("pov_plot2",
                            width = 450,
                            height = 400)),
        
        # this references the source file 
        
        tabPanel("Other Map",
                 titlePanel("Another way to bring in a Map"),
                 plotOutput("map2")),
        
        #this is just an image, and it needs to be in a www file, 
        # called in the img function. Loads faster
        
        tabPanel("Working Map",
                 titlePanel("Yet Another way to bring in a Map"),
                 img(src = "img01", align = "center", 
                     height = "80%", width = "80%")),
        
        tabPanel("Discussion",
                 titlePanel("Discussion Title"),
                 p("Show the iterations you walked through in model construction
                   and choosing your covariates, often this will look like tables
                   and distribution plots.")),
        
        tabPanel("Table",
                 titlePanel("You can include tables too!"),
                 gt_output("table1")),
        
        tabPanel("About", 
                 titlePanel("About"),
                 h3("Project Background and Motivations"),
                 p("Here you tell the story of your project, acknowledge sources,
                   and leave GH link and whatever contact(s) you feel comfortable with."),
                 uiOutput("link"))
        
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
output$object <- renderPlot({
    
   # ifelse(input$rufio == "1", 
   #        z <- 1, 
   #        z <- 0)
    
     cces %>%
        mutate(vote1 = ifelse(voted == "Voted", 1, 0)) %>%
        filter(vote1 == input$rufio) %>%
         
        ggplot(aes(x = faminc,
                   y = age)) +
        geom_col()

})    
    
    output$link <- renderUI({
        tags$a(href="https://beaumeche.shinyapps.io/Shiny-Recitation-Demo-wk4/", "Here is the link to this repo")
    })
        
    output$line_plot <- renderPlot({
        
    # this is how you reference the input variable (input$var_plot)
    # previous plot from Beau's example
        
        ifelse(input$var_plot == TRUE,
               z <- cces$voted,
               z <- qscores$rating)
        
            qscores %>% 
                ggplot(aes(x = hours,
                           y = z,
                           color = term)) +
                geom_point(alpha = 0.5) +
                labs(
                    title = "Student Reports from Courses at Harvard",
                    y = str_to_title(input$var_plot),
                    x = "Expected Workload / Week",
                    color = "Term",
                    caption = "Source: Harvard Registrar's Office")
        
        
    })
    
    # output$pov_plot <- renderPlot({
    #     x1 %>%
    #         ggplot(aes(fill = pov_ratio)) +
    #         geom_sf() +
    #         scale_fill_viridis_c(option = "viridis") +
    #         labs(title = "Impoverished Households - 2015",
    #              subtitle = "Poverty Line in 2015 was $24,250 for a family of 4",
    #              caption = "Sources: ACS 2015, ASPE",
    #              fill = "% HH") +
    #         theme_few()
    # })
    
    output$pov_plot2 <- renderPlot({
        
        data1 %>%
            ggplot(aes(fill = pov_ratio)) +
            geom_sf() +
            scale_fill_viridis_c(option = "viridis") +
            labs(title = "Impoverished Households - 2015",
                 subtitle = "Poverty Line in 2015 was $24,250 for a family of 4",
                 caption = "Sources: ACS 2015, ASPE",
                 fill = "% HH") +
            theme_few()
    
    # for the 3rd map    
    # ggsave("img01.png", plot = last_plot())
        
    })
    
    output$table1 <- render_gt({
        table
    })
    
    output$map2 <- renderPlot({
        map2
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
