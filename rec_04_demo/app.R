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
source(file = "clean_pov_map.R")

# for plot opt. 2:
# CSV had same tibble force issue with [geometry]

data1 <- read_rds("poverty_map.Rds")

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("flatly"),
   mainPanel( 
    tabsetPanel(
        tabPanel("Interactivity",
                 fluidPage(
                     titlePanel("Qscores Data"),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput(
                                 "var_plot",
                                 "Choose a Response Category",
                                 choices = c("Enrollment" = "enrollment", 
                                   "Workload per Week" = "hours")
                             )),
                         plotOutput("line_plot")))),
        
        tabPanel("Map",
                 titlePanel("Map of % HH Below Poverty Line"),
                 plotOutput("pov_plot2")),
        
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
                   and leave GH link and whatever contact(s) you feel comfortable with.")))
        ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
            
        ifelse(input$var_plot == "enrollment",
               z <- qscores$enrollment,
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
    })
    
    output$table1 <- render_gt({
        table
    })
    
    output$image <- renderImage({

    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
