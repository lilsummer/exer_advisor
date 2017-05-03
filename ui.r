
source('./recommender.r')
source('./eval_plot2.r')
source('./gen_healthplot.r')
source('./ExercisePlot.r')
source('./BMIplot.r')
source('./importFplot.r')
source('./eval_plot3.r')
source('./mentplot.r')
source('./recommeder2.r')

library(shinythemes)
library(shiny)
#library(shinydashboard)
library(ggplot2)
#library(plotly)
library(magrittr)
#library(dplyr)
###############
    Type <- readRDS('./Data/exercise.rds')
    mylist_type <- as.list(as.character(Type$NAME))
    
    #####
    #Per <- data.frame(ID = c(1,2), choice = c('week', 'month'))
    #mylist_per <- as.list(as.character(Per$choice))
    
    #####
    Sex <- data.frame(ID = c(1,2), choice = c('Male', 'Female'))
    mylist_sex <- as.list(as.character(Sex$choice))
    
    #####
    State <- readRDS('./Data/state.rds')
    mylist_state <- as.list(as.character(State$NAME))
    
    
    #####
    Age <- readRDS('./Data/age.rds')
    mylist_age <- as.list(as.character(Age$NAME))


## app.R ##
library(shinydashboard)

    
    source('./recommender.r')
    source('./eval_plot2.r')
    source('./gen_healthplot.r')
    source('./ExercisePlot.r')
    source('./BMIplot.r')
    source('./importFplot.r')
    
    library(shinythemes)
    library(shiny)
    #library(shinydashboard)
    library(ggplot2)
    #library(plotly)
    library(magrittr)
    #library(dplyr)
    ###############
    Type <- readRDS('./Data/exercise.rds')
    mylist_type <- as.list(as.character(Type$NAME))
    
    #####
    #Per <- data.frame(ID = c(1,2), choice = c('week', 'month'))
    #mylist_per <- as.list(as.character(Per$choice))
    
    #####
    Sex <- data.frame(ID = c(1,2), choice = c('Male', 'Female'))
    mylist_sex <- as.list(as.character(Sex$choice))
    
    #####
    State <- readRDS('./Data/state.rds')
    mylist_state <- as.list(as.character(State$NAME))
    
    
    #####
    Age <- readRDS('./Data/age.rds')
    mylist_age <- as.list(as.character(Age$NAME))
    
    
    ## app.R ##
    library(shinydashboard)
    
    navbarPage(
        theme = shinytheme("cerulean"),    
        "Smart Exercise Advisor",
        tabPanel("Evaluation",
                 sidebarLayout(
                     sidebarPanel(
                         h4('Enter your information:'),
                         radioButtons("sex","Sex",choices = list("Male" = 1,"Female" = 2),selected = 2),
                         
                         selectInput('age', "Age group",choices = mylist_age),
                         
                         sliderInput("weight", "Weight (kg)", 30, 400, 60),
                         
                         sliderInput("height", 'Height (cm)', 50, 250, 170),
                         
                         radioButtons("healthC",
                                      'Healthcare coverage',
                                      choices = list("Yes" = 1,
                                                     "No" = 2)),
                         selectInput("type",
                                     'Exercise type',
                                     choices = mylist_type),
                         
                         sliderInput("freq",
                                     "Exercise frequency (time)/week", 1, 15, 3),
                         
                         sliderInput("min",
                                     "Exercise duration each time (min)", 1, 180, 30),
                         selectInput("goal",
                                     'What is your health goal?',
                                     choices = list("I want to lose weight" = 1,
                                                    "I want to get stronger" = 2,
                                                    "I want to reduce chronic dieases risk"=3)),
                         
                         actionButton("actionButton", "Evaluate")
                     )
                     ,
                     
                     mainPanel(
                         fluidRow(
                             column(width = 12, 
                                    box(width = NULL, solidHeader = T,
                                        h4("Comparing with people in the US..."),
                                        plotOutput('barplot1')),
                                    br(),
                                    box(width = NULL, solidHeader = T,
                                       
                                         h4("Your exercise recommendation:"),
                                        textOutput('text1'),
                                        #textOutput('answer'),
                                         tags$head(tags$style("#text1{color: red;font-size: 25px;font-style: bold;}"))),
                                   br(),
                                   box(width = NULL, solidHeader = T,
                                       h4("Similar people are also doing these exercise..."),
                                       plotOutput('barplot2'))
                             )
                         )
                     )
                 )
        ),
        
        
        
        tabPanel("About the Data",
                 h4('Data source: ',
                    a("CDC BRFSS Survey (2000-2015)", href="https://www.cdc.gov/brfss/")),
                 br(),
                 fixedRow(
                column( 
                    width = 8,
                     h4('General health ratings in different age groups'),
                 plotOutput("GenHealthPlot"),
                 h5('As age increases, more people have fair health conditions and less people have excellent conditions'),
                 br(),
                 h4('Getting overweight?'),
                 plotOutput("BMIplot"),
                 h5('The speed of BMI increase in the US is as twice as much as other countries in the world.'),
                 br(),
                 h4('Mental health condition'),
                 plotOutput('mentalplot'),
                 h5('')
                 )
                 )
                ),
                 
        tabPanel("Models",
                 h4('Lose weight model'),
                 h5('A logistic regression model based on the recommendation provided in the original dataset (Precision: 0.79; Recall: 0.37; AUC: 0.89).',
                    a("Click here for more information.", href="http://rpubs.com/lilsummer1989/270862")),
                 br(),
                 h4('Get stronger model'),
                 h5('Random forest model (Precision: 0.65)',
                    a("Click here for more information",
                      href="http://rpubs.com/lilsummer1989/273222"))),
            
        tabPanel("More")
    )  
    
    