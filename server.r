
source('./eval_plot2.r')
source('./recommender.r')
source('./gen_healthplot.r')
source('./ExercisePlot.r')
source('./BMIplot.r')
source('./importFplot.r')
source('./eval_plot3.r')
source('./mentplot.r')
source('./recommeder2.r')

library(shiny)
library(shinydashboard)
library(ggplot2)
#library(plotly)
library(magrittr)
library(viridis)

data1 = readRDS('./Data/generalhealthdata.rds')
dataBMI = readRDS('./Data/BMI_df.rds')
totalment = readRDS('./Data/mentalhealth.rds')
#library(dplyr)
# load('/Users/LilSummer/Desktop/coursera-capstone/Data/gramdata.RData')


# unigram <- read.csv(file = './Data/unigram_training.csv', header = T)

server <- function(input, output) {
   
    
    #source('./eval_nation.r')
    #source('./eval_plot2.r')
    
    
   barplotInput <- eventReactive(input$actionButton, {
        ## code to generate results as a plot
        eval_plot2(input$sex, 
                  input$age,
                  input$height,
                  input$weight,
                  input$min,
                  input$freq,
                  input$type,
                  input$healthC)
        
        })
   
    barplotInput2 <- eventReactive(input$actionButton, {
        eval_plot3(input$type)
    })
    
    output$barplot1 <- renderPlot({
        barplotInput()
    })
    output$barplot2 <- renderPlot({
        barplotInput2()
    })
    output$GenHealthPlot <- renderPlot({
        
        ggplot(data1, aes(x = age)) + 
            geom_bar(stat = 'count',position = 'fill',
                     aes(fill = general_health_rating)) + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            labs(title = "General health ratings") +
            xlab('Age group') +
            ylab('Percentage % population') +
            theme_minimal() +
            scale_fill_viridis(discrete = T, 
                               name = 'General_health_ratings',
                               direction = -1)
        
    })
   
    output$ExercisePlot <- renderPlot({
        ExercisePlot()
    })
    output$BMIplot <- renderPlot({
        
        
            ggplot(data=dataBMI, 
                   aes(x=Age_Group,y=BMI,color=Year,group=Year))+
            geom_jitter(height=0,width=0.2,alpha=0.1,color="gray")+
            geom_jitter(height=0,width=0.2,alpha=0.82)+
            #scale_x_discrete(breaks=seq(2000,2015,1))+
            theme_minimal()+
            scale_color_viridis(direction=-1,name="Year")+
            labs(y="BMI",        x="Age Group",
                 title="Average BMI from BRFSS Survey",
                 caption="@Data source: CDC")+
            theme(legend.position="right",plot.subtitle=element_text(face="italic"),
                  plot.title=element_text(face="bold"),
                  #plot.caption=element_text(hjust=10),
                  axis.text.x = element_text(angle = 90, hjust =1))
        
        
        
    })
    output$mentalplot <- renderPlot({
        ggplot(totalment, 
               aes(x = age, y = days_of_bad_mental_health, fill = age)) + 
            geom_boxplot(stat = 'boxplot', outlier.shape = NA) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            theme_minimal() +
            scale_fill_viridis(discrete = T, 
                               name = 'days_of_bad_mental_health',
                               direction = -1)
    })
   
    output$importF <- renderPlot({
        importFplot()
    })
    
    output$answer <- renderText({
        outputAnswer()
    })
    
    test1Input <- eventReactive(input$actionButton,
                                {
                ## here to generate a function using glm to give you the text prediction
            # result has to be in print
            if (input$goal == 1 | input$goal == 3) {
                     recommender(input$sex,
                                 input$age,
                                 input$height,
                                 input$weight,
                                 input$min,
                                 input$freq,
                                 input$type,
                                 input$healthC)
            }
                                    else {
                                    
                                recommender2(input$sex,
                                        input$age,
                                        input$height,
                                        input$weight,
                                        input$min,
                                        input$freq,
                                        input$type,
                                        input$healthC)
                                    }
                                    
                                })
    output$text1 <- renderText({
        test1Input()
        #outputAnswer()
    })
    
    outputAnswer <- eventReactive(input$actionButton,
                                  {paste(
                                      input$goal,
                                      input$sex,
                                      input$age,
                                      input$height,
                                      input$weight,
                                      input$min,
                                      input$freq,
                                      input$type,
                                      input$healthC, sep = " "
                                  )
                                      
                                  })
}