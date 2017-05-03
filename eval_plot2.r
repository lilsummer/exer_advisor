# this is a program that evalute exercise on the national level
## preprocess the input
## this function is the same as final_eval_nation
## but it generates a ggplotly
   sex = "1"
   age = "Age 35 to 44"
  height = '168'
   weight = '68'
   min = '45'
   freq = '3'
  type = 'Weight_lifting'
   healthC = 'Yes'

eval_plot2 <- function (sex, age, height, weight, min, freq, type, healthC) {
    
   
    library(ggplot2)
    #library(plotly)
    library(magrittr)
    #library(dplyr)
    
    total <- readRDS('./Data/exer_trainD.rds') ##changed
    
    ### input processing
    ####
    #Sex <- data.frame(ID = c(1,2), choice = c('Male', 'Female'))
    #sex <- Sex$ID[which(Sex$choice == sex)]
    sex = as.numeric(sex)
    ####
    Age <- readRDS('./Data/age.rds')
    age <- Age$ID[which(Age$NAME == age)]
    #########
    height = as.numeric(height)
    ######
    weight = as.numeric(weight)
    #####
    min = as.numeric(min)
    ####
    freq = as.numeric(freq)
    
    HealthC <- data.frame(ID = c(1,2), choice = c("Yes", "No"))
    healthC = healthC # ???
    ########
    Type <- readRDS('./Data/exercise.rds')
    type <- Type$ID[which(Type$NAME == type)]
    #####
    BMI = weight / (height)^2 * 10000
    BMI_cat = c(18.5, 25, 30, 99)
    BMI_dist = BMI - BMI_cat
    bmi = which(BMI_dist == min(abs(BMI_dist))) + 1
    ####
    Dur = min * freq
    #Dur_cat = c(30, 45, 60, 270)
    #Dur_dist = Dur - Dur_cat
    #dur = which(Dur_dist == min(abs(Dur_dist))) + 1
    ####################processing per and freq
    
    subset1 = subset(total, total$EXRACT11 == type & total$SEX == sex)
    subset2 = subset(subset1, subset1$X_AGE_G == age)
    subset3 = subset(subset2, subset2$X_BMI5CAT == bmi)
    
    
    ######
    if (dim(subset3)[1] == 0) {
        subset3 = subset2
    }
    
    
    
    #############
    #library(ggplot2)
    subset3$dur = as.numeric(subset3$PADUR1_)
    subset3$fre = as.numeric(as.character(subset3$PAFREQ1_))/1000
    subset3$load = subset3$dur*subset3$fre
    if(dim(subset3)[1] == 0) {
        p <- ggplot(data.frame(x=1,y=1)) + ggtitle('Sorry, no similar data found')
        return(p)
    }
    else {
    binnedSamples <- cut(subset3$load, breaks = seq(from=0, to=2500, by =100))
    plotdata = summary(binnedSamples)/length(subset3$dur)*100
    library(reshape2)
    plotdata = melt(plotdata)
    plotdata$your_load = rep(' ', dim(plotdata)[1])
    
    ### generate
    
    
    #############
    load_cat = seq(from=0, to=2500, by =100)
    #Dur_dist = Dur - Dur_cat
    load_dist = freq*min - load_cat
    
    inx = which(abs(load_dist) == min(abs(load_dist)))
    plotdata$your_load[inx] = 'Your level'
    plotdata$Var1 = seq(1:length(plotdata$value))
    ### 
    percentage_dur = (100-sum(plotdata$value[1:inx])) 
    percentage_dur = round(percentage_dur, 2)
    percentage_dur = as.character(percentage_dur)
    
    #%>% as.character()
    my_title = paste('Your exercise level is in the top ',percentage_dur)
    my_title = paste(my_title, "% nationwide!")
    plotdata$value = round(plotdata$value,1)

    ### generate x-tick labels###
    breaks = seq(from = 0, to = 2500, by = 100)
    x_tick = rep('', 26)
    for (i in c(2:26)) {
        x_tick[i-1] = paste('Less than',as.character(breaks[i]),'min', sep = ' ' )
        
    }
    plotdata$tick = x_tick[c(1:dim(plotdata)[1])]
    ###
    
   p<- ggplot(plotdata, aes(x= plotdata$Var1, 
                              y = plotdata$value, 
                              fill=plotdata$your_load)) + 
        geom_bar(stat = 'identity') + 
        ggtitle(my_title ) +
       xlab('Exercise load (frequency X duration)') +
       scale_x_continuous(breaks=1:26, labels = x_tick) +
       theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
       ylab('Percentage % population nationwide') +
       geom_text(aes(label=value), vjust=-0.2) +
       theme(plot.title = element_text(size = 20, face = "bold")) +
       theme(legend.position="right")  +
       scale_fill_discrete("")
       
   
    
    
    return(p)
    }
}