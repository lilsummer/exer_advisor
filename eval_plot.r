# this is a program that evalute exercise on the national level
## preprocess the input
## this function is the same as final_eval_nation
## but it generates a ggplotly


eval_plot <- function (sex, age, height, weight, min, freq, type) {
    
#     sex = 'Female'
#     age = "Age 30 to 34"
#     height = '160'
#     weight = '80'
#     min = '45'
#     freq = '2'
#     per = "week"
#     type = 'Bicycling'
    library(ggplot2)
    library(plotly)
    library(magrittr)
    library(dplyr)
    
    total <- readRDS('./Data/total.rds')
    
    ### input processing
    ####
    Sex <- data.frame(ID = c(1,2), choice = c('Male', 'Female'))
    sex <- Sex$ID[which(Sex$choice == sex)]
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
    #####
    # if per is week
    # freq + 250
    # if per is month
    # freq + 100
    if (per == "week") {
        freq = freq + 250
    }else {
        freq = freq + 100
    }
    ########
    Type <- readRDS('./Data/exercise.rds')
    type <- Type$ID[which(Type$NAME == type)]
    ####################processing per and freq
    
    ##########
    
    
    ######

    ## subset the data based on input
    subset_1 <- intersect(which(total$SEX == sex),
                          which(total$X_AGEG5YR == age))
    ## 
    BMI = weight / (height^2) * 1000000
    low_BMI = BMI - 250
    high_BMI = BMI + 250
    subset_2 <- intersect(which(total$X_BMI5 >= low_BMI),
                          which(total$X_BMI5 <= high_BMI))
    
    ##
    low_min = min - 100
    high_min = min + 100
    subset_3 <- intersect(which(total$EXERHMM1 >= low_min),
                          which(total$EXERHMM1 <= high_min))
    
    ## 
    subset_4 <- which(total$EXRACT11 == type)
    
    low_freq = freq - 50
    high_freq = freq + 50
    subset_5 <- intersect(which(total$EXEROFT1 >= low_freq),
                          which(total$EXEROFT1 <= high_freq))
    
    subset <- Reduce(intersect, list(subset_1, subset_2,
                                     subset_3, subset_4,
                                     subset_5))
    if (length(subset) == 0) {
        print('No data available nationwide')
    }
    else {
    subset1 <- total[subset, ]
    }
    
    new_df <- data.frame(X_STATE = NA,
                         ADDEPEV2 = 0,
                         SEX = NA,
                         EXRACT11 = NA,
                         EXEROFT1 = freq, 
                         EXERHMM1 = min,
                         X_AGEG5YR = NA,
                         X_BMI5 = NA,
                         chronic = 0,
                         score = 0,
                         your_data = TRUE)
    subset1$your_data = rep(FALSE, dim(subset1)[1])
    new_df_2 <- rbind(new_df, subset1)
    
    p <- ggplot(new_df_2, aes(x = EXEROFT1, y = EXERHMM1, 
                               color = ADDEPEV2, 
                               size = chronic,
                               shape = your_data)) + 
        geom_point(alpha = 0.8) +
        scale_size(range = c(4, 8)) +
        theme( legend.title = element_blank() ) +
        ggtitle('People in the US who have similar exercise plans as yours') + 
        xlab('Exercise frequency') +
        ylab('Exercise duration') 
  
   ggplotly(p) %>% add_annotations(text = "Title", x = 1.02, y = .8, 
                                   xanchor = 'left',
                                   yanchor = 'bottom',
                                   legendtitle = TRUE) %>%
       layout(legend = list(y = .8, yanchor = "top"))
    
}