gen_healthplot <- function () {
    # data1 = readRDS('./Data/exer_trainD.rds')
    # GENH_map = c('Excellent',
    #              'Very Good',
    #              'Good',
    #              'Fair',
    #              'Poor',
    #              'Don\'t know\\not sure',
    #              'Refused')
    # data1$GENHLTH_F = as.factor(GENH_map[data1$GENHLTH])
    # data1$GENHLTH_F = factor(data1$GENHLTH_F,
    #                          levels(data1$GENHLTH_F)[c(2,7,4,3,5,6,1)])
    # 
    # 
    # 
    # AGE_map = c('18-24',
    #             '25-29',
    #             '30-34',
    #             '35-39',
    #             '40-44',
    #             '45-49',
    #             '50-54',
    #             '55-59',
    #             '60-64',
    #             '65-69',
    #             '70-74',
    #             '75-79',
    #             '80 to older',
    #             'Missing')
    # 
    # data1$X_AGEG5YR_F = as.factor(AGE_map[data1$X_AGEG5YR])
    library(ggplot2)
    library(viridis)
    data1 = readRDS('./Data/generalhealthdata.rds')
    p <- ggplot(data1, aes(x = age)) + 
        geom_bar(stat = 'count',position = 'fill',
                 aes(fill = general_health_rating)) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        labs(title = "General health ratings") +
        xlab('Age group') +
        ylab('Percentage % population') +
        theme_minimal() +
        scale_fill_viridis(discrete = T, name = 'General_health_ratings',
                           direction = -1)
    
    
    
    
    
    return(p)
}