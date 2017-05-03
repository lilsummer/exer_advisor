mentplot <- function() {
    totalment = readRDS('./Data/mentalhealth.rds')
    library(ggplot2)
    library(viridis)
    p <- ggplot(totalment, aes(x = age, y = days_of_bad_mental_health, fill = age)) + 
        geom_boxplot(stat = 'boxplot', outlier.shape = NA) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme_minimal() +
        scale_fill_viridis(discrete = T, name = 'days_of_bad_mental_health',
                           direction = -1)
    return(p)
    
}