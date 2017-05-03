importFplot <- function () {
    
    scores = read.csv(file='./Data/scores.csv', header = F)
    name = read.csv(file='./Data/name-food.csv', header = F)
    plotdata = data.frame(V1 = scores, V2 = name)
    
    library(ggplot2)
    p <- ggplot(plotdata, aes(x=plotdata$V2.V1, y=plotdata$V1)) + 
        geom_bar(stat='identity', fill='darkblue') +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        ggtitle('Importance Score of Features') +
        theme(plot.title = element_text(size = 20, face = "bold")) +
        xlab('Feature') +
        ylab('Score') 

    return(p)
    
    
}