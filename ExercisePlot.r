ExercisePlot <- function() {
    Type <- readRDS('./Data/exercise.rds')
    mylist_type <- as.list(as.character(Type$NAME))
    data1 = readRDS('./Data/exer_trainD.rds')
    exer_map = Type$NAME
    data1$type_f = exer_map[data1$EXRACT11]
    library(reshape2)
    plotdata = summary(data1$type_f)
    plotdata = melt(plotdata)
    plotdata$type = rownames(plotdata)
    plotdata = plotdata[plotdata$value > 800,]
    library(ggplot2)
    library(RColorBrewer)
    p <- ggplot(plotdata, aes(y = plotdata$value/sum(plotdata$value), 
                              x=plotdata$type)) + 
        geom_bar(stat = 'identity', fill="darkblue") + 
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        ggtitle('Popularity of Exercise') +
        theme(plot.title = element_text(size = 20, face = "bold")) +
        xlab('Exercise Type') +
        ylab('Percentage % population') 
        
    
    return(p)
    
}