
    
eval_plot3 <- function(type){
    library(ggplot2)
    library(wordcloud)
    library(RColorBrewer)
    library(png)
    
    Type <- readRDS('./Data/exercise.rds')
    type <- Type$ID[which(Type$NAME == type)]
    
    
    sorted = readRDS('./Data/sorted_2.rds')
    names(sorted) = c('Var.1', 'Var.2', 'value')
    sorted = subset(sorted, Var.1 == type )
    
    exer = read.csv(file = './Data/exercise_list.csv')
    sorted$Var.1 = exer$NAME[as.numeric(sorted$Var.1)]
    sorted$Var.2 = exer$NAME[as.numeric(sorted$Var.2)]
    sorted$ID = c(1:dim(sorted)[1])
    
    sorted = sorted[sorted$value > 3,]
    bad = is.na(sorted$Var.2)
    sorted = sorted[!bad,]
    
    p = wordcloud(words = sorted$Var.2, freq = sorted$value, min.freq = 1,
                  scale = c(5,1.5),
              max.words=300, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
    
    #p = png(p, res = 300)
    
    
    return(p)
    
    
                       
                       
                       
                       
}