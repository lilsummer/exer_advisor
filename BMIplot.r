BMIplot <- function() {
    dataBMI = readRDS('./Data/BMI_df.rds')
    library(ggplot2)
    library(viridis)
    g2<-
        ggplot(data=dataBMI, aes(x=Age_Group,y=BMI,color=Year,group=Year))+
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
    return(g2)

    
}