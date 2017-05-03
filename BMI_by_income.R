
library(ggplot2)
library(viridis)
library(plotly)
# g2<-
#     ggplot(data=data1, aes(x=Age_Group,y=BMI,color=Year,group=Year))+
#     geom_jitter(height=0,width=0.2,alpha=0.1,color="gray")+
#     geom_jitter(height=0,width=0.2,alpha=0.82, aes(frame=Year))+
#     #scale_x_discrete(breaks=seq(2000,2015,1))+
#     theme_minimal()+
#     scale_color_viridis(direction=-1,name="Year")+
#     labs(y="BMI",        x="Age Group",
#          title="Americans are getting overweight?",
#          subtitle="Average BMI from BRFSS Survey",
#          caption="@Data source: CDC")+
#     theme(legend.position="right",plot.subtitle=element_text(face="italic"),
#           plot.title=element_text(face="bold"),
#           #plot.caption=element_text(hjust=10),
#           axis.text.x = element_text(angle = 90, hjust =1))
# 



total = readRDS('BMI_df_by_income.rds')
# p <- ggplot(total, aes(x = X_AGEG5YR_F)) + 
#     geom_bar(stat = 'count',position = 'fill',
#              aes(fill = GENHLTH_F)) + 
#     theme(axis.text.x = element_text(angle = 90, hjust = 1))


BMI_table = tapply(total$X_BMI_n_100, 
                   INDEX = list(total$IYEAR_F, total$INCOME2_F),
                   FUN = mean, na.rm = T)
library(reshape2)
BMI_df = melt(BMI_table)
names(BMI_df) = c('Year', 'Income_group', 'BMI')


p1 <-
    ggplot(data=BMI_df, aes(x=Year, y = BMI, group = Income_group)) + 
    geom_line(aes(color=Income_group)) + 
    theme_minimal() +
    labs(y="BMI",        x="Year",
                  title="BMI Increase in different income group",
                  subtitle="Average BMI from BRFSS Survey",
                  caption="@Data source: CDC")+
             theme(legend.position="right",plot.subtitle=element_text(face="italic"),
                   plot.title=element_text(face="bold"),
                   #plot.caption=element_text(hjust=10),
                   axis.text.x = element_text(angle = 90, hjust =1))


p2 <- ggplotly(p1)
p2
