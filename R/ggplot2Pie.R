#++++++++++++++GGPLOT2+++++++++++++++++++++#
#ggplot
#Deadline 16 Oct, 2022
#Statistical Data Science class
install.packages("ggplot2")
library(ggplot2)

#####Selection
crime_Black=c()
for(n in 1:length(Crime_Dataset$black)){
  if(Crime_Dataset$black[n]==1){
    crime_Black[n]="Yes"
  }else{
    crime_Black[n]="No"
  }
}


black_criminal=cbind(Crime_Dataset$black, crime_Black)
View(black_criminal)
fdtBlackCrime= table(crime_Black)
fdtBlackCrime = as.data.frame(fdtBlackCrime)
fdtBlackCrime

colnames(fdtBlackCrime)=c("BLACK", "freq")
fdtBlackCrime
PieChart0 = ggplot(fdtBlackCrime, aes(x="", y=freq, fill=BLACK))

pieChart1 = PieChart0+geom_col()+
  coord_polar(theta ="y" )+
  theme_void()+
  theme(plot.title = element_text(color = "blue" ,size = 10, 
                                  face ="bold", 
                                  hjust = .5))+
  ggtitle('Black Criminals Pie Chart-0=No, 1=Yes-')+
  geom_text(aes(label = freq),position = position_stack(vjust = .5))+
  scale_fill_manual(values = c("yellow", "Green"),
                    labels=c("No","Yes"))+
  theme(legend.position = "left")


pieChart1
