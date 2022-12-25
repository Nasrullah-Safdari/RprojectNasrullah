#++++++++++++++++++Final Project++++++++++++++++++#
#Nasrullah Safdari
###Y: households spending, Y~N(35, 5)
###X1: Households income, X1~N(40, 6)
###X2:Household size, X2~N(4,1.5)
###Y=a+b1*X1+b2X2
###Simulate Y.
###You need to apply lm model to estimate a, b1 and b2.



#First we need to Generate our sample data 
#in order to interpret regarding our population 
#since mine is countinus we use rnorm for X1,X2, and Y
y=rnorm(1000,35,5)
mean(y)
x1=rnorm(1000,40,6)
mean(x1)
x2=rnorm(1000,4,1.5)
mean(x2)

#####++++++++++++Plot density graph of our continous variable++++++++++####
#install.packages("ggplot2")
library(ggplot2)

total_variables= data.frame(y,x1,x2)
View(total_variables)

##++++++++++++Histogram for the Y variable++++++++++++##
gHist_yVar=ggplot(total_variables, aes(x=y))
gHist_yVar+geom_histogram(bins = 20, fill="orange", colour="black")+
  theme_classic()+ theme(plot.title = element_text(face = "bold", hjust = 0.5),
                         axis.title.x = element_text(),
                         axis.title.y = element_text())+
  ggtitle("histogram of Y variable")+ xlab("N=1000")+ylab("frequency")+
  geom_vline(xintercept = 35.04224, linetype="dashed",size=1, color="red")

##++++++++++++Histogram for the X1 variable++++++++++++##
gHist_x1Var=ggplot(total_variables, aes(x=x1))
gHist_x1Var+geom_histogram(bins = 20, fill="green", colour="black")+
  theme_classic()+ theme(plot.title = element_text(face = "bold", hjust = 0.5),
                         axis.title.x = element_text(),
                         axis.title.y = element_text())+
  ggtitle("histogram of x1 variable")+ xlab("N=1000")+ylab("frequency")+
  geom_vline(xintercept = 39.89731, linetype="dashed",size=1, color="red")

##++++++++++++Histogram for the x2 variable++++++++++++##
gHist_x2Var=ggplot(total_variables, aes(x=x2))
gHist_x2Var+geom_histogram(bins = 20, fill="blue", colour="black")+
  theme_classic()+ theme(plot.title = element_text(face = "bold", hjust = 0.5),
                         axis.title.x = element_text(),
                         axis.title.y = element_text())+
  ggtitle("histogram of x2 variable")+ xlab("N=1000")+ylab("frequency")+
  geom_vline(xintercept = 3.869017, linetype="dashed",size=1, color="red")

##----------------------------------------------------------------------------------------------##

#####++++++++++++++++++++++++++++++++Regression model++++++++++++++++++++++++++++++####
#In this part we need to run the regession model on our 
#sample data that we collect from our sample size(1000 people(students, workers, citizens))
#then, we use lm model in order to find out whether X1 and Y and also X2 
#and Y are linearly associated or not, and also wheather they are they have direct or indirect association
# in the lm result we need ahat, bhat and p-value in order to make inferences regarding our population
###+++++++++++++++++++++++++lm for X1+++++++++++++++++++++++++###
lm_forX1 = lm(y~x1)
summary(lm_forX1)

#for x1: ahat = 35.73007   &&&& bhat = -0.01567 
#bhat<0 it shows that in the sample data X has indirect effect on y 
# p-value: 0.5389 
##+++++++++++++++ Hypothesis testing for x1 +++++++++++++++##
#Here in this part we are gonna check whether it is the case for population or not!
#H0: b=0 (There is no effect of x1 on y)
#H1= b>0 (There is a direct effect of x1 on y)
#Alpha=0.1-----> since p-value(0.5389)> Alpha(0.1) we accept H0 because the probability of getting
#type error is more than 50% which is higher than our Alpha value.

##-----------------------------------------------------------------------------##

###+++++++++++++++++++++++++lm for X2+++++++++++++++++++++++++###
lm_forX2 = lm(y~x2)
summary(lm_forX2)
#for x2: ahat = 34.3191    &&&&  bhat = 0.1972  
#bhat>0 it shows that in the sample data X has direct effect on y 
# p-value: 0.05
###+++++++++++++++ Hypothesis testing for x2+++++++++++++++###
#Here in this part we are gonna check whether it is the case for population or not
#H0: b=0 (There is no effect of x1 on y)
#H1= b>0 (There is a direct effect of x1 on y)
#Alpha=0.1-----> since p-value(0.05) < Alpha(0.1), we reject H0 in favor of H1

