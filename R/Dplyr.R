library(dplyr)
getwd()
install.packages("wooldridge")
library(wooldridge)
Crime_Dataset=wooldridge::crime1
View(Crime_Dataset)
#Dplyr package
##Filter
    #one Quantiitive vrauable
quantile(Crime_Dataset$inc86)
income= filter(Crime_Dataset, inc86>=90)
head(income)

    #qualitative and qualitative variable
black_totTime=filter(Crime_Dataset, black==1,tottime>10 )
head(black_totTime)

    ##using &, |, ! operator
timeArrest= filter(Crime_Dataset, narr86==3 | narr86==6  )
head(timeArrest)
timeArrest_hispanic= filter(Crime_Dataset, (narr86==3 | narr86==6) & hispan==1)
head(timeArrest_hispanic)

black_hispan = filter(Crime_Dataset, black==1 & hispan !=1)
head(black_hispan)

##using function %in%
unique(Crime_Dataset$inc86)
subset_nparr=filter(Crime_Dataset,nparr86 %in% c(1,5))
head(subset_nparr)

subset_narr86=filter(Crime_Dataset,narr86 %in% c(2,4))
head(subset_narr86)

## for numerical variables <, >, =
ptime=filter(Crime_Dataset, ptime86>5)
head(ptime)

#++++++++++++++++++++++++++++++++++++++#
##Data manipulation with dplyr package
  #Average
    ##quantitative
names(Crime_Dataset)
head(arrange(Crime_Dataset, desc(inc86)))  
    ##Qualitative variable
head(arrange(Crime_Dataset, black))


  #Select
select(Crime_Dataset, c(black, hispan,born60))
    ##using everything function
select(Crime_Dataset, born60, everything())
select(Crime_Dataset, inc86:born60)
select(Crime_Dataset, -(narr86:black))

  #rename
Crime_upd= rename(Crime_Dataset, Black=black)
head(Crime_upd)

  #mutate
str(Crime_Dataset)
      ###Average_Income=inc86/total observation(2725)
head(mutate(Crime_Dataset, Average_Income=inc86/2725 ))

  #summarize
summarise(Crime_Dataset, mean(inc86),sd(inc86), mean(inc86sq),sd(inc86sq) )
b_crime=group_by(Crime_Dataset, black)
summarise(b_crime,mean(inc86), sd(inc86),mean(tottime), )

  #Pull: Pull column as a vector
pull(Crime_Dataset, hispan)

  #sample_n
dim(Crime_Dataset)
sampled_crime=sample_n(Crime_Dataset,10 )
sampled_crime





















