#TITLE: Group 2
#DATE: 5/17/2021
#CA1 Graphs Assignment

#Load relevant packages
library(Sleuth3)
library (tidyverse)

#Load Question 1 dataset (ex0126)
data(ex0126)

#Explore the first 6 rows of the dataset
head(ex0126)


#Producing the descriptive statistics for the respective parties
ex0126%>%
  group_by(Party) %>%
    summarise(n = n(), Average = mean(PctPro), Median = median(PctPro),
            SD = sd(PctPro), IQR = IQR(PctPro)) %>%
  knitr::kable(digits = 2)

      #SUMMARY STATISTICS
#|Party |   n| Average| Median|    SD|   IQR|
# |:-----|---:|-------:|------:|-----:|-----:|
 # |D     | 243|   85.90|  92.00| 15.31| 16.00|
 # |I     |   1|  100.00| 100.00|    NA|  0.00|
  #|R     | 246|   16.17|   7.28| 20.96| 14.63|
  #|NA    |   2|   47.50|  47.50| 67.18| 47.50|
  
#Visualization of party differences in Environmental voting 
#Load packages
library(ggplot2)
library(ggthemes)

ggplot(data= ex0126, mapping=aes(x= Party, y= PctPro))+
  geom_boxplot(fill="gray")+
  ggtitle("Environmental Support by Party")+
  xlab ("Party Identification")+
  ylab("Percentage of Pro-environment votes") 


 ####QUESTION 2#####
#Load Question 2 dataset (case0501)
data(case0501)

#Explore the first 6 rows of the dataset
head(case0501)  

##Producing the descriptive statistics for the 6 groups
case0501 %>%
  group_by(Diet) %>%
  summarise(n = n(), Average = mean(Lifetime), Median = median(Lifetime),
            SD = sd(Lifetime), IQR = IQR(Lifetime)) %>%
  knitr::kable(digits = 2)

            #SUMMARY STATISTICS
#|Diet  |  n| Average| Median|   SD|   IQR|
#  |:-----|--:|-------:|------:|----:|-----:|
#  |N/N85 | 57|   32.69|  33.10| 5.13|  5.00|
  #|N/R40 | 60|   45.12|  46.05| 6.70|  8.07|
#  |N/R50 | 71|   42.30|  43.90| 7.77| 10.25|
#  |NP    | 49|   27.40|  28.90| 6.13|  6.60|
 # |R/R50 | 56|   42.89|  43.95| 6.68|  9.20|
#  |lopro | 56|   39.69|  41.05| 6.99| 11.45|
  

#Visualization of 6 groups on Life Expectancy ####

#Boxplot of the six groups
ggplot(data= case0501, mapping=aes(x= Diet, y= Lifetime))+
  geom_boxplot(fill="gray")+
  ggtitle("Impact of Diet on Life Expectancy")+
  xlab ("Diet")+
  ylab("Life span")

#Density plot of the six groups-1
ggplot(data= case0501, mapping=aes(x= Lifetime, fill= Diet))+
  geom_density(alpha=0.5)+
  ggtitle("Impact of Diet on Life Expectancy")+
  xlab ("Life span")+
  ylab("Density")

#Density plot of the six groups-2
ggplot(data= case0501, mapping=aes(x= Lifetime, fill= Diet))+
  geom_density(alpha=0.5)+
  facet_wrap(~Diet)+
  ggtitle("Impact of Diet on Life Expectancy")+
  xlab ("Life span")+
  ylab("Density")

#Histogram of the six groups
ggplot(data= case0501, mapping=aes(x=Lifetime , fill=Diet))+
  geom_histogram()+
    facet_wrap(~Diet)
  
#QQ plot of the six groups
ggplot(data = case0501, mapping = aes(sample = Lifetime)) +
  stat_qq() +
  stat_qq_line() +
  geom_qq( color = "dark red") +
  facet_wrap( ~ Diet)

 