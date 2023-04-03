library(dplyr)
library(ggplot2)
library(GGally)
set.seed(1020)


#Data Preprocess
#read the entire dataset
data <- read.csv("~/Desktop/insurance.csv")

#filter out the part where the region is southeast
insurance<- data |> filter(region == "southeast") |> select(-region)|> select(-index)
head(insurance)

#Exploratory analysis and visualization

#summarize the mean values of the numerical variables
summary_averages <- insurance |> 
  summarize(count=n(), mean_age=mean(age), mean_bmi=mean(bmi),
            mean_children=mean(children),mean_charges=mean(charges))

summary_averages

#visualize the distribution of charges, colored by smoke
smoke_plot<-ggplot(insurance, aes(x= 1:nrow(insurance),
               y=charges,
               col = smoker))+geom_point()+xlab("index")
smoke_plot
#We see that smokers generally have high charges than non-smokers.


children_plot<-boxplot(charges~children,data=insurance)
#It is hard to visually explore the associatiion between number of children and charges 
#as there are uneven number of individuals with each number of children

sex_plot<-ggplot(insurance, aes(x=1:nrow(insurance),
                                y= charges,
                                col = sex))+geom_point()+xlab("index")
sex_plot
#There is no strong distribution pattern for different sex

ggpairs(insurance,title="correlogram")
