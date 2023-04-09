library(dplyr)
library(ggplot2)
library(tidyverse)
library(GGally)
library(leaps)
set.seed(1020)


#Data Preprocess
#read the entire dataset
data <- read.csv("~/insurance.csv")

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
#It is hard to visually explore the association between number of children and charges 
#as there are uneven number of individuals with each number of children

sex_plot<-ggplot(insurance, aes(x=1:nrow(insurance),
                                y= charges,
                                col = sex))+geom_point()+xlab("index")
sex_plot
#There is no strong distribution pattern for different sex

ggpairs(insurance,title="correlogram")

insurance2 <- mutate(insurance, sex=as.factor(sex), smoker=as.factor(smoker))
#Transform into factors

levels(insurance2$sex)
levels(insurance2$smoker)

model <- glm(charges ~ age + sex + bmi + children + smoker, data = insurance2)
empty <- glm(charges ~ 1, data = insurance2)

backward = step(model)
forward = step(empty, scope=list(lower=formula(empty),upper=formula(model)), direction="forward")

formula(forward)

forwardmodel <- regsubsets(x=charges ~ age + bmi + children + smoker,
                            method="forward", data = insurance2)
summaryforward <- summary(forwardmodel)
summaryforward
summaryforward$cp
#Forward regression results

backwardmodel <- regsubsets(x=charges ~ age + bmi + children + smoker,
                             method="backward", data=insurance2)
summarybackward <- summary(backwardmodel)
summarybackward
summarybackward$cp
summarybackward$bic
#Backward regression results

plot(x=seq(1,4,by=1), y=summaryforward$adjr2, xlab="Model", ylab="Adjusted R^2 Values")
plot(x=seq(1,4,by=1), y=summarybackward$bic, xlab="Model", ylab="BIC Values")

modelfinal <- lm(data=insurance2, charges ~ age + bmi + children + smoker)
summary(modelfinal)
#Final model
resid_plot <- plot(resid(modelfinal))
