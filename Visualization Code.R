#######################################
# DSCI 304                            #
# Final Project                       #
# Madison Stodola                     # 
#######################################

dev.off()
cat("\014")
rm(list=ls())
set.seed(18552)

library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyverse)
library(waffle)
library(gganimate)
#install.packages('animation')
library(animation)
#install.packages('survival')
library(survival)
#install.packages('ggsurvfit')
library(ggsurvfit)
library(stargazer)
library(ggeffects)

hranalytics <- read.csv("C:/Users/kitch/OneDrive/Documents/Rice/Fall 2022/DSCI 304/assignments/final project/DSCI-304/IBMEmployeeAttrition.csv")
View(hranalytics)

#1) waffle chart of attrition
#using information from past exam 1
attrition<-c("Left"=16, "Stayed"=84)
wafflecolors<-c("coral2","darkslategray3")
waffle(attrition, colors=wafflecolors,title="Employee Attrition Rate")+theme(text=element_text(size=20))

#2) satisfactions animation plot
#using information from past exam 1
hranalytics$Attrition2<-ifelse(hranalytics$Attrition=="No", 0, ifelse(hranalytics$Attrition=="Yes", 1, NA))
hranalytics$Attritionfactor<-factor(hranalytics$Attrition2, levels=c(1, 0))

wlbanalytics<-hranalytics%>%
  group_by(WorkLifeBalance,Attritionfactor)%>%
  summarise(count=n(),proportion = n()/sum(count))
wlb<-ggplot(data=hranalytics, aes(x=WorkLifeBalance, fill=Attritionfactor))+
  geom_bar(position = "fill")+  
  labs(title="Proportion of Employee Attrition by",subtitle="Work Life Balance Rating",y="Proportion of Respondents",x="Work Life Balance Rating")+
  theme(text=element_text(size=15))+
  scale_fill_manual(name="Attrition",labels=c("Yes", "No"),values=c("coral2","darkslategray3"))

jobsatanalytics<-hranalytics%>%
  group_by(JobSatisfaction,Attritionfactor)%>%
  summarise(count=n(),proportion = n()/sum(count))
jobsat<-ggplot(data=hranalytics, aes(x=JobSatisfaction, fill=Attritionfactor))+
  geom_bar(position = "fill")+  
  labs(title="Proportion of Employee Attrition by",subtitle="Job Satisfaction Rating",y="Proportion of Respondents",x="Job Satisfaction Rating")+
  theme(text=element_text(size=15))+
  scale_fill_manual(name="Attrition",labels=c("Yes", "No"),values=c("coral2","darkslategray3"))

envanalytics<-hranalytics%>%
  group_by(EnvironmentSatisfaction, Attritionfactor)%>%
  summarise(count=n(),proportion = n()/sum(count))
envsat<-ggplot(data=hranalytics, aes(x=EnvironmentSatisfaction, fill=Attritionfactor))+
  geom_bar(position = "fill")+  
  labs(title="Proportion of Employee Attrition by",subtitle="Environment Satisfaction Rating",y="Proportion of Respondents",x="Environment Satisfaction Rating")+
  theme(text=element_text(size=15))+
  scale_fill_manual(name="Attrition",labels=c("Yes", "No"),values=c("coral2","darkslategray3"))

jobinvanalytics<-hranalytics%>%
  group_by(JobInvolvement, Attritionfactor)%>%
  summarise(count=n(),proportion = n()/sum(count))
jobinv<-ggplot(data=hranalytics, aes(x=JobInvolvement, fill=Attritionfactor))+
  geom_bar(position = "fill")+  
  labs(title="Proportion of Employee Attrition by",subtitle="Job Involvement Rating",y="Proportion of Respondents",x="Job Involvement Rating")+
  theme(text=element_text(size=15))+
  scale_fill_manual(name="Attrition",labels=c("Yes", "No"),values=c("coral2","darkslategray3"))

animation::saveGIF(
  expr = {
    plot(wlb)
    plot(jobsat)
    plot(envsat)
    plot(jobinv)
  },
  anim.name = "plot2.gif",
  
)

#3) box plots of wlb and commute
ggplot(hranalytics,aes(x=WorkLifeBalance,y=DistanceFromHome,group=WorkLifeBalance,fill=WorkLifeBalance))+
  geom_boxplot(alpha=0.7)+
  theme(legend.position="none")+
  facet_wrap(~Attrition)+
  theme(text=element_text(size=15))+
  labs(title="Employee Attrition by Work Life Balance Rating and Commute Distance",y="Commute Distance (miles)",x="Work Life Balance Rating")

#4) histogram of commutes
ggplot(data=hranalytics, aes(DistanceFromHome))+
  geom_histogram(stat="count",aes(fill=Attritionfactor))+
  ggtitle("Employee Attrition by Commute Distance")+ 
  labs(y= "Number of Employees", x = "Distance of Commute (miles)", fill = "Attrition")+
  scale_fill_manual(labels=c("Yes", "No"),values=c("coral2","darkslategray3"))+
  theme(text=element_text(size=15))

#5) survival plot x=commute distance y=proportion attrition, lines by age (ranges?)
hranalytics$generation<-ifelse(hranalytics$Age<=25, "Gen Z", ifelse(hranalytics$Age<=41, "Millennial", ifelse(hranalytics$Age<=57,"Gen X",ifelse(hranalytics$Age>=58,"Boomer",NA))))
hranalytics$generation<-factor(hranalytics$generation, levels=c("Gen Z","Millennial","Gen X","Boomer"))

ageanalytics<-hranalytics%>%
  group_by(generation,Attrition2)%>%
  summarise(count=n(),proportion = n()/sum(count))
View(ageanalytics)
survfit2(Surv(DistanceFromHome, Attrition2) ~ generation, data = hranalytics) %>% 
  ggsurvfit() +
  labs(
    title = "Employee Survival Curve by Commute Distance and Age",
    x = "Commute Distance (miles)",
    y = "Probability of Staying"
  )+ add_confidence_interval()+
  theme(text=element_text(size=15))

#6) box plots of attrition by department
hranalytics$Department<-factor(hranalytics$Department)
mod1<-lm(Attrition2~Department,data = hranalytics)
summary(mod1)

p<-ggpredict(mod1, "Department")
plot(p)+
  ggtitle("Effect of Department on Employee Attrition")+
  labs(y= "Department", x = "Attrition \n(likelihood of employees leaving)")+
  theme(text=element_text(size=13))
