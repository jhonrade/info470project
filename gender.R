library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)

g.overall <- read.csv('data/GenderDistributionOverall.csv')
g.new <- read.csv('data/GenderDistributionNewHires.csv')
g.resign <- read.csv('data/GenderDistributionResignations.csv')

#remove NA rows from data sets
g.new <- filter(g.new, Professor > 0)
g.resign <- filter(g.resign, Professor > 0)

#4 tables total, prof, assoc, assistant, overall

#add id's to different tables because we will combine them later
g.overall <- mutate(g.overall, ID = "Overall")
names(g.overall)[3]<-"Faculty"

g.new<- mutate(g.new, ID = "New.Hires") %>%
  group_by(Year, Gender) %>%
  mutate(Faculty = sum(Professor, Assoc.Prof, Asst.Prof))

g.resign <- mutate(g.resign, ID = "Resignation") %>%
  group_by(Year, Gender) %>%
  mutate(Faculty = sum(Professor, Assoc.Prof, Asst.Prof))

#make a new df for professor new hires and resignations by gender
F.new <- select(g.new, Year, Faculty, Gender, ID)
F.resign <- select(g.resign, Year, Faculty, Gender, ID) 
F.combined <- rbind(F.new, F.resign)
F.all <- bind_rows(F.combined, g.overall)

#reshape data for analysis
F.all <- F.all %>%
  group_by(Year, ID) %>%
  mutate(total = sum(Faculty)) %>%
  arrange(ID, Year)

#calculate percent change
F.all <- F.all  %>%
  group_by(ID, Gender) %>%
  mutate(pChange.value = (Faculty - lag(Faculty))/lag(Faculty) * 100) %>%
  mutate(percentage = (Faculty/total)*100) %>%
  mutate(pDiff = (percentage-lag(percentage)))

#adjust year for graphing purposes
F.all <- F.all %>%
  mutate(Year = substr(Year, 0, 4))

#change type to numeric 
F.all$Faculty <- as.numeric(F.all$Faculty)
F.all$Year <- as.numeric(F.all$Year)

female.all <- filter(F.all, Gender == "M") 
male.all <- filter(F.all, Gender == "F") 

#format data to perform a linear regression model on
percentages <- select(F.all, Year, Gender, ID, percentage) %>%
  spread(ID, percentage)
percentages$Year = percentages$Year - 2005
fp <- filter(percentages, Gender == "M") 
mp <- filter(percentages, Gender == "F") 

#get slope and y-intercept of each line for females
nh.f <- lm(New.Hires ~ Year, data=fp)
r.f <- lm(Resignation ~ Year, data=fp)
o.f <- lm(Overall ~ Year, data=fp)
summary(nh.f)

summary(r.f)
summary(o.f)

#plot of percentage of each year for females
female.percent <- ggplot(female.all, aes(x = Year, y = percentage, colour = ID, group=ID))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage of Female Faculty per Academic Year", x = "Academic Year", y = "Percentage(%)")+
  xlim(2007, 2015)

#total percentage DIFFERENCE betweeen each year for females
female.percentDiff <- ggplot(female.all, aes(x = Year, y = pDiff, colour = ID, group=ID))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change in Female Faculty per Academic Year", x = "Academic Year", y = "Percentage Diff(%)")+
  xlim(2007, 2015)

#get slope and y-intercept of each line for males
nh.m <- lm(New.Hires ~ Year, data=mp)
r.m <- lm(Resignation ~ Year, data=mp)
o.m <- lm(Overall ~ Year, data=mp)
summary(nh.m)
summary(r.m)
summary(o.m)

#plot of percentage of each year for males
male.percent <- ggplot(male.all, aes(x = Year, y = percentage, colour = ID, group=ID))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage of Male Faculty Over Time", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)

#total percentage DIFFERENCE betweeen each year for males
male.percentDiff <- ggplot(male.all, aes(x = Year, y = pDiff, colour = ID, group=ID))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Difference for Male Faculty", x = "Academic Year", y = "Percentage Diff(%)")+
  xlim(2007, 2015)

