new.data <- read.csv("data/EthnicDistributionNewHires.csv")
overall.data <- read.csv("data/EthnicDistributionOverall.csv")
resign.data <- read.csv("data/EthnicDistributionResignations.csv")

library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

#resignation
resign <- resign.data %>%
  select(Academic.Year, White, Asian, Black, Hispanic, American.Indian, Pacific.Islander, Multi.Racial,Not.Reported, Refused.to.Respond,Total) %>%
  arrange(Academic.Year) %>%
  mutate(id = "Resignation")

#overall
overall <- overall.data %>%
  select(Academic.Year, White, Asian, Black, Hispanic, American.Indian, Pacific.Islander, Multi.Racial,Not.Reported, Refused.to.Respond,Total) %>%
  arrange(Academic.Year) %>%
  mutate(id = "Overall")

#New hires
new <- new.data %>%
  select(Academic.Year, White, Asian, Black, Hispanic, American.Indian, Pacific.Islander, Multi.Racial,Not.Reported, Refused.to.Respond, Total) %>%
  arrange(Academic.Year) %>%
  mutate (id= "New.Hires")

types <- rbind(resign,overall,new)
all <- melt(types, id.vars = c("Academic.Year", "id", "Not.Reported", "Refused.to.Respond","Total"))

#calculate percentage change
all <- all %>%
  group_by(id, variable) %>%
  mutate(pChange.value = (value - lag(value))/lag(value) * 100) %>%
  replace_na(list(Refused.to.Respond = 0)) %>%
  mutate(percentage = (value/(Total-(Not.Reported+Refused.to.Respond))*100)) %>%
  mutate(pDiff = (percentage-lag(percentage)))

#add year (for graphing purposes)
all <- all %>%
  mutate(Year = substr(Academic.Year, 0, 4))

#change type to numeric 
all$value <- as.numeric(all$value)
all$Year <- as.numeric(all$Year)

#CHARTS FOR RESIGNATION, NEW HIRES, OVERALL
r <- dplyr::filter(all, grepl("Resignation", id))
re <- ggplot(r, aes(x = Year, y = pDiff, colour = variable, group=variable))+
  geom_point()+ theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(color = "Ethnicity", title = "Percentage Difference for Resignation", x = "Academic Year", y = "Percentage Diff(%)")+
  xlim(2007, 2015)
re

#Resignation linear model
whiteR <- dplyr::filter(r, grepl("White", variable))
linearModel <- lm(pDiff ~ Year, data = whiteR)
print(linearModel)

asianR <- dplyr::filter(r, grepl("Asian", variable))
linearModel <- lm(pDiff ~ Year, data = asianR)
print(linearModel)

blackR <- dplyr::filter(r, grepl("Black", variable))
linearModel <- lm(pDiff ~ Year, data = blackR)
print(linearModel)

hispanicR <- dplyr::filter(r, grepl("Hispanic", variable))
linearModel <- lm(pDiff ~ Year, data = hispanicR)
print(linearModel)

indianR <- dplyr::filter(r, grepl("American.Indian", variable))
linearModel <- lm(pDiff ~ Year, data = indianR)
print(linearModel)

islandR <- dplyr::filter(r, grepl("Pacific.Islander", variable))
linearModel <- lm(pDiff ~ Year, data = islandR)
print(linearModel)

multiR <- dplyr::filter(r, grepl("Multi.Racial", variable))
linearModel <- lm(pDiff ~ Year, data = multiR)
print(linearModel)

#NEW HIRES CHART
new.hires <- dplyr::filter(all, grepl("New.Hires", id))
nh <- ggplot(new.hires, aes(x = Year, y = pDiff, colour = variable, group=variable))+
  geom_point()+ theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(color = "Ethnicity", title = "Percentage Difference for New Hires", x = "Academic Year", y = "Percentage Diff(%)")+
  xlim(2008, 2015)
nh

#LINEAR MODELS FOR NEW HIRES
whiteH <- dplyr::filter(new.hires, grepl("White", variable))
linearModel <- lm(pDiff ~ Year, data = whiteH)
print(linearModel)

asianH <- dplyr::filter(new.hires, grepl("Asian", variable))
linearModel <- lm(pDiff ~ Year, data = asianH)
print(linearModel)

blackH <- dplyr::filter(new.hires, grepl("Black", variable))
linearModel <- lm(pDiff ~ Year, data = blackH)
print(linearModel)

hispanicH <- dplyr::filter(new.hires, grepl("Hispanic", variable))
linearModel <- lm(pDiff ~ Year, data = hispanicH)
print(linearModel)

indianH <- dplyr::filter(new.hires, grepl("American.Indian", variable))
linearModel <- lm(pDiff ~ Year, data = indianH)
print(linearModel)

islandH <- dplyr::filter(new.hires, grepl("Pacific.Islander", variable))
linearModel <- lm(pDiff ~ Year, data = islandH)
print(linearModel)

multiH <- dplyr::filter(new.hires, grepl("Multi.Racial", variable))
linearModel <- lm(pDiff ~ Year, data = multiH)
print(linearModel)

#Overall Profile 
overall <- dplyr::filter(all, grepl("Overall", id))
o <- ggplot(overall, aes(x = Year, y = pDiff, colour = variable, group=variable))+
  geom_point()+ theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(color = "Ethnicity", title = "Percentage Difference for Overall Profile", x = "Academic Year", y = "Percentage Diff(%)")+
  xlim(2008, 2015)

#Linear Model for Overall Profile
whiteO <- dplyr::filter(overall, grepl("White", variable))
linearModel <- lm(pDiff ~ Year, data = whiteO)
print(linearModel)

asianO <- dplyr::filter(overall, grepl("Asian", variable))
linearModel <- lm(pDiff ~ Year, data = asianO)
print(linearModel)

blackO <- dplyr::filter(overall, grepl("Black", variable))
linearModel <- lm(pDiff ~ Year, data = blackO)
print(linearModel)

hispanicO <- dplyr::filter(overall, grepl("Hispanic", variable))
linearModel <- lm(pDiff ~ Year, data = hispanicO)
print(linearModel)

indianO <- dplyr::filter(overall, grepl("American.Indian", variable))
linearModel <- lm(pDiff ~ Year, data = indianO)
print(linearModel)

islandO <- dplyr::filter(overall, grepl("Pacific.Islander", variable))
linearModel <- lm(pDiff ~ Year, data = islandO)
print(linearModel)

multiO <- dplyr::filter(overall, grepl("Multi.Racial", variable))
linearModel <- lm(pDiff ~ Year, data = multiO)
print(linearModel)



#INDIVIDUAL CHARTS FOR ETHNICITIES
#resignation
white.percentDiff <- ggplot(data_white, aes(x = Year, y = pDiff, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Difference for White Faculty", x = "Academic Year", y = "Percentage Diff(%)")+
  xlim(2007, 2015)

#WHITE
data_white <- filter(all, variable == "White")

#linear model graph for white faculty PERCENTANGE CHANGE FOR EACH VALUE
white.plot <- ggplot(data_white, aes(x = Year, y = pChange.value, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change between each year for White Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)

#percentage of each year
white.percent <- ggplot(data_white, aes(x = Year, y = percentage, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage for White Faculty each Academic Year", x = "Academic Year", y = "Percentage(%)")+
  xlim(2007, 2015)

#total percentage DIFFERENCE betweeen each year
white.percentDiff <- ggplot(data_white, aes(x = Year, y = pDiff, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Difference for White Faculty", x = "Academic Year", y = "Percentage Diff(%)")+
  xlim(2007, 2015)


#white.plot
white.percent
white.percentDiff

whiteResign <- dplyr::filter(data_white, grepl("Resignation", id))
linearModel <- lm(whiteResign$pDiff ~ whiteResign$Year, data = whiteResign)

whiteHire <- dplyr::filter(data_white, grepl("New.Hires", id))
linearModel2 <- lm(whiteHire$pDiff ~ whiteHire$Year, data = whiteHire)
print(linearModel2)

whiteOverall <- dplyr::filter(data_white, grepl("Overall", id))
linearModel3 <- lm(pDiff ~ Year, data = whiteOverall)
print(linearModel3)
#ASIAN
data_asian <- filter(all, variable == "Asian")

#linear model graph for asian faculty
asian.plot <- ggplot(data_asian, aes(x = Year, y = pChange.value, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change for Asian Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)

#percentage of each year
asian.percent <- ggplot(data_asian, aes(x = Year, y = percentage, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage for Asian Faculty each Academic Year", x = "Academic Year", y = "Percentage(%)")+
  xlim(2007, 2015)

#total percentage DIFFERENCE betweeen each year
asian.percentDiff <- ggplot(data_asian, aes(x = Year, y = pDiff, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Difference for Asian Faculty", x = "Academic Year", y = "Percentage Diff(%)")+
  xlim(2007, 2015)

#asian.plot
asian.percent
asian.percentDiff

#BLACK
data_black<- filter(all, variable == "Black")

#linear model graph for black faculty
black.plot <- ggplot(data_black, aes(x = Year, y = pChange.value, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change for Black Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)

#percentage of each year
black.percent <- ggplot(data_black, aes(x = Year, y = percentage, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage for Black Faculty each Academic Year", x = "Academic Year", y = "Percentage(%)")+
  xlim(2007, 2015)

#total percentage DIFFERENCE betweeen each year
black.percentDiff <- ggplot(data_black, aes(x = Year, y = pDiff, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Difference for Black Faculty", x = "Academic Year", y = "Percentage Diff(%)")+
  xlim(2007, 2015)

#black.plot
black.percent
black.percentDiff

#HISPANIC
data_hispanic<- filter(all, variable == "Hispanic")

#linear model graph for hispanic faculty
hispanic.plot <- ggplot(data_hispanic, aes(x = Year, y = pChange.value, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change for Hispanic Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)

#percentage of each year
hispanic.percent <- ggplot(data_hispanic, aes(x = Year, y = percentage, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage for Hispanic Faculty each Academic Year", x = "Academic Year", y = "Percentage(%)")+
  xlim(2007, 2015)

#total percentage DIFFERENCE betweeen each year
hispanic.percentDiff <- ggplot(data_hispanic, aes(x = Year, y = pDiff, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Difference for Hispanic Faculty", x = "Academic Year", y = "Percentage Diff(%)")+
  xlim(2007, 2015)

#hispanic.plot
hispanic.percent
hispanic.percentDiff

#American Indian
data_indian<- filter(all, variable == "American.Indian")

#linear model graph for hispanic faculty
indian.plot <- ggplot(data_indian, aes(x = Year, y = pChange.value, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change for American Indian Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)
#indian.plot

#percentage of each year
indian.percent <- ggplot(data_indian, aes(x = Year, y = percentage, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage for American Indian Faculty each Academic Year", x = "Academic Year", y = "Percentage(%)")+
  xlim(2007, 2015)

#total percentage DIFFERENCE betweeen each year
indian.percentDiff <- ggplot(data_indian, aes(x = Year, y = pDiff, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Difference for American Indian Faculty", x = "Academic Year", y = "Percentage Diff(%)")+
  xlim(2007, 2015)

indian.percent
indian.percentDiff

#Pacific Islander (N/A no resignations)
data_islander<- filter(all, variable == "Pacific.Islander")

#linear model graph for hispanic faculty
islander.plot <- ggplot(data_islander, aes(x = Year, y = pChange.value, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change for Pacific Islander Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)

#percentage of each year
islander.percent <- ggplot(data_islander, aes(x = Year, y = percentage, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage for Pacific Islander Faculty each Academic Year", x = "Academic Year", y = "Percentage(%)")+
  xlim(2007, 2015)

#total percentage DIFFERENCE betweeen each year
islander.percentDiff <- ggplot(data_islander, aes(x = Year, y = pDiff, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Difference for Pacific Islander Faculty", x = "Academic Year", y = "Percentage Diff(%)")+
  xlim(2007, 2015)

#islander.plot

islander.percent
islander.percentDiff


#Multi racial
data_multi<- filter(all, variable == "Multi.Racial")

#linear model graph for hispanic faculty
multi.plot <- ggplot(data_multi, aes(x = Year, y = pChange.value, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change for Multi Racial Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)

#percentage of each year
multi.percent <- ggplot(data_multi, aes(x = Year, y = percentage, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage for Multi-Racial Faculty each Academic Year", x = "Academic Year", y = "Percentage(%)")+
  xlim(2007, 2015)

#total percentage DIFFERENCE betweeen each year
multi.percentDiff <- ggplot(data_multi, aes(x = Year, y = pDiff, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Difference for Multi-Racial Faculty", x = "Academic Year", y = "Percentage Diff(%)")+
  xlim(2007, 2015)

#multi.plot
multi.percent
multi.percentDiff


#not related
#overall percentage change CHANGE for overall chart below
dat_m <- dat_m %>%
  mutate(pChange.value = (value - lag(value))/lag(value) * 100)

#overall trends between all ethnicities
g <- ggplot(dat_m, aes(x = Academic.Year, y = pChange.value, colour = as.factor(variable))) +
  geom_point()+geom_line(aes(group = variable))+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Percentage Change of Ethnicity for Resignation", x = "Academic Year", y = "Percentage Change (%)")+ 
  labs(color='Ethnicity') + ylim(-100, 700)
