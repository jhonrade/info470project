news<- read.csv("EthnicDistribution- New Hires.csv")
overall <- read.csv("EthnicDistribution - Overall.csv")
resign <- read.csv("EthnicDistribution - Resignation.csv")

library(ggplot2)
library(reshape2)
library(dplyr)

#select ethnicity
ethnicity <- resign %>%
  select(Academic.Year, White, Asian, Black, Hispanic, American.Indian, Pacific.Islander, Multi.Racial) %>%
  arrange(Academic.Year)

#melt everything together
dat_m <- melt(ethnicity, id.vars = "Academic.Year")

#add year (for graphing purposes)
dat_m <- mutate(dat_m, Year = substr(dat_m$Academic.Year, 0, 4))

#overall percentage change
dat_m <- dat_m %>%
  mutate(pChange = (value - lag(value))/lag(value) * 100)


#change type to numeric 
dat_m$value <- as.numeric(dat_m$value)
dat_m$Year <- as.numeric(dat_m$Year)



#WHITE
data_white <- filter(dat_m,variable == "White")

#linear model graph for white faculty
white.plot <- ggplot(data_white, aes(x = Year, y = pChange, colour))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm") +
  labs(title = "Percentage Change for White Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2014)

#ASIAN
data_asian <- filter(dat_m, variable == "Asian")

#linear model graph for asian faculty
asian.plot <- ggplot(data_asian, aes(x = Year, y = pChange, colour))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm") +
  labs(title = "Percentage Change for Asian Faculty", x = "Academic Year", y = "Percentage Change (%)")+ 
  xlim(2007,2014)

#BLACK
data_black<- filter(dat_m, variable == "Black")

#linear model graph for black faculty
black.plot <- ggplot(data_black, aes(x = Year, y = pChange, colour))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm") +
  labs(title = "Percentage Change for Black Faculty", x = "Academic Year", y = "Percentage Change (%)")+ 
  xlim(2007,2014)

#HISPANIC
data_hispanic<- filter(dat_m, variable == "Hispanic")

#linear model graph for hispanic faculty
hispanic.plot <- ggplot(data_hispanic, aes(x = Year, y = pChange, colour))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm") +
  labs(title = "Percentage Change for Hispanic Faculty", x = "Academic Year", y = "Percentage Change (%)")+ 
  xlim(2007,2014)

#American Indian
data_indian<- filter(dat_m, variable == "American.Indian")

#linear model graph for hispanic faculty
indian.plot <- ggplot(data_indian, aes(x = Year, y = pChange, colour))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm") +
  labs(title = "Percentage Change for Indian Faculty", x = "Academic Year", y = "Percentage Change (%)")+ 
  xlim(2007,2014)

#Pacific Islander (N/A no resignations)
data_islander<- filter(dat_m, variable == "Pacific.Islander")

#linear model graph for hispanic faculty
islander.plot <- ggplot(data_islander, aes(x = Year, y = pChange, colour))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm") +
  labs(title = "Percentage Change for Pacific Islander Faculty", x = "Academic Year", y = "Percentage Change (%)")+ 
  xlim(2007,2014)


#Multi racial
data_multi<- filter(dat_m, variable == "Multi.Racial")

#linear model graph for hispanic faculty
multi.plot <- ggplot(data_multi, aes(x = Year, y = pChange, colour))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm") +
  labs(title = "Percentage Change for Indian Faculty", x = "Academic Year", y = "Percentage Change (%)")+ 
  xlim(2007,2014)
multi.plot

#overall trends between all ethnicities
g <- ggplot(dat_m, aes(x = Academic.Year, y = pChange, colour = as.factor(variable)))
g + geom_point()+geom_line(aes(group = variable))+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Percentage Change of Ethnicity for Resignation", x = "Academic Year", y = "Percentage Change (%)")+ 
  labs(color='Ethnicity') + ylim(-100, 700)
