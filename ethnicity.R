news<- read.csv("EthnicDistribution- New Hires.csv")
overall <- read.csv("EthnicDistribution - Overall.csv")
resign <- read.csv("EthnicDistribution - Resignation.csv")

library(ggplot2)
library(reshape2)
library(dplyr)

#resignation
resign <- resign %>%
  select(Academic.Year, White, Asian, Black, Hispanic, American.Indian, Pacific.Islander, Multi.Racial) %>%
  arrange(Academic.Year) %>%
  mutate(id = "Resignation")

#overall
overall <- overall %>%
  select(Academic.Year, White, Asian, Black, Hispanic, American.Indian, Pacific.Islander, Multi.Racial) %>%
  arrange(Academic.Year) %>%
  mutate(id = "Overall")

#New hires
new <- news %>%
  select(Academic.Year, White, Asian, Black, Hispanic, American.Indian, Pacific.Islander, Multi.Racial) %>%
  arrange(Academic.Year) %>%
  mutate (id= "New.Hires")

types <- rbind(resign,overall,new)
all <- melt(types, id.vars = c("Academic.Year", "id"))

all <- all %>%
  group_by(id, variable) %>%
  mutate(pChange = (value - lag(value))/lag(value) * 100)

#add year (for graphing purposes)
all <- all %>%
  mutate(Year = substr(Academic.Year, 0, 4))

#change type to numeric 
all$value <- as.numeric(all$value)
all$Year <- as.numeric(all$Year)

#WHITE
data_white <- data_white %>%
  filter(variable == "White") %>%
  na.omit()

#linear model graph for white faculty
white.plot <- ggplot(data_white, aes(x = Year, y = pChange, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change for White Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)

white.plot

#ASIAN
data_asian <- filter(all, variable == "Asian")

#linear model graph for asian faculty
asian.plot <- ggplot(data_asian, aes(x = Year, y = pChange, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change for Asian Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)

asian.plot

#BLACK
data_black<- filter(all, variable == "Black")

#linear model graph for black faculty
black.plot <- ggplot(data_black, aes(x = Year, y = pChange, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change for Black Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)

black.plot

#HISPANIC
data_hispanic<- filter(all, variable == "Hispanic")

#linear model graph for hispanic faculty
hispanic.plot <- ggplot(data_hispanic, aes(x = Year, y = pChange, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change for Hispanic Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)

hispanic.plot

#American Indian
data_indian<- filter(all, variable == "American.Indian")

#linear model graph for hispanic faculty
indian.plot <- ggplot(data_indian, aes(x = Year, y = pChange, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change for American Indian Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)
indian.plot

#Pacific Islander (N/A no resignations)
data_islander<- filter(all, variable == "Pacific.Islander")

#linear model graph for hispanic faculty
islander.plot <- ggplot(data_islander, aes(x = Year, y = pChange, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change for Pacific Islander Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)

islander.plot

#Multi racial
data_multi<- filter(all, variable == "Multi.Racial")

#linear model graph for hispanic faculty
multi.plot <- ggplot(data_multi, aes(x = Year, y = pChange, colour = id, group=id))+
  geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Percentage Change for Multi Racial Faculty", x = "Academic Year", y = "Percentage Change (%)")+
  xlim(2007, 2015)

multi.plot



#not related
#overall percentage change CHANGE for overall chart below
dat_m <- dat_m %>%
  mutate(pChange = (value - lag(value))/lag(value) * 100)

#overall trends between all ethnicities
g <- ggplot(dat_m, aes(x = Academic.Year, y = pChange, colour = as.factor(variable)))
g + geom_point()+geom_line(aes(group = variable))+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Percentage Change of Ethnicity for Resignation", x = "Academic Year", y = "Percentage Change (%)")+ 
  labs(color='Ethnicity') + ylim(-100, 700)
