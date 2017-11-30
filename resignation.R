news<- read.csv("EthnicDistribution- New Hires.csv")
overall <- read.csv("EthnicDistribution - Overall.csv")
resign <- read.csv("EthnicDistribution - Resignation.csv")

library(ggplot2)
library(reshape2)
library(dplyr)

#select ethnicity
ethnicity <- resign %>%
  select(Academic.Year, White, Asian, Black, Hispanic, American.Indian, Pacific.Islander, Multi.Racial)

#melt everything together
dat_m <- melt(ethnicity, id.vars = "Academic.Year")

#change type to numeric 
dat_m$value <- as.numeric(dat_m$value)

#find percentage change for white faculty
data_white <- dat_m %>% 
  filter(variable == "White") %>%
  group_by(variable) %>%
  mutate(pChange = (value - lag(value))/lag(value) * 100) %>%
  mutate(Year = substr(data_white$Academic.Year, 0, 4))

#convert year to numeric value
data_white$Year = as.numeric(data_white$Year)

#linear model graph for white faculty
g <- ggplot(data_white, aes(x = Year, y = pChange, colour))
g+ geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm") +
  labs(title = "Percentage Change for White Faculty", x = "Academic Year", y = "Percentage Change (%)")

#asian linear model
data_asian <- dat_m %>% 
  filter(variable == "Asian") %>%
  group_by(variable) %>%
  mutate(pChange = (value - lag(value))/lag(value) * 100) %>%
  mutate(Year = substr(data_white$Academic.Year, 0, 4))

#linear model graph for asian faculty
g <- ggplot(data_asian, aes(x = Year, y = pChange, colour))
g+ geom_point()+ xlab("Academic Year") + ylab("Percentage Change (%)") + 
  theme_bw() + geom_smooth(method = "lm") +
  labs(title = "Percentage Change for Asian Faculty", x = "Academic Year", y = "Percentage Change (%)")+ 
  xlim(2007,2014)



#overall trends between all ethnicities
g <- ggplot(dat_m, aes(x = Academic.Year, y = pChange, colour = as.factor(variable)))
g + geom_point()+geom_line(aes(group = variable))+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Percentage Change of Ethnicity for Resignation", x = "Academic Year", y = "Percentage Change (%)")+ 
  labs(color='Ethnicity') + ylim(-100, 700)
