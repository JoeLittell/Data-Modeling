###############################################################################
###############################################################################
##                                                                           ##
##   Duke University - Masters of Interdisciplinary Data Science             ##
##   IDS 702 - Modeling and Data Represenation                               ##
##   Instructor: Dr. Jerry Reiter                                            ##
##   Team Members: Joe Littell                                               ##
##                                                                           ##
##   This is for the final project for IDS 702. The project is               ##
##   utilizing the University of Maryland's Global Terrorism Database.       ##
##   This database utilizes all known attacks since 1970 spanning            ##
##   every nation of the world. For my purposes I will only be               ##
##   utilzing the attacks in the United States.                              ##
##                                                                           ##
##   The goal of this project is to determine if legislation following       ##
##   major terrorist events has made us safe as a country.                   ##
##                                                                           ##
###############################################################################
###############################################################################

# Libraries needed
library(tidyverse)      # required for fancy plots
library(dplyr)          # grep/text cleaning
library(ggplot2)        # Visualization Package
library(usmap)          # plotting against the United States
library(fiftystater)    # for plotting all 50 states using ggplot
library(reshape2)       # for ease of use in ggplots
library(tidyr)          # For gather function
library(ggfortify)      # Ease of use for forcasting graphics
library(forecast)       # required for multiple forecasting techniques
library(statebins)      # Used in created state density visualizations
library(ggpubr)         # Additional plots for ggplot
library(car)            # used in model dignosis

###############################################################################
#                                                                             #
#   Code Book/Legend                                                          #
#                                                                             #
#   Variable            Description                                           #
#                                                                             #
#   iyear               year the attack occured                               #
#   imonth              month the attack occured                              #
#   iday                day the attack occured                                #
#   latitude            IRT location of attack                                #
#   longitude           IRT location of attack                                #
#   success             1 if successfuly attacked target, 0 otherwise         #
#   suicide             1 if it was a suicide attack, 0 otherwise             #
#   attacktype1         1 - Assassination                                     #
#                       2 - Armed Assault                                     #
#                       3 - Bombing/Explosion                                 #
#                       4 - Hijacking                                         #
#                       5 - Hostage Taking (barricade)                        #
#                       6 - Hostage Taking (kidnapping)                       #
#                       7 - Facility/Infrastructure Attack                    #
#                       8 - Unarmed Assualt                                   #
#                       9 - Unknown                                           #
#    targtype1          1 - Business                                          #
#                       2 - Government (general)                              #
#                       3 - Police                                            #
#                       4 - Military                                          #
#                       5 - Abortion Related                                  #
#                       6 - Airports & Aircraft                               #
#                       7 - Government (Diplomatic)                           #
#                       8 - Educational Institution                           #
#                       9 - Food or Water Supply                              #
#                       10- Journalists & Media                               #
#                       11- Maritime                                          #
#                       12- NGO                                               #
#                       13- Other                                             #
#                       14- Private Citizens & Property                       #
#                       15- Religious Figures/Institutions                    #
#                       16- Telecommunication                                 #
#                       17- Terrorists/Non-State Militia                      #
#                       18- Tourists                                          #
#                       19- Transportation                                    #
#                       20- Unknown                                           #
#                       21- Utilities                                         #
#                       22- Violent Political Party                           #
#     gname             Group's name if known                                 #
#     gtype             Type of group (Nationalist, Racial, Jihadi, etc.)     #
#     grace             Group's race if Racial Supremist                      #
#     weaptype1         1 - Biological                                        #
#                       2 - Chemical                                          #
#                       3 - Radiological                                      #
#                       4 - N/A                                               #
#                       5 - Firearms                                          #
#                       6 - Explosives/Bombs/Dynamite                         #
#                       7 - Fake Weapons                                      #
#                       8 - Incendiary                                        #
#                       9 - Melee                                             #
#                       10- Vehicle (not to include vbied)                    #
#                       11- Sabotage Equipment                                #
#                       12- Other                                             #
#                       13- Unknown                                           #
#     nkill             Number of people killed in attack                     #
#     nkillus           Number of US citizen's killed                         #
#     nkillter          Number of terrorist suspsects killed                  #
#     nwound            Number of people wounded in attack                    #
#     nwoundus          Number of US citizen's wounded                        #
#     nwoundte          Number of terrorist suspects wounded                  #
#     provstate         The name of the state or province                     #
#                                                                             #
###############################################################################

###############################################################################
#                                                                             #
#               Initial Exploritory Data Analysis                             #
#                                                                             #
###############################################################################

# Read in the data
GTD <- read.csv("file:///C:/Users/Joseph/Documents/R/IDS 702/US Terrorism.csv")

# initial check to ensure the data was read in correctly
dim(GTD)
head(GTD)
skimr::skim(GTD)

# arrange by date
GTD <- GTD %>% 
  arrange(iyear, imonth, iday)
  

# set the map_data in GG Plot to the state level
WorldData <- map_data('world')


###############################################################################
#                                                                             #
#               Location based analysis                                       #
#                                                                             #
###############################################################################


# Incidents by location 
ggplot() + geom_polygon(data = WorldData, aes(x = long, y = lat, group = group),
                        fill = "black", color = "white") + 
  coord_fixed(1.3) + 
  geom_point(data = GTD, aes(x = longitude, y = latitude), color = "tomato3", fill = "tomato3") +
  labs(title = "Location of Individual Attacks since 1970", fill = NULL) +
  scale_x_continuous(limits = c(-170, -50), expand = c(0, 0)) +
  scale_y_continuous(limits = c(10,75), expand = c(0, 0))

# Statebin (locational) of number of incidents

GTDslvlinc <- GTD %>%
  group_by(provstate) %>%
  select(provstate) %>%
  gather(variable, value) %>%
  count(variable, value)


ggplot(GTDslvlinc, aes(state = value, fill = n)) +
  geom_statebins() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Attacks", option = "inferno", direction = -1) +
  labs(title="Number of Terrorism Attacks from 1970-2016") +
  theme_statebins(legend_position = "right") +
  theme(plot.title = element_text(size = 16, hjust = 0)) +
  theme(plot.margin = margin(30,30,30,30))

# Statebin (locational) of number of deaths 

GTDslvldeaths <- GTD %>%
  select(provstate, nkill) %>% 
  group_by(provstate) %>%
  summarise(nKills = sum(nkill, na.rm = TRUE)) 

GTDslvldeaths$provstate <- as.character(GTDslvldeaths$provstate)

ggplot(GTDslvldeaths, aes(state = provstate, fill = nKills)) +
  geom_statebins() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Deaths", option = "inferno", direction = -1) +
  labs(title="Number of Deaths from Terrorist Attacks from 1970-2016") +
  theme_statebins(legend_position = "right") +
  theme(plot.title = element_text(size = 16, hjust = 0)) +
  theme(plot.margin = margin(30,30,30,30))
          
# Statebin (locational) of number of deaths (filtered)

GTDslvlfdeaths <- GTD %>%
  select(provstate, nkill) %>% 
  filter(nkill <= 200) %>%
  group_by(provstate) %>%
  summarise(nKills = sum(nkill, na.rm = TRUE)) 

GTDslvlfdeaths$provstate <- as.character(GTDslvldeaths$provstate)

ggplot(GTDslvlfdeaths, aes(state = provstate, fill = nKills)) +
  geom_statebins() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Deaths (Filtered)", option = "inferno", direction = -1) +
  labs(title="Number of Deaths from Terrorist Attacks from 1970-2016") +
  theme_statebins(legend_position = "right") +
  theme(plot.title = element_text(size = 16, hjust = 0)) +
  theme(plot.margin = margin(30,30,30,30))

# Statebin (locational) of number of injuries

ggplot(GTDslvlfdeaths, aes(state=provstate, fill=nKills)) +
  geom_statebins() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Deaths", option = "inferno", direction = -1) +
  labs(title="Number of Deaths by Terrorism from 1970-2016") +
  theme_statebins(legend_position = "right") +
  theme(plot.title = element_text(size = 16, hjust = 0)) +
  theme(plot.margin = margin(30,30,30,30))

###############################################################################
#                                                                             #
#               Incidences and deaths by time                                 #
#                                                                             #
###############################################################################


# Incidents by year
ggplot(data = GTD, aes(x = iyear)) +
  labs(title ="US Terrorist attacks between 1970-2016 by year", 
       x = "Year", y = "Number of Attacks", size = 15) + 
  geom_bar(stat='count', color = "black", fill = "tomato3") +
  theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
  theme(strip.text = element_text(size = 16, face = "bold"))

# Plot of total deaths
totaldeaths <- GTD %>% 
  group_by(iyear) %>% 
  summarise(nKills = sum(nkill, na.rm = TRUE))

ggplot(data = totaldeaths) + 
  geom_line(mapping = aes(x = iyear,y = nKills, colour = "tomato3")) +
  guides(colour=FALSE) +
  labs(title = "US Terrorist attack deaths between 1970-2016",
       x ='Year', y ='Number of Deaths')

ggplot(data = totaldeaths, aes(x = iyear, y = nKills)) +
  labs(title = "US Terrorist attack deaths between 1970-2016",
       x ='Year', y ='Number of Deaths') +
  geom_bar(stat = "identity", color = "black", fill = "tomato3") +
  theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
  theme(strip.text = element_text(size = 16, face = "bold"))

# Plot of total deaths but ommitting 9/11 - twin tower data
filtertotaldeaths <- GTD %>% 
  filter(nkill <= 200) %>%
  group_by(iyear) %>% 
  summarise(nKills = sum(nkill, na.rm = TRUE))

ggplot(data = filtertotaldeaths, aes(x = iyear, y = nKills)) +
  labs(title = "US Terrorist attack deaths between 1970-2016",
       x ='Year', y ='Number of Deaths') +
  geom_bar(stat = "identity", color = "black", fill = "tomato3") +
  theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
  theme(strip.text = element_text(size = 16, face = "bold"))

ggplot(data = Filtertotaldeaths) + 
  geom_line(mapping = aes(x = iyear,y = nKills, colour = "tomato3")) +
  guides(colour=FALSE) +
  labs(title ="US Terrorist attack deaths between 1970-2016",
       x ='Year', y ='Number of Deaths')

ggplot() + geom_polygon(data = WorldData, aes(x = long, y = lat, group = group),
                        fill = "black", color = "white") + 
  coord_fixed(1.3) + 
  geom_point(data = GTD, aes(x = longitude, y = latitude), color = "tomato3", fill = "tomato3") +
  labs(title = "Location of Individual Attacks since 1970", fill = NULL) +
  scale_x_continuous(limits = c(-170, -50), expand = c(0, 0)) +
  scale_y_continuous(limits = c(10,75), expand = c(0, 0))

# Plot the average number of deaths

meandeaths <- GTD %>% 
  group_by(iyear) %>% 
  summarise(meanKills = mean(nkill,na.rm=TRUE))

ggplot(data = meandeaths) + 
  geom_line(mapping = aes(x = iyear,y = meanKills, colour = "tomato3")) +
  guides(colour=FALSE) +
  labs(title ="US Terrorist attack average number of deaths between 1970-2016",
       x ='Year', y ='Number of Deaths')

ggplot(data = meandeaths, aes(x = iyear, y = meanKills)) +
  labs(title = "US Terrorist attack average number of deaths between 1970-2016",
       x ='Year', y ='Number of Deaths') +
  geom_bar(stat = "identity", color = "black", fill = "tomato3") +
  theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
  theme(strip.text = element_text(size = 16, face = "bold"))

# Plot the average number of deaths but ommiting 9/11 - twin tower data
filtermeandeaths <- GTD %>% 
  filter(nkill <= 200) %>%
  group_by(iyear) %>% 
  summarise(meanKills = mean(nkill,na.rm=TRUE))

ggplot(data = filtermeandeaths) + 
  geom_line(mapping = aes(x = iyear,y = meanKills, colour = "tomato3")) +
  guides(colour=FALSE) +
  labs(title ="Average number of deaths by terrorist attack in the US between 1970-2016",
       x ='Year', y ='Number of Deaths')

ggplot(data = filtermeandeaths, aes(x = iyear, y = meanKills)) +
  labs(title = "Average number of deaths by terrorist attack in the US between 1970-2016",
       x ='Year', y ='Number of Deaths') +
  geom_bar(stat = "identity", color = "black", fill = "tomato3") +
  theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
  theme(strip.text = element_text(size = 16, face = "bold"))

# Plot of total wounded 

totalwounded <- GTD %>%
  group_by(iyear) %>%
  summarise(nWounds = sum(nwound, na.rm = TRUE))

ggplot(data = totalwounded) + 
  geom_line(mapping = aes(x = iyear, y = nWounds, colour = "tomato3")) +
  guides(colour = FALSE) +
  labs(title ="US Terrorist attack injuries between 1970-2016",
       x ='Year', y ='Number of Wounded')

ggplot(data = totalwounded, aes(x = iyear, y = nWounds)) +
  labs(title = "US Terrorist attack injuries between 1970-2016",
       x ='Year', y ='Number of Deaths') +
  geom_bar(stat = "identity", color = "black", fill = "tomato3") +
  theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
  theme(strip.text = element_text(size = 16, face = "bold"))

# Plot of total wounded but ommiting the 9/11 - twin tower data

filtertotalwounded <- GTD %>%
  filter(nwound <= 752) %>%
  group_by(iyear) %>%
  summarise(nWounds = sum(nwound, na.rm = TRUE))

ggplot(data = filtertotalwounded) + 
  geom_line(mapping = aes(x = iyear, y = nWounds, colour = "tomato3")) +
  guides(colour = FALSE) +
  labs(title ="US Terrorist attack injuries between 1970-2016",
       x ='Year', y ='Number of Wounded')

ggplot(data = filtertotalwounded, aes(x = iyear, y = nWounds)) +
  labs(title = "US Terrorist attack injuries between 1970-2016",
       x ='Year', y ='Number of Deaths') +
  geom_bar(stat = "identity", color = "black", fill = "tomato3") +
  theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
  theme(strip.text = element_text(size = 16, face = "bold"))
# Plot the average number of injuries

meanwounded <- GTD %>%
  filter(nwound <= 752) %>%
  group_by(iyear) %>%
  summarise(meanWound = mean(nwound,na.rm=TRUE))

ggplot(data = meanwounded) + 
  geom_line(mapping = aes(x = iyear, y = meanWound, colour = "tomato3")) +
  guides(colour = FALSE) +
  labs(title ="US Terrorist attack average injuries between 1970-2016",
       x ='Year', y ='Number of Wounded')

# Plot the average number of injuries but ommiting the 9/11 - twin tower data
filtermeanwounded <- GTD %>%
  filter(nwound <= 752) %>%
  group_by(iyear) %>%
  summarise(meanWound = mean(nwound,na.rm=TRUE))

ggplot(data = filtermeanwounded) + 
  geom_line(mapping = aes(x = iyear, y = meanWound, colour = "tomato3")) +
  guides(colour = FALSE) +
  labs(title ="US Terrorist attack average injuries between 1970-2016",
       x ='Year', y ='Number of Wounded')

###############################################################################
#                                                                             #
#               Incidences by type/ideaology/weapon/target                    #
#                                                                             #
###############################################################################

# Total Incidents of attacks by year by type
ggplot(data = GTD, aes(x=iyear,fill=attacktype1_txt)) + 
  geom_bar() + 
  ggtitle("US Terrorist attacks between 1970-2016") +         
  labs(x = "Years", y = "Number of Attacks")

# Incidents by attack type
ggplot(GTD, aes(x = iyear)) + 
  labs(title ="US Terrorist attacks between 1970-2016 by ATTACK type", 
       x = "Years", y = "Number of Attacks", size = 15) + 
  geom_bar(color = "black", fill = "tomato3") + 
  facet_wrap(~attacktype1_txt,scales = "free", ncol = 3) + 
  theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
  theme(strip.text = element_text(size = 16, face = "bold"))

GTD.noNA <- GTD[which(GTD$targsubtype1_txt !='.'), ] 

# Incidents by target type
ggplot(GTD.noNA, aes(x = iyear)) + 
  labs(title ="US Terrorist attacks between 1970-2016 by TARGET type", 
       x = "Years", y = "Number of Attacks") + 
  geom_bar(color = "black", fill = "tomato3") + 
  facet_wrap(~targtype1_txt, ncol = 4) + 
  theme(axis.text.x = element_text(hjust = 1, size = 12)) +
  theme(strip.text = element_text(size = 16, face = "bold"))

# Incidents by weapon type
ggplot(GTD, aes(x = iyear)) + 
  labs(title ="US Terrorist attacks between 1970-2016 by WEAPON type", 
       x = "Years", y = "Number of Attacks") + 
  geom_bar(color = "grey19", fill = "tomato3") + 
  facet_wrap(~weaptype1_txt, ncol = 2) + 
  theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
  theme(strip.text = element_text(size = 15, face = "bold"))

# Incidents by weapon by year
ggplot(GTD, aes(x=iyear,fill=weaptype1_txt)) + 
  geom_bar() + 
  ggtitle("Yearly terrorist attacks by WEAPON type") + 
  labs(x = "Years", y = "Number of Attacks")

# Incidents by group type
ggplot(GTD, aes(x = iyear)) + 
  labs(title ="US Terrorist attacks between 1970-2016 by IDEOLOGY type", 
       x = "Years", y = "Number of Attacks") + 
  geom_bar(colour = "black", fill = "tomato3") + 
  facet_wrap(~gtype, ncol = 5, scales = "free_y") + 
  theme(axis.text.x = element_text(hjust = 1))+
  theme(strip.text = element_text(size = 11, face = "bold"))

# Incidents by group type by year
ggplot(GTD, aes(x=iyear,fill=gtype)) + 
  geom_bar() + 
  ggtitle("Yearly terrorist attacks by IDEOLOGY type") + 
  labs(x = "Years", y = "Number of Attacks")

###############################################################################
#                                                                             #
#               Severity                                                      #
#                                                                             #
###############################################################################

# calculating the number of attacks per year

incidents <- GTD %>%                             # From the main data set "GTD"
  group_by(iyear) %>%                            #
  select(iyear) %>%
  gather(variable, value) %>%
  count(variable, value)

# changing the column name of "value" to "iyear"

colnames(incidents)[which(names(incidents)== "value")] <- "iyear"

# creating a new data frame, taking the total deaths and total wounder calculated above
# and joining those to the number of incidents.

severity <- left_join(totaldeaths, totalwounded)
severity <- left_join(severity, incidents)

# changing the column name of "n" which denotes the number of attacks to "attacks"

colnames(severity)[which(names(severity) == "n")] <- "attacks"

# creating a "severity" variable within the data set

severity <- severity %>%
  mutate(sevOf = (nKills + nWounds) / attacks)

# Line plot of the severity of events from 1970-2016

ggplot(data = severity) + 
  geom_line(mapping = aes(x = iyear,y = sevOf, colour = "tomato3")) +
  guides(colour=FALSE) +
  labs(title ="Severity of US Terrorist Attacks between 1970-2016",
       x ='Year', y ='Deaths + Wounded / Number of Attacks')

# Bar plot of the severity of events from 1970-2016

ggplot(data = severity, aes(x = iyear, y = sevOf)) +
  labs(title = "US Terrorist attack Severity between 1970-2016",
       x ='Year', y ='Deaths + Wounded / Number of Attacks') +
  geom_bar(stat = "identity", color = "black", fill = "tomato3") +
  theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
  theme(strip.text = element_text(size = 16, face = "bold"))

# Filtered Data without Twin Tower information
incidents <- GTD %>%
  group_by(iyear) %>%
  select(iyear) %>%
  gather(variable, value) %>%
  count(variable, value)

colnames(incidents)[which(names(incidents)== "value")] <- "iyear"

filterseverity <- left_join(filtertotaldeaths, filtertotalwounded)
filterseverity <- left_join(severity, incidents)

colnames(filterseverity)[which(names(severity) == "n")] <- "attacks"

filterseverity <- filterseverity %>%
  mutate(sevOf = (filterseverity$nKills + filterseverity$nWounds) / filterseverity$attacks)

ggplot(data = filterseverity, aes(x = iyear, y = sevOf)) +
  labs(title = "US Terrorist attack Severity between 1970-2016",
       x ='Year', y ='Deaths + Wounded / Number of Attacks') +
  geom_bar(stat = "identity", color = "black", fill = "tomato3") +
  theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
  theme(strip.text = element_text(size = 16, face = "bold"))

###############################################################################
#                                                                             #
#               Linear Regression (Incidents/Time ~ Population Change)        #
#                                                                             #
###############################################################################

pop <- read.csv("file:///C:/Users/Joseph/Documents/R/uspop.csv")

# initial check to ensure the data was read in correctly
dim(pop)
head(pop)
skimr::skim(pop)

# arrange by date
pop <- pop %>% 
  select(iyear, pop) %>%
  arrange(iyear)

inc <- incidents %>%
  select(value, n)

incpop <- cbind(pop, inc, by = c("iyear" = "value"))

incpop <- incpop %>% select(-by, -value)

# Pearson correlation test of US population vs. number of incidents
cor.test(incpop$pop, incpop$n, method = "pearson")

ggscatter(incpop, y = "n", x = "pop", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          ylab = "Terrorist Attack Counts", xlab = "U.S. Population")

# Linear Regression of population vs incidents
incidentmodel1 <- lm(data = incpop, n ~ pop)
summary(incidentmodel1)

ggplot(data = incpop, aes(x = pop, y = n)) +
  geom_point() +
  geom_smooth(method="lm",formula= y ~ x)+
  scale_y_continuous(limits = c(-50, 500))

plot(y = incidentmodel1$residual, x = incidentmodel1$n, xlab = "Attacks", ylab = "Residual")
abline(0,0)

# Polynomial Regression of population vs incidents
incidentmodel2 <- lm(data = incpop, n ~ pop + I(pop^2))
summary(incidentmodel2)

ggplot(data = incpop, aes(x = pop, y = n)) +
  geom_point() +
  geom_smooth(method="lm",formula= y ~ x + I(x^2))+
  scale_y_continuous(limits = c(-30, 500))

# Multi-Polynomial Regression of Population vs incidents
incidentmodel3 <- lm(data = incpop, n ~ pop + I(pop^2) + I(pop^3))
summary(incidentmodel3)

ggplot(data = incpop, aes(x = pop, y = n)) +
  geom_point() +
  geom_smooth(method="lm",formula= y ~ x + I(x^2) + I(x^3))+
  scale_y_continuous(limits = c(-30, 500))


anova(incidentmodel1, incidentmodel2, test = "F")
anova(incidentmodel2, incidentmodel3, test = "F")

outlierTest(incidentmodel2)

infl1 <- influencePlot(incidentmodel2, id.method = "noteworthy")

#visual, without confidence interval for visiblity
ggplot(data = incpop, aes(x = pop, y = n)) +
  geom_point() +
  geom_smooth(method = "lm",formula = y ~ x, se = F) +
  geom_smooth(method = "lm",formula = y ~ x + I(x^2), se = F, col = 'green') +
  geom_smooth(method = "lm",formula = y ~ x + I(x^2) + I(x^3), se = F, col = 'red') +
  scale_y_continuous(limits = c(-30, 500))

### servity modeling

sev <- left_join(totaldeaths, totalwounded)
sev <- sev %>%
  mutate(n = nKills + nWounds)
sevpop <- left_join(pop, sev)

sevpop <- sevpop %>% select(-nKills, -nWounds)

# Pearson correlation test of US population vs. number of incidents
cor.test(sevpop$pop, sevpop$n, method = "pearson")

ggscatter(sevpop, y = "n", x = "pop", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          ylab = "Terrorist Death and Wounded Counts", xlab = "U.S. Population")

# Linear Regression of population vs incidents
sevmodel1 <- lm(data = sevpop, n ~ pop)
summary(sevmodel1)

ggplot(data = sevpop, aes(x = pop, y = n)) +
  geom_point() +
  geom_smooth(method="lm",formula= y ~ x)+
  scale_y_continuous(limits = c(-50, 500))

# Polynomial Regression of population vs incidents
sevmodel2 <- lm(data = sevpop, n ~ pop + I(pop^2))
summary(sevmodel2)

ggplot(data = sevpop, aes(x = pop, y = n)) +
  geom_point() +
  geom_smooth(method="lm",formula= y ~ x + I(x^2))+
  scale_y_continuous(limits = c(-50, 500))

# Multi-Polynomial Regression of Population vs incidents
sevmodel3 <- lm(data = sevpop, n ~ pop + I(pop^2) + I(pop^3))
summary(sevmodel3)

ggplot(data = sevpop, aes(x = pop, y = n)) +
  geom_point() +
  geom_smooth(method="lm",formula= y ~ x + I(x^2) + I(x^3))+
  scale_y_continuous(limits = c(-50, 500))


anova(sevmodel1, sevmodel2, test = "F")
anova(sevmodel2, sevmodel3, test = "F")

outlierTest(sevmodel1)

sevinfl1 <- influencePlot(sevmodel1, id.method = "noteworthy")


###############################################################################
#                                                                             #
#               Time Series Models                                            #
#                                                                             #
###############################################################################

### TS modeling of the Severity of the attacks.

tsSeverity <- ts(severity$sevOf)
ts.plot(tsSeverity)

acf(severity$sevOf)
pacf(severity$sevOf)

Severitymodel = arima(severity$sevOf, order = c(1,0,0))
Severitymodel

predict(Severitymodel, n.ahead = 5)

### TS modeling of the number of attacks per year
install.packages("astsa")
require(astsa)
tsAttacks <- ts(incidents$n)
ts.plot(tsAttacks)

acf(tsAttacks)
pacf(tsAttacks)

ts.plot(diff(tsAttacks))
acf(diff(tsAttacks))
pacf(diff(tsAttacks))


attacksmodel = arima(diff(tsAttacks), order = c(1,0,0))
attacksmodel

predict(attacksmodel, n.ahead = 5)

autoplot(tsAttacks) +
  autolayer(meanf(tsAttacks, h = 5), series ="Mean", PI = FALSE) +
  autolayer(rwf(tsAttacks, h = 5), series = "Naïve", PI = FALSE) +
  autolayer(holt(tsAttacks, h = 5), series="Holt", PI = FALSE) +
  autolayer(splinef(tsAttacks, h = 5), series="Spline", PI = FALSE) +
  ggtitle("Prediction of number of Domestic Teror Attacks") +
  xlab("Year") + ylab("Number of attacks") +
  guides(colour=guide_legend(title = "Forecast"))

summary(holt(tsAttacks, h = 5))
summary(splinef(tsAttacks, h = 5))

autoplot(holt(tsAttacks, h = 5)) +
  coord_cartesian(ylim=c(-10,200), xlim=c(25,60))

autoplot(splinef(tsAttacks, h = 5)) +
  ggtitle("Forecast from Cubic Smoothing Spline") +
  coord_cartesian(ylim=c(-10,200), xlim=c(25,60))

### TS modeling of the number of deaths per year

tsDeaths <- ts(totaldeaths$nKills)
ts.plot(tsDeaths)

acf(tsDeaths)
pacf(tsDeaths)

deathsmodel <- arima(tsDeaths, order = c(1,0,0))
deathsmodel

predict(deathsmodel, n.ahead = 5)

autoplot(tsDeaths) +
  autolayer(meanf(tsDeaths, h = 5), series ="Mean", PI = FALSE) +
  autolayer(rwf(tsDeaths, h = 5), series = "Naïve", PI = FALSE) +
  autolayer(holt(tsDeaths, h = 5), series="Holt", PI=FALSE) +
  autolayer(splinef(tsDeaths, h = 5), series="Spline", PI=FALSE) +
  ggtitle("Prediction of number of Deaths from Domestic Terror Attacks") +
  xlab("Year") + ylab("Number of attacks") +
  guides(colour=guide_legend(title = "Forecast"))

summary(holt(tsDeaths, h = 5))
summary(splinef(tsDeaths, h = 5))

autoplot(holt(tsDeaths, h = 5)) +
  coord_cartesian(ylim=c(-10,200), xlim=c(25,60))

autoplot(splinef(tsDeaths, h = 5)) +
  ggtitle("Forecast from Cubic Smoothing Spline") +
  coord_cartesian(ylim=c(-10,200), xlim=c(25,60))
 

### TS modeling of the number of deaths(filtered) per year

tsfDeaths <- ts(filtertotaldeaths$nKills)
ts.plot(tsfDeaths)

acf(tsfDeaths)
pacf(tsfDeaths)

fDeathsmodel <- arima(tsfDeaths, order = c(1,0,0))
fDeathsmodel

predict(fDeathsmodel, n.ahead = 5)

autoplot(tsfDeaths) +
  autolayer(meanf(tsfDeaths, h = 5), series ="Mean", PI = FALSE) +
  autolayer(rwf(tsfDeaths, h = 5), series = "Naïve", PI = FALSE) +
  autolayer(holt(tsfDeaths, h = 5), series="Holt", PI=FALSE) +
  autolayer(splinef(tsfDeaths, h = 5), series="Spline", PI=FALSE) +
  ggtitle("Prediction of number of Deaths from Domestic Terror Attacks") +
  xlab("Year") + ylab("Number of attacks") +
  guides(colour=guide_legend(title = "Forecast"))


autoplot(holt(tsfDeaths)) 
autoplot(splinef(tsfDeaths))                 #increase

###############################################################################
#                                                                             #
#               Linear time series modelling                                  #
#                                                                             #
###############################################################################

# using our linear regression model and our polynomial model
incpop2 <- incpop %>%
  select(-iyear)
tsALModel <- ts(incpop2)

TSscaled <- scale(tsALModel)

CoT <- diff(tsALModel)/tsALModel[-nrow(tsALModel),] * 100

autoplot(TSscaled)

autoplot(CoT)

tm2 <- tslm(n ~ pop + I(pop^2), data = tsALModel)

library(caret)

# Train a prediction model
trainmethod <- trainControl(method="cv", number=5, returnData=TRUE, returnResamp='all')
predictModel1 <- train(data = incpop, n ~ pop, method = "lm", trControl = trainmethod)
predictModel2 <- train(data = incpop, n ~ pop + I(pop^2), method = 'lm', trControl = trainmethod)
predictModel3 <- train(data = incpop, n ~ pop + I(pop^2) + I(pop^3), method = "lm", trControl = trainmethod)

# New "estimated" populations of the US through 2025 for prediction
future <- read.csv("file:///C:/Users/Joseph/Documents/R/newpop.csv")

# create a row for number of attacks in the future dataset
future$n <- NA

# make a df for each pediction model for thier predicted values for comparison
f1 <- future
f2 <- future
f3 <- future

#Predict the number of attacks per model and insert them in the appropriate df
f1$n <- predict(object = predictModel1, newdata = f1)
f2$n <- predict(object = predictModel2, newdata = f2)
f3$n <- predict(object = predictModel3, newdata = f3)

#plot these models for comparison and diagnosis
ggplot() +
  geom_line(data = incpop, aes(x = iyear, y = n, col='Observed Data')) +
  geom_point(data = f1, aes(x = iyear, y = n, col='Model 1 - Linear')) +
  geom_point(data = f2, aes(x = iyear, y = n, col='Model 2 - 2nd order polynomial')) +
  geom_point(data = f3, aes(x = iyear, y = n, col='Model 3 - 3rd order polynomial')) +
  labs(title='Predicted for terrorist attacks') +
  theme(legend.position = c(0.5, 0.85)) +
  labs(x = 'Year', y = 'Number of Terrorist Attacks', color = 'Legend') +
  scale_x_continuous(breaks = seq(1970, 2025, 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


### Severity Prediction Modeling
# using our linear regression model and our polynomial model
sevpop2 <- sevpop %>%
  select(-iyear)
tsSLModel <- ts(sevpop2)

TSscaled <- scale(tsSLModel)

CoT <- diff(tsSLModel)/tsSLModel[-nrow(tsSLModel),] * 100

autoplot(TSscaled)

autoplot(CoT)

tm2 <- tslm(n ~ pop + I(pop^2), data = tsSLModel)

# Train a prediction model
trainmethod <- trainControl(method="cv", number=5, returnData=TRUE, returnResamp='all')
spredictModel1 <- train(data = sevpop, n ~ pop, method = "lm", trControl = trainmethod)
spredictModel2 <- train(data = sevpop, n ~ pop + I(pop^2), method = 'lm', trControl = trainmethod)
spredictModel3 <- train(data = sevpop, n ~ pop + I(pop^2) + I(pop^3), method = "lm", trControl = trainmethod)

# New "estimated" populations of the US through 2025 for prediction
future <- read.csv("file:///C:/Users/Joseph/Documents/R/newpop.csv")

# create a row for number of attacks in the future dataset
future$n <- NA

# make a df for each pediction model for thier predicted values for comparison
sf1 <- future
sf2 <- future
sf3 <- future

#Predict the number of attacks per model and insert them in the appropriate df
sf1$n <- predict(object = spredictModel1, newdata = sf1)
sf2$n <- predict(object = spredictModel2, newdata = sf2)
sf3$n <- predict(object = spredictModel3, newdata = sf3)

#plot these models for comparison and diagnosis
ggplot() +
  geom_line(data = sevpop, aes(x = iyear, y = n, col='Observed Data')) +
  geom_point(data = sf1, aes(x = iyear, y = n, col='Model 1 - Linear')) +
  geom_point(data = sf2, aes(x = iyear, y = n, col='Model 2 - 2nd order polynomial')) +
  geom_point(data = sf3, aes(x = iyear, y = n, col='Model 3 - 3rd order polynomial')) +
  labs(title='Predicted for Deaths from terrorist attacks') +
  theme(legend.position = c(0.5, 0.85)) +
  labs(x = 'Year', y = 'Number of Deaths', color = 'Legend') +
  scale_x_continuous(breaks = seq(1970, 2025, 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
