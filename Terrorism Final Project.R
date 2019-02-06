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
statecount <- read.csv("file:///C:/Users/Joseph/Documents/R/statecount.csv")

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

GTDslvlinj <- GTD %>%
  select(provstate, nwound) %>% 
  group_by(provstate) %>%
  summarise(nWounds = sum(nwound, na.rm = TRUE)) 

GTDslvlinj$provstate <- as.character(GTDslvlinj$provstate)

ggplot(GTDslvlinj, aes(state=provstate, fill=nWounds)) +
  geom_statebins() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Injured", option = "inferno", direction = -1) +
  labs(title="Number of Injuries by Terrorism from 1970-2016") +
  theme_statebins(legend_position = "right") +
  theme(plot.title = element_text(size = 16, hjust = 0)) +
  theme(plot.margin = margin(30,30,30,30))

# Statebin (locational) of number of injuries mines 9/11 - Twin Tower Data

GTDslvlfinj <- GTD %>%
  select(provstate, nwound) %>%
  filter(nwound <= 752) %>%
  group_by(provstate) %>%
  summarise(nWounds = sum(nwound, na.rm = TRUE)) 

GTDslvlfinj$provstate <- as.character(GTDslvlfinj$provstate)

ggplot(GTDslvlfinj, aes(state=provstate, fill=nWounds)) +
  geom_statebins() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Injured (Filtered)", option = "inferno", direction = -1) +
  labs(title="Number of Injuries by Terrorism from 1970-2016") +
  theme_statebins(legend_position = "right") +
  theme(plot.title = element_text(size = 16, hjust = 0)) +
  theme(plot.margin = margin(30,30,30,30))

# Statebin (locational) number of attacks by ideology

# Jihadist

GTDJihad <- GTD %>%
  filter(gtype == "Jihadist") %>%
  group_by(provstate) %>%
  select(provstate, gtype) %>% 
  gather(variable, value) %>%
  count(variable, value)

GTDJihad <- rbind(GTDJihad, statecount)

GTDJihad <- GTDJihad %>%
  count(variable, value)

ggplot(GTDJihad, aes(state = value, fill = n)) +
  geom_statebins() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Attacks", option = "inferno", direction = -1) +
  labs(title="Number of Terrorism Attacks by Jihadist from 1970-2016") +
  theme_statebins(legend_position = "right") +
  theme(plot.title = element_text(size = 16, hjust = 0)) +
  theme(plot.margin = margin(30,30,30,30))

# Right Wing

GTDRight <- GTD %>%
  filter(gtype == "Right-Wing") %>%
  group_by(provstate) %>%
  select(provstate, gtype) %>% 
  gather(variable, value) %>%
  count(variable, value)

ggplot(GTDRight, aes(state = value, fill = n)) +
  geom_statebins() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Attacks", option = "inferno", direction = -1) +
  labs(title="Number of Terrorism Attacks by Right-Wing Groups from 1970-2016") +
  theme_statebins(legend_position = "right") +
  theme(plot.title = element_text(size = 16, hjust = 0)) +
  theme(plot.margin = margin(30,30,30,30))

# Left Wing

GTDLeft <- GTD %>%
  filter(gtype == "Left-Wing") %>%
  group_by(provstate) %>%
  select(provstate, gtype) %>% 
  gather(variable, value) %>%
  count(variable, value)

ggplot(GTDRight, aes(state = value, fill = n)) +
  geom_statebins() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Attacks", option = "inferno", direction = -1) +
  labs(title="Number of Terrorism Attacks by Left-Wing Groups from 1970-2016") +
  theme_statebins(legend_position = "right") +
  theme(plot.title = element_text(size = 16, hjust = 0)) +
  theme(plot.margin = margin(30,30,30,30))

# Ecoterrorism

GTDEco <- GTD %>%
  filter(gtype == "Left-Wing") %>%
  group_by(provstate) %>%
  select(provstate, gtype) %>% 
  gather(variable, value) %>%
  count(variable, value)

ggplot(GTDEco, aes(state = value, fill = n)) +
  geom_statebins() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Attacks", option = "inferno", direction = -1) +
  labs(title="Number of Terrorism Attacks by Eco-Terror Groups from 1970-2016") +
  theme_statebins(legend_position = "right") +
  theme(plot.title = element_text(size = 16, hjust = 0)) +
  theme(plot.margin = margin(30,30,30,30))

# White Supremacy

GTDWhite <- GTD %>%
  filter(grace == "White") %>%
  group_by(provstate) %>%
  select(provstate, gtype) %>% 
  gather(variable, value) %>%
  count(variable, value)

ggplot(GTDWhite, aes(state = value, fill = n)) +
  geom_statebins() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Attacks", option = "inferno", direction = -1) +
  labs(title="Number of Terrorism Attacks by White Supremacist Groups from 1970-2016") +
  theme_statebins(legend_position = "right") +
  theme(plot.title = element_text(size = 16, hjust = 0)) +
  theme(plot.margin = margin(30,30,30,30))

# Black Supremacy

GTDBlack <- GTD %>%
  filter(grace == "Black") %>%
  group_by(provstate) %>%
  select(provstate) %>% 
  gather(variable, value) %>%
  count(variable, value)

ggplot(GTDBlack, aes(state = value, fill = n)) +
  geom_statebins() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Attacks", option = "inferno", direction = -1) +
  labs(title="Number of Terrorism Attacks by Black Supremacist Groups from 1970-2016") +
  theme_statebins(legend_position = "right") +
  theme(plot.title = element_text(size = 16, hjust = 0)) +
  theme(plot.margin = margin(30,30,30,30))

# Nationalist/Independence Movements

GTDNat <- GTD %>%
  filter(gtype == "Nationalist") %>%
  group_by(provstate) %>%
  select(provstate) %>% 
  gather(variable, value) %>%
  count(variable, value) %>%
  complete(value)

ggplot(GTDNat, aes(state = value, fill = n)) +
  geom_statebins() +
  coord_equal() +
  viridis::scale_fill_viridis(name = "Attacks", option = "inferno", direction = -1) +
  labs(title="Number of Terrorism Attacks by Nationalist/Independence Movements from 1970-2016") +
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

# Plot of total wounded 

totalwounded <- GTD %>%
  group_by(iyear) %>%
  summarise(nWounds = sum(nwound, na.rm = TRUE))

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

ggplot(data = filtertotalwounded, aes(x = iyear, y = nWounds)) +
  labs(title = "US Terrorist attack injuries between 1970-2016",
       x ='Year', y ='Number of Deaths') +
  geom_bar(stat = "identity", color = "black", fill = "tomato3") +
  theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
  theme(strip.text = element_text(size = 16, face = "bold"))

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
#               deaths by type/ideaology/weapon/target                        #
#                                                                             #
###############################################################################

# Deaths by weapon type

deathsbytype <- GTD %>%
  select(nkill, weaptype1_txt) %>%
  group_by(weaptype1_txt) %>%
  summarise(nKills = sum(nkill, na.rm = TRUE))

  ggplot(data = deathsbytype, aes(x = weaptype1_txt, y = nKills)) +
    labs(title = "US Terrorist deaths by Weapon type",
         x ='Weapon Types', y ='Number of Deaths') +
    geom_bar(stat = "identity", color = "black", fill = "tomato3") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) + 
    theme(strip.text = element_text(size = 16, face = "bold"))
  
  # Deaths by weapon type (filtered for 9/11 data)
  
  fdeathsbytype <- GTD %>%
    filter(nkill <= 200) %>%
    select(nkill, weaptype1_txt) %>%
    group_by(weaptype1_txt) %>%
    summarise(nKills = sum(nkill, na.rm = TRUE))
  
  ggplot(data = fdeathsbytype, aes(x = weaptype1_txt, y = nKills)) +
    labs(title = "US Terrorist deaths by Weapon type",
         x ='Weapon Types', y ='Number of Deaths') +
    geom_bar(stat = "identity", color = "black", fill = "tomato3") +
    theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
    theme(strip.text = element_text(size = 16, face = "bold"))
  
  # Deaths by Ideology 
  
  deathsbyid <- GTD %>%
    select(nkill, gtype) %>%
    group_by(gtype) %>%
    summarise(nKills = sum(nkill, na.rm = TRUE))
  
  ggplot(data = deathsbyid, aes(x = gtype, y = nKills)) +
    labs(title = "US Terrorist deaths by Ideology",
         x ='Ideology', y ='Number of Deaths') +
    geom_bar(stat = "identity", color = "black", fill = "tomato3") +
    theme(axis.text.x = element_text(hjust = 1, size = 12)) + 
    theme(strip.text = element_text(size = 16, face = "bold"))
  
  # Deaths by Ideology (9/11 twin tower information filtered out)
  
  fdeathsbyid <- GTD %>%
    filter(nkill <= 200) %>%
    select(nkill, gtype) %>%
    group_by(gtype) %>%
    summarise(nKills = sum(nkill, na.rm = TRUE))
  
  ggplot(data = fdeathsbyid, aes(x = gtype, y = nKills)) +
    labs(title = "US Terrorist deaths by Ideology",
         x ='Ideology', y ='Number of Deaths') +
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
cor.test(incpop$iyear, incpop$n, method = "pearson")

ggscatter(incpop, y = "n", x = "iyear", 
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
incidentmodel3 <- lm(data = incpop, n ~ iyear + I(iyear^2))
summary(incidentmodel3)

ggplot(data = incpop, aes(x = iyear, y = n)) +
  geom_point() +
  geom_smooth(method="lm",formula= y ~ x + I(x^2))+
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


### Deaths modeling

death <- totaldeaths 
deathpop <- left_join(pop, death)

# Pearson correlation test of US population vs. number of incidents
cor.test(deathpop$pop, sevpop$n, method = "pearson")

ggscatter(deathpop, y = "nKills", x = "pop", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          ylab = "Terrorist Death and Wounded Counts", xlab = "U.S. Population")

# Linear Regression of population vs incidents
deathmodel1 <- lm(data = deathpop, nKills ~ pop)
summary(deathmodel1)

ggplot(data = deathpop, aes(x = pop, y = nKills)) +
  geom_point() +
  geom_smooth(method="lm",formula= y ~ x)+
  scale_y_continuous(limits = c(-40, 3100))

# Polynomial Regression of population vs incidents
deathmodel2 <- lm(data = deathpop, nKills ~ pop + I(pop^2))
summary(deathmodel2)

ggplot(data = deathpop, aes(x = pop, y = nKills)) +
  geom_point() +
  geom_smooth(method="lm",formula= y ~ x + I(x^2))+
  scale_y_continuous(limits = c(-40, 3100))

# Multi-Polynomial Regression of Population vs incidents
deathmodel3 <- lm(data = deathpop, nKills ~ pop + I(pop^2) + I(pop^3))
summary(deathmodel3)

ggplot(data = deathpop, aes(x = pop, y = nKills)) +
  geom_point() +
  geom_smooth(method="lm",formula= y ~ x + I(x^2) + I(x^3))+
  scale_y_continuous(limits = c(-50, 3500))


anova(deathmodel1, deathmodel2, test = "F")
anova(deathmodel2, deathmodel3, test = "F")

outlierTest(deathmodel1)

deathinfl1 <- influencePlot(deathmodel1, id.method = "noteworthy")


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
predictModel2 <- train(data = incpop, n ~ iyear + I(iyear^2), method = 'lm', trControl = trainmethod)


# New "estimated" populations of the US through 2025 for prediction
future <- read.csv("file:///C:/Users/Joseph/Documents/R/newpop.csv")

# create a row for number of attacks in the future dataset
future$n <- NA

# make a df for each pediction model for thier predicted values for comparison
f2 <- future

#Predict the number of attacks per model and insert them in the appropriate df
f2$n <- predict(object = predictModel2, newdata = f2)

#plot these models for comparison and diagnosis
ggplot() +
  geom_line(data = incpop, aes(x = iyear, y = n, col='Observed Data')) +
  geom_point(data = f2, aes(x = iyear, y = n, col='Model - 2nd order polynomial')) +
  labs(title='Predicted for terrorist attacks') +
  theme(legend.position = c(0.5, 0.85)) +
  labs(x = 'Year', y = 'Number of Terrorist Attacks', color = 'Legend') +
  scale_x_continuous(breaks = seq(1970, 2025, 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

summary(f2)

### Deaths Prediction Modeling
# using our linear regression model and our polynomial model

tsAttacks <- ts(incidents)
ts.plot(tsAttacks)

# Train a prediction model
trainmethod <- trainControl(method="cv", number=5, returnData=TRUE, returnResamp='all')
dpredictModel2 <- train(data = deathpop, nKills ~ pop + I(pop^2), method = 'lm', trControl = trainmethod)


# New "estimated" populations of the US through 2025 for prediction
future <- read.csv("file:///C:/Users/Joseph/Documents/R/newpop.csv")

# create a row for number of attacks in the future dataset
future$n <- NA

# make a df for each pediction model for thier predicted values for comparison
df2 <- future

# Predict the number of attacks per model and insert them in the appropriate df
df2$n <- predict(object = dpredictModel2, newdata = sf2)


# plot these models for comparison and diagnosis
ggplot() +
  geom_line(data = deathpop, aes(x = iyear, y = nKills, col='Observed Data')) +
  geom_point(data = df1, aes(x = iyear, y = n, col='Model 1 - Linear')) +
  geom_point(data = df2, aes(x = iyear, y = n, col='Model 2 - 2nd order polynomial')) +
  geom_point(data = df3, aes(x = iyear, y = n, col='Model 3 - 3rd order polynomial')) +
  labs(title='Predicted for Deaths from terrorist attacks') +
  theme(legend.position = c(0.25, 0.85)) +
  labs(x = 'Year', y = 'Number of Deaths', color = 'Legend') +
  scale_x_continuous(breaks = seq(1970, 2025, 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

### Deaths (filtered for 9/11) Prediction Modeling
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
fdpredictModel1 <- train(data = fdeathpop, nKills ~ pop, method = "lm", trControl = trainmethod)
fdpredictModel2 <- train(data = fdeathpop, nKills ~ pop + I(pop^2), method = 'lm', trControl = trainmethod)
fdpredictModel3 <- train(data = fdeathpop, nKills ~ pop + I(pop^2) + I(pop^3), method = "lm", trControl = trainmethod)

# New "estimated" populations of the US through 2025 for prediction
future <- read.csv("file:///C:/Users/Joseph/Documents/R/newpop.csv")

# create a row for number of attacks in the future dataset
future$n <- NA

# make a df for each pediction model for thier predicted values for comparison
fdf1 <- future
fdf2 <- future
fdf3 <- future

# Predict the number of attacks per model and insert them in the appropriate df
fdf1$n <- predict(object = fdpredictModel1, newdata = sf1)
fdf2$n <- predict(object = fdpredictModel2, newdata = sf2)
fdf3$n <- predict(object = fdpredictModel3, newdata = sf3)

# plot these models for comparison and diagnosis
ggplot() +
  geom_line(data = fdeathpop, aes(x = iyear, y = nKills, col='Observed Data')) +
  geom_point(data = fdf1, aes(x = iyear, y = n, col='Model 1 - Linear')) +
  geom_point(data = fdf2, aes(x = iyear, y = n, col='Model 2 - 2nd order polynomial')) +
  geom_point(data = fdf3, aes(x = iyear, y = n, col='Model 3 - 3rd order polynomial')) +
  labs(title='Predicted for Deaths (filtered) from terrorist attacks') +
  theme(legend.position = c(0.25, 0.85)) +
  labs(x = 'Year', y = 'Number of Deaths', color = 'Legend') +
  scale_x_continuous(breaks = seq(1970, 2025, 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
