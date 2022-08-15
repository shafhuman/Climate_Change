
rm(list=ls())


# Climate change 
# The effect of green house gases on the planet
# and the causes behind climate change. 
## Looking at the effect of greenhouse gases on temprature.



library(plotly)
library("RColorBrewer")
library(MASS)
library(ISLR)
library(glmnet)
library(dplyr)
library(plyr)    
library(ggplot2)
library(ggridges)
library(corrplot)
library(lubridate)
library(boot)
library(ggpubr)
library(tidyverse)
library(caret)
library(Rcpp)
library(class)

climate_change <- read.csv(choose.files(),header=TRUE, na.strings = ",")
head(climate_change)
tail(climate_change)

summary(climate_change)

str(climate_change)
dim(climate_change)

sapply(climate_change, class)

# Missing values 
sum(is.na(climate_change)) # no missing values

# Histogram of Temprature.
hist(climate_change$Temp + climate_change$Year, main = "Difference in average Temprature", xlab = "Years", col = "blue")

# Linear regression
# test and train data
# splitting dataset using subset function.
climate_train <- subset(climate_change, Year <=2005)
climate_test <- subset(climate_change, Year > 2005)

Reg_Temp <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,
               data = climate_train)
summary(Reg_Temp)
# checking for correlation
reg_corr <- cor(climate_train)
corrplot(reg_corr, method = "circle")
reg_corr

# second regression model
Reg_Temp2 <- lm(Temp ~ MEI + CO2 + CH4 + CFC.12 + TSI, 
                data = climate_train)
summary(Reg_Temp2)

# Step function

Reg_Temp_Step <- step(Reg_Temp)
summary(Reg_Temp_Step)

# prediction using step funcation.
Temp_Pred <- predict(Reg_Temp_Step, newdata = climate_test)
SSE = sum((Temp_Pred - climate_test$Temp)^2)
SST = sum((mean(climate_train$Temp) - climate_test$Temp)^2)
R2 = 1 - SSE/SST
R2


# Data exploration
# Temp and MEI, CO2, CH4, TSI

?geom_bar
?ggplotly
# Effect of MEI on Temprature from 1983 to 2008.
MEI_temp <- ggplot(climate_change, aes(x = Year, y = Temp, fill = MEI)) + 
  xlab("Year") +
  ylab("Temperature") +
  theme_minimal(base_size = 14)

barplot1 <- MEI_temp +
  geom_bar( position = "dodge", stat = "identity",color= "gray")
ggplotly(barplot1)

# Effect of CO2 on Temprature from 1983 to 2008.
CO2_temp <- ggplot(climate_change, aes(x = Year, y = Temp, fill = CO2)) + 
  xlab("Year") +
  ylab("Temperature") +
  theme_minimal(base_size = 14)

barplot2 <- CO2_temp +
  geom_bar( position = "dodge", stat = "identity",color= "gray")
ggplotly(barplot2)



# Effect of CH4 on Temprature from 1983 to 2008.
CH4_temp <- ggplot(climate_change, aes(x = Year, y = Temp, fill = CH4)) + 
  xlab("Year") +
  ylab("Temperature") +
  theme_minimal(base_size = 14)

barplot3 <- CH4_temp +
  geom_bar( position = "dodge", stat = "identity",color= "gray")
ggplotly(barplot3)


# Effect of TSI on Temprature from 1983 to 2008.
TSI_temp <- ggplot(climate_change, aes(x = Year, y = Temp, fill = TSI)) + 
  xlab("Year") +
  ylab("Temperature") +
  theme_minimal(base_size = 14)

barplot4 <- TSI_temp +
  geom_bar(  position = "dodge", stat = "identity",color= "gray")
ggplotly(barplot4)


# Labeling months

climate_change$Month[climate_change$Month == 1]<- "January"
climate_change$Month[climate_change$Month == 2]<- "February"
climate_change$Month[climate_change$Month == 3]<- "March"
climate_change$Month[climate_change$Month == 4]<- "April"
climate_change$Month[climate_change$Month == 5]<- "May"
climate_change$Month[climate_change$Month == 6]<- "June"
climate_change$Month[climate_change$Month == 7]<- "July"
climate_change$Month[climate_change$Month == 8]<- "August"
climate_change$Month[climate_change$Month == 9]<- "September"
climate_change$Month[climate_change$Month == 10]<- "October"
climate_change$Month[climate_change$Month == 11]<- "November"
climate_change$Month[climate_change$Month == 12]<- "December"

head(climate_change)

# line plot of months/years and temperature

climate_change_ymt <- climate_change %>%
  mutate(year_month = ymd(paste(climate_change$Year, climate_change$Month, truncated = 1))) 

plot <- ggplot(climate_change_ymt, aes(year_month, Temp)) + 
  geom_line() + 
  geom_smooth(se=FALSE, linetype = "dotted") + 
  labs(title = "Temperature (1983-2008)",
       x = "Year", 
       y = "Temperature") +
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(plot)


# Variations of MEI, CO2, CH4 and N2O by year 

par(mfrow=c(2,2))
plot1 <-  ggplot(climate_change_ymt, aes(year_month, MEI))+geom_line(colour="blue")+geom_smooth(method = "lm")+ggtitle("MEI")
plot2 <-  ggplot(climate_change_ymt, aes(year_month, CO2))+geom_line()+geom_smooth(method = "lm")+ggtitle("Carbon Dioxide")
plot3 <-  ggplot(climate_change_ymt, aes(year_month, CH4))+geom_line(colour="green")+geom_smooth(method = "lm")+ggtitle("Methane")
plot4 <-  ggplot(climate_change_ymt, aes(year_month, N2O))+geom_line(colour="red")+ggtitle("Nitrous Oxide")
graph_arrange <- ggarrange(plot1, plot2, plot3, plot4 + rremove("x.text"), ncol = 2, nrow = 2)
annotate_figure(graph_arrange, top = text_grob("Variations of MEI, CO2, CH4 and N2O by year ", 
                                               color = "blue", face = "bold", size = 14))

# Variations of CFC.11, CFC.12, Total Solar Irradiance (TSI) and Aerosolos by year

plot5 <-  ggplot(climate_change_ymt, aes(year_month, CFC.11))+geom_line(colour="blue")+ggtitle("CFC.11") + ylab("CFC.11")
plot6 <-  ggplot(climate_change_ymt, aes(year_month, CFC.12))+geom_line(colour="green")+ggtitle("CFC.12") + ylab("CFC.12")
plot7 <-  ggplot(climate_change_ymt, aes(year_month, TSI))+geom_line(colour="red")+ggtitle("TSI")
plot8 <-  ggplot(climate_change_ymt, aes(year_month, Aerosols))+geom_line(colour="magenta")+ggtitle("Aerosols")
grapgh_arrange <- ggarrange(plot5, plot6, plot7, plot8+ rremove("x.text"), ncol = 2, nrow = 2)
annotate_figure(grapgh_arrange, top = text_grob("Vartations of CFC.11,CFC.12, TSI and Aerosols by year", color = "blue", face = "bold", size = 14))

