## Topic
## My topic for this project is the relativity of income of households/individuals and the cost of living. This includes how income of some households/individuals affect their spending and what are some things that attribute to their income.

## Variables
##    Note: (entity) is the states(OR, CA, NY, OH, TX, FL) that I used for this project they appear in the URL
##          In some of the URLs I put what I searched at the end of (https://fred.stlouisfed.org/series/(Here)) to get my data 

##    1. Average Hourly Wage: dollar/hr, the average hourly earnings of all employees in the private sector, https://fred.stlouisfed.org/series/(Average Hourly Earnings of All Employees: Total Private in (Entity))
##    2. Unemployment Rate: Percentage, the rate of unemployment, https://fred.stlouisfed.org/series/(Entity)UR
##    3. Minimum Wage: Dollar, the state minimum wage, https://fred.stlouisfed.org/series/STTMINWG(Entity)
##    4. BA+ Degree: Percentage, ppl 18+ with BA or higher education, https://fred.stlouisfed.org/series/GCT1502(Entity)
##    5. Snap Participants: persons, ppl receiving Snap Benefits, https://fred.stlouisfed.org/series/(SNAP Benefits in (Entity))
##    6. PCE: Millions of dollars, a measure of spending on goods and services, https://fred.stlouisfed.org/series/(Entity)PCE

---------------------------------------------------------------------------------

options(max.print = 1000000)
  
## This is to merge the data sets

Merge_list <- list(Panel_Data, MinWage, Snap, BA_Degree_, Personal_Consumer_Expenditure)

test <- Reduce(function(x, y) merge(x, y, by = "Date", all.x=TRUE), Merge_list)

rm(BA_Degree_, MinWage, Snap)
----------------------------------------------------------------------------------
## This is for the scatter plot
install.packages(ggplot2)
library(ggplot2)

## plots for variables over time
## wage over time
ggplot(test, aes(x=Date, y=`AVG Wage`)) + 
  geom_point()+
  ggtitle('Wage over Time')
## We see that there is a gradual increase in the average wage throughout the years.

## Unemployment over time
ggplot(test, aes(x=Date, y=`Unemployment Rate`)) + 
  geom_point()+
  ggtitle('Unemployment over Time')
## We see that there is a sudden spike around the year 2020 that breaks the pattern and somewhat goes back to a low point.


## Total Snap participants over time
ggplot(test, aes(x=Date, y=`Snap Total`)) + 
  geom_point()+
  ylab('People with Snap Benefits')+
  ggtitle('Total Snap Participants over Time')
## We see a big increase from 2007 and 2012, then there is also a sudden increase again around 2020 for Snap participants.


## Percentage of people with BA degrees or higher over time
ggplot(test, aes(x=Date, y=`Percent_BA`)) + 
  geom_point()+
  ylab('Percentage of BA Degrees or Higher')+
  ggtitle('Percentage of people with BA degrees or higher over time')
## We can see that throughout the years from 2007 to 2023, there is a gradual increase in percentage of people with a bachelors degree between these six states.


## PCE over time
ggplot(test, aes(x= Date, y= Total_PCE))+
  geom_point()+
  ylab('Consumption')+
  ggtitle('Personal Consumption Expenditure over Time')


---------------------------------------------------------------------------------

## unemployment and wage

ggplot(test, aes(x=`Unemployment Rate`, y=`AVG Wage`)) + 
  geom_point()+
  ylab('Average Wage')+
  ggtitle('Effect Unemployment has on Average Wage')


---------------------------------------------------------------------------------
## wage to people with snap
  
  
plot(test$`AVG Wage`, test$`Snap Total`)

plot(test$`AVG Wage`, test$CA_Snap,
     main = "Snap Participants Compared to Unemployment Rate",
     xlab = "Year",
     ylab = "People with Snap Benefits",
     ylim = c(0, 5000000))
points(test$`AVG Wage`, test$OH_Snap,
       col = 'blue')

points(test$`AVG Wage`, test$FL_Snap,
       col = 'red')

points(test$`AVG Wage`, test$OR_Snap,
       col = 'green')

points(test$`AVG Wage`, test$TX_Snap,
       col = 'darkorchid')

points(test$`AVG Wage`, test$NY_Snap,
       col = 'brown4')

---------------------------------------------------------------------------------

##  wage to PCE

ggplot(test, aes(x=`AVG Wage`, y=Total_PCE))+
  geom_point()+
  ylab('Total Consumption')+
  ggtitle('Wage to Spending')
---------------------------------------------------------------------------------
  
## Unemployment to Snap
ggplot(test, aes(x=`Unemployment Rate`, y=`Snap Total`))+
  geom_point()+
  ylab('People with Snap Benefits')+
  geom_smooth(method='lm', se=FALSE)+
  ggtitle('Unemployment to People Receiving Snap Benefits')


---------------------------------------------------------------------------------  
## Descriptive Stats

## Unemployment
mean(test$`Unemployment Rate`)

median(test$`Unemployment Rate`)

sd(test$`Unemployment Rate`)

max(test$`Unemployment Rate`)

min(test$`Unemployment Rate`)


## Average wage

mean(test$`AVG Wage`, na.rm = TRUE)

median(test$`AVG Wage`, na.rm = TRUE)

sd(test$`AVG Wage`, na.rm = TRUE)

max(test$`AVG Wage`, na.rm = TRUE)

min(test$`AVG Wage`, na.rm = TRUE)


## Snap total

mean(test$`Snap Total`, na.rm = TRUE)

median(test$`Snap Total`, na.rm = TRUE)

sd(test$`Snap Total`, na.rm = TRUE)

max(test$`Snap Total`, na.rm = TRUE)

min(test$`Snap Total`, na.rm = TRUE)


## BA+ Degree 

mean(test$Percent, na.rm = TRUE)

median(test$Percent, na.rm = TRUE)

sd(test$Percent, na.rm = TRUE)

max(test$Percent, na.rm = TRUE)

min(test$Percent, na.rm = TRUE)


--------------------------------------------------------------------------------
  
## reg 1


Reg1 <- lm(test$`AVG Wage`~test$`Unemployment Rate`+ test$Percent_BA, data = test)
summary(Reg1)

hist(residuals(Reg1), col = "steelblue")

plot(fitted(Reg1), residuals(Reg1))

abline(h = 0, lty = 2)



## reg 2


Reg2 <- lm(test$Total_PCE~test$`AVG Wage`+test$`Snap Total`, data = test)
summary(Reg2)


## reg 3

Reg3 <- lm(test$`Snap Total`~test$`AVG Wage`+test$`Unemployment Rate`, data = test)
summary(Reg3)




## reg 4

Reg4 <- lm(test$Total_PCE~test$`Unemployment Rate`+test$`AVG Wage`, data = test)
summary(Reg4)


---------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  



