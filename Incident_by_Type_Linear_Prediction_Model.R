getwd()
setwd("/Users/polly.mckim/Desktop")
crime<-read.csv("averaged_desc_linear.csv", header = TRUE)
crime_low<-read.csv("Lower_counts.csv", header = TRUE)
crime_high<-read.csv("high_counts.csv", header = TRUE)
#install.packages(c('devtools','curl'))
#library("devtools"); install_github("lme4",user="lme4")

#data(Dyestuff, package='lme4')
#attach(Dyestuff)
#head(Dyestuff)

# all incidents + linear regression 
ggplot(data = crime, aes(x = MeanTemp, y = Incidents)) +
geom_point() +
geom_smooth(method = 'lm')

#linear regression model for all 
summary(lm(Incidents ~ MeanTemp, data = crime)) 


# plot the data using ggplot2 using a different color for each crime description all incidents 
ggplot(data = crime, aes(x = MeanTemp, y = Incidents, color = Description)) + 
  geom_point() + stat_smooth(method = 'lm')
crimelmall<-lm(Incidents ~  MeanTemp * Description, data = crime)
summary(crimelmall)
(confint(crimelmall))


# lower graph (less common incidents so patterns can be more clearly seen)
ggplot(data = crime_low, aes(x = MeanTemp, y = Incidents, color = Description)) + 
  geom_point() + stat_smooth(method = 'lm') + ggtitle("Incidents versus Mean Temperature (Infrequent Incidents)")

# build a linear model for lower 
crimelmlow<-lm(Incidents ~  MeanTemp * Description, data = crime_low)
summary(crimelmlow)
(confint(crimelmlow))

# higher graph 
ggplot(data = crime_high, aes(x = MeanTemp, y = Incidents, color = Description)) + 
  geom_point() + stat_smooth(method = 'lm')+ ggtitle("Incidents versus Mean Temperature (Frequent Incidents)")


# build a linear model for higher 
crimelmhigh<-lm(Incidents ~  MeanTemp * Description, data = crime_high)
summary(crimelmhigh)
(confint(crimelmhigh))
mse <- mean(residuals(crimelmhigh)^2)
print(mse)




'''

'''
