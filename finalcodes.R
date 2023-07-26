library(tidyverse)
library(skimr)
library(lattice)
library(car)
library(broom)
library(olsrr)
library(lindia)
library(lmtest)
library(ggplot2)
library(GGally)
library(dplyr)
library(magrittr)



data <- Data_set

data$season <- as.factor(data$season)
data$workingday <- as.factor(data$workingday)
data$weathersit <- as.factor(data$weathersit)


training_set <- data %>% dplyr::filter(yr == 1)
testing_set <- data %>% dplyr::filter(yr == 0)

#both holiday and weekday information is given by working day variable.
#month variable is removed since it has 12 levels and the model becomes complicated.
#


training_set <- training_set %>% dplyr::select(-c(instant, dteday, yr, mnth, holiday, weekday))

head(data)

training_set %>% dplyr::select(temp,atemp,hum,windspeed,cnt) %>% 
  ggpairs()+
  labs(title = "Pearson correlation and variable distribution")



ggplot(training_set,
       aes(x=hum,y=cnt))+
  geom_point(color="spring green")+
  labs(title = "Scatter plot of count Vs Normalised humidity")

ggplot(training_set,
       aes(x=temp,y=cnt))+
  geom_point(color="hot pink")+
  labs(title = "Scatter plot of count Vs Normalised temperature")


ggplot(training_set,
       aes(x=atemp,y=cnt))+
  geom_point(color="deep sky blue")+
  labs(title = "Scatter plot of count Vs Normalised feeling temperature")

ggplot(training_set,
       aes(x=windspeed,y=cnt))+
  geom_point(color="dark violet")+
  labs(title = "Scatter plot of count Vs Normalised wind speed")


#humidity removed since no effect on cnt

ggplot(training_set,
       aes(x=cnt,y=season,fill=season,col=season))+
  geom_boxplot(outlier.shape = NA, alpha=0.2)+
  geom_jitter(aes(col=season))+
  coord_flip()+
  labs(title = "Total bike rental Vs Season")


ggplot(training_set,
       aes(x=cnt,y=workingday,fill=workingday,col=workingday))+
  geom_boxplot(outlier.shape = NA, alpha=0.2)+
  geom_jitter(aes(col=workingday))+
  coord_flip()+
  labs(title = "Total bike rental Vs Working day")


ggplot(training_set,
       aes(x=cnt,y=weathersit,fill=weathersit,col=weathersit))+
  geom_boxplot(outlier.shape = NA, alpha=0.2)+
  geom_jitter(aes(col=weathersit))+
  coord_flip()+
  labs(title = "Total bike rental Vs Weather situation")


qqnorm(training_set$cnt, frame = FALSE)
qqline(training_set$cnt, col = "red")

ggplot(training_set, aes(x=cnt)) +   
  geom_histogram(color="white", fill="#66CDAA")+
  labs(title = "Distrbution of total bike rental count")


quantitative_data <- training_set %>% select(c(temp, atemp, hum, cnt))
cor(quantitative_data, use = "complete.obs")
cor.test(training_set$hum, training_set$cnt) #put into data analysis part

#Now plot 2 quantitative variables with one catergorical variables and check for interactions.
#Get interaction plot for two catergorical variables and tell that there is interaction between catergorical varibales
#There is a possiblity that 3 way interactions are also there.
#Limitaion 01 - No interactions are considered

#humidity removed, atemp removed

model1 <- lm(data = training_set, cnt ~ temp + windspeed + season + workingday + weathersit)
summary(model1)
tidy(model1,conf.int = TRUE)
#working day removed by backward elimination


library(broom)

plot(model1)
plot(model1,which = 1)
plot(model1,which = 2)

ggplot(data = training_set, aes(x = model1$residuals)) +
  geom_histogram(fill = "#66CDAA", color = "white") +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')



model1_aug <- augment(model1)
shapiro.test(model1_aug$.resid)


car :: vif(model1)

#checking outliers
#influential points
ols_plot_cooksd_bar(model1)
ols_plot_resid_stand(model1)
#Y outliers are also there but removing them leads to non normality.


#Step 01
#removinf influential points
model1_aug<- model1_aug %>% dplyr::filter (.cooksd <= 0.011)


#refitting without influentials

model2<- model1_aug %>%
  lm(cnt ~ temp + windspeed + season + workingday + weathersit,.)
 summary(model2) 
 
 
 
 tidy(model2)
 

model2_aug <- augment(model2)


glance(model1)
glance(model2)

anova(model2)

shapiro.test(model2_aug$.resid)

#normality is good

plot(model2)
plot(model2,which = 1)

#Graphical check for constant error variance
sq_resid <- model3_fitresid$.resid^2

model3_fitresid <- data.frame(model3_fitresid, sq_resid)

ggplot(model3_fitresid, aes(x = .fitted, y = sq_resid)) +
    geom_point()

ggplot(model3_fitresid, aes(x = .fitted, y = .resid)) +
  geom_point()

#BP Test
bptest(model2)
bgtest(model2)



bptest(model1)
bgtest(model1)

#H0 - error variance is constant
#error variance is not constant hence its heterscedastic

#Now we have to check for the independency.

#Checking for multicollinearity
vif(model2)
#Note that multicollinearity of qualitative predictors are not interpretable
car :: vif(model2)

#Now we have to check for autocorrelation
autoplot()
acf(model2_aug$.std.resid,main="ACF of Standardised residuals")

#model validating

library(caret)
library(modelr)

predict(model3,testing_set)

library(Metrics)
rmse(testing_set$cnt,predict(model1,testing_set))
mae(testing_set$cnt,predict(model1,testing_set))


rmse(testing_set$cnt,predict(model2,testing_set))
mae(testing_set$cnt,predict(model2,testing_set))


MAE <- mean(abs(testing_set$cnt-predict(model1,testing_set)));MAE
MAE1 <- mean(abs(testing_set$cnt-predict(model2,testing_set)));MAE1

RMSE <- sqrt(mean((testing_set$cnt-predict(model1,testing_set))^2));RMSE
RMSE1 <- sqrt(mean((testing_set$cnt-predict(model2,testing_set))^2));RMSE1


R2 = 1-(sum((testing_set$cnt-predict(model2,testing_set)^2)/sum((testing_set$cnt-mean(testing_set$cnt))^2)))


summary(model2,data=testing_set)$r.squared

















