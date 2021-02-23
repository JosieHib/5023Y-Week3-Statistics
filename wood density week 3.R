library(tidyverse)
library(skimr)
library(rstatix)
library(equatiomatic)
library(patchwork)
wood_density <- read_csv("Data/wood_density.csv")

wood_density %>% ###pipes data set to the plot
  ggplot(aes(x=Density, y=Hardness))+ ##sets x and y axis
  geom_point()+ ### adds data points
  geom_smooth(method="lm") ###method = "lm" makes the line straight not curved

density_model <- lm(Hardness~Density, data=wood_density)
### make predictions about the change in hardness for every unit change in wood density
density_model

(24.7*57.507)+-1160.5   ###calculating the predicted hardness for the lowest density point
coef(density_model)[1]+
  coef(density_model)[2]*
  24.7   ### coef of the model directly stops rounding errors
### predictions for one point at a time
fitted(density_model) ###can compare our model’s predicted values to our realised values.
###pull out the models estimates of the dependent variable based on each value of the independent value.
### predictions for whole data set at once
### We can now also calculate the difference between these predictions or model-fitted values and the observed values - these values are know as the residuals
484-259.9152 ### residual

wood_density_augmented <- wood_density %>% 
  mutate(predictions=fitted(density_model)) %>% 
  mutate(residuals=Hardness-predictions)
### makes 2 columns in original dataset - one predicted and one residuals 
### mutate(column name = content of column)

p1 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_line()+
  ggtitle("Full Data")

p2 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=predictions))+
  geom_line()+
  ggtitle("Linear trend")

p3 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=residuals))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining pattern")

p1+p2+p3  ###adds all plots together 

library(broom)
broom::glance(density_model) ###gives R^2, F values etc
broom::tidy(density_model, conf.int=TRUE) ###summary and adds confidence intervals
broom::augment(density_model, wood_density, interval="confidence") ###Takes computations from our model fit and adds them back onto our original dataframe.

plot1 <- broom::augment(density_model, wood_density, interval="confidence") %>% 
  ggplot(aes(x=Density, y=Hardness))+geom_line(aes(x=Density, y=.fitted))+
  geom_line(aes(x=Density, y=.upper), linetype="dashed")+
  geom_line(aes(x=Density, y=.lower), linetype="dashed")+
  geom_point() +ggtitle("Manually fitting linear model \n and confidence intervals")
plot1
###plotting a graph of hardness and density and adding computations of the modelfit into our dataframe

plot2 <- wood_density %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_smooth(method=lm)+geom_point()+
  ggtitle("Geom smooth method to plotting \n a linear model")
plot2
###plotting linear model via geom smooth 

plot1 + plot2
###shows both plots next to each other 

### Wood density is an excellent predictor of timber hardness. 
### On average for every pound per cubic foot increase in the density of wood, we see a 57.5 point increase in the Janka “hardness scale”
###(F1,34= 637, P <0.001, R^2 = 0.94).
### writing this to test a push to GitHub