
library(ggplot2)
library(dplyr)

ir1 <- iris

head(ir1)

## focus on two species
ir2 <- ir1 %>% filter(Species=="setosa"|Species=="versicolor")
head(ir2)

## plot petal.width vs sepal.width with linear model line
ggplot(ir2, aes(x=Sepal.Width, y=Petal.Width))+geom_point()+
  geom_smooth(method='lm')

## check correlation - negative
cor(ir2$Sepal.Width, ir2$Petal.Width)
cor.test(ir2$Sepal.Width, ir2$Petal.Width)

## create model of petal.width based on sepal.width
model1 <- lm(ir2$Petal.Width~ir2$Sepal.Width)
## review model - negative coefficient on sepal.width (as expected, given negative correlation)
## relatively low adjusted r-sq at 0.32 but statistically significant
summary(model1)

## split out two species and show lm lines for each
## positive relationships for petal.width when species taken into account
ggplot(ir2, aes(x=Sepal.Width, y=Petal.Width, shape=Species))+geom_point()+
  geom_smooth(method='lm')

## identify species as either 1 for setosa and 0 for versicolor
ir3 <- ir2 %>%
  mutate(spec=ifelse(Species=="setosa",1,0))

## create model that takes into account species
model2 <- lm(ir3$Petal.Width~ir3$Sepal.Width+ir3$spec)
## now a positive coefficient for sepal.width, as expected
## very high adjusted r-sq at 0.94
summary(model2)

## RESULTS FROM ABOVE
## if we know sepal width and species, we can predict petal width with pretty good accuracy

## another question: what metric or combination of metrics will enable us to make a good guess at species?

## boxplot on sepal.width shows sepal.width above 3 is probably setosa, with small chance of versicolor
## and below 3 is probably versicolor with small chance of setosa
ggplot(ir2, aes(x=Species,y=Sepal.Width))+geom_boxplot()
## boxplot on petal.width shows that petal width above 1 is versicolor, below 1 is setosa
ggplot(ir2, aes(x=Species,y=Petal.Width))+geom_boxplot()

## WHAT I WANT TO KNOW IS:
## what are the techniques for determining more certainly where to draw the lines
## - logistic regression using species identifier as dependent variable ?
##    - test with both sepal width and pedal width
## - clustering to establish break-points?

## - logistic regression on sepal.width
logr1 <- glm(formula=spec~Sepal.Width, family=binomial, data=ir3)
summary(logr1)
## get Sepal.Width coefficient
logr1$coefficients[2]
## get exp of coefficient to determine % increase chance of identifying species with increase in sepal.width
## (not sure if that is at all correct)
exp(logr1$coefficient[2])-1

## generate plot with fitted values - note how switches over around 3
plot(ir3$Sepal.Width,fitted.values(logr1))
## add observed values in red
points(ir3$Sepal.Width,ir3$spec,col="red")

## use popbio for more insightful visualization
library(popbio)
logi.hist.plot(ir3$Sepal.Width, ir3$spec, boxp=FALSE, type="count", col="gray", xlabel="size")

## get prediction based on sepal width
nd <- data.frame(Sepal.Width=3.5)
## at 3.5 there is 94% chance that dependent variable is 1 (versicolor)
predict(logr1,nd, type='response')
predict(logr1, list(Sepal.Width=3), type="response")

## logistic regression on pedal.width
## - model doesn't work - 'algorithm did not converge'
## - because there is no overlap?
logr2 <- glm(formula=ir3$spec~ir3$Petal.Width, family=binomial)
summary(logr2)

## generate plot with fitted values - note how switches over around 3
plot(ir3$Petal.Width,fitted.values(logr2))
## add observed values in red
points(ir3$Petal.Width,ir3$spec,col="red")

## get prediction based on petal width

### LOGISTIC REGRESSION WITH MTCARS
## can we determine if a car likely to have V engine or not based on car weight and engine displacement
## 
head(mtcars)
summary(mtcars) ## can see that vs is either 0 or 1
## dependent variable is categorical, independent (explanatory) variables are continuous
## create a model
model <- glm(formula=vs~wt+disp, data=mtcars, family='binomial')
## check out the model
model
## get summary
summary(model)
## notes:
## - deviance: lower better
##   - Null deviance: without explanatory variables, null dev is 44
##   - Residual deviance: with explanatory variables added, deviance drops to 21.4 - sign of progress
## - AIC: lower better
##   - used to compare models (not useful here, since not comparing)
## - coefficients
##   - wt and disp for explanatory variables
##   - logit coefficients -> represent e to the power of the coefficient
##   - wt: adding 1 unit of weight means log of odds of car being a '1' in response variable increase by e to 1.63

## make prediction based on wt and displacement
newdata<- data.frame(wt=2.1,disp=180)
predict(model, newdata, type='response')



