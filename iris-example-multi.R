
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
## - logistic regression on petal.width ?
## - clustering to establish break-points?

