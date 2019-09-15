## Correlation examples

library(tidyverse)
library(scales)

## get example data
data <- read_csv('data-source/examples.csv')

cor(data[,1:8])

ggplot(data, aes(x=A, y=B))+geom_point()
cor.test(data$A, data$B)

ggplot(data, aes(x=A, y=C))+geom_point()
cor.test(data$A, data$C)

ggplot(data, aes(x=D, y=E))+geom_point()
ggplot(data, aes(x=num))+geom_line(aes(y=D))+
  geom_line(aes(y=E))

cor.test(data$D, data$E)

ggplot(data, aes(x=num, y=Fexp))+geom_line()
ggplot(data, aes(x=num, y=log(Fexp)))+geom_line()

## plot exponents of 2
ggplot(data, aes(x=num, y=Fexp2))+geom_line()+
  scale_y_continuous(labels=comma)
## transform by log2 -> straight line
ggplot(data, aes(x=num, y=log2(Fexp2)))+geom_line()+
  scale_y_continuous(labels=comma)
## note that standard log transformation also provides straight line
##  - lower y-axis because natural log (e or exp(1)) = 2.7
ggplot(data, aes(x=num, y=log(Fexp2)))+geom_line()+
  scale_y_continuous(labels=comma)

## find another straight line to correlate against
ggplot(data, aes(x=num, y=B))+geom_line()
## visualize
ggplot(data, aes(x=B, y=log2(Fexp2)))+geom_point()+
  scale_y_continuous(labels=comma)
## get correlation
cor.test(data$B, log2(data$Fexp2))

## compare with untransformed data
ggplot(data, aes(x=B, y=Fexp2))+geom_point()+
  scale_y_continuous(labels=comma)
cor.test(data$B, data$Fexp2)

## linear model for relationship based on log2
mod <- lm(data=data, log2(Fexp2)~B)
summary(mod)

## prediction based on model and value of B
dependent <- 35
log2value <- predict(mod, data.frame(B=dependent))
print(paste0('if dependent value B is ',dependent,' model predicts log2 value = ',log2value))
## transform the predicted outcome var back to original scale
2^log2value
## check with actual values in dataset
data %>% filter(B==dependent) %>% select(num, B, Fexp2)
