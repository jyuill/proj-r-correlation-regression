
library(UsingR)
library(ggplot2)

## Regression example

data(diamond)

ggplot(diamond, aes(x=carat,y=price))+
  xlab("Carats")+
  ylab("Price")+
  geom_point()+
  geom_smooth(method="lm", color="purple")

fit <- lm(price ~ carat, data=diamond)
coef(fit)

summary(fit)

## mean-center carat values by subtracting mean from each value to reset the intercept (x=0) to the mean
fit2 <- lm(price ~ I(carat-mean(carat)), data=diamond)
summary(fit2)
