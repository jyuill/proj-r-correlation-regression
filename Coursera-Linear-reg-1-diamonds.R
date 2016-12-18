
library(UsingR)
library(ggplot2)

## Regression example

data(diamond)

ggplot(diamond, aes(x=carat,y=price))+
  xlab("Carats")+
  ylab("Price")+
  geom_point()+
  geom_smooth(method="lm", color="green")

fit <- lm(price ~ carat, data=diamond)
coef(fit)