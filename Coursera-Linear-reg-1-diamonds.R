
library(UsingR) # this library does not load because depends on HistData, which won't load on work laptop :[
library(ggplot2)

## Regression example

data(diamond) ## original example uses 'diamond' data set from UsingR - not available for reasons mentioned above

diamond <- diamonds
summary(diamond)

ggplot(diamond, aes(x=price))+geom_histogram(binwidth=1000)
ggplot(diamond, aes(x=carat))+geom_histogram(binwidth=0.2)

ggplot(diamond, aes(x=carat,y=price))+
  xlab("Carats")+
  ylab("Price")+
  geom_point()+
  geom_smooth(method="lm", color="purple")

fit <- lm(price ~ carat, data=diamonds)
coef(fit)

summary(fit)

## mean-center carat values by subtracting mean from each value to reset the intercept (x=0) to the mean
fit2 <- lm(price ~ I(carat-mean(carat)), data=diamond)
summary(fit2)
## slope stays same but now intercept indicates price for average size diamond - more useful

## slope indicates price increase per carat - big increment
## multiply carat values by 10 to get increase for 1/10 of carat
fit3 <- lm(price ~ I(carat*10), data=diamond)
summary(fit3)
## previous slope, divided by 10

## predict value of other sizes

newcarats <- c(0.16, 0.27, 0.34)
## manually calculate predicted value of new carats using intercept and slope from fit
coef(fit)[1] + coef(fit)[2] * newcarats # order of operations means multiplication done first
## can use generic predict function - more scalable
predict(fit, newdata=data.frame(carat=newcarats))
## if you omit new data, predict will give you predicted value at existing observed x values
predict(fit)
