
library(UsingR) # this library does not load because depends on HistData, which won't load on work laptop :[
library(ggplot2)

#### Regression example

data(diamond) ## original example uses 'diamond' data set from UsingR - not available for reasons mentioned above

diamond <- diamonds

#### CHECK OUT DATA

summary(diamond)
ggplot(diamond, aes(x=price))+geom_histogram(binwidth=1000)
ggplot(diamond, aes(x=carat))+geom_histogram(binwidth=0.2)

## plot data with regression line
ggplot(diamond, aes(x=carat,y=price))+
  xlab("Carats")+
  ylab("Price")+
  geom_point()+
  geom_smooth(method="lm", color="purple") ## shows linear model line (regression line)

#### LINEAR REGRESSION
## fit linear model to price data dependent on carat size

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

#### PREDICTION
## predict value of other sizes

newcarats <- c(0.16, 0.27, 0.34)
## manually calculate predicted value of new carats using intercept and slope from fit
coef(fit)[1] + coef(fit)[2] * newcarats # order of operations means multiplication done first
## can use generic predict function - more scalable
predict(fit, newdata=data.frame(carat=newcarats))
## if you omit new data, predict will give you predicted value at existing observed x values
predict(fit)

#### RESIDUALS
## difference between actual values of Y at each X and predicted values of Y along regression line
## for each value of X

## complete set of residuals
resid(fit)

## sum of residuals should equal 0 (or very close)
sum(resid(fit))
## residuals X X values should equal 0 (or very close)
sum(resid(fit)*diamond$carat)

## 
e <- resid(fit)
x <- diamond$carat
n <- nrow(diamond)

## check out residual plot: look for patterns - should be none
plot(x,e,xlab="carats", ylab="Residuals", bg="lightblue", col="black")
abline(h=0, lwd=2)
for (i in 1:n)
  lines(c(x[i],x[i]), c(e[i],0), col="red", lwd=2)

## 
ggplot(diamond, aes(x=carat,y=resid(fit)))+geom_point()+
  geom_hline()

## example of residuals for a wave pattern
x=runif(100,-3,3)
y=x+sin(x)+rnorm(100,sd=0.2)
ggplot(data.frame(x=x,y=y), aes(x=x,y=y))+
  geom_smooth(method="lm", color="black")+
  geom_point(size=4, colour="red", alpha=0.5)
## you can make out a wave pattern, but not super-obvious
## model is not 'correct' because doesn't account for wave - not totally linear
## still useful because it explains much of the relationship
## get model data:
fitwave <- lm(y ~x, data=data.frame(x=x,y=y))
summary(fitwave)
## r-sq = 0.96

## check residuals
ggplot(data.frame(x=x,y=resid(lm(y~x))),aes(x=x,y=y))+
  geom_hline(yintercept=0, size=2)+
  geom_point(size=4, colour="red", alpha=0.7)+
  xlab("x")+ylab("residual")
## pattern is MUCH more obvious
  

