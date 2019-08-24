## PROBABILITIES

## Data set: NORM ####
dataset <- round(rnorm(n=1000, mean=100, sd=10))
## DISTRIBUTION ####
## WHAT IS DISTRIBUTION OF PROBABILITIES? (density)
## visualize counts
hist(dataset)
## calculate
dist <- dnorm(x=dataset, mean=mean(dataset), sd=sd(dataset))
## combine numbers and probabilities
df <- data.frame(num=dataset,
                 dist=dist)
## visualize probs
## geom_col will mislead because it will sum probs when num values repeated
ggplot(df, aes(x=num, y=dist))+geom_col()
## % probabilities by num -> line or points give same result
ggplot(df, aes(x=num, y=dist))+geom_line()
ggplot(df, aes(x=num, y=dist))+geom_point()
plot(df)

## WHAT IS CUMULATIVE DISTRIBUTION?
cumdist <- pnorm(q=dataset, mean=mean(dataset),
                 sd=sd(dataset))

plot(cumdist)
df <- bind_cols(df, data.frame(cumdist))
ggplot(df, aes(x=num, y=cumdist))+geom_line()

## PROBABILITIES ####
## What is the probability of outcome less than X?
options(scipen = 999)
X <- 90
pnorm(q=X, mean=mean(dataset), sd=sd(dataset))

## What is the probability of outcome greater than X?
X <- 90
pnorm(q=X, mean=mean(dataset), sd=sd(dataset),
      lower.tail = FALSE)
## or...
1-pnorm(q=X, mean=mean(dataset), sd=sd(dataset))

## What is the probability of outcome between X and Y?
X <- mean(dataset)-sd(dataset)
Y <- mean(dataset)+sd(dataset)
pXless <- pnorm(q=X, mean=mean(dataset), sd=sd(dataset))
pYmore <- pnorm(q=Y, mean=mean(dataset), sd=sd(dataset), 
      lower.tail = FALSE)
pbtwn <- 1-(pXles+pYmore)
ggplot(df, aes(x=num, y=dist))+geom_line()+
  geom_vline(xintercept = X, linetype='dashed')+
  geom_vline(xintercept=Y, linetype='dashed')

## PERCENTILES ####
## calc value at given percentile 
## theoretical - based on mean, sd
qnorm(p=0.75, mean=mean(dataset),
      sd=sd(dataset))
## actual based on dataset
## add percentiles to data
quantile(x=dataset, probs=0.75)

## Percentiles:
## - CDF (pnorm) provides percentile values for each num
## What is the total value up to 25th percentile?
## how many items are covered in the 25th percentile?
## what % of total items are covered in the 25th percentile?
plot(ecdf(df$num))
plot.ecdf(df$num)
plot(df$cumdist~df$num)
ggplot(df, aes(x=num, y=cumdist))+geom_line()
ggplot(df, aes(x=cumdist, y=num))+geom_line()

## calculate value of num as % of total
df <- df %>% arrange(num) %>% mutate(
  pct=num/sum(num),
  pctt=cumsum(pct)
)       
ggplot(df, aes(x=cumdist, y=pct))+geom_line()
ggplot(df, aes(x=cumdist, y=pctt))+geom_line()
#ggplot(df, aes(x=cumdist, y=pct))+geom_col()

## Data 2: longer tail ####
# dataset2 <- round(rnorm(n=250, mean=mean(dataset)*2, 
#                                    sd=sd(dataset)*5))
dataset2 <- round(rnorm(n=250, mean=mean(dataset)*2,
                  sd=sd(dataset)*10))
dataset2 <- dataset2[dataset2>100]
hist(dataset2)
dataset <- c(dataset, dataset2)

## DISTRIBUTION ####
## WHAT IS DISTRIBUTION OF PROBABILITIES? (density)
## visualize counts
hist(dataset)
## calculate
dist <- dnorm(x=dataset, mean=mean(dataset), sd=sd(dataset))
## combine numbers and probabilities
df <- data.frame(num=dataset,
                 dist=dist)
## visualize probs
## geom_col will mislead because it will sum probs when num values repeated
#ggplot(df, aes(x=num, y=dist))+geom_col()
## % probabilities by num -> line or points give same result
ggplot(df, aes(x=num, y=dist))+geom_line()
ggplot(df, aes(x=num, y=dist))+geom_point()
plot(df)

## WHAT IS CUMULATIVE DISTRIBUTION?
cumdist <- pnorm(q=dataset, mean=mean(dataset),
                 sd=sd(dataset))

plot(cumdist)
df <- bind_cols(df, data.frame(cumdist))
ggplot(df, aes(x=num, y=cumdist))+geom_line()
ggplot(df, aes(x=cumdist, y=num))+geom_line()

## PROBABILITIES ####
## What is the probability of outcome less than X?
options(scipen = 999)
X <- 90
pnorm(q=X, mean=mean(dataset), sd=sd(dataset))

## What is the probability of outcome greater than X?
X <- 90
pnorm(q=X, mean=mean(dataset), sd=sd(dataset),
      lower.tail = FALSE)
## or...
1-pnorm(q=X, mean=mean(dataset), sd=sd(dataset))

## What is the probability of outcome between X and Y?
X <- mean(dataset)-sd(dataset)
Y <- mean(dataset)+sd(dataset)
pXless <- pnorm(q=X, mean=mean(dataset), sd=sd(dataset))
pYmore <- pnorm(q=Y, mean=mean(dataset), sd=sd(dataset), 
                lower.tail = FALSE)
pbtwn <- 1-(pXles+pYmore)
ggplot(df, aes(x=num, y=dist))+geom_line()+
  geom_vline(xintercept = X, linetype='dashed')+
  geom_vline(xintercept=Y, linetype='dashed')

## WHAT ARE PERCENTILES?
## calc value at given percentile 
## theoretical - based on mean, sd
qnorm(p=0.75, mean=mean(dataset),
      sd=sd(dataset))
## actual based on dataset
## add percentiles to data
quantile(x=dataset, probs=0.75)

## Percentiles:
## - CDF (pnorm) provides percentile values for each num
## what value corresponds to 80th percentile?
## what percentile rank is 100?
plot(ecdf(df$num))
plot.ecdf(df$num)
plot(df$cumdist~df$num)
ggplot(df, aes(x=num, y=cumdist))+geom_line()
ggplot(df, aes(x=cumdist, y=num))+geom_line()

## What % of total is covered by top 20th percentile rank?
## calculate value of num as % of total
df <- df %>% arrange(num) %>% mutate(
  pct=num/sum(num),
  pctt=cumsum(pct)
)       
ggplot(df, aes(x=cumdist, y=pct))+geom_line()
ggplot(df, aes(x=cumdist, y=pctt))+geom_line()
#ggplot(df, aes(x=cumdist, y=pct))+geom_col()

## looks suspicious - but the math holds up
df.test <- df %>% filter(cumdist<0.4)
sum(df.test$num)
sum(df$num)
sum(df.test$num)/sum(df$num)
