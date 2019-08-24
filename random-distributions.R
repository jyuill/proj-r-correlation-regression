library(tidyverse)

uni <- runif(n=1000, min=1, max=100)
nor <- rnorm(n=1000, mean=50, sd=10)
poi <- rpois(n=1000, lambda=5)
sam <- sample(x=min(uni):max(uni), size=1000, replace=TRUE)
## can use prob to skew distribution
## number of items in prob range have to match number of items in x
sam2 <- sample(x=min(uni):max(uni), size=1000, replace=TRUE, prob=max(uni):1)

hist(uni)
hist(nor)
hist(poi)
hist(sam)
hist(sam2)

dists <- data.frame("dist"="uniform",
                    "nums"=uni)
dists2 <- data.frame("dist"="normal",
                    "nums"=nor)
dists3 <- data.frame("dist"="poisson",
                    "nums"=poi)
dists4 <- data.frame("dist"="sample",
                    "nums"=sam)
dists <- rbind(dists,dists2,dists3,dists4)

library(ggplot2)
ggplot(dists, aes(x=nums))+geom_histogram()+
  facet_grid(dist~.)

### POISSON DISTRIBUTION

p1 <- rpois(10000, 1)
p2 <- rpois(10000, 2)
p3 <- rpois(10000, 3)
p4 <- rpois(10000, 4)
p5 <- rpois(10000,5)
p10 <- rpois(10000, 10)
p25 <- rpois(10000, 25)
p100 <- rpois(10000, 100)
p500 <- rpois(10000, 1000)

pd <- data.frame("pd"="p1","num"=p1)
pd2 <- data.frame("pd"="p2","num"=p2)
pd3 <- data.frame("pd"="p3","num"=p3)
pd4 <- data.frame("pd"="p4","num"=p4)
pd5 <- data.frame("pd"="p5","num"=p5)
pd10 <- data.frame("pd"="p10","num"=p10)
pd25 <- data.frame("pd"="p25","num"=p25)
pd100 <- data.frame("pd"="p100","num"=p100)
pd500 <- data.frame("pd"="p500", "num"=p500)

pdist <- rbind(pd, pd2, pd3, pd4, pd5, pd10, pd25, pd100, pd500)

## histogram all
ggplot(pdist, aes(x=num))+geom_histogram()+
  facet_grid(pd~.)

## histogram focus smaller lambda
pdist %>% filter(pd=='p1'|pd=='p2'|pd=='p3'|pd=='p4'|pd=='p5') %>%
  ggplot(aes(x=num))+geom_histogram()+
  facet_grid(pd~.)

## bar plot focus smaller lambda
pdist %>% filter(pd=='p1'|pd=='p2'|pd=='p3'|pd=='p4'|pd=='p5') %>%
  ggplot(aes(x=num))+geom_bar(stat='count')+
  facet_grid(pd~.)

## boxplot all
ggplot(pdist, aes(x=pd, y=num))+geom_boxplot()
## boxplot focus on small
pdist %>% filter(pd=='p1'|pd=='p2'|pd=='p3'|pd=='p4'|pd=='p5') %>%
ggplot(aes(x=pd, y=num))+geom_boxplot()

pdist %>% group_by(pd) %>%
  summarize(
    pmean=mean(num),
    pmed=median(num),
    std=sd(num)
  )

## Density Poisson
## setting the range of possible values from 0 to 40:
## - creates probability density functions (technicall probability mass function 
## for discrete variables) that add up to 1
## - with lower lambda values skewed more to right
## - higher lambda values fit normal distrib
lnums <- c(0:40)
dp1 <- data.frame("lnums"=lnums, "dpois"=dpois(lnums, lambda=1))
dp4 <- data.frame("lnums"=lnums, "dpois"=dpois(lnums, lambda=4))
dp10 <- data.frame("lnums"=lnums, "dpois"=dpois(lnums, lambda=10))
dp15 <- data.frame("lnums"=lnums, "dpois"=dpois(lnums, lambda=15))

ggplot(dp1, aes(x=lnums, y=dpois))+geom_bar(stat='identity')
ggplot(dp4, aes(x=lnums, y=dpois))+geom_bar(stat='identity')
ggplot(dp10, aes(x=lnums, y=dpois))+geom_bar(stat='identity')
ggplot(dp15, aes(x=lnums, y=dpois))+geom_bar(stat='identity')

sum(dp1$dpois)
sum(dp4$dpois)
sum(dp10$dpois)
sum(dp15$dpois)

## poisson - generate random sample
lda <- 4
pprpois <- rpois(n=1000, lambda=lda)
hist(pprpois)

## density - probability of each point occurring
ppdpois <- dpois(x=1:10, lambda=lda)
plot(ppdpois)
sum(ppdpois)
ppdpois

## distribution - cumulative probability at each point (point or less)
ppppois <- ppois(q=1:10, lambda=lda)
plot(ppppois)
ppppois

## quantile - don't get it
## p = vector of probabilities
ppqpois <- qpois(p=ppdpois, lambda=lda)
plot(ppqpois)
ppqpois
