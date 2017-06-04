
runif(100, 1,100)
rnorm(100, 50, 25)
rpois(100, 5)

uni <- runif(1000, 1,100)
nor <- rnorm(1000, 50, 10)
poi <- rpois(1000, 10)
sam <- sample(uni, 200, replace=FALSE)

hist(uni)
hist(nor)
hist(poi)
hist(sam)

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
p5 <- rpois(10000,5)
p10 <- rpois(10000, 10)
p25 <- rpois(10000, 25)
p100 <- rpois(10000, 100)
p500 <- rpois(10000, 1000)

pd <- data.frame("pd"="p1","num"=p1)
pd5 <- data.frame("pd"="p5","num"=p5)
pd10 <- data.frame("pd"="p10","num"=p10)
pd25 <- data.frame("pd"="p25","num"=p25)
pd100 <- data.frame("pd"="p100","num"=p100)
pd500 <- data.frame("pd"="p500", "num"=p500)

pdist <- rbind(pd, pd5, pd10, pd25, pd100, pd500)

ggplot(pdist, aes(x=num))+geom_histogram()+
  facet_grid(pd~.)

ggplot(pdist, aes(x=pd, y=num))+geom_boxplot()

pdist %>% group_by(pd) %>%
  summarize(
    pmean=mean(num),
    pmed=median(num),
    std=sd(num)
  )

lnums <- c(1:20)
dp1 <- data.frame("lnums"=lnums, "dpois"=dpois(lnums, 1))
dp4 <- data.frame("lnums"=lnums, "dpois"=dpois(lnums, 4))
dp10 <- data.frame("lnums"=lnums, "dpois"=dpois(lnums, 10))
dp15 <- data.frame("lnums"=lnums, "dpois"=dpois(lnums, 15))

ggplot(dp1, aes(x=lnums, y=dpois))+geom_bar(stat='identity')
ggplot(dp4, aes(x=lnums, y=dpois))+geom_bar(stat='identity')
ggplot(dp10, aes(x=lnums, y=dpois))+geom_bar(stat='identity')
ggplot(dp15, aes(x=lnums, y=dpois))+geom_bar(stat='identity')