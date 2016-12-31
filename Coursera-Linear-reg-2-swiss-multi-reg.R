
## Look at swiss dataset on fertility
head(swiss)

library(ggplot2)
library(GGally)

## doesn't work due to updates since code provided in course
## creates pairwise plot in ggplot2
ggpairs(swiss, lower=list(continuous="smooth"))

