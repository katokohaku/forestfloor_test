# randomForest multi-class classification with abalone data set
# BY: S.KATO / DATE: 2016.07.27-
rm(list=ls())
getwd()

require("dplyr")
require("magrittr")

# system("wget https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data")
labels <- c("Sex","Length","Diameter","Height","Whole","Shucked","Viscera","Shell","Rings")
abalone <- read.csv("abalone.data",header=FALSE)
colnames(abalone) <- labels

str(abalone)
library(randomForest)
library(forestFloor)
require(utils)

mt100 <- abalone %$% table(Rings) %>% data.frame %>% filter(Freq>100)
abalone.sub <- abalone %>% filter(Rings %in% c(7,8,9))
abalone.sub %$% table(Rings)

X = abalone.sub[,!names(abalone.sub) %in% "Rings"]
Y = factor(abalone.sub[,"Rings"])
str(Y)
table(Y)

rf = randomForest(
  X,Y,
  keep.forest=TRUE,  # mandatory
  keep.inbag=TRUE,   # mandatory
  samp=20,           # reduce complexity of mapping structure, with same OOB%-explained
  importance  = TRUE, # recommended, else ordering by giniImpurity (unstable)
  ntree = if(interactive()) 1000 else 25 #speedup CRAN testing
)

ff = forestFloor(rf,X)

plot(ff,plot_GOF=TRUE,cex=.7) #one col per plotted class

forestFloor:::plot.forestFloor_multiClass(ff,plot_GOF=TRUE,cex=.7)

