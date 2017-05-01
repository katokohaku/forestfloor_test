require(randomForest)
require(forestFloor)
boston <- MASS::Boston

str(MASS::Boston)
X <- boston[,-14]
Y <- boston[,14]
rfo = randomForest(
#   X, #features, data.frame or matrix. Recommended to name columns.
#   Y, #targets, vector of integers or floats
  medv ~ ., data = boston,
  keep.inbag = TRUE,  # mandatory,
  importance = TRUE,  # recommended, else ordering by giniImpurity (unstable)
  proximity = TRUE,
  ntree = if(interactive()) 500 else 50 #speedup CRAN testing
)

imp <- importance(rfo)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
png(filename = "partial_boston.png", 800, 600  )
op <- par(mfrow=c(3,5))
for (i in seq_along(impvar)) {
  partialPlot(rfo, boston, impvar[i], xlab="",
              main=impvar[i],
              ylim=c(18,35))
}
par(mfrow = c(1,1))
dev.off()


#compute forestFloor object, often only 5-10% time of growing forest
ff = forestFloor(
  rf.fit = rfo,       # mandatory
  X = X,              # mandatory
  calc_np = FALSE,    # TRUE or FALSE both works, makes no difference
  binary_reg = FALSE  # takes no effect here when rfo$type="regression"
)

#print forestFloor
print(ff) #prints a text of what an 'forestFloor_regression' object is
plot(ff)

#plot partial functions of most important variables first
plot(ff,                       # forestFloor object
     #plot_seq = 1:6,           # optional sequence of features to plot
     orderByImportance=TRUE    # if TRUE index sequence by importance, else by X column
)

#Non interacting features are well displayed, whereas X3 and X4 are not
#by applying color gradient, interactions reveal themself
#also a k-nearest neighbor fit is applied to evaluate goodness-of-fit
Col=fcol(ff,3,orderByImportance=FALSE) #create color gradient see help(fcol)
plot(ff,col=Col,plot_GOF=TRUE)

#feature contributions of X3 and X4 are well explained in the context of X3 and X4
# as GOF R^2>.8

show3d(ff,3:4,col=Col,plot_GOF=TRUE,orderByImportance=FALSE)
