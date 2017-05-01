require(rgl)
require(dplyr)
require(randomForest)
require(ICEbox)
require(forestFloor)


wwine <- WhiteWine
wwine %>% str

ICEbox::ice
## Not run:
## avoid testing of rgl 3D plot on headless non-windows OS
## users can disregard this sentence.

library(randomForest)
#simulate data
obs=1500
vars = 6
X = data.frame(replicate(vars,runif(obs)))*2-1
Y = with(X, X1*2 + 2*sin(X2*pi) + 3* (X3+X2)^2 ) #X4, X5,X6 are noises

Yerror = 1 * rnorm(obs)
var(Y)/var(Y+Yerror)
Y= Y+Yerror
#grow a forest, remember to include inbag
multireg.rfo=randomForest::randomForest(X,Y,
                               keep.inbag=TRUE,
                               ntree=1000,
                               replace=TRUE,
                               sampsize=500,
                               importance=TRUE)
names.X <- c("X2","X3","X1","X5","X6","X4")

# randomForest::partialPlot()
par(mfrow=c(2, 3))
for (i in seq_along(names.X)) {
  partialPlot(multireg.rfo, X, names.X[i], xlab=names.X[i], main = names.X[i], ylim=c(-4,10))
}
par(mfrow=c(1, 1))


# disaggregation using ICEbox::ice()
require(ICEbox)
par(mfrow=c(2, 3))
for (i in seq_along(names.X)) {
  multireg.ice <- ice(object = multireg.rfo, X = X, y = Y, predictor = names.X[i], frac_to_build = .1) 
  plot(multireg.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 1, main=names.X[i], ylim=c(-4,10)) 
}
par(mfrow=c(1, 1))


require(forestFloor)

#compute ff
multireg.ff = forestFloor(multireg.rfo,X)

#print forestFloor
print(multireg.ff)
#plot partial functions of most important variables first
Col=fcol(multireg.ff,1)
plot(multireg.ff,col=Col,orderByImportance=TRUE)



#the pure feature contributions

library(rgl)
rgl::plot3d(multireg.ff$X[,2],multireg.ff$X[,3],apply(multireg.ff$FCmatrix[,2:3],1,sum),
            #add some colour gradients to ease visualization
            #box.outliers squese all observations in a 2 std.dev box
            #univariately for a vector or matrix and normalize to [0;1]
            col=fcol(multireg.ff,2,orderByImportance=FALSE))
#add grid convolution/interpolation
#make grid with current function
grid23 = convolute_grid(multireg.ff,Xi=2:3,userArgs.kknn= alist(k=25,kernel="gaus"),grid=50,zoom=1.2)
#apply grid on 3d-plot
rgl::persp3d(unique(grid23[,2]),unique(grid23[,3]),grid23[,1],alpha=0.3,
             col=c("black","grey"),add=TRUE)
#anchor points of grid could be plotted also
rgl::plot3d(grid23[,2],grid23[,3],grid23[,1],alpha=0.3,col=c("black"),add=TRUE)
## and we se that their is almost no variance out of the surface, thus is FC2 and FC3
## well explained by the feature context of both X3 and X4
### next example show how to plot a 3D grid + feature contribution
## this 4D application is very experimental
#Make grid of three effects, 25^3 = 15625 anchor points
grid123 = convolute_grid(ff,
                         Xi=c(1:3),
                         FCi=c(1:3),
                         userArgs.kknn = alist(
                           k= 100,
                           kernel = "gaussian",
                           distance = 1),
                         grid=25,
                         zoom=1.2)



## End




## Not run:
#example 1 - fcol used on data.frame or matrix
library(forestFloor)
X = data.frame(matrix(rnorm(1000),nrow=1000,ncol=4))
X[] = lapply(X,jitter,amount = 1.5)
plot(X)


#single variable gradient by X1 (Unique colour system)
plot(X,col=fcol(X,1))
plot(X,col=fcol(X,1))

#double variable gradient by X1 and X2 (linear colour system)
plot(X,col=fcol(X,1:2))
#triple variable gradient (PCA-decomposed, linear colour system)
plot(X,col=fcol(X,1:3))
#higher based gradient    (PCA-decomposed, linear colour system)
plot(X,col=fcol(X,1:4))
#force linear col + modify colour wheel
plot(X,col=fcol(X,
                cols=1, #colouring by one variable
                RGB=FALSE,
                hue.range = 4, #cannot exceed 1, if colouing by more than one var
                #except if max.df=1 (limits to 1D gradient)
                saturation=1,
                brightness = 0.6))
#colour by one dimensional gradient first PC of multiple variables
plot(X,col=fcol(X,
                cols=1:2, #colouring by multiple
                RGB=TRUE, #possible because max.df=1
                max.df = 1, #only 1D gradient (only first principal component)
                hue.range = 2, #can exceed 1, because max.df=1
                saturation=.95,
                brightness = 0.8))






## Not run: example of plot_simplex3
library(randomForest)
library(forestFloor)
require(utils)
data(iris)
X = iris[,!names(iris) %in% "Species"]
Y = iris[,"Species"]
as.numeric(Y)

rf.test42 = randomForest(X,Y,keep.forest=TRUE,
                         replace=FALSE,keep.inbag=TRUE,samp=15,ntree=100)
ff.test42 = forestFloor(rf.test42,X,calc_np=FALSE,binary_reg=FALSE)
plot(ff.test42,plot_GOF=TRUE,cex=.7,
     colLists=list(c("#FF0000A5"),
                   c("#00FF0050"),
                   c("#0000FF35")))
show3d(ff.test42,1:2,3:4,plot_GOF=TRUE)

#plot all effect 2D only
pars = plot_simplex3(ff.test42,Xi=c(1:3),restore_par=FALSE,zoom.fit=NULL,
                     var.col=NULL,fig.cols=2,fig.rows=1,fig3d=FALSE,includeTotal=TRUE,auto.alpha=.4
                     ,set_pars=TRUE)
pars = plot_simplex3(ff.test42,Xi=0,restore_par=FALSE,zoom.fit=NULL,
                     var.col=alist(alpha=.3,cols=1:4),fig3d=FALSE,includeTotal=TRUE,
                     auto.alpha=.8,set_pars=FALSE)
for (I in ff.test42$imp_ind[1:4])  {
  #plotting partial OOB-CV separation(including interactions effects)
  #coloured by true class
  pars = plot_simplex3(ff.test42,Xi=I,restore_par=FALSE,zoom.fit=NULL,
                       var.col=NULL,fig.cols=4,fig.rows=2,fig3d=TRUE,includeTotal=FALSE,label.col=1:3,
                       auto.alpha=.3,set_pars = (I==ff.test42$imp_ind[1]))
  #coloured by varaible value
  pars = plot_simplex3(ff.test42,Xi=I,restore_par=FALSE,zoom.fit=TRUE,
                       var.col=alist(order=FALSE,alpha=.8),fig3d=FALSE,includeTotal=(I==4),
                       auto.alpha=.3,set_pars=FALSE)
}
## End(Not run)











