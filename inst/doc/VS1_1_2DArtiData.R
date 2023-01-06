## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",fig.width=6, fig.height=4, fig.align = "center") 

## ----setup, message=FALSE, results='hide'-------------------------------------
library(pcds)

## -----------------------------------------------------------------------------
nx<-10; ny<-5;  #try also nx<-40; ny<-10 or nx<-1000; ny<-20;
set.seed(123)
Xp<-cbind(runif(nx),runif(nx))
Yp<-cbind(runif(ny,0,.25),runif(ny,0,.25))+cbind(c(0,0,0.5,1,1),c(0,1,.5,0,1))  
#try also Yp<-cbind(runif(ny,0,1),runif(ny,0,1))

## ----ADfig, eval=F, fig.cap="The scatterplot of the 2D artificial data set with two classes; black circles are class $X$ points and red triangles are class $Y$ points."----
#  XYpts = rbind(Xp,Yp) #combined Xp and Yp
#  lab=c(rep(1,nx),rep(2,ny))
#  lab.fac=as.factor(lab)
#  plot(XYpts,col=lab,pch=lab,xlab="x",ylab="y",main="Scatterplot of 2D Points from Two Classes")

## ----AD-DTfig, fig.cap="The scatterplot of the X points in the artificial data set together with the Delaunay triangulation of $Y$ points (dashed lines)."----
Xlim<-range(Xp[,1],Yp[,1])
Ylim<-range(Xp[,2],Yp[,2])
xd<-Xlim[2]-Xlim[1]
yd<-Ylim[2]-Ylim[1]
plot(Xp,xlab="x", ylab="y",xlim=Xlim+xd*c(-.05,.05),
     ylim=Ylim+yd*c(-.05,.05),pch=".",cex=3,main="X points and Delaunay Triangulation of Y Points")
#now, we add the Delaunay triangulation based on $Y$ points
DT<-interp::tri.mesh(Yp[,1],Yp[,2],duplicate="remove")
interp::plot.triSht(DT, add=TRUE, do.points = TRUE)

## ----eval=F-------------------------------------------------------------------
#  num.del.tri(Yp)
#  #> [1] 4

## -----------------------------------------------------------------------------
M<-"CC" #try also M<-c(1,1,1) #or M<-c(1,2,3)

## ----eval=F-------------------------------------------------------------------
#  NumArcsAS(Xp,Yp,M)
#  #> $num.arcs
#  #> [1] 3
#  #>
#  #> $tri.num.arcs
#  #> [1] 0 0 0 3
#  #>
#  #> $num.in.conv.hull
#  #> [1] 7
#  #>
#  #> $num.in.tris
#  #> [1] 2 1 1 3
#  #>
#  #> $weight.vec
#  #> [1] 0.2214646 0.2173192 0.2593852 0.2648197
#  #>
#  #> $del.tri.ind
#  #>      [,1] [,2] [,3] [,4]
#  #> [1,]    1    5    3    3
#  #> [2,]    3    2    4    1
#  #> [3,]    2    3    5    4
#  #>
#  #> $data.tri.ind
#  #>  [1]  1  4  1  3 NA NA  4 NA  4  2

## ----eval=F-------------------------------------------------------------------
#  IM<-IncMatAS(Xp,Yp,M)
#  IM[1:6,1:6]
#  #>      [,1] [,2] [,3] [,4] [,5] [,6]
#  #> [1,]    1    0    0    0    0    0
#  #> [2,]    0    1    0    0    0    0
#  #> [3,]    0    0    1    0    0    0
#  #> [4,]    0    0    0    1    0    0
#  #> [5,]    0    0    0    0    1    0
#  #> [6,]    0    0    0    0    0    1

## ----eval=F-------------------------------------------------------------------
#  dom.greedy(IM)  #try also dom.exact(IM)  #this might take a longer time for large  nx (i.e. nx >= 19)
#  #> $approx.dom.num
#  #> [1] 8
#  #>
#  #> $ind.approx.mds
#  #> [1]  9  1 10  5  8  4  3  6

## ----adASarcs1, fig.cap="The arcs of the AS-PCD for the 2D artificial data set using the CC-vertex regions together with the Delaunay triangles based on the $Y$ points (dashed lines)."----
plotASarcs(Xp,Yp,M,asp=1,xlab="",ylab="")

## ----adASpr1, fig.cap="The AS proximity regions for all $X$ points in the 2D artificial data set using the CC-vertex regions together with the Delaunay triangles based on the $Y$ points (dashed lines)."----
plotASregs(Xp,Yp,M,xlab="",ylab="")

## ----adASarcs2, eval=F, fig.cap="The arcs of the AS-PCD for the 2D artificial data set using the CC-vertex regions together with the Delaunay triangles based on the $Y$ points (dashed lines)."----
#  Arcs<-ArcsAS(Xp,Yp,M)
#  Arcs
#  #> Call:
#  #> ArcsAS(Xp = Xp, Yp = Yp, M = M)
#  #>
#  #> Type:
#  #> [1] "Arc Slice Proximity Catch Digraph (AS-PCD) for 2D Points in Multiple Triangles with CC-Vertex Regions"
#  summary(Arcs)
#  #> Call:
#  #> ArcsAS(Xp = Xp, Yp = Yp, M = M)
#  #>
#  #> Type of the digraph:
#  #> [1] "Arc Slice Proximity Catch Digraph (AS-PCD) for 2D Points in Multiple Triangles with CC-Vertex Regions"
#  #>
#  #>  Vertices of the digraph =  Xp
#  #>  Partition points of the region =  Yp
#  #>
#  #>  Selected tail (or source) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #>           [,1]      [,2]
#  #> [1,] 0.5281055 0.2460877
#  #> [2,] 0.5514350 0.3279207
#  #> [3,] 0.5514350 0.3279207
#  #>
#  #>  Selected head (or end) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #>           [,1]      [,2]
#  #> [1,] 0.5514350 0.3279207
#  #> [2,] 0.7883051 0.4533342
#  #> [3,] 0.5281055 0.2460877
#  #>
#  #> Parameters of the digraph
#  #> $center
#  #> [1] "CC"
#  #>
#  #> Various quantities of the digraph
#  #>         number of vertices number of partition points
#  #>                 7.00000000                 5.00000000
#  #>        number of triangles             number of arcs
#  #>                 4.00000000                 3.00000000
#  #>                arc density
#  #>                 0.07142857
#  plot(Arcs, asp=1)

## -----------------------------------------------------------------------------
M<-c(1,1,1) #try also M<-c(1,2,3) #or M<-"CC"
r<-1.5 #try also r<-2 or r=1.25

## ----eval=F-------------------------------------------------------------------
#  NumArcsPE(Xp,Yp,r,M)
#  #> $num.arcs
#  #> [1] 3
#  #>
#  #> $tri.num.arcs
#  #> [1] 1 0 0 2
#  #>
#  #> $num.in.conv.hull
#  #> [1] 7
#  #>
#  #> $num.in.tris
#  #> [1] 2 1 1 3
#  #>
#  #> $weight.vec
#  #> [1] 0.2214646 0.2173192 0.2593852 0.2648197
#  #>
#  #> $del.tri.ind
#  #>      [,1] [,2] [,3] [,4]
#  #> [1,]    1    5    3    3
#  #> [2,]    3    2    4    1
#  #> [3,]    2    3    5    4
#  #>
#  #> $data.tri.ind
#  #>  [1]  1  4  1  3 NA NA  4 NA  4  2

## ----include=FALSE------------------------------------------------------------
IM<-IncMatPE(Xp,Yp,r,M)
head(IM)

## ----adPEarcs1, fig.cap="The arcs of the PE-PCD for the 2D artificial data set using the CM-vertex regions and expansion parameter $r=1.5$ together with the Delaunay triangles based on the $Y$ points (dashed lines)."----
plotPEarcs(Xp,Yp,r,M,xlab="",ylab="")

## ----adPEpr1, fig.cap="The PE proximity regions for all the points the 2D artificial data set  using the CM-vertex regions and expansion parameter $r=1.5$ together with the Delaunay triangles based on the $Y$ points (dashed lines)."----
plotPEregs(Xp,Yp,r,M,xlab="",ylab="")

## ----adPEarcs2, eval=F, fig.cap="The arcs of the PE-PCD for the 2D artificial data set using the CM-vertex regions and expansion parameter $r=1.5$ together with the Delaunay triangles based on the $Y$ points (dashed lines)."----
#  Arcs<-ArcsPE(Xp,Yp,r,M)
#  Arcs
#  #> Call:
#  #> ArcsPE(Xp = Xp, Yp = Yp, r = r, M = M)
#  #>
#  #> Type:
#  #> [1] "Proportional Edge Proximity Catch Digraph (PE-PCD) for 2D points in Multiple Triangles with Expansion parameter r = 1.5 and Center M = (1,1,1)"
#  summary(Arcs)
#  #> Call:
#  #> ArcsPE(Xp = Xp, Yp = Yp, r = r, M = M)
#  #>
#  #> Type of the digraph:
#  #> [1] "Proportional Edge Proximity Catch Digraph (PE-PCD) for 2D points in Multiple Triangles with Expansion parameter r = 1.5 and Center M = (1,1,1)"
#  #>
#  #>  Vertices of the digraph =  Xp
#  #>  Partition points of the region =  Yp
#  #>
#  #>  Selected tail (or source) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #>           [,1]      [,2]
#  #> [1,] 0.4089769 0.6775706
#  #> [2,] 0.5281055 0.2460877
#  #> [3,] 0.5514350 0.3279207
#  #>
#  #>  Selected head (or end) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #>           [,1]      [,2]
#  #> [1,] 0.2875775 0.9568333
#  #> [2,] 0.5514350 0.3279207
#  #> [3,] 0.5281055 0.2460877
#  #>
#  #> Parameters of the digraph
#  #> $center
#  #> [1] 1 1 1
#  #>
#  #> $`expansion parameter`
#  #> [1] 1.5
#  #>
#  #> Various quantities of the digraph
#  #>         number of vertices number of partition points
#  #>                 7.00000000                 5.00000000
#  #>        number of triangles             number of arcs
#  #>                 4.00000000                 3.00000000
#  #>                arc density
#  #>                 0.07142857
#  plot(Arcs)

## ----eval=F-------------------------------------------------------------------
#  TSArcDensPE(Xp,Yp,r) #try also TSArcDensPE(Xp,Yp,r,alt="l") or with alt="g"
#  #>
#  #>  Large Sample z-Test Based on Arc Density of PE-PCD for Testing
#  #>  Uniformity of 2D Data ---
#  #>  without Convex Hull Correction
#  #>
#  #> data:  Xp
#  #> standardized arc density (i.e., Z) = -0.21983, p-value = 0.826
#  #> alternative hypothesis: true (expected) arc density is not equal to 0.09712203
#  #> 95 percent confidence interval:
#  #>  0.04234726 0.14084889
#  #> sample estimates:
#  #> arc density
#  #>  0.09159807

## ----eval=F-------------------------------------------------------------------
#  PEdom(Xp,Yp,r,M) #try also PEdom(Xp,Yp,r=2,M)
#  #> $dom.num
#  #> [1] 5
#  #>
#  #> $ind.mds
#  #> [1]  3 10  4  9  2
#  #>
#  #> $tri.dom.nums
#  #> [1] 1 1 1 2
#  PEdom.nd(Xp,Yp,r) #try also PEdom.nd(Xp,Yp,r=1.25)
#  #> $dom.num
#  #> [1] 5
#  #>
#  #> $ind.mds
#  #> [1]  3 10  4  2  9
#  #>
#  #> $tri.dom.nums
#  #> [1] 1 1 1 2

## ----eval=F-------------------------------------------------------------------
#  TSDomPEBin(Xp,Yp,r) #try also TSDomPEBin(Xp,Yp,r,alt="g") or with alt="l"
#  #>
#  #>  Large Sample Binomial Test based on the Domination Number of PE-PCD for
#  #>  Testing Uniformity of 2D Data ---
#  #>  without Convex Hull Correction
#  #>
#  #> data:  Xp
#  #> # of times domination number is <= 2 = 4, p-value = 0.5785
#  #> alternative hypothesis: true Pr(Domination Number <=2) is not equal to 0.7413
#  #> 95 percent confidence interval:
#  #>  0.3976354 1.0000000
#  #> sample estimates:
#  #>           domination number   || Pr(domination number <= 2)
#  #>                             5                             1

## ----eval=F-------------------------------------------------------------------
#  TSDomPENorm(Xp,Yp,r) #try also TSDomPENorm(Xp,Yp,r,alt="g") or with alt="l"
#  #>
#  #>  Normal Approximation to the Domination Number of PE-PCD for Testing
#  #>  Uniformity of 2D Data ---
#  #>  without Convex Hull Correction
#  #>
#  #> data:  Xp
#  #> standardized domination number (i.e., Z) = 1.1815, p-value = 0.2374
#  #> alternative hypothesis: true expected domination number is not equal to 2.9652
#  #> 95 percent confidence interval:
#  #>  3.283383 6.716617
#  #> sample estimates:
#  #>          domination number   || Pr(domination number = 3)
#  #>                            5                            1

## -----------------------------------------------------------------------------
M<-c(1,1,1) #try also M<-c(1,2,3)
tau<-1.5 #try also tau<-2

## ----eval=F-------------------------------------------------------------------
#  NumArcsCS(Xp,Yp,tau,M)
#  #> $num.arcs
#  #> [1] 3
#  #>
#  #> $tri.num.arcs
#  #> [1] 1 0 0 2
#  #>
#  #> $num.in.conv.hull
#  #> [1] 7
#  #>
#  #> $num.in.tris
#  #> [1] 2 1 1 3
#  #>
#  #> $weight.vec
#  #> [1] 0.2214646 0.2173192 0.2593852 0.2648197
#  #>
#  #> $del.tri.ind
#  #>      [,1] [,2] [,3] [,4]
#  #> [1,]    1    5    3    3
#  #> [2,]    3    2    4    1
#  #> [3,]    2    3    5    4
#  #>
#  #> $data.tri.ind
#  #>  [1]  1  4  1  3 NA NA  4 NA  4  2

## ----include=FALSE------------------------------------------------------------
IM<-IncMatCS(Xp,Yp,tau,M)
head(IM)

## ----adCSarcs1, fig.cap="The arcs of the CS-PCD for the 2D artificial data set using the CM-edge regions and expansion parameter $t=1.5$ together with the Delaunay triangles based on the $Y$ points (dashed lines)."----
plotCSarcs(Xp,Yp,tau,M,xlab="",ylab="")

## ----adCSpr1, fig.cap="The CS proximity regions for all the points the 2D artificial data set  using the CM-edge regions and expansion parameter $t=1.5$ together with the Delaunay triangles based on the $Y$ points (dashed lines)."----
plotCSregs(Xp,Yp,tau,M,xlab="",ylab="")

## ----adCSarcs2, eval=F, fig.cap="The arcs of the CS-PCD for the 2D artificial data set using the CM-edge regions and expansion parameter $t=1.5$ together with the Delaunay triangles based on the $Y$ points (dashed lines)."----
#  Arcs<-ArcsCS(Xp,Yp,tau,M)
#  Arcs
#  #> Call:
#  #> ArcsCS(Xp = Xp, Yp = Yp, t = tau, M = M)
#  #>
#  #> Type:
#  #> [1] "Central Similarity Proximity Catch Digraph (CS-PCD) for 2D Points in the Multiple Triangles with Expansion Parameter t = 1.5 and Center M = (1,1,1)"
#  summary(Arcs)
#  #> Call:
#  #> ArcsCS(Xp = Xp, Yp = Yp, t = tau, M = M)
#  #>
#  #> Type of the digraph:
#  #> [1] "Central Similarity Proximity Catch Digraph (CS-PCD) for 2D Points in the Multiple Triangles with Expansion Parameter t = 1.5 and Center M = (1,1,1)"
#  #>
#  #>  Vertices of the digraph =  Xp
#  #>  Partition points of the region =  Yp
#  #>
#  #>  Selected tail (or source) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #>           [,1]      [,2]
#  #> [1,] 0.4089769 0.6775706
#  #> [2,] 0.5281055 0.2460877
#  #> [3,] 0.5514350 0.3279207
#  #>
#  #>  Selected head (or end) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #>           [,1]      [,2]
#  #> [1,] 0.2875775 0.9568333
#  #> [2,] 0.5514350 0.3279207
#  #> [3,] 0.5281055 0.2460877
#  #>
#  #> Parameters of the digraph
#  #> $center
#  #> [1] 1 1 1
#  #>
#  #> $`expansion parameter`
#  #> [1] 1.5
#  #>
#  #> Various quantities of the digraph
#  #>         number of vertices number of partition points
#  #>                 7.00000000                 5.00000000
#  #>        number of triangles             number of arcs
#  #>                 4.00000000                 3.00000000
#  #>                arc density
#  #>                 0.07142857
#  plot(Arcs)

## ----eval=F-------------------------------------------------------------------
#  TSArcDensCS(Xp,Yp,tau) #try also TSArcDensCS(Xp,Yp,tau,alt="l") or with alt="g"
#  #>
#  #>  Large Sample z-Test Based on Arc Density of CS-PCD for Testing
#  #>  Uniformity of 2D Data ---
#  #>  without Convex Hull Correction
#  #>
#  #> data:  Xp
#  #> standardized arc density (i.e., Z) = 0.6039, p-value = 0.5459
#  #> alternative hypothesis: true (expected) arc density is not equal to 0.06749794
#  #> 95 percent confidence interval:
#  #>  0.0252619 0.1473522
#  #> sample estimates:
#  #> arc density
#  #>  0.08630702

