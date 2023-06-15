## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>",fig.width=6, fig.height=4, fig.align = "center") 

## ----setup, message=FALSE, results='hide'-------------------------------------
library(pcds)

## -----------------------------------------------------------------------------
a<-0; b<-10; int<-c(a,b)
#nx is number of X points (target) and ny is number of Y points (nontarget)
nx<-10; ny<-5; #try also nx<-40; ny<-10 or nx<-1000; ny<-10;

xf<-(b-a)*.1
set.seed(11)
Xp<-runif(nx,a-xf,b+xf)
Yp<-runif(ny,-1,1)*(b-a)/(10*ny)+ ((b-a)/(ny-1))*(0:(ny-1)) #try also Yp<-runif(ny,a,b)

## ----arti-data1D-plot, eval=F, fig.cap="The scatterplot of the 1D artificial data set with two classes; black circles are class $X$ and red triangles are class $Y$ points."----
#  XYpts =c(Xp,Yp) #combined Xp and Yp
#  lab=c(rep(1,nx),rep(2,ny))
#  lab.fac=as.factor(lab)
#  plot(XYpts,rep(0,length(XYpts)),col=lab,pch=lab,xlab="x",ylab="",ylim=.005*c(-1,1),
#       main="Scatterplot of 1D Points from Two Classes")

## ----ADpl, fig.cap="The plot of the $X$ points (black circles) in the artificial data set together with the intervals (blue rounded brackets) based on $Y$ points (red circles)."----
Xlim<-range(Xp)
Ylim<-.005*c(-1,1)
xd<-Xlim[2]-Xlim[1]
plot(Xp,rep(0,nx),xlab="x", ylab=" ",xlim=Xlim+xd*c(-.05,.05), yaxt='n',
     ylim=Ylim,pch=".",cex=3,main="X Points and Intervals based on Y Points")
abline(h=0,lty=2)
#now, we add the intervals based on Y points
par(new=TRUE)
plotIntervals(Xp,Yp,xlab="",ylab="",main="")

## -----------------------------------------------------------------------------
r<-2 #try also r=1.5
c<-.4  #try also c=.3

## ----eval=F-------------------------------------------------------------------
#  Narcs = num.arcsPE1D(Xp,Yp,r,c)
#  summary(Narcs)
#  #> Call:
#  #> num.arcsPE1D(Xp = Xp, Yp = Yp, r = r, c = c)
#  #>
#  #> Description of the output:
#  #> Number of Arcs of the PE-PCD with vertices Xp and Related Quantities for the Induced Subdigraphs for the Points in the Partition Intervals
#  #>
#  #> Number of data (Xp) points in the range of Yp (nontarget) points =  6
#  #> Number of data points in the partition intervals based on Yp points =  3 3 2 0 1 1
#  #> Number of arcs in the entire digraph =  5
#  #> Numbers of arcs in the induced subdigraphs in the partition intervals =  4 1 0 0 0 0
#  #> Lengths of the (middle) partition intervals (used as weights in the arc density of multi-interval case):
#  #> 2.606255 2.686573 2.477544 2.453178
#  #>
#  #> End points of the partition intervals (each column refers to a partition interval):
#  #>            [,1]       [,2]     [,3]     [,4]      [,5]     [,6]
#  #> [1,]       -Inf -0.1299548 2.476300 5.162873  7.640417 10.09359
#  #> [2,] -0.1299548  2.4763001 5.162873 7.640417 10.093595      Inf
#  #>
#  #> Indices of the partition intervals data points resides:
#  #> 2 1 3 1 1 6 2 3 5 2
#  #>
#  #plot(Narcs)

## ----AD1dPEarcs2, fig.cap="The arcs of the PE-PCD for the 1D artificial data set with centrality parameter $c=.4$, the end points of the $Y$ intervals (red) and the centers (green) are plotted with vertical dashed lines."----
jit<-.1
set.seed(1)
plotPEarcs1D(Xp,Yp,r,c,jit,xlab="",ylab="",centers=TRUE)

## ----AD1dPEPR2, fig.cap="The PE proximity regions (blue) for the 1D artificial data set, the end points of the $Y$ intervals (black) and the centers (green) are plotted with vertical dashed lines."----
set.seed(12)
plotPEregs1D(Xp,Yp,r,c,xlab="x",ylab="",centers = TRUE)

## ----AD1dPEarcs3, eval=F, fig.cap="The arcs of the PE-PCD for the 1D artificial data set; the end points of the intervals are plotted with vertical dashed lines."----
#  Arcs<-arcsPE1D(Xp,Yp,r,c)
#  Arcs
#  #> Call:
#  #> arcsPE1D(Xp = Xp, Yp = Yp, r = r, c = c)
#  #>
#  #> Type:
#  #> [1] "Proportional Edge Proximity Catch Digraph (PE-PCD) for 1D Points with Expansion Parameter r = 2 and Centrality Parameter c = 0.4"
#  summary(Arcs)
#  #> Call:
#  #> arcsPE1D(Xp = Xp, Yp = Yp, r = r, c = c)
#  #>
#  #> Type of the digraph:
#  #> [1] "Proportional Edge Proximity Catch Digraph (PE-PCD) for 1D Points with Expansion Parameter r = 2 and Centrality Parameter c = 0.4"
#  #>
#  #>  Vertices of the digraph =  Xp
#  #>  Partition points of the region =  Yp
#  #>
#  #>  Selected tail (or source) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #> [1] 3.907723 4.479377 5.617220 8.459662 8.459662 9.596209
#  #>
#  #>  Selected head (or end) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #> [1] 4.479377 3.907723 5.337266 9.596209 9.709029 9.709029
#  #>
#  #> Parameters of the digraph
#  #> centrality parameter  expansion parameter
#  #>                  0.4                  2.0
#  #>
#  #> Various quantities of the digraph
#  #>         number of vertices number of partition points
#  #>                10.00000000                 5.00000000
#  #>        number of intervals             number of arcs
#  #>                 6.00000000                 6.00000000
#  #>                arc density
#  #>                 0.06666667
#  
#  set.seed(1)
#  plot(Arcs)

## ----eval=F-------------------------------------------------------------------
#  PEarc.dens.test1D(Xp,Yp,r,c) # try also PEarc.dens.test1D(Xp,Yp,r,c,alt="l")
#  #>
#  #>  Large Sample z-Test Based on Arc Density of PE-PCD for Testing
#  #>  Uniformity of 1D Data ---
#  #>  without End Interval Correction
#  #>
#  #> data:  Xp
#  #> standardized arc density (i.e., Z) = -0.77073, p-value = 0.4409
#  #> alternative hypothesis: true (expected) arc density is not equal to 0.1279913
#  #> 95 percent confidence interval:
#  #>  0.05557408 0.15952931
#  #> sample estimates:
#  #> arc density
#  #>   0.1075517

## ----eval=F-------------------------------------------------------------------
#  PEdom.num1D(Xp,Yp,r,c)
#  #> $dom.num
#  #> [1] 6
#  #>
#  #> $mds
#  #> [1] -0.453322  2.450930  3.907723  5.617220  8.459662 10.285607
#  #>
#  #> $ind.mds
#  #> [1] 6 1 3 9 2 5
#  #>
#  #> $int.dom.nums
#  #> [1] 1 1 1 1 1 0 0 1
#  PEdom.num1Dnondeg(Xp,Yp,r)
#  #> $dom.num
#  #> [1] 7
#  #>
#  #> $mds
#  #> [1] -0.453322  2.450930  3.907723  5.617220  8.459662  9.596209 10.285607
#  #>
#  #> $ind.mds
#  #> [1] 6 1 3 9 2 4 5
#  #>
#  #> $int.dom.nums
#  #> [1] 1 1 1 1 2 0 0 1

## ----eval=F-------------------------------------------------------------------
#  PEdom.num.binom.test1D(Xp,Yp,c) #try also PEdom.num.binom.test1D(Xp,Yp,c,alt="l")
#  #>
#  #>  Large Sample Binomial Test based on the Domination Number of PE-PCD for
#  #>  Testing Uniformity of 1D Data ---
#  #>  without End Interval Correction
#  #>
#  #> data:  Xp
#  #> adjusted domination number = 0, p-value = 0.3042
#  #> alternative hypothesis: true Pr(Domination Number=2) is not equal to 0.375
#  #> 95 percent confidence interval:
#  #>  0.0000000 0.6023646
#  #> sample estimates:
#  #>          domination number   || Pr(domination number = 2)
#  #>                            6                            0

## -----------------------------------------------------------------------------
tau<-2; c<-.4

## ----eval=F-------------------------------------------------------------------
#  Narcs = num.arcsCS1D(Xp,Yp,tau,c)
#  summary(Narcs)
#  #> Call:
#  #> num.arcsCS1D(Xp = Xp, Yp = Yp, t = tau, c = c)
#  #>
#  #> Description of the output:
#  #> Number of Arcs of the CS-PCD with vertices Xp and Related Quantities for the Induced Subdigraphs for the Points in the Partition Intervals
#  #>
#  #> Number of data (Xp) points in the range of Yp (nontarget) points =  6
#  #> Number of data points in the partition intervals based on Yp points =  3 3 2 0 1 1
#  #> Number of arcs in the entire digraph =  6
#  #> Numbers of arcs in the induced subdigraphs in the partition intervals =  4 2 0 0 0 0
#  #> Lengths of the (middle) partition intervals (used as weights in the arc density of multi-interval case):
#  #> 2.606255 2.686573 2.477544 2.453178
#  #>
#  #> End points of the partition intervals (each column refers to a partition interval):
#  #>            [,1]       [,2]     [,3]     [,4]      [,5]     [,6]
#  #> [1,]       -Inf -0.1299548 2.476300 5.162873  7.640417 10.09359
#  #> [2,] -0.1299548  2.4763001 5.162873 7.640417 10.093595      Inf
#  #>
#  #> Indices of the partition intervals data points resides:
#  #> 2 1 3 1 1 6 2 3 5 2
#  
#  #plot(Narcs)

## ----AD1dCSarcs2, fig.cap="The arcs of the CS-PCD for the 1D artificial data set with centrality parameter $c=.4$, the end points of the $Y$ intervals (red) and the centers (green) are plotted with vertical dashed lines."----
set.seed(1)
plotCSarcs1D(Xp,Yp,tau,c,jit,xlab="",ylab="",centers=TRUE)

## ----AD1dCSPR2, fig.cap="The CS proximity regions (blue) for the 1D artificial data set, the end points of the $Y$ intervals (black) and the centers (green) are plotted with vertical dashed lines."----
plotCSregs1D(Xp,Yp,tau,c,xlab="",ylab="",centers = TRUE)

## ----AD1dCSarcs3, eval=F, fig.cap="The arcs of the CS-PCD for the 1D artificial data set; the end points of the intervals are plotted with vertical dashed lines."----
#  Arcs<-arcsCS1D(Xp,Yp,tau,c)
#  Arcs
#  #> Call:
#  #> arcsCS1D(Xp = Xp, Yp = Yp, t = tau, c = c)
#  #>
#  #> Type:
#  #> [1] "Central Similarity Proximity Catch Digraph (CS-PCD) for 1D Points with Expansion Parameter t = 2 and Centrality Parameter c = 0.4"
#  summary(Arcs)
#  #> Call:
#  #> arcsCS1D(Xp = Xp, Yp = Yp, t = tau, c = c)
#  #>
#  #> Type of the digraph:
#  #> [1] "Central Similarity Proximity Catch Digraph (CS-PCD) for 1D Points with Expansion Parameter t = 2 and Centrality Parameter c = 0.4"
#  #>
#  #>  Vertices of the digraph =  Xp
#  #>  Partition points of the region =  Yp
#  #>
#  #>  Selected tail (or source) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #> [1] 3.907723 4.479377 5.337266 5.617220 8.459662 8.459662
#  #>
#  #>  Selected head (or end) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #> [1] 4.479377 3.907723 5.617220 5.337266 9.596209 9.709029
#  #>
#  #> Parameters of the digraph
#  #> centrality parameter  expansion parameter
#  #>                  0.4                  2.0
#  #> Various quantities of the digraph
#  #>         number of vertices number of partition points
#  #>                10.00000000                 5.00000000
#  #>        number of intervals             number of arcs
#  #>                 6.00000000                 8.00000000
#  #>                arc density
#  #>                 0.08888889
#  plot(Arcs)

## ----eval=F-------------------------------------------------------------------
#  CSarc.dens.test1D(Xp,Yp,tau,c) #try also CSarc.dens.test1D(Xp,Yp,tau,c,alt="l")
#  #>
#  #>  Large Sample z-Test Based on Arc Density of CS-PCD for Testing
#  #>  Uniformity of 1D Data ---
#  #>  without End Interval Correction
#  #>
#  #> data:  Xp
#  #> standardized arc density (i.e., Z) = -0.75628, p-value = 0.4495
#  #> alternative hypothesis: true (expected) arc density is not equal to 0.1658151
#  #> 95 percent confidence interval:
#  #>  0.08507259 0.20159565
#  #> sample estimates:
#  #> arc density
#  #>   0.1433341

