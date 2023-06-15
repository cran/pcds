## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>",fig.width=6, fig.height=4, fig.align = "center") 

## ----setup, message=FALSE, results='hide'-------------------------------------
library(pcds)

## -----------------------------------------------------------------------------
c<-.4
a<-0; b<-10; int<-c(a,b)
n<-5 #try also n=10, 50, 100

## -----------------------------------------------------------------------------
xf<-(int[2]-int[1])*.1

set.seed(123)
Xp<-runif(n,a-xf,b+xf)

## ----oneint, fig.cap="Scatterplot of the uniform $X$ points in the interval $(0,10)$."----
Xp2 =c(Xp,int)
Xlim<-range(Xp2)
Ylim<-.005*c(-1,1)
xd<-Xlim[2]-Xlim[1]
plot(Xp2,rep(0,n+2),xlab="x", ylab=" ",xlim=Xlim+xd*c(-.05,.05), yaxt='n',
     ylim=Ylim,pch=".",cex=3,
     main="X Points and One Interval (based on Y points)")
abline(h=0,lty=2)
#now, we add the intervals based on Y points
par(new=TRUE)
plotIntervals(Xp,int,xlab="",ylab="",main="")

## ----include=F----------------------------------------------------------------
r<-1.5

## ----eval=F-------------------------------------------------------------------
#  r<-1.5
#  NPEint(7,int,r,c)
#  #> [1]  5.5 10.0
#  NPEint(Xp[1],int,r,c)
#  #> [1] 0.000000 3.676395

## ----eval=F-------------------------------------------------------------------
#  IarcPEint(7,7,int,r,c)
#  #> [1] 1
#  IarcPEint(Xp[1],Xp[2],int,r,c)
#  #> [1] 0

## ----eval=F-------------------------------------------------------------------
#  Narcs = num.arcsPEint(Xp,int,r,c)
#  summary(Narcs)
#  #> Call:
#  #> num.arcsPEint(Xp = Xp, int = int, r = r, c = c)
#  #>
#  #> Description of the output:
#  #> Number of Arcs of the CS-PCD with vertices Xp and Quantities Related to the Support Interval
#  #>
#  #> Number of data (Xp) points in the range of Yp (nontarget) points =  4
#  #> Number of data points in the partition intervals based on Yp points =  0 4 1
#  #> Number of arcs in the entire digraph =  2
#  #> Numbers of arcs in the induced subdigraphs in the partition intervals =  0 2 0
#  #>
#  #> End points of the support interval:
#  #>  0 10
#  #> Indices of data points in the intervals:
#  #> left end interval:  NA
#  #> middle interval:  1 2 3 4
#  #> right end interval:  5
#  #>
#  #plot(Narcs)

## ----1dPEarcs2, fig.cap="The arcs of the PE-PCD for a 1D data set, the end points of the interval (red) and the center (green) are plotted with vertical dashed lines."----
jit<-.1
set.seed(1)
plotPEarcs.int(Xp,int,r=1.5,c=.3,jit,xlab="",ylab="",center=TRUE)

## ----1dPEpr2, fig.cap="The PE proximity regions for 10 $X$ points on the real line, the end points of the interval (black) and the center (green) are plotted with vertical dashed lines."----
set.seed(1)
plotPEregs.int(Xp,int,r,c,xlab="x",ylab="",center = TRUE)

## ----PEarcs1i, eval=F, fig.cap="Arcs of the PE-PCD for $X$ points in the interval $(0,10)$. Arcs are jittered along the $y$-axis for better visualization."----
#  Arcs<-arcsPEint(Xp,int,r,c)
#  Arcs
#  #> Call:
#  #> arcsPEint(Xp = Xp, int = int, r = r, c = c)
#  #>
#  #> Type:
#  #> [1] "Proportional Edge Proximity Catch Digraph (PE-PCD) for 1D Points with Expansion Parameter r = 1.5 and Centrality Parameter c = 0.4"
#  summary(Arcs)
#  #> Call:
#  #> arcsPEint(Xp = Xp, int = int, r = r, c = c)
#  #>
#  #> Type of the digraph:
#  #> [1] "Proportional Edge Proximity Catch Digraph (PE-PCD) for 1D Points with Expansion Parameter r = 1.5 and Centrality Parameter c = 0.4"
#  #>
#  #>  Vertices of the digraph =  Xp
#  #>  Partition points of the region =  int
#  #>
#  #>  Selected tail (or source) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #> [1] 8.459662 3.907723
#  #>
#  #>  Selected head (or end) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #> [1] 9.596209 2.450930
#  #>
#  #> Parameters of the digraph
#  #> $`centrality parameter`
#  #> [1] 0.4
#  #>
#  #> $`expansion parameter`
#  #> [1] 1.5
#  #>
#  #> Various quantities of the digraph
#  #>         number of vertices number of partition points
#  #>                        5.0                        2.0
#  #>        number of intervals             number of arcs
#  #>                        1.0                        2.0
#  #>                arc density
#  #>                        0.1
#  
#  plot(Arcs)

## ----include=F----------------------------------------------------------------
tau<-1.5

## ----eval=F-------------------------------------------------------------------
#  tau<-1.5
#  NCSint(Xp[3],int,tau,c)
#  #> [1]  0 10

## ----eval=F-------------------------------------------------------------------
#  IarcCSint(Xp[1],Xp[2],int,tau,c) #try also IarcCSint(Xp[2],Xp[1],int,tau,c)
#  #> [1] 0

## ----eval=F-------------------------------------------------------------------
#  Narcs = num.arcsCSint(Xp,int,tau,c)
#  summary(Narcs)
#  #> Call:
#  #> num.arcsCSint(Xp = Xp, int = int, t = tau, c = c)
#  #>
#  #> Description of the output:
#  #> Number of Arcs of the CS-PCD with vertices Xp and Quantities Related to the Support Interval
#  #>
#  #> Number of data (Xp) points in the range of Yp (nontarget) points =  4
#  #> Number of data points in the partition intervals based on Yp points =  0 4 1
#  #> Number of arcs in the entire digraph =  5
#  #> Numbers of arcs in the induced subdigraphs in the partition intervals =  0 5 0
#  #>
#  #> End points of the support interval:
#  #>  0 10
#  #> Indices of data points in the intervals:
#  #> left end interval:  NA
#  #> middle interval:  1 2 3 4
#  #> right end interval:  5
#  #>
#  #plot(Narcs)

## ----1dCSarcs2, fig.cap="The arcs of the CS-PCD for a 1D data set, the end points of the interval (red) and the center (green) are plotted with vertical dashed lines."----
set.seed(1)
plotCSarcs.int(Xp,int,t=1.5,c=.3,jit,xlab="",ylab="",center=TRUE)

## ----1dCSpr2, fig.cap="The CS proximity regions for 10 $X$ points on the real line, the end points of the interval (black) and the center (green) are plotted with vertical dashed lines."----
set.seed(1)
plotCSregs.int(Xp,int,tau,c,xlab="x",ylab="",center=TRUE)

## ----CSarcs1i, eval=F, fig.cap="Arcs of the CS-PCD for points in the interval $(0,10)$. Arcs are jittered along the $y$-axis for better visualization."----
#  Arcs<-arcsCSint(Xp,int,tau,c)
#  Arcs
#  #> Call:
#  #> arcsCSint(Xp = Xp, int = int, t = tau, c = c)
#  #>
#  #> Type:
#  #> [1] "Central Similarity Proximity Catch Digraph (CS-PCD) for 1D Points with Expansion Parameter t = 1.5 and Centrality Parameter c = 0.4"
#  summary(Arcs)
#  #> Call:
#  #> arcsCSint(Xp = Xp, int = int, t = tau, c = c)
#  #>
#  #> Type of the digraph:
#  #> [1] "Central Similarity Proximity Catch Digraph (CS-PCD) for 1D Points with Expansion Parameter t = 1.5 and Centrality Parameter c = 0.4"
#  #>
#  #>  Vertices of the digraph =  Xp
#  #>  Partition points of the region =  int
#  #>
#  #>  Selected tail (or source) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #> [1] 2.450930 8.459662 3.907723 3.907723 3.907723
#  #>
#  #>  Selected head (or end) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #> [1] 3.907723 9.596209 2.450930 8.459662 9.596209
#  #>
#  #> Parameters of the digraph
#  #> $`centrality parameter`
#  #> [1] 0.4
#  #>
#  #> $`expansion parameter`
#  #> [1] 1.5
#  #>
#  #> Various quantities of the digraph
#  #>         number of vertices number of partition points
#  #>                       5.00                       2.00
#  #>        number of intervals             number of arcs
#  #>                       1.00                       5.00
#  #>                arc density
#  #>                       0.25
#  plot(Arcs)

## ----eval=F-------------------------------------------------------------------
#  c<-.4 #try also c<-runif(1)
#  a<-0; b<-10
#  int = c(a,b)
#  centerMc(int,c)
#  #> [1] 4
#  
#  n<-5 #try also n=10, 50, 100
#  y<-runif(n)
#  centersMc(y,c)
#  #> [1] 0.2887174 0.6417875 0.7558345 0.9169039

## ----eval=F-------------------------------------------------------------------
#  c<-.4
#  a<-0; b<-10; int = c(a,b)
#  rel.vert.mid.int(6,int,c)
#  #> $rv
#  #> [1] 2
#  #>
#  #> $int
#  #> vertex 1 vertex 2
#  #>        0       10

## ----include=F----------------------------------------------------------------
n<-5 #try also n=10, 50, 100

## ----1DVR, eval=F, fig.cap="$M_c$-Vertex regions in the interval $(0,10)$. Also plotted are the $X$ points which are labeled according to the vertex region they reside in.", echo=FALSE----
#  Mc<-centerMc(int,c)
#  n<-10  #try also n<-20
#  xr<-range(a,b,Mc)
#  xf<-(int[2]-int[1])*.5
#  Xp<-runif(n,a,b)
#  
#  Rv<-vector()
#  for (i in 1:n)
#    Rv<-c(Rv,rel.vert.mid.int(Xp[i],int,c)$rv)
#  #Rv
#  
#  jit<-.1
#  yjit<-runif(n,-jit,jit)
#  
#  Xlim<-range(a,b,Xp)
#  xd<-Xlim[2]-Xlim[1]
#  
#  plot(cbind(Mc,0),main="Vertex region indices for the X points", xlab=" ", ylab=" ",
#       xlim=Xlim+xd*c(-.05,.05),ylim=3*range(yjit),pch=".",cex=3)
#  abline(h=0)
#  points(Xp,yjit,pch=".",cex=3)
#  abline(v=c(a,b,Mc),lty=2,col=c(1,1,2))
#  text(Xp,yjit,labels=factor(Rv))
#  text(cbind(c(a,b,Mc),.02),c("rv=1","rv=2","Mc"))

## ----eval=F-------------------------------------------------------------------
#  a<-0; b<-10; int<-c(a,b)
#  rel.vert.end.int(-6,int)
#  #> $rv
#  #> [1] 1
#  #>
#  #> $int
#  #> vertex 1 vertex 2
#  #>        0       10
#  rel.vert.end.int(16,int)
#  #> $rv
#  #> [1] 2
#  #>
#  #> $int
#  #> vertex 1 vertex 2
#  #>        0       10

