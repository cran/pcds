## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>",fig.width=6, fig.height=4, fig.align = "center") 

## ----setup, message=FALSE, results='hide'-------------------------------------
library(pcds)

## -----------------------------------------------------------------------------
A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
Tr<-rbind(A,B,C);
n<-5

set.seed(1)
Xp<-runif.tri(n,Tr)$g  #try also Xp<-cbind(runif(n,1,2),runif(n,0,2))
M<-c(1.6,1.2) #try also M<-as.numeric(runif.tri(1,Tr)$g) or M="CC"

## ----one-tri, eval=F, fig.cap="Scatterplot of the $X$ points in the triangle $T=T(A,B,C)$ with vertices $A=(1,1)$, $B=(2,0)$, and $C=(1.5,2)$."----
#  Xlim<-range(Tr[,1])
#  Ylim<-range(Tr[,2])
#  plot(Tr,pch=".",xlab="",ylab="",xlim=Xlim,ylim=Ylim+c(0,.1),main="Points in One Triangle")
#  polygon(Tr)
#  points(Xp)
#  
#  #add the vertex names and annotation
#  txt<-rbind(Tr)
#  xc<-txt[,1]+c(-.01,.015,.02)
#  yc<-txt[,2]+c(.02,.02,.02)
#  txt.str<-c("A","B","C")
#  text(xc,yc,txt.str)

## ----eval=F-------------------------------------------------------------------
#  cent<-c(1,1); rad<-1; p<-c(1.4,1.2) #try also cent<-runif(2); rad<-runif(1); p<-runif(2);
#  in.circle(p,cent,rad)
#  p<-c(.4,-.2)
#  in.circle(p,cent,rad)
#  
#  #> [1] TRUE
#  #> [1] FALSE

## ----eval=F-------------------------------------------------------------------
#  ny<-5
#  Y<-cbind(runif(ny),runif(ny))
#  A<-c(1,1);
#  radius(A,Y)
#  #> $rad
#  #> [1] 0.3767951
#  #>
#  #> $index.of.clYpnt
#  #> [1] 4
#  #>
#  #> $closest.Ypnt
#  #> [1] 0.6684667 0.8209463

## ----eval=F-------------------------------------------------------------------
#  nx<-6
#  ny<-5
#  X<-cbind(runif(nx),runif(nx))
#  Y<-cbind(runif(ny),runif(ny))
#  Rad<-radii(X,Y)
#  Rad
#  #> $radiuses
#  #> [1] 0.38015546 0.23618472 0.02161322 0.48477828 0.05674010 0.16819521
#  #>
#  #> $indices.of.closest.points
#  #> [1] 4 4 4 4 1 3
#  #>
#  #> $closest.points
#  #>            [,1]      [,2]
#  #> [1,] 0.51863426 0.4590657
#  #> [2,] 0.51863426 0.4590657
#  #> [3,] 0.51863426 0.4590657
#  #> [4,] 0.51863426 0.4590657
#  #> [5,] 0.07067905 0.4068302
#  #> [6,] 0.31627171 0.2936034

## ----eval=F-------------------------------------------------------------------
#  P<-c(1.8,.5)
#  NAStri(P,Tr,M)
#  #> $L
#  #>      [,1] [,2]
#  #> [1,]    2    0
#  #> [2,]    2    0
#  #>
#  #> $R
#  #>          [,1]     [,2]
#  #> [1,] 1.741176 1.035294
#  #> [2,] 1.300000 0.700000
#  #>
#  #> $arc.slices
#  #>          [,1]     [,2]
#  #> [1,] 1.741176 1.035294
#  #> [2,] 1.300000 0.700000
#  #>
#  #> $Angles
#  #> [1] 1.680247 2.761086

## ----eval=F-------------------------------------------------------------------
#  #between two arbitrary points P1 and P2
#  P1<-as.numeric(runif.tri(1,Tr)$g)
#  P2<-as.numeric(runif.tri(1,Tr)$g)
#  IarcAStri(P1,P2,Tr,M)
#  #> [1] 0
#  #between the first two points in Xp
#  IarcAStri(Xp[1,],Xp[2,],Tr,M)
#  #> [1] 0

## ----eval=F-------------------------------------------------------------------
#  Narcs = num.arcsAStri(Xp,Tr)  #with default M="CC"; try also num.arcsAStri(Xp,Tr,M)
#  summary(Narcs)
#  #> Call:
#  #> num.arcsAStri(Xp = Xp, tri = Tr)
#  #>
#  #> Description of the output:
#  #> Number of Arcs of the AS-PCD and the Related Quantities with vertices Xp in One Triangle
#  #>
#  #> Number of data (Xp) points in the triangle =  5
#  #> Number of arcs in the digraph =  10
#  #>
#  #> Indices of data points in the triangle:
#  #> 1 2 3 4 5
#  #>
#  #plot(Narcs)

## ----eval=F-------------------------------------------------------------------
#  ASarc.dens.tri(Xp,Tr,M)
#  #> [1] 0.5

## ----ASarcs2, fig.cap="Arcs of the AS-PCD with 10 $X$ points, vertex regions are added with dashed lines."----
plotASarcs.tri(Xp,Tr,M,xlab="",ylab="",vert.reg = TRUE)

## ----ASarcs3, eval=F, fig.cap="Arcs of the AS-PCD with 10 $X$ points and vertex regions (dashed lines) are based on circumcenter. The vertices and the center are labeled."----
#  oldpar <- par(pty = "s")
#  plotASarcs.tri(Xp,Tr,asp=1,xlab="",ylab="",vert.reg = TRUE); M = (arcsAStri(Xp,Tr)$param)$c
#  
#  CC<-circumcenter.tri(Tr)
#  
#  #determine whether the center used for vertex regions is circumcenter or not
#  if (identical(M,CC) || identical(M,"CC"))
#  {cent<-CC
#  D1<-(B+C)/2; D2<-(A+C)/2; D3<-(A+B)/2;
#  Ds<-rbind(D1,D2,D3)
#  cent.name<-"CC"
#  } else
#  {cent<-M
#  cent.name<-"M"
#  Ds<-prj.cent2edges(Tr,M)
#  }
#  
#  #add the vertex names and annotation
#  txt<-rbind(Tr,cent,Ds)
#  xc<-txt[,1]+c(-.02,.02,.02,.05,.05,-0.03,-.01)
#  yc<-txt[,2]+c(.02,.02,.02,.07,.02,.05,-.06)
#  txt.str<-c("A","B","C",cent.name,"D1","D2","D3")
#  text(xc,yc,txt.str)
#  par(oldpar)

## ----ASPR1, fig.cap="AS proximity regions for the $X$ points used above."-----
M<-c(1.6,1.2) #try also M<-c(1.6620051,0.8136604) or M="CC"
oldpar <- par(pty = "s")
plotASregs.tri(Xp,Tr,M,vert.reg = T,xlab="",ylab="")
par(oldpar)

## ----eval=F-------------------------------------------------------------------
#  M=c(1.6,1.2) #try also M=c(1.6620051,0.8136604)
#  
#  Arcs<-arcsAStri(Xp,Tr,M) #try also Arcs<-arcsAStri(Xp,Tr) #uses the default center, namely circumcenter for M
#  Arcs
#  #> Call:
#  #> arcsAStri(Xp = Xp, tri = Tr, M = M)
#  #>
#  #> Type:
#  #> [1] "Arc Slice Proximity Catch Digraph (AS-PCD) for 2D Points in the Triangle with Center M = (1.6,1.2)"
#  summary(Arcs)
#  #> Call:
#  #> arcsAStri(Xp = Xp, tri = Tr, M = M)
#  #>
#  #> Type of the digraph:
#  #> [1] "Arc Slice Proximity Catch Digraph (AS-PCD) for 2D Points in the Triangle with Center M = (1.6,1.2)"
#  #>
#  #>  Vertices of the digraph =  Xp
#  #>  Partition points of the region =  Tr
#  #>
#  #>  Selected tail (or source) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #>          [,1]      [,2]
#  #> [1,] 1.265509 0.7442478
#  #> [2,] 1.687023 0.7682074
#  #> [3,] 1.687023 0.7682074
#  #> [4,] 1.687023 0.7682074
#  #> [5,] 1.380035 1.5548904
#  #> [6,] 1.267221 0.7722282
#  #>
#  #>  Selected head (or end) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #>          [,1]      [,2]
#  #> [1,] 1.267221 0.7722282
#  #> [2,] 1.265509 0.7442478
#  #> [3,] 1.267221 0.7722282
#  #> [4,] 1.482080 1.1991317
#  #> [5,] 1.482080 1.1991317
#  #> [6,] 1.265509 0.7442478
#  #>
#  #> Parameters of the digraph
#  #> $center
#  #> [1] 1.6 1.2
#  #>
#  #> Various quantities of the digraph
#  #>         number of vertices number of partition points
#  #>                        5.0                        3.0
#  #>        number of triangles             number of arcs
#  #>                        1.0                       10.0
#  #>                arc density
#  #>                        0.5
#  
#  plot(Arcs)

## -----------------------------------------------------------------------------
#A<-c(1,1); B<-c(2,0); C<-c(1.5,2); Tr<-rbind(A,B,C); n<-5
#set.seed(1); Xp<-runif.tri(n,Tr)$g
M<-c(1.6,1.0)  #try also M<-as.numeric(runif.tri(1,Tr)$g)
r<-1.5  #try also r<-2

## ----eval=F-------------------------------------------------------------------
#  P<-c(1.8,.5)
#  NPEtri(P,Tr,r,M)
#  #>       [,1] [,2]
#  #> [1,] 2.000 0.00
#  #> [2,] 1.550 0.45
#  #> [3,] 1.775 0.90

## ----eval=F-------------------------------------------------------------------
#  P1<-as.numeric(runif.tri(1,Tr)$g)
#  P2<-as.numeric(runif.tri(1,Tr)$g)
#  IarcPEtri(P1,P2,Tr,r,M)
#  #> [1] 1
#  IarcPEtri(Xp[1,],Xp[5,],Tr,r,M) #try also IarcPEtri(Xp[5,],Xp[1,],Tr,r,M)
#  #> [1] 0

## ----eval=F-------------------------------------------------------------------
#  Narcs = num.arcsPEtri(Xp,Tr,r,M)
#  summary(Narcs)
#  #> Call:
#  #> num.arcsPEtri(Xp = Xp, tri = Tr, r = r, M = M)
#  #>
#  #> Description of the output:
#  #> Number of Arcs of the PE-PCD with vertices Xp and Quantities Related to the Support Triangle
#  #>
#  #> Number of data (Xp) points in the triangle =  5
#  #> Number of arcs in the digraph =  7
#  #>
#  #> Indices of data points in the triangle:
#  #> 1 2 3 4 5
#  #>
#  #plot(Narcs)

## ----eval=F-------------------------------------------------------------------
#  PEarc.dens.tri(Xp,Tr,r,M)
#  #> $arc.dens
#  #> [1] 0.35

## ----PEarcs3, fig.cap="Arcs of the PE-PCD with 10 $X$ points and vertex regions (dashed lines) are based on $M$. The vertices and the center are labeled."----
plotPEarcs.tri(Xp,Tr,r,M,xlab="",ylab="",vert.reg = TRUE)
#add vertex labels and text to the figure (with vertex regions)
ifelse(isTRUE(all.equal(M,circumcenter.tri(Tr))),
       {Ds<-rbind((B+C)/2,(A+C)/2,(A+B)/2); cent.name="CC"},{Ds<-prj.cent2edges(Tr,M); cent.name="M"})

txt<-rbind(Tr,M,Ds)
xc<-txt[,1]+c(-.02,.02,.02,.02,.04,-0.03,-.01)
yc<-txt[,2]+c(.02,.02,.02,.05,.02,.04,-.06)
txt.str<-c("A","B","C",cent.name,"D1","D2","D3")
text(xc,yc,txt.str)

## ----PEPR1, fig.cap="PE proximity regions for the $X$ points used above."-----
M<-c(1.6,1.2) #try also M<-c(1.6620051,0.8136604) or M="CC"
plotPEregs.tri(Xp,Tr,r,M,vert.reg = T,xlab="",ylab="")

## ----eval=F-------------------------------------------------------------------
#  Arcs<-ArcsPEtri(Xp,Tr,r,M) #or try with the default center Arcs<-ArcsPEtri(Xp,Tr,r); M= (Arcs$param)$cent
#  Arcs
#  #> Call:
#  #> ArcsPEtri(Xp = Xp, tri = Tr, r = r, M = M)
#  #>
#  #> Type:
#  #> [1] "Proportional Edge Proximity Catch Digraph (PE-PCD) for 2D Points in the Triangle with Expansion Parameter r = 1.5 and Center M = (1.6,1.2)"
#  summary(Arcs)
#  #> Call:
#  #> ArcsPEtri(Xp = Xp, tri = Tr, r = r, M = M)
#  #>
#  #> Type of the digraph:
#  #> [1] "Proportional Edge Proximity Catch Digraph (PE-PCD) for 2D Points in the Triangle with Expansion Parameter r = 1.5 and Center M = (1.6,1.2)"
#  #>
#  #>  Vertices of the digraph =  Xp
#  #>  Partition points of the region =  Tr
#  #>
#  #>  Selected tail (or source) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #>          [,1]      [,2]
#  #> [1,] 1.265509 0.7442478
#  #> [2,] 1.380035 1.5548904
#  #> [3,] 1.380035 1.5548904
#  #> [4,] 1.380035 1.5548904
#  #> [5,] 1.380035 1.5548904
#  #> [6,] 1.267221 0.7722282
#  #>
#  #>  Selected head (or end) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #>          [,1]      [,2]
#  #> [1,] 1.267221 0.7722282
#  #> [2,] 1.265509 0.7442478
#  #> [3,] 1.687023 0.7682074
#  #> [4,] 1.267221 0.7722282
#  #> [5,] 1.482080 1.1991317
#  #> [6,] 1.265509 0.7442478
#  #>
#  #> Parameters of the digraph
#  #> $center
#  #> [1] 1.6 1.2
#  #>
#  #> $`expansion parameter`
#  #> [1] 1.5
#  #>
#  #> Various quantities of the digraph
#  #>         number of vertices number of partition points
#  #>                        5.0                        3.0
#  #>        number of triangles             number of arcs
#  #>                        1.0                       10.0
#  #>                arc density
#  #>                        0.5
#  
#  plot(Arcs)

## -----------------------------------------------------------------------------
tau<-1.5

## ----eval=F-------------------------------------------------------------------
#  P<-c(1.8,.5)
#  NCStri(P,Tr,tau,M)
#  #>         [,1]   [,2]
#  #> [1,] 1.74375 0.9500
#  #> [2,] 1.51250 0.4875
#  #> [3,] 1.97500 0.0250

## ----eval=F-------------------------------------------------------------------
#  P1<-as.numeric(runif.tri(1,Tr)$g)
#  P2<-as.numeric(runif.tri(1,Tr)$g)
#  IarcCStri(P1,P2,Tr,tau,M)
#  #> [1] 1
#  IarcCStri(Xp[1,],Xp[2,],Tr,tau,M)
#  #> [1] 0

## ----eval=F-------------------------------------------------------------------
#  Narcs = num.arcsCStri(Xp,Tr,t=.5,M)
#  summary(Narcs)
#  #> Call:
#  #> num.arcsCStri(Xp = Xp, tri = Tr, t = 0.5, M = M)
#  #>
#  #> Description of the output:
#  #> Number of Arcs of the CS-PCD with vertices Xp and Quantities Related to the Support Triangle
#  #>
#  #> Number of data (Xp) points in the triangle =  5
#  #> Number of arcs in the digraph =  0
#  #>
#  #> Indices of data points in the triangle:
#  #> 1 2 3 4 5
#  #>
#  #plot(Narcs)

## ----eval=F-------------------------------------------------------------------
#  CSarc.dens.tri(Xp,Tr,tau,M)
#  #> $arc.dens
#  #> [1] 0.35

## ----CSarcs3, fig.cap="Arcs of the CS-PCD with 10 $X$ points and edge regions (dashed lines) are based on M. The vertices and the center are labeled."----
t<-1.5  #try also t<-2
plotCSarcs.tri(Xp,Tr,t,M,xlab="",ylab="",edge.reg = TRUE)
txt<-rbind(Tr,M)
xc<-txt[,1]+c(-.02,.02,.02,.03)
yc<-txt[,2]+c(.02,.02,.02,.03)
txt.str<-c("A","B","C","M")
text(xc,yc,txt.str)

## ----CSPR1, fig.cap="CS proximity regions for the $X$ points used above."-----
plotCSregs.tri(Xp,Tr,t,M,edge.reg=T,xlab="",ylab="")

## ----eval=F-------------------------------------------------------------------
#  Arcs<-arcsCStri(Xp,Tr,t,M)
#  Arcs
#  #> Call:
#  #> arcsCStri(Xp = Xp, tri = Tr, t = t, M = M)
#  #>
#  #> Type:
#  #> [1] "Central Similarity Proximity Catch Digraph (CS-PCD) for 2D Points in the Triangle with Expansion Parameter t = 1.5 and Center M = (1.6,1.2)"
#  summary(Arcs)
#  #> Call:
#  #> arcsCStri(Xp = Xp, tri = Tr, t = t, M = M)
#  #>
#  #> Type of the digraph:
#  #> [1] "Central Similarity Proximity Catch Digraph (CS-PCD) for 2D Points in the Triangle with Expansion Parameter t = 1.5 and Center M = (1.6,1.2)"
#  #>
#  #>  Vertices of the digraph =  Xp
#  #>  Partition points of the region =  Tr
#  #>
#  #>  Selected tail (or source) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #>          [,1]      [,2]
#  #> [1,] 1.687023 0.7682074
#  #> [2,] 1.687023 0.7682074
#  #> [3,] 1.687023 0.7682074
#  #> [4,] 1.267221 0.7722282
#  #> [5,] 1.482080 1.1991317
#  #> [6,] 1.482080 1.1991317
#  #>
#  #>  Selected head (or end) points of the arcs in the digraph
#  #>       (first 6 or fewer are printed)
#  #>          [,1]      [,2]
#  #> [1,] 1.265509 0.7442478
#  #> [2,] 1.267221 0.7722282
#  #> [3,] 1.482080 1.1991317
#  #> [4,] 1.265509 0.7442478
#  #> [5,] 1.265509 0.7442478
#  #> [6,] 1.687023 0.7682074
#  #>
#  #> Parameters of the digraph
#  #> $center
#  #> [1] 1.6 1.2
#  #>
#  #> $`expansion parameter`
#  #> [1] 1.5
#  #>
#  #> Various quantities of the digraph
#  #>         number of vertices number of partition points
#  #>                        5.0                        3.0
#  #>        number of triangles             number of arcs
#  #>                        1.0                        8.0
#  #>                arc density
#  #>                        0.4
#  plot(Arcs)

## ----eval=F-------------------------------------------------------------------
#  A<-c(0,0); B<-c(1,0); C<-c(1/2,sqrt(3)/2);
#  Te<-rbind(A,B,C);
#  n<-5  #try also n<-10, 50, or 100
#  set.seed(1)
#  Xp<-runif.std.tri(n)$gen.points
#  M<-c(.6,.2)  #try also M<-c(1,1,1)

## ----Te, eval=F, fig.cap="Scatterplot of points in the standard equilateral triangle."----
#  Xlim<-range(Te[,1])
#  Ylim<-range(Te[,2])
#  plot(Te,asp=1,pch=".",xlab="",ylab="",xlim=Xlim,ylim=Ylim,main="Points in Standard Equilateral Triangle")
#  polygon(Te)
#  points(Xp)
#  
#  #add the vertex names and annotation
#  txt<-rbind(Te)
#  xc<-txt[,1]+c(-.02,.02,.02)
#  yc<-txt[,2]+c(.01,.01,.01)
#  txt.str<-c("A","B","C")
#  text(xc,yc,txt.str)

## ----eval=F-------------------------------------------------------------------
#  P1<-as.numeric(runif.tri(1,Te)$g)
#  P2<-as.numeric(runif.tri(1,Te)$g)
#  r=2
#  IarcPEstd.tri(P1,P2,r,M)
#  #> [1] 1
#  IarcPEstd.tri(Xp[1,],Xp[2,],r,M)
#  #> [1] 1

## ----eval=F-------------------------------------------------------------------
#  Narcs = num.arcsPEstd.tri(Xp,r=1.25,M)
#  summary(Narcs)
#  #> Call:
#  #> num.arcsPEstd.tri(Xp = Xp, r = 1.25, M = M)
#  #>
#  #> Description of the output:
#  #> Number of Arcs of the PE-PCD and the Related Quantities with vertices Xp in the Standard Equilateral Triangle
#  #>
#  #> Number of data (Xp) points in the triangle =  5
#  #> Number of arcs in the digraph =  5
#  #>
#  #> Indices of data points in the triangle:
#  #> 1 2 3 4 5
#  #>
#  #plot(Narcs)

## ----eval=F-------------------------------------------------------------------
#  P1<-as.numeric(runif.tri(1,Te)$g)
#  P2<-as.numeric(runif.tri(1,Te)$g)
#  tau=1
#  IarcCSstd.tri(P1,P1,tau,M)
#  IarcCSstd.tri(P1,P2,tau,M)
#  IarcCSstd.tri(Xp[1,],Xp[2,],tau,M)

## ----eval=F-------------------------------------------------------------------
#  set.seed(123)
#  M<-as.numeric(runif.std.tri(1)$g)  #try also M<-c(.6,.2)
#  Narcs = num.arcsCStri(Xp,Te,t=1.5,M)
#  summary(Narcs)
#  #> Call:
#  #> num.arcsCStri(Xp = Xp, tri = Te, t = 1.5, M = M)
#  #>
#  #> Description of the output:
#  #> Number of Arcs of the CS-PCD with vertices Xp and Quantities Related to the Support Triangle
#  #>
#  #> Number of data (Xp) points in the triangle =  5
#  #> Number of arcs in the digraph =  3
#  #>
#  #> Indices of data points in the triangle:
#  #> 1 2 3 4 5
#  #>
#  #plot(Narcs)

## ----eval=F-------------------------------------------------------------------
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2); p<-c(1.4,1.2)
#  Tr<-rbind(A,B,C)
#  in.triangle(p,Tr)
#  #> $in.tri
#  #> [1] TRUE
#  #>
#  #> $barycentric
#  #> [1] 0.4 0.2 0.4

## ----eval=F-------------------------------------------------------------------
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  Tr<-rbind(A,B,C);  #the vertices of the triangle Tr
#  (CC<-circumcenter.tri(Tr)) #the circumcenter
#  #> [1] 2.083333 1.083333

## ----CC, eval=F, fig.cap="Circumcenter of an obtuse triangle."----------------
#  D1<-(B+C)/2; D2<-(A+C)/2; D3<-(A+B)/2; #midpoints of the edges
#  Ds<-rbind(D1,D2,D3)
#  
#  Xlim<-range(Tr[,1],CC[1])
#  Ylim<-range(Tr[,2],CC[2])
#  xd<-Xlim[2]-Xlim[1]
#  yd<-Ylim[2]-Ylim[1]
#  
#  oldpar <- par(pty="s")
#  plot(A,asp=1,pch=".",xlab="",ylab="",
#       main="Circumcenter of a Triangle",
#       axes=TRUE,xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05))
#  polygon(Tr)
#  points(rbind(CC))
#  L<-matrix(rep(CC,3),ncol=2,byrow=TRUE); R<-Ds
#  segments(L[,1], L[,2], R[,1], R[,2], lty=2)
#  
#  txt<-rbind(Tr,CC,Ds)
#  xc<-txt[,1]+c(-.08,.08,.08,.12,-.09,-.1,-.09)
#  yc<-txt[,2]+c(.02,-.02,.03,-.06,.02,.06,-.04)
#  txt.str<-c("A","B","C","CC","D1","D2","D3")
#  text(xc,yc,txt.str)
#  par(oldpar)

## ----eval=F-------------------------------------------------------------------
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  Tr<-rbind(A,B,C);
#  r<-1.35
#  (Ms<-center.nondegPE(Tr,r))
#  #>        [,1]      [,2]
#  #> M1 1.388889 1.0000000
#  #> M2 1.611111 0.7777778
#  #> M3 1.500000 1.2222222

## ----nd-C, eval=F, fig.cap="Nondegeneracy centers for the PE-PCDs with uniform vertices in the triangle $T$."----
#  Xlim<-range(Tr[,1])
#  Ylim<-range(Tr[,2])
#  xd<-Xlim[2]-Xlim[1]
#  yd<-Ylim[2]-Ylim[1]
#  
#  plot(Tr,pch=".",xlab="",ylab="",
#       main="Centers of nondegeneracy of the domination number\n of the PE-PCD in a triangle",
#       axes=TRUE,xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05))
#  polygon(Tr)
#  points(Ms,pch=".",col=1)
#  polygon(Ms,lty=2)
#  
#  xc<-Tr[,1]+c(-.02,.02,.02)
#  yc<-Tr[,2]+c(.02,.02,.03)
#  txt.str<-c("A","B","C")
#  text(xc,yc,txt.str)
#  
#  xc<-Ms[,1]+c(-.04,.04,.03)
#  yc<-Ms[,2]+c(.02,.02,.05)
#  txt.str<-c(expression(M[1]),"M2","M3")
#  text(xc,yc,txt.str)

## ----eval=F-------------------------------------------------------------------
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  Tr<-rbind(A,B,C);
#  M<-c(1.6,1.0)  #try also M<-as.numeric(runif.tri(1,Tr)$g)
#  (Ds<-prj.cent2edges(Tr,M))  #try also prj.cent2edges(Tr,M=c(1,1))
#  #>          [,1]      [,2]
#  #> [1,] 1.750000 1.0000000
#  #> [2,] 1.333333 1.6666667
#  #> [3,] 1.666667 0.3333333

## ----C2e, eval=F, fig.cap="Projection of a center M to the edges in the triangle $T$."----
#  M<-c(1.6,1.0)
#  Xlim<-range(Tr[,1])
#  Ylim<-range(Tr[,2])
#  xd<-Xlim[2]-Xlim[1]
#  yd<-Ylim[2]-Ylim[1]
#  
#  if (dimension(M)==3) {M<-bary2cart(M,Tr)} #need to run this when M is given in barycentric coordinates
#  
#  plot(Tr,pch=".",xlab="",ylab="",
#       main="Projection of Center M to the edges of a triangle",axes=TRUE,
#       xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05))
#  polygon(Tr)
#  L<-rbind(M,M,M); R<-Ds
#  segments(L[,1], L[,2], R[,1], R[,2], lty=2)
#  
#  xc<-Tr[,1]
#  yc<-Tr[,2]
#  txt.str<-c("rv=1","rv=2","rv=3")
#  text(xc,yc,txt.str)
#  
#  txt<-rbind(M,Ds)
#  xc<-txt[,1]+c(-.02,.04,-.04,-.02)
#  yc<-txt[,2]+c(-.02,.04,.04,-.06)
#  txt.str<-c("M","D1","D2","D3")
#  text(xc,yc,txt.str)

## ----eval=F-------------------------------------------------------------------
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  Tr<-rbind(A,B,C);
#  r<-1.35
#  prj.nondegPEcent2edges(Tr,r,cent=2)
#  #>       [,1] [,2]
#  #> [1,] 1.825 0.70
#  #> [2,] 1.250 1.50
#  #> [3,] 1.650 0.35

## ----ndC2e, eval=F, fig.cap="Projection of a nondegeneracy center to the edges in the triangle $T$."----
#  Ms<-center.nondegPE(Tr,r)
#  M1=Ms[1,]
#  
#  Ds<-prj.nondegPEcent2edges(Tr,r,cent=1)
#  
#  Xlim<-range(Tr[,1])
#  Ylim<-range(Tr[,2])
#  xd<-Xlim[2]-Xlim[1]
#  yd<-Ylim[2]-Ylim[1]
#  
#  plot(Tr,pch=".",xlab="",ylab="",
#       main="Projections from a non-degeneracy center for domination number\n of PE-PCD to the edges of the triangle",
#       axes=TRUE,xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05))
#  polygon(Tr)
#  points(Ms,pch=".",col=1)
#  polygon(Ms,lty=2)
#  
#  xc<-Tr[,1]+c(-.02,.03,.02)
#  yc<-Tr[,2]+c(-.02,.04,.04)
#  txt.str<-c("A","B","C")
#  text(xc,yc,txt.str)
#  
#  txt<-Ms
#  xc<-txt[,1]+c(-.02,.04,-.04)
#  yc<-txt[,2]+c(-.02,.04,.04)
#  txt.str<-c("M1","M2","M3")
#  text(xc,yc,txt.str)
#  
#  points(Ds,pch=4,col=2)
#  L<-rbind(M1,M1,M1); R<-Ds
#  segments(L[,1], L[,2], R[,1], R[,2], lty=2,lwd=2,col=4)
#  txt<-Ds
#  xc<-txt[,1]+c(-.02,.04,-.04)
#  yc<-txt[,2]+c(-.02,.04,.04)
#  txt.str<-c("D1","D2","D3")
#  text(xc,yc,txt.str)

## ----eval=F-------------------------------------------------------------------
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  p<-c(1.4,1.2)
#  Tr<-rbind(A,B,C)
#  in.triangle(p,Tr)
#  #> $in.tri
#  #> [1] TRUE
#  #>
#  #> $barycentric
#  #> [1] 0.4 0.2 0.4
#  
#  #data set and checking all points in it are inside the triangle or not
#  n<-5
#  Xp1<-cbind(runif(n),runif(n))
#  in.tri.all(Xp1,Tr)
#  #> [1] FALSE

## ----eval=F-------------------------------------------------------------------
#  A<-c(0,0); B<-c(1,0); C<-c(1/2,sqrt(3)/2);
#  Te<-rbind(A,B,C)  #try adding +10^(-16) to each vertex
#  is.std.eq.tri(Te)
#  #> [1] TRUE
#  
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  Tr<-rbind(A,B,C);
#  is.std.eq.tri(Tr)
#  #> [1] FALSE

## ----eval=F-------------------------------------------------------------------
#  c1<-.4; c2<-.6
#  A<-c(0,0); B<-c(1,0); C<-c(c1,c2);
#  as.basic.tri(rbind(B,C,A))
#  #> $tri
#  #>   [,1] [,2]
#  #> A  0.0  0.0
#  #> B  1.0  0.0
#  #> C  0.4  0.6
#  #>
#  #> $desc
#  #> [1] "Edges (in decreasing length are) AB, BC, and AC"
#  #>
#  #> $orig.order
#  #> [1] 3 1 2
#  
#  x<-c(1,1); y<-c(2,0); z<-c(1.5,2);
#  as.basic.tri(rbind(x,y,z))
#  #> $tri
#  #>   [,1] [,2]
#  #> A  1.5    2
#  #> B  2.0    0
#  #> C  1.0    1
#  #>
#  #> $desc
#  #> [1] "Edges (in decreasing length are) AB, BC, and AC"
#  #>
#  #> $orig.order
#  #> [1] 3 2 1
#  as.basic.tri(rbind(x,y,z),scaled = TRUE)
#  #> $tri
#  #>        [,1]      [,2]
#  #> A 0.7276069 0.9701425
#  #> B 0.9701425 0.0000000
#  #> C 0.4850713 0.4850713
#  #>
#  #> $desc
#  #> [1] "Edges (in decreasing length are) AB, BC, and AC"
#  #>
#  #> $orig.order
#  #> [1] 3 2 1

## ----eval=F-------------------------------------------------------------------
#  c1<-.4; c2<-.6
#  A<-c(0,0); B<-c(1,0); C<-c(c1,c2);
#  tri2std.basic.tri(rbind(B,C,A))
#  #> $Cvec
#  #> [1] 0.4 0.6
#  #>
#  #> $orig.order
#  #> [1] 3 1 2
#  
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  tri2std.basic.tri(rbind(A,B,C))
#  #> $Cvec
#  #> [1] 0.4117647 0.3529412
#  #>
#  #> $orig.order
#  #> [1] 3 2 1

## ----eval=F-------------------------------------------------------------------
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  Tr<-rbind(A,B,C);
#  cart2bary(c(1.4,1.2),Tr)
#  #> [1] 0.4 0.2 0.4
#  bary2cart(c(1.4,1.2,1),Tr)
#  #> [1] 1.4722222 0.9444444
#  
#  CM<-(A+B+C)/3; CM
#  #> [1] 1.5 1.0
#  cart2bary(CM,Tr)
#  #> [1] 0.3333333 0.3333333 0.3333333
#  bary2cart(c(1,1,1),Tr)
#  #> [1] 1.5 1.0

## -----------------------------------------------------------------------------
nx<-10 #number of X points (target)
ny<-5 #number of Y points (nontarget)

## ----eval=F-------------------------------------------------------------------
#  set.seed(1)
#  Yp<-cbind(runif(ny),runif(ny))
#  
#  Xp<-runif.multi.tri(nx,Yp)$g #data under CSR in the convex hull of Ypoints
#  #try also Xp<-cbind(runif(nx),runif(nx))
#  
#  index.delaunay.tri(Xp[10,],Yp)
#  #> [1] 2
#  
#  #or use
#  DTY<-interp::tri.mesh(Yp[,1],Yp[,2],duplicate="remove")  #Delaunay triangulation
#  index.delaunay.tri(Xp[10,],Yp,DTY)
#  #> [1] 2
#  
#  (tr.ind<-indices.delaunay.tri(Xp,Yp,DTY))  #indices of the Delaunay triangles
#  #>  [1] 3 3 1 4 3 2 3 3 2 2

## ----ptsDT, eval=F, fig.cap="Scatterplot of Uniform $X$ Points in Convex Hull of $Y$ points. Points are marked with the indices of the Delaunay triangle it resides in."----
#  Xlim<-range(Yp[,1],Xp[,1])
#  Ylim<-range(Yp[,2],Xp[,2])
#  xd<-Xlim[2]-Xlim[1]
#  yd<-Ylim[2]-Ylim[1]
#  
#  # plot of the data in the convex hull of Y points together with the Delaunay triangulation
#  plot(Xp,main="X Points in Delaunay Triangles for Y Points", xlab=" ", ylab=" ",
#       xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05),pch=".")
#  interp::plot.triSht(DTY, add=TRUE, do.points = TRUE,pch=16,col="blue")
#  text(Xp,labels = factor(tr.ind) )

## ----eval=F-------------------------------------------------------------------
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  Tr<-rbind(A,B,C);
#  M<-c(1.6,1.0)
#  
#  P<-c(1.8,.5)
#  rel.vert.tri(P,Tr,M)
#  #> $rv
#  #> [1] 2
#  #>
#  #> $tri
#  #>          [,1] [,2]
#  #> vertex 1  1.0    1
#  #> vertex 2  2.0    0
#  #> vertex 3  1.5    2

## ----include=F----------------------------------------------------------------
n<-5  #try also n<-10, 50, or 100

## ----VRs, eval=F, fig.cap="M-Vertex regions in the triangle $T$. Also plotted are the $X$ points which are labeled according to the vertex region they reside in."----
#  set.seed(1)
#  Xp<-runif.tri(n,Tr)$g
#  M<-c(1.6,1.0)  #try also M<-as.numeric(runif.tri(1,Tr)$g)
#  
#  Rv<-vector()
#  for (i in 1:n)
#  {Rv<-c(Rv,rel.vert.tri(Xp[i,],Tr,M)$rv)}
#  Rv
#  
#  Ds<-prj.cent2edges(Tr,M)
#  
#  Xlim<-range(Tr[,1],Xp[,1])
#  Ylim<-range(Tr[,2],Xp[,2])
#  xd<-Xlim[2]-Xlim[1]
#  yd<-Ylim[2]-Ylim[1]
#  
#  if (dimension(M)==3) {M<-bary2cart(M,Tr)} #need to run this when M is given in barycentric coordinates
#  
#  plot(Tr,pch=".",xlab="",ylab="",main="Illustration of M-Vertex Regions\n in a Triangle",axes=TRUE,
#       xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05))
#  polygon(Tr)
#  points(Xp,pch=".",col=1)
#  L<-rbind(M,M,M); R<-Ds
#  segments(L[,1], L[,2], R[,1], R[,2], lty=2)
#  
#  xc<-Tr[,1]
#  yc<-Tr[,2]
#  txt.str<-c("rv=1","rv=2","rv=3")
#  text(xc,yc,txt.str)
#  
#  txt<-rbind(M,Ds)
#  xc<-txt[,1]+c(-.02,.04,-.04,0)
#  yc<-txt[,2]+c(-.02,.04,.05,-.08)
#  txt.str<-c("M","D1","D2","D3")
#  text(xc,yc,txt.str)
#  
#  text(Xp,labels=factor(Rv))

## ----eval=F-------------------------------------------------------------------
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  Tr<-rbind(A,B,C);
#  
#  n<-5  #try also n<-10, 50, or 100
#  set.seed(1)
#  Xp<-runif.tri(n,Tr)$g
#  M<-c(1.6,1.0)  #try also M<-as.numeric(runif.tri(1,Tr)$g)
#  
#  (rv<-rel.verts.tri(Xp,Tr,M))
#  #> $rv
#  #> [1] 1 2 3 1 1
#  #>
#  #> $tri
#  #>          [,1] [,2]
#  #> vertex 1  1.0    1
#  #> vertex 2  2.0    0
#  #> vertex 3  1.5    2

## ----eval=F-------------------------------------------------------------------
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  Tr<-rbind(A,B,C);
#  
#  P<-c(1.3,1.2)
#  rel.vert.triCC(P,Tr)
#  #> $rv
#  #> [1] 1
#  #>
#  #> $tri
#  #>          [,1] [,2]
#  #> vertex 1  1.0    1
#  #> vertex 2  2.0    0
#  #> vertex 3  1.5    2

## ----eval=F-------------------------------------------------------------------
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  Tr<-rbind(A,B,C);
#  n<-5  #try also n<-10, 50, or 100
#  set.seed(1)
#  Xp<-runif.tri(n,Tr)$g
#  
#  (rv<-rel.verts.triCC(Xp,Tr))
#  #> $rv
#  #> [1] 1 1 3 1 1
#  #>
#  #> $tri
#  #>          [,1] [,2]
#  #> vertex 1  1.0    1
#  #> vertex 2  2.0    0
#  #> vertex 3  1.5    2

## ----CCVR2, eval=F,fig.cap="CC-Vertex regions in an obtuse triangle. Also plotted are 10 X points which are labeled according to the vertex region they reside in."----
#  CC<-circumcenter.tri(Tr)
#  D1<-(B+C)/2; D2<-(A+C)/2; D3<-(A+B)/2;
#  Ds<-rbind(D1,D2,D3)
#  
#  Xlim<-range(Tr[,1],Xp[,1],CC[1])
#  Ylim<-range(Tr[,2],Xp[,2],CC[2])
#  xd<-Xlim[2]-Xlim[1]
#  yd<-Ylim[2]-Ylim[1]
#  
#  plot(Tr,pch=".",asp=1,xlab="",ylab="",
#       main="Scatterplot of data points with the CC-vertex regions",
#       axes=TRUE,xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05))
#  polygon(Tr)
#  points(Xp,pch=".",col=1)
#  L<-matrix(rep(CC,3),ncol=2,byrow=TRUE); R<-Ds
#  segments(L[,1], L[,2], R[,1], R[,2], lty=2)
#  
#  xc<-Tr[,1]
#  yc<-Tr[,2]
#  txt.str<-c("rv=1","rv=2","rv=3")
#  text(xc,yc,txt.str)
#  
#  txt<-rbind(CC,Ds)
#  xc<-txt[,1]+c(.04,.04,-.03,0)
#  yc<-txt[,2]+c(-.07,.04,.06,-.08)
#  txt.str<-c("CC","D1","D2","D3")
#  text(xc,yc,txt.str)
#  
#  text(Xp,labels=factor(rv$rv))

## ----eval=F-------------------------------------------------------------------
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  Tr<-rbind(A,B,C);
#  
#  P<-c(1.4,1.2)
#  M<-c(1.6,1.2) #try also set.seed(1234); M<-as.numeric(runif.tri(1,Tr)$g)
#  rel.edge.tri(P,Tr,M)
#  #> $re
#  #> [1] 2
#  #>
#  #> $tri
#  #>   [,1] [,2]
#  #> A  1.0    1
#  #> B  2.0    0
#  #> C  1.5    2
#  #>
#  #> $desc
#  #> [1] "Edge labels are AB=3, BC=1, and AC=2"

## ----eval=F-------------------------------------------------------------------
#  A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
#  Tr<-rbind(A,B,C);
#  n<-5  #try also n<-10, 50, or 100
#  set.seed(1)
#  Xp<-runif.tri(n,Tr)$g
#  
#  M<-c(1.6,1.2)  #try also M<-as.numeric(runif.tri(1,Tr)$g)
#  
#  (re<-rel.edges.tri(Xp,Tr,M))
#  #> $re
#  #> [1] 3 3 2 3 2
#  #>
#  #> $tri
#  #>   [,1] [,2]
#  #> A  1.0    1
#  #> B  2.0    0
#  #> C  1.5    2
#  #>
#  #> $desc
#  #> [1] "Edge labels are AB=3, BC=1, and AC=2"

## ----ER2, eval=F, fig.cap="$M$-Edge regions in the triangle $T$. Also plotted are 10 X points which are labeled according to the edge region they reside in."----
#  D1<-(B+C)/2; D2<-(A+C)/2; D3<-(A+B)/2;
#  Ds<-rbind(D1,D2,D3)
#  
#  Xlim<-range(Tr[,1],Xp[,1])
#  Ylim<-range(Tr[,2],Xp[,2])
#  xd<-Xlim[2]-Xlim[1]
#  yd<-Ylim[2]-Ylim[1]
#  
#  if (dimension(M)==3) {M<-bary2cart(M,Tr)} #need to run this when M is given in barycentric coordinates
#  
#  plot(Tr,pch=".",xlab="",ylab="",
#       main="Scatterplot of data points with the M-edge regions",axes=TRUE,
#       xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05))
#  polygon(Tr)
#  points(Xp,pch=".",col=1)
#  L<-Tr; R<-rbind(M,M,M)
#  segments(L[,1], L[,2], R[,1], R[,2], lty=2)
#  
#  xc<-Tr[,1]+c(-.02,.03,.02)
#  yc<-Tr[,2]+c(.02,.02,.04)
#  txt.str<-c("A","B","C")
#  text(xc,yc,txt.str)
#  
#  txt<-rbind(M,Ds)
#  xc<-txt[,1]+c(.05,.06,-.05,-.02)
#  yc<-txt[,2]+c(.03,.03,.05,-.08)
#  txt.str<-c("M","re=1","re=2","re=3")
#  text(xc,yc,txt.str)
#  text(Xp,labels=factor(re$re))

