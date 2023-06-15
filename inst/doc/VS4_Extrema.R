## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>",fig.width=6, fig.height=4, fig.align = "center") 

## ----setup, message=FALSE, results='hide'-------------------------------------
library(pcds)

## -----------------------------------------------------------------------------
A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
Tr<-rbind(A,B,C);

n<-10  #try also n<-20 or 100

## ----eval=F-------------------------------------------------------------------
#  set.seed(1)
#  Xp<-runif.tri(n,Tr)$g

## ----cl2eCCvr, eval=F, fig.cap="Scatterplot of the uniform $X$ points in the triangle $T=T(A,B,C)$ with vertices $A=(1,1)$, $B=(2,0)$, and $C=(1.5,2)$. The closest points in CC-vertex regions to opposite edges are marked with red crosses."----
#  Ext<-cl2edgesCCvert.reg(Xp,Tr)
#  Ext
#  #> Call:
#  #> cl2edgesCCvert.reg(Xp = Xp, tri = Tr)
#  #>
#  #> Type:
#  #> [1] "Closest Points in CC-Vertex Regions of the Triangle with Vertices A=(1,1), B=(2,0), and C=(1.5,2) \n to the Opoosite Edges"
#  summary(Ext)
#  #> Call:
#  #> cl2edgesCCvert.reg(Xp = Xp, tri = Tr)
#  #>
#  #> Type of the Extrema
#  #> [1] "Closest Points in CC-Vertex Regions of the Triangle with Vertices A=(1,1), B=(2,0), and C=(1.5,2) \n to the Opoosite Edges"
#  #>
#  #>  Extremum Points: Closest Points in CC-Vertex Regions of the Triangle to its Edges
#  #>  (Row i corresponds to vertex region i for i=1,2,3)
#  #>          [,1]      [,2]
#  #> [1,] 1.723711 0.8225489
#  #> [2,] 1.794240 0.2158873
#  #> [3,] 1.380035 1.5548904
#  #> [1] "Vertex labels are A=1, B=2, and C=3 (correspond to row number in Extremum Points)"
#  #>
#  #>  Distances between the Edges and the Closest Points to the Edges in CC-Vertex Regions
#  #>  (i-th entry corresponds to vertex region i for i=1,2,3)
#  #> [1] 0.06854235 1.06105561 0.66109225
#  #>
#  #>  Vertices of the Support Triangle
#  #>   [,1] [,2]
#  #> A  1.0    1
#  #> B  2.0    0
#  #> C  1.5    2
#  plot(Ext)

## ----cl2eCMvr, eval=F, fig.cap="Scatterplot of the uniform $X$ points in the triangle $T=T(A,B,C)$. The closest points in CM-vertex regions to opposite edges are marked with red crosses."----
#  Ext<-cl2edgesCMvert.reg(Xp,Tr)
#  Ext
#  #> Call:
#  #> cl2edgesCMvert.reg(Xp = Xp, tri = Tr)
#  #>
#  #> Type:
#  #> [1] "Closest Points in CM-Vertex Regions of the Triangle with Vertices A=(1,1), B=(2,0), and C=(1.5,2) \n to the Opposite Edges"
#  summary(Ext)
#  #> Call:
#  #> cl2edgesCMvert.reg(Xp = Xp, tri = Tr)
#  #>
#  #> Type of the Extrema
#  #> [1] "Closest Points in CM-Vertex Regions of the Triangle with Vertices A=(1,1), B=(2,0), and C=(1.5,2) \n to the Opposite Edges"
#  #>
#  #>  Extremum Points: Closest Points in CM-Vertex Regions of the Triangle to its Edges
#  #>  (Row i corresponds to vertex region i for i=1,2,3)
#  #>          [,1]      [,2]
#  #> [1,] 1.316272 1.0372685
#  #> [2,] 1.687023 0.7682074
#  #> [3,] 1.482080 1.1991317
#  #> [1] "Vertex labels are A=1, B=2, and C=3 (correspond to row number in Extremum Points)"
#  #>
#  #>  Distances between the Edges and the Closest Points to the Edges in CM-Vertex Regions
#  #>  (i-th entry corresponds to vertex region i for i=1,2,3)
#  #> [1] 0.4117393 0.7181527 0.4816895
#  #>
#  #>  Vertices of the Support Triangle
#  #>   [,1] [,2]
#  #> A  1.0    1
#  #> B  2.0    0
#  #> C  1.5    2
#  plot(Ext)

## ----cl2eMvr, eval=F, fig.cap="Scatterplot of the uniform $X$ points in the triangle $T=T(A,B,C)$. The closest points in M-vertex regions to opposite edges are marked with red crosses."----
#  M<-c(1.6,1.0) #try also M<-as.numeric(runif.tri(1,Tr)$g)
#  Ext<-cl2edgesMvert.reg(Xp,Tr,M)
#  Ext
#  #> Call:
#  #> cl2edgesMvert.reg(Xp = Xp, tri = Tr, M = M)
#  #>
#  #> Type:
#  #> [1] "Closest Points in M-Vertex Regions of the Triangle with Vertices A=(1,1), B=(2,0), and C=(1.5,2)\n to Opposite Edges"
#  summary(Ext)
#  #> Call:
#  #> cl2edgesMvert.reg(Xp = Xp, tri = Tr, M = M)
#  #>
#  #> Type of the Extrema
#  #> [1] "Closest Points in M-Vertex Regions of the Triangle with Vertices A=(1,1), B=(2,0), and C=(1.5,2)\n to Opposite Edges"
#  #>
#  #>  Extremum Points: Closest Points in M-Vertex Regions of the Triangle to its Edges
#  #>  (Row i corresponds to vertex region i for i=1,2,3)
#  #>          [,1]      [,2]
#  #> [1,] 1.482080 1.1991317
#  #> [2,] 1.687023 0.7682074
#  #> [3,] 1.380035 1.5548904
#  #> [1] "Vertex labels are A=1, B=2, and C=3 (correspond to row number in Extremum Points)"
#  #>
#  #>  Distances between the Edges and the Closest Points to the Edges in M-Vertex Regions
#  #>  (i-th entry corresponds to vertex region i for i=1,2,3)
#  #> [1] 0.2116239 0.7181527 0.6610922
#  #>
#  #>  Vertices of the Support Triangle
#  #>   [,1] [,2]
#  #> A  1.0    1
#  #> B  2.0    0
#  #> C  1.5    2
#  plot(Ext)

## ----cl2CC_CCvr, eval=F, fig.cap="Scatterplot of the uniform $X$ points in the triangle $T=T(A,B,C)$ . The closest points in CC-vertex regions to the circumcenter are marked with red crosses."----
#  Ext<-cl2CCvert.reg(Xp,Tr)
#  Ext
#  #> Call:
#  #> cl2CCvert.reg(Xp = Xp, tri = Tr)
#  #>
#  #> Type:
#  #> [1] "Closest Points in CC-Vertex Regions of the Triangle with Vertices A=(1,1), B=(2,0), and C=(1.5,2) to its Circumcenter"
#  summary(Ext)
#  #> Call:
#  #> cl2CCvert.reg(Xp = Xp, tri = Tr)
#  #>
#  #> Type of the Extrema
#  #> [1] "Closest Points in CC-Vertex Regions of the Triangle with Vertices A=(1,1), B=(2,0), and C=(1.5,2) to its Circumcenter"
#  #>
#  #>  Extremum Points: Closest Points in CC-Vertex Regions of the Triangle to its Circumcenter
#  #>  (Row i corresponds to vertex i for i=1,2,3)
#  #>          [,1]      [,2]
#  #> [1,] 1.723711 0.8225489
#  #> [2,] 1.794240 0.2158873
#  #> [3,] 1.529720 1.5787125
#  #> [1] "Vertex labels are A=1, B=2, and C=3 (correspond to row number in Extremum Points)"
#  #>
#  #>  Distances between the Circumcenter and the Closest Points to the Circumcenter in CC-Vertex Regions
#  #>  (i-th entry corresponds to vertex i for i=1,2,3)
#  #> [1] 0.4442261 0.9143510 0.7428921
#  #>
#  #>  Vertices of the Support Triangle
#  #>   [,1] [,2]
#  #> A  1.0    1
#  #> B  2.0    0
#  #> C  1.5    2
#  plot(Ext)

## ----f2CCvr, eval=F, fig.cap="Scatterplot of the uniform $X$ points in the triangle $T=T(A,B,C)$. The furthest points in CC-vertex regions to the vertices are marked with red crosses."----
#  Ext<-fr2vertsCCvert.reg(Xp,Tr)
#  Ext
#  #> Call:
#  #> fr2vertsCCvert.reg(Xp = Xp, tri = Tr)
#  #>
#  #> Type:
#  #> [1] "Furthest Points in CC-Vertex Regions of the Triangle with Vertices A=(1,1), B=(2,0), and C=(1.5,2) from its Vertices"
#  summary(Ext)
#  #> Call:
#  #> fr2vertsCCvert.reg(Xp = Xp, tri = Tr)
#  #>
#  #> Type of the Extrema
#  #> [1] "Furthest Points in CC-Vertex Regions of the Triangle with Vertices A=(1,1), B=(2,0), and C=(1.5,2) from its Vertices"
#  #>
#  #>  Extremum Points: Furthest Points in CC-Vertex Regions of the Triangle from its Vertices
#  #>  (Row i corresponds to vertex i for i=1,2,3)
#  #>          [,1]      [,2]
#  #> [1,] 1.723711 0.8225489
#  #> [2,] 1.794240 0.2158873
#  #> [3,] 1.380035 1.5548904
#  #> [1] "Vertex labels are A=1, B=2, and C=3 (correspond to row number in Extremum Points)"
#  #>
#  #>  Distances between the vertices and the furthest points in the vertex regions
#  #>  (i-th entry corresponds to vertex i for i=1,2,3)
#  #> [1] 0.7451486 0.2982357 0.4609925
#  #>
#  #>  Vertices of the Support Triangle
#  #>   [,1] [,2]
#  #> A  1.0    1
#  #> B  2.0    0
#  #> C  1.5    2
#  plot(Ext)

## ----kf2CCvr, eval=F, fig.cap="Scatterplot of the uniform $X$ points in the triangle $T=T(A,B,C)$ . The $k=3$ most furthest points in CC-vertex regions to the vertices are marked with red crosses."----
#  k=3
#  Ext<-kfr2vertsCCvert.reg(Xp,Tr,k)
#  Ext
#  #> Call:
#  #> kfr2vertsCCvert.reg(Xp = Xp, tri = Tr, k = k)
#  #>
#  #> Type:
#  #> [1] "3 Furthest Points in CC-Vertex Regions of the Triangle with Vertices A=(1,1), B=(2,0), and C=(1.5,2) from its Vertices"
#  summary(Ext)
#  #> Call:
#  #> kfr2vertsCCvert.reg(Xp = Xp, tri = Tr, k = k)
#  #>
#  #> Type of the Extrema
#  #> [1] "3 Furthest Points in CC-Vertex Regions of the Triangle with Vertices A=(1,1), B=(2,0), and C=(1.5,2) from its Vertices"
#  #>
#  #>  Extremum Points: 3 Furthest Points in CC-Vertex Regions of the Triangle from its Vertices
#  #>  (Row i corresponds to vertex i for i=1,2,3)
#  #>                               [,1]      [,2]
#  #> 1. furthest from vertex 1 1.723711 0.8225489
#  #> 2. furthest from vertex 1 1.687023 0.7682074
#  #> 3. furthest from vertex 1 1.482080 1.1991317
#  #> 1. furthest from vertex 2 1.794240 0.2158873
#  #> 2. furthest from vertex 2       NA        NA
#  #> 3. furthest from vertex 2       NA        NA
#  #> 1. furthest from vertex 3 1.380035 1.5548904
#  #> 2. furthest from vertex 3 1.529720 1.5787125
#  #> 3. furthest from vertex 3 1.477620 1.7224190
#  #> [1] "Vertex labels are A=1, B=2, and C=3 (where vertex i corresponds to row numbers 3(i-1) to 3i in Extremum Points)"
#  #>
#  #>  Distances between the vertices and the 3 furthest points in the vertex regions
#  #>  (i-th entry corresponds to vertex i for i=1,2,3)
#  #>           [,1]      [,2]      [,3]
#  #> [1,] 0.3686516 0.7250712 0.3511223
#  #> [2,] 0.2982357        NA        NA
#  #> [3,] 0.4609925 0.4223345 0.2784818
#  #>
#  #>  Vertices of the Support Triangle
#  #>   [,1] [,2]
#  #> A  1.0    1
#  #> B  2.0    0
#  #> C  1.5    2
#  plot(Ext)

## ----eval=F-------------------------------------------------------------------
#  n<-10  #try also n<-20
#  Xp<-runif.std.tri(n)$gen.points

## ----closest2edges-Te, eval=F, fig.cap="Scatterplot of the uniform $X$ points in the triangle $T_e$. The closest points to the edges are marked with red crosses."----
#  Ext<-cl2edges.std.tri(Xp)
#  Ext
#  #> Call:
#  #> cl2edges.std.tri(Xp = Xp)
#  #>
#  #> Type:
#  #> [1] "Closest Points in the Standard Equilateral Triangle Te=T(A,B,C) with Vertices A=(0,0), B=(1,0), and C=(1/2,sqrt(3)/2) to its Edges"
#  summary(Ext)
#  #> Call:
#  #> cl2edges.std.tri(Xp = Xp)
#  #>
#  #> Type of the Extrema
#  #> [1] "Closest Points in the Standard Equilateral Triangle Te=T(A,B,C) with Vertices A=(0,0), B=(1,0), and C=(1/2,sqrt(3)/2) to its Edges"
#  #>
#  #>  Extremum Points: Closest Points in the Standard Equilateral Triangle to its Edges
#  #>  (Row i corresponds to edge i for i=1,2,3)
#  #>           [,1]      [,2]
#  #> [1,] 0.4763512 0.7726664
#  #> [2,] 0.4763512 0.7726664
#  #> [3,] 0.7111212 0.1053883
#  #> [1] "Edge labels are AB=3, BC=1, and AC=2 (correspond to row number in Extremum Points)"
#  #>
#  #>  Distances between Edges and the Closest Points in the Standard Equilateral Triangle
#  #>  (Row i corresponds to edge i for i=1,2,3)
#  #> [1] 0.06715991 0.02619907 0.10538830
#  #>
#  #>  Vertices of the Support Standard Equilateral Triangle
#  #>   [,1]      [,2]
#  #> A  0.0 0.0000000
#  #> B  1.0 0.0000000
#  #> C  0.5 0.8660254
#  plot(Ext,asp=1)

## ----f2eCMer, eval=F, fig.cap="Scatterplot of the uniform $X$ points in the triangle $T_e$. The furthest points in CM-edge regions to the corresponding edges are marked with red crosses."----
#  Ext<-fr2edgesCMedge.reg.std.tri(Xp)
#  Ext
#  #> Call:
#  #> fr2edgesCMedge.reg.std.tri(Xp = Xp)
#  #>
#  #> Type:
#  #> [1] "Furthest Points in the CM-Edge Regions of the Standard Equilateral Triangle T=(A,B,C) with A=(0,0), B=(1,0), and C=(1/2,sqrt(3)/2) from its Edges"
#  summary(Ext)
#  #> Call:
#  #> fr2edgesCMedge.reg.std.tri(Xp = Xp)
#  #>
#  #> Type of the Extrema
#  #> [1] "Furthest Points in the CM-Edge Regions of the Standard Equilateral Triangle T=(A,B,C) with A=(0,0), B=(1,0), and C=(1/2,sqrt(3)/2) from its Edges"
#  #>
#  #>  Extremum Points: Furthest Points in the CM-Edge Regions of the Standard Equilateral Triangle from its Edges
#  #>  (Row i corresponds to edge i for i=1,2,3)
#  #>           [,1]      [,2]
#  #> [1,] 0.6508705 0.2234491
#  #> [2,] 0.4590657 0.2878622
#  #> [3,] 0.7111212 0.1053883
#  #> [1] "Edge Labels are AB=3, BC=1, and AC=2 (correspond to row number in Extremum Points)"
#  #>
#  #>  Distances between the edges and the furthest points in the edge regions
#  #>  (i-th entry corresponds to edge i for i=1,2,3)
#  #> [1] 0.1906305 0.2536315 0.1053883
#  #>
#  #>  Vertices of the Support Standard Equilateral Triangle
#  #>   [,1]      [,2]
#  #> A  0.0 0.0000000
#  #> B  1.0 0.0000000
#  #> C  0.5 0.8660254
#  plot(Ext,asp=1)

## -----------------------------------------------------------------------------
c<-.4
a<-0; b<-10; int<-c(a,b)

nx<-10
Xp<-runif(nx,a,b)

## ----cl2McVR, eval=F, fig.cap="Scatterplot of the uniform $X$ points in the interval $(0,10)$. The closest points in $M_c-vertex regions to the center $M_c$ are marked with red crosses. The end points of the interval and the center are marked with dashed vertical lines."----
#  Ext<-cl2Mc.int(Xp,int,c)
#  Ext
#  #> Call:
#  #> cl2Mc.int(Xp = Xp, int = int, c = c)
#  #>
#  #> Type:
#  #> [1] "Closest Points in Mc-Vertex Regions of the Interval (a,b) = (0,10) to its Center Mc = 4"
#  summary(Ext)
#  #> Call:
#  #> cl2Mc.int(Xp = Xp, int = int, c = c)
#  #>
#  #> Type of the Extrema
#  #> [1] "Closest Points in Mc-Vertex Regions of the Interval (a,b) = (0,10) to its Center Mc = 4"
#  #>
#  #>  Extremum Points: Closest Points in Mc-Vertex Regions of the Interval to its Center
#  #>  (i-th entry corresponds to vertex i for i=1,2)
#  #> [1] 2.454885 4.100841
#  #> [1] "Vertex Labels are a=1 and b=2 for the interval (a,b)"
#  #>
#  #>  Distances between the Center Mc and the Closest Points to Mc in Mc-Vertex Regions
#  #>  (i-th entry corresponds to vertex i for i=1,2)
#  #> [1] 1.5451149 0.1008408
#  #>
#  #>  Vertices of the Support Interval
#  #>  a  b
#  #>  0 10
#  plot(Ext)

## ----eval=F-------------------------------------------------------------------
#  A<-c(0,0,0); B<-c(1,0,0); C<-c(1/2,sqrt(3)/2,0); D<-c(1/2,sqrt(3)/6,sqrt(6)/3)
#  set.seed(1)
#  tetra<-rbind(A,B,C,D)+matrix(runif(12,-.25,.25),ncol=3)
#  n<-10  #try also n<-20
#  Cent<-"CC"  #try also "CM"
#  
#  n<-10  #try also n<-20
#  Xp<-runif.tetra(n,tetra)$g  #try also Xp<-cbind(runif(n),runif(n),runif(n))

## ----cl2fCCvr, eval=F, fig.cap="Scatterplot of the uniform $X$ points in the tetrahedron $T(A,B,C,D)$. The closest points in CC-vertex regions to opposite faces are marked with red crosses."----
#  Ext<-cl2faces.vert.reg.tetra(Xp,tetra,Cent)
#  Ext
#  #> Call:
#  #> cl2faces.vert.reg.tetra(Xp = Xp, th = tetra, M = Cent)
#  #>
#  #> Type:
#  #> [1] "Closest Points in CC-Vertex Regions of the Tetrahedron with Vertices A=(-0.12,-0.15,0.06), B=(0.94,0.2,-0.22), C=(0.54,1.09,-0.15), and D=(0.7,0.37,0.65) to the Opposite Faces"
#  summary(Ext)
#  #> Call:
#  #> cl2faces.vert.reg.tetra(Xp = Xp, th = tetra, M = Cent)
#  #>
#  #> Type of the Extrema
#  #> [1] "Closest Points in CC-Vertex Regions of the Tetrahedron with Vertices A=(-0.12,-0.15,0.06), B=(0.94,0.2,-0.22), C=(0.54,1.09,-0.15), and D=(0.7,0.37,0.65) to the Opposite Faces"
#  #>
#  #>  Extremum Points: Closest Points in CC-Vertex Regions of the Tetrahedron to its Faces
#  #>  (Row i corresponds to face i for i=1,2,3,4)
#  #>           [,1]      [,2]       [,3]
#  #> [1,] 0.1073281 0.0109421 0.19871179
#  #> [2,] 0.6535570 0.2922984 0.15795015
#  #> [3,] 0.5199352 0.6610763 0.08954581
#  #> [4,] 0.5127296 0.5449680 0.24057920
#  #> [1] "Vertex labels are A=1, B=2, C=3, and D=4 (correspond to row number in Extremum Points)"
#  #>
#  #>  Distances between Faces and the Closest Points to the Faces in CC-Vertex Regions
#  #>  (i-th entry corresponds to vertex region i for i=1,2,3,4)
#  #> [1] 0.7554212 0.2773495 0.4803672 0.3509001
#  #>
#  #>  Vertices of the Support Tetrahedron
#  #>         [,1]       [,2]        [,3]
#  #> A -0.1172457 -0.1491590  0.06455702
#  #> B  0.9360619  0.1991948 -0.21910686
#  #> C  0.5364267  1.0883630 -0.14701271
#  #> D  0.7041039  0.3690740  0.65477496
#  plot(Ext)

