## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>",fig.width=6, fig.height=4, fig.align = "center") 

## ----setup, message=FALSE, results='hide'-------------------------------------
library(pcds)

## ----SPch1--------------------------------------------------------------------
A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
Tr<-rbind(A,B,C)
n<-5  #try also n<-10, 20, 50 or 100

## ----CSR1T, eval=F, fig.cap="Scatterplot of the uniform points in the triangle $T=T(A,B,C)$ with vertices $A=(1,1)$, $B=(2,0)$, and $C=(1.5,2)$."----
#  set.seed(123)
#  Xdt<-runif.tri(n,Tr)
#  Xdt
#  #> Call:
#  #> runif.tri(n = n, tri = Tr)
#  #>
#  #> Type:
#  #> [1] "Uniform Distribution in the Triangle with Vertices (1,1), (2,0) and (1.5,2)"
#  summary(Xdt)
#  #> Call:
#  #> runif.tri(n = n, tri = Tr)
#  #>
#  #> Type of the Pattern : [1] "Uniform Distribution in the Triangle with Vertices (1,1), (2,0) and (1.5,2)"
#  #>
#  #> Study Window
#  #> range in x-coordinate = 1 2
#  #> range in y-coordinate = 0 2
#  #>
#  #>  Vertices of the Support of the Uniform Distribution
#  #>   [,1] [,2]
#  #> A  1.0    1
#  #> B  2.0    0
#  #> C  1.5    2
#  #>
#  #>  5 uniform points in the triangle with vertices (1,1), (2,0) and (1.5,2)
#  #>  (first 6 or fewer are printed)
#  #>          [,1]      [,2]
#  #> [1,] 1.408977 1.7660348
#  #> [2,] 1.940467 0.0911130
#  #> [3,] 1.528105 1.7848381
#  #> [4,] 1.551435 0.9132295
#  #> [5,] 1.677571 1.1452668
#  #>
#  #> Number of points
#  #> nx ny
#  #>  5  3
#  #> nx : the number of uniform points
#  #> ny : the number of vertices of the support region
#  plot(Xdt)

## ----SPch2--------------------------------------------------------------------
nx<-10; ny<-5  #try also nx<-40; ny<-5 or nx<-100;  #try also nx<-1000;  ny<-10;
set.seed(1)
Yp<-cbind(runif(ny,0,10),runif(ny,0,10))

## ----CSRmT, fig.cap="Scatterplot of the uniform $X$ points in the Delaunay triangles based on 5 $Y$ points."----
Xdt<-runifMT(nx,Yp)  #data under CSR in the convex hull of Ypoints
Xdt
summary(Xdt)
plot(Xdt)

## ----SPch3, eval=F------------------------------------------------------------
#  set.seed(11)
#  A<-sample(1:12,3); B<-sample(1:12,3); C<-sample(1:12,3); D<-sample(1:12,3)
#  tetra<-rbind(A,B,C,D)/6
#  n<-5 #try also n<-10, 20, 50, or 100

## ----CSR1th, eval=F, fig.cap="Scatterplot of the uniform $X$ points in the tetrahedron $T=T(A,B,C,D)$."----
#  Xdt<-runif.tetra(n,tetra)
#  Xdt
#  #> Call:
#  #> runif.tetra(n = n, th = tetra)
#  #>
#  #> Type:
#  #> [1] "Uniform Distribution in the Tetrahedron with Vertices (1.67,0.33,1.33), (1.5,0.17,0.83), (2,1,0.83) and (1,1.17,0.83)"
#  summary(Xdt)
#  #> Call:
#  #> runif.tetra(n = n, th = tetra)
#  #>
#  #> Type of the Pattern : [1] "Uniform Distribution in the Tetrahedron with Vertices (1.67,0.33,1.33), (1.5,0.17,0.83), (2,1,0.83) and (1,1.17,0.83)"
#  #>
#  #> Study Window
#  #> range in x-coordinate = 1 2
#  #> range in y-coordinate = 0.1666667 1.166667
#  #>
#  #>  Vertices of the Support of the Uniform Distribution
#  #>       [,1]      [,2]      [,3]
#  #> A 1.666667 0.3333333 1.3333333
#  #> B 1.500000 0.1666667 0.8333333
#  #> C 2.000000 1.0000000 0.8333333
#  #> D 1.000000 1.1666667 0.8333333
#  #>
#  #>  5 uniform points in the tetrahedron with vertices (1.67,0.33,1.33), (1.5,0.17,0.83), (2,1,0.83) and (1,1.17,0.83)
#  #>  (first 6 or fewer are printed)
#  #>          [,1]      [,2]      [,3]
#  #> [1,] 1.398149 0.6715843 0.9971730
#  #> [2,] 1.642032 0.4435400 0.8851113
#  #> [3,] 1.502856 0.7644274 1.0461309
#  #> [4,] 1.425548 0.5928684 0.9355892
#  #> [5,] 1.239314 0.9320898 0.9298472
#  #>
#  #> Number of points
#  #> nx ny
#  #>  5  4
#  #> nx is the number of Uniform points
#  #>  ny is the number of vertices of the support region
#  plot(Xdt)

## ----SPch4--------------------------------------------------------------------
A<-c(1,1); B<-c(2,0); C<-c(1.5,7/3);
Tr<-rbind(A,B,C)
del<-.4
n<-10  #try also n<-100 or 1000

## ----seg1T, eval=F, fig.cap="Scatterplot of the points segregated (in a type I fashion) from the vertices of the triangle $T=T(A,B,C)$ with vertices $A=(1,1)$, $B=(2,0)$, and $C=(1.5,2)$."----
#  Xdt<-rseg.tri(n,Tr,del)
#  Xdt
#  #> Call:
#  #> rseg.tri(n = n, tri = Tr, delta = del)
#  #>
#  #> Type:
#  #> [1] "Type I Segregation of 10 points in the triangle with vertices (1,1), (2,0) and (1.5,2.33) and exclusion parameter delta = 0.4"
#  summary(Xdt)
#  #> Call:
#  #> rseg.tri(n = n, tri = Tr, delta = del)
#  #>
#  #> Type of the Pattern
#  #> [1] "Type I Segregation of 10 points in the triangle with vertices (1,1), (2,0) and (1.5,2.33) and exclusion parameter delta = 0.4"
#  #>
#  #> Parameters of the Pattern
#  #> exclusion parameter
#  #>                 0.4
#  #>
#  #> Study Window
#  #> range in x-coordinate = 1 2
#  #> range in y-coordinate = 0 2.333333
#  #>
#  #>  Generated Points from Pattern of Type I Segregation of One Class from Vertices of the Triangle
#  #>  (first 6 or fewer are printed)
#  #>         [,1]      [,2]
#  #> pnt 1.587007 0.5205062
#  #> pnt 1.409048 0.9539932
#  #> pnt 1.669594 0.7064556
#  #> pnt 1.523988 0.5721596
#  #> pnt 1.271635 1.6545486
#  #> pnt 1.674897 1.3032453
#  #>
#  #> Number of points:
#  #> nx ny
#  #> 10  3
#  #> nx = number of generated points according to the pattern
#  #> ny = number of reference (i.e. Y) points
#  plot(Xdt)

## ----segI---------------------------------------------------------------------
ny<-5;
set.seed(1)
Yp<-cbind(runif(ny),runif(ny))
del<-.4
nx<-10;  #try also nx<-100 or 1000;  

## ----segmT, fig.cap="Scatterplot of the $X$ points segregated (in a type I fashion) from the $Y$ points in each Delaunay triangle."----
Xdt<-rsegMT(nx,Yp,del)
Xdt
summary(Xdt)
plot(Xdt)

## -----------------------------------------------------------------------------
nx<-10;  #try also nx<-100 or 1000; 
ny<-5
e<-.15;

## ----SPch5, eval=F------------------------------------------------------------
#  #with default bounding box (i.e., unit square)
#  set.seed(1)
#  Yp<-cbind(runif(ny),runif(ny))

## ----segmTcirc, eval=F, fig.cap="Scatterplot of the $X$ points segregated (in a circular fashion) from the $Y$ points in the unit square."----
#  Xdt<-rseg.circ(nx,Yp,e)
#  Xdt
#  #> Call:
#  #> rseg.circ(n = nx, Yp = Yp, e = e)
#  #>
#  #> Type:
#  #> [1] "Segregation of 10 X points from 5 Y points with circular (or radial) exclusion parameter e = 0.15"
#  summary(Xdt)
#  #> Call:
#  #> rseg.circ(n = nx, Yp = Yp, e = e)
#  #>
#  #> Type of the Pattern
#  #> [1] "Segregation of 10 X points from 5 Y points with circular (or radial) exclusion parameter e = 0.15"
#  #>
#  #> Parameters of the Pattern
#  #> exclusion parameter
#  #>                0.15
#  #>
#  #> Study Window
#  #> range in x-coordinate = 0.05168193 1.058208
#  #> range in y-coordinate = -0.08821373 1.094675
#  #>
#  #>  Generated Points from Pattern of Segregation of Class X from Class Y
#  #>  (first 6 or fewer are printed)
#  #>            [,1]       [,2]
#  #> [1,] 0.82654723 0.50050923
#  #> [2,] 0.77398352 1.08510108
#  #> [3,] 0.99248692 0.16272732
#  #> [4,] 0.70760843 0.06030401
#  #> [5,] 0.32064644 0.36851638
#  #> [6,] 0.06515965 0.36410878
#  #>
#  #> Number of points:
#  #> nx ny
#  #> 10  5
#  #> nx = number of generated points according to the pattern
#  #> ny = number of reference (i.e. Y) points
#  plot(Xdt,asp=1)

## ----SPch6--------------------------------------------------------------------
A<-c(1,1); B<-c(2,0); C<-c(1.5,7/3);
Tr<-rbind(A,B,C)
del<-.4
n<-5  #try also n<-100 or 1000

## ----asc1T, eval=F, fig.cap="Scatterplot of the points associated (in a type I fashion) with the vertices of the triangle $T=T(A,B,C)$ with vertices $A=(1,1)$, $B=(2,0)$, and $C=(1.5,2)$."----
#  Xdt<-rasc.tri(n,Tr,del)
#  Xdt
#  #> Call:
#  #> rasc.tri(n = n, tri = Tr, delta = del)
#  #>
#  #> Type:
#  #> [1] "Type I Association of 5 points in the triangle with vertices (1,1), (2,0) and (1.5,2.33) with attraction parameter delta = 0.4"
#  summary(Xdt)
#  #> Call:
#  #> rasc.tri(n = n, tri = Tr, delta = del)
#  #>
#  #> Type of the Pattern
#  #> [1] "Type I Association of 5 points in the triangle with vertices (1,1), (2,0) and (1.5,2.33) with attraction parameter delta = 0.4"
#  #>
#  #> Parameters of the Pattern
#  #> attraction parameter
#  #>                  0.4
#  #>
#  #> Study Window
#  #> range in x-coordinate = 1 2
#  #> range in y-coordinate = 0 2.333333
#  #>
#  #>  Generated Points from Pattern of Type I Association of One Class with Vertices of the Triangle
#  #>  (first 6 or fewer are printed)
#  #>         [,1]      [,2]
#  #> pnt 1.529720 1.8418312
#  #> pnt 1.477620 2.0094888
#  #> pnt 1.478545 1.7880582
#  #> pnt 1.476351 2.0817961
#  #> pnt 1.757087 0.4729486
#  #>
#  #> Number of points:
#  #> nx ny
#  #>  5  3
#  #> nx = number of generated points according to the pattern
#  #> ny = number of reference (i.e. Y) points
#  plot(Xdt)

## ----SPch7--------------------------------------------------------------------
ny<-5;
set.seed(1)
Yp<-cbind(runif(ny),runif(ny))
del<-.4
nx<-10;  #try also nx<-100 or 1000; 

## ----ascmT, fig.cap="Scatterplot of the $X$ points associated (in a type I fashion) with the $Y$ points in each Delaunay triangle."----
Xdt<-rascMT(nx,Yp,del)
Xdt
summary(Xdt)
plot(Xdt)

## ----SPch8, eval=F------------------------------------------------------------
#  ny<-5;
#  e<-.15;
#  #with default bounding box (i.e., unit square)
#  set.seed(1)
#  Yp<-cbind(runif(ny),runif(ny))
#  nx<-10;  #try also nx<-100 or 1000;

## ----ascmTcirc, eval=F, fig.cap="Scatterplot of the $X$ points associated (in a circular fashion) with the $Y$ points in each Delaunay triangle."----
#  Xdt<-rasc.circ(nx,Yp,e)
#  Xdt
#  #> Call:
#  #> rasc.circ(n = nx, Yp = Yp, e = e)
#  #>
#  #> Type:
#  #> [1] "Association of 10 points with 5 Y points with circular (or radial) attraction parameter e = 0.15"
#  summary(Xdt)
#  #> Call:
#  #> rasc.circ(n = nx, Yp = Yp, e = e)
#  #>
#  #> Type of the Pattern
#  #> [1] "Association of 10 points with 5 Y points with circular (or radial) attraction parameter e = 0.15"
#  #>
#  #> Parameters of the Pattern
#  #> attraction parameter
#  #>                 0.15
#  #>
#  #> Study Window
#  #> range in x-coordinate = 0.05168193 1.058208
#  #> range in y-coordinate = -0.08821373 1.094675
#  #>
#  #>  Generated Points from Pattern of Association of one Class with Class Y
#  #>  (first 6 or fewer are printed)
#  #>           [,1]      [,2]
#  #> [1,] 0.4341972 0.8314177
#  #> [2,] 0.5369080 0.6210061
#  #> [3,] 0.8844546 0.7025082
#  #> [4,] 0.8779856 0.6771867
#  #> [5,] 0.8397240 0.5659668
#  #> [6,] 0.1228222 0.0294437
#  #>
#  #> Number of points:
#  #> nx ny
#  #> 10  5
#  #> nx = number of generated points according to the pattern
#  #> ny = number of reference (i.e. Y) points
#  plot(Xdt,asp=1)

## ----SPch9, eval=F------------------------------------------------------------
#  ny<-5;
#  e<-.15;
#  #with default bounding box (i.e., unit square)
#  set.seed(1)
#  Yp<-cbind(runif(ny),runif(ny))
#  nx<-10;  #try also nx<-100 or 1000;

## ----ascmTmat, eval=F, fig.cap="Scatterplot of the $X$ points associated (in a Matérn-like fashion) with the $Y$ points."----
#  Xdt<-rasc.matern(nx,Yp,e)
#  Xdt
#  #> Call:
#  #> rasc.matern(n = nx, Yp = Yp, e = e)
#  #>
#  #> Type:
#  #> [1] "Matern-like Association of 10 points with 5 Y points with circular attraction parameter e = 0.15"
#  summary(Xdt)
#  #> Call:
#  #> rasc.matern(n = nx, Yp = Yp, e = e)
#  #>
#  #> Type of the Pattern
#  #> [1] "Matern-like Association of 10 points with 5 Y points with circular attraction parameter e = 0.15"
#  #>
#  #> Parameters of the Pattern
#  #> attraction parameter
#  #>                 0.15
#  #>
#  #> Study Window
#  #> range in x-coordinate = 0.05168193 1.058208
#  #> range in y-coordinate = -0.08821373 1.094675
#  #>
#  #>  Generated Points from Pattern of Matern-like Association of one Class with Class Y
#  #>  (first 6 or fewer are printed)
#  #>            [,1]       [,2]
#  #> [1,] 0.56278796 0.75345984
#  #> [2,] 0.66528156 0.66859254
#  #> [3,] 0.32528878 0.83448201
#  #> [4,] 0.08626992 0.07483616
#  #> [5,] 0.13700582 0.06441234
#  #> [6,] 0.42942439 0.83624485
#  #>
#  #> Number of points:
#  #> nx ny
#  #> 10  5
#  #> nx = number of generated points according to the pattern
#  #> ny = number of reference (i.e. Y) points
#  plot(Xdt,asp=1)

