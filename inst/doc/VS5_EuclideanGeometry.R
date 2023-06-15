## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>",fig.width=6, fig.height=4, fig.align = "center")
#knitr::write_bib("knitr", file = "References.bib")

## ----setup, message=FALSE, results='hide'-------------------------------------
library(pcds)
library(plot3D)

## ----lAB, fig.cap="The line joining the points $A$ and $B$ in 2D space."------
A<-c(-1.22,-2.33); B<-c(2.55,3.75)

xr<-range(A,B);
xf<-(xr[2]-xr[1])*.1 #how far to go at the lower and upper ends in the x-coordinate
x<-seq(xr[1]-xf,xr[2]+xf,l=5)  #try also l=10, 20, or 100

lnAB<-Line(A,B,x)
lnAB
summary(lnAB)
plot(lnAB)

## ----EGch6, eval=F------------------------------------------------------------
#  line(A,B)  #this takes vector A as the x points and vector B as the y points and fits the line
#  #>
#  #> Call:
#  #> line(A, B)
#  #>
#  #> Coefficients:
#  #> [1]   1.231  -1.081
#  
#  line(x,lnAB$y) #gives the same line as Line(A,B,x) above
#  #>
#  #> Call:
#  #> line(x, lnAB$y)
#  #>
#  #> Coefficients:
#  #> [1]  -0.3625   1.6127
#  c(lnAB$intercept,lnAB$slope)
#  #>  intercept      slope
#  #> -0.3624668  1.6127321

## ----EGch7, eval=F------------------------------------------------------------
#  slope(A,B)
#  #> [1] 1.612732

## ----lPpl2AB, eval=F, fig.cap="The line crossingn the point $P$ and parallel to the line segment $[AB]$ in 2D space."----
#  A<-c(1.1,1.2); B<-c(2.3,3.4); P<-c(.51,2.5)
#  
#  pts<-rbind(A,B,P)
#  xr<-range(pts[,1])
#  xf<-(xr[2]-xr[1])*.25 #how far to go at the lower and upper ends in the x-coordinate
#  x<-seq(xr[1]-xf,xr[2]+xf,l=5)  #try also l=10, 20, or 100
#  
#  plnAB<-paraline(P,A,B,x)
#  plnAB
#  #> Call:
#  #> paraline(p = P, a = A, b = B, x = x)
#  #>
#  #> Coefficients of the line (in the form: y = slope * x + intercept)
#  #>     slope intercept
#  #>  1.833333  1.565000
#  summary(plnAB)
#  #> Call:
#  #> paraline(p = P, a = A, b = B, x = x)
#  #>
#  #> Defining Points
#  #>   [,1] [,2]
#  #> A 1.10  1.2
#  #> B 2.30  3.4
#  #> P 0.51  2.5
#  #>
#  #>  Selected x points (first row) and estimated y points (second row) that fall on the Line
#  #>       (first 6 or fewer are printed at each row)
#  #> [1] 0.06250 0.73375 1.40500 2.07625 2.74750
#  #> [1] 1.679583 2.910208 4.140833 5.371458 6.602083
#  #>
#  #> Equation of the Line Crossing Point P and Parallel to Line Segment [AB]
#  #> [1] "y=1.83333333333333x+1.565"
#  #>
#  #> Coefficients of the line (when the line is in the form: y = slope * x + intercept)
#  #> intercept     slope
#  #>  1.565000  1.833333
#  plot(plnAB)

## ----lPprp2AB, eval=F, fig.cap="The line crossing point $P$ and perpendicular to the line joining $A$ and $B$."----
#  A<-c(1.1,1.2); B<-c(2.3,3.4); P<-c(.51,2.5)
#  
#  pts<-rbind(A,B,P)
#  xr<-range(pts[,1])
#  xf<-(xr[2]-xr[1])*.25 #how far to go at the lower and upper ends in the x-coordinate
#  x<-seq(xr[1]-xf,xr[2]+xf,l=5)  #try also l=10, 20, or 100
#  
#  plnAB<-perpline(P,A,B,x)
#  plnAB
#  #> Call:
#  #> perpline(p = P, a = A, b = B, x = x)
#  #>
#  #> Coefficients of the line (in the form: y = slope * x + intercept)
#  #>      slope  intercept
#  #> -0.5454545  2.7781818
#  summary(plnAB)
#  #> Call:
#  #> perpline(p = P, a = A, b = B, x = x)
#  #>
#  #> Defining Points
#  #>   [,1] [,2]
#  #> A 1.10  1.2
#  #> B 2.30  3.4
#  #> P 0.51  2.5
#  #>
#  #>  Selected x points (first row) and estimated y points (second row) that fall on the Line
#  #>       (first 6 or fewer are printed at each row)
#  #> [1] 0.06250 0.73375 1.40500 2.07625 2.74750
#  #> [1] 2.744091 2.377955 2.011818 1.645682 1.279545
#  #>
#  #> Equation of the Line Crossing Point P Perpendicular to Line Segment [AB]
#  #> [1] "y=-0.545454545454545x+2.77818181818182"
#  #>
#  #> Coefficients of the line (when the line is in the form: y = slope * x + intercept)
#  #>  intercept      slope
#  #>  2.7781818 -0.5454545
#  plot(plnAB,asp=1)

## ----EGch1, eval=F------------------------------------------------------------
#  A<-c(-1.22,-2.33); B<-c(2.55,3.75); C<-c(0,6); D<-c(3,-2)
#  ip<-intersect2lines(A,B,C,D)
#  ip
#  #> [1] 1.486767 2.035289

## ----EGch2, eval=F------------------------------------------------------------
#  A<-c(.3,.2); B<-c(.6,.3); C<-c(1,1)
#  
#  angle.str2end(A,B,C)
#  #> $small.arc.angles
#  #> [1] 1.051650 3.463343
#  #>
#  #> $ccw.arc.angles
#  #> [1] -2.819842  1.051650
#  angle.str2end(A,B,C,radian=FALSE)
#  #> $small.arc.angles
#  #> [1]  60.25512 198.43495
#  #>
#  #> $ccw.arc.angles
#  #> [1] -161.56505   60.25512

## ----angBA-BC, eval=F, fig.cap="The line segments $BA$ and $BC$ and the angles (in degrees) between them and between the line segments and the $x$-axis."----
#  pts<-rbind(A,B,C)
#  
#  Xp<-c(B[1]+max(abs(C[1]-B[1]),abs(A[1]-B[1])),0)
#  
#  #plot of the line segments
#  ang.rad<-angle.str2end(A,B,C,radian=TRUE); ang.rad
#  ang.deg<-angle.str2end(A,B,C,radian=FALSE); ang.deg
#  ang.deg1<-ang.deg$s; ang.deg1
#  ang.deg2<-ang.deg$c; ang.deg2
#  
#  rad<-min(Dist(A,B),Dist(B,C))
#  
#  Xlim<-range(pts[,1],Xp[1],B+Xp,B[1]+c(+rad,-rad))
#  Ylim<-range(pts[,2],B[2]+c(+rad,-rad))
#  xd<-Xlim[2]-Xlim[1]
#  yd<-Ylim[2]-Ylim[1]
#  
#  #plot for the counter-clockwise arc
#  plot(pts,pch=1,asp=1,xlab="x",ylab="y",xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05))
#  L<-rbind(B,B,B); R<-rbind(A,C,B+Xp)
#  segments(L[,1], L[,2], R[,1], R[,2], lty=2)
#  plotrix::draw.arc(B[1],B[2],radius=.3*rad,angle1=ang.rad$c[1],angle2=ang.rad$c[2])
#  plotrix::draw.arc(B[1],B[2],radius=.6*rad,angle1=0, angle2=ang.rad$s[1],lty=2,col=2)
#  plotrix::draw.arc(B[1],B[2],radius=.9*rad,angle1=0,angle2=ang.rad$s[2],col=3)
#  txt<-pts
#  text(txt+cbind(rep(xd*.02,nrow(txt)),rep(-xd*.02,nrow(txt))),c("A","B","C"))
#  
#  text(rbind(B)+.5*rad*c(cos(mean(ang.rad$c)),sin(mean(ang.rad$c))),
#       paste(abs(round(ang.deg2[2]-ang.deg2[1],2))," degrees",sep=""))
#  text(rbind(B)+.6*rad*c(cos(ang.rad$s[1]/2),sin(ang.rad$s[1]/2)),paste(round(ang.deg1[1],2)),col=2)
#  text(rbind(B)+.9*rad*c(cos(ang.rad$s[2]/2),sin(ang.rad$s[2]/2)),paste(round(ang.deg1[2],2)),col=3)

## ----EGch3, eval=F------------------------------------------------------------
#  A<-c(0,0); B<-c(1,0); C<-c(0.5,.8);
#  Tr<-rbind(A,B,C);
#  area.polygon(Tr)
#  #> [1] 0.4
#  
#  A<-c(0,0); B<-c(1,0); C<-c(.7,.6); D<-c(0.3,.8);
#  h1<-rbind(A,B,C,D);  #try also h1<-rbind(A,B,D,C) or h1<-rbind(A,C,B,D) or h1<-rbind(A,D,C,B);
#  area.polygon(h1)
#  #> [1] 0.49

## ----EGch4, eval=F------------------------------------------------------------
#  dpl<-dist.point2line(P,A,B);
#  dpl
#  #> $distance
#  #> [1] 2.5
#  #>
#  #> $cl2p
#  #> [1] 0.51 0.00

## ----EGch5, eval=F------------------------------------------------------------
#  X1<-cbind(runif(10),runif(10))
#  dist.point2set(c(1,2),X1)
#  #> $distance
#  #> [1] 1.086349
#  #>
#  #> $ind.cl.point
#  #> [1] 3
#  #>
#  #> $closest.point
#  #> [1] 0.7933641 0.9334844

## ----l3dPQ, eval=F, fig.cap="The line crossing the point $P$ parallel to the vector $OQ$."----
#  P<-c(1,10,3); Q<-c(1,1,3);
#  
#  vecs<-rbind(P,Q)
#  
#  Line3D(P,Q,.1)
#  #> Call:
#  #> Line3D(p = P, v = Q, t = 0.1)
#  #>
#  #> Coefficients of the parameterized line passing through initial point P = (x0,y0,z0) in the direction of OQ = (A,B,C) (for the form: x=x0 + A*t, y=y0 + B*t, and z=z0 + C*t)
#  #>                  [,1] [,2] [,3]
#  #> initial point       1   10    3
#  #> direction vector    1    1    3
#  Line3D(P,Q,.1,dir.vec=FALSE)
#  #> Call:
#  #> Line3D(p = P, v = Q, t = 0.1, dir.vec = FALSE)
#  #>
#  #> Coefficients of the parameterized line passing through initial point P = (x0,y0,z0) in the direction of PQ = (A,B,C) (for the form: x=x0 + A*t, y=y0 + B*t, and z=z0 + C*t)
#  #>                  [,1] [,2] [,3]
#  #> initial point       1   10    3
#  #> direction vector    0   -9    0
#  
#  tr<-range(vecs);
#  tf<-(tr[2]-tr[1])*.1 #how far to go at the lower and upper ends in the x-coordinate
#  tsq<-seq(-tf*10-tf,tf*10+tf,l=5)  #try also l=10, 20, or 100
#  
#  lnPQ3D<-Line3D(P,Q,tsq)
#  lnPQ3D
#  #> Call:
#  #> Line3D(p = P, v = Q, t = tsq)
#  #>
#  #> Coefficients of the parameterized line passing through initial point P = (x0,y0,z0) in the direction of OQ = (A,B,C) (for the form: x=x0 + A*t, y=y0 + B*t, and z=z0 + C*t)
#  #>                  [,1] [,2] [,3]
#  #> initial point       1   10    3
#  #> direction vector    1    1    3
#  summary(lnPQ3D)
#  #> Call:
#  #> Line3D(p = P, v = Q, t = tsq)
#  #>
#  #> Defining Vectors
#  #>                  [,1] [,2] [,3]
#  #> initial point       1   10    3
#  #> direction vector    1    1    3
#  #>
#  #>  Estimated x points (first row), y points (second row), and z points (third row)
#  #>    that fall on the Line
#  #>       (first 6 or fewer are printed at each row)
#  #> [1] -8.90 -3.95  1.00  5.95 10.90
#  #> [1]  0.10  5.05 10.00 14.95 19.90
#  #> [1] -26.70 -11.85   3.00  17.85  32.70
#  #>
#  #> Equation of the line passing through point P in the direction of OQ  with O representing the origin (0,0,0)
#  #>                    (i.e., parallel to OQ )
#  #>      [,1]
#  #> [1,] "x = 1 + t"
#  #> [2,] "y = 10 + t"
#  #> [3,] "z = 3 + 3t"
#  #>
#  #> Coefficients of the parameterized line passing through initial point P = (x0,y0,z0) in the direction of OQ = (A,B,C) (in the form: x = x0 + A*t, y = y0 + B*t, and z = z0 + C*t)
#  #>    [,1] [,2] [,3]
#  #> P     1   10    3
#  #> OQ    1    1    3
#  
#  plot(lnPQ3D)

## ----l3dPpl2QR, eval=F, fig.cap="The line crossing the point $P$ and parallel to the line segment $QR$ in 3D space."----
#  P<-c(1,10,4); Q<-c(1,1,3); R<-c(3,9,12)
#  
#  vecs<-rbind(P,R-Q)
#  pts<-rbind(P,Q,R)
#  tr<-range(pts,vecs);
#  tf<-(tr[2]-tr[1])*.1 #how far to go at the lower and upper ends in the x-coordinate
#  tsq<-seq(-tf*10-tf,tf*10+tf,l=5)  #try also l=10, 20, or 100
#  
#  pln3D<-paraline3D(P,Q,R,tsq)
#  pln3D
#  #> Call:
#  #> paraline3D(p = P, a = Q, b = R, t = tsq)
#  #>
#  #> Coefficients of the parameterized line passing through initial point P = (x0,y0,z0) in the direction of R - Q = (A,B,C) (for the form: x=x0 + A*t, y=y0 + B*t, and z=z0 + C*t)
#  #>                  [,1] [,2] [,3]
#  #> initial point       1   10    4
#  #> direction vector    2    8    9
#  summary(pln3D)
#  #> Call:
#  #> paraline3D(p = P, a = Q, b = R, t = tsq)
#  #>
#  #> Defining Vectors
#  #>                  [,1] [,2] [,3]
#  #> initial point       1   10    4
#  #> direction vector    2    8    9
#  #>
#  #>  Estimated x points (first row), y points (second row), and z points (third row)
#  #>    that fall on the Line
#  #>       (first 6 or fewer are printed at each row)
#  #> [1] -23.2 -11.1   1.0  13.1  25.2
#  #> [1] -86.8 -38.4  10.0  58.4 106.8
#  #> [1] -104.90  -50.45    4.00   58.45  112.90
#  #>
#  #> Equation of the line passing through point P parallel to the line joining points Q and R
#  #>      [,1]
#  #> [1,] "x = 1 + 2t"
#  #> [2,] "y = 10 + 8t"
#  #> [3,] "z = 4 + 9t"
#  #>
#  #> Coefficients of the parameterized line passing through initial point P = (x0,y0,z0) in the direction of R - Q = (A,B,C) (in the form: x = x0 + A*t, y = y0 + B*t, and z = z0 + C*t)
#  #>       [,1] [,2] [,3]
#  #> P        1   10    4
#  #> R - Q    2    8    9
#  
#  plot(pln3D)

## ----plP123, eval=F, fig.cap="The plane joining the points $P_1$, $P_2$, and $P_3$ in 3D space."----
#  P1<-c(1,10,3); P2<-c(1,1,3); P3<-c(3,9,12) #also try P2=c(2,2,3)
#  
#  pts<-rbind(P1,P2,P3)
#  Plane(P1,P2,P3,.1,.2)
#  #> Call:
#  #> Plane(a = P1, b = P2, c = P3, x = 0.1, y = 0.2)
#  #>
#  #> Coefficients of the Plane (in the form: z = A*x + B*y + C):
#  #>    A    B    C
#  #>  4.5  0.0 -1.5
#  
#  xr<-range(pts[,1]); yr<-range(pts[,2])
#  xf<-(xr[2]-xr[1])*.1 #how far to go at the lower and upper ends in the x-coordinate
#  yf<-(yr[2]-yr[1])*.1 #how far to go at the lower and upper ends in the y-coordinate
#  x<-seq(xr[1]-xf,xr[2]+xf,l=3)  #try also l=10, 20, or 100
#  y<-seq(yr[1]-yf,yr[2]+yf,l=3)  #try also l=10, 20, or 100
#  
#  plP123<-Plane(P1,P2,P3,x,y)
#  plP123
#  #> Call:
#  #> Plane(a = P1, b = P2, c = P3, x = x, y = y)
#  #>
#  #> Coefficients of the Plane (in the form: z = A*x + B*y + C):
#  #>    A    B    C
#  #>  4.5  0.0 -1.5
#  summary(plP123)
#  #> Call:
#  #> Plane(a = P1, b = P2, c = P3, x = x, y = y)
#  #>
#  #> Defining Points
#  #>    [,1] [,2] [,3]
#  #> P1    1   10    3
#  #> P2    1    1    3
#  #> P3    3    9   12
#  #>
#  #>  Selected x and y points and estimated z points --- presented row-wise, respectively --- that fall on the Plane
#  #>       (first 6 or fewer are printed on each row)
#  #> [1] 0.8 2.0 3.2
#  #> [1]  0.1  5.5 10.9
#  #> [1]  2.1  7.5 12.9
#  #>
#  #> Equation of the Plane Passing through Points P1, P2, and P3
#  #> [1] "z = 4.5x -1.5"
#  #>
#  #> Coefficients of the Plane (in the form z = A*x + B*y + C):
#  #>    A    B    C
#  #>  4.5  0.0 -1.5
#  plot(plP123,theta = 225, phi = 30, expand = 0.7, facets = FALSE, scale = TRUE)

## ----plPpl2QRS, eval=F, fig.cap="The plane crossing the point $P$ and parallel to the plane spanned by the points $Q$, $R$, and $S$ in 3D space."----
#  Q<-c(1,10,3); R<-c(2,2,3); S<-c(3,9,12); P<-c(1,2,4)
#  
#  pts<-rbind(Q,R,S,P)
#  xr<-range(pts[,1]); yr<-range(pts[,2])
#  xf<-(xr[2]-xr[1])*.25 #how far to go at the lower and upper ends in the x-coordinate
#  yf<-(yr[2]-yr[1])*.25 #how far to go at the lower and upper ends in the y-coordinate
#  x<-seq(xr[1]-xf,xr[2]+xf,l=5)  #try also l=10, 20, or 100
#  y<-seq(yr[1]-yf,yr[2]+yf,l=5)  #try also l=10, 20, or 100
#  
#  plP2QRS<-paraplane(P,Q,R,S,x,y)
#  plP2QRS
#  #> Call:
#  #> paraplane(p = P, a = Q, b = R, c = S, x = x, y = y)
#  #>
#  #> Coefficients of the Plane (in the form: z = A*x + B*y + C):
#  #>    A    B    C
#  #>  4.8  0.6 -2.0
#  summary(plP2QRS)
#  #> Call:
#  #> paraplane(p = P, a = Q, b = R, c = S, x = x, y = y)
#  #>
#  #> Defining Points
#  #>   [,1] [,2] [,3]
#  #> Q    1   10    3
#  #> R    2    2    3
#  #> S    3    9   12
#  #> P    1    2    4
#  #>
#  #>  Selected x and y points and estimated z points --- presented row-wise, respectively --- that fall on the Plane
#  #>       (first 6 or fewer are printed on each row)
#  #> [1] 0.50 1.25 2.00 2.75 3.50
#  #> [1]  0  3  6  9 12
#  #> [1]  0.4  4.0  7.6 11.2 14.8
#  #>
#  #> Equation of the Plane Passing through Point P Parallel to the Plane
#  #>  Passing through Points Q, R and S
#  #> [1] "z = 4.8x + 0.6y -2"
#  #>
#  #> Coefficients of the Plane (in the form z = A*x + B*y + C):
#  #>    A    B    C
#  #>  4.8  0.6 -2.0
#  
#  plot(plP2QRS,theta = 225, phi = 30, expand = 0.7, facets = FALSE, scale = TRUE)

## ----lprp2plQRS, eval=F, fig.cap="The line crossing the point $P$ and perpendicular to the plane spanned by $Q$, $R$, and $S$ in 3D space."----
#  P<-c(1,1,1); Q<-c(1,10,4); R<-c(1,1,3); S<-c(3,9,12)
#  
#  cf<-as.numeric(Plane(Q,R,S,1,1)$coeff)
#  a<-cf[1]; b<-cf[2]; c<- -1;
#  
#  vecs<-rbind(Q,c(a,b,c))
#  pts<-rbind(P,Q,R,S)
#  tr<-range(pts,vecs);
#  tf<-(tr[2]-tr[1])*.1 #how far to go at the lower and upper ends in the x-coordinate
#  tsq<-seq(-tf*10-tf,tf*10+tf,l=5)  #try also l=10, 20, or 100
#  
#  pln2pl<-perpline2plane(P,Q,R,S,tsq)
#  pln2pl
#  #> Call:
#  #> perpline2plane(p = P, a = Q, b = R, c = S, t = tsq)
#  #>
#  #> Coefficients of the parameterized line passing through initial point P = (x0,y0,z0) in the direction of normal vector = (A,B,C) (for the form: x=x0 + A*t, y=y0 + B*t, and z=z0 + C*t)
#  #>                   [,1]      [,2] [,3]
#  #> initial point 1.000000 1.0000000    1
#  #> normal vector 4.055556 0.1111111   -1
#  summary(pln2pl)
#  #> Call:
#  #> perpline2plane(p = P, a = Q, b = R, c = S, t = tsq)
#  #>
#  #> Defining Vectors
#  #>                   [,1]      [,2] [,3]
#  #> initial point 1.000000 1.0000000    1
#  #> normal vector 4.055556 0.1111111   -1
#  #>
#  #>  Estimated x points (first row), y points (second row), and z points (third row)
#  #>    that fall on the Line
#  #>       (first 6 or fewer are printed at each row)
#  #> [1] -56.99444 -27.99722   1.00000  29.99722  58.99444
#  #> [1] -0.5888889  0.2055556  1.0000000  1.7944444  2.5888889
#  #> [1]  15.30   8.15   1.00  -6.15 -13.30
#  #>
#  #> Equation of the line crossing point P perpendicular to the plane spanned by points Q, Rand S
#  #>      [,1]
#  #> [1,] "x = 1 + 4.05555555555555t"
#  #> [2,] "y = 1 + 0.111111111111111t"
#  #> [3,] "z = 1-t"
#  #>
#  #> Coefficients of the parameterized line passing through initial point P = (x0,y0,z0) in the direction of normal vector = (A,B,C) (in the form: x = x0 + A*t, y = y0 + B*t, and z = z0 + C*t)
#  #>                   [,1]      [,2] [,3]
#  #> P             1.000000 1.0000000    1
#  #> normal vector 4.055556 0.1111111   -1
#  
#  plot(pln2pl,theta = 225, phi = 30, expand = 0.7, facets = FALSE, scale = TRUE)

## ----EGch8, eval=F------------------------------------------------------------
#  L1<-c(2,4,6); L2<-c(1,3,5);
#  A<-c(1,10,3); B<-c(1,1,3); C<-c(3,9,12)
#  
#  Pint<-intersect.line.plane(L1,L2,A,B,C)
#  Pint
#  #> [1] 1.571429 3.571429 5.571429

## ----EGch9,eval=F-------------------------------------------------------------
#  P<-c(5,2,40)
#  P1<-c(1,2,3); P2<-c(3,9,12); P3<-c(1,1,3);
#  
#  dis<-dist.point2plane(P,P1,P2,P3);
#  dis
#  #> $distance
#  #> [1] 4.121679
#  #>
#  #> $prj.pt2plane
#  #> [1]  9.023529  2.000000 39.105882

