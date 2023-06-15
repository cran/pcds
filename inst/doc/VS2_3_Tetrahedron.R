## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>",fig.width=6, fig.height=4, fig.align = "center") 

## ----setup, message=FALSE, results='hide'-------------------------------------
library(pcds)

## -----------------------------------------------------------------------------
set.seed(1)
A<-c(0,0,0)+runif(3,-.25,.25);
B<-c(1,0,0)+runif(3,-.25,.25);
C<-c(1/2,sqrt(3)/2,0)+runif(3,-.25,.25);
D<-c(1/2,sqrt(3)/6,sqrt(6)/3)+runif(3,-.25,.25)
tetra<-rbind(A,B,C,D)
n<-5  #try also n<-10 or 20

## -----------------------------------------------------------------------------
Xp<-runif.tetra(n,tetra)$g  #try also Xp[,1]<-Xp[,1]+1

## ----one-th, fig.cap="Scatterplot of the uniform $X$ points in the tetrahedron $T$."----
xlim<-range(tetra[,1],Xp[,1])
ylim<-range(tetra[,2],Xp[,2])
zlim<-range(tetra[,3],Xp[,3])

xr<-xlim[2]-xlim[1]
yr<-ylim[2]-ylim[1]
zr<-zlim[2]-zlim[1]

plot3D::scatter3D(Xp[,1],Xp[,2],Xp[,3], phi=0,theta=-60, bty = "g",main="Points in One Tetrahedron",
                  xlab="x", ylab="y", zlab="z", xlim=xlim+xr*c(-.05,.05), ylim=ylim+yr*c(-.05,.05),
                  zlim=zlim+zr*c(-.05,.05), pch = 20, cex = 1, ticktype = "detailed")
#add the vertices of the tetrahedron
plot3D::points3D(tetra[,1],tetra[,2],tetra[,3], add = TRUE)
A<-tetra[1,]; B<-tetra[2,]; C<-tetra[3,]; D<-tetra[4,]
L<-rbind(A,A,A,B,B,C); R<-rbind(B,C,D,C,D,D)
plot3D::segments3D(L[,1], L[,2], L[,3], R[,1], R[,2],R[,3], add=TRUE,lwd=1,lty=2)
#now we add the vertex names and annotation
txt<-tetra
xc<-txt[,1]+c(-.02,.02,-.15,.02)
yc<-txt[,2]+c(.02,.02,.02,.02)
zc<-txt[,3]+c(-.04,.02,.02,.02)
txt.str<-c("A","B","C","D")
plot3D::text3D(xc,yc,zc,txt.str, add = TRUE)

## ----eval=F-------------------------------------------------------------------
#  M<-"CM"  #try also M<-"CC"
#  r<-1.5
#  NPEtetra(Xp[1,],tetra,r)  #uses the default M="CM"
#  #>            [,1]      [,2]       [,3]
#  #> [1,] 0.72233763 0.9464243 0.06455702
#  #> [2,] 0.06169821 0.1514047 0.04242222
#  #> [3,] 1.10142305 0.0843472 0.17049892
#  #> [4,] 0.37498004 0.3131847 0.52897936

## ----eval=F-------------------------------------------------------------------
#  IarcPEtetra(Xp[1,],Xp[2,],tetra,r)  #uses the default M="CM"
#  #> [1] 1
#  IarcPEtetra(Xp[2,],Xp[1,],tetra,r,M)
#  #> [1] 1

## ----eval=F-------------------------------------------------------------------
#  Narcs = num.arcsPEtetra(Xp,tetra,r,M)
#  summary(Narcs)
#  #> Call:
#  #> num.arcsPEtetra(Xp = Xp, th = tetra, r = r, M = M)
#  #>
#  #> Description of the output:
#  #> Number of Arcs of the PE-PCD with vertices Xp and Quantities Related to the Support Tetrahedron
#  #>
#  #> Number of data (Xp) points in the tetrahedron =  5
#  #> Number of arcs in the digraph =  10
#  #>
#  #> Indices of data points in the tetrahedron:
#  #> 1 2 3 4 5
#  #>
#  #plot(Narcs) #gives error

## ----eval=F-------------------------------------------------------------------
#  PEarc.dens.tetra(Xp,tetra,r,M)
#  #> [1] 0.5

## ----3DPEPR2, fig.cap="PE proximity regions for one of the $X$ points in the tetrahedron $T$ for better visualization."----
plotPEregs.tetra(Xp[1,],tetra,r=1.5)  #uses the default M="CM"

## ----eval=F-------------------------------------------------------------------
#  A<-c(0,0,0); B<-c(1,0,0); C<-c(1/2,sqrt(3)/2,0); D<-c(1/2,sqrt(3)/6,sqrt(6)/3)
#  tetra<-rbind(A,B,C,D)
#  
#  n<-10  #try also n<-20

## ----eval=F-------------------------------------------------------------------
#  Xp<-runif.std.tetra(n)$g

## ----std-reg-th, eval=F, fig.cap="Scatterplot of 10 $X$ points in the standard regular tetrahedron $T_{reg}$."----
#  xlim<-range(tetra[,1],Xp[,1])
#  ylim<-range(tetra[,2],Xp[,2])
#  zlim<-range(tetra[,3],Xp[,3])
#  
#  xr<-xlim[2]-xlim[1]
#  yr<-ylim[2]-ylim[1]
#  zr<-zlim[2]-zlim[1]
#  
#  plot3D::scatter3D(Xp[,1],Xp[,2],Xp[,3], phi=0,theta=-60, bty = "g",main="Points in the Standard Regular Tetrahedron",
#                    xlab="x", ylab="y", zlab="z", xlim=xlim+xr*c(-.05,.05), ylim=ylim+yr*c(-.05,.05),
#                    zlim=zlim+zr*c(-.05,.05), pch = 20, cex = 1, ticktype = "detailed")
#  #add the vertices of the tetrahedron
#  plot3D::points3D(tetra[,1],tetra[,2],tetra[,3], add = TRUE)
#  A<-tetra[1,]; B<-tetra[2,]; C<-tetra[3,]; D<-tetra[4,]
#  L<-rbind(A,A,A,B,B,C); R<-rbind(B,C,D,C,D,D)
#  plot3D::segments3D(L[,1], L[,2], L[,3], R[,1], R[,2],R[,3], add=TRUE,lwd=1,lty=2)
#  
#  #now we add the vertex names and annotation
#  txt<-tetra
#  xc<-txt[,1]+c(-.01,.02,-.12,.02)
#  yc<-txt[,2]+c(.02,.02,.02,-.01)
#  zc<-txt[,3]+c(-.04,.02,.02,.02)
#  txt.str<-c("A","B","C","D")
#  plot3D::text3D(xc,yc,zc,txt.str, add = TRUE)

## ----eval=F-------------------------------------------------------------------
#  r<-1.5
#  NPEstd.tetra(Xp[1,],r)
#  #>           [,1]      [,2]      [,3]
#  #> [1,] 0.5000000 0.8660254 0.0000000
#  #> [2,] 0.1618881 0.2803983 0.0000000
#  #> [3,] 0.5000000 0.4756074 0.5521345
#  #> [4,] 0.8381119 0.2803983 0.0000000
#  NPEstd.tetra(Xp[5,],r)
#  #>            [,1]      [,2]      [,3]
#  #> [1,] 1.00000000 0.0000000 0.0000000
#  #> [2,] 0.52499658 0.8227301 0.0000000
#  #> [3,] 0.52499658 0.2742434 0.7756773
#  #> [4,] 0.04999316 0.0000000 0.0000000

## ----eval=F-------------------------------------------------------------------
#  IarcPEstd.tetra(Xp[1,],Xp[3,],r)  #uses the default M="CM"
#  #> [1] 1

## ----3DPEPRsth1, eval=F, fig.cap="PE proximity regions for the 10 uniform $X$ points in $T_{reg}$ used above."----
#  plotPEregs.std.tetra(Xp,r)

## ----eval=F-------------------------------------------------------------------
#  set.seed(123)
#  A<-c(0,0,0)+runif(3,-.2,.2);
#  B<-c(1,0,0)+runif(3,-.2,.2);
#  C<-c(1/2,sqrt(3)/2,0)+runif(3,-.2,.2);
#  D<-c(1/2,sqrt(3)/6,sqrt(6)/3)+runif(3,-.2,.2);
#  tetra<-rbind(A,B,C,D)
#  
#  CC<-circumcenter.tetra(tetra)
#  CC
#  #> [1] 0.5516851 0.3386671 0.1212977

## ----eval=F-------------------------------------------------------------------
#  n<-10  #try also n<-20
#  Xp<-runif.tetra(n,tetra)$g
#  rel.vert.tetraCC(Xp[1,],tetra)
#  #> $rv
#  #> [1] 2
#  #>
#  #> $tetra
#  #>                 [,1]      [,2]        [,3]
#  #> vertex 1 -0.08496899 0.1153221 -0.03640923
#  #> vertex 2  1.15320696 0.1761869 -0.18177740
#  #> vertex 3  0.51124220 1.0229930  0.02057401
#  #> vertex 4  0.48264589 0.4714085  0.79783024
#  
#  Rv<-vector()
#  for (i in 1:n)
#    Rv<-c(Rv,rel.vert.tetraCC(Xp[i,],tetra)$rv)
#  Rv
#  #>  [1] 2 2 1 3 2 1 2 3 2 1

## ----3DCCVR, eval=F, fig.cap="CC-Vertex regions in the tetrahedron $T=(A,B,C,D)$."----
#  CC<-circumcenter.tetra(tetra)
#  CC
#  
#  Xlim<-range(tetra[,1],Xp[,1],CC[1])
#  Ylim<-range(tetra[,2],Xp[,2],CC[2])
#  Zlim<-range(tetra[,3],Xp[,3],CC[3])
#  xd<-Xlim[2]-Xlim[1]
#  yd<-Ylim[2]-Ylim[1]
#  zd<-Zlim[2]-Zlim[1]
#  
#  plot3D::scatter3D(tetra[,1],tetra[,2],tetra[,3], phi =0,theta=40, bty = "g",
#                    main="Scatterplot of data points with CC-vertex regions",
#                    xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05), zlim=Zlim+zd*c(-.05,.05),
#                    pch = 20, cex = 1, ticktype = "detailed")
#  L<-rbind(A,A,A,B,B,C); R<-rbind(B,C,D,C,D,D)
#  plot3D::segments3D(L[,1], L[,2], L[,3], R[,1], R[,2],R[,3], add=TRUE,lwd=2)
#  #add the data points
#  plot3D::points3D(Xp[,1],Xp[,2],Xp[,3],pch=".",cex=3, add=TRUE)
#  
#  plot3D::text3D(tetra[,1],tetra[,2],tetra[,3], labels=c("A","B","C","D"), add=TRUE)
#  plot3D::text3D(CC[1],CC[2],CC[3], labels=c("CC"), add=TRUE)
#  
#  D1<-(A+B)/2; D2<-(A+C)/2; D3<-(A+D)/2; D4<-(B+C)/2; D5<-(B+D)/2; D6<-(C+D)/2;
#  L<-rbind(D1,D2,D3,D4,D5,D6); R<-matrix(rep(CC,6),ncol=3,byrow=TRUE)
#  plot3D::segments3D(L[,1], L[,2], L[,3], R[,1], R[,2],R[,3], add=TRUE,lty=2)
#  
#  F1<-intersect.line.plane(A,CC,B,C,D)
#  L<-matrix(rep(F1,4),ncol=3,byrow=TRUE); R<-rbind(D4,D5,D6,CC)
#  plot3D::segments3D(L[,1], L[,2], L[,3], R[,1], R[,2],R[,3],col=2, add=TRUE,lty=2)
#  
#  F2<-intersect.line.plane(B,CC,A,C,D)
#  L<-matrix(rep(F2,4),ncol=3,byrow=TRUE); R<-rbind(D2,D3,D6,CC)
#  plot3D::segments3D(L[,1], L[,2], L[,3], R[,1], R[,2],R[,3],col=3, add=TRUE,lty=2)
#  
#  F3<-intersect.line.plane(C,CC,A,B,D)
#  L<-matrix(rep(F3,4),ncol=3,byrow=TRUE); R<-rbind(D3,D5,D6,CC)
#  plot3D::segments3D(L[,1], L[,2], L[,3], R[,1], R[,2],R[,3],col=4, add=TRUE,lty=2)
#  
#  F4<-intersect.line.plane(D,CC,A,B,C)
#  L<-matrix(rep(F4,4),ncol=3,byrow=TRUE); R<-rbind(D1,D2,D4,CC)
#  plot3D::segments3D(L[,1], L[,2], L[,3], R[,1], R[,2],R[,3],col=5, add=TRUE,lty=2)
#  
#  plot3D::text3D(Xp[,1],Xp[,2],Xp[,3], labels=factor(Rv), add=TRUE)

## ----eval=F-------------------------------------------------------------------
#  #The index of the $CM$-vertex region in a tetrahedron that contains a point
#  A<-c(0,0,0); B<-c(1,0,0); C<-c(1/2,sqrt(3)/2,0); D<-c(1/2,sqrt(3)/6,sqrt(6)/3)
#  tetra<-rbind(A,B,C,D)
#  
#  n<-10  #try also n<-20
#  Xp<-runif.std.tetra(n)$g
#  rel.vert.tetraCM(Xp[1,],tetra)
#  #> $rv
#  #> [1] 4
#  #>
#  #> $tetra
#  #>          [,1]      [,2]      [,3]
#  #> vertex 1  0.0 0.0000000 0.0000000
#  #> vertex 2  1.0 0.0000000 0.0000000
#  #> vertex 3  0.5 0.8660254 0.0000000
#  #> vertex 4  0.5 0.2886751 0.8164966
#  
#  Rv<-vector()
#  for (i in 1:n)
#    Rv<-c(Rv, rel.vert.tetraCM(Xp[i,],tetra)$rv )
#  Rv
#  #>  [1] 4 4 3 4 4 1 1 3 3 2

