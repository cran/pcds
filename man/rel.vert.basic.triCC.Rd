% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AuxDelaunay.R
\name{rel.vert.basic.triCC}
\alias{rel.vert.basic.triCC}
\title{The index of the \eqn{CC}-vertex region
in a standard basic triangle form that contains a point}
\usage{
rel.vert.basic.triCC(p, c1, c2)
}
\arguments{
\item{p}{A 2D point for which \eqn{CC}-vertex region
it resides in is to be determined in the
standard basic triangle form \eqn{T_b}.}

\item{c1, c2}{Positive real numbers
which constitute the upper vertex of the standard basic triangle form
(i.e., the vertex adjacent to the shorter edges of \eqn{T_b}); \eqn{c_1}
must be in \eqn{[0,1/2]},
\eqn{c_2>0} and \eqn{(1-c_1)^2+c_2^2 \le 1}.}
}
\value{
A \code{list} with two elements
\item{rv}{Index of the \eqn{CC}-vertex region that contains point,
\code{p} in the standard basic triangle form \eqn{T_b}}
\item{tri}{The vertices of the triangle,
where row number corresponds to the vertex index in \code{rv}
with row \eqn{1=(0,0)}, row \eqn{2=(1,0)}, and row \eqn{3=(c_1,c_2)}.}
}
\description{
Returns the index of the vertex
whose region contains point \code{p} in
the standard basic triangle form \eqn{T_b=T((0,0),(1,0),(c_1,c_2))}
where \eqn{c_1} is in \eqn{[0,1/2]}, \eqn{c_2>0}
and \eqn{(1-c_1)^2+c_2^2 \le 1}
and vertex regions are
based on the circumcenter \eqn{CC} of \eqn{T_b}.
(see the plots in the example for illustrations).

The vertices of the standard basic triangle form \eqn{T_b} are labeled as
\eqn{1=(0,0)}, \eqn{2=(1,0)},and \eqn{3=(c_1,c_2)}
also according to the row number the vertex is recorded in \eqn{T_b}.
If the point, \code{p}, is not inside \eqn{T_b},
then the function yields \code{NA} as output.
The corresponding vertex region is the polygon
whose interior points are closest to that vertex.

Any given triangle can be mapped to the standard basic triangle form
by a combination of rigid body motions
(i.e., translation, rotation and reflection) and scaling,
preserving uniformity of the points in the original triangle.
Hence, standard basic triangle form is useful for simulation
studies under the uniformity hypothesis.

See also (\insertCite{ceyhan:Phd-thesis,ceyhan:comp-geo-2010,ceyhan:mcap2012;textual}{pcds}).
}
\examples{
\donttest{
c1<-.4; c2<-.6;  #try also c1<-.5; c2<-.5;

P<-c(.3,.2)
rel.vert.basic.triCC(P,c1,c2)

A<-c(0,0);B<-c(1,0);C<-c(c1,c2);
Tb<-rbind(A,B,C)
CC<-circumcenter.basic.tri(c1,c2)  #the circumcenter
D1<-(B+C)/2; D2<-(A+C)/2; D3<-(A+B)/2;
Ds<-rbind(D1,D2,D3)

Xlim<-range(Tb[,1])
Ylim<-range(Tb[,2])
xd<-Xlim[2]-Xlim[1]
yd<-Ylim[2]-Ylim[1]

plot(Tb,asp=1,xlab="",ylab="",axes=TRUE,pch=".",xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05))
polygon(Tb)
L<-matrix(rep(CC,3),ncol=2,byrow=TRUE); R<-Ds
segments(L[,1], L[,2], R[,1], R[,2], lty = 2)

txt<-rbind(Tb,CC,Ds)
xc<-txt[,1]+c(-.03,.03,0.02,-.01,.05,-.05,.01)
yc<-txt[,2]+c(.02,.02,.03,.06,.03,.03,-.03)
txt.str<-c("A","B","C","CC","D1","D2","D3")
text(xc,yc,txt.str)

RV1<-(A+D3+CC+D2)/4
RV2<-(B+D3+CC+D1)/4
RV3<-(C+D2+CC+D1)/4

txt<-rbind(RV1,RV2,RV3)
xc<-txt[,1]
yc<-txt[,2]
txt.str<-c("rv=1","rv=2","rv=3")
text(xc,yc,txt.str)

n<-20  #try also n<-40
Xp<-runif.basic.tri(n,c1,c2)$g

Rv<-vector()
for (i in 1:n)
  Rv<-c(Rv,rel.vert.basic.triCC(Xp[i,],c1,c2)$rv)
Rv

Xlim<-range(Tb[,1],Xp[,1])
Ylim<-range(Tb[,2],Xp[,2])
xd<-Xlim[2]-Xlim[1]
yd<-Ylim[2]-Ylim[1]

plot(Tb,asp=1,xlab="",pch=".",ylab="",axes=TRUE,xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05))
points(Xp,pch=".")
polygon(Tb)
L<-matrix(rep(CC,3),ncol=2,byrow=TRUE); R<-Ds
segments(L[,1], L[,2], R[,1], R[,2], lty = 2)
text(Xp,labels=factor(Rv))

txt<-rbind(Tb,CC,Ds)
xc<-txt[,1]+c(-.03,.03,0.02,-.01,.05,-.05,.01)
yc<-txt[,2]+c(.02,.02,.03,.06,.03,.03,-.04)
txt.str<-c("A","B","C","CC","D1","D2","D3")
text(xc,yc,txt.str)
}

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{rel.vert.triCM}}, \code{\link{rel.vert.tri}},
\code{\link{rel.vert.triCC}}, \code{\link{rel.vert.basic.triCM}},
\code{\link{rel.vert.basic.tri}}, and \code{\link{rel.vert.std.triCM}}
}
\author{
Elvan Ceyhan
}
