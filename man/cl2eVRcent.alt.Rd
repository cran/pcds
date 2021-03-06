% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCDFunctions.r
\name{cl2eVRcent.alt}
\alias{cl2eVRcent.alt}
\title{An alternative function to the function \code{\link{cl2eVRcent}} which finds the closest points
among a data set in the vertex regions to the respective edges in a triangle}
\usage{
cl2eVRcent.alt(dat, tri, M)
}
\arguments{
\item{dat}{A set of 2D points representing the set of data points.}

\item{tri}{Three 2D points, stacked row-wise, each row representing a vertex of the triangle.}

\item{M}{A 2D point in Cartesian coordinates or a 3D point in barycentric coordinates
which serves as a center in the interior of the triangle \code{tri} or the circumcenter of \code{tri}.}
}
\value{
A \code{list} with the elements
\item{txt1}{Vertex labels are \eqn{A=1}, \eqn{B=2}, and \eqn{C=3} (corresponds to row number in Extremum Points).}
\item{txt2}{A short description of the distances as \code{"Distances to Edges in the Respective \eqn{M}-Vertex Regions"}.}
\item{type}{Type of the extrema points}
\item{desc}{A short description of the extrema points}
\item{mtitle}{The \code{"main"} title for the plot of the extrema}
\item{ext}{The extrema points, here, closest points to edges in the respective vertex region.}
\item{X}{The input data, \code{Dt}, can be a \code{matrix} or \code{data frame}}
\item{num.points}{The number of data points, i.e., size of \code{Dt}}
\item{supp}{Support of the data points, here, it is \code{tri}}
\item{cent}{The center point used for construction of vertex regions}
\item{ncent}{Name of the center, \code{cent}, it is \code{"M"} or \code{"CC"} for this function}
\item{regions}{Vertex regions inside the triangle, \code{tri}, provided as a list}
\item{region.names}{Names of the vertex regions as \code{"vr=1"}, \code{"vr=2"}, and \code{"vr=3"}}
\item{region.centers}{Centers of mass of the vertex regions inside \code{tri}}
\item{dist2ref}{Distances of closest points in the \code{M}-vertex regions to corresponding edges.}
}
\description{
An object of class \code{"Extrema"}.
An alternative function to the function \code{\link{cl2eVRcent}}
}
\examples{
A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
Tr<-rbind(A,B,C);
M<-c(1.6,1.0)  #try also M<-c(1.3,1.3)

n<-20
set.seed(1)
dat<-runif.tri(n,Tr)$g

Ext<-cl2eVRcent.alt(dat,Tr,M)
Ext
summary(Ext)
plot(Ext)

cl2eVRcent.alt(dat[1,],Tr,M)

cl2e<-cl2eVRcent.alt(dat,Tr,M)
cl2e

Ds<-cp2e.tri(Tr,M)

Xlim<-range(Tr[,1])
Ylim<-range(Tr[,2])
xd<-Xlim[2]-Xlim[1]
yd<-Ylim[2]-Ylim[1]

plot(Tr,pch=".",xlab="",ylab="",axes=TRUE,
xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05))
polygon(Tr)
points(dat,pch=1,col=1)
L<-rbind(M,M,M); R<-Ds
segments(L[,1], L[,2], R[,1], R[,2], lty=2)
points(cl2e$ext,pch=3,col=2)

xc<-Tr[,1]+c(-.02,.03,.02)
yc<-Tr[,2]+c(.02,.02,.04)
txt.str<-c("A","B","C")
text(xc,yc,txt.str)

txt<-rbind(M,Ds)
xc<-txt[,1]+c(.02,.04,-.03,0)
yc<-txt[,2]+c(.07,.04,.06,-.08)
txt.str<-c("M","D1","D2","D3")
text(xc,yc,txt.str)

dat.fr<-data.frame(a=dat)
cl2eVRcent.alt(dat.fr,Tr,M)

dat.fr<-data.frame(a=Tr)
cl2eVRcent.alt(dat,dat.fr,M)

}
\seealso{
\code{\link{cl2eVRcent}}
}
\author{
Elvan Ceyhan
}
