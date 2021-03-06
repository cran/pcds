% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCDFunctions.r
\name{XinCHY}
\alias{XinCHY}
\title{Points from one class inside the convex hull of the points from the other class}
\usage{
XinCHY(Xp, Yp)
}
\arguments{
\item{Xp}{A set of 2D points which constitute the data set.}

\item{Yp}{A set of 2D points which constitute the vertices of the Delaunay triangles.}
}
\value{
\code{Xp} points inside the convex hull of \code{Yp} points
}
\description{
Given two 2D data sets, \code{Xp} and \code{Yp}, it returns the \code{Xp} points inside the convex hull of \code{Yp} points.

See (\insertCite{okabe:2000,ceyhan:comp-geo-2010,sinclair:2016;textual}{pcds}) for more on Delaunay triangulation and the corresponding algorithm.
}
\examples{
#nx is number of X points (target) and ny is number of Y points (nontarget)
nx<-20; ny<-4;  #try also nx<-40; ny<-10 or nx<-1000; ny<-10;

set.seed(1)
Xp<-cbind(runif(nx,0,1),runif(nx,0,1))
Yp<-cbind(runif(ny,0,1),runif(ny,0,1))

DT<-interp::tri.mesh(Yp[,1],Yp[,2],duplicate="remove")

Xlim<-range(Xp[,1],Yp[,1])
Ylim<-range(Xp[,2],Yp[,2])
xd<-Xlim[2]-Xlim[1]
yd<-Ylim[2]-Ylim[1]

Xch<-XinCHY(Xp,Yp)

plot(Xp,main=" ", xlab=" ", ylab=" ",
xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05),pch=".",cex=3)
interp::convex.hull(DT,plot.it = TRUE, add = TRUE)  # or try polygon(Yp[ch$i,])
points(Xch,pch=4,col="red")

XinCHY(Xp,Yp)
XinCHY(Xp,Yp[1:3,])

XinCHY(Xp,rbind(Yp,Yp))

dat.fr<-data.frame(a=Xp)
XinCHY(dat.fr,Yp)

dat.fr<-data.frame(a=Yp)
XinCHY(Xp,dat.fr)

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{plotDeltri}}
}
\author{
Elvan Ceyhan
}
