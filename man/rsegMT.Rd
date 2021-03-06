% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCDFunctions.r
\name{rsegMT}
\alias{rsegMT}
\title{Generation of points segregated (in a Type I fashion) from a given set of points}
\usage{
rsegMT(n, Yp, delta, DTmesh = NULL, DTr = NULL)
}
\arguments{
\item{n}{A positive integer representing the number of points to be generated.}

\item{Yp}{A set of 2D points from which Delaunay triangulation is constructed.}

\item{delta}{A positive real number in \eqn{(0,4/9)}. \code{delta} is the parameter of segregation (that is, \eqn{\delta 100} %
area around each vertex in each Delaunay triangle is forbidden for point generation).}

\item{DTmesh}{Delaunay triangulation of \code{Yp}, default is \code{NULL}, which is computed via \code{\link[interp]{tri.mesh}} function
in \code{interp} package. \code{\link[interp]{tri.mesh}} function yields the triangulation nodes with their neighbours, and
creates a triangulation object.}

\item{DTr}{Delaunay triangles based on \code{Yp}, default is \code{NULL}, which is computed via \code{\link[interp]{tri.mesh}} function
in \code{interp} package. \code{\link[interp]{triangles}} function yields a triangulation data structure from the triangulation object created
by \code{\link[interp]{tri.mesh}}.}
}
\value{
A \code{list} with the elements
\item{type}{The type of the pattern from which points are to be generated}
\item{mtitle}{The \code{"main"} title for the plot of the point pattern}
\item{parameters}{Exclusion parameter, \code{delta}, of the Type I segregation pattern. \code{delta} is in \eqn{(0,4/9)}
\eqn{\delta 100} \% area around each vertex in each Delaunay triangle is forbidden for point generation.}
\item{ref.points}{The input set of points \code{Yp};
reference points, i.e., points from which generated points are segregated.}
\item{gen.points}{The output set of generated points segregated from \code{Yp} points.}
\item{tri.Y}{Logical output, \code{TRUE} if triangulation based on \code{Yp} points should be implemented.}
\item{desc.pat}{Description of the point pattern}
\item{num.points}{The \code{vector} of two numbers, which are the number of generated points
and the number of reference (i.e., \code{Yp}) points.}
\item{xlimit,ylimit}{The ranges of the \eqn{x}- and \eqn{y}-coordinates of the reference points, which are the
\code{Yp} points}
}
\description{
An object of class \code{"Patterns"}.
Generates \code{n} points uniformly in the support for Type I segregation in the convex hull of
set of points, \code{Yp}.

\code{delta} is the parameter of segregation (that is, \eqn{\delta 100} \% of the area around each vertex
in each Delaunay
triangle is forbidden for point generation). \code{delta} corresponds to \code{eps} in the standard equilateral triangle
\eqn{T_e} as \eqn{delta=4 eps ^2/3} (see \code{rsegTe}  function).

If \code{Yp} consists only of 3 points, then the function behaves like the
function \code{\link{rseg.tri}}.

\code{DTmesh} must be the Delaunay triangulation of \code{Yp}
and \code{DTr} must be the corresponding Delaunay triangles (both \code{DTmesh} and \code{DTr} are \code{NULL} by default).
If \code{NULL}, \code{DTmesh} is computed via \code{\link[interp]{tri.mesh}} and \code{DTr} is computed via \code{\link[interp]{triangles}} function in \code{interp} package.

\code{\link[interp]{tri.mesh}} function yields the triangulation nodes with their neighbours, and creates a triangulation object,
and \code{\link[interp]{triangles}} function yields a triangulation data structure from the triangulation object created
by \code{\link[interp]{tri.mesh}} (the first three columns are the vertex indices of the Delaunay triangles.)

See (\insertCite{ceyhan:arc-density-PE,ceyhan:arc-density-CS,ceyhan:dom-num-NPE-Spat2011;textual}{pcds}) for more on
the segregation pattern.
Also see (\insertCite{okabe:2000,ceyhan:comp-geo-2010,sinclair:2016;textual}{pcds}) for more on Delaunay triangulation and the corresponding algorithm.
}
\examples{
#nx is number of X points (target) and ny is number of Y points (nontarget)
nx<-20; ny<-4;  #try also nx<-1000; ny<-10;

set.seed(1)
Yp<-cbind(runif(ny),runif(ny))
del<-.4

Xdt<-rsegMT(nx,Yp,del)
Xdt
summary(Xdt)
plot(Xdt)

Yp<-cbind(runif(ny),runif(ny))
del<-.3 #try .5, .75, .85
dat<-rsegMT(nx,Yp,del)  #data under CSR in the convex hull of Ypoints

#or use
DTY<-interp::tri.mesh(Yp[,1],Yp[,2],duplicate="remove")  #Delaunay triangulation based on Y points
dat<-rsegMT(nx,Yp,del,DTY)  #data under CSR in the convex hull of Ypoints

#or use
DTY<-interp::tri.mesh(Yp[,1],Yp[,2],duplicate="remove")  #Delaunay triangulation based on Y points
TRY<-interp::triangles(DTY)[,1:3];
dat<-rsegMT(nx,Yp,del,DTr=TRY)  #data under CSR in the convex hull of Ypoints

#or use
DTY<-interp::tri.mesh(Yp[,1],Yp[,2],duplicate="remove")  #Delaunay triangulation based on Y points
TRY<-interp::triangles(DTY)[,1:3];
dat<-rsegMT(nx,Yp,del,DTY,TRY)$gen.points #data under CSR in the convex hull of Ypoints

Xlim<-range(Yp[,1])
Ylim<-range(Yp[,2])
xd<-Xlim[2]-Xlim[1]
yd<-Ylim[2]-Ylim[1]

#plot of the data in the convex hull of Y points together with the Delaunay triangulation
DTY<-interp::tri.mesh(Yp[,1],Yp[,2],duplicate="remove")  #Delaunay triangulation based on Y points

oldpar <- par(no.readonly = TRUE)
par(pty="s")
plot(dat,main=" ", xlab=" ", ylab=" ",xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05),type="n")
interp::plot.triSht(DTY, add=TRUE, do.points=TRUE,col="blue")
points(dat,pch=".",cex=3)
par(oldpar)

Yp<-rbind(c(.3,.2),c(.4,.5),c(.14,.15))
rsegMT(nx,Yp,del)

dat.fr<-data.frame(a=Yp)
rsegMT(nx,dat.fr,del)

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{rseg.disc}}, \code{\link{rsegTe}}, \code{\link{rsegIITe}}, and \code{\link{rascMT}}
}
\author{
Elvan Ceyhan
}
