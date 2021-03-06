% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCDFunctions.r
\name{ArcsPEMT}
\alias{ArcsPEMT}
\title{The arcs of Proportional Edge Proximity Catch Digraph (PE-PCD) for 2D data - multiple triangle case}
\usage{
ArcsPEMT(Xp, Yp, r, M = c(1, 1, 1))
}
\arguments{
\item{Xp}{A set of 2D points which constitute the vertices of the PE-PCD.}

\item{Yp}{A set of 2D points which constitute the vertices of the Delaunay triangles.}

\item{r}{A positive real number which serves as the expansion parameter in PE proximity region;
must be \eqn{\ge 1}.}

\item{M}{A 3D point in barycentric coordinates which serves as a center in the interior of each Delaunay
triangle or circumcenter of each Delaunay triangle (for this argument should be set as \code{M="CC"}),
default for \eqn{M=(1,1,1)} which is the center of mass of each triangle.}
}
\value{
A \code{list} with the elements
\item{type}{A description of the type of the digraph}
\item{parameters}{Parameters of the digraph, here, it is the center used to construct the vertex regions.}
\item{tess.points}{Points on which the tessellation of the study region is performed, here, tessellation
is Delaunay triangulation based on \code{Yp} points.}
\item{tess.name}{Name of data set used in tessellation, it is \code{Yp} for this function}
\item{vertices}{Vertices of the digraph, \code{Xp} points}
\item{vert.name}{Name of the data set which constitute the vertices of the digraph}
\item{S}{Tails (or sources) of the arcs of PE-PCD for 2D data set \code{Xp} as vertices of the digraph}
\item{E}{Heads (or arrow ends) of the arcs of PE-PCD for 2D data set \code{Xp} as vertices of the digraph}
\item{mtitle}{Text for \code{"main"} title in the plot of the digraph}
\item{quant}{Various quantities for the digraph: number of vertices, number of partition points,
number of intervals, number of arcs, and arc density.}
}
\description{
An object of class \code{"PCDs"}.
Returns arcs as tails (or sources) and heads (or arrow ends) of Proportional Edge Proximity Catch Digraph
(PE-PCD) whose vertices are the data points in \code{Xp} in the multiple triangle case.

PE proximity regions are
defined with respect to the Delaunay triangles based on \code{Yp} points with expansion parameter \eqn{r \ge 1} and
vertex regions in each triangle are based on the center \eqn{M=(\alpha,\beta,\gamma)} in barycentric coordinates
in the interior of each Delaunay triangle or based on circumcenter of each Delaunay triangle
(default for \eqn{M=(1,1,1)} which is the center of mass of the triangle).
Each Delaunay triangle is first converted to an (unscaled) basic triangle so that \code{M} will be the same
type of center for each Delaunay triangle (this conversion is not necessary when \code{M} is \eqn{CM}).

Convex hull of \code{Yp} is partitioned by the Delaunay triangles based on \code{Yp} points
(i.e., multiple triangles are the set of these Delaunay triangles whose union constitutes the
convex hull of \code{Yp} points). For the number of arcs, loops are not allowed so arcs are only possible
for points inside the convex hull of \code{Yp} points.

See (\insertCite{ceyhan:Phd-thesis,ceyhan:arc-density-PE,ceyhan:dom-num-NPE-Spat2011;textual}{pcds})
for more on the PE-PCDs.
Also see (\insertCite{okabe:2000,ceyhan:comp-geo-2010,sinclair:2016;textual}{pcds}) for more on Delaunay triangulation and the corresponding algorithm.
}
\examples{
#nx is number of X points (target) and ny is number of Y points (nontarget)
nx<-20; ny<-4;  #try also nx<-40; ny<-10 or nx<-1000; ny<-10;

set.seed(1)
Xp<-cbind(runif(nx,0,1),runif(nx,0,1))
Yp<-cbind(runif(ny,0,1),runif(ny,0,1))

M<-c(1,1,1)  #try also M<-c(1,2,3)

r<-1.5  #try also r<-2

ArcsPEMT(Xp,Yp,r,M)

Arcs<-ArcsPEMT(Xp,Yp,r,M)
Arcs
summary(Arcs)
plot(Arcs)

S<-Arcs$S
E<-Arcs$E
DT<-interp::tri.mesh(Yp[,1],Yp[,2],duplicate="remove")

Xlim<-range(Xp[,1],Yp[,1])
Ylim<-range(Xp[,2],Yp[,2])
xd<-Xlim[2]-Xlim[1]
yd<-Ylim[2]-Ylim[1]

plot(Xp,main=" ", xlab=" ", ylab=" ",
xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05),pch=".",cex=3)
interp::plot.triSht(DT, add=TRUE, do.points = TRUE)
arrows(S[,1], S[,2], E[,1], E[,2], length = 0.1, col= 4)

ArcsPEMT(Xp,Yp,r)
ArcsPEMT(Xp,Yp[1:3,],r)

ArcsPEMT(Xp,rbind(Yp,Yp),r)

dat.fr<-data.frame(a=Xp)
ArcsPEMT(dat.fr,Yp,r)

dat.fr<-data.frame(a=Yp)
ArcsPEMT(Xp,dat.fr,r)

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{ArcsPEtri}}, \code{\link{ArcsASMT}} and \code{\link{ArcsCSMT}}
}
\author{
Elvan Ceyhan
}
