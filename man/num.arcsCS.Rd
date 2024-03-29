% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CentSim2D.R
\name{num.arcsCS}
\alias{num.arcsCS}
\title{Number of arcs of Central Similarity Proximity Catch Digraphs (CS-PCDs)
and related quantities of the induced subdigraphs for points in the Delaunay triangles -
multiple triangle case}
\usage{
num.arcsCS(Xp, Yp, t, M = c(1, 1, 1))
}
\arguments{
\item{Xp}{A set of 2D points which constitute the vertices of the CS-PCD.}

\item{Yp}{A set of 2D points which constitute the vertices of the Delaunay triangles.}

\item{t}{A positive real number which serves as the expansion parameter in CS proximity region.}

\item{M}{A 3D point in barycentric coordinates which serves as a center in the interior of each Delaunay
triangle, default for \eqn{M=(1,1,1)} which is the center of mass of each triangle.}
}
\value{
A \code{list} with the elements
\item{desc}{A short description of the output: number of arcs
and related quantities for the induced subdigraphs in the Delaunay triangles}
\item{num.arcs}{Total number of arcs in all triangles, i.e., the number of arcs for the entire CS-PCD}
\item{num.in.conv.hull}{Number of \code{Xp} points in the convex hull of \code{Yp} points}
\item{num.in.tris}{The vector of number of \code{Xp} points in the Delaunay triangles based on \code{Yp} points}
\item{weight.vec}{The \code{vector} of the areas of Delaunay triangles based on \code{Yp} points}
\item{tri.num.arcs}{The \code{vector} of the number of arcs of the components of the CS-PCD in the
Delaunay triangles based on \code{Yp} points}
\item{del.tri.ind}{A matrix of indices of vertices of the Delaunay triangles based on \code{Yp} points,
each column corresponds to the vector of indices of the vertices of one triangle.}
\item{data.tri.ind}{A \code{vector} of indices of vertices of the Delaunay triangles in which data points reside,
i.e., column number of \code{del.tri.ind} for each \code{Xp} point.}
\item{tess.points}{Tessellation points, i.e., points on which the tessellation of
the study region is performed,
here, tessellation is the Delaunay triangulation based on \code{Yp} points.}
\item{vertices}{Vertices of the digraph, \code{Xp}.}
}
\description{
An object of class \code{"NumArcs"}.
Returns the number of arcs and various other quantities related to the Delaunay triangles
for Central Similarity Proximity Catch Digraph
(CS-PCD) whose vertices are the data points in \code{Xp}
in the multiple triangle case.

CS proximity regions are defined with respect to the
Delaunay triangles based on \code{Yp} points with expansion parameter \eqn{t>0} and edge regions in each triangle
is based on the center \eqn{M=(\alpha,\beta,\gamma)} in barycentric coordinates in the interior of each
Delaunay triangle or based on circumcenter of each Delaunay triangle (default for \eqn{M=(1,1,1)}
which is the center of mass of the triangle).
Each Delaunay triangle is first converted to an (nonscaled) basic triangle so that \code{M} will be the same
type of center for each Delaunay triangle (this conversion is not necessary when \code{M} is \eqn{CM}).

Convex hull of \code{Yp} is partitioned
by the Delaunay triangles based on \code{Yp} points
(i.e., multiple triangles are the set of these Delaunay triangles
whose union constitutes the
convex hull of \code{Yp} points).
For the number of arcs, loops are not allowed so arcs are only possible
for points inside the convex hull of \code{Yp} points.

See (\insertCite{ceyhan:Phd-thesis,ceyhan:arc-density-CS,ceyhan:test2014;textual}{pcds}) for more on CS-PCDs.
Also see (\insertCite{okabe:2000,ceyhan:comp-geo-2010,sinclair:2016;textual}{pcds}) for more on Delaunay triangulation and the corresponding algorithm.
}
\examples{
\donttest{
#nx is number of X points (target) and ny is number of Y points (nontarget)
nx<-20; ny<-5;  #try also nx<-40; ny<-10 or nx<-1000; ny<-10;

set.seed(1)
Xp<-cbind(runif(nx),runif(nx))
Yp<-cbind(runif(ny,0,.25),runif(ny,0,.25))+cbind(c(0,0,0.5,1,1),c(0,1,.5,0,1))
#try also Yp<-cbind(runif(ny,0,1),runif(ny,0,1))

M<-c(1,1,1)  #try also M<-c(1,2,3)

Narcs = num.arcsCS(Xp,Yp,t=1,M)
Narcs
summary(Narcs)
plot(Narcs)
}

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{num.arcsCStri}}, \code{\link{num.arcsCSstd.tri}}, \code{\link{num.arcsPE}},
and \code{\link{num.arcsAS}}
}
\author{
Elvan Ceyhan
}
