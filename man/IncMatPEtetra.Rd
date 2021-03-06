% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCDFunctions.r
\name{IncMatPEtetra}
\alias{IncMatPEtetra}
\title{Incidence matrix for Proportional Edge Proximity Catch Digraphs (PE-PCDs) - one tetrahedron case}
\usage{
IncMatPEtetra(dat, th, r, M = "CM")
}
\arguments{
\item{dat}{A set of 3D points which constitute the vertices of PE-PCD.}

\item{th}{Four 3D points, stacked row-wise, each row representing a vertex of the tetrahedron.}

\item{r}{A positive real number which serves as the expansion parameter in PE proximity region;
must be \eqn{\ge 1}.}

\item{M}{The center to be used in the construction of the vertex regions in the tetrahedron, \code{th}.
Currently it only takes \code{"CC"} for circumcenter and \code{"CM"} for center of mass; default=\code{"CM"}.}
}
\value{
Incidence matrix for the PE-PCD with vertices being 3D data set, \code{dat},
in the tetrahedron \code{th} with vertex regions based on circumcenter or center of mass
}
\description{
Returns the incidence matrix for the PE-PCD whose vertices are the given 3D numerical data set, \code{dat},
in the tetrahedron \eqn{th=T(v=1,v=2,v=3,v=4)}.

PE proximity regions are constructed with respect to tetrahedron
\code{th} with expansion parameter \eqn{r \ge 1} and vertex regions are based on the center \code{M} which is circumcenter (\code{"CC"})
or center of mass (\code{"CM"}) of \code{th} with default=\code{"CM"}.
Loops are allowed, so the diagonal entries are all equal to 1.

See also (\insertCite{ceyhan:Phd-thesis,ceyhan:comp-geo-2010;textual}{pcds}).
}
\examples{
\donttest{
A<-c(0,0,0); B<-c(1,0,0); C<-c(1/2,sqrt(3)/2,0); D<-c(1/2,sqrt(3)/6,sqrt(6)/3)
tetra<-rbind(A,B,C,D)
n<-10

dat<-runif.tetra(n,tetra)$g  #try also dat<-c(.5,.5,.5)

M<-"CM"  #try also M<-"CC"
r<-1.5

IM<-IncMatPEtetra(dat,tetra,r=1.25)  #uses the default M="CM"
IM<-IncMatPEtetra(dat,tetra,r=1.25,M)
IM
dom.greedy(IM)
IndUBdom(IM,3)
dom.exact(IM)  #this might take a long time for large n

IncMatPEtetra(dat,tetra,r=1.5)
IncMatPEtetra(dat,tetra,r=2)

r<-2
IncMatPEtetra(dat,tetra,r,M)

dat.fr<-data.frame(a=dat)
IncMatPEtetra(dat.fr,tetra,r,M)

dat.fr<-data.frame(a=tetra)
IncMatPEtetra(dat,dat.fr,r,M)
}

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{IncMatPEtri}}, \code{\link{IncMatPE1D}}, and  \code{\link{IncMatPEMT}}
}
\author{
Elvan Ceyhan
}
