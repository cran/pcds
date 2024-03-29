% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PropEdge2D.R
\name{PEarc.dens.tri}
\alias{PEarc.dens.tri}
\title{Arc density of Proportional Edge Proximity Catch Digraphs
(PE-PCDs) - one triangle case}
\usage{
PEarc.dens.tri(Xp, tri, r, M = c(1, 1, 1), in.tri.only = FALSE)
}
\arguments{
\item{Xp}{A set of 2D points
which constitute the vertices of the PE-PCD.}

\item{tri}{A \eqn{3 \times 2} matrix with each row
representing a vertex of the triangle.}

\item{r}{A positive real number
which serves as the expansion parameter in PE proximity region;
must be \eqn{\ge 1}.}

\item{M}{A 2D point in Cartesian coordinates
or a 3D point in barycentric coordinates
which serves as a center in the interior of the triangle \code{tri}
or the circumcenter of \code{tri}
which may be entered as "CC" as well;
default is \eqn{M=(1,1,1)}, i.e., the center of mass of \code{tri}.}

\item{in.tri.only}{A logical argument (default is \code{in.tri.only=FALSE})
for computing the arc density for only the points inside the triangle, \code{tri}.
That is,
if \code{in.tri.only=TRUE} arc density of the induced subdigraph with the vertices
inside \code{tri} is computed, otherwise
otherwise arc density of the entire digraph (i.e., digraph with all the vertices) is computed.}
}
\value{
A \code{list} with the elements
\item{arc.dens}{Arc density of PE-PCD
whose vertices are the 2D numerical data set, \code{Xp};
PE proximity regions are defined
with respect to the triangle \code{tri} and \code{M}-vertex regions}
\item{std.arc.dens}{Arc density standardized
by the mean and asymptotic variance of the arc
density of PE-PCD for uniform data in the triangle \code{tri}.
This will only be returned, if \code{M} is the center of mass.}
}
\description{
Returns the arc density of PE-PCD
whose vertex set is the given 2D numerical data set, \code{Xp},
(some of its members are) in the triangle \code{tri}.

PE proximity regions is defined with respect to \code{tri} with
expansion parameter \eqn{r \ge 1} and vertex regions are
based on center \eqn{M=(m_1,m_2)} in Cartesian coordinates or
\eqn{M=(\alpha,\beta,\gamma)} in barycentric coordinates
in the interior of the triangle \code{tri} or based on
circumcenter of \code{tri}; default is \eqn{M=(1,1,1)}, i.e.,
the center of mass of \code{tri}.
The function also provides arc density standardized
by the mean and asymptotic variance of the arc density
of PE-PCD for uniform data in the triangle \code{tri}
only when \code{M} is the center of mass.
For the number of arcs, loops are not allowed.

\code{in.tri.only} is a logical argument (default is \code{FALSE}) for considering only the points
inside the triangle or all the points as the vertices of the digraph.
if \code{in.tri.only=TRUE}, arc density is computed only for
the points inside the triangle (i.e., arc density of the subdigraph
induced by the vertices in the triangle is computed),
otherwise arc density of the entire digraph (i.e., digraph with all the vertices) is computed.

See also (\insertCite{ceyhan:Phd-thesis,ceyhan:arc-density-PE;textual}{pcds}).
}
\examples{
\donttest{
A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
Tr<-rbind(A,B,C);
n<-10  #try also n<-20

set.seed(1)
Xp<-runif.tri(n,Tr)$g

M<-as.numeric(runif.tri(1,Tr)$g)  #try also M<-c(1.6,1.0)

num.arcsPEtri(Xp,Tr,r=1.5,M)
PEarc.dens.tri(Xp,Tr,r=1.5,M)
PEarc.dens.tri(Xp,Tr,r=1.5,M,in.tri.only = TRUE)
}

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{ASarc.dens.tri}}, \code{\link{CSarc.dens.tri}},
and \code{\link{num.arcsPEtri}}
}
\author{
Elvan Ceyhan
}
