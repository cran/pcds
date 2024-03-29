% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PropEdge2D.R
\name{NPEtri}
\alias{NPEtri}
\title{The vertices of the Proportional Edge (PE) Proximity Region
in a general triangle}
\usage{
NPEtri(p, tri, r, M = c(1, 1, 1), rv = NULL)
}
\arguments{
\item{p}{A 2D point whose PE proximity region is to be computed.}

\item{tri}{A \eqn{3 \times 2} matrix with each row
representing a vertex of the triangle.}

\item{r}{A positive real number which serves
as the expansion parameter in PE proximity region;
must be \eqn{\ge 1}.}

\item{M}{A 2D point in Cartesian coordinates
or a 3D point in barycentric coordinates
which serves as a center in the interior of the triangle \code{tri}
or the circumcenter of \code{tri}
which may be entered as "CC" as well;
default is \eqn{M=(1,1,1)}, i.e., the center of mass of \code{tri}.}

\item{rv}{Index of the \code{M}-vertex region
containing the point \code{p}, either \code{1,2,3} or \code{NULL}
(default is \code{NULL}).}
}
\value{
Vertices of the triangular region
which constitutes the PE proximity region with expansion parameter
\code{r} and center \code{M} for a point \code{p}
}
\description{
Returns the vertices of the PE proximity region
(which is itself a triangle) for a point in the
triangle \code{tri}\eqn{=T(A,B,C)=}\code{(rv=1,rv=2,rv=3)}.

PE proximity region is defined with respect to the triangle \code{tri}
with expansion parameter \eqn{r \ge 1}
and vertex regions based on center \eqn{M=(m_1,m_2)}
in Cartesian coordinates or
\eqn{M=(\alpha,\beta,\gamma)} in barycentric coordinates
in the interior of the triangle \code{tri}
or based on the circumcenter of \code{tri};
default is \eqn{M=(1,1,1)}, i.e.,
the center of mass of \code{tri}.

Vertex regions are labeled as \eqn{1,2,3}
rowwise for the vertices
of the triangle \code{tri}.
\code{rv} is the index of the vertex region \code{p} resides,
with default=\code{NULL}.
If \code{p} is outside of \code{tri},
it returns \code{NULL} for the proximity region.

See also (\insertCite{ceyhan:Phd-thesis,ceyhan:arc-density-PE,ceyhan:dom-num-NPE-Spat2011;textual}{pcds}).
}
\examples{
\donttest{
A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
Tr<-rbind(A,B,C);

M<-as.numeric(runif.tri(1,Tr)$g)  #try also M<-c(1.6,1.0)

r<-1.5

n<-3
set.seed(1)
Xp<-runif.tri(n,Tr)$g

NPEtri(Xp[3,],Tr,r,M)

P1<-as.numeric(runif.tri(1,Tr)$g)  #try also P1<-c(.4,.2)
NPEtri(P1,Tr,r,M)

M<-c(1.3,1.3)
r<-2

P1<-c(1.4,1.2)
P2<-c(1.5,1.26)
NPEtri(P1,Tr,r,M)
NPEtri(P2,Tr,r,M)

#or try
Rv<-rel.vert.tri(P1,Tr,M)$rv
NPEtri(P1,Tr,r,M,Rv)
}

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{NPEbasic.tri}}, \code{\link{NAStri}},
\code{\link{NCStri}}, and \code{\link{IarcPEtri}}
}
\author{
Elvan Ceyhan
}
