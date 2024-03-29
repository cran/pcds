% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PropEdge2D.R
\name{NPEbasic.tri}
\alias{NPEbasic.tri}
\title{The vertices of the Proportional Edge (PE) Proximity Region
in a standard basic triangle}
\usage{
NPEbasic.tri(p, r, c1, c2, M = c(1, 1, 1), rv = NULL)
}
\arguments{
\item{p}{A 2D point whose PE proximity region is to be computed.}

\item{r}{A positive real number which serves
as the expansion parameter in PE proximity region;
must be \eqn{\ge 1}.}

\item{c1, c2}{Positive real numbers
representing the top vertex in standard basic triangle
\eqn{T_b=T((0,0),(1,0),(c_1,c_2))},
\eqn{c_1} must be in \eqn{[0,1/2]}, \eqn{c_2>0}
and \eqn{(1-c_1)^2+c_2^2 \le 1}.}

\item{M}{A 2D point in Cartesian coordinates or a 3D point
in barycentric coordinates
which serves as a center
in the interior of the standard basic triangle \eqn{T_b}
or the circumcenter of \eqn{T_b}
which may be entered as "CC" as well;
default is \eqn{M=(1,1,1)}, i.e., the center of mass of \eqn{T_b}.}

\item{rv}{Index of the \code{M}-vertex region
containing the point \code{p}, either \code{1,2,3} or \code{NULL}
(default is \code{NULL}).}
}
\value{
Vertices of the triangular region
which constitutes the PE proximity region with expansion parameter
r and center \code{M} for a point \code{p}
}
\description{
Returns the vertices of the PE proximity region
(which is itself a triangle) for a point in the
standard basic triangle
\eqn{T_b=T((0,0),(1,0),(c_1,c_2))=}\code{(rv=1,rv=2,rv=3)}.

PE proximity region is defined with respect
to the standard basic triangle \eqn{T_b}
with expansion parameter \eqn{r \ge 1}
and vertex regions based on center \eqn{M=(m_1,m_2)} in
Cartesian coordinates or \eqn{M=(\alpha,\beta,\gamma)}
in barycentric coordinates in the interior of the basic
triangle \eqn{T_b} or based on the circumcenter of \eqn{T_b};
default is \eqn{M=(1,1,1)}, i.e., the center of mass of \eqn{T_b}.

Vertex regions are labeled as \eqn{1,2,3} rowwise for the vertices
of the triangle \eqn{T_b}. \code{rv} is the index of the vertex region
\code{p} resides, with default=\code{NULL}.
If \code{p} is outside of \code{tri},
it returns \code{NULL} for the proximity region.

See also (\insertCite{ceyhan:Phd-thesis,ceyhan:comp-geo-2010,ceyhan:mcap2012;textual}{pcds}).
}
\examples{
\donttest{
c1<-.4; c2<-.6
A<-c(0,0); B<-c(1,0); C<-c(c1,c2);
Tb<-rbind(A,B,C);

M<-as.numeric(runif.basic.tri(1,c1,c2)$g)  #try also M<-c(.6,.2)

r<-2

P1<-as.numeric(runif.basic.tri(1,c1,c2)$g)  #try also P1<-c(.4,.2)
NPEbasic.tri(P1,r,c1,c2,M)

#or try
Rv<-rel.vert.basic.tri(P1,c1,c2,M)$rv
NPEbasic.tri(P1,r,c1,c2,M,Rv)

P1<-c(1.4,1.2)
P2<-c(1.5,1.26)
NPEbasic.tri(P1,r,c1,c2,M) #gives an error if M=c(1.3,1.3)
#since center is not the circumcenter or not in the interior of the triangle
}

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{NPEtri}}, \code{\link{NAStri}}, \code{\link{NCStri}},
and \code{\link{IarcPEbasic.tri}}
}
\author{
Elvan Ceyhan
}
