% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CentSim2D.R
\name{IarcCSedge.reg.std.tri}
\alias{IarcCSedge.reg.std.tri}
\title{The indicator for the presence of an arc from a point to another for Central Similarity Proximity Catch
Digraphs (CS-PCDs) - standard equilateral triangle case}
\usage{
IarcCSedge.reg.std.tri(p1, p2, t, M = c(1, 1, 1), re = NULL)
}
\arguments{
\item{p1}{A 2D point whose CS proximity region is constructed.}

\item{p2}{A 2D point. The function determines whether \code{p2} is inside the CS proximity region of
\code{p1} or not.}

\item{t}{A positive real number which serves as the expansion parameter in CS proximity region.}

\item{M}{A 2D point in Cartesian coordinates or a 3D point in barycentric coordinates
which serves as a center in the interior of the standard equilateral triangle \eqn{T_e}; default is \eqn{M=(1,1,1)} i.e.
the center of mass of \eqn{T_e}.}

\item{re}{The index of the edge region in \eqn{T_e} containing the point, either \code{1,2,3} or \code{NULL}
(default is \code{NULL}).}
}
\value{
\eqn{I(}\code{p2} is in \eqn{N_{CS}(p1,t))} for \code{p1},
that is, returns 1 if \code{p2} is in \eqn{N_{CS}(p1,t)}, returns 0 otherwise
}
\description{
Returns \eqn{I(}\code{p2} is in \eqn{N_{CS}(p1,t))} for points \code{p1} and \code{p2},
that is, returns 1 if \code{p2} is in \eqn{N_{CS}(p1,t)},
returns 0 otherwise, where \eqn{N_{CS}(x,t)} is the CS proximity region for point \eqn{x} with expansion parameter \eqn{t>0}.
This function is equivalent to \code{IarcCSstd.tri}, except that it computes the indicator using the functions
\code{IarcCSstd.triRAB}, \code{IarcCSstd.triRBC} and \code{IarcCSstd.triRAC} which are edge-region specific indicator functions.
For example,
\code{IarcCSstd.triRAB} computes \eqn{I(}\code{p2} is in \eqn{N_{CS}(p1,t))} for points \code{p1} and \code{p2} when \code{p1}
resides in the edge region of edge \eqn{AB}.

CS proximity region is defined with respect to the standard equilateral triangle
\eqn{T_e=T(v=1,v=2,v=3)=T((0,0),(1,0),(1/2,\sqrt{3}/2))} and edge regions are based on the center \eqn{M=(m_1,m_2)}
in Cartesian coordinates or \eqn{M=(\alpha,\beta,\gamma)} in barycentric coordinates in the interior of \eqn{T_e};
default is \eqn{M=(1,1,1)} i.e., the center of mass of \eqn{T_e}.
\code{re} is the index of the edge region \code{p1} resides, with default=\code{NULL}.

If \code{p1} and \code{p2} are distinct and either of them are outside \eqn{T_e}, it returns 0,
but if they are identical, then it returns 1 regardless of their locations (i.e., it allows loops).

See also (\insertCite{ceyhan:Phd-thesis,ceyhan:arc-density-CS,ceyhan:test2014;textual}{pcds}).
}
\examples{
\donttest{
A<-c(0,0); B<-c(1,0); C<-c(1/2,sqrt(3)/2);
Te<-rbind(A,B,C);
n<-3

set.seed(1)
Xp<-runif.std.tri(n)$gen.points

M<-as.numeric(runif.std.tri(1)$g)  #try also M<-c(.6,.2)

t<-1
IarcCSedge.reg.std.tri(Xp[1,],Xp[2,],t,M)
IarcCSstd.tri(Xp[1,],Xp[2,],t,M)

#or try
re<-rel.edge.std.triCM(Xp[1,])$re
IarcCSedge.reg.std.tri(Xp[1,],Xp[2,],t,M,re=re)
}

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{IarcCStri}} and \code{\link{IarcPEstd.tri}}
}
\author{
Elvan Ceyhan
}
