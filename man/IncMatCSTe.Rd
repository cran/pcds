% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCDFunctions.r
\name{IncMatCSTe}
\alias{IncMatCSTe}
\title{Incidence matrix for Central Similarity Proximity Catch Digraphs (CS-PCDs) - standard
equilateral triangle case}
\usage{
IncMatCSTe(dat, t, M = c(1, 1, 1))
}
\arguments{
\item{dat}{A set of 2D points which constitute the vertices of the CS-PCD.}

\item{t}{A positive real number which serves as the expansion parameter in CS proximity region.}

\item{M}{A 2D point in Cartesian coordinates or a 3D point in barycentric coordinates.
which serves as a center in the interior of the standard equilateral triangle \eqn{T_e}; default is \eqn{M=(1,1,1)} i.e.
the center of mass of \eqn{T_e}.}
}
\value{
Incidence matrix for the CS-PCD with vertices being 2D data set, \code{dat} and CS proximity
regions are defined in the standard equilateral triangle \eqn{T_e} with \code{M}-edge regions.
}
\description{
Returns the incidence matrix for the CS-PCD whose vertices are the given 2D numerical data set, \code{dat},
in the standard equilateral triangle \eqn{T_e=T(v=1,v=2,v=3)=T((0,0),(1,0),(1/2,\sqrt{3}/2))}.

CS proximity region is defined with respect to the standard equilateral triangle
\eqn{T_e=T(v=1,v=2,v=3)=T((0,0),(1,0),(1/2,\sqrt{3}/2))} and edge regions are based on the center \eqn{M=(m_1,m_2)}
in Cartesian coordinates or \eqn{M=(\alpha,\beta,\gamma)} in barycentric coordinates in the interior of \eqn{T_e};
default is \eqn{M=(1,1,1)} i.e., the center of mass of \eqn{T_e}.
Loops are allowed, so the diagonal entries are all equal to 1.

See also (\insertCite{ceyhan:Phd-thesis,ceyhan:arc-density-CS,ceyhan:test2014;textual}{pcds}).
}
\examples{
A<-c(0,0); B<-c(1,0); C<-c(1/2,sqrt(3)/2);
Te<-rbind(A,B,C);
n<-10

set.seed(1)
dat<-runifTe(n)$gen.points

M<-as.numeric(runifTe(1)$g)  #try also M<-c(.6,.2)

NumArcsCSTe(dat,t=1.25)

inc.mat<-IncMatCSTe(dat,t=1.25,M)
inc.mat
sum(inc.mat)-n

dom.greedy(inc.mat)
\donttest{
dom.exact(inc.mat)  #might take a long time for large n
}
IndUBdom(inc.mat,1)

inc.mat<-IncMatCSTe(rbind(dat,c(0,1)),t=1.25,M)
inc.mat
sum(inc.mat)-(n+1)

IncMatCSTe(dat,t=1.5,M);

IncMatCSTe(rbind(dat,dat),t=1.5,M)

dat.fr<-data.frame(a=dat)
IncMatCSTe(dat.fr,t=1.5,M);

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{IncMatCStri}}, \code{\link{IncMatCSMT}} and \code{\link{IncMatPETe}}
}
\author{
Elvan Ceyhan
}
