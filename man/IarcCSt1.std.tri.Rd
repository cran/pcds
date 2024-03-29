% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CentSim2D.R
\name{IarcCSt1.std.tri}
\alias{IarcCSt1.std.tri}
\title{The indicator for the presence of an arc from a point to another for Central Similarity Proximity Catch
Digraphs (CS-PCDs) - standard equilateral triangle case with \eqn{t=1}}
\usage{
IarcCSt1.std.tri(p1, p2)
}
\arguments{
\item{p1}{A 2D point whose CS proximity region is constructed.}

\item{p2}{A 2D point. The function determines whether \code{p2} is inside the CS proximity region of
\code{p1} or not.}
}
\value{
\eqn{I(}\code{p2} is in \eqn{N_{CS}(p1,t=1))} for \code{p1} in \eqn{T_e} that is, returns 1 if \code{p2}
is in \eqn{N_{CS}(p1,t=1)}, returns 0 otherwise
}
\description{
Returns \eqn{I(}\code{p2} is in \eqn{N_{CS}(p1,t=1))} for points \code{p1} and \code{p2}, that is, returns 1 if \code{p2} is in \eqn{N_{CS}(p1,t=1)},
returns 0 otherwise, where \eqn{N_{CS}(x,t=1)} is the CS proximity region for point \eqn{x} with expansion parameter \eqn{t=1}.

CS proximity region is defined with respect to the standard equilateral triangle
\eqn{T_e=T(A,B,C)=T((0,0),(1,0),(1/2,\sqrt{3}/2))} and edge regions are based on the center of mass \eqn{CM=(1/2,\sqrt{3}/6)}.

If \code{p1} and \code{p2} are distinct and either are outside \eqn{T_e}, it returns 0,
but if they are identical, then it returns 1 regardless of their locations (i.e., it allows loops).
}
\examples{
\donttest{
A<-c(0,0); B<-c(1,0); C<-c(1/2,sqrt(3)/2);
Te<-rbind(A,B,C);
n<-3

set.seed(1)
Xp<-runif.std.tri(n)$gen.points

IarcCSt1.std.tri(Xp[1,],Xp[2,])
IarcCSt1.std.tri(c(.2,.5),Xp[2,])
}

}
\seealso{
\code{\link{IarcCSstd.tri}}
}
\author{
Elvan Ceyhan
}
