% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CentSim1D.R
\name{IarcCSend.int}
\alias{IarcCSend.int}
\title{The indicator for the presence of an arc from a point to another for
Central Similarity Proximity Catch Digraphs (CS-PCDs) - end-interval case}
\usage{
IarcCSend.int(p1, p2, int, t, rv = NULL)
}
\arguments{
\item{p1}{A 1D point for which the CS proximity region is constructed.}

\item{p2}{A 1D point to check whether it is inside the proximity region or not.}

\item{int}{A \code{vector} of two real numbers representing an interval.}

\item{t}{A positive real number which serves as the expansion parameter in CS proximity region.}

\item{rv}{Index of the end-interval containing the point, either \code{1,2} or \code{NULL} (default=\code{NULL}).}
}
\value{
\eqn{I(p_2} in \eqn{N_{CS}(p_1,t))} for points \eqn{p_1} and \eqn{p_2}, that is, returns 1 if \eqn{p_2} is in \eqn{N_{CS}(p_1,t)}
(i.e., if there is an arc from \eqn{p_1} to \eqn{p_2}), returns 0 otherwise
}
\description{
Returns \eqn{I(p_2} in \eqn{N_{CS}(p_1,t))} for points \eqn{p_1} and \eqn{p_2}, that is, returns 1 if \eqn{p_2} is in \eqn{N_{CS}(p_1,t)}, returns 0
otherwise, where \eqn{N_{CS}(x,t)} is the CS proximity region for point \eqn{x} with expansion parameter \eqn{t>0}
for the region outside the interval \eqn{(a,b)}.

\code{rv} is the index of the end vertex region \eqn{p_1} resides, with default=\code{NULL},
and \code{rv=1} for left end-interval and \code{rv=2} for the right end-interval.
If \eqn{p_1} and \eqn{p_2} are distinct and either of them are inside interval \code{int}, it returns 0,
but if they are identical, then it returns 1 regardless of their locations (i.e., it allows loops).

See also (\insertCite{ceyhan:revstat-2016;textual}{pcds}).
}
\examples{
a<-0; b<-10; int<-c(a,b)
t<-2

IarcCSend.int(15,17,int,t)
IarcCSend.int(15,15,int,t)

IarcCSend.int(1.5,17,int,t)
IarcCSend.int(1.5,1.5,int,t)

IarcCSend.int(-15,17,int,t)

IarcCSend.int(-15,-17,int,t)

a<-0; b<-10; int<-c(a,b)
t<-.5

IarcCSend.int(15,17,int,t)

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{IarcCSmid.int}}, \code{\link{IarcPEmid.int}}, and \code{\link{IarcPEend.int}}
}
\author{
Elvan Ceyhan
}
