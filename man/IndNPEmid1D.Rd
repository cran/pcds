% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCDFunctions.r
\name{IndNPEmid1D}
\alias{IndNPEmid1D}
\title{The indicator for the presence of an arc from a point to another for Proportional Edge
Proximity Catch Digraphs (PE-PCDs) - middle interval case}
\usage{
IndNPEmid1D(x1, x2, r, c, int, rv = NULL)
}
\arguments{
\item{x1, x2}{1D points; \eqn{x_1} is the point for which the proximity region, \eqn{N_{PE}(x_1,r,c)} is
constructed and \eqn{x_2} is the point which the function is checking whether its inside
\eqn{N_{PE}(x_1,r,c)} or not.}

\item{r}{A positive real number which serves as the expansion parameter in PE proximity region;
must be \eqn{\ge 1}.}

\item{c}{A positive real number in \eqn{(0,1)} parameterizing the center inside \code{int}\eqn{=(a,b)}.
For the interval, \code{int}\eqn{=(a,b)}, the parameterized center is \eqn{M_c=a+c(b-a)}.}

\item{int}{A \code{vector} of two real numbers representing an interval.}

\item{rv}{The index of the vertex region \eqn{x_1} resides, with default=\code{NULL}.}
}
\value{
\eqn{I(x_2 \in N_{PE}(x_1,r,c))} for points \eqn{x_1} and \eqn{x_2} that is, returns 1 if \eqn{x_2} is in \eqn{N_{PE}(x_1,r,c)},
returns 0 otherwise
}
\description{
Returns \eqn{I(x_2 \in N_{PE}(x_1,r,c))} for points \eqn{x_1} and \eqn{x_2}, that is, returns 1 if \eqn{x_2} is in \eqn{N_{PE}(x_1,r,c)}, returns 0
otherwise, where \eqn{N_{PE}(x,r,c)} is the PE proximity region for point \eqn{x} and is constructed with expansion
parameter \eqn{r \ge 1} and centrality parameter \eqn{c \in (0,1)} for the interval \eqn{(a,b)}.

PE proximity regions are defined with respect to the middle interval \code{int} and vertex regions are based
on the center associated with the centrality parameter \eqn{c \in (0,1)}. For the interval, \code{int}\eqn{=(a,b)}, the
parameterized center is \eqn{M_c=a+c(b-a)}. \code{rv} is the index of the vertex region \eqn{x_1} resides, with default=\code{NULL}.
If \eqn{x_1} and \eqn{x_2} are distinct and either of them are outside interval \code{int}, it returns 0,
but if they are identical, then it returns 1 regardless of their locations
(i.e., loops are allowed in the digraph).

See also (\insertCite{ceyhan:metrika-2012,ceyhan:revstat-2016;textual}{pcds}).
}
\examples{
c<-.4
r<-2
a<-0; b<-10; int<-c(a,b)

IndNPEmid1D(7,5,r,c,int)
IndNPEmid1D(17,17,r,c,int)
IndNPEmid1D(1,3,r,c,int)

IndNPEmid1D(3,5,r,c,int)
IndNPEmid1D(3,3,r,c,int)
IndNPEmid1D(4,5,r,c,int)
IndNPEmid1D(a,5,r,c,int)

c<-.4
r<-2
a<-0; b<-10; int<-c(a,b)
IndNPEmid1D(7,5,r,c,int)

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{IndNPEend1D}}, \code{\link{IndNCSmid1D}}, and \code{\link{IndNCSend1D}}
}
\author{
Elvan Ceyhan
}
