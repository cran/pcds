% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCDFunctions.r
\name{IndNPEend1D}
\alias{IndNPEend1D}
\title{The indicator for the presence of an arc from a point to another for
Proportional Edge Proximity Catch Digraphs (PE-PCDs) - end interval case}
\usage{
IndNPEend1D(x1, x2, r, int, rv = NULL)
}
\arguments{
\item{x1}{A 1D point whose PE proximity region is constructed.}

\item{x2}{A 1D point. The function determines whether \eqn{x_2} is inside the PE proximity region of
\eqn{x_1} or not.}

\item{r}{A positive real number which serves as the expansion parameter in PE proximity region;
must be \eqn{\ge 1}.}

\item{int}{A \code{vector} of two real numbers representing an interval.}

\item{rv}{Index of the end interval containing the point, either \code{1,2} or \code{NULL} (default is \code{NULL}).}
}
\value{
\eqn{I(x_2 \in N_{PE}(x_1,r))} for points \eqn{x_1} and \eqn{x_2}, that is, returns 1 if \eqn{x_2} is in \eqn{N_{PE}(x_1,r)}
(i.e., if there is an arc from \eqn{x_1} to \eqn{x_2}), returns 0 otherwise
}
\description{
Returns \eqn{I(x_2 \in N_{PE}(x_1,r))} for points \eqn{x_1} and \eqn{x_2}, that is, returns 1 if \eqn{x_2} is in \eqn{N_{PE}(x_1,r)}, returns 0
otherwise, where \eqn{N_{PE}(x,r)} is the PE proximity region for point \eqn{x} with expansion parameter \eqn{r \ge 1}
for the region outside the interval \eqn{(a,b)}.

\code{rv} is the index of the end vertex region \eqn{x_1} resides, with default=\code{NULL},
and \code{rv=1} for left end interval and \code{rv=2} for the right end interval.
If \eqn{x_1} and \eqn{x_2} are distinct and either of them are inside interval \code{int}, it returns 0,
but if they are identical, then it returns 1 regardless of their locations (i.e., it allows loops).

See also (\insertCite{ceyhan:metrika-2012;textual}{pcds}).
}
\examples{
a<-0; b<-10; int<-c(a,b)
r<-2

IndNPEend1D(15,17,r,int)
IndNPEend1D(15,15,r,int)

IndNPEend1D(1.5,17,r,int)

IndNPEend1D(-15,17,r,int)

IndNPEend1D(a,17,r,int)
IndNPEend1D(15,17,r=1.1,int)

a<-0; b<-10; int<-c(a,b)
r<-2
IndNPEend1D(15,17,r,int)

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{IndNPEmid1D}}, \code{\link{IndNCSmid1D}}, and \code{\link{IndNCSend1D}}
}
\author{
Elvan Ceyhan
}
