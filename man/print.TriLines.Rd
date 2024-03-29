% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClassFunctions.R
\name{print.TriLines}
\alias{print.TriLines}
\title{Print a \code{TriLines} \code{object}}
\usage{
\method{print}{TriLines}(x, ...)
}
\arguments{
\item{x}{A \code{TriLines} \code{object}.}

\item{\dots}{Additional arguments for the S3 method \code{'print'}.}
}
\value{
The \code{call} of the \code{object} of class \code{"TriLines"} ,
the \code{coefficients} of the line
(in the form: \code{y = slope * x + intercept}),
and the vertices of the triangle
with respect to which the line is defined.
}
\description{
Prints the \code{call} of the \code{object}
of class \code{"TriLines"}
and also the \code{coefficients} of the line
(in the form: \code{y = slope * x + intercept}),
and the vertices of the triangle
with respect to which the line is defined.
}
\examples{
\donttest{
A<-c(0,0); B<-c(1,0); C<-c(1/2,sqrt(3)/2);
Te<-rbind(A,B,C)
xfence<-abs(A[1]-B[1])*.25
#how far to go at the lower and upper ends in the x-coordinate
x<-seq(min(A[1],B[1])-xfence,max(A[1],B[1])+xfence,l=3)

lnACM<-lineA2CMinTe(x)
lnACM
print(lnACM)

typeof(lnACM)
attributes(lnACM)
}

}
\seealso{
\code{\link{summary.TriLines}},
\code{\link{print.summary.TriLines}},
and \code{\link{plot.TriLines}}
}
