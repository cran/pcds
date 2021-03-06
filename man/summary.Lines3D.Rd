% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AuxRFuncs4PCDClasses.r
\name{summary.Lines3D}
\alias{summary.Lines3D}
\title{Return a summary of a \code{Lines3D} \code{object}}
\usage{
\method{summary}{Lines3D}(object, ...)
}
\arguments{
\item{object}{An \code{object} of class \code{Lines3D}.}

\item{\dots}{Additional parameters for \code{summary}.}
}
\value{
\code{call} of the function defining the \code{object}, the defining vectors (i.e., initial and direction vectors),
selected \eqn{x}, \eqn{y}, and \eqn{z} points on the line,
equation of the line (in parametric form), and \code{coefficients} of the line
(for the form: \code{x=x0 + a*t}, \code{y=y0 + b*t}, and \code{z=z0 + c*t}).
}
\description{
Returns the below information about the \code{object}:

\code{call} of the function defining the \code{object}, the defining vectors (i.e., initial and direction vectors),
selected \eqn{x}, \eqn{y}, and \eqn{z} points on the line,
equation of the line (in parametric form), and \code{coefficients} of the line.
}
\examples{
A<-c(1,10,3); B<-c(1,1,3);
vecs<-rbind(A,B)
Line3D(A,B,.1)
Line3D(A,B,.1,dir.vec=FALSE)

tr<-range(vecs);
tf<-(tr[2]-tr[1])*.1 #how far to go at the lower and upper ends in the x-coordinate
tsq<-seq(-tf*10-tf,tf*10+tf,l=100)

lnAB3D<-Line3D(A,B,tsq)
lnAB3D
summary(lnAB3D)

}
\seealso{
\code{\link{print.Lines3D}}, \code{\link{print.summary.Lines3D}}, and \code{\link{plot.Lines3D}}
}
