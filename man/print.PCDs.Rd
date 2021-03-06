% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AuxRFuncs4PCDClasses.r
\name{print.PCDs}
\alias{print.PCDs}
\title{Print a \code{PCDs} \code{object}}
\usage{
\method{print}{PCDs}(x, ...)
}
\arguments{
\item{x}{A \code{PCDs} \code{object}.}

\item{\dots}{Additional arguments for the S3 method \code{'print'}.}
}
\value{
The \code{call} of the \code{object} of class \code{"PCDs"}
and also the \code{type} (i.e. a brief description) of the proximity catch digraph (PCD)).
}
\description{
Prints the \code{call} of the \code{object} of class \code{"PCDs"}
and also the \code{type} (i.e. a brief description) of the proximity catch digraph (PCD)).
}
\examples{
A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
Tr<-rbind(A,B,C); n<-10
dat<-runif.tri(n,Tr)$g
M<-as.numeric(runif.tri(1,Tr)$g)
Arcs<-ArcsAStri(dat,Tr,M)
Arcs
print(Arcs)

}
\seealso{
\code{\link{summary.PCDs}}, \code{\link{print.summary.PCDs}}, and \code{\link{plot.PCDs}}
}
