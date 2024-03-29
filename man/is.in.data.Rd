% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ArcSliceFunctions.R
\name{is.in.data}
\alias{is.in.data}
\title{Check a point belong to a given data set}
\usage{
is.in.data(p, Xp)
}
\arguments{
\item{p}{A 2D point for which the function checks membership to the data set \code{Xp}.}

\item{Xp}{A set of 2D points representing the set of data points.}
}
\value{
\code{TRUE} if \code{p} belongs to the data set \code{Xp}.
}
\description{
returns \code{TRUE} if the point \code{p} of any dimension is inside the data set \code{Xp} of the same dimension as \code{p};
otherwise returns \code{FALSE}.
}
\examples{
\donttest{
n<-10
Xp<-cbind(runif(n),runif(n))

P<-Xp[7,]
is.in.data(P,Xp)
is.in.data(P,Xp[7,])

P<-Xp[7,]+10^(-7)
is.in.data(P,Xp)

P<-Xp[7,]+10^(-9)
is.in.data(P,Xp)

is.in.data(P,P)

is.in.data(c(2,2),c(2,2))

#for 1D data
n<-10
Xp<-runif(n)

P<-Xp[7]
is.in.data(P,Xp[7])  #this works because both entries are treated as 1D vectors but
#is.in.data(P,Xp) does not work since entries are treated as vectors of different dimensions

Xp<-as.matrix(Xp)
is.in.data(P,Xp)
#this works, because P is a 1D point, and Xp is treated as a set of 10 1D points

P<-Xp[7]+10^(-7)
is.in.data(P,Xp)

P<-Xp[7]+10^(-9)
is.in.data(P,Xp)

is.in.data(P,P)

#for 3D data
n<-10
Xp<-cbind(runif(n),runif(n),runif(n))

P<-Xp[7,]
is.in.data(P,Xp)
is.in.data(P,Xp[7,])

P<-Xp[7,]+10^(-7)
is.in.data(P,Xp)

P<-Xp[7,]+10^(-9)
is.in.data(P,Xp)

is.in.data(P,P)

n<-10
Xp<-cbind(runif(n),runif(n))
P<-Xp[7,]
is.in.data(P,Xp)
}

}
\author{
Elvan Ceyhan
}
