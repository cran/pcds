% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PatternGen.R
\name{runif.std.tri.onesixth}
\alias{runif.std.tri.onesixth}
\title{Generation of Uniform Points in the first one-sixth of
standard equilateral triangle}
\usage{
runif.std.tri.onesixth(n)
}
\arguments{
\item{n}{a positive integer representing number of uniform points
to be generated
in the first one-sixth of \eqn{T_e}.}
}
\value{
A \code{list} with the elements
\item{type}{The type of the point pattern}
\item{mtitle}{The \code{"main"} title for the plot of the point pattern}
\item{support}{The vertices of the support of
the uniformly generated points}
\item{gen.points}{The output set of uniformly generated points
in the first 1/6th of
the standard equilateral triangle.}
\item{out.region}{The outer region for the one-sixth of \eqn{T_e},
which is just \eqn{T_e} here.}
\item{desc.pat}{Description of the point pattern}
\item{num.points}{The \code{vector} of two numbers,
which are the number of generated points
and the number of vertices of the support (i.e., \code{Y}) points.}
\item{txt4pnts}{Description of the two numbers in \code{num.points}.}
\item{xlimit,ylimit}{The ranges of the \eqn{x}-
and \eqn{y}-coordinates of the generated,
support and outer region points}
}
\description{
An object of class \code{"Uniform"}.
Generates \code{n} points uniformly
in the first 1/6th of the standard equilateral triangle \eqn{T_e=(A,B,C)}
with vertices with \eqn{A=(0,0)}; \eqn{B=(1,0)}, \eqn{C=(1/2,\sqrt{3}/2)}
(see the examples below).
The first 1/6th of the standard equilateral triangle is the triangle with vertices
\eqn{A=(0,0)}, \eqn{(1/2,0)}, \eqn{C=(1/2,\sqrt{3}/6)}.
}
\examples{
\donttest{
A<-c(0,0); B<-c(1,0); C<-c(1/2,sqrt(3)/2);
Te<-rbind(A,B,C);
CM<-(A+B+C)/3;
D1<-(B+C)/2; D2<-(A+C)/2; D3<-(A+B)/2;
Ds<-rbind(D1,D2,D3)
nx<-100  #try also nx<-1000

#data generation step
set.seed(1)
Xdt<-runif.std.tri.onesixth(nx)
Xdt
summary(Xdt)
plot(Xdt,asp=1)

Xd<-Xdt$gen.points

#plot of the data with the regions in the equilateral triangle
Xlim<-range(Te[,1])
Ylim<-range(Te[,2])
xd<-Xlim[2]-Xlim[1]
yd<-Ylim[2]-Ylim[1]

plot(Te,asp=1,pch=".",xlim=Xlim+xd*c(-.01,.01),
ylim=Ylim+yd*c(-.01,.01),xlab=" ",ylab=" ",
     main="first 1/6th of the \n standard equilateral triangle")
polygon(Te)
L<-Te; R<-Ds
segments(L[,1], L[,2], R[,1], R[,2], lty=2)
polygon(rbind(A,D3,CM),col=5)
points(Xd)

#letter annotation of the plot
txt<-rbind(A,B,C,CM,D1,D2,D3)
xc<-txt[,1]+c(-.02,.02,.02,.04,.05,-.03,0)
yc<-txt[,2]+c(.02,.02,.02,.03,0,.03,-.03)
txt.str<-c("A","B","C","CM","D1","D2","D3")
text(xc,yc,txt.str)
}

}
\seealso{
\code{\link{runif.std.tri}}, \code{\link{runif.basic.tri}},
\code{\link{runif.tri}}, and \code{\link{runif.multi.tri}}
}
\author{
Elvan Ceyhan
}
