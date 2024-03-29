% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PatternGen.R
\name{seg.tri.support}
\alias{seg.tri.support}
\title{The auxiliary triangle to define the support of type I segregation}
\usage{
seg.tri.support(delta, tri)
}
\arguments{
\item{delta}{A positive real number between 0 and 1
that determines the percentage of area of the triangle
around the vertices forbidden for point generation.}

\item{tri}{A \eqn{3 \times 2} matrix with each row
representing a vertex of the triangle.}
}
\value{
the vertices of the triangle (stacked row-wise)
whose intersection with a general triangle
gives the support for type I segregation for the given \code{delta}
}
\description{
Returns the triangle whose intersection
with a general triangle gives the support for
type I segregation given the \code{delta}
(i.e., \eqn{\delta 100} \% area of a triangle around the
vertices is chopped off).
See the plot in the examples.

Caveat: the vertices of this triangle may be
outside the triangle, \code{tri}, depending on the value of
\code{delta} (i.e., for small values of \code{delta}).
}
\examples{
\donttest{
#for a general triangle
A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
Tr<-rbind(A,B,C);
delta<-.3  #try also .5,.75,.85
Tseg<-seg.tri.support(delta,Tr)

Xlim<-range(Tr[,1],Tseg[,1])
Ylim<-range(Tr[,2],Tseg[,2])
xd<-Xlim[2]-Xlim[1]
yd<-Ylim[2]-Ylim[1]

oldpar <- par(pty="s")
plot(Tr,pch=".",xlab="",ylab="",
main="segregation support is the intersection\n of these two triangles",
axes=TRUE,xlim=Xlim+xd*c(-.05,.05),ylim=Ylim+yd*c(-.05,.05))
polygon(Tr)
polygon(Tseg,lty=2)

txt<-rbind(Tr,Tseg)
xc<-txt[,1]+c(-.03,.03,.03,.06,.04,-.04)
yc<-txt[,2]+c(.02,.02,.04,-.03,0,0)
txt.str<-c("A","B","C","T1","T2","T3")
text(xc,yc,txt.str)
par(oldpar)
}

}
\seealso{
\code{\link{rseg.std.tri}} and \code{\link{rseg.multi.tri}}
}
\author{
Elvan Ceyhan
}
