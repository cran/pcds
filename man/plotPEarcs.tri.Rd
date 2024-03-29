% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PropEdge2D.R
\name{plotPEarcs.tri}
\alias{plotPEarcs.tri}
\title{The plot of the arcs of Proportional Edge Proximity Catch Digraph
(PE-PCD) for a 2D data set - one triangle case}
\usage{
plotPEarcs.tri(
  Xp,
  tri,
  r,
  M = c(1, 1, 1),
  asp = NA,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  vert.reg = FALSE,
  ...
)
}
\arguments{
\item{Xp}{A set of 2D points
which constitute the vertices of the PE-PCD.}

\item{tri}{A \eqn{3 \times 2} matrix with each row
representing a vertex of the triangle.}

\item{r}{A positive real number
which serves as the expansion parameter in PE proximity region;
must be \eqn{\ge 1}.}

\item{M}{A 2D point in Cartesian coordinates
or a 3D point in barycentric coordinates
which serves as a center in the interior of the triangle \code{tri}
or the circumcenter of \code{tri}
which may be entered as "CC" as well;
default is \eqn{M=(1,1,1)}, i.e., the center of mass of \code{tri}.}

\item{asp}{A \code{numeric} value,
giving the aspect ratio \eqn{y/x} (default is \code{NA}),
see the official help page for \code{asp} by
typing "\code{? asp}".}

\item{main}{An overall title for the plot (default=\code{NULL}).}

\item{xlab, ylab}{Titles for the \eqn{x} and \eqn{y} axes,
respectively (default=\code{NULL} for both).}

\item{xlim, ylim}{Two \code{numeric} vectors of length 2,
giving the \eqn{x}- and \eqn{y}-coordinate ranges
(default=\code{NULL} for both).}

\item{vert.reg}{A logical argument to add vertex regions to the plot,
default is \code{vert.reg=FALSE}.}

\item{\dots}{Additional \code{plot} parameters.}
}
\value{
A plot of the arcs of the PE-PCD
whose vertices are the points in data set \code{Xp}
and the triangle \code{tri}
}
\description{
Plots the arcs of PE-PCD whose vertices are the data points, \code{Xp}
and the triangle \code{tri}.
PE proximity regions
are constructed with respect to the triangle \code{tri}
with expansion parameter \eqn{r \ge 1}, i.e., arcs may exist only
for \code{Xp} points inside the triangle \code{tri}.
If there are duplicates of \code{Xp} points,
only one point is retained for each duplicate value,
and a warning message is printed.

Vertex regions are based on center \eqn{M=(m_1,m_2)}
in Cartesian coordinates
or \eqn{M=(\alpha,\beta,\gamma)} in barycentric coordinates
in the interior of the triangle \code{tri}
or based on the circumcenter of \code{tri};
default is \eqn{M=(1,1,1)}, i.e.,
the center of mass of \code{tri}.
When the center is the circumcenter, \code{CC},
the vertex regions are constructed based on the
orthogonal projections to the edges,
while with any interior center \code{M},
the vertex regions are constructed using the extensions
of the lines combining vertices with \code{M}.
\code{M}-vertex regions are recommended spatial inference,
due to geometry invariance property of the arc density
and domination number the PE-PCDs based on uniform data.

See also (\insertCite{ceyhan:Phd-thesis,ceyhan:arc-density-PE,ceyhan:dom-num-NPE-Spat2011;textual}{pcds}).
}
\examples{
\donttest{
A<-c(1,1); B<-c(2,0); C<-c(1.5,2);
Tr<-rbind(A,B,C);
n<-10  #try also n<-20

set.seed(1)
Xp<-runif.tri(n,Tr)$g

M<-as.numeric(runif.tri(1,Tr)$g)
#try also M<-c(1.6,1.0) or M<-circumcenter.tri(Tr)
r<-1.5  #try also r<-2
plotPEarcs.tri(Xp,Tr,r,M,main="Arcs of PE-PCD with r = 1.5",
xlab="",ylab="",vert.reg = TRUE)

# or try the default center
#plotPEarcs.tri(Xp,Tr,r,main="Arcs of PE-PCD with r = 1.5",
#xlab="",ylab="",vert.reg = TRUE);
#M=(arcsPEtri(Xp,Tr,r)$param)$cent
#the part "M=(arcsPEtri(Xp,Tr,r)$param)$cent" is optional,
#for the below annotation of the plot

#can add vertex labels and text to the figure (with vertex regions)
ifelse(isTRUE(all.equal(M,circumcenter.tri(Tr))),
{Ds<-rbind((B+C)/2,(A+C)/2,(A+B)/2); cent.name="CC"},
{Ds<-prj.cent2edges(Tr,M); cent.name="M"})

txt<-rbind(Tr,M,Ds)
xc<-txt[,1]+c(-.02,.02,.02,.02,.04,-0.03,-.01)
yc<-txt[,2]+c(.02,.02,.02,.07,.02,.04,-.06)
txt.str<-c("A","B","C",cent.name,"D1","D2","D3")
text(xc,yc,txt.str)
}

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{plotASarcs.tri}}, \code{\link{plotCSarcs.tri}},
and \code{\link{plotPEarcs}}
}
\author{
Elvan Ceyhan
}
