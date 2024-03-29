% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PropEdge1D.R
\name{PEdom.num1Dnondeg}
\alias{PEdom.num1Dnondeg}
\title{The domination number of Proportional Edge Proximity Catch Digraph (PE-PCD) with
non-degeneracy centers - multiple interval case}
\usage{
PEdom.num1Dnondeg(Xp, Yp, r)
}
\arguments{
\item{Xp}{A set of 1D points which constitute the vertices of the PE-PCD.}

\item{Yp}{A set of 1D points which constitute the end points of the intervals which
partition the real line.}

\item{r}{A positive real number
which serves as the expansion parameter in PE proximity region;
must be in \eqn{(1,2]} here.}
}
\value{
A \code{list} with three elements
\item{dom.num}{Domination number of PE-PCD with vertex set \code{Xp}
and expansion parameter \eqn{r in (1,2]} and
centrality parameter \eqn{c \in \{(r-1)/r,1/r\}}.}
\item{mds}{A minimum dominating set of the PE-PCD.}
\item{ind.mds}{The data indices of the minimum dominating set of the PE-PCD
whose vertices are \code{Xp} points.}
\item{int.dom.nums}{Domination numbers of the PE-PCD components for the partition intervals.}
}
\description{
Returns the domination number,
a minimum dominating set of PE-PCD whose vertices are the 1D data set \code{Xp},
and the domination numbers for partition intervals based on \code{Yp}
when PE-PCD is constructed with vertex regions based on non-degeneracy centers.

\code{Yp} determines the end points of the intervals
(i.e., partition the real line via intervalization).
If there are duplicates of \code{Yp} points,
only one point is retained for each duplicate value,
and a warning message is printed.

PE proximity regions are defined with respect to the intervals based on \code{Yp} points with
expansion parameter \eqn{r \ge 1} and
 vertex regions in each interval are based on the centrality parameter \code{c}
which is one of the 2 values of \code{c} (i.e., \eqn{c \in \{(r-1)/r,1/r\}})
that renders the asymptotic distribution of domination number
to be non-degenerate for a given value of \code{r} in \eqn{(1,2)}
and \code{c} is center of mass for \eqn{r=2}.
These values are called non-degeneracy centrality parameters
and the corresponding centers are called
nondegeneracy centers.
}
\examples{
\donttest{
a<-0; b<-10
r<-1.5

#nx is number of X points (target) and ny is number of Y points (nontarget)
nx<-15; ny<-4;  #try also nx<-40; ny<-10 or nx<-1000; ny<-10;

set.seed(1)
Xp<-runif(nx,a,b)
Yp<-runif(ny,a,b)

PEdom.num1Dnondeg(Xp,Yp,r)
PEdom.num1Dnondeg(Xp,Yp,r=1.25)
}

}
\seealso{
\code{\link{PEdom.num.nondeg}}
}
\author{
Elvan Ceyhan
}
