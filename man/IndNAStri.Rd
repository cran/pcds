% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCDFunctions.r
\name{IndNAStri}
\alias{IndNAStri}
\title{The indicator for the presence of an arc from a point to another for Arc Slice Proximity Catch Digraphs
(AS-PCDs) - one triangle case}
\usage{
IndNAStri(pt1, pt2, tri, M = "CC", rv = NULL)
}
\arguments{
\item{pt1}{A 2D point whose AS proximity region is constructed.}

\item{pt2}{A 2D point. The function determines whether \code{pt2} is inside the AS proximity region of
\code{pt1} or not.}

\item{tri}{Three 2D points, stacked row-wise, each row representing a vertex of the triangle.}

\item{M}{The center of the triangle. \code{"CC"} stands for circumcenter of the triangle \code{tri} or a 2D point in Cartesian coordinates or
a 3D point in barycentric coordinates which serves as a center in the interior of \code{tri};
default is \code{M="CC"} i.e., the circumcenter of \code{tri}.}

\item{rv}{The index of the \code{M}-vertex region in \code{tri} containing the point, either \code{1,2,3} or \code{NULL}
(default is \code{NULL}).}
}
\value{
\eqn{I(pt2 \in N_{AS}(pt1))} for \code{pt1}, that is, returns 1 if \eqn{pt2} is in \eqn{N_{AS}(pt1)}, returns 0 otherwise
}
\description{
Returns \eqn{I(pt2 \in N_{AS}(pt1))} for points \code{pt1} and \code{pt2}, that is, returns 1 if \eqn{pt2} is in \eqn{N_{AS}(pt1)}, returns 0
otherwise, where \eqn{N_{AS}(x)} is the AS proximity region for point \eqn{x}.

AS proximity regions are constructed with respect to the triangle, \code{tri}\eqn{=T(A,B,C)=}\code{(rv=1,rv=2,rv=3)},
and vertex regions are based on the center \code{M="CC"} for circumcenter of \code{tri};
or \eqn{M=(m_1,m_2)} in Cartesian coordinates or \eqn{M=(\alpha,\beta,\gamma)} in barycentric coordinates in the
interior of the triangle \code{tri}; default is \code{M="CC"} i.e., circumcenter of \code{tri}.
\code{rv} is the index of the vertex region \code{pt1} resides, with default=\code{NULL}.

If \code{pt1} and \code{pt2} are distinct and either of them are outside \code{tri}, the function returns 0,
but if they are identical, then it returns 1 regardless of their locations (i.e., it allows loops).

See also (\insertCite{ceyhan:Phd-thesis,ceyhan:comp-geo-2010,ceyhan:mcap2012;textual}{pcds}).
}
\examples{
A<-c(1,1); B<-c(2,0); C<-c(1.5,2);

Tr<-rbind(A,B,C);

M<-as.numeric(runif.tri(1,Tr)$g)  #try also M<-c(1.6,1.2)

P1<-as.numeric(runif.tri(1,Tr)$g)
P2<-as.numeric(runif.tri(1,Tr)$g)
IndNAStri(P1,P2,Tr,M)

P1<-c(1.3,1.2)
P2<-c(1.8,.5)
IndNAStri(P1,P2,Tr,M)
IndNAStri(P1,P1,Tr,M)

#or try
Rv<-rv.triCC(P1,Tr)$rv
IndNAStri(P1,P2,Tr,M,Rv)

P3<-c(1.6,1.4)
IndNAStri(P1,P3,Tr,M)

P4<-c(1.5,1.0)
IndNAStri(P1,P4,Tr,M)

P5<-c(.5,1.0)
IndNAStri(P1,P5,Tr,M)
IndNAStri(P5,P5,Tr,M)

#or try
Rv<-rv.triCC(P5,Tr)$rv
IndNAStri(P5,P5,Tr,M,Rv)

dat.fr<-data.frame(a=Tr)
IndNAStri(P1,P2,dat.fr,M)

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link{IndNASbastri}}, \code{\link{IndNPEtri}}, and \code{\link{IndNCStri}}
}
\author{
Elvan Ceyhan
}
