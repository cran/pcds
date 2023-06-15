## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>",fig.width=6, fig.height=4, fig.align = "center") 
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, message=FALSE, results='hide'-------------------------------------
library(pcds)

## ----DTfig, eval=FALSE, fig.cap = "Delaunay triangulation of 20 uniform $Y$ points in the unit square $(0,1)$-by-$(0,1)$."----
#  ny<-20;
#  
#  set.seed(1)
#  #Xp<-cbind(runif(nx),runif(nx))
#  Yp<-cbind(runif(ny),runif(ny))
#  
#  #oldpar <- par(no.readonly = TRUE)
#  plotDelaunay.tri(Yp,Yp,xlab="",ylab="",main="Delaunay Triangulation of Y points")

