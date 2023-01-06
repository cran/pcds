## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>",fig.width=6, fig.height=4, fig.align = "center") 

## ----setup, message=FALSE, results='hide'-------------------------------------
library(pcds)

## -----------------------------------------------------------------------------
trees<-swamptrees
head(trees)
Xp<-trees[trees[,3]==1,][,1:2] # coordinates of all live trees
Yp<-trees[trees[,3]==0,][,1:2] # coordinates of all dead trees

## ----SwTrfig, fig.cap="The scatterplot of the Live Trees (red circles) and Dead Trees (black squares) in the Swamp Tree Dataset."----
lab.fac=as.factor(trees$live)
lab=as.numeric(trees$live)
plot(trees[,1:2],col=lab+1,pch=lab,xlab="x",ylab="y",main="Scatter plot of live and dead trees")

## ----SwTrDTfig, eval=F, fig.cap="The scatterplot of the live trees in the swamp trees data and the Delaunay triangulation of dead trees (dashed lines)."----
#  Xlim<-range(Xp[,1],Yp[,1])
#  Ylim<-range(Xp[,2],Yp[,2])
#  xd<-Xlim[2]-Xlim[1]
#  yd<-Ylim[2]-Ylim[1]
#  plot(Xp,xlab="x", ylab="y",xlim=Xlim+xd*c(-.05,.05),
#       ylim=Ylim+yd*c(-.05,.05),pch=".",cex=3,main="Live Trees (solid squares) and Delaunay
#       Triangulation of Dead Treess")
#  #now, we add the Delaunay triangulation based on Y points
#  DT<-interp::tri.mesh(Yp[,1],Yp[,2],duplicate="remove")
#  interp::plot.triSht(DT, add=TRUE, do.points = TRUE)

## ----eval=F-------------------------------------------------------------------
#  num.del.tri(Yp)
#  [1] 194

## ----eval=FALSE---------------------------------------------------------------
#  M<-"CC" #try also M<-c(1,1,1) #or M<-c(1,2,3)
#  narcs=NumArcsAS(Xp,Yp,M)
#  narcs$num.arcs
#  #> [1] 1849

## ----eval=FALSE---------------------------------------------------------------
#  M<-c(1,1,1) #try also M<-c(1,2,3) #or M<-"CC"
#  r<-1.5 #try also r<-2
#  
#  arcs=NumArcsPE(Xp,Yp,r,M)
#  arcs$num.arcs
#  #> [1] 1429

## ----eval=FALSE---------------------------------------------------------------
#  TSArcDensPE(Xp,Yp,r) #try also TSArcDensPE(Xp,Yp,r,alt="l") or #TSArcDensPE(Xp,Yp,r,ch=TRUE)
#  
#  	Large Sample z-Test Based on Arc Density of PE-PCD for Testing Uniformity of 2D Data ---
#  	without Convex Hull Correction
#  
#  data:  Xp
#  standardized arc density (i.e., Z) = -1.7333, p-value = 0.08304
#  alternative hypothesis: true (expected) arc density is not equal to 0.005521555
#  95 percent confidence interval:
#   0.003780462 0.005628405
#  sample estimates:
#  arc density
#  0.004704434

## ----eval=FALSE---------------------------------------------------------------
#  PEdom(Xp,Yp,r,M)
#  $dom.num
#  [1] 198
#  
#  $ind.mds
#    [1]   8   4  18   6  16   7  11  67  64  17  14  78  82  75  19  60  83  88  87  27  23  28  29  54  94  80 154 148  95 153
#   [31] 152  47  30 149 144 147  50 101  45  48 143 173 105 109  31  46 117  44  79 168 169 134 140 139 128 113 172 165 161 160
#   [61] 177 219 187 214 209 126 220 201 208 183 188 125 225 226 224 223 210 213 242 247 200 196 250 282  43 123 241 243 294 258
#   [91] 283 284 281 289 288 300 304 298 306 308 286 321 278 277 311 314 313 312 276 273 319 317 318 316 320 363 364 339 369 333
#  [121] 377 331 307 345 346 385 384 379 380 328 375 373 374 383 410 408 414 388 425 423 235 303 398 355 396 381 434 433 460 457
#  [151] 412 194 329 193 445 458 507 442 514 464 482 517 519 506 541 573 530 532 562 539 491 500 490 533 568 566 590 586 578 569
#  [181] 588 589 610 565 593 591 557 559 594 602 601 550 555 607 609 614 616 611
#  
#  $tri.dom.nums
#    [1] 0 1 0 1 1 0 1 0 3 0 0 0 0 0 2 0 2 0 1 1 1 0 0 0 0 1 1 0 0 0 0 1 2 0 0 0 1 0 0 1 2 1 0 1 1 0 1 0 1 1 0 1 1 1 1 0 3 2 2 0 0
#   [62] 0 2 0 1 1 0 0 2 0 1 1 0 1 0 2 1 2 2 1 1 2 0 1 0 0 0 0 0 1 1 0 0 1 1 1 1 0 2 1 1 1 0 3 1 1 1 2 0 2 2 2 2 1 0 1 0 2 3 1 2 2
#  [123] 1 0 1 2 2 2 0 2 0 0 0 0 2 2 0 1 2 1 1 1 2 3 2 1 0 1 1 3 1 3 1 2 1 1 3 3 3 3 1 1 0 1 1 3 2 2 0 2 1 2 3 1 1 0 0 2 2 2 1 1 0
#  [184] 3 0 2 1 0 2 2 1 1 3 0
#  
#  PEdom.nd(Xp,Yp,r)
#  $dom.num
#  [1] 198
#  
#  $ind.mds
#    [1]   8   4  18   6  11  16   7  67  64  65  14  78  82  75  19  60  83  87  88  27  23  29  28  54  94  80 154 148  95 153
#   [31] 152  47  30 147 144 149 101  51  48  45 173 143 105 109  32  46 118  44  79 171 169 134 139 140 113 128 172 165 161 160
#   [61] 177 219 187 214 209 126 220 208 201 183 188 125 226 225 224 223 210 213 218 242 200 196 250 282 123  43 243 241 294 258
#   [91] 283 284 289 281 288 300 304 298 306 308 286 321 278 277 311 314 313 312 276 273 319 317 316 318 320 364 363 339 369 333
#  [121] 331 377 346 345 307 385 384 379 380 328 373 375 374 383 410 414 408 388 423 425 235 303 396 398 355 381 433 434 412 457
#  [151] 460 329 193 194 445 458 507 442 464 482 514 517 519 506 541 573 530 532 562 539 490 491 500 533 568 566 590 586 578 569
#  [181] 588 589 610 591 565 593 559 557 594 601 602 555 550 607 609 611 614 616
#  
#  $tri.dom.nums
#    [1] 0 1 0 1 1 0 1 0 3 0 0 0 0 0 2 0 2 0 1 1 1 0 0 0 0 1 1 0 0 0 0 1 2 0 0 0 1 0 0 1 2 1 0 1 1 0 1 0 1 1 0 1 1 1 1 0 3 2 2 0 0
#   [62] 0 2 0 1 1 0 0 2 0 1 1 0 1 0 2 1 2 2 1 1 2 0 1 0 0 0 0 0 1 1 0 0 1 1 1 1 0 2 1 1 1 0 3 1 1 1 2 0 2 2 2 2 1 0 1 0 2 3 1 2 2
#  [123] 1 0 1 2 2 2 0 2 0 0 0 0 2 2 0 1 2 1 1 1 2 3 2 1 0 1 1 3 1 3 1 2 1 1 3 3 3 3 1 1 0 1 1 3 2 2 0 2 1 2 3 1 1 0 0 2 2 2 1 1 0
#  [184] 3 0 2 1 0 2 2 1 1 3 0

## ----eval=FALSE---------------------------------------------------------------
#  TSDomPEBin(Xp,Yp,r) #try also TSDomPEBin(Xp,Yp,r,alt="g")
#  
#  	Large Sample Binomial Test based on the Domination Number of PE-PCD for Testing Uniformity of 2D Data ---
#  	without Convex Hull Correction
#  
#  data:  Xp
#  # of times domination number is <= 2 = 179, p-value = 1.921e-10
#  alternative hypothesis: true Pr(Domination Number <=2) is not equal to 0.7413
#  95 percent confidence interval:
#   0.8756797 0.9560803
#  sample estimates:
#            domination number   || Pr(domination number <= 2)
#                    198.0000000                     0.9226804

## ----eval=FALSE---------------------------------------------------------------
#  TSDomPENorm(Xp,Yp,r) #try also TSDomPENorm(Xp,Yp,r,alt="g")
#  
#  	Normal Approximation to the Domination Number of PE-PCD for Testing Uniformity of 2D Data ---
#  	without Convex Hull Correction
#  
#  data:  Xp
#  standardized domination number (i.e., Z) = 5.7689, p-value = 7.977e-09
#  alternative hypothesis: true expected domination number is not equal to 143.8122
#  95 percent confidence interval:
#   186.0451 209.9549
#  sample estimates:
#           domination number   || Pr(domination number = 3)
#                   198.0000000                    0.9226804

## ----eval=FALSE---------------------------------------------------------------
#  M<-c(1,1,1) #try also M<-c(1,2,3)
#  tau<-1.5 #try also tau<-2, and tau=.5
#  
#  arcs=NumArcsCS(Xp,Yp,tau,M)
#  arcs$num.arcs
#  #> [1] 955

## ----eval=FALSE---------------------------------------------------------------
#  TSArcDensCS(Xp,Yp,tau) #try also TSArcDensCS(Xp,Yp,tau,alt="l")
#  
#  	Large Sample z-Test Based on Arc Density of CS-PCD for Testing Uniformity of 2D Data ---
#  	without Convex Hull Correction
#  
#  data:  Xp
#  standardized arc density (i.e., Z) = -1.7446, p-value = 0.08106
#  alternative hypothesis: true (expected) arc density is not equal to 0.003837374
#  95 percent confidence interval:
#   0.002373249 0.003922496
#  sample estimates:
#  arc density
#  0.003147873

