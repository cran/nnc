\name{nnc-package}
\Rdversion{1.1}
\alias{nnc-package}
\docType{package}
\title{
Nearest neighbour auto-covariates
}
\description{
The nearest neighbour auto-covariates were defined by
Holmes and Adams (2003).
The algorithm in this package was developed in Islam (2008)
and the method was applied to the classification problems
in microarray experiments.
}

\details{
\tabular{ll}{
Package: \tab nnc\cr
Type: \tab Package\cr
Version: \tab 1.12\cr
Date: \tab 2010-08-22\cr
License: \tab GPL (>= 2)\cr
}
}

\author{
A. I. McLeod and M. S. Islam
Maintainer: <aimcleod@uwo.ca>
}

\references{
Holmes, C. C. and Adams, N. M. (2003). 
Likelihood inference in nearest-neighbour classification models, 
Biometrika, 90(1), 99-112.
\url{http://biomet.oxfordjournals.org/cgi/content/abstract/90/1/99}

Islam, M. S. (2008). Periodicity, Change Detection and Prediction in Microarrays.
Ph.D. Thesis, The University of Western Ontario.
}

\examples{
#Use pseudolikelihood method to estimate k in kNN
#
library(MASS)
X <- synth.tr[,1:2]
y <- synth.tr[,3]
KMAX<-100
L<-numeric(KMAX)
for (k in 1:KMAX){
    z <- nnc(X = X, Y = y, k=k)
    L[k]<- deviance(glm.fit(x = z, y = y, family = binomial(link = "logit")))
    }
khat <- which.min(L)    
plot(L, xlab="k", ylab="deviance")
points(khat, L[khat], col="red", pch=16, cex=1.4)
title(sub=bquote(hat(k) == .(khat)))
}

\keyword{ package }
\keyword{classif}



