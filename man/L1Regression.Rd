% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/L1Regression.R
\name{L1Regression}
\alias{L1Regression}
\title{Computes a L1 multivariate regression 
 This is the equivalent of median regression when the 
 response is possibly multivariate}
\usage{
L1Regression(Data.Y, Data.X, Weights, 
				InitialValue = "WLS", MaxIteration, epsilon, lambda)
}
\arguments{
\item{Data.Y}{a numeric matrix, to act as response}

\item{Data.X}{a numeric matrix, to act as covariates}

\item{Weights}{a numeric matrix, to act as weights}

\item{InitialValue}{a character, to denote how the initial estimate will be computed
currently the only available option is WLS}

\item{MaxIteration}{an integer, for the maximum number of iterations allowed}

\item{epsilon}{a positive real number, as tolerance value for convergence}

\item{lambda}{a real number between 0 and 1, to control the amount of update allowed
in each iteration}
}
\value{
a list consisting of the iteration value at the last step, the 
				difference in norms between the last two iterations, and the 
				estimate of slope
}
\description{
Computes a L1 multivariate regression 
 This is the equivalent of median regression when the 
 response is possibly multivariate
}
\examples{
 \dontrun{
DataY = cbind(CCU12_Precip$Precip, CCU12_Precip$TMax);
DataX = cbind(rep(1, length(CCU12_Precip$Precip)), CCU12_Precip$TMin)			
A2 = L1Regression(DataY, DataX)
    		}


}
