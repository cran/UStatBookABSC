

##############################################
###### This is the title/description of the program
##############################################
#' Computes a linear regression fit and residuals 
#'  on possibly multivariate responses 
#'
##############################################
###### These are the arguments of the function, one in each line
##############################################
#' @param Y a numeric matrix, to act as response
#'
#' @param X a numeric matrix, to act as covariates
#'
#' @param BetaHat a numeric matrix, to act as slope
#'
##############################################
###### These is what we get of out the function
##############################################
#' @return a list consisting of two vectors, the 
#'			fitted values and residuals
#'
##############################################
###### These is the way to use the function
##############################################
#' @usage FitAndResiduals(Y, X, BetaHat)
#'
##############################################
###### This is an example, each function needs to have one
######   there are options of do-not-run etc etc
##############################################
#' @examples
#'  \dontrun{
#'  DataY = cbind(CCU12_Precip$Precip, CCU12_Precip$TMax);
#'	DataX = cbind(rep(1, length(CCU12_Precip$Precip)), CCU12_Precip$TMin)			
#'	BetaHat.New = WLS(DataY, DataX)
#'	Results.New = FitAndResiduals(DataY, DataX, BetaHat.New);
#'     		}
#'
#'
#' @export




FitAndResiduals = function(Y, X, BetaHat){
	Fit = X %*% BetaHat;
	Residuals = Y - Fit;
	OutList = list(Fit = Fit, Residuals = Residuals);
	return(OutList);
}
