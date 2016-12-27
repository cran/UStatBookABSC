

##############################################
###### This is the title/description of the program
##############################################
#' Computes a weighted least squares linear regression 
#'  on possibly multivariate responses
#'
##############################################
###### These are the arguments of the function, one in each line
##############################################
#' @param Y a numeric matrix, to act as response
#'
#' @param X a numeric matrix, to act as covariates
#'
#' @param W a numeric matrix, to act as weights
#'
##############################################
###### These is what we get of out the function
##############################################
#' @return a vector of regression coefficients
#'
##############################################
###### These is the way to use the function
##############################################
#' @usage WLS(Y, X, W)
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
#'     		}
#'
#'
#' @export




WLS = function(Y, X, W= diag(rep(1, nrow(as.matrix(Y))))){
		# Y = as.matrix(Y)
		BetaHat.WLS = solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% Y;
		return(BetaHat.WLS);
}

