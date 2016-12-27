
##############################################
###### This is the title/description of the program
##############################################
#' Computes a L1 multivariate regression 
#'  This is the equivalent of median regression when the 
#'  response is possibly multivariate
##############################################
###### These are the arguments of the function, one in each line
##############################################
#' @param Data.Y a numeric matrix, to act as response
#'
#' @param Data.X a numeric matrix, to act as covariates
#'
#' @param Weights a numeric matrix, to act as weights
#'
#' @param InitialValue a character, to denote how the initial estimate will be computed
#'				currently the only available option is WLS
#'
#' @param MaxIteration an integer, for the maximum number of iterations allowed
#'
#' @param epsilon a positive real number, as tolerance value for convergence
#'
#' @param lambda a real number between 0 and 1, to control the amount of update allowed
#'				in each iteration

##############################################
###### These is what we get of out the function
##############################################
#' @return a list consisting of the iteration value at the last step, the 
#' 				difference in norms between the last two iterations, and the 
#' 				estimate of slope
##############################################
###### These is the way to use the function
##############################################
#' @usage L1Regression(Data.Y, Data.X, Weights, 
#' 				InitialValue = "WLS", MaxIteration, epsilon, lambda)
#'
#'
##############################################
###### This is an example, each function needs to have one
######   there are options of do-not-run etc etc
##############################################
#' @examples
#'  \dontrun{
#'	DataY = cbind(CCU12_Precip$Precip, CCU12_Precip$TMax);
#'	DataX = cbind(rep(1, length(CCU12_Precip$Precip)), CCU12_Precip$TMin)			
#'	A2 = L1Regression(DataY, DataX)
#'     		}
#'
#'
#' @export





L1Regression = function(Data.Y, Data.X, Weights= diag(rep(1, nrow(as.matrix(Data.Y)))), 
							InitialValue = "WLS", MaxIteration = 1e3, epsilon = 1e-3, 
							lambda = 0.5){
	if(InitialValue == "WLS"){
		Weights.Old = Weights;
		BetaHat.Old = WLS(Data.Y, Data.X, Weights.Old);
			}

			BetaHat.Old2 = BetaHat.Old;
			Results.Old = FitAndResiduals(Data.Y, Data.X, BetaHat.Old);
			Residuals.Old.Norm = apply(Results.Old$Residuals, 1, Norm);
			Weights.New = diag(diag(Weights) * Residuals.Old.Norm);
			BetaHat.New = WLS(Data.Y, Data.X, Weights.New);
			BetaHat.Old = BetaHat.New;	
			Absolute.Difference = 1e2*epsilon;
			
	 		Iteration = 1;
	 	while((Absolute.Difference > epsilon) && (Iteration < MaxIteration)){
			Iteration = Iteration + 1;
			Results.Old = FitAndResiduals(Data.Y, Data.X, BetaHat.Old);
			Residuals.Old.Norm = apply(Results.Old$Residuals, 1, Norm);
			Weights.New = diag(diag(Weights) * Residuals.Old.Norm);
			BetaHat.New = WLS(Data.Y, Data.X, Weights.New);
			Difference = sum(apply (BetaHat.New - BetaHat.Old, 1, Norm));
			Difference2 = sum(apply (BetaHat.New - BetaHat.Old2, 1, Norm));
			Scale = sum(apply (BetaHat.Old, 1, Norm));
			Absolute.Difference = Difference/Scale;
			Absolute.Difference2 = Difference2/Scale;
				
			BetaHat.Old2 = BetaHat.Old;
			BetaHat.Old = (1 - lambda ) * BetaHat.Old + lambda * BetaHat.New;	
				
		 } ###ends condition for iteration
	
	OutList = list(Iteration = Iteration, Convergence = Absolute.Difference, BetaHat = BetaHat.Old);	
	return(OutList);

	}
