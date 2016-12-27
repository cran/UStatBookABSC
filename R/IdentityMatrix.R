

##############################################
###### This is the title of the program
##############################################
#' Obtains the identity matrix of dimension n 
#'
#'
##############################################
###### These are the arguments of the function, one in each line
##############################################
#' @param n an integer
#'
#'
##############################################
###### These is what we get of out the function
##############################################
#' @return an identity matrix 
#'
##############################################
###### These is the way to use the function
##############################################
#' @usage IdentityMatrix(n)
#'
##############################################
###### This is an example, each function needs to have one
######   there are options of do-not-run etc etc
##############################################
#' @examples
#'  I.3 = IdentityMatrix(3)
#'  print(I.3)
#'
#' @export





IdentityMatrix = function(n){
	I = diag(rep(1,n));
	return(I);
}
