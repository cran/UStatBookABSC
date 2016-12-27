
##############################################
###### This is the title of the program
##############################################
#' Computes the Euclidean norm
#'
#'
##############################################
###### These are the arguments of the function, one in each line
##############################################
#' @param a a numeric vector
#'
#' @param na.rm logical
#'
##############################################
###### These is what we get out of the function
##############################################
#' @return a real number
#'
##############################################
###### These is the way to use the function
##############################################
#' @usage Norm(a, na.rm)
#'
##############################################
###### This is an example, each function needs to have one
######   there are options of do-not-run etc etc
##############################################
#' @examples
#'  x <- c(1, 2)
#'  Norm(x)
#'
#'
#'
#' @export

Norm = function (a,  na.rm = TRUE){
		
		if(na.rm == TRUE){
			if(sum(is.na(a))==length(a)){
				T = NA} 
			else{
				T = sqrt(sum(a*a, na.rm=TRUE))}
								};
		if(na.rm == FALSE){
				T = sqrt(sum(a*a));
								};
			
			return(T)}



