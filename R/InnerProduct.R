
##############################################
###### This is the title of the program
##############################################
#' Computes the Euclidean inner product 
#'
#'
##############################################
###### These are the arguments of the function, one in each line
##############################################
#' @param a a numeric vector
#' @param b another numeric vector
#' @param na.rm logical
#'
#'
##############################################
###### These are the arguments of the function, one in each line
##############################################
#' @return a real number
#'
##############################################
###### These is the way to use the function
##############################################
#' @usage InnerProduct(a, b, na.rm)
#'
##############################################
###### This is an example, each function needs to have one
######   there are options of do-not-run etc etc
##############################################
#' @examples
#'  x <- c(1, 2, 3)
#'  y <- c(3, 0, 1)
#'  InnerProduct(x, y)
#'
#'
#'
#' @export

InnerProduct = function (a, b, na.rm = TRUE){
		if(is.numeric(a) != TRUE){
		stop("Non-numeric vectors are not allowed")};

		if(is.numeric(b) != TRUE){
		stop("Non-numeric vectors are not allowed")};
	
		if(length(a) != length(b)){
		stop("Incompatible vectors for inner product")};
		
		if(na.rm == TRUE){
			if((sum(is.na(a))==length(a))||(sum(is.na(b))==length(b))){
				T=NA} 
			else{
				T=sum(a*b, na.rm=TRUE)}
								};
		if(na.rm == FALSE){
				T=sum(a*b);
								};
			
			return(T)}

