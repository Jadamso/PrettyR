#------------------------------------------------------------------
##################
#' Welchs T-test for a Y variable calculated from a window on each side of x0
################## 
#'
#' @param wind fraction (percent of data to include)
#' @param dframe data to cut
#' @param x0 
#' @param yvar
#' @param xvar
#' 
#' @return list of loess outputs
#' 
# @examples
# @details 
#'
#' @export


Wstat <- compiler::cmpfun( function(
    wind, dframe, x0, yvar, xvar, corr=0, ...){
    
	window <- wind*diff(range(dframe[,xvar]))
	x1 <- x0-window
	x2 <- x0+window
	
	df1 <- dframe[ which( dframe[,xvar]< x0  & dframe[,xvar] > x1 ) , ]
	df2 <- dframe[ which( dframe[,xvar]>= x0 & dframe[,xvar] < x2 ) , ]
	
	u1  <- mean(df1[,yvar], na.rm=TRUE)
	u2  <- mean(df2[,yvar], na.rm=TRUE)
	U   <- u1 - u2
	
	v1  <- stats::var(df1[,yvar], na.rm=TRUE)/nrow(df1)
	v2  <- stats::var(df2[,yvar], na.rm=TRUE)/nrow(df2)
	V   <- sqrt(v1 + v2 - 2*corr*sqrt(v1)*sqrt(v2) )
	
	T_VAL <- U/V
	return( c(U, T_VAL) )
} )
