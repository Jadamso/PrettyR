#------------------------------------------------------------------
##################
#' Take the Mode of Data after Binning
################## 
#' @param x a numeric vector
#' @param breaks see hist
#' @return scalar
#' @examples bin_mode(runif(100))
#' @export
bin_mode <- compiler::cmpfun( function(x, breaks="Sturges") {
  hx <- graphics::hist(x, breaks, plot=F)
  m  <- hx$mids[which.max(hx$counts)]
  return(m)
} )
