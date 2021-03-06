#------------------------------------------------------------------
##################
#' Custom confidence intervals
################## 
#' 
#' @param Pred predict(reg, se.fit = TRUE) object
#' @param CI Compute CI From Regression Prediction Object
#' @param Fit Compute Yhat From Regression Prediction Object
#' @param SE Compute Standard errors
#' @param level confidence level
#' @param deg degrees of freedom in t-distribution
#'
#' @return data frame of upper and lower confidence bounds 
#' 
#' @details Should first try
#'    predict(reg, interval = 'confidence', level=level) 
#'    note deg=inf coef estimates are approx normally distributed 
# @examples
#'  
#' @export

CI_lu <- compiler::cmpfun( function(
    Pred=NULL,
    CI=NULL,
    Fit=NULL,
    SE=NULL,
    level=.95,
    degf=Inf) {

	if( is.null(CI) ) { 
		if( is.null(Fit) & is.null(SE) ) {
		## Compute From Regression Prediction Object
			Fit  <- Pred$fit
			SE   <- Pred$se.fit
			# degf <- Pred$df
			# degf=Inf assumes asymptotic properties
		}

		ci_l  <- Fit + qt( (.5 - level/2), df=degf )*SE
		ci_u  <- Fit + qt( (.5 + level/2), df=degf )*SE


	} else {
	## coefficient estimates are t distributed 
	## with correct degrees of freedom
	
	# CI <- predict(reg, interval = 'confidence', level=level)
        ci_l  <- CI[,2] 
		ci_u  <- CI[,3]


	}
	ci_lu <- data.frame(ci_l, ci_u)
})



#------------------------------------------------------------------
##################
#' Polygon Plot
################## 
#' 
#' @param reg output from lm() or loess()
#' @param xname name of RHS of regression formula
#' @param level confidence levels
#' @param ... passed to predict()
#'
#' @return list with matrix of lower,upper confidence intervals and X variable
#' 
# @details
# @examples
#'  
#' @export

polygon_ci <- compiler::cmpfun( function(
    reg,
    xname,
    ...){
    
    ci_lu <- predict(reg,
        interval='confidence',
        ...)[,2:3]
    
    X <- reg$model[,xname]
    
    orx <- order(X)
    
    CI_LU <- list(ci_lu=ci_lu[orx,], X=X[orx])
    
    return( CI_LU )
    
})


#------------------------------------------------------------------
##################
#' Polygon Plot
################## 
#' 
#' @param X X matrix from polygon_ci
#' @param ci_lu matrix from polygon_ci
#' @param col,bcol color objects
#'
#' @return plots polygon
#' 
# @details
# @examples
#'  
#' @export


polygon_add <- compiler::cmpfun( function(
    X,
    ci_lu,
    col=rgb(0,0,0,.25),
    bcol=NA){

	xylst <- polygon_format(X, ci_lu)
	polygon( xylst$x, xylst$y, col=col, border=bcol)
})


#' @rdname polygon_add
#' @export
polygon_format <- compiler::cmpfun( function(
    X,
    ci_lu){

	xlst <- c( X, rev(X) )
	ylst <- c( ci_lu[,1] ,rev(ci_lu[,2]) )
	xylst <- na.omit( data.frame(x=xlst, y=ylst) )

    return(xylst)
    
})
#------------------------------------------------------------------
##################
#' Plot the polygon
################## 
#' 
#' @param reg lm object
#' @param xname name of xvariable of interest
#' @param level confidence interval range
#'
#' @return plots polygon
#' 
# @details
# @examples
#'  
#' @export

## see also http://www.ats.ucla.edu/stat/r/faq/spagplot.htm

polygon_plot <- compiler::cmpfun( function(
    reg,
    xname,
	xlb="X",
	ylb="Y",
	xlm=NULL,
	ylm=NULL,
	ttl=NULL,
	off=FALSE,
	...) {

    ## Regression CI
    reglist <- polygon_ci(reg, xname, ...)

    X     <- reglist$X
    ci_lu <- reglist$ci_lu
    
    
	xlm <- if( is.null(xlm) ) {
	    range( X, na.rm=T)
	} else { xlm }
	ylm <- if( is.null(ylm) ) { 
	    range( ci_lu, na.rm=T)
	} else { ylm }

    ## New Plot
	plot.new()
	plot.window( xlim=xlm, ylim=ylm )

	## Axis
	axis(1)
	axis(2)

	## Axis Lables
	mtext( xlb, 1)
	mtext( ylb, 2)
	title( ttl )
	
	
    ## Polygon
    polygon_add(X, ci_lu)
    
    if(off){ dev.off() }
})



