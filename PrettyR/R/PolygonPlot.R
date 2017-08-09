#------------------------------------------------------------------
##################
# Custom confidence intervals
################## 

# Should first try: predict(reg, interval = 'confidence', level=level) 

CI_lu <- compiler::cmpfun( function(
    Pred=NULL,
    CI=NULL,
    Fit=NULL,
    SE=NULL,
    level=.95,
    degf=Inf) {

	## Symmetric Confidence Interval

	if( is.null(CI) ) { 
	## coefficient estimates are approximately normally distributed 
		if( is.null(Fit) & is.null(SE) ) {
		## Compute From Regression Prediction Object
		# Pred <- predict(reg, se.fit = TRUE)

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
# Polygon Plot
################## 

polygon_ci <- compiler::cmpfun( function(
    reg,
    xname,
    level, 
    ...){
    
    ci_lu <- predict(reg,
        interval='confidence',
        level=level)[,2:3]
    
    X <- reg$model[,xname]
    
    return( list(ci_lu, X) )
    
})


#------------------------------------------------------------------
##################
# Polygon Plot
################## 


polygon_add <- compiler::cmpfun( function(
    X,
    ci_lu,
    col=rgb(0,0,0,.25),
    bcol=NA,
    ...){

	xlst <- c( X, rev(X) )
	ylst <- c( ci_lu[,1] ,rev(ci_lu[,2]) )
	xylst <- na.omit( data.frame(x=xlst, y=ylst) )

	polygon( xylst$x, xylst$y, col=col, border=bcol, ...)
})

#------------------------------------------------------------------
##################
# Spaghetti Plot
################## 
## Plot the loess line for each list element

## see also http://www.ats.ucla.edu/stat/r/faq/spagplot.htm

polygon_plot <- compiler::cmpfun( function(
    reg,
    xname,
    level,
	xlb="X",
	ylb="Y",
	xlm=NULL,
	ylm=NULL,
	ttl=NULL,
	...) {


    ## Regression CI
    reglist <- polygon_ci(reg, xname, level)

    X <- reglist$X
    ci_lu <- reglist$ci_lu
    
    
	xlm <- if( is.null(xlm) ) {
	    range( X, na.rm=T)
	} else { xlm }
	ylm <- if( is.null(ylm) ) { 
	    range( ci_lu, na.rm=T)
	} else { ylm }


	plot.new()
	plot.window( xlim=xlm, ylim=ylm )

    ## Spaghetti Lines
    polygon_add(X, ci_lu, ...)

	## Axis
	axis(1)
	axis(2)

	## Axis Lables
	mtext( xlb, 1)
	mtext( ylb, 2)
	title( ttl )
})



