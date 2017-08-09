## see also http://www.ats.ucla.edu/stat/r/faq/spagplot.htm

#------------------------------------------------------------------
##################
# Spaghetti Plots
################## 


loess_list <- compiler::cmpfun( function(
    form,
    DF,
    spag_idname,
    split=FALSE,
    parallel=TRUE,
    ...){

    if(split){
        splitDF <- split(DF, spag_idname)
    }
    
    if(parallel){
        Ylist <- parallel::mclapply( splitDF,
            loess, formula=form, ... )
    } else {
        Ylist <- lapply( splitDF, loess, formula=form, ... )
    }
})

#------------------------------------------------------------------
##################
# Spaghetti Plot Lines
################## 


## Plot the loess line for each list element
spaghetti_lines <- compiler::cmpfun( function(
    YList,
    Xlist,
    plot_col="#00000080",
    lwd=.5,
    ...) {
    
	if(plot_col=="#00000080") {
		COL <- rep(plot_col, length(Ylist) )
	} else if ( class(plot_col)=="function") {
		COL <- plot_col( length(Ylist) )
	} else {
	    message( "plot_col must be string like '#00000080'",
	        " or function like 'rainbow' ")
	}

	for(j in seq(Ylist)){	
		try( silent=T, 
			lines( Ylist[[j]] ~ Xlist[[j]],
			    col=COL[[j]],
			    lwd=lwd,
			    ...)
		)
	}
} )

#------------------------------------------------------------------
##################
# Spaghetti Plot Line Averages
################## 

## Plot the loess line for each list element
spaghetti_mean <- compiler::cmpfun( function(
    YList,
    Xlist,
    plot_col=1,
    lwd=2,
    ...) {
    
	if(plot_col=="#00000080") {
		COL <- rep(plot_col, length(Ylist) )
	} else if ( class(plot_col)=="function") {
		COL <- plot_col( length(Ylist) )
	} else {
	    message( "plot_col must be string like '#00000080'",
	        " or function like 'rainbow' ")
	}

    Y <- colMeans( do.call("rbind", Ylist) )
    X <- Xlist[[1]]

	lines( Ylist[[j]] ~ Xlist[[j]],
	    col=COL[[j]],
	    lwd=lwd,
	    ...)
} )

#------------------------------------------------------------------
##################
# Spaghetti Plot
################## 
## Plot the loess line for each list element

## see also http://www.ats.ucla.edu/stat/r/faq/spagplot.htm

spaghetti_plot <- compiler::cmpfun( function(
    LoList,
	xlb="X",
	ylb="Y",
	ttl=NULL,
	xln=3, yln=3,
	xlm=NULL, ylm=NULL,
	plot_col=rgb(0,0,0,.5),
	meanline=FALSE,
	...) {

    Ylist <- lapply(LoList, function(e) e$y)
    Xlist <- lapply(LoList, function(e) e$x)


	xlm <- if( is.null(xlm) ) {
	    range( Xlist, na.rm=T)
	} else { xlm }
	ylm <- if( is.null(ylm) ) { 
	    range( Ylist, na.rm=T)
	} else { ylm }


	plot.new()
	plot.window( xlim=xlm, ylim=ylm )

    ## Spaghetti Lines
	spaghetti_lines(Ylist, Xlist, plot_col=plot_col, ...)

    ## Plots the mean of the spaghetti lines
    if(meanline){
        ## currently requires all Xlist items to be identical
        spaghetti_mean(Ylist, Xlist)
    }

	## Axis
	axis(1)
	axis(2)

	## Axis Lables
	mtext( xlb, 1, line=xln)
	mtext( ylb, 2, line=yln)
	title( ttl )
})



