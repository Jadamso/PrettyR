#------------------------------------------------------------------
##################
#' Plot Spaghetti Lines
################## 
#'
#' @param YList,Xlist output from PrettyR::spag_loess
#' @param plot_col col in lines()
#' @param lwd,... passed to lines()
#' 
#' @return nothing
#' @details a loess line for each list element
#' 
# @examples
#'
#' @export

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
#' Plot Average of Spaghetti Lines
################## 
#'
#' @param YList,Xlist output from PrettyR::spag_loess
#' @param plot_col,lwd passed to lines()
#' @param ... passed to lines()
#'  
#' @return
#' 
# @examples
# @details
#'
#' @export


spaghetti_mean <- compiler::cmpfun( function(
    YList,
    Xlist,
    plot_col=1,
    lwd=2,
    ...) {

    Y <- colMeans( do.call("rbind", Ylist) )
    X <- Xlist[[1]]

	lines( Y ~ X,
	    col=plot_col,
	    lwd=lwd,
	    ...)
} )

#------------------------------------------------------------------
##################
#' Spaghetti Plot, a loess line for each list element
################## 
#'
#' @param LoList output from PrettyR::spag_loess
#' @param pdfname full name of pdf
#' @param xlb,ylb,ttl axis names
#' @param xln,yln axis lines
#' @param xlm,ylm axis limits
#' @param plot_col color of spaghetti lines
#' @param meanline draw the mean of the spaghettis
#' @param ... passed to spaghetti_lines
#' 
#' @details seealso http://www.ats.ucla.edu/stat/r/faq/spagplot.htm
#'
#' @return
#' 
# @examples
#'
#' @export

spaghetti_plot <- compiler::cmpfun( function(
    LoList,
    pdfname,
	xlb="X",
	ylb="Y",
	ttl=NULL,
	xln=3,
	yln=3,
	xlm=NULL,
	ylm=NULL,
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


	pdf( pdfname, onefile=F, width=w, height=h)
	
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
	    
    dev.off()
})


