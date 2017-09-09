#------------------------------------------------------------------
##################
#' Scatter Plot with Histograms
################## 
#' 
#' @param X vector of values
#' @param Y vector of values
#' @param XBINS bin the X,Y for plotting
#' @param xbin_scale how to scale XBINS
#' @param xbks,ybks how to make histograms
#' @param col color of the plot
#' @param xlb,ylb axis lables
#' @param ttl plot title
#'
#' @return plots polygon
#' 
# @details
#' @examples scatter_gram(1:100, runif(100))
#'  
#' @export


scatter_gram <- compiler::cmpfun( function(
    X, Y,
    XBINS=NULL,
    xbin_scale=function(x){x*2+.5},
	xbks="Sturges",
	ybks="Sturges",
	col=rgb(0,0,0,.5),
	xlb="X",
	ylb="Y",
	ttl=NULL ) {


	## Binning Points
	if( !is.null(XBINS)) {
		hb  <- hexbin::hexbin(X, Y, xbins=XBINS)
		CEX <- xbin_scale(hb@count/max(hb@count))
		x   <- hb@xcm
		y   <- hb@ycm
	} else {
		x <- X
		y <- Y
	}


    ## Plot Dimensions
	#x1 <- 0
	#x2 <- 0.9
	#y1 <- 0
	#y2 <- 0.9
	#xymax <- 1
    #xymin <- .7
    

	
    zones <- matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
    layout(zones, widths=c(9/10,1/10), heights=c(1/10,9/10))
    
    ## Scatterplot, ## bottom left
	#par(fig=c(x1,x2,y1,y2)) 
    par(mar=c(3,3,1,1))
	plot.new()
    title( ttl )
    
    ## Plot Raw Points
	plot.window( range(x, na.rm=T), range(y, na.rm=T))
	points(x=x, y=y, pch=16, col=col, cex=.2)
	
	## Axis 
	axis(1)
	axis(2)	
	mtext(xlb, 1, line=2)
	mtext(ylb, 2, line=2)
	
	#par(oma=c(3,3,0,0))
    #mtext(xlb, side=1, line=1, outer=TRUE, adj=0, 
    #    at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
    #mtext(ylb, side=2, line=1, outer=TRUE, adj=0, 
    #    at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))

	## X Histogram, on top
	#par(fig=c(x1,x2,xymin,xymax), new=TRUE)
	par(mar=c(0,3,1,1))
	xhist <- hist(X, plot=FALSE, breaks=xbks)$density
    barplot(xhist,
	    axes=FALSE,
	    ylim=range(xhist, na.rm=TRUE),
	    space=0,
	    horiz=FALSE,
	    col=col,
	    border=TRUE,
	    axis.lty=1,
	    plot=TRUE) 

	## Y Histogram, on right
	#par(fig=c(xymin,xymax,y1,y2), new=TRUE)
	par(mar=c(3,0,1,1))
	yhist <- hist(Y, plot=FALSE, breaks=ybks)$density
    barplot(yhist,
        axes=FALSE,
        xlim=range(yhist, na.rm=TRUE),
        space=0,
        horiz=TRUE,
        col=col,
        border=TRUE,
	    axis.lty=1,
        plot=TRUE)

})
	
# https://stackoverflow.com/questions/8545035/scatterplot-with-marginal-histograms-in-ggplot2
# https://www.r-bloggers.com/example-8-41-scatterplot-with-marginal-histograms/
