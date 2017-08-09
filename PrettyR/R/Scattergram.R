#------------------------------------------------------------------
##################
#' Scatter Plot with Histograms
################## 


scatter_gram <- compiler::cmpfun( function(
    X, Y,
    XBINS=NULL,
    xbin_scale=function(x){x*2+.5},
	xbks,
	ybks,
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


	## Scatterplot
	par(fig=c(0,0.9,0,0.9))
	#range x1,x2,y1,y2 => bottom left
	plot.new()
	plot.window( c(0.2,1), range(Y, na.rm=T))

    ## Plot Raw Points
	points(x=x, y=y, pch=16, col=col, cex=.2)
	
	## Axis 
	axis(1)
	axis(2)	
	mtext(xlb, 1)
	mtext(ylb, 2, line=2)

	## X Histogram
	par(fig=c(0,0.9,0.65,1), new=TRUE)
	xhist <- hist(X, plot=FALSE, breaks=xbks)
    barplot(xhist$density,
	    axes=FALSE,
	    ylim=c(0, max(xhist$density, na.rm=TRUE)),
	    space=0,
	    horiz=FALSE, col=col) 

	## Y Histogram
	par(fig=c(0.75,1,0,0.9), new=TRUE)
	yhist <- hist(Y, plot=FALSE, breaks=ybks)
    barplot(yhist$density,
        axes=FALSE,
        xlim=c(0.2, max(yhist$density, na.rm=TRUE)),
        space=0,
        horiz=TRUE,
        col=col)

	title( ttl )
})

