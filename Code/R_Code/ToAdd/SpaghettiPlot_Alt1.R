#------------------------------------------------------------------
##################
#' Summary Spaghetti Plot
##################
#'
#' @param pdfname full name of pdf
#' @param Y output variable of interest
#' @param X input variable of interest
#' @param xs subset of X to plot over
#' @param xlb,ylb axis names
#' @param years for which years of data
#' @param plot_colors colors
#' @param w,h pdf width and height
#' 
#' @return
#' 
# @examples
# @details 
#'
#' @export

sum_plot <- compiler::cmpfun( function( 
    pdfname, Y, ylb, xlb, years, X, xs,
    plot_colors=NA, w=5, h=5, CEX=1.5) {
    
	pdf( pdfname, onefile=F, width=w, height=h)

		#par(fig=c(0,1,0,0.95), new=F)
		xlm <- range(X[xs], na.rm=T)
		ylm <- range(Y[years], na.rm=T)
		
		plot.new()
		plot.window( xlm, ylm)
		
		## Line Colors
		COLORS <- if( is.na(plot_colors) ) {
		    years
		} else {
		    plot_colors
		}

		## Lines
		for( i in years ){
			col  <- COLORS[i]
			lines( y=Y[[i]], x=X[xs], lwd=2, type="l", col=col)
		}

		## Axis
		axis(1, at=unique(round(X,1)))
		axis(2)	
		mtext(xlb, 1, line=3, cex=CEX)
		mtext(ylb, 2, line=3, cex=CEX)

	dev.off()
})

## Points
#points(x=hb@xcm, y=hb@ycm, pch=21, col=NULL, bg=rgb(1,0,0,.5), cex=CEX)
#points(x=hb@xcm, y=hb@ycm, pch=16, col="darkred", cex=.2)

## LOESS
#polygon( c( R[,1],rev(R[,1]) ), c( R[,3],rev(R[,4]) ) ,col=rgb(1,0,0,.5), border=NA)
#lines( R[,1], R[,2], col="red")
