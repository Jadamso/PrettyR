#------------------------------------------------------------------
##################
#' Plot Average Differences
##################
#'
#' @param 
#' @param
#' @param
#' 
#' @return
#' 
# @examples
# @details 
#'
#' @export

sum_plot2 <- compiler::cmpfun( function(
    pdfname, MM, ylm, xlm, ylb, xlb, 
    plot_colors=NA, years=NA, YEARS=NA, CEX=1.5,
    yvar="DIFF", xvar="X.u"){
		
	print(pdfname)

	pdf(pdfname, width=5, height=5)
		plot.new()
		plot.window(xlim=xlm, ylim=ylm)

		## Plotting Lines
		if(!is.na(YEARS[1])) {

			## Line Colors
			COLORS <- if( is.na(plot_colors[1])) {
			    years
			} else {
			    plot_colors
			}

			## Plot Lines
			for(ii in seq(MM)) {
				YEARS[ii]
				mm <- MM[[ii]]
				lines(mm[,yvar] ~ mm[,xvar], col=COLORS[ii],
				    type="b", pch=16, lwd=1)
			}
			
		} else {
			lines(MM[,yvar] ~ MM[,xvar], col=1,
			    type="b", pch=16, lwd=1)
		}
		axis(1)
		axis(2)
		mtext( xlb, 1, line=3, cex=CEX)
		mtext( ylb, 2, line=3, cex=CEX)
		    #legend("topleft", title="Bin Size", 
		    #col=1:3, lwd=1:3, legend=BINS)
	dev.off()
})


