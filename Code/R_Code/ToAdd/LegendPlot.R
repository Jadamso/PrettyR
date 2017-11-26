#------------------------------------------------------------------
##################
#' Plot Stand-Alone Legend  
##################
#'
#' @param pdfname 
#' @param VAR variable to create legend for
#' @param subvar vector of names or columns to subset VAR
#' @param plot_colors colors
#' @param lwd,lty,CEX line style parameters
#' 
#' @return nothing
#' 
# @examples
# @details 
#'
#' @export

leg_plot <- compiler::cmpfun( function(
    pdfname, VAR, subvar, plot_colors=NA,
    lwd=2, lty=1, CEX=1) {

	if( is.na(plot_colors[1]) ) {
		plot_colors <- colorRampPalette(c('blue', 'red'),
		    alpha=.5)(length(subvar))[rank(subvar)]
	}

    VAR0 <- VAR[subvar]

	pdf(pdfname, onefile=F, height=.5, width=length(VAR0))

	par(oma=c(0,0,0,0), mar=c(0.1,0.1,0.1,0.1))
	plot(1, type = "n", axes=FALSE, xlab="", ylab="")
	legend(x = "top",
	    inset = 0,
	    horiz = TRUE,
        legend = VAR0, 
        col = plot_colors,
        lwd=lwd,
        lty=lty,
        cex=CEX )

	dev.off()
})

