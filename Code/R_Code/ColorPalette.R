#------------------------------------------------------------------
##################
#' My Color Palette
################## 
#' @param n integer number of colors
#' @export
#' @example spatstat::colourmap(mypal(20), breaks=seq(0,1,by=.05))
mypal <- compiler::cmpfun( function(n){
    colorRampPalette(c('dark red','white','dark blue'))(n)
})

