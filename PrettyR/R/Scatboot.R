#------------------------------------------------------------------
##################
#' Bootstrapped Loess Confidence Intervals
################## 
#'
#' @param x,y coordinates
#' @param nreps number of bootstrap replications 
#' @param confidence CI interval
#' @param degree,span,family  loess parameters
#' 
#' @return list of loess outputs
#' 
# @examples
#' @details see "http://content.csbs.utah.edu/~rogers/datanal/R/scatboot.r"
#'
#' @export

scatboot <- compiler::cmpfun( function(
    x, y,
    nreps=100, confidence=0.9,
    degree=2, span=2/3, family="gaussian"){

    # Put input data into a data frame, sorted by x, with no missing
    # values.
    dat <- na.omit(data.frame(x=x,y=y))
    if(nrow(dat) == 0) {
        print("Error: No data left after dropping NAs")
        print(dat)
        return(NULL)
    }
    dat <- dat[order(dat$x),]

    # Establish a series of x values for use in fits
    r <- range(dat$x, na.rm=T)
    x.out <- seq(r[1], r[2], length.out=40)

    # Fit curve to data
    f <- loess(y~x, data=dat, degree=degree, span=span,
               family=family)
    y.fit <- approx(f$x, fitted(f), x.out,rule=2)$y

    # Generate bootstrap replicates
    PREDS <- parallel::mclapply( seq(nreps), function(i, len=length(dat$x)){
        ndx  <- sample(len,replace=T)
        fit  <- loess(y[ndx]~x[ndx], degree=degree,
                   span=span, family=family)
        pred <- predict(fit, newdata=x.out)
        return(pred)
    } )
    
    pred <- as.data.frame( do.call("rbind", PREDS) )
    
    
    # Calculate limits and standard deviation   
    # Too few good values to get estimate?
    n.na <- apply(is.na(pred), 2, sum)  # num NAs in each column
    n.na.test <- !(n.na > nreps*(1.0-confidence) ) 
    pred <- pred[, n.na.test]
    
    # effective confidence excluding NAs
    pr   <- 0.5*(1.0 - confidence)
    up.low.lim <- apply(pred, 2, quantile, c(1.0-pr, pr), na.rm=T)
    stddev     <- apply(pred, 2, sd, na.rm=T)

    # Output
    pfit <- as.data.frame( cbind(
        x=x.out[n.na.test],
        y.fit=y.fit[n.na.test],
        up.lim=up.low.lim[1,],
        low.lim=up.low.lim[2,],
        stddev=stddev) )
    
    ret_list <- list(
        nreps=nreps,
        confidence=confidence,
        degree=degree,
        span=span,
        family=family,
        data=dat,
        fit=pfit)
        
    return( ret_list)
})


#------------------------------------------------------------------
##################
#' Welchs T-test for a Y variable calculated from a window on each side of x0
################## 
#'
#' @param wind fraction (percent of data to include)
#' @param dframe data to cut
#' @param x0 
#' @param yvar
#' @param xvar
#' 
#' @return list of loess outputs
#' 
# @examples
# @details 
#'
#' @export


Wstat <- compiler::cmpfun( function(
    wind, dframe, x0, yvar, xvar, corr=0, ...){
    
	window <- wind*diff(range(dframe[,xvar]))
	x1 <- x0-window
	x2 <- x0+window
	
	df1 <- dframe[ which( dframe[,xvar]< x0  & dframe[,xvar] > x1 ) , ]
	df2 <- dframe[ which( dframe[,xvar]>= x0 & dframe[,xvar] < x2 ) , ]
	
	u1  <- mean(df1[,yvar], na.rm=TRUE)
	u2  <- mean(df2[,yvar], na.rm=TRUE)
	U   <- u1 - u2
	
	v1  <- stats::var(df1[,yvar], na.rm=TRUE)/nrow(df1)
	v2  <- stats::var(df2[,yvar], na.rm=TRUE)/nrow(df2)
	V   <- sqrt(v1 + v2 - 2*corr*sqrt(v1)*sqrt(v2) )
	
	T_VAL <- U/V
	return( c(U, T_VAL) )
} )
