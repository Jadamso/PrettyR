#------------------------------------------------------------------
##################
#' M out of N Bootstrapped Loess Confidence Intervals
################## 
#'
#' @param x,y coordinates
#' @param breps number of bootstrap replications
#' @param mfun function to define m out of n subsample
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
    breps=100, mfun=function(m){m^(.9)},
    confidence=0.9, family="gaussian",
    degree=2, span=2/3){

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
    PREDS <- parallel::mclapply( seq(breps), function(i, len=length(dat$x)){
        m    <- mfun(len)
        ndx  <- sample.int(len, size=m, replace=TRUE)
        
        fit  <- loess(y[ndx]~x[ndx], degree=degree,
                   span=span, family=family)
        pred <- predict(fit, newdata=x.out)
        return(pred)
    } )
    
    pred <- as.data.frame( do.call("rbind", PREDS) )
    
    
    # Calculate limits and standard deviation   
    # Too few good values to get estimate?
    n.na <- apply(is.na(pred), 2, sum)  # num NAs in each column
    n.na.test <- !(n.na > breps*(1.0-confidence) ) 
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
        breps=breps,
        confidence=confidence,
        degree=degree,
        span=span,
        family=family,
        data=dat,
        fit=pfit)
        
    return( ret_list)
})


