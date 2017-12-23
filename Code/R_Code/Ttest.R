#------------------------------------------------------------------
##################
#' Welchs t-test
################## 
#'
#' @param m1,m2 the sample means
#' @param s1,s2 the sample standard errors
#' @param n1,n2 the sample sizes
#' @param m0 the null value for the difference in means to be tested for. Default is 0. 
#' @param side upper, lower, or 2sided
#' 
#' @return a summary of the T-test
#'
#' @ details see http://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
#' @examples 
#' n1 <- 100
#' x1 <- 1:n1
#' y1 <- 3*x1+ rnorm(n1, 0, 50)
#' lm1 <- summary( lm( y1~x1) )
#' coefs1 <- coef(lm1)[2,1:2]
#' n2 <- 200
#' x2 <- 1:n2
#' y2 <- 2*x2 + rnorm(n2, 0, 50)
#' lm2 <- summary( lm( y2~x2) )
#' coefs2 <- coef(lm2)[2,1:2]
#' Ttest2( coefs1[1], coefs2[1], coefs1[2], coefs2[2], n1, n2  )
#' 
#' @aliases t.test2
#'
#' @export
Ttest2 <- compiler::cmpfun( function(m1, m2, s1, s2, n1, n2, side=2, m0=0) {
	requireNamespace("stats")

	## Calculate TVALUES
	B      <- (m1 - m2) - m0
	SE     <- sqrt(s1**2 + s2**2)
	TVAL   <- B/ SE

	# Calculate PVALUES
    # welch-satterthwaite df
    DF     <- ( (se1**2 + se2**2)^2 )/( (se1**2)^2/(n1-1) + (se2**2)^2/(n2-1) )

	## Pvalue calculation
		# pvalue = P(t>t_hat) = 1- P(t < t_hat)
		## since t has symmetric distrbutio pvalue=P(-t < -t_hat)
	
	## 2sided
	if( side==2 ) { PVAL <- 2*stats::pt(-abs(TVAL),DF) }
    ## 1sided: t > TVAL
	if( side=="u"){ PVAL <- stats::pt(-TVAL, DF) }
    ## 1sided: t < TVAL
	if( side=="l"){ PVAL <- stats::pt(TVAL, DF) }
	
	## Output
	DAT <- c(m1-m2, SE, TVAL, PVAL)    
	names(DAT) <- c("Difference of Means", "Std. Error", "t value", "p value")
	#Pr(>|t|)
	
	return(DAT) 
})


#------------------------------------------------------------------
##################
#' Welchs t-test, variant3
################## 

#TVAL_bot <- sqrt(var1/N1 + var2/N2)
#var1/N1 <- se1**2
#var2/N2 <- se2**2
#sd1/N1 <- se1**2
#sd1**2/N1 <- se1**2
#sd2**2/N2 <- se2**2
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE. 

#' @rdname Ttest2
Ttest3 <- compiler::cmpfun( function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE){
	requireNamespace("stats")
    if( equal.variance==FALSE )  {
        se <- sqrt( (s1^2/n1) + (s2^2/n2) )
        # welch-satterthwaite df
        df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
    } else {
        # pooled standard deviation, scaled by the sample sizes
        se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
        df <- n1+n2-2
    }
    
    dat <- c(m1-m2, se, (m1-m2-m0)/se, 2*stats::pt(-abs(t),df))    
    names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
    return(dat) 
} )


