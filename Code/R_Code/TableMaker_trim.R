#------------------------------------------------------------------
##################
#' Table Summarizing
################## 
#'
#' @param testlist list of lmtest::coeftest objects
#' @param varnames names of variable names
#' @param stat which statistic to retrieve 
#' 
#' @return list of summary tables
#' 
# @examples
#'  
#' @export

mfxtrim <- compiler::cmpfun(function(
    testlist,
    rnames,
    stat="Std. Error"){
    
    lapply(testlist, function(e) {
    
        ei  <- e[,c("Estimate", stat)]
        ri  <- sapply( rnames, function(i) {
            grep(i, rownames(ei)) } )
        ri  <- unlist(ri)
        eri <- ei[ri,]

        if (length(ri)==0){
            message("No Matches")
            eri <- NA
        }

        if (length(ri)==1){
            eri <- t(eri)
            rownames(eri) <- names(ri)
        }  

        return(eri)
    })
})

        
#------------------------------------------------------------------
##################
#' Table Formatting
################## 
#'
#' @param trimlist object from mfxtrim
#' @param nvar number of variables of interest
#' 
#' @return list of formatted summary tables
#' 
# @examples
#'  
#' @export


mfxlist2vec <- compiler::cmpfun( function(
    trimlist,
    nvar=3){

    MFXVEC <- lapply(trimlist, function(eri){
    
        tabi <- lapply(1:nrow(eri), function(i) {
            nmi <- rbind( eri[i,1], eri[i,2] )
            colnames(nmi) <- rownames(eri)[i]
            rownames(nmi) <- colnames(eri)
            return(nmi)
        })

        nna  <- nvar*2
        vi   <- c(unlist(tabi) , rep(NA,nna) )[1:nna]
        names(vi) <- unlist( lapply( rownames(eri),
            function(i) c(i, colnames(eri)[2]) ) )

        return(vi)
    })

})



#------------------------------------------------------------------
##################
# Unit Root
################## 
#' Formatting Inputs to Unit Root
#'
#' @param x sequence
#' @param b1,b2 coefficients
#' 
#' @return a vector
#' 
# @examples
#'  
#' @export


UROOT <- compiler::cmpfun( function(x, b1, b2, ... ){
    y <- cbind(x,x**2) %*% rbind( b1, b2 )
    return(y)
})


#' Formatting Inputs to Unit Root
#'
#' @param MFX matrix of coefficients 
#' @param mfxn1,mfxn2 variable names (rows)
#' @param min0 minimum of possible values
#' @param max1 maximum of possible values
#' 
#' @return datatable
#' 
#' @details each column of MFX should provide a different set of estimates
# @examples
#'  
#' @export

root <- compiler::cmpfun( function(MFX,
    mfxn1="HHI",
    mfxn2="HHI^2",
    min0=0,
    max1=1){

    lapply(1:ncol(MFX), function(i){
    
        OPT <- optimize(UROOT,
            interval=c(min0,max1),
            b1=MFX[mfxn1,i],
            b2=MFX[mfxn2,i])
            
        OPTmin <- round(OPT$minimum, 3)
        return(OPTmin)
    })
})

#------------------------------------------------------------------
##################
#' Formatting
################## 
#'
#' @param MFXall
#' @param trimnames
#' @param varnames
#' @param cnames
#' @param ROOT
#' @param nvar number of variables of interest
#' @param colrep
#' 
#' @return datatable
#' 
# @examples
#'  
#' @export



mfx <- compiler::cmpfun(function(
    MFXall,
    trimnames="PCrast5",
    varnames=c("HHI", "HHI^2"),
    cnames=c("ACLED", "UCDP"),
    ROOT=TRUE,
    nvar=3,
    colrep=2){

    MFXtrim <- mfxtrim(MFXall, trimnames)

    MFXvec  <- mfxlist2vec(MFXtrim, nvar=nvar)

    MFX <- do.call(cbind, MFXvec)

    colnames(MFX)  <- rep(cnames, colrep)

    row.names(MFX) <- rep("", nrow(MFX))
    for(i in seq(varnames) ){
        row.names(MFX)[2*i -1] <- varnames[i] }

    if( ROOT ){
        ROOT <- root(MFX, mfxn1=rnames[1], mfxn2=rnames[2])
        RET  <- rbind(MFX, ArgMin=unlist(ROOT))
    } else {
        RET <- MFX
    }

    return(RET)
})

