#------------------------------------------------------------------
##################
#' Run a Regression 
################## 
#'
#' @param testlist list of lmtest::coeftest objects
#' @param varnames names of variable names
#' @param stat which statistic to retrieve 
#' 
#' @return list of summary tables
#' 
# @details
# @examples
#'  
#' @export

mfxi.lm <- compiler::cmpfun(function(
    formi,
    datai,
    vcv="standard"
){
        ## Regression
        REG <- lm( formula(formi), data=datai)
            
        ## Standard Errors
        if(vcv=="standard"){
            VCOV <- vcov(reg)
        } else {
            message("unsupported")
        }
        
        ## Coefficient and Standard Errors
        SUMMARY <- lmtest::coeftest(REG, vcov=VCOV )
        return(SUMMARY)
})
    
#-------------------------------------------------------------------
##################
#' Run multiple regressions on the same dataset
##################
#'
#' @param FORMS list of lmtest::coeftest objects
#' @param varnames names of variable names
#' @param stat which statistic to retrieve 
#' 
#' @return list of summary tables
#' 
# @details
# @examples
#'  
#' @export

mfxall <- compiler::cmpfun(function(
    FORMS,
    ...,
    mc.cores=as.numeric(system("nproc", intern=TRUE)) ){
    
    if( !is.na(mc.cores) ){
        MFX <- parallel::mclapply(FORMS, mc.cores=mc.cores,
            mfxi, ...)
    } else {
        MFX <- lapply(FORMS, mfxi, ...)
    }
})



