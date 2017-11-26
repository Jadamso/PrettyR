#------------------------------------------------------------------
##################
#' Run Multiple Loess for Spaghetti Plot
################## 
#'
#' @param form regression formula
#' @param splitDF dataframe to regress on 
#' @param spag_idname split data frame by this id
#' @param split split up splitDF into list of dataframes?
#' @param parallel use multiple cores
#' @param ... arguments passed to loess 
#' 
#' @return list of loess outputs
#' 
# @examples
# @details 
#'
#' @export

loess_list <- compiler::cmpfun( function(
    form,
    splitDF,
    spag_idname,
    split=FALSE,
    parallel=TRUE,
    ...){

    if(split){
        splitDF <- split(splitDF, spag_idname)
    } else {
        splitDF <- splitDF
    }
    
    if(parallel){
        Lolist <- parallel::mclapply( splitDF,
            loess, formula=form, ... )
    } else {
        Lolist <- lapply( splitDF, loess, formula=form, ... )
    }
    
    return(Lolist)
})

