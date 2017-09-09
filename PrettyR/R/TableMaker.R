#------------------------------------------------------------------
##################
#' Table Formatting
################## 
#'
#' @param Xmat object from matJ
#' @param new.align
#' @param new.title
#' @param new.metatitle
#' @param new.tail
#' @param tab.out
#' @param new.notes
#' @param pb.tw
#' @param new.omit
#' @param out NULL
#' @param column.sep.width
#' @param new.digits
#' @param new.digits.extra
#' @param new.table.placement
#' @param new.other
#' @param new.parbox
#'
#' @return datatable
#' 
# @details
# @examples
#'  
#' @export


stargazerJ <- function(Xmat,
    ...,
    new.align=NULL,
    new.title=NULL,
    new.metatitle=NULL, 
    new.tail=NULL,
    tab.out=NULL,
    new.notes=NULL,
    pb.tw=1,
    new.omit=NULL, 
    out=NULL,
    column.sep.width="0pt",
    new.digits=2,
    new.digits.extra=5, 
    new.table.placement="H",
    new.other=TRUE,
    new.parbox=NULL){

    ## stargazer table
    utils::capture.output(
        TABLE <- stargazer::stargazer(Xmat,
            ...,
            table.placement=new.table.placement,
            summary=FALSE,
            no.space=TRUE,
            model.names=FALSE,
            align=FALSE,
            column.sep.width=column.sep.width,
            notes=NULL,
            out=out,
            digits=new.digits,
            digits.extra=new.digits.extra)
    )
    TABLE <- TABLE[-c(1,2)]

    ## Should work from bottom to top
    message("grab anchors")
    endtab <- grep("\\\\end\\{tabular\\}", TABLE)
    begtab <- grep("\\\\begin\\{tabular\\}", TABLE)

    ## Xmat and Table dimensions
    row_name <- (length(row.names(Xmat))>0)*1
    col_mat  <- ncol(Xmat) 
    col_tab  <- col_mat + row_name
    if( length(col_tab)<=0){ message("is matrix")}

    ## Customize Spacing 
    old.align <- paste0( rep("c", col_tab), collapse="")  
    if(!is.null(new.align)){ message("new.align")
        new.align <- new.align
    } else {
        new.align <- paste0( "l ",
            rep("c", col_tab -1), collapse="")  
    }
    TABLE <- gsub(old.align, new.align, TABLE)

    ## Customize Column Groupings 
    if(!is.null(new.metatitle)){ message("new.metatitle")
        TABLE <- append(TABLE,
            paste0(new.metatitle, " \\\\"),
            begtab+2)
        endtab <- endtab +1
        begtab <- begtab +1

    }

    ## Customize Title 
    if(!is.null(new.title)){ message("new.title")
        old.title <- gsub( " \\\\\\\\", "", TABLE[begtab+3])
        TABLE <- gsub(old.title, new.title, TABLE)
    }

    ## New Notes,  as single string
    if(!is.null(new.notes)){ message("new.notes")
        new.notes  <- paste0(
            " \\parbox{ ", pb.tw, "\\textwidth }",
            "{ \\scriptsize Notes: ", new.notes, "}")
        TABLE <- append(TABLE, new.notes, endtab)
    }

    ## New Bottoms,  as vector
    if(!is.null(new.tail)){ message("new.tail")
        TABLE <- append(TABLE, new.tail, endtab-2)
    }

    if(!is.null(new.parbox)){ message("new.parbox")
        TABLE      <- append(TABLE, "\\\\", endtab)
    }
    ## Customize Other Layout
    if(!is.null( new.other )){ message("new.other")
        TABLE <- gsub("NA", " ", TABLE )
        TABLE <- gsub("\\\\\\$", "$", TABLE)
    }
    if(!is.null( new.omit )){ message("new.omit")
        TABLE <- gsub( new.omit, " ", TABLE)
    }

    ## Save Table
    if(!is.null(out)){ message("writing ", out)
        writeLines(TABLE, out)
    }
    
    return(TABLE)
}
## utils::str(stargazer:::.stargazer.wrap)


#------------------------------------------------------------------
##################
#' Convert Matrix for stargazerJ
################## 
#' 
#' @param Xmat a matrix of summary regression information
#' @param Xrows which rows to add braces to
#' @param Xcols which columns to add braces to
#' @param Xrd how many digits to round
#' @param Xfm formatC format
#'
#' @return datatable
#' 
# @details
# @examples
#'  
#' @export

matJ <- compiler::cmpfun( function( Xmat,
    Xrows=1:nrow(Xmat),
    Xcols=1:ncol(Xmat),
    Xrd=0,
    Xfm=NULL){
    
    ## Round and Format
    X <- round( Xmat, Xrd)
    if( !is.null(Xfm) ) { X <- formatC( X , format=Xfm) }
    X[Xrows,Xcols] <- paste0( "(", X[Xrows,Xcols] , ")" )
    X[] <- paste0( "$",X[],"$" )
    X[ is.na(Xmat) ] <- ""

    #if( is.null(colnames(X) ) ){
    #    colnames(X) <- rep("", ncol(X))
    #}

    return(X)
})

#------------------------------------------------------------------
##################
#' Table Formatting for Lists 
##################
#' 
#' @rdname stargazerJlist
#' 
#' @details different handling in part ## Xmat and Table dimensions
# @examples
#'  
#' @export

stargazerJlist <- function(
    Xlist,
    ...,
    new.align=NULL, 
    new.title=NULL,
    new.metatitle=NULL, 
    new.tail=NULL,
    tab.out=NULL,
    new.notes=NULL,
    pb.tw=1,
    new.omit=NULL, 
    out=NULL,
    column.sep.width="0pt",
    new.digits=2,
    new.digits.extra=5, 
    new.table.placement="H",
    new.other=TRUE,
    new.parbox=NULL){

    ## stargazer table
    utils::capture.output(
        TABLE <- stargazer::stargazer(Xlist,
            ...,
            table.placement=new.table.placement, 
            summary=FALSE,
            no.space=TRUE,
            model.names=FALSE,
            align=FALSE,
            column.sep.width=column.sep.width,
            notes=NULL,
            out=out,
            digits=new.digits,
            digits.extra=new.digits.extra)
    )
    TABLE <- TABLE[-c(1,2)]

    ## Should work from bottom to top
    message("grab anchors")
    endtab <- grep("\\\\end\\{tabular\\}", TABLE)
    begtab <- grep("\\\\begin\\{tabular\\}", TABLE)

    ## Xmat and Table dimensions
    col_tab  <- length(Xlist) 

    ## Customize Spacing 
    old.align <- paste0( rep("c", col_tab), collapse="")  
    if(!is.null(new.align)){ message("new.align")
        new.align <- new.align
    } else {
        new.align <- paste0( "l ",
            rep("c", col_tab -1), collapse="")  
    }
    TABLE <- gsub(old.align, new.align, TABLE)

    ## Customize Title 
    if(!is.null(new.title)){ message("new.title")
        old.title <- gsub( " \\\\\\\\", "", TABLE[begtab+3])
        TABLE <- gsub(old.title, new.title, TABLE)
    }

    ## Customize Column Groupings 
    if(!is.null(new.metatitle)){ message("new.metatitle")
        TABLE <- append(TABLE, new.metatitle , begtab+2)
    }

    ## New Notes
    if(!is.null(new.notes)){ message("new.notes")
        new.notes <- paste0(
        " \\parbox{ ", pb.tw, "\\textwidth }",
        "{ \\scriptsize Notes: ", new.notes, "}")
        TABLE <- append(TABLE, new.notes, endtab)
    }

    ## New Bottoms
    if(!is.null(new.tail)){ message("new.tail")
        TABLE <- append(TABLE, new.tail, endtab-2)
    }

    if(!is.null(new.parbox)){ message("new.parbox")
        TABLE <- append(TABLE, "\\\\", endtab)
    }
    ## Customize Other Layout
    if(!is.null( new.other )){ message("new.other")
        TABLE <- gsub("NA", " ", TABLE )
        TABLE <- gsub("\\\\\\$", "$", TABLE)
    }
    if(!is.null( new.omit )){ message("new.omit")
        TABLE <- gsub( new.omit, " ", TABLE)
    }

    ## Save Table
    if(!is.null(out)){ message("writing ", out)
        writeLines(TABLE, out)
    }
    
    return(TABLE)
}
## utils::str(stargazer:::.stargazer.wrap)



#------------------------------------------------------------------
##################
# Restarring Tables
################## 

#http://stackoverflow.com/questions/36611610/inserting-stars-in-custom-stargazer-table

#for standard errors and stars that are saved in a matrix 
#(i.e. -7.25071259471702***)

restar <- function(x, out=NULL, ...){
    startab <- stargazer::stargazer(x, ...)
    undo <- gsub("\\\\textasteriskcentered", "*", startab)
    restar <- gsub("* * *", "${}^{***}$", undo, fixed = TRUE)
    restar <- gsub("* *", "${}^{**}$", restar, fixed = TRUE)
    restar <- gsub("* ", "${}^{*}$", restar, fixed = TRUE)
    if(!is.null(out)) cat(restar, file = out, sep="\n")
    restar
}





















