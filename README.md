# PrettyR

Functions to make pretty graphs and tables


## To install latest release from Github: 

    #install.packages("devtools")
    devtools::install_github("Jadamso/PrettyR",subdir="PrettyR")
    citation("PrettyR")



## Example of Tables for non-standed objects
<!--
source("~/Desktop/Common/R_Code/TableMaker.R")
stargazerJlist(c(REGS, REGS), keep="X1")

    FORMULASw <- c("Y~X1|0|0", "Y~X1|FE|0",
        "Y~X1+X2+X3+X4+X5|0|0", "Y~X1+X2+X3+X4+X5|FE|0")

    REGSw <- lapply(FORMULASw, function(form){
        reg <- lfe::felm( as.formula(form), DF)
        lmtest::coeftest(reg)
    })
-->


<!--

-->

```{r}

    library(PrettyR)
    
    #--------------------------------------------------------------
    # Setup

    k <- 5
    n <- 2000
    
    X <- sapply(seq(k), function(i) runif(n)*i)
    colnames(X) <- paste0("X", seq(k))
    
    #FE <- factor(rep( LETTERS[1:4], n/4 ) )
    FE <- factor( round(rowSums(X) ) )
    
    
    Y <- X %*% c(1,2,3,4,5)^1/2 + as.numeric(FE) + rnorm(n)
    colnames(Y) <- "Y"
    
    DF <- data.frame(Y,X, FE)
    
    #--------------------------------------------------------------
    # Regressions
    
    FORMULAS <- c("Y~X1", "Y~X1+FE",
        "Y~X1+X2+X3+X4+X5", "Y~X1+X2+X3+X4+X5+FE")
    REGS <- lapply(FORMULAS, function(form){
        reg <- lm(form, DF)  })
    
    TESTS <- lapply(REGS, function(reg){
        lmtest::coeftest(reg)   })
            
    ## REGS <- STrollR::mfxall.mc(FORMULAS,1) for vcovSCL se's

    #--------------------------------------------------------------
    # Table Formatting
  
    new.align <- 'l ll @{\\\\hspace{18pt}}ll'
    new.notes <- "All the variables are made up.
        Remember that no statistical procedure is a panacea.
        Table notes should make the table readable by itself.
        And thus may be quite quite quite long."

    new.title <- c(" & 1 & 2 & Cat & Cat")
    new.tail  <- paste0( paste( c("FE","N","Y","N","Y"), collapse=" & "), " \\\\")
    new.metatitle <- c( rep("Type 1",2), rep("Type 2",2) )

   
    stargazerJlist(
        TESTS,
        keep="X1",
        title=new.title,
        label="tab:new",
        column.sep.width="-4pt",
        new.notes=new.notes,
        new.tail=new.tail,
        new.align=new.align)

    #--------------------------------------------------------------
    # Plotting
    
    scatter_gram(DF$X1, DF$Y)
  
    polygon_plot(REGS[[1]], "X1")
    
    # rci <- polygon_ci(REGS[[2]], "X1")
    # polygon_add(rci$X, rci$ci_lu, col=rgb(0,0,1,.25) )
    ## Looks like noise here because of prediction `type`
  
```



## Caveat Emptor
This is a pre-release to the public and not gauranteed to work correctly with your code and data.

