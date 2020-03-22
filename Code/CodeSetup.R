#-------------------------------------------------------------------
##################
# Libraries
##################

## Imported Packages: (.packages())
rfiles <- c("utils", "stargazer")
for( i in rfiles) { usethis::use_package( i ) }

#sfiles <-c("parallel", "formattable")
#for( i in rfiles) { usethis::use_package( i,"Suggests", pkg=packg)}


#-------------------------------------------------------------------
##################
# Add Codes
################## 

rfile <- c(
    "SpaghettiPlot.R",
    "spag_loess.R",
    "mfxi_lm.R",
    "PolygonPlot.R",
    "Scattergram.R",
    "Scatboot.R",
    "StdError.R",
    "bin_mode.R",
    "ColorPalette.R",
    "Ttest.R",
    "TableMaker_trim.R",
    "TableMaker.R",
    "add_legend.R"
)

rfiles <- paste0(pdir,"Code/R_Code/",rfile)
    
# Move Code
file.copy(rfiles, rdir, overwrite=T )
devtools::load_all( rdir )

