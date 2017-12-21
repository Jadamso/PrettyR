#-------------------------------------------------------------------
##################
# Libraries
##################

## Imported Packages: (.packages())
rfiles <- c("utils", "stargazer", "parallel")
for( i in rfiles) {
    devtools::use_package( i, pkg=packg)
}

# devtools::use_package( i, "Suggests", pkg=pdir)}

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
    "TableMaker_trim.R",
    "TableMaker.R"
)

rfiles <- paste0(pdir,"Code/R_Code/",rfile)
    
# Move Code
file.copy(rfiles, rdir, overwrite=T )
devtools::load_all( rdir )

# Create Code Documentation
devtools::document( pkg=packg)


