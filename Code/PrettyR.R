#-------------------------------------------------------------------
##################
# Setup 
##################
library(devtools)
library(roxygen2)
library(MiscUtils)

hmdir <- path.expand("~/Desktop/Common/R_Code/")
pmdir <- path.expand("~/Desktop/Packages/")

#-------------------------------------------------------------------
##################
# Make 
##################
pack  <- "PrettyR"
pdir  <- paste0(path.expand("~/Desktop/Packages/"),pack,"/")
packg <- paste0(pdir, pack)


Version <- numeric_version("0.1.0")


# Setup R Package
source(paste0(pdir,"Code/PackageSetup.R") )

# Create R Package Contents
source(paste0(pdir,"Code/CodeSetup.R") )

pack_up(pdir)

#-------------------------------------------------------------------
##################
# Install 
##################
devtools::install(packg) ## Locally Works

devtools::install_github( paste0("Jadamso/",pack), subdir=pack)
## Public Package From Github Fails Often

citation(pack)

print("Done")

## source("~/Desktop/Packages/PrettyR/Code/PrettyR.R")

## R CMD BATCH Code/PrettyR.R && rm PrettyR.Rout && rm .Rdata
