# sandc
All general-purpose scripts developed at SandC.

Note 1: Although these are general-purpose functions, they are mostly written with a specific goal in mind and may not generalize to certain situations. You are free to adapt it to whatever purpose, or to contact me.

Note 2: Functions may be under active development and not optimized yet. In general this repository can also contain quick-fixes, not intended for mass reliable use.

#### How to use
1) Download or clone the repository to your own device.  
2) To make the function available to you within RStudio, use the R code below:

       # Add your path to the newly cloned/downloaded folder
       scriptLocation <- "Your/Path/Here/sandc"  
       
       # Find all scripts within this folder
       files <- list.files(scriptLocation, "*.R$", full.names = TRUE)  
       
       # Source all these scripts to your environment
       invisible(sapply(files, source))  
