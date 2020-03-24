describeData <- function(df, columns) {
  # This function create a descriptive dataframe with any number of columns as "group-by"-input. 
  # All columns not in the "columns" input will be described.
  # Input:
  #   df = R data.frame that needs to be described.
  #   columns = string-vector of columns that are used as "group-by" columns. 
  #             Can be 'all' in which case columns will just be summarized.
  # Output:
  #   output = R data.frame that describes the data grouped by the "columns" input
  #
  # Use: output <- describeData(df, columns)
  
  # attach packages
  library("psych")
  library("dplyr")
  
  if (columns == 'all') {
    nam <- colnames(df)
    df <- data.frame(cbind(df,matrix('all', nrow(df),1)))
    colnames(df)<- c(nam, 'all')
  }
  
  # get unique combinations of columns
  combinations <- data.frame(unique(df[,columns]), stringsAsFactors = FALSE)
  
  # check if output exists, if so delete
  if (exists("output")) {
    rm("output")
  }
  
  # loop over combinations to get statistics
  leftOut <- colnames(df)[!(colnames(df) %in% columns)]
  for (iCombi in 1:nrow(combinations)) {
    # create temporary DF for convenience
    if (length(columns) == 1) {
      tempDf <- df[df[,columns] == combinations[iCombi,], leftOut]
    } else {
      tempDf <- inner_join(df, combinations[iCombi,], by = columns)
      tempDf <- tempDf[,names(tempDf)[!names(tempDf) %in% columns]]
    }

    
    # calculate statistics    
    # add calculation function for standard error first
    calcSE <- function(x) sqrt(var(x[!is.na(x)])/length(x[!is.na(x)]))
    # turn off warnings to deal with great amounts during large dataset description
    oldw <- getOption("warn")
    options(warn = -1)
    ns         <- apply(tempDf,2, function(x) length(which(!is.na(x))))
    means      <- apply(tempDf,2, mean, na.rm = TRUE) 
    sds        <- apply(tempDf,2, sd, na.rm = TRUE) 
    medians    <- apply(tempDf,2, median, na.rm = TRUE)
    trims      <- apply(tempDf,2, mean, na.rm = TRUE, trim = 0.2)
    mads       <- apply(tempDf,2, mad, na.rm = TRUE)
    mins       <- apply(tempDf,2, min, na.rm = TRUE)
    maxs       <- apply(tempDf,2, max, na.rm = TRUE)
    ranges     <- apply(tempDf,2, range, na.rm = TRUE)
    skews      <- apply(tempDf,2, skew, na.rm = TRUE)
    kurtosiss  <- apply(tempDf,2, kurtosi, na.rm = TRUE)
    ses        <- apply(tempDf,2, calcSE)
    options(warn = oldw)
    
    # character conversion for ease of use
    cc <- combinations[iCombi,]
    cc[] <- lapply(combinations[iCombi,], as.character)

    # merge into dataframe
    availCols <- leftOut[ns > 0]
    statDf <- matrix(NA, nrow = length(availCols), ncol = length(columns) + 12)
    for (iRow in which(ns>0)) { 
      statDf_idx <- which(iRow == which(ns>0))
      statDf[statDf_idx,] <- c(paste0(cc), ns[iRow], means[iRow], sds[iRow], medians[iRow],
                               trims[iRow], mads[iRow], mins[iRow], maxs[iRow], ranges[iRow],
                               skews[iRow], kurtosiss[iRow], ses[iRow])
    }
    
    # convert to data frame and add column and row names
    statDf <- data.frame(statDf, availCols)
    colnames(statDf) <- c(columns, "n", "mean", "sd", "median", "trimmed", "mad", "min", "max",
                          "range", "skew", "kurtosis", "se", "var")
    row.names(statDf) <- availCols
    
    # merge into big output dataframe
    if (exists("output")) {
      output <- rbind(output,statDf)
    } else {
      output <- statDf
    }
  }
  
  # trim data with zero cases
  output <- output[output$n !=0,]
  
  return(output)
}
