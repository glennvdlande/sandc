is_outlier <- function(x, threshold = 1.5) {
  # This function calculates which values of a vector are outliers. 
  # Method: Boxplot method, i.e. 1st/3rd quantile -/+ 1.5 * Inter Quantile Range
  # Input:
  #   x = vector, can be data.frame column
  #   threshold = threshold to label a datapoint an outlier, default is 1.5
  # Output:
  #   outliers = boolean vector with for every element whether (TRUE) or not (FALSe) it is an outlier
  #
  # Use: outlier <- is_outlier(x, threshold)
  
  # calculate quantiles
  quantile1 <- quantile(x, 0.25)
  quantile3 <- quantile(x, 0.75)
  
  # calculate Inter quantile range
  iqr <- IQR(x)
  
  # Define lower and upper outlier thresholds
  lower <- quantile1 - threshold * iqr
  upper <- quantile3 + threshold * iqr
  
  # detect values outside of lower and upper bounds
  outlier <- x < lower | x > upper
  
  # return outlier
  return(outlier)
}
