#' @title Convert Data to Quantiles
#' @description Function to calculate quantiles.
#' @details Used in Alpha Testing Functions
#' @param x vector to quantiled
#' @param fftile integer number of fractiles to use in splitting data
#' @return retval
#' @import tidyverse
#' @import DescTools
#' @export
ctq <- function(x, fftile){
  b<-sum(!is.na(unique(x)))
  labels<-gettextf("Q%s", 1:fftile)
  if(b>=fftile){
    qs<-round(rank(x, na.last = "keep")/sum(!is.na(x))/(1/fftile)+.4999)
    qs<-ifelse(qs<1,1,qs)
    retval<-factor(cut_interval(qs,n = fftile,labels = labels), ordered = T)
    retval<-fct_explicit_na(retval, na_level = "na")
  }else{
    retval<-factor(
      rep(NA, times = length(x)),
      levels = labels,
      ordered = T)
    retval<-fct_explicit_na(retval, na_level = "na")
  }
  return(retval)
}

#' @title Calculate Z-Score with Winsorization
#' @description Function to calculate normalized value with windsorization.
#' @details Used in Alpha Testing Functions
#' @param x a numeric vector to be winsorized and normalized.
#' @param win.prob numeric vector of probabilities with values in [0,1]
#' as used in quantile.
#' @return A vector of the same length as the original data x containing the
#' winsorized and normalized data.
#' @import tidyverse
#' @import DescTools
#' @export
ctz <- function(x, win.prob = c(0,1)){
  win.x <- DescTools::Winsorize(x = x, probs = win.prob, na.rm = T)
  norm.x <- (win.x - mean(win.x, na.rm = T)) / sd(win.x, na.rm = T)
  return(norm.x)
}

#' @title Quantile & Z-Scoring
#' @description Add Windsorized Z-Score and Quantile Score to Data
#' @details Used in Alpha Testing Functions
#' @param data dataframe containing column with data to be scored
#' @param fname character column name of factor
#' @param fftile integer number of fractiles to use in spliting data
#' @param winsor pair of numeric to bound data in windsorization
#' @return data
#' @import tidyverse
#' @import DescTools
#' @export
f_scoring <- function(data, fname, fftile, winsor = c(0,1)){
    data<-data %>%
      mutate(fzscore = scale(Winsorize(.data[[fname]], probs = winsor, na.rm = T))) %>%
      mutate(fgroup = ctq(fzscore,{{fftile}}))
  return(data)
}
