#' @title Alpha Testing Pre-Processing
#' @description Pre-Process Data for Alpha Testing
#' @details This function pre-processes factor and return data to be
#' used in AT functions. Inputs raw data and returns \cr
#' ID columns \cr
#' factor value \cr
#' factor quantile \cr
#' normalized factor value and \cr
#' normalized returns
#' @param data dataframe containing id columns, return column and factor value column
#' @param iname character column names of identifiers
#' @param fname character column name of factor
#' @param rname character column name of return
#' @param fftile integer number of fractiles to use in splitting data
#' @param winsor pair of numeric to bound data in windsorization
#' @return dataframe containing id columns,
#' "return", "factor.value","factor.q", "factor.z", "return.z"
#' @import tidyverse
#' @import DescTools
#' @export
ATPreProcess <- function(
    data,
    iname,
    fname,
    rname,
    fftile = 5,
    win.prob = c(0,1)){
  # Remove unnecessary columns from data
  data.raw <- select(data, all_of(iname), all_of(rname), all_of(fname))
  # Calculate Normalized and Quantiled Data
  data.adj <-
    data.raw %>%
    rename(
      `return` = {{rname}},
      `factor.value` = {{fname}}) %>%
    mutate(
      `factor.q` = ctq(`factor.value`, {{fftile}}),
      `factor.z` = ctz(`factor.value`, {{win.prob}}),
      `return.z` = ctz(`return`))
  return(data.adj)
}

#' @title Alpha Testing -
#' Single Return Horizon, Single Factor, Single Period
#' @description This is the most baseline AT function.
#' @details This function returns for the universe and each quantile:
#'      number of observations \cr
#'      average return \cr
#'      average relative return for quantiles \cr
#'      security level hit rate of universe vs 0 \cr
#'      security level hit rate of universe vs universe average \cr
#'      information coefficient based on Z-Score of factor \cr
#'      information coefficient based on rank of factor \cr
#'      quantile spread between top and bottom quantile \cr
#'      regression alpha - universe only \cr
#'      regression beta - universe only
#' @param data dataframe containing id columns, return column and factor value column
#' @return None
#' @import tidyverse
#' @import DescTools
#' @import reshape2
#' @export
AlphaTestSRSFSP<-function(data){

  reg.factor <- tryCatch(
    {
      reg <- lm(`return.z` ~ `factor.z`, data = data)
      c("alpha" = reg$coefficients[[1]],
        "beta" = reg$coefficients[[2]])
      },
    error = function(cond){
      return(
        c("alpha" = as.numeric(NA),
          "beta" = as.numeric(NA)))})

  # Universe level statistics
  data.universe <-
    data %>%
    summarize(
      u_n = n(),
      u_return_avg = mean(`return`, na.rm = T),
      u_hitrate_z = sum(ifelse(`return` > 0, 1, 0), na.rm = T)/u_n,
      u_hitrate_u = sum(ifelse(`return` > u_return_avg, 1, 0), na.rm = T)/u_n,
      u_ic_score = cor(
        `factor.z`,
        `return.z`,
        use = "pairwise.complete.obs"),
      u_ic_rank = cor(
        rank(`factor.value`),
        rank(`return`),
        use = "pairwise.complete.obs"),
      u_alpha = !!reg.factor["alpha"],
      u_beta = !!reg.factor["beta"])

  # Quantile level statistics
  data.quantile <-
    data %>%
    group_by(`factor.q`) %>%
    summarize(
      n = n(),
      return_avg = mean(`return`, na.rm = T),
      return_rel = return_avg - !!data.universe$u_return_avg,
      med_return = median(`return`, na.rm = T),
      hit_rate_z = sum(ifelse(`return`>0,1,0), na.rm = T)/n(),
      hit_rate_u = sum(ifelse(`return`>!!data.universe$u_return_avg,1,0), na.rm = T)/n(),
      .groups = "drop")

  # Add NA Factor Level if Not Present
  if(length(which(factor(data.quantile$factor.q)=="na")) == 0){
    data.quantile <- data.quantile %>%
      add_row(`factor.q` = "na", `n` = 0) %>%
      mutate(
        `factor.q` = forcats::fct_explicit_na(
          `factor.q`,
          na_level = "na"))}

  # Add Non-NA Factor Levels if Not Present
  if(length(which(factor(data.quantile$factor.q) != "na")) == 0){

    fct <- levels(data.quantile$factor.q)
    fct <- fct[!fct %in% "na"]

    data.quantile <- data.quantile %>%
      add_row(`factor.q` = fct, `n` = 0) %>%
      mutate(
        `factor.q` = forcats::fct_explicit_na(`factor.q`, na_level = "na"))}


  # Calculate QN-Q1
  if(length(which(factor(data.quantile$factor.q) != "na")) > 1){
    lvl.all <- levels(data.quantile$factor.q)
    lvl <- lvl.all[!lvl.all %in% "na"]
    qn <- tail(lvl, n = 1)
    q1 <- lvl[1]
    q.spread <-
      data.quantile$return_avg[data.quantile$factor.q == qn] -
      data.quantile$return_avg[data.quantile$factor.q == q1]
  }else{
    q.spread <- as.numeric(NA)
  }

  # Add Q spread to universe data
  data.universe <- add_column(data.universe, q_spread =  q.spread)

  # Flatten Quantile Data
  data.quantile.flat <- data.quantile %>%
    pivot_longer(c(everything(), -`factor.q`)) %>%
    arrange(`factor.q`) %>%
    pivot_wider(
      names_from = c(`name`, `factor.q`),
      values_from = `value`,
      names_sep = ".") %>%
    select(
      starts_with("n."),
      starts_with("return_avg"),
      starts_with("return_rel"),
      starts_with("med_return"),
      starts_with("hit_rate_z"),
      starts_with("hit_rate_u"))

  # Bind universe data with quantile data
  tempret <- cbind(data.universe, data.quantile.flat)
  return(tempret)
}

#' @title Alpha Testing -
#' Single Return Horizon, Single Factor, Multiple Periods
#' @description Runs AT_sr_sf_sp for multiple periods
#' @details This function returns for the universe and each quantile for each period: \cr
#'       number of observations\cr
#'       average return\cr
#'       average relative return for quantiles\cr
#'       security level hit rate of universe vs 0\cr
#'       security level hit rate of universe vs universe average\cr
#'       information coefficient based on Z-Score of factor\cr
#'       information coefficient based on rank of factor\cr
#'       quantile spread between top and bottom quantile\cr
#' @param data dataframe containing return column and factor value column
#' @param fname character column name of factor
#' @param rname character column name of return
#' @param fftile integer number of fractiles to use in spliting data
#' @import tidyverse
#' @import lubridate
#' @import magrittr
#' @import DescTools
#' @import reshape2
#' @export
AlphaTest<-function(
    data,
    pname,
    iname,
    fname,
    rname,
    fftile = 5,
    win.prob = c(0,1))
  {

  # Pivot Data Longer
  data <-
    data %>%
    arrange(all_of(pname)) %>%
    select(all_of(pname), all_of(iname), all_of(fname), all_of(rname)) %>%
    pivot_longer(
      cols = all_of(fname),
      names_to = "factor.name",
      values_to = "factor.value") %>%
    pivot_longer(
      cols = all_of(rname),
      names_to = "return.period",
      values_to = "return.value") %>%
    named_group_split(`factor.name`) %>%
    map(., \(x) named_group_split(x, `return.period`)) %>%
    map_depth(., 2, \(x) named_group_split(x, `Periods`))

  # Adjust Data
  data.adj <-
    map_depth(
      data,
      3,
      ATPreProcess,
      iname = all_of(iname),
      fname = "factor.value",
      rname = "return.value",
      fftile = all_of(fftile),
      win.prob = all_of(win.prob))

  # Alpha Test Data
  data.AT <-
    data.adj %>%
    map_depth(., 3, AlphaTestSRSFSP) %>%
    map_depth(., 2, bind_rows, .id = "Periods")

  out <- list("data.adj" = data.adj, "AT" = data.AT)
  return(out)
}
