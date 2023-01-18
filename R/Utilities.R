#' @title Names Group Split
#' @description Splits Data by Group into List and Names
#' @details Splits Data by Group into List and Names
#' @param ... dynamic dots
#' @import tidyverse
#' @export
named_group_split <- function(...) {
  data <- group_by(...)

  names <- group_keys(data) %>%
    map(as.character) %>%
    reduce(paste, sep = "~~")

  group_split(data) %>%
    set_names(names)
}


#' @title Rselenium Launch Web
#' @description Launches RSelenium Remote
#' @details Launches RSelenium Remote
#' @param ... dynamic dots
#' @import tidyverse
#' @import RSelenium
#' @import  netstat
#' @export
launchWeb <- function(...) {
  driver <-
    rsDriver(
      browser = c("chrome"),
      version = "latest",
      chromever = "109.0.5414.74",
      port = netstat::free_port(),
      verbose = T)
  return(driver)
}



