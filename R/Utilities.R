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
