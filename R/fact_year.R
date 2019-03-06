#' Random NBA Fact of the Year
#'
#' This function returns a random NBA fact for the input year.
#' @param year Input a year between 1950 and 2017. Defaults to 2017 and 
#' generates a random year if input is out of range.
#' @keywords nba, statistics
#' @examples fact_year(1989)
#' @export

fact_year <- function(year = 1983) {
  require(dplyr)
  if (year < 1950 | year > 2017 | is.numeric(year) == FALSE) {
    year <- sample(c(1950:2017), 1)
    print("Non-valid year. Random year generated.")}
  
  nba_0 <- nba
  nba_0[is.na(nba_0)] <- 0
  
  output <- dplyr::filter(nba_0, Year == year)
  
  output[sample(nrow(output), 1), c(1, 2, sample(3: ncol(output), 1))]
}