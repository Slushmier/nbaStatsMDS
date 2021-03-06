#' Get the Oldest Basketball Player by Year
#'
#' This function returns the oldest basketball player in a year. If there
#' is more than one player with the same age, it returns one random player
#' of the oldest age.
#' @param year Input a year between 1950 and 2017. Defaults to 2017 and
#' generates a random year if input is out of range.
#' @keywords nba, statistics, age
#' @examples oldest_year(2002)
#' @export

oldest_year <- function(year = 1983) {

  if (year < 1950 | year > 2017 | is.numeric(year) == FALSE) {
    year <- sample(c(1950:2017), 1)
    print("Non-valid year. Random year generated.")}

  output <- dplyr::filter(nba, Year == year)
  output <- dplyr::filter(output, Age == max(Age))
  output <- dplyr::select(output, Year, Player, Age)

  output[sample(nrow(output), 1),]
}
