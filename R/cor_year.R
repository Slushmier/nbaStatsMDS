#' Correlation of Basketball Statistics
#'
#' This function returns the oldest basketball player in a year. If there
#' is more than one player with the same age, it returns one random player
#' of the oldest age.
#' @param year Input a year between 1950 and 2017. Defaults to 2017 and
#' generates a random year if input is out of range.
#' @param corrgram If you want a corrgram (requires "corrgram" package) returned,
#' set parameter to TRUE. Defaults to FALSE.
#' @keywords nba, statistics, age
#' @examples cor_year(2004)
#' @examples cor_year(2005, corrgram = TRUE)
#' @export

cor_year <- function(year = 1983, corrgram = FALSE) {

  if (year < 1950 | year > 2017) {
    year <- sample(c(1950:2017), 1)
    print("Non-valid year. Random year generated.")}

  output <- dplyr::filter(nba, Year == year)
  output <- dplyr::select(output, -Year) # Manually removed since it's numeric
  output <- purrr::keep(output, is.numeric)

  if ("corrgram" %in% rownames(installed.packages()) == TRUE && corrgram == TRUE) {
    require(corrgram)
    corrgram::corrgram(output,
                       main = paste("Correlation of NBA Stats in", as.character(year)))

  } else {cor(output, use = "complete.obs")}

}
