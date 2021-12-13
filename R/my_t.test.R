#' My Student's T-Test
#'
#' This function performs a student's t-test on the inputted data.
#' @param x Numeric vector containing data to perform the test on.
#' @param alternative String with the type of t-test to use (two.sided,
#'   less, greater).
#' @param mu Numeric with the expected mean of the data \code{x}.
#'
#' @return List with the test stat (\code{test_stat}), df (\code{df}),
#'   alternative (\code{alternative}), and p values (\code{p_val}).
#'
#' @examples
#' my_t.test(my_penguins$bill_depth_mm, alternative = "two.sided", mu = 15)
#' my_t.test(my_penguins$body_mass_g, alternative = "less", mu = 4000)
#'
#' @importFrom stats sd pt
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # check input variables
  if(!is.vector(x) | !is.numeric(x)) {
    stop("Variable \"x\" must be a numeric vector.")
  }
  if(alternative != "two.sided" &
     alternative != "less" &
     alternative != "greater") {
    stop("Variable \"alternative\" must be either \"two.sided\", \"less\",
         or \"greater\".")
  }
  if(!is.numeric(mu)) {
    stop("Variable \"mu\" must be numeric.")
  }

  # find sample size
  n <- length(x)

  # find degrees of freedom
  df <- n - 1

  # find sample standard deviation
  s <- sd(x) / sqrt(n)

  # find sample mean
  m <- mean(x)

  # find t statistics
  test_stat <- (m - mu) / s

  # find p-value
  p_val <- NA
  if (alternative == "two.sided") {
    p_val <- pt(q = abs(test_stat), df = df, lower.tail = FALSE) * 2
  } else if (alternative == "less") {
    p_val <- pt(q = test_stat, df = df, lower.tail = TRUE)
  } else {
    p_val <- pt(q = test_stat, df = df, lower.tail = FALSE)
  }

  # return the list with test_stat, df, alternative, and p_val
  return(list(
    test_stat = test_stat,
    df = df,
    alternative = alternative,
    p_val = p_val
  ))
}
