#' My OLS Regression Function
#'
#' @param formula Formula type object to use for regression.
#' @param data Data.frame containing the variables from which regression will
#'   be applied.
#'
#' @return Matrix containing the estimates, standard error, t values, and
#'   individual statistical significance for each variable used in regression.
#'
#' @importFrom stats as.formula model.matrix model.frame model.response pt sd
#'
#' @examples
#' my_lm(flipper_length_mm ~ body_mass_g, data = my_penguins)
#'
#' @export
my_lm <- function(formula, data) {
  # check variables
  if (!inherits(formula, "formula")) {
    tryCatch(as.formula(formula), error = function(e) {
      stop("Variable \"formula\" must be a formula type object or be able to be
           coerced into a formula type object")
    })
    formula <- as.formula(formula)
  }
  if (!is.data.frame(data)) {
    stop("Variable \"data\" must be a data.frame type object")
  }

  # get matrix X
  mat_X <- as.matrix(model.matrix(object = formula, data = data))

  # get matrix Y
  mat_Y <- as.matrix(model.response(model.frame(formula = formula,
                                                data = data)))
  # find X^T
  mat_XT <- t(mat_X)

  # find beta_hat
  mat_XTX <- mat_XT %*% mat_X
  beta_hat <- solve(a = mat_XTX, b = (mat_XT %*% mat_Y))

  # find df
  df <- length(mat_Y) - length(beta_hat)

  # find variance
  var_hat <- 0
  for (i in 1:length(mat_Y)) {
    var_hat <- var_hat + ((mat_Y[i,] - mat_X[i,] %*% beta_hat)^2 / df)
  }
  var_hat <- as.numeric(var_hat)

  # find the standard errors
  se <- diag(sqrt(solve(a = mat_XTX,
                        b = diag(nrow(mat_XTX))) * as.numeric(var_hat)))

  # find the t-values
  t_vals <- as.vector(beta_hat) / se

  # find P(>|t|)
  p_vals <- pt(abs(t_vals), df = (length(mat_Y) - 1), lower.tail = FALSE) * 2

  # format return table
  ret_table <- matrix(data = c(as.vector(beta_hat), se, t_vals, p_vals),
                      nrow = 2,
                      dimnames = list(row.names(beta_hat),
                                      c("Estimate",
                                        "Std. Error",
                                        "t value",
                                        "Pr(>|t|)")))
  return(ret_table)
}
