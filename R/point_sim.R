#' Creates a systematic grid of points.
#'
#' Creates a systematic grid of points. This function is only used
#' in simulating data sets.
#'
#' @param lower_x_lim the lower limit for x-coordinate, default is 0
#' @param upper_x_lim the upper limit for x-coordinate, default is 1
#' @param lower_y_lim the lower limit for y-coordinate, default is 0
#' @param upper_y_lim the upper limit for y-coordinate, default is 1
#' @param ncol the number of cols in the systematic grid, default is 10
#' @param nrow the number of rows in the systematic grid, default is 10

#' @return A data.frame with x- and y-coordinates of simulated locations
#' @author Jay Ver Hoef
#' @export

pointSimSyst <- function (nrow = 10, ncol = 10, lower_x_lim = 0, upper_x_lim = 1,
                          lower_y_lim = 0, upper_y_lim = 1)
{
  x_range <- upper_x_lim - lower_x_lim
  y_range <- upper_y_lim - lower_y_lim
  y_mat <- lower_y_lim + y_range * (nrow - matrix(rep(1:nrow,
                                                      times = ncol), nrow = nrow, ncol = ncol))/(nrow) + y_range/(2 *
                                                                                                                    nrow)
  x_mat <- lower_x_lim + x_range * (t(matrix(rep(1:ncol, times = nrow),
                                             nrow = ncol, ncol = nrow)) - 1)/(ncol) + x_range/(2 *
                                                                                                 ncol)
  data.frame(x = matrix(x_mat, ncol = 1), y = matrix(y_mat,
                                                     ncol = 1))
}

#' simulate completely spatially random point patterns.
#'
#' simulates a completely spatially random point patterns. This function is
#' only used in simulating data sets.
#'
#' @param npoints number of points to add that are completely spatially random (CSR), default = 100
#' @param lower_x_lim left limit of boundary, default = 0
#' @param upper_x_lim right limit of boundary, default = 1
#' @param lower_y_lim lower limit of boundary, default = 0
#' @param upper_y_lim upper limit of boundary, default = 1
#'
#' @return data.frame of two columns, x-coordinate in the first, and y-coordinate in the second.
#'
#' @author Jay Ver Hoef

pointSimCSR <- function(npoints = 100, lower_x_lim = 0, upper_x_lim = 1,
                        lower_y_lim = 0, upper_y_lim = 1)
{
  x_range <- upper_x_lim - lower_x_lim
  y_range <- upper_y_lim - lower_y_lim
  tibble::data_frame(x=lower_x_lim + runif(npoints)*x_range,
                     y=lower_y_lim + runif(npoints)*y_range)
}
