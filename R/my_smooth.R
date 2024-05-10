#' Smooth data using moving averaging
#'
#' @param x A numeric vector to be smoothed
#' @param N An integer that defines the span of the moving average, it should be
#' an odd number.
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' x = rnorm(100, 0, 1)
#' y = my_smooth(x, N = 9)
#' plot(x)
#' lines(y, col = "red")
my_smooth = function(x, N) {
  # Moving average filtering function for smoothing noisy data
  # Brian Pile
  # 2022-12-23

  # # smooth moving average
  # # https://stackoverflow.com/questions/67442878/matlab-smooth-function-in-r
  # mySmooth2 <- function(y){
  #   h <- c(head(y, 1), mean(head(y, 3)))
  #   t <- c(mean(tail(y, 3)), tail(y, 1))
  #   m <- stats::convolve(y, rep(1/5, 5), type = "filter")
  #   c(h, m, t)
  # }

  # make sure N is a non-negative integer
  if (N < 0) {
    stop("N must be a non-negative integer!!!")
  }

  # make sure the number of smooth point is odd
  if (N %% 2 == 0) {
    N = N + 1
  }

  # Pad the data so the filtered output length is the same as the input.
  # Use fist and last elements of x as the padding values.
  x_pad = c(rep(x[1], floor(N/2)), x, rep(utils::tail(x, 1), floor(N/2)))

  # smoothing filter
  ysmooth = stats::filter(x_pad, rep(1/N, N))

  # remove NAs at beginning and end of filtered output
  ysmooth = ysmooth[!is.na(ysmooth)]

  return( as.numeric(ysmooth) )
}
