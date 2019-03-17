
#' Dataframe time resampling.
#'
#' Function for timeseries data resampling
#' similar to pandas.DataFrame.resample(). It works
#' with both numeric and datetime timelines. The timeline
#' has to be placed in the first column of the dataframe.
#'
#' @param df Dataframe to be resampled, first column is the timeline
#' @param new_t vector, new timeline
#' @return Resampled Dataframe
#' @examples
#' # Example with numeric timeline
#' df <- data.frame(time = seq(0, 1800, by = 600))
#' df$x1 <- 1:nrow(df)
#' df$x2 <- 1:nrow(df) * 10
#'
#' new_t <- c(0, 60, 120, 500, 600, 1800)
#'
#' new_df <- resample_df(df, new_t)
#'
#' print(head(df))
#' print(head(new_df))
#'
#'
#' # Example with datetime timeline
#' # (works with both POSIXct and POSIXlt)
#' t1 <- strptime('2019-01-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")
#' t2 <- strptime('2019-01-01 03:00:00', format = "%Y-%m-%d %H:%M:%S")
#'
#' df <- data.frame(time = seq(t1, t2, by = 'hour'))
#' df$x1 <- 1:nrow(df)
#' df$x2 <- 1:nrow(df) * 10
#'
#' new_t <- seq(t1, t2, by = 'min')
#'
#' new_df <- resample_df(df, new_t)
#'
#' print(head(df))
#' print(head(new_df))

resample_df <- function(df, t_new) {

  t_old <- df[[1]]
  cols <- names(df)

  ndf <- data.frame(t_new)
  names(ndf)[1] <- cols[1]

  i = 0
  for (c in cols) {
    if (i > 0) {
      y_old <- df[[c]]
      y_new <- approx(x = t_old, y = y_old, xout = t_new)$y
      ndf[c] <- y_new
    }
    i <- i + 1
  }

  ndf
}


# # Example with numeric timeline ----
# df <- data.frame(time = seq(0, 1800, by = 600))
# df$x1 <- 1:nrow(df)
# df$x2 <- 1:nrow(df) * 10
#
# new_t <- c(0, 60, 120, 500, 600, 1800)
#
# new_df <- resample_df(df, new_t)
#
# print(head(df))
# print(head(new_df))
#
#
# # Example with datetime timeline ----
# # (works with both POSIXct and POSIXlt)
# t1 <- strptime('2019-01-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")
# t2 <- strptime('2019-01-01 03:00:00', format = "%Y-%m-%d %H:%M:%S")
#
# df <- data.frame(time = seq(t1, t2, by = 'hour'))
# df$x1 <- 1:nrow(df)
# df$x2 <- 1:nrow(df) * 10
#
# new_t <- seq(t1, t2, by = 'min')
#
# new_df <- resample_df(df, new_t)
#
# print(head(df))
# print(head(new_df))


