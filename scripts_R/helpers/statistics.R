mae <- function(y, yp, na_rm) {
  error <- mean(abs(yp - y), na.rm = na_rm)
  return(error)
}

rmse <- function(y, yp, na_rm) {
  error <- sqrt(mean((yp - y) ^ 2, na.rm = na_rm))
  return(error)
}

mbe <- function(y, yp, na_rm) {
  error <- mean(yp - y, na.rm = na_rm)
  return(error)
}

rmae <- function(y, yp, na_rm) {
  error <- mae(y = y, yp = yp, na_rm = na_rm) / mean(y, na.rm = na_rm)
  error <- error * 100
  return(error)
}

rrmse <- function(y, yp, na_rm) {
  error <- rmse(y = y, yp = yp, na_rm = na_rm) / mean(y, na.rm = na_rm)
  error <- error * 100
  return(error)
}

rmbe <- function(y, yp, na_rm) {
  error <- mbe(y = y, yp = yp, na_rm = na_rm) / mean(y, na.rm = na_rm)
  error <- error * 100
  return(error)
}

summarize_stats <- function(df_grouped, y, y_p, na_rm = T) {
  y <- enquo(y)
  y_p <- enquo(y_p)
  df_grouped %>%
    summarise(
      n_val = sum(!is.na(!!y) & !is.na(!!y_p)), 
      y_avg = mean((!!y)[!is.na(!!y) & !is.na(!!y_p)]),
      mbe = mbe(y = !!y, yp = !!y_p, na_rm = !!na_rm),
      rmbe = rmbe(y = !!y, yp = !!y_p, na_rm = !!na_rm),
      mae = mae(y = !!y, yp = !!y_p, na_rm = !!na_rm),
      rmae = rmae(y = !!y, yp = !!y_p, na_rm = !!na_rm),
      rmse = rmse(y = !!y, yp = !!y_p, na_rm = !!na_rm),
      rrmse = rrmse(y = !!y, yp = !!y_p, na_rm = !!na_rm)) %>%
    ungroup() 
} 

mean_abs <- function(x, na.rm = T) {
  mean(abs(x), na.rm = na.rm)
}


mean2 <- function(x, nas_max) {
  if (sum(is.na(x)) <= nas_max) {
    mean(x, na.rm = T)
  } else {
    NA
  }
}

sum2 <- function(x, nas_max = 0) {
  if (sum(is.na(x)) <= nas_max) {
    sum(x, na.rm = T)
  } else {
    NA
  }
}

# angle conversions --------------------------------------------------------------------------------

r2d <- function(x) {
  x * 180 / pi
}

d2r <- function(x) {
  x * pi / 180
}
