# peak function with retention times. Credit Stephanie Gamble with modifications made by BL.

#' @title peak calculations with retention times
#'
#' @description
#' Locate the maxima of the peaks corresponding to the retention times.
#'
#' @param raw_data time versus intensity dataframe.
#' @param retention_times retention time file.
#' @param rt_index which method to evaluate for.
#' @param bl_noise constant level at which response should be considered as noise.
#'
#' @return list containing the peaks' heights, times where they occur, the full
#' data TIC, full time index, and the peaks' widths

peaks_rts <- function(raw_data, retention_times, rt_index, bl_noise = 0) {
  file <- raw_data
  rts <- retention_times
  peak_n <- ncol(rts) - 1

  time <- file$time
  tic <- file$TIC

  data <- as.data.frame(cbind(time,tic))
  names(data)<-c('time','TIC')

  # perform baseline correction
  frame <- blc(data, noise = bl_noise)
  bl <- frame$baseline
  frame <- data.frame(`Overall Time Index` = frame$time, TIC = frame$TIC, check.names = FALSE)

  h <- c()
  w <- c()
  t <- c()
  locations <- c()
  for (j in 1:peak_n){
    if (is.na(rts[rt_index,(j+1)])) {
      h <- append(h, NA)
      t <- append(t, NA)
      w <- append(w, NA)
    }
    else {
      loc <- which.min(abs(rts[rt_index,(j+1)]-frame$'Overall Time Index'))
      locations <- c(locations, loc)
      a <- frame$TIC[loc]
      h <- append(h,a)
      # time <- frame$`Overall Time Index`[(loc-10):(loc+10)]
      # y <- frame$TIC[(loc-10):(loc+10)]
      b <- frame$`Overall Time Index`[loc]
      t <- append(t,b)
      # if (a==0){
      #   w <- append(w,Inf)
      # }
      # else{
      #   suppressWarnings({
      #     fitting <- stats::nls(y~gauss(a,b,c,time),
      #                           start=list(b=b,c=.01),
      #                           control=stats::nls.control(maxiter=300, warnOnly=TRUE))
      #   })
      #   cat <- abs(fitting$m$getPars()[2])
      #   w <- append(w,as.numeric(cat))
      # }
    }
  }

  max_data <- data.frame(time = t, height = h)

  return(list(
    h,
    t,
    frame$TIC,
    frame$`Overall Time Index`,
    w
  ))
}

# Notice: These data were produced by Battelle Savannah River Alliance, LLC under Contract No 89303321CEM000080 with the Department of Energy. During the period of commercialization or such other time period specified by DOE, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, and perform publicly and display publicly, by or on behalf of the Government. Subsequent to that period, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, distribute copies to the public, perform publicly and display publicly, and to permit others to do so. The specific term of the license can be identified by inquiry made to Contract or DOE. NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY DATA, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
# ©2024 Battelle Savannah River Alliance, LLC

