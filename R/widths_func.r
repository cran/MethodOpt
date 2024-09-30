# calculate widths. Portion of code taken from SG.

#' @title Calculate widths
#'
#' @description
#' Calculate the widths (standard deviation) for each identified peak.
#'
#' @param data time versus intensity dataframe.
#' @param hts heights of peaks.
#' @param tms times of peaks.
#'
#' @return width for each peak

widths <- function(data, hts, tms) {
  w <- c()
  frame <- data

  for (i in 1:length(hts)) {
    if (is.na(tms[i])) {
      w <- append(w,NA)
    }
    else {
      loc <- which(frame$time == tms[i])
      a <- hts[i]
      st <- loc-10
      if (loc < 10) {
        st <- 1
      }
      # else {
      time <- frame$time[st:(loc+10)]
      y <- frame$TIC[st:(loc+10)]
      b <- tms[i]
      if (a == 0) {
        w <- append(w,Inf)
      }
      else {
        suppressWarnings({
          fitting <- stats::nls(y ~ gauss(a,b,c,time),
                                start = list(b = b, c = 0.01),
                                control = stats::nls.control(maxiter = 300, warnOnly = TRUE))
        })
        cat <- abs(fitting$m$getPars()[2])
        w <- append(w, as.numeric(cat))
      }
      # }
    }
  }
  return(w)
}


# Notice: These data were produced by Battelle Savannah River Alliance, LLC under Contract No 89303321CEM000080 with the Department of Energy. During the period of commercialization or such other time period specified by DOE, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, and perform publicly and display publicly, by or on behalf of the Government. Subsequent to that period, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, distribute copies to the public, perform publicly and display publicly, and to permit others to do so. The specific term of the license can be identified by inquiry made to Contract or DOE. NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY DATA, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
# ©2024 Battelle Savannah River Alliance, LLC
