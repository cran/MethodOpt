# area of peak function

#' @title Computes areas of peaks
#'
#' @description
#' Computes the areas beneath the specified maxima.
#'
#' @param data dataframe of time versus intensity.
#' @param hts heights of peaks in chronological order.
#' @param tms time locations of peaks in chronological order.
#'
#' @return area for each peak

area <- function(data, hts, tms){
  ar <- c()
  frame <- data
  colnames(frame) <- c("time", "TIC")

  for (i in 1:length(hts)) {
    if (is.na(tms[i])) {
      ar <- append(ar,NA)
    }
    else {
      loc <- which(frame$time == tms[i])
      a <- hts[i]
      st <- loc-10
      if (loc < 10) {
        st <- 1
      }
      time <- frame$time[st:(loc+10)]
      y <- frame$TIC[st:(loc+10)]
      b <- tms[i]
      if (a == 0) {
        ar <- append(ar,0)
      }
      else {
        suppressWarnings({
          fitting <- stats::nls(y ~ gauss(a,b,c,time),
                                start = list(b = b, c = .01),
                                control = stats::nls.control(maxiter = 300, warnOnly = TRUE))
        })
        b <- as.numeric(abs(fitting$m$getPars()[1]))
        c <- as.numeric(abs(fitting$m$getPars()[2]))
        integrand <- function(x) {a*exp(-(x-b)^2/(2*(c)^2))}
        result <- stats::integrate(integrand, b-2,b+2)$value # area under the gaussian for +-2 s.d.
        ar <- append(ar, as.numeric(result))
      }
    }
  }
  return(ar)
}


# Notice: These data were produced by Battelle Savannah River Alliance, LLC under Contract No 89303321CEM000080 with the Department of Energy. During the period of commercialization or such other time period specified by DOE, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, and perform publicly and display publicly, by or on behalf of the Government. Subsequent to that period, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, distribute copies to the public, perform publicly and display publicly, and to permit others to do so. The specific term of the license can be identified by inquiry made to Contract or DOE. NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY DATA, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
# ©2024 Battelle Savannah River Alliance, LLC
