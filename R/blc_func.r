# baseline correction function. Credit to Stephanie Gamble with small modifications made by Ben Luke.

#' @title Baseline correction
#'
#' @description
#' Generates the baseline of the spectra, interpolates between points, and subtracts from the intensity to generate corrected baseline.
#'
#' @param frame data frame of time versus intensity.
#' @param noise strength of the baseline subtraction.
#' @param subtract how much intensity to initially subtract.
#'
#' @return data frame of of data that has been baseline corrected

blc <- function(frame, noise=10^5, subtract=NULL){
  N <- length(frame$TIC)
  if (!(is.null(subtract))){
    if (length(subtract$'TIC')!=length(frame$'TIC')){
      stop('To perform baseline subtractions, samples must be of the same size.')
    }
    else{
      frame$'TIC' <- frame$'TIC'-subtract$'TIC'
    }
  }
  bl <- c(rep(NA,N))
  sig <- c(rep(NA,N))
  win_size <- 100
  if (win_size > N){
    win_size <- 1
  }
  else if (N<10000){
    win_size <- round(N/100)
  }
  points <- c(1:round(N/win_size))
  points <- points[1:(length(points)-1)]*win_size
  for (i in c(1:N)){
    if (i %in% points){
      if (i== points[1]){
        window <- c(1:i+win_size)
      }
      else if (i== points[length(points)]){
        window <- c(i-win_size,N)
      }
      else {
        window <- c((i-win_size):(i+win_size))
      }
      bl[i] <- min(frame$TIC[window])
    }
  }
  bl[1] <- bl[points[1]]
  bl[length(bl)] <- bl[points[length(points)]]
  min <- bl
  bl2 <- bl[is.na(bl)==FALSE]
  bl3 <- bl2+noise
  new_bl <- rep(NA,N)
  new_bl[c(1,points,N)] <- bl3
  new_bl <- zoo::na.approx(new_bl)
  frame$TIC <- frame$TIC-new_bl
  frame$TIC[frame$TIC<0]<- 0

  return(data.frame(time = frame$time, TIC = frame$TIC, baseline = new_bl))

}

# Notice: These data were produced by Battelle Savannah River Alliance, LLC under Contract No 89303321CEM000080 with the Department of Energy. During the period of commercialization or such other time period specified by DOE, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, and perform publicly and display publicly, by or on behalf of the Government. Subsequent to that period, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, distribute copies to the public, perform publicly and display publicly, and to permit others to do so. The specific term of the license can be identified by inquiry made to Contract or DOE. NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY DATA, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
# ©2024 Battelle Savannah River Alliance, LLC
