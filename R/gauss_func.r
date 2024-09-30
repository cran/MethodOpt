# Gaussian and Gaussian fit functions

#' @title Generate a Gaussian fit
#'
#' @description
#' Generate a Gaussian fit with given parameters.
#'
#' @param a amplitute.
#' @param b phase shift.
#' @param c standard deviation.
#' @param t time at which to fit.
#'
#' @return value of Gaussian function

gauss <- function(a,b,c,t){
  G <- a*exp(-(t-b)^2/(2*c^2))
  return(G)
}

# Notice: These data were produced by Battelle Savannah River Alliance, LLC under Contract No 89303321CEM000080 with the Department of Energy. During the period of commercialization or such other time period specified by DOE, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, and perform publicly and display publicly, by or on behalf of the Government. Subsequent to that period, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, distribute copies to the public, perform publicly and display publicly, and to permit others to do so. The specific term of the license can be identified by inquiry made to Contract or DOE. NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY DATA, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
# ©2024 Battelle Savannah River Alliance, LLC
