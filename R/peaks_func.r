# peak search algorithm for finding peaks without retention times

#' @title Peak searching algorithm without retention times
#'
#' @description
#' Returns the maxima, the times they occur at, the index location of the maxima, and the next viable peaks with times after the first `keep`.
#'
#' @param data time versus intensity dataframe.
#' @param begin_search time at which to start the search for the peaks.
#' @param end_search time at which to stop the search for the peaks.
#' @param keep a whole number indicating how many peaks to search for and return.
#' @param precision  an integer indicating the size of the window for searching for local maxima.
#' @param factor constant of proportionality indicating the cutoff peak height (i.e., peaks greater than `factor` times height are not returned as viable peaks).
#' @param bl_noise constant level at which response should be considered as noise.
#'
#' @return list containing spectra maxima,the times they occur at, the index
#' location of the maxima, and the the same info for the next viable peaks with
#' times after the first `keep`.

peaks <- function(data, begin_search = NULL, end_search = NULL, keep = 11, precision = 15, factor = 10, bl_noise = 0) {
  # 'precision' is how finely we want to search for all local maxima, 'factor' is the cutoff proportionality constant
  # such that we eliminate maxima that are that times taller than the median maximum, and 'keep' is the number
  # of maxima to keep in the end (i.e., the number of peaks the scientist is looking for).


  if (!is.null(begin_search)) {
    data <- data[data$time > begin_search,]
  }
  if (!is.null(end_search)) {
    data <- data[data$time < end_search,]
  }

  data_subset <- c()
  index <- c()
  maxima <- c()
  locations <- c()

  # we use the baseline correction function to find peaks.
  data <- blc(data, noise = bl_noise)

  # reduce the total number of points to check by taking a subset of them;
  # we check for major changes using this method, not small shifts which may be "local" maxima
  i <- 1
  while (i < length(data$TIC)) {
    data_subset <- c(data_subset, data$TIC[i])
    index <- c(index, i)
    i <- i + precision
  }

  # find all the maxima of the subset
  fake_max_index <- which(diff(sign(diff(data_subset))) == -2) +1
  maxima_loc <- index[fake_max_index]

  # now find the maxima of those local regions using the actual data
  for (j in 1:length(fake_max_index)) {
    max_est <- maxima_loc[j]
    max <- max(data$TIC[(max_est-precision/2):(max_est+precision/2)])
    maxima <- c(maxima, max)
    loc <- which.max(data$TIC[(max_est-precision/2):(max_est+precision/2)]) - 1 + maxima_loc[j] - precision/2
    locations <- c(locations, loc)
  }

  # first trim to get rid of smallest spikes. We need two trims to properly identify outliers in the second while-loop.
  first_trim <- keep + 5 #ceiling((length(maxima) - keep)/2) + keep
  while (length(maxima) > first_trim) {
    min_temp <- min(maxima)
    loc_temp <- which(maxima == min_temp)
    maxima <- maxima[! maxima %in% min(maxima)]
    delete <- locations[loc_temp]
    locations <- locations[! locations %in% delete]
  }

  # to trim out any enormous spikes that are clearly not due to actual detection
  while (max(maxima) > factor * stats::median(maxima)) {
    loc_temp <- which.max(maxima)
    maxima <- maxima[! maxima %in% max(maxima)]
    delete <- locations[loc_temp]
    locations <- locations[!locations %in% delete]
  }

  h <- maxima
  index <- locations

  # trim to keep the desired number of tallest spikes remaining (other than those deleted above)
  maxima_discard <- c()
  locations_discard <- c()
  while (length(maxima) > keep) {
    loc_temp <- which.min(maxima)
    maxima_discard <- c(maxima_discard, min(maxima))
    locations_discard <- c(locations_discard, locations[loc_temp])
    maxima <- maxima[! maxima %in% min(maxima)]
    delete <- locations[loc_temp]
    locations <- locations[! locations %in% delete]
  }
  times_discard <- data$time[locations_discard]
  times <- data$time[locations]
  data <- data.frame(time = data$time, TIC = data$TIC)

  return(list(maxima,
              times,
              locations,
              data,
              maxima_discard,
              times_discard,
              locations_discard
  ))
}

# Notice: These data were produced by Battelle Savannah River Alliance, LLC under Contract No 89303321CEM000080 with the Department of Energy. During the period of commercialization or such other time period specified by DOE, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, and perform publicly and display publicly, by or on behalf of the Government. Subsequent to that period, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, distribute copies to the public, perform publicly and display publicly, and to permit others to do so. The specific term of the license can be identified by inquiry made to Contract or DOE. NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY DATA, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
# ©2024 Battelle Savannah River Alliance, LLC
