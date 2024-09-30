# anova function/calculation
# objectives = input$objectives_new; the selected objectives
# data = vars$obj_data; the tibble with peak data
# ffd = data_uploaded(); fractional/full factorial design that is user uploaded
# obj_results = dwnld_results_obj(); data frame with calculated objectives
# alpha = test(); alpha value; input$alpha

#' @title Computes ANOVA test
#'
#' @description Runs an ANOVA test for any variables that were selected by the user.
#'
#' @param objectives User selected objectives.
#' @param data Peak information (all times, heights, etc.).
#' @param ffd Fractional factorial design
#' @param obj_results Calculated objectives.
#' @param alpha Alpha value.
#'
#' @return list containing the results of ANOVA on the data from the FFD
#' (including p-values and which parameters are significant), the suggested
#' changes to add a level for a BBD, suggested values for the BBD, row indices
#' for the results, and the data frame of the FFD with objective results included

anovaben <- function(objectives, data, ffd, obj_results, alpha) {
  names <- c()
  params <- c()
  all_names <- list()
  display_names <- c()
  colnames <- c()

  # create column names for various height items
  if("height" %in% objectives == TRUE) {
    names <- c()
    for (k in 1:length(data$all_heights[[1]])) {
      #names of columns
      names <- c(names, paste0("h",k))
    }
    all_names <- c(all_names, names, "havg")
    display_names <- c(display_names, "havg")
    colnames <- c(colnames, "havg p-val", "havg Significance Status")
  }

  # create column names for various width items
  if ("width" %in% objectives == TRUE) {
    names <- c()
    for (k in 1:length(data$all_widths[[1]])) {
      names <- c(names, paste0("w",k))
    }
    all_names <- c(all_names, names, "wavg")
    display_names <- c(display_names, "wavg")
    colnames <- c(colnames, "wavg p-val", "wavg Significance Status")
  }

  # create column names for various area items
  if ("area" %in% objectives == TRUE) {
    names <- c()
    for (k in 1:length(data$all_areas[[1]])) {
      names <- c(names, paste0("a",k))
    }
    all_names <- c(all_names, names, "aavg")
    display_names <- c(display_names, "aavg")
    colnames <- c(colnames, "aavg p-val", "aavg Significance Status")
  }

  # create column names for various baseline items
  if ("bl" %in% objectives == TRUE) {
    all_names <- c(all_names, names, "bl")
    display_names <- c(display_names, "bl")
    colnames <- c(colnames, "Baseline p-val", "Baseline Significance Status")
  }

  # create column names for various baseline variability items
  if ("blv" %in% objectives == TRUE) {
    all_names <- c(all_names, names, "blv")
    display_names <- c(display_names, "blv")
    colnames <- c(colnames, "Baseline Var p-val", "Baseline Var Significance Status")
  }

  # we only need to make names for the case of the rts, because there is onl one column for them
  if ("rt" %in% objectives == TRUE) {
    all_names <- c(all_names, "drt")
    display_names <- c(display_names, "drt")
    colnames <- c(colnames, "RT Sep p-val", "RT Sep Significance Status")
  }

  if ("rt2" %in% objectives == TRUE) {
    all_names <- c(all_names, "drt2")
    display_names <- c(display_names, "drt2")
    colnames <- c(colnames, "RT Sep (2) p-val", "RT Sep (2) Significance Status")
  }

  if ("rt3" %in% objectives == TRUE) {
    all_names <- c(all_names, "drt3")
    display_names <- c(display_names, "drt3")
    colnames <- c(colnames, "RT Sep (3) p-val", "RT Sep (3) Significance Status")
  }

  # get results from objective calculations
  results <- obj_results[-1]
  results[] <- lapply(results, as.numeric)
  if (length(obj_results==1)){
    results <- matrix(obj_results)
  }

  # to download all objectives with FFD
  dwnld_results <- cbind(ffd,results)

  input <- colnames(ffd)[1]
  for (i in 1:length(ffd)) {
    assign(colnames(ffd)[i], ffd[,i])
    #names of rows
    params <- c(params, colnames(ffd)[i])
  }

  # paste together the argument for linear fit
  for (j in 2:length(ffd)) {
    input <- paste0(input, " + ",colnames(ffd)[j])
  }

  # linear fit
  lin_fit <- function(vars) {
    rlang::inject(
      stats::lm(y ~ !!vars)
    )
  }
  pval <- c()
  for (i in 1:length(results)) {
    y <- results[,i]
    if (length(results)==1){
      y <- results
    }
    fit <- rlang::inject(
      lin_fit(vars = quote(!!str2lang(input)))
    )
    aov <- stats::anova(fit)
    p <- cbind(aov$'Pr(>F)'[1:length(params)])
    pval <- cbind(pval, p)
  }
  if(length(obj_results)!=1){
    colnames(pval) <- unlist(all_names)}
  rownames(pval) <- params

  # print results of anova, which parameters are significant, with specified confidence
  confidence_results <- c()
  significance <- c()
  num <- c()
  for (i in 1:length(display_names)) {
    num <- c(num, which(colnames(pval) == display_names[i]))
  }
  for (j in num) {
    assign(paste0("conf_res",j), c())
    assign(paste0("significance",j), c())
  }

  for (i in num) {
    for (x in 1:length(ffd)) {
      res <- pval[x,i]
      assign(paste0("conf_res",i), append(eval(str2lang(paste0("conf_res",i))), res))
      if (res < alpha) {
        assign(paste0("significance",i), append(eval(str2lang(paste0("significance",i))), paste(rownames(pval)[x], "Significant")))
      }
      else {
        assign(paste0("significance",i), append(eval(str2lang(paste0("significance",i))), paste(rownames(pval)[x], "Not Significant")))
      }
    }
  }

  ffd_results <- params
  col_names <- c()
  for (k in num) {
    ffd_results <- cbind(ffd_results, eval(str2lang(paste0("conf_res",k))), eval(str2lang(paste0("significance",k))))
  }

  # MAKE SUGGESTIONS FOR BBD
  values <- list()
  # get min and max values from uploaded ffd
  for (i in 1:length(ffd)) {
    pair <- c(min(ffd[,i]), max(ffd[,i]))
    values <- c(values, list(pair))
  }
  bbd_change <- c()
  bbd_num <- c()
  count_all <- c()
  for (j in 1:length(ffd)) {
    count <- 0
    # df1 is the a subset of the master dataframe that contains ffd with results, defined by taking
    # all rows corresponding to the min ffd val for a particular parameter. df2 is the same but for
    # the max.
    df1 <- dwnld_results[dwnld_results[,j] == values[[j]][1],]
    df2 <- dwnld_results[dwnld_results[,j] == values[[j]][2],]
    # to know which objectives to make suggestions for, we first test to see if
    # there exist columns with that objective data
    search <- colnames(dwnld_results)
    h_test <- any(grepl("h", colnames(dwnld_results)))
    w_test <- any(grepl("w", colnames(dwnld_results)))
    a_test <- "aavg" %in% colnames(dwnld_results)
    # a_test <- any(grepl("a", colnames(dwnld_results))) && search[which(grepl("a", search))] != "havg"
    bl_test <- "bl" %in% colnames(dwnld_results)
    blv_test <- "blv" %in% colnames(dwnld_results)
    rt_test <- "drts" %in% colnames(dwnld_results)
    rt2_test <- "drts2" %in% colnames(dwnld_results)
    rt3_test <- "drts3" %in% colnames(dwnld_results)

    if(h_test == TRUE) {
      indices <- which(grepl("h", colnames(dwnld_results)) == TRUE)
      # h1 and h2 are similar to df1 and df2 but we take the mean across methods for each peak
      # then count whether or not the min or max ffd value yielded more taller peaks for each peak
      h1 <- c()
      h2 <- c()
      for (k in indices) {
        h1 <- append(h1, mean(df1[,k]))
        h2 <- append(h2, mean(df2[,k]))
      }
      # count how many peaks for the min ffd value were taller than for the max ffd val
      count <- count + length(which(h1 > h2))
    }
    # see comments in h_test above for description of width code
    if (w_test == TRUE) {
      indices <- which(grepl("w", colnames(dwnld_results)) == TRUE)
      w1 <- c()
      w2 <- c()
      for (k in indices) {
        w1 <- append(w1, mean(df1[,k]))
        w2 <- append(w2, mean(df2[,k]))
      }
      count <- count + length(which(w1 < w2))
    }
    # see comments in h_test above for description of area code
    if (a_test == TRUE) {
      indices <- which(grepl("a", colnames(dwnld_results)) == TRUE)
      # two if statements must be added to delete any misread indices. Specifically, we do not want to count
      # colnames that are "havg" or "wavg" just because they contain "a"
      if ("havg" %in% colnames(dwnld_results)) {
        loc <- which(colnames(dwnld_results) == "havg")
        indices <- indices[-which(indices == loc)]
      }
      if ("wavg" %in% colnames(dwnld_results)) {
        loc <- which(colnames(dwnld_results) == "wavg")
        indices <- indices[-which(indices == loc)]
      }
      a1 <- c()
      a2 <- c()
      for (k in indices) {
        a1 <- append(a1, mean(df1[,k]))
        a2 <- append(a2, mean(df2[,k]))
      }
      count <- count + length(which(a1 > a2))
    }

    # there is only one column of bls/rts to average (compared to npeak number of columns in the first three objectives)
    # so we merely average the bls/rts column for df1 and df2, and compare
    if (bl_test == TRUE) {
      index <- which(colnames(dwnld_results) == "bls")
      bl1 <- mean(df1[,index])
      bl2 <- mean(df2[,index])
      if (bl1 > bl2) {
        count <- count + 1
      }
    }
    if (blv_test == TRUE) {
      index <- which(colnames(dwnld_results) == "blvs")
      blv1 <- mean(df1[,index])
      blv2 <- mean(df2[,index])
      if (blv1 > blv2) {
        count <- count + 1
      }
    }
    # there is only one column of bls/rts to average (compared to npeak number of columns in the first three objectives)
    # so we merely average the bls/rts column for df1 and df2, and compare
    if (rt_test == TRUE) {
      index <- which(colnames(dwnld_results) == "drts")
      rt1 <- mean(df1[,index])
      rt2 <- mean(df2[,index])
      if (rt1 > rt2) {
        count <- count + 1
      }
    }
    if (rt2_test == TRUE) {
      index <- which(colnames(dwnld_results) == "drts2")
      rt12 <- mean(df1[,index])
      rt22 <- mean(df2[,index])
      if (rt12 > rt22) {
        count <- count + 1
      }
    }
    if (rt3_test == TRUE) {
      index <- which(colnames(dwnld_results) == "drts3")
      rt13 <- mean(df1[,index])
      rt23 <- mean(df2[,index])
      if (rt13 > rt23) {
        count <- count + 1
      }
    }

    num <- (length(dwnld_results) - length(ffd)) / 2
    if (count < num) {
      bbd_change <- c(bbd_change, "Increase new value")
      sugg <- (values[[j]][2] - values[[j]][1]) + values[[j]][2]
    }
    else if (count > num) {
      bbd_change <- c(bbd_change, "Decrease new value")
      sugg <- values[[j]][1] - (values[[j]][2] - values[[j]][1])
    }
    else {
      bbd_change <- c(bbd_change, "No conclusion")
      sugg <- NA
    }
    bbd_num <- c(bbd_num, sugg)
    count_all <- c(count_all, count)
  }
  return(list(ffd_results, bbd_change, bbd_num, colnames, dwnld_results))
}


# Notice: These data were produced by Battelle Savannah River Alliance, LLC under Contract No 89303321CEM000080 with the Department of Energy. During the period of commercialization or such other time period specified by DOE, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, and perform publicly and display publicly, by or on behalf of the Government. Subsequent to that period, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, distribute copies to the public, perform publicly and display publicly, and to permit others to do so. The specific term of the license can be identified by inquiry made to Contract or DOE. NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY DATA, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
# ©2024 Battelle Savannah River Alliance, LLC
