# optimization algorithm function
# objectives = input$objectives_new
# bbd = data_uploaded_bbd()

#' @title Optimization algorithm
#'
#' @description
#' Calculate the optimal parameter values for given objectives.
#'
#' @param objectives objectives input by user.
#' @param bbd Box-Behnken design.
#' @param results objective results.
#' @param lim_fac limiting factors.
#' @param valid_range_data ranges of validity corresponding to the limiting factors.
#'
#' @return a list containing the parameters which cannot be set to the unbounded
#' solution, the maximum value of the objectives, and the unbounded parameter
#' solutions

opt <- function(objectives, bbd, results, lim_fac, valid_range_data) {
  h_test <- "height" %in% objectives
  w_test <- "width" %in% objectives
  a_test <- "area" %in% objectives
  bl_test <- "bl" %in% objectives
  blv_test <- "blv" %in% objectives
  rt_test <- "rt" %in% objectives
  rt2_test <- "rt2" %in% objectives
  rt3_test <- "rt3" %in% objectives

  # results <- dwnld_results_obj()[-1]
  #
  # output$dwnld_results_bbd_b <- renderUI({downloadButton("dwnld_results_bbd", "Download Objective Data")})
  #
  # # for downloading objective data with bbd
  # dwnld_results(cbind(data_uploaded_bbd(),results))

  df <- as.data.frame(cbind(bbd, results))
  df[] <- lapply(df, as.numeric)

  # setting up variables for curve fitting
  yh <- df$havg
  yw <- df$wavg
  ya <- df$aavg
  ybl <- df$bls
  yblv <- df$blvs
  yrt <- df$drts
  yrt2 <- df$drts2
  yrt3 <- df$drts3

  # create arbitrary variable names using letters for the fittings below
  var_names <- c()
  # we are constructing the input statement piece by piece for the fittings; it's the only way I could figure to do it.
  input2 <- "polym("
  for (i in 1:length(colnames(bbd))) {
    assign(LETTERS[i], df[,i])
    var_names <- c(var_names, LETTERS[i])
    input2 <- paste0(input2, LETTERS[i], ",")
  }
  input2 <- paste0(input2, "degree = 2, raw = TRUE)")
  input <- data.frame(df[,1])
  for (j in 2:length(colnames(bbd))) {
    input <- data.frame(input, df[,j])
  }
  colnames(input) <- var_names
  input <- as.matrix(input)

  nvars <- length(colnames(bbd))
  Amat <- 0
  Bmat <- 0

  fits <- list()
  # we do a number of if statements for each objective to see whether or not said objective needs to be
  # fitted to a second order polynomial.

  # fitting height to second order equation (if 'height' selected)
  if (h_test == TRUE) {
    rlang::inject(
      fitting_h <- stats::lm(yh ~ !!str2lang(input2))
    )
    # fitting_h <- stats::lm(yh ~ polym(input , degree = 2, raw = TRUE))
    fits <- c(fits, fitting_h)

    # Create a list of indices corresponding to the coefficient indices that the fit returns.

    # for second order terms
    index <- 3
    # for first order terms
    index_1 <- 2
    for (j in 1:(length(colnames(bbd))-1)) {
      index <- c(index, (max(index) + 2):(max(index) + 2 + j))
      index_1 <- c(index_1, (max(index_1)+1+j))
    }

    # need to dynamically create the gradient and Hessian for height equation
    Mh <- matrix(ncol = nvars, nrow = nvars)
    Hh <- matrix(ncol = nvars, nrow = nvars)
    for (i in 1:nvars) {
      for (j in 1:nvars) {
        Hh[i,j] <- 0
      }
    }
    for (i in 1:nvars) {
      Mh[i,1:i] <- as.numeric(fitting_h[[1]][index[1:i]])
      Mh[1:i,i] <- as.numeric(fitting_h[[1]][index[1:i]])
      Mh[i,i] <- Mh[i,i]*2
      Hh[i,i] <- Mh[i,i]
      index <- index[-(1:i)]
    }
    MMh <- as.numeric(-fitting_h[[1]][index_1])

    # solve Lagrangian for unbounded solutions
    non_kkt_sol_h <- solve(Mh,MMh)

    Amat <- Amat -1/max(df$havg)*Mh

    Bmat <- Bmat -1/max(df$havg)*MMh
  }

  if (w_test == TRUE) {
    rlang::inject(
      fitting_w <- stats::lm(yw ~ !!str2lang(input2))
    )
    # fitting_w <- stats::lm(yw ~ polym(input, degree = 2, raw = TRUE))
    fits <- c(fits, fitting_w)

    # Create a list of indices corresponding to the coefficient indices that the fit returns.

    # for second order terms
    index <- 3
    # for first order terms
    index_1 <- 2
    for (j in 1:(length(colnames(bbd))-1)) {
      index <- c(index, (max(index) + 2):(max(index) + 2 + j))
      index_1 <- c(index_1, (max(index_1)+1+j))
    }

    # make gradient and hessian for width equation
    Mw <- matrix(ncol = nvars, nrow = nvars)
    Hw <- matrix(ncol = nvars, nrow = nvars)
    for (i in 1:nvars) {
      for (j in 1:nvars) {
        Hw[i,j] <- 0
      }
    }
    for (i in 1:nvars) {
      Mw[i,1:i] <- as.numeric(fitting_w[[1]][index[1:i]])
      Mw[1:i,i] <- as.numeric(fitting_w[[1]][index[1:i]])
      Mw[i,i] <- Mw[i,i]*2
      Hw[i,i] <- Mw[i,i]
      index <- index[-(1:i)]
    }
    MMw <- as.numeric(-fitting_w[[1]][index_1])

    # solving Lagrangian for unbounded optimization
    non_kkt_sol_w <- solve(Mw,MMw)

    Amat <- Amat +1/max(df$wavg)*Mw
    Bmat <- Bmat +1/max(df$wavg)*MMw
  }

  if (a_test == TRUE) {
    rlang::inject(
      fitting_a <- stats::lm(ya ~ !!str2lang(input2))
    )
    # fitting_a <- stats::lm(ya ~ polym(input, degree = 2, raw = TRUE))
    fits <- c(fits, fitting_a)

    # Create a list of indices corresponding to the coefficient indices that the fit returns.

    # for second order terms
    index <- 3
    # for first order terms
    index_1 <- 2
    for (j in 1:(length(colnames(bbd))-1)) {
      index <- c(index, (max(index) + 2):(max(index) + 2 + j))
      index_1 <- c(index_1, (max(index_1)+1+j))
    }

    # make gradient and hessian for area equation
    Ma <- matrix(ncol = nvars, nrow = nvars)
    Ha <- matrix(ncol = nvars, nrow = nvars)
    for (i in 1:nvars) {
      for (j in 1:nvars) {
        Ha[i,j] <- 0
      }
    }
    for (i in 1:nvars) {
      Ma[i,1:i] <- as.numeric(fitting_a[[1]][index[1:i]])
      Ma[1:i,i] <- as.numeric(fitting_a[[1]][index[1:i]])
      Ma[i,i] <- Ma[i,i]*2
      Ha[i,i] <- Ma[i,i]
      index <- index[-(1:i)]
    }
    MMa <- as.numeric(-fitting_a[[1]][index_1])

    # solving Lagrangian for unbounded optimization
    non_kkt_sol_a <- solve(Ma,MMa)

    # we are maximizing area, so we subtract the matrices
    Amat <- Amat -1/max(df$aavg)*Ma
    Bmat <- Bmat -1/max(df$aavg)*MMa
  }

  if (bl_test == TRUE) {
    rlang::inject(
      fitting_bl <- stats::lm(ybl ~ !!str2lang(input2))
    )
    # fitting_a <- stats::lm(ya ~ polym(input, degree = 2, raw = TRUE))
    fits <- c(fits, fitting_bl)

    # Create a list of indices corresponding to the coefficient indices that the fit returns.

    # for second order terms
    index <- 3
    # for first order terms
    index_1 <- 2
    for (j in 1:(length(colnames(bbd))-1)) {
      index <- c(index, (max(index) + 2):(max(index) + 2 + j))
      index_1 <- c(index_1, (max(index_1)+1+j))
    }

    # make gradient and hessian for bl equation
    Mbl <- matrix(ncol = nvars, nrow = nvars)
    Hbl <- matrix(ncol = nvars, nrow = nvars)
    for (i in 1:nvars) {
      for (j in 1:nvars) {
        Hbl[i,j] <- 0
      }
    }
    for (i in 1:nvars) {
      Mbl[i,1:i] <- as.numeric(fitting_bl[[1]][index[1:i]])
      Mbl[1:i,i] <- as.numeric(fitting_bl[[1]][index[1:i]])
      Mbl[i,i] <- Mbl[i,i]*2
      Hbl[i,i] <- Mbl[i,i]
      index <- index[-(1:i)]
    }
    MMbl <- as.numeric(-fitting_bl[[1]][index_1])

    # solving Lagrangian for unbounded optimization
    non_kkt_sol_bl <- solve(Mbl,MMbl)

    # we are minimizing avg baseline, so we add the matrices
    Amat <- Amat +1/max(df$bls)*Mbl
    Bmat <- Bmat +1/max(df$bls)*MMbl
  }

  if (blv_test == TRUE) {
    rlang::inject(
      fitting_blv <- stats::lm(yblv ~ !!str2lang(input2))
    )
    # fitting_a <- stats::lm(ya ~ polym(input, degree = 2, raw = TRUE))
    fits <- c(fits, fitting_blv)

    # Create a list of indices corresponding to the coefficient indices that the fit returns.

    # for second order terms
    index <- 3
    # for first order terms
    index_1 <- 2
    for (j in 1:(length(colnames(bbd))-1)) {
      index <- c(index, (max(index) + 2):(max(index) + 2 + j))
      index_1 <- c(index_1, (max(index_1)+1+j))
    }

    # make gradient and hessian for blv equation
    Mblv <- matrix(ncol = nvars, nrow = nvars)
    Hblv <- matrix(ncol = nvars, nrow = nvars)
    for (i in 1:nvars) {
      for (j in 1:nvars) {
        Hblv[i,j] <- 0
      }
    }
    for (i in 1:nvars) {
      Mblv[i,1:i] <- as.numeric(fitting_blv[[1]][index[1:i]])
      Mblv[1:i,i] <- as.numeric(fitting_blv[[1]][index[1:i]])
      Mblv[i,i] <- Mblv[i,i]*2
      Hblv[i,i] <- Mblv[i,i]
      index <- index[-(1:i)]
    }
    MMblv <- as.numeric(-fitting_blv[[1]][index_1])

    # solving Lagrangian for unbounded optimization
    non_kkt_sol_blv <- solve(Mblv,MMblv)

    # we are minimizing avg baseline, so we add the matrices
    Amat <- Amat +1/max(df$blvs)*Mblv
    Bmat <- Bmat +1/max(df$blvs)*MMblv
  }

  if (rt_test == TRUE) {
    rlang::inject(
      fitting_rt <- stats::lm(yrt ~ !!str2lang(input2))
    )
    fits <- c(fits, fitting_rt)

    # Create a list of indices corresponding to the coefficient indices that the fit returns.

    # for second order terms
    index <- 3
    # for first order terms
    index_1 <- 2
    for (j in 1:(length(colnames(bbd))-1)) {
      index <- c(index, (max(index) + 2):(max(index) + 2 + j))
      index_1 <- c(index_1, (max(index_1)+1+j))
    }

    # make gradient and hessian for rt equation
    Mrt <- matrix(ncol = nvars, nrow = nvars)
    Hrt <- matrix(ncol = nvars, nrow = nvars)
    for (i in 1:nvars) {
      for (j in 1:nvars) {
        Hrt[i,j] <- 0
      }
    }
    for (i in 1:nvars) {
      Mrt[i,1:i] <- as.numeric(fitting_rt[[1]][index[1:i]])
      Mrt[1:i,i] <- as.numeric(fitting_rt[[1]][index[1:i]])
      Mrt[i,i] <- Mrt[i,i]*2
      Hrt[i,i] <- Mrt[i,i]
      index <- index[-(1:i)]
    }
    MMrt <- as.numeric(-fitting_rt[[1]][index_1])

    # solving Lagrangian for unbounded optimization
    non_kkt_sol_rt <- solve(Mrt,MMrt)

    # we are maximizing rt diff, so we subtract the matrices
    Amat <- Amat -1/max(df$drts)*Mrt
    Bmat <- Bmat -1/max(df$drts)*MMrt
  }

  if (rt2_test == TRUE) {
    rlang::inject(
      fitting_rt2 <- stats::lm(yrt2 ~ !!str2lang(input2))
    )
    fits <- c(fits, fitting_rt2)

    # Create a list of indices corresponding to the coefficient indices that the fit returns.

    # for second order terms
    index <- 3
    # for first order terms
    index_1 <- 2
    for (j in 1:(length(colnames(bbd))-1)) {
      index <- c(index, (max(index) + 2):(max(index) + 2 + j))
      index_1 <- c(index_1, (max(index_1)+1+j))
    }

    # make gradient and hessian for rt2 equation
    Mrt2 <- matrix(ncol = nvars, nrow = nvars)
    Hrt2 <- matrix(ncol = nvars, nrow = nvars)
    for (i in 1:nvars) {
      for (j in 1:nvars) {
        Hrt2[i,j] <- 0
      }
    }
    for (i in 1:nvars) {
      Mrt2[i,1:i] <- as.numeric(fitting_rt2[[1]][index[1:i]])
      Mrt2[1:i,i] <- as.numeric(fitting_rt2[[1]][index[1:i]])
      Mrt2[i,i] <- Mrt2[i,i]*2
      Hrt2[i,i] <- Mrt2[i,i]
      index <- index[-(1:i)]
    }
    MMrt2 <- as.numeric(-fitting_rt2[[1]][index_1])

    # solving Lagrangian for unbounded optimization
    non_kkt_sol_rt2 <- solve(Mrt2,MMrt2)

    # we are maximizing rt diff, so we subtract the matrices
    Amat <- Amat -1/max(df$drts2)*Mrt2
    Bmat <- Bmat -1/max(df$drts2)*MMrt2
  }

  if (rt3_test == TRUE) {
    rlang::inject(
      fitting_rt3 <- stats::lm(yrt3 ~ !!str2lang(input2))
    )
    fits <- c(fits, fitting_rt3)

    # Create a list of indices corresponding to the coefficient indices that the fit returns.

    # for second order terms
    index <- 3
    # for first order terms
    index_1 <- 2
    for (j in 1:(length(colnames(bbd))-1)) {
      index <- c(index, (max(index) + 2):(max(index) + 2 + j))
      index_1 <- c(index_1, (max(index_1)+1+j))
    }

    # make gradient and hessian for rt3 equation
    Mrt3 <- matrix(ncol = nvars, nrow = nvars)
    Hrt3 <- matrix(ncol = nvars, nrow = nvars)
    for (i in 1:nvars) {
      for (j in 1:nvars) {
        Hrt3[i,j] <- 0
      }
    }
    for (i in 1:nvars) {
      Mrt3[i,1:i] <- as.numeric(fitting_rt3[[1]][index[1:i]])
      Mrt3[1:i,i] <- as.numeric(fitting_rt3[[1]][index[1:i]])
      Mrt3[i,i] <- Mrt3[i,i]*2
      Hrt3[i,i] <- Mrt3[i,i]
      index <- index[-(1:i)]
    }
    MMrt3 <- as.numeric(-fitting_rt3[[1]][index_1])

    # solving Lagrangian for unbounded optimization
    non_kkt_sol_rt3 <- solve(Mrt3,MMrt3)

    # we are maximizing rt diff, so we subtract the matrices
    Amat <- Amat -1/max(df$drts3)*Mrt3
    Bmat <- Bmat -1/max(df$drts3)*MMrt3
  }

  # find our unbounded objective solution
  obj_sol <- solve(Amat, Bmat)

  obj_sol_disp <- obj_sol
  optimal_display <- cbind(colnames(bbd),obj_sol_disp)
  colnames(optimal_display) <- c("Parameter", "Unbounded Solution")
  optimal_display <- as.data.frame(optimal_display)

  # check Hessian
  # note that the Hessian is the same as the matrix Amat, so we use it interchangeably here
  # ideally the scientist is well trained enough to know roughly where to find the minimum
  # but if the data is really bad then we may end up modelling a maximum
  hold <- c()
  bad_vars <- c()
  # if (exists("all_sols")) {
  #   obj_sol <- all_sols[length(all_sols[,1]),]
  # }
  for (i in 1:length(diag(Amat))) {
    if (diag(Amat)[i] < 0) {
      hold <- c(hold, i)
      bad_vars <- c(bad_vars, optimal_display$Parameter[i])
    }
  }

  obj_sol_disp <- obj_sol
  optimal_display <- cbind(colnames(bbd),obj_sol_disp)
  colnames(optimal_display) <- c("Parameter", "Unbounded Solution")
  optimal_display <- as.data.frame(optimal_display)


  # KKT conditions calculation--new

  # we will proceed if there are limiting values entered.
  if (!is.null(lim_fac)) {
    solutions <- data.frame(t(obj_sol))
    bound_params <- valid_range_data$Parameter
    all_params <- optimal_display$Parameter
    options <- c("fix_low", "fix_high", "free")
    # we create a matrix of permutations of the above vector, then we fill in each permutation accordingly with
    # vals from the limiting values.
    combinations <- gtools::permutations(n = 3, r = length(all_params), v = options, repeats.allowed = TRUE)
    for (i in 1:length(combinations[,1])) {
      obj_sol_kkt <- obj_sol
      Amat_kkt <- Amat
      Bmat_kkt <- Bmat
      fixed <- c()
      matrix_names <- all_params
      for (j in 1:length(combinations[1,])) {
        # we have to consider cases: first case: Amat is at least a 2x2; second case: Amat is 2x2;
        # third case: Amat has been reduced to only a single equation because all other vals are fixed.
        # first case: greater than 2x2
        if(dim(Amat_kkt)[1] > 2) {
          if (combinations[i,j] == "fix_high" && all_params[j] %in% bound_params) {
            index <- which(bound_params == all_params[j])
            mat_index <- which(matrix_names == bound_params[index])
            obj_sol_kkt[j] <- valid_range_data$High[index]
            Amat_kkt <- Amat_kkt[-mat_index,]
            Bmat_kkt <- Bmat_kkt[-mat_index]
            Bmat_kkt <- Bmat_kkt - Amat_kkt[,mat_index]*valid_range_data$High[index]
            Amat_kkt <- Amat_kkt[,-mat_index]
            fixed <- c(fixed, j)
            matrix_names <- matrix_names[-mat_index]
          }
          else if (combinations[i,j] == "fix_low" && all_params[j] %in% bound_params) {
            index <- which(bound_params == all_params[j])
            mat_index <- which(matrix_names == bound_params[index])
            obj_sol_kkt[j] <- valid_range_data$Low[index]
            Amat_kkt <- Amat_kkt[-mat_index,]
            Bmat_kkt <- Bmat_kkt[-mat_index]
            Bmat_kkt <- Bmat_kkt - Amat_kkt[,mat_index]*valid_range_data$Low[index]
            Amat_kkt <- Amat_kkt[,-mat_index]
            fixed <- c(fixed, j)
            matrix_names <- matrix_names[-mat_index]
          }
        }
        # second case: exactly a 2x2
        else if (dim(Amat_kkt)[1] == 2 && length(matrix_names) == 2) {
          if (combinations[i,j] == "fix_high" && all_params[j] %in% bound_params) {
            index <- which(bound_params == all_params[j])
            mat_index <- which(matrix_names == bound_params[index])
            obj_sol_kkt[j] <- valid_range_data$High[index]
            if (mat_index == 1) {index_prime <- 2}
            else if (mat_index == 2) {index_prime <- 1}
            obj_sol_temp <- (Bmat_kkt[index_prime] - Amat_kkt[index_prime,mat_index]*valid_range_data$High[index])/Amat_kkt[index_prime, index_prime]
            matrix_names <- matrix_names[-mat_index]
            fixed <- c(fixed, j)
          }
          else if (combinations[i,j] == "fix_low" && all_params[j] %in% bound_params) {
            index <- which(bound_params == all_params[j])
            mat_index <- which(matrix_names == bound_params[index])
            obj_sol_kkt[j] <- valid_range_data$Low[index]
            if (mat_index == 1) {index_prime <- 2}
            else if (mat_index == 2) {index_prime <- 1}
            obj_sol_temp <- (Bmat_kkt[index_prime] - Amat_kkt[index_prime,mat_index]*valid_range_data$Low[index])/Amat_kkt[index_prime, index_prime]
            matrix_names <- matrix_names[-mat_index]
            fixed <- c(fixed, j)
          }
        }
        # third case: only one equation left "1x1"
        else {
          if (combinations[i,j] == "fix_high" && all_params[j] %in% bound_params) {
            index <- which(bound_params == all_params[j])
            mat_index <- which(matrix_names == bound_params[index])
            obj_sol_kkt[j] <- valid_range_data$High[index]
            fixed <- c(fixed, j)
          }
          else if (combinations[i,j] == "fix_low" && all_params[j] %in% bound_params) {
            index <- which(bound_params == all_params[j])
            mat_index <- which(matrix_names == bound_params[index])
            obj_sol_kkt[j] <- valid_range_data$Low[index]
            fixed <- c(fixed, j)
          }
        }
      }

      # make replacements in the objective solution
      if (length(fixed) <= length(all_params) - 2) {
        obj_sol_temp <- solve(Amat_kkt, Bmat_kkt)
        n <- 1
        for (k in 1:length(obj_sol)) {
          if (! k %in% fixed) {
            obj_sol_kkt[k] <- obj_sol_temp[n]
            n <- n+1
          }
        }
      }
      else if (length(fixed) == length(all_params) - 1) {
        n <- 1
        for (k in 1:length(obj_sol)) {
          if (! k %in% fixed) {
            obj_sol_kkt[k] <- obj_sol_temp[n]
            n <- n+1
          }
        }
      }

      # make predictions about objective function
      predict <- 0
      data_input <- data.frame(t(obj_sol_kkt))
      colnames(data_input) <- var_names
      objective_na <- rep(NA, length(objectives))
      objective <- c()
      display_names <- c()
      if ("height" %in% objectives) {
        predict <- predict - 1/max(df$havg)*predict(fitting_h, data_input)
        objective <- c(objective, predict(fitting_h, data_input))
        display_names <- c(display_names, "Peak Height")
      }
      if ("width" %in% objectives) {
        predict <- predict + 1/max(df$wavg)*predict(fitting_w, data_input)
        objective <- c(objective, predict(fitting_w, data_input))
        display_names <- c(display_names, "Peak Width")
      }
      if ("area" %in% objectives) {
        predict <- predict - 1/max(df$aavg)*predict(fitting_a, data_input)
        objective <- c(objective, predict(fitting_a, data_input))
        display_names <- c(display_names, "Peak Area")
      }
      if ("bl" %in% objectives) {
        predict <- predict + 1/max(df$bls)*predict(fitting_bl, data_input)
        objective <- c(objective, predict(fitting_bl, data_input))
        display_names <- c(display_names, "Baseline")
      }
      if ("blv" %in% objectives) {
        predict <- predict + 1/max(df$blvs)*predict(fitting_blv, data_input)
        objective <- c(objective, predict(fitting_blv, data_input))
        display_names <- c(display_names, "Baseline")
      }
      if ("rt" %in% objectives) {
        predict <- predict - 1/max(df$drts)*predict(fitting_rt, data_input)
        objective <- c(objective, predict(fitting_rt, data_input))
        display_names <- c(display_names, "RT Separation")
      }
      if ("rt2" %in% objectives) {
        predict <- predict - 1/max(df$drts2)*predict(fitting_rt2, data_input)
        objective <- c(objective, predict(fitting_rt2, data_input))
        display_names <- c(display_names, "RT Separation 2")
      }
      if ("rt3" %in% objectives) {
        predict <- predict - 1/max(df$drts3)*predict(fitting_rt3, data_input)
        objective <- c(objective, predict(fitting_rt3, data_input))
        display_names <- c(display_names, "RT Separation 3")
      }

      # make data frame with combination, objective solution, and prediction
      if (i == 1) {
        solutions <- data.frame(t(c(obj_sol_kkt, predict, objective)))
      }
      else {
        solutions <- rbind(solutions, c(obj_sol_kkt, predict, objective))
      }
    }

    # delete any solutions that don't satisfy constraints
    to_del <- c()
    lows <- valid_range_data$Low
    highs <- valid_range_data$High

    for (i in 1:length(combinations[,1])) {
      for (j in 1:length(combinations[1,])) {
        if (all_params[j] %in% bound_params) {
          index <- which(bound_params == all_params[j])
          test <- solutions[i,j] < lows[index] || solutions[i,j] > highs[index]
          if(test) {
            to_del <- c(to_del, i)
          }
        }
      }
    }
    all_solutions <- solutions
    if (!is.null(to_del)) {
      solutions <- solutions[-to_del,]
    }

    # find min index, min solution, and min objective values
    min_index <- which.min(solutions[,length(combinations[1,])+1])
    min_val <- solutions[min_index, length(combinations[1,])+1]
    min_sol <- solutions[min_index, 1:length(combinations[1,])]
    min_obj <- solutions[min_index, (length(combinations[1,])+2):length(solutions)]

    min_sol <- as.numeric(min_sol)
    optimal_display <- data.frame(optimal_display, min_sol)
    colnames(optimal_display) <- c("Parameter", "Unbounded Solution", "Bounded Solution")

    min_obj <- as.numeric(min_obj)
    obj_display <- data.frame(t(min_obj))
    colnames(obj_display) <- display_names
  }

  # if there are no bounds entered for any parameters, we take the non-kkt sol and calculate the predicted objective vals
  if (is.null(lim_fac)) {
    data_input <- data.frame(t(obj_sol))
    colnames(data_input) <- var_names
    objective <- c()
    display_names <- c()
    if ("height" %in% objectives) {
      objective <- c(objective, predict(fitting_h, data_input))
      display_names <- c(display_names, "Peak Height")
    }
    if ("width" %in% objectives) {
      objective <- c(objective, predict(fitting_w, data_input))
      display_names <- c(display_names, "Peak Width")
    }
    if ("area" %in% objectives) {
      objective <- c(objective, predict(fitting_a, data_input))
      display_names <- c(display_names, "Peak Area")
    }
    if ("bl" %in% objectives) {
      objective <- c(objective, predict(fitting_bl, data_input))
      display_names <- c(display_names, "Baseline")
    }
    if ("blv" %in% objectives) {
      objective <- c(objective, predict(fitting_blv, data_input))
      display_names <- c(display_names, "Baseline")
    }
    if ("rt" %in% objectives) {
      objective <- c(objective, predict(fitting_rt, data_input))
      display_names <- c(display_names, "RT Separation")
    }
    if ("rt2" %in% objectives) {
      objective <- c(objective, predict(fitting_rt2, data_input))
      display_names <- c(display_names, "RT Separation 2")
    }
    if ("rt3" %in% objectives) {
      objective <- c(objective, predict(fitting_rt3, data_input))
      display_names <- c(display_names, "RT Separation 3")
    }

    min_obj <- as.numeric(objective)
    obj_display <- data.frame(t(min_obj))
    colnames(obj_display) <- display_names
  }

  return(list(bad_vars, obj_display, optimal_display))
}

# Notice: These data were produced by Battelle Savannah River Alliance, LLC under Contract No 89303321CEM000080 with the Department of Energy. During the period of commercialization or such other time period specified by DOE, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, and perform publicly and display publicly, by or on behalf of the Government. Subsequent to that period, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, distribute copies to the public, perform publicly and display publicly, and to permit others to do so. The specific term of the license can be identified by inquiry made to Contract or DOE. NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY DATA, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
# ©2024 Battelle Savannah River Alliance, LLC
