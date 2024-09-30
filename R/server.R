# --------------Server component of Shiny app, server ----------------

server <- function(input, output, session) {

  #----------Callback----------------
  callback <- c(
    "table.on('row-reorder', function(e, details, edit){",
    "  var oldRows = [], newRows = [];",
    "  for(let i=0; i < details.length; ++i){",
    "    oldRows.push(details[i].oldData);",
    "    newRows.push(details[i].newData);",
    "  }",
    "  Shiny.setInputValue('rowreorder', {old: oldRows, new: newRows});",
    "});"
  )

  session$onSessionEnded(function() {
    shiny::stopApp()
  })


  # These shiny::reactive variables are used for storing the input variables in a vector
  vars <- shiny::reactiveValues(ffd_data = data.frame(Parameter = c(), Low = c(), High = c()),
                         bbd_data = data.frame(Parameter = c(), Low = c(), Mid = c(), High = c()),
                         opt_data = data.frame(Parameter = c(), Low = c(), High = c()),
                         obj_data = tibble::tibble(file_name = character(),
                                                   all_data = list(),
                                                   all_times = list(),
                                                   all_heights = list(),
                                                   all_times_discard = list(),
                                                   all_heights_discard = list()))

  #======================FFD================FFD=====================

  #-----This is a response to the button that saves the input values------
  shiny::observeEvent(input$add_factor, {
    if (input$factor != "" && input$low_val < input$hi_val) {

      ffd_temp = data.frame(Parameter = c(input$factor), Low = c(input$low_val), High = c(input$hi_val))

      vars$ffd_data <- rbind(vars$ffd_data, ffd_temp)

      shiny::updateTextInput(session, "factor", value = "")
    }
    else {
      shinyalert::shinyalert("Oops!", "Please check that the parameter field is nonempty and the low value is less than the high value.", type = "error")
    }
  })

  #---------Print the table of factors-------------
  output$fctr_tbl <- DT::renderDataTable(vars$ffd_data,
                                         rownames = FALSE,
                                         filter = "none",
                                         options = list(
                                           dom = 't',
                                           searching = FALSE,
                                           paging = FALSE
                                         ),
                                         editable = 'cell',
                                         selection = 'single')

  #---------In case user wants to edit data from table-----------
  shiny::observeEvent(input$fctr_tbl_cell_edit, {
    vars$ffd_data <<- DT::editData(data = vars$ffd_data,
                                   info = input$fctr_tbl_cell_edit,
                                   rownames = FALSE)
  })

  #--------In case user wants to delete a row entry entirely----------
  shiny::observeEvent(input$deleteRow, {
    if (!is.null(input$fctr_tbl_rows_selected)) {
      vars$ffd_data <- vars$ffd_data[-as.numeric(input$fctr_tbl_rows_selected),]
    }
  })

  #--------This is a test to see if the resolution is less than the number of parameters---
  #--------If not, it returns a warning message and defaults to the number of parameters---
  res_test <-shiny::reactive({
    val_check1 <- input$res <= nrow(vars$ffd_data)
    shinyFeedback::feedbackWarning("res", !val_check1, "Resolution defaults to number of parameters if number of parameters is less than defined resolution.")
  })

  output$res_test <- shiny::renderText(res_test())

  #------Render FFD table-------
  shiny::observeEvent(input$make_ffd, {
    if (nrow(vars$ffd_data) > 1) {
      ffd_tbl <- as.matrix(FrF2::FrF2(
        nruns = NULL,
        nfactors = nrow(vars$ffd_data),
        factor.names = vars$ffd_data[,1],
        resolution = input$res
      )
      )
      output$ffd_table <- DT::renderDataTable(ffd_tbl,
                                              options = list(
                                                dom = 'ltip'
                                              ),
                                              selection = 'none')

      vars$ffd_tbl_dwnld <- ffd_tbl #--------This is so I can easily download

      #---------This is to make the experimental utility screening sheet----------
      # replace all 1 and -1 with actual high and low data
      ffd_tbl_exper <- ffd_tbl
      for (col in 1:ncol(ffd_tbl)) {
        for (row in 1:nrow(ffd_tbl)) {
          if (ffd_tbl[row,col] == 1) {
            ffd_tbl_exper[row,col] <- vars$ffd_data[col,3]
          }
          else {
            ffd_tbl_exper[row,col] <- vars$ffd_data[col,2]
          }
        }
      }
      output$ffd_table_exper <- DT::renderDataTable(ffd_tbl_exper,
                                                    options = list(
                                                      dom = 'ltip'
                                                    ),
                                                    selection = 'none')

      #-----the two lines of code below makes the dwnld button appear after generating table-------
      output$button1 <- shiny::renderUI({shiny::downloadButton("dwnld_ffd", "Download FFD")})
      output$button2 <- shiny::renderUI({shiny::downloadButton("dwnld_ffd_exper", "Download FFD Experimental")})

      vars$ffd_tbl_exper_dwnld <- ffd_tbl_exper #So I can easily download

      #---------This is to check if the input resolution is less than number of parameters
      #--------If it is not, then resolultion is set to lowest value, 3-----------
      if (input$res > nrow(vars$ffd_data)) {
        if(nrow(vars$ffd_data) == 2) {
          shiny::updateTextInput(session, "res", value = 3)
        }
        else(shiny::updateTextInput(session, "res", value = nrow(vars$ffd_data)))
      }
    }
    else {
      shinyalert::shinyalert("Oops!", "There must be at least 2 parameters.", type = "error")
      #shinyFeedback::feedbackWarning("ffd_table", length(vars$fctr == 0), "There must be at least one parameter")
    }
  })

  #-----------To download FFD to a csv file----------
  output$dwnld_ffd <- shiny::downloadHandler(
    filename = function() {
      paste("screening_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      utils::write.csv(vars$ffd_tbl_dwnld, file, row.names = FALSE)
    }
  )

  output$dwnld_ffd_exper <- shiny::downloadHandler(
    filename = function() {
      paste("screening_data-exper-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      utils::write.csv(vars$ffd_tbl_exper_dwnld, file, row.names = FALSE)
    }
  )


  #================UPLOAD RAW DATA/PLOT==========UPLOAD RAW DATA/PLOT============

  #----------Form a table of information for uploaded data----------------

  dat <- shiny::reactiveVal()
  shiny::observeEvent(input$upload_data, {
    dat(input$upload_data)
  })

  # if any of the method files is not formatted properly (i.e., 2 cols only), an error message is given
  shiny::observe({
    shiny::req(input$upload_data)
    for (i in 1:length(input$upload_data$name)) {
      table <- utils::read.csv(input$upload_data$datapath[i], skip = 4)
      if (ncol(table) != 2) {
        shinyjs::show("file_test")
        output$file_test <- shiny::renderText({"ERROR: One or more of the uploaded data files is not formatted properly. Raw data must be only two columns."})
      }
      else {
        shinyjs::hide("file_test")
      }
    }
  })

  # give a suggestion to reorganize files by clicking and dragging if needed
  shiny::observe({shiny::req(input$upload_data)
    output$reorder <- shiny::renderUI({htmltools::h5("Tip: Reorganize input files to match the orders of the retention times, Box-Behnken Designs, or Full Factorial Designs by dragging the index to the appropriate position.")})
  })

  output$files <- DT::renderDT({DT::datatable(dat(), #input$upload_data[c('name', 'size', 'type')],
                                              selection = 'single',
                                              extensions = "RowReorder", #list("RowReorder", "Buttons"),
                                              callback = DT::JS(callback),
                                              options = list(
                                                dom = 't',
                                                scrollY = "350px",
                                                scrollCollapse = TRUE,
                                                pageLength = length(input$upload_data[,1]),
                                                ordering = FALSE,
                                                rowReorder = TRUE,
                                                columnDefs = list(list(visible=FALSE, targets=c(4)))
                                              ),
  )})

  # in order to reorder files such that the data frame is reorganized accordingly as well
  # (without this process, only the displayed table will reorder, not the underlying dataframe.)
  proxy <- DT::dataTableProxy("files")

  shiny::observeEvent(input[["rowreorder"]], {
    data <- dat()
    old <- unlist(input[["rowreorder"]]$old)
    new <- unlist(input[["rowreorder"]]$new)
    data[new, ] <- data[old, ]
    dat(data)
    DT::replaceData(proxy, dat(), resetPaging = FALSE)
    vars$opt_data <- data.frame(Parameter = c(), Low = c(), High = c())
    vars$obj_data <- tibble::tibble(file_name = character(),
                                    all_data = list(),
                                    all_times = list(),
                                    all_heights = list(),
                                    all_times_discard = list(),
                                    all_heights_discard = list())
  })

  # two shiny::observeEvents are used to disable both background subtraction and log scale from being used simultaneously
  shiny::observeEvent(input$log,{
    if (input$log == TRUE) {
      shiny::updateCheckboxInput(session, "bg_sub", value = FALSE)
    }
  })
  shiny::observeEvent(input$bg_sub, {
    if (input$bg_sub == TRUE) {
      shiny::updateCheckboxInput(session, "log", value = FALSE)
    }
  })

  #------------Render selected plots from table----------------

  # grapher function to render the proper plot according to the checkbox input
  grapher <- function(df, xlab, ylab, filename) {
    if (input$log == TRUE && input$overlay == FALSE) {
      printplot({ggplot2::ggplot(df, ggplot2::aes(x = time, y = log10(TIC))) +
          ggplot2::geom_line() +
          ggplot2::labs(x = xlab, y = paste(ylab, "[log10]"), caption = paste("BL subtraction =", input$bg_sub, "; Source:", filename))
      })
    }
    else if (input$bg_sub == TRUE && input$overlay == FALSE) {
      plot_data <- blc(df, noise = input$noise)
      printplot({ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = TIC)) +
          ggplot2::geom_line() +
          ggplot2::labs(x = xlab, y = ylab, caption = paste("BL subtraction =", input$bg_sub, "; Source:", filename))
      })
    }
    else if (input$log == TRUE && input$overlay == TRUE) {
      plot_data <- blc(df, noise = input$noise)
      plot_data$baseline <- log10(plot_data$baseline)
      printplot({ggplot2::ggplot(df, ggplot2::aes(x = time, y = log10(TIC))) +
          ggplot2::geom_line() +
          ggplot2::labs(x = xlab, y = paste(ylab, "[log10]"), caption = paste("BL subtraction =", input$bg_sub, "; Source:", filename)) +
          ggplot2::geom_line(data = plot_data, ggplot2::aes_(x = ~time, y = ~baseline), color = "red")
      })
    }
    else if (input$bg_sub == TRUE && input$overlay == TRUE) {
      plot_data <- blc(df, noise = input$noise)
      printplot({ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = TIC)) +
          ggplot2::geom_line() +
          ggplot2::labs(x = xlab, y = ylab, caption = paste("BL subtraction =", input$bg_sub, "; Source:", filename)) +
          ggplot2::geom_line(data = plot_data, ggplot2::aes_string(x = 'time', y = 'baseline'), color = "red")
      })
    }
    else if (input$bg_sub == FALSE && input$log == FALSE && input$overlay == TRUE) {
      plot_data <- blc(df, noise = input$noise)
      printplot({ggplot2::ggplot(df, ggplot2::aes(x = time, y = TIC)) +
          ggplot2::geom_line() +
          ggplot2::labs(x = xlab, y = ylab, caption = paste("BL subtraction =", input$bg_sub, "; Source:", filename)) +
          ggplot2::geom_line(data = plot_data, ggplot2::aes_string(x = 'time', y = 'baseline'), color = "red")
      })
    }
    else {
      printplot({ggplot2::ggplot(df, mapping = ggplot2::aes(x = time, y = TIC)) +
          ggplot2::geom_line() +
          ggplot2::labs(x = xlab, y = ylab, caption = paste("BL subtraction =", input$bg_sub, "; Source:", filename))
      })
    }
  } # Function for plotting with the plot options

  printplot <- shiny::reactiveVal()
  plot_display <- shiny::reactiveVal()

  # render plot feature; uses above grapher function
  shiny::observe(
    if (shiny::isTruthy(input$upload_data)) {
      if (length(input$files_rows_selected) == 1) {
        inFile <- dat()[input$files_rows_selected,]
        if (ncol(utils::read.csv(inFile$datapath, skip = 4)) == 2) {
          df <- utils::read.csv(inFile$datapath, skip = input$row_skip, header = FALSE, col.names= c("time", "TIC"))
          xlab <- input$xlabel
          ylab <- input$ylabel
          filename <- inFile$name

          grapher(df, xlab, ylab, filename) # This is a function I wrote that produces the plots based on the plot options
          plot_display(printplot())

          output$plot1 <- shiny::renderPlot(plot_display(), res = 96)
          output$plotname <- shiny::renderText(paste("Plot Display: ", filename))
          output$button3 <- shiny::renderUI(shiny::downloadButton("saveplot", "Download Plot"))
          shinyjs::show("show_dwnld_b")
        }
      }
    }
  )

  #----------Mass Download Button-----------------------

  # We first must trigger an shiny::observeEvent to make all of the plots.
  mass_download_files <- shiny::reactiveValues(all_plots = NULL)

  shiny::observeEvent(input$dwnld_all, {
    if (shiny::isTruthy(input$upload_data)) {
      for (i in 1:length(input$upload_data[,1])) {
        inFile_temp <- dat()[i,]
        df_temp <- utils::read.csv(inFile_temp$datapath, header = FALSE, skip = input$row_skip, col.names = c("time", "TIC"))
        xlab_temp <- input$xlabel
        ylab_temp <- input$ylabel
        filename_temp <- inFile_temp$name

        grapher(df_temp, xlab_temp, ylab_temp, filename_temp)
        mass_download_files$all_plots[[i]] <- printplot() # Here we load all the plots into the shiny::reactive list.
      }
    }
  }
  )

  # Here we use the download handler to throw all the plots into a zip file that can be downloaded.
  output$mass_download <- shiny::downloadHandler(
    filename = function(){
      paste("plots-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file){

      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)

      mass_download_files$all_plots %>%
        purrr::imap(function(x,y){
          if(!is.null(x)){
            file_name <- glue::glue("plot_{y}.png")
            ggplot2::ggsave(plot = x, filename = file.path(temp_directory, file_name), width = 10, height = 6, units = "in")
          }
        })


      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )

    },
    contentType = "application/zip"

  )

  #----------Save Plot button---------------------------
  output$saveplot <- shiny::downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(plot_display(), filename = file, width = 10, height = 6, units = "in")
    }
  )

  #===============Objectives calculation=================

  #--------------Peak upload and search algorithm stuff---------------

  # if the user selects to refine the peak search, then the search min and max boxes pop up
  shiny::observeEvent(input$search_select, {
    shinyjs::toggleState("search_min")
    shinyjs::toggleState("search_max")
  })

  # test to make sure that the retention times are formatted basically properly;
  # ensure number of rows in rt file equals number of method files input.
  shiny::observe({
    shiny::req(input$upload_rts)
    test <- nrow(utils::read.csv(input$upload_rts$datapath)) == length(input$upload_data$name)
    if (input$rt_select == "yes" && test == FALSE) {
      shinyjs::show("rts_warning")
      output$rts_warning <- shiny::renderText("WARNING: Retention time file: Number of rows does not equal number of uploaded datasets.")
    }
    else {
      shinyjs::hide("rts_warning")
    }
  })

  # toggle the states of certain UI features based on whether or not the user has selected to
  # use retention times or use the search algorithm
  shiny::observe({
    if (input$rt_select == "yes") {
      shinyjs::hide("peak_num")
      shinyjs::show("upload_rts")
    }
    else {
      shinyjs::show("peak_num")
      shinyjs::hide("upload_rts")
      # reset("upload_rts")
    }
  })

  # throw a pop up window with confirm button to make peak search
  modal_confirm <- shiny::modalDialog(
    "Are you sure you want to continue? This action will overwrite any existing data.",
    title = "WARNING",
    footer = htmltools::tagList(
      shiny::actionButton("cancel", "Cancel"),
      shiny::actionButton("ok", "Confirm", class = "btn btn-danger")
    )
  )

  shiny::observeEvent(input$find_peaks, {
    shiny::showModal(modal_confirm)
  })

  # run algorithm to find maxima and other information
  shiny::observeEvent(input$ok, {
    # first case: no rts
    if (shiny::isTruthy(input$upload_data) && input$rt_select == 'no') {
      #clear information in tibble before doing the "apply all" job
      vars$obj_data <- tibble::tibble(file_name = character(),
                                      all_data = list(),
                                      all_times = list(),
                                      all_heights = list(),
                                      all_times_discard = list(),
                                      all_heights_discard = list())

      for (i in 1:length(input$upload_data[,1])) {
        file <- dat()[i,]
        data <- utils::read.csv(file$datapath, skip = 4, header = FALSE, col.names = c("time", "TIC"))
        # run the peak search function
        results <- peaks(data = data, keep = input$peak_num, bl_noise = input$noise)
        maxima <- results[[1]]
        time <- results[[2]]
        data <- results[[4]]
        maxima_discard <- results[[5]]
        time_discard <- results[[6]]

        # shiny::reactive tibble to store all of the information
        vars$obj_data <- tibble::add_row(vars$obj_data,
                                         file_name = file$name,
                                         all_data = list(data),
                                         all_times = list(time),
                                         all_heights = list(maxima),
                                         all_times_discard = list(time_discard),
                                         all_heights_discard = list(maxima_discard))
      }
      output$instructions <- shiny::renderUI(
        htmltools::h5("Select a file from the table to view its plot.")
      )

      shiny::removeModal()
    }

    # second case: we have rts
    else if (shiny::isTruthy(input$upload_data) && input$rt_select == 'yes') {
      if (shiny::isTruthy(input$upload_rts) && nrow(utils::read.csv(input$upload_rts$datapath)) >= length(input$upload_data$name)) {
        #clear information in tibble before doing the "apply all" job
        vars$obj_data <- tibble::tibble(file_name = character(),
                                        all_data = list(),
                                        all_times = list(),
                                        all_heights = list(),
                                        all_times_discard = list(),
                                        all_heights_discard = list())

        for (i in 1:length(input$upload_data[,1])) {
          file <- dat()[i,]
          data <- utils::read.csv(file$datapath, skip = 2, header = FALSE, col.names = c("time", "TIC"))
          rts <- utils::read.csv(input$upload_rts$datapath)
          # run rt peak function
          results <- peaks_rts(data, rts, rt_index = i, bl_noise = input$noise)
          maxima <- results[[1]]
          time <- results[[2]]
          TIC <- results[[3]]
          time_index <- results[[4]]
          data <- data.frame(time = time_index, TIC = TIC)

          # shiny::reactive tibble to store all of the information
          vars$obj_data <- tibble::add_row(vars$obj_data,
                                           file_name = file$name,
                                           all_data = list(data),
                                           all_times = list(time),
                                           all_heights = list(maxima),
                                           all_times_discard = NA,
                                           all_heights_discard = NA)
        }
        output$instructions <- shiny::renderUI(
          htmltools::h5("Select a file from the table to view its plot.")
        )
        output$search_select_o <- NULL
        output$search_min_o <- NULL
        output$search_max_o <- NULL
        output$replace_b <- NULL
        output$regen_b <- NULL
      }
      else {
        shiny::removeModal()
        shinyalert::shinyalert(title = "No RT file", "Retention time option selected but no file uploaded. Please upload a retention time csv file to proceed with this method.", type = "error", size = "s")
      }
      shiny::removeModal()
    }
    else {
      shiny::removeModal()
      shinyalert::shinyalert(title = "No data", "Please upload data to find peaks.", type = "error", size = "s")
    }
  })

  shiny::observeEvent(input$cancel, {
    shiny::removeModal()
  })

  #---------peak plotting stuff (tables, graphs, etc)---------------

  #render the plot with peaks on it
  plot <- shiny::reactiveValues() # data, peaks, caption
  peak_plot_display <- shiny::reactiveVal()
  shiny::observe(
    if (!is.null(input$upload_data) &&
        length(vars$obj_data$all_times) == length(input$upload_data$name) &&
        length(input$files_rows_selected) == 1) {
      inFile <- dat()[input$files_rows_selected,]
      if (inFile$name %in% vars$obj_data$file_name == TRUE) {
        index <- which(vars$obj_data$file_name == inFile$name)
        plot$peaks <- data.frame(time = vars$obj_data$all_times[[index]], peak_height = vars$obj_data$all_heights[[index]])
        plot$caption <- vars$obj_data$file_name[[index]]
        plot$data <- vars$obj_data$all_data[[index]]

        # put plot data in shiny::reactive variable so that it can be downloaded
        peak_plot_display({
          ggplot2::ggplot(data = plot$data, mapping = ggplot2::aes(x = time, y = TIC)) + ggplot2::geom_line() +
            ggplot2::geom_point(data = plot$peaks, mapping = ggplot2::aes_(x = ~time, y = ~peak_height, colour = "red")) +
            ggplot2::theme(legend.position = "none") +
            ggplot2::labs(caption = paste("Source:", plot$caption))
        })

        output$peak_plot <- shiny::renderPlot({
          ggplot2::ggplot(data = plot$data, mapping = ggplot2::aes(x = time, y = TIC)) + ggplot2::geom_line() +
            ggplot2::geom_point(data = plot$peaks, mapping = ggplot2::aes_(x = ~time, y = ~peak_height, colour = "red")) +
            ggplot2::theme(legend.position = "none") +
            ggplot2::labs(caption = paste("Source:", plot$caption))
        }, res = 100)
        shinyjs::show("dwnld_peaks")
        shinyjs::show("peak_plot")
        # hide peak refinement tools if rt_select is set to "no"
        if (input$rt_select == "no") {
          shinyjs::show("search_select")
          shinyjs::show("search_min")
          shinyjs::show("search_max")
          shinyjs::show("regen_plot")
          shinyjs::show("replace")
        }
        else {
          shinyjs::hide("search_select")
          shinyjs::hide("search_min")
          shinyjs::hide("search_max")
          shinyjs::hide("regen_plot")
          shinyjs::hide("replace")
        }
      }
    }
    else {
      shinyjs::hide("search_select")
      shinyjs::hide("search_min")
      shinyjs::hide("search_max")
      shinyjs::hide("regen_plot")
      shinyjs::hide("replace")
      shinyjs::hide("peak_plot")
      shinyjs::hide("dwnld_peaks")
    }
  )

  # Download peak plot
  output$dwnld_peaks <- shiny::downloadHandler(
    filename = function() {
      paste("plot-peaks-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(peak_plot_display(), filename = file, width = 10, height = 6, units = "in")
    }
  )

  # This is to regenerate the plot according to a narrower search window
  shiny::observeEvent(input$regen_plot, {
    if(shiny::isTruthy(input$files_rows_selected)) {
      inFile <- dat()[input$files_rows_selected,]
      index <- which(vars$obj_data$file_name == inFile$name)

      #we only want to get input search window if the checkbox is selected
      if (input$search_select == TRUE) {
        min <- input$search_min
        max <- input$search_max
      }
      else {
        min <- NULL
        max <- NULL
      }

      data <- utils::read.csv(inFile$datapath, skip = 2, header = FALSE, col.names = c("time", "TIC"))
      results <- peaks(data = data, begin_search = min, end_search = max, keep = input$peak_num)
      maxima <- results[[1]]
      time <- results[[2]]
      data <- results[[4]]
      maxima_discard <- results[[5]]
      times_discard <- results[[6]]

      vars$obj_data$file_name[index] <- inFile$name
      vars$obj_data$all_data[index] <- list(data)
      vars$obj_data$all_times[index] <- list(time)
      vars$obj_data$all_heights[index] <- list(maxima)
      vars$obj_data$all_times_discard[index] <- list(times_discard)
      vars$obj_data$all_heights_discard[index] <- list(maxima_discard)

      max_data <- data.frame(times = time, height = maxima)
      output$peak_plot <- shiny::renderPlot({
        ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = time, y = TIC)) + ggplot2::geom_line() +
          ggplot2::geom_point(data = max_data, mapping = ggplot2::aes_string(x = 'time', y = 'height', colour = "red")) +
          ggplot2::theme(legend.position = "none") +
          ggplot2::labs(caption = paste("Source:", inFile$name))
      }, res = 100)
    }
    else {
      shinyalert::shinyalert(title = "No selection", "Please select from the table which dataset you would like to refine.", type = "error", size = "s")
    }
  })

  #In order to view peak data
  info <- shiny::reactiveVal() # these really do not need to be shiny::reactive but I started that way and don't feel like changing them all right now
  data_caption <- shiny::reactiveVal()
  shiny::observe(
    if (!is.null(input$upload_data)) {
      if(length(vars$obj_data$all_times) == length(input$upload_data$name)) {
        if (length(input$files_rows_selected) == 1) {
          inFile <- dat()[input$files_rows_selected,]
          if (inFile$name %in% vars$obj_data$file_name == TRUE) {
            index <- which(vars$obj_data$file_name == inFile$name)
            info(data.frame(time = vars$obj_data$all_times[[index]], peak_height = vars$obj_data$all_heights[[index]]))
            data_caption(vars$obj_data$file_name[[index]])

            output$master_data <- DT::renderDT(
              info(),
              options = list(dom = 'ti',
                             scrollY = "200px",
                             scrollCollapse = TRUE,
                             pageLength = length(vars$obj_data$all_times[[index]])
              ),
              selection = 'single'
            )
          }
        }
        else {
          output$master_data <- NULL
        }
      }
    }
  )

  # to selectively eliminate peaks that were found on search and replace them with next available.
  shiny::observeEvent(input$replace, {
    if (!is.null(input$upload_data)) {
      if(length(vars$obj_data$all_times) == length(input$upload_data$name)) {
        if (length(input$master_data_rows_selected) == 1) {
          inFile <- dat()[input$files_rows_selected,] # read highlighted file from data table
          index <- which(vars$obj_data$file_name == inFile$name) # find the index in tibble based on matching file names
          if(length(vars$obj_data$all_heights_discard[[index]]) != 0) {
            next_max <- max(vars$obj_data$all_heights_discard[[index]]) # get the next maximum found by the algorithm
            next_max_index <- which.max(vars$obj_data$all_heights_discard[[index]]) # get the index of the next max to find the corresponding time
            next_time <- vars$obj_data$all_times_discard[[index]][next_max_index] # get the next time

            vars$obj_data$all_heights[[index]][input$master_data_rows_selected] <- next_max
            vars$obj_data$all_times[[index]][input$master_data_rows_selected] <- next_time

            df <- data.frame(c1 = vars$obj_data$all_times[[index]], y = vars$obj_data$all_heights[[index]])

            c1 <- NULL # need this, otherwise R CMD check throws a NOTE (I don't know why...)
            df <- dplyr::arrange(df, c1)

            vars$obj_data$all_times[[index]] <- df$c1
            vars$obj_data$all_heights[[index]] <- df$y

            vars$obj_data$all_heights_discard[[index]] <- vars$obj_data$all_heights_discard[[index]][! vars$obj_data$all_heights_discard[[index]] %in% next_max]
            vars$obj_data$all_times_discard[[index]] <- vars$obj_data$all_times_discard[[index]][! vars$obj_data$all_times_discard[[index]] %in% next_time]
          }
          else {
            shinyalert::shinyalert(title = "No more peaks", "All peaks found by the algorithm have been shown. Press \"Refine\" to start the plot over.", type = "error", size = "s")
          }
        }
      }
    }
  })






  #==============ANOVA===========ANOVA======================

  #--------------Upload working FFD table and format the display option-----------
  data_uploaded <- shiny::reactive({
    file1 <- input$upload_ffd
    fulldf <- utils::read.table(file = file1$datapath, sep = ",", header = TRUE)
    #fulldf[,2:ncol(fulldf)]
  })

  output$FFD_actual <- DT::renderDataTable(
    data_uploaded(), options = list(scrollX = TRUE, dom = 'tip'), selection = 'none')

  ffd_test <- shiny::reactive({
    shiny::req(input$upload_ffd)
    good_ffd <- length(data_uploaded()[,1]) == length(input$upload_data[,1])
    shinyFeedback::feedbackWarning("upload_ffd", !good_ffd, "Improper format", icon = NULL)
  })

  output$ffd_test <- shiny::renderText(ffd_test())

  #------------ANOVA calculation----------------

  # output statement about significance precision
  output$percent_conf <- shiny::renderText({paste("Will calculate which parameters are significant with", (1 - as.numeric(input$alpha))*100, "% confidence.")})
  test <- shiny::reactiveVal()
  shiny::observe(
    test(as.numeric(input$alpha))
  )

  # do anova calculations
  dwnld_results <- shiny::reactiveVal()
  shiny::observeEvent(input$objectives_b, {
    # must pass 4 conditions: objectives selection is non-empty, screening data is uploaded, and there are the same number of height vectors as ffd rows as uploaded screening files
    if (!is.null(input$objectives_new) &&
        !is.null(input$upload_data) &&
        length(vars$obj_data$all_heights) == length(data_uploaded()[,1]) &&
        length(input$upload_data[,1]) == length(vars$obj_data$all_heights)) {

      ffd <- data_uploaded()
      obj_results <- dwnld_results_obj()
      peak_info <- vars$obj_data

      anova_results <- anovaben(objectives = input$objectives_new, data = peak_info, ffd = ffd, obj_results = obj_results, alpha = as.numeric(input$alpha))
      ffd_results <- anova_results[[1]]
      bbd_change <- anova_results[[2]]
      bbd_num <- anova_results[[3]]
      colnames <- anova_results[[4]]
      dwnld_results(anova_results[[5]])

      ffd_results <- cbind(ffd_results, bbd_change, bbd_num)
      colnames(ffd_results) <- c("Parameter", colnames, "Suggested change", "Suggested")
      # ffd_results <- as.data.frame(ffd_results)
      # ffd_results <- ffd_results %>% dplyr::mutate(across(is.numeric, round, digits = 5))
      output$filled_ffd <- DT::renderDataTable(ffd_results,
                                               options = list(
                                                 dom = 't',
                                                 scrollX = TRUE
                                               ),
                                               selection = 'none',
      )
      output$dwnld_results_b <- shiny::renderUI(shiny::downloadButton("dwnld_all_results", "Download Objective Data (with FFD)"))
    }
    else (
      shinyalert::shinyalert("Error", "Insufficient data to compute ANOVA. Potential error sources: (1) No uploaded screening data; (2) Heights not computed; (3) No objectives selection; (4) FFD table improperly formatted.", type = "error")
    )
  }
  )

  # download all results
  output$dwnld_all_results <- shiny::downloadHandler(
    filename = function() {
      paste("all_results-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      utils::write.csv(dwnld_results(), file, row.names = FALSE)
    }
  )

  #------------Objectives tab---------------


  rts <- shiny::reactive({
    utils::read.csv(input$upload_rts$datapath)
  })

  # render UI to select RT to maximize difference
  shiny::observeEvent(input$upload_rts, {
    if (nrow(utils::read.csv(input$upload_rts$datapath)) >= length(input$upload_data$name)) {
      output$rt_input_render <- shiny::renderUI({list(shiny::selectizeInput("rt_input", "Retention Times to Separate", colnames(rts())[-1], multiple = TRUE, options = list(maxItems = 2)),
                                               shiny::selectizeInput("rt_input2", "Retention Times to Separate (2)", colnames(rts())[-1], multiple = TRUE, options = list(maxItems = 2)),
                                               shiny::selectizeInput("rt_input3", "Retention Times to Separate (3)", colnames(rts())[-1], multiple = TRUE, options = list(maxItems = 2)))})

    }
  })

  # calculate objectives shiny::reactively after selection an objective checkbox
  dwnld_results_obj <- shiny::reactiveVal()
  shiny::observeEvent(input$objectives_new, {
    # must pass some conditions: objectives selected, data uploaded, and heights found
    if (!is.null(input$objectives_new) &&
        !is.null(input$upload_data) &&
        length(input$upload_data[,1]) == length(vars$obj_data$all_heights)) {
      objectives_temp <- input$objectives_new
      if ("rt" %in% objectives_temp && length(input$rt_input) != 2) {
        index <- which(objectives_temp == "rt")
        objectives_temp <- objectives_temp[-index]
        shiny::updateCheckboxGroupInput(session, "objectives_new", selected = objectives_temp)
      }
      else if ("rt2" %in% objectives_temp && length(input$rt_input2) != 2) {
        index <- which(objectives_temp == "rt2")
        objectives_temp <- objectives_temp[-index]
        shiny::updateCheckboxGroupInput(session, "objectives_new", selected = objectives_temp)
      }
      else if ("rt3" %in% objectives_temp && length(input$rt_input3) != 2) {
        index <- which(objectives_temp == "rt3")
        objectives_temp <- objectives_temp[-index]
        shiny::updateCheckboxGroupInput(session, "objectives_new", selected = objectives_temp)
      }
      else if ("area" %in% objectives_temp && "width" %in% objectives_temp) {
        index <- which(objectives_temp == "width")
        index2 <- which(objectives_temp == "area")
        objectives_temp <- objectives_temp[-c(index,index2)]
        shiny::updateCheckboxGroupInput(session, "objectives_new", selected = objectives_temp)
        shinyalert::shinyalert(title = "Conflicting Objectives", text = "The objectives Maximize Peak Area and Minimize Peak Width are contradictory and may not be selected concurrently.")
      }
      else {

        names <- c()
        params <- c()
        all_names <- list()
        display_names <- c("File Names")
        colnames <- c()
        results <- c(rep.int(NA, times = length(vars$obj_data$all_heights))) # this is necessary in order to form the final dataframe
        display_objectives <- c(rep.int(NA, times = length(vars$obj_data$all_heights)))

        # MAXIMIZE HEIGHT
        if("height" %in% input$objectives_new == TRUE) {
          havg <- c()
          heights <- c()
          names <- c()
          # calculate the average height vector, and bind together as rows the peak height data for each method
          for (i in 1:length(vars$obj_data$all_heights)) {
            havg <- append(havg, mean(vars$obj_data$all_heights[[i]], na.rm = TRUE))
            heights <- rbind(heights, vars$obj_data$all_heights[[i]])
          }
          #make names for each peak; these are column names
          for (k in 1:length(vars$obj_data$all_heights[[1]])) {
            #names of columns
            names <- c(names, paste0("h",k))
          }
          # concatenate peak height names with "havg", the average height column name
          all_names <- c(all_names, names, "havg")
          # concatenate the columns of peak heights with the column of average heights
          results <- cbind(results, heights, havg)
          # form the vector of column names for the display in the GUI
          display_names <- c(display_names, "Peak Height")
          # choose which objectives to display, namely the average peak height of the methods
          display_objectives <- cbind(display_objectives, havg)
          # begin forming the column names for the ANOVA display
          colnames <- c(colnames, "havg p-val", "havg Significance Status")
        }

        # MINIMIZE WIDTH
        # see MAXIMIZE HEIGHT above for analogous commentary
        if ("width" %in% input$objectives_new == TRUE) {
          # in case there are already widths in all_widths, we should clear them and redo
          if ("all_widths" %in% colnames(vars$obj_data) == TRUE) {
            vars$obj_data <- vars$obj_data %>% dplyr::mutate(all_widths = NULL)
          }

          # First we have to get the widths
          widths <- list()
          for (i in 1:length(input$upload_data[,1])) {
            data <- vars$obj_data$all_data[[i]]
            h <- vars$obj_data$all_heights[[i]]
            t <- vars$obj_data$all_times[[i]]
            w <- widths(data = data, hts = h, tms = t)
            widths <- c(widths, list(w))
          }
          # add new column of widths to tibble
          vars$obj_data <- vars$obj_data %>% tibble::add_column(all_widths = widths)

          wavg <- c()
          widths <- c()
          names <- c()
          for (i in 1:length(vars$obj_data$all_widths)) {
            wavg <- append(wavg, mean(vars$obj_data$all_widths[[i]], na.rm = TRUE))
            widths <- rbind(widths, vars$obj_data$all_widths[[i]])
          }
          for (k in 1:length(vars$obj_data$all_widths[[1]])) {
            names <- c(names, paste0("w",k))
          }
          all_names <- c(all_names, names, "wavg")
          results <- cbind(results, widths, wavg)
          display_names <- c(display_names, "Peak Width")
          display_objectives <- cbind(display_objectives, wavg)
          colnames <- c(colnames, "wavg p-val", "wavg Significance Status")
        }

        # MAXIMIZE AREA
        # see MAXIMIZE HEIGHT for analogous commentary
        if ("area" %in% input$objectives_new == TRUE) {
          # in case there are already areas in all_areas, we should clear them and redo
          if ("all_areas" %in% colnames(vars$obj_data) == TRUE) {
            vars$obj_data <- vars$obj_data %>% dplyr::mutate(all_areas = NULL)
          }

          # now we have to calculate the areas
          areas <- list()
          for (i in 1:length(input$upload_data[,1])) {
            data <- vars$obj_data$all_data[[i]]
            h <- vars$obj_data$all_heights[[i]]
            t <- vars$obj_data$all_times[[i]]
            a <- area(data = data, hts = h, tms = t)
            areas <- c(areas, list(a))
          }
          # add new column of areas to tibble
          vars$obj_data <- vars$obj_data %>% tibble::add_column(all_areas = areas)

          aavg <- c()
          areas <- c()
          names <- c()

          for (i in 1:length(vars$obj_data$all_areas)) {
            aavg <- append(aavg, mean(vars$obj_data$all_areas[[i]], na.rm = TRUE))
            areas <- rbind(areas, vars$obj_data$all_areas[[i]])
          }
          for (k in 1:length(vars$obj_data$all_areas[[1]])) {
            names <- c(names, paste0("a",k))
          }
          all_names <- c(all_names, names, "aavg")
          results <- cbind(results, areas, aavg)
          display_names <- c(display_names, "Peak Area")
          display_objectives <- cbind(display_objectives, aavg)
          colnames <- c(colnames, "aavg p-val", "aavg Significance Status")
        }

        # MINIMIZE AVG BASELINE
        # see MAXIMIZE HEIGHT for analogous commentary
        if ("bl" %in% input$objectives_new == TRUE) {
          # in case there are already baselines in all_bls, we should clear them and redo
          if ("all_bls" %in% colnames(vars$obj_data) == TRUE) {
            vars$obj_data <- vars$obj_data %>% dplyr::mutate(all_bls = NULL)
          }

          # now we have to calculate the areas
          bls <- c()
          for (i in 1:length(input$upload_data[,1])) {
            file <- dat()[i,]
            data <- utils::read.csv(file$datapath, skip = 2, header = FALSE, col.names = c("time", "TIC"))
            bl <- blc(data)
            bl <- bl$baseline
            bls <- c(bls, mean(bl))
          }
          # add new column of bls to tibble
          vars$obj_data <- vars$obj_data %>% tibble::add_column(all_bls = bls)

          name <- "bls"
          all_names <- c(all_names, name)
          results <- cbind(results, bls)
          display_names <- c(display_names, "Average Baseline")
          display_objectives <- cbind(display_objectives, bls)
          colnames <- c(colnames, "Baseline p-val", "Baseline Significance Status")
        }

        # MINIMIZE BASELINE VARIABILITY
        # see MAXIMIZE HEIGHT for analogous commentary
        if ("blv" %in% input$objectives_new == TRUE) {
          # in case there are already baselines in all_blvs, we should clear them and redo
          if ("all_blvs" %in% colnames(vars$obj_data) == TRUE) {
            vars$obj_data <- vars$obj_data %>% dplyr::mutate(all_blvs = NULL)
          }

          # now we have to calculate the baseline slopes
          blvs <- c()
          for (i in 1:length(input$upload_data[,1])) {
            file <- dat()[i,]
            data <- utils::read.csv(file$datapath, skip = 2, header = FALSE, col.names = c("time", "TIC"))
            bl <- blc(data)
            bl <- bl$baseline
            blvs <- c(blvs, mean(abs(diff(bl))))
          }
          # add new column of bls to tibble
          vars$obj_data <- vars$obj_data %>% tibble::add_column(all_blvs = blvs)

          name <- "blvs"
          all_names <- c(all_names, name)
          results <- cbind(results, blvs)
          display_names <- c(display_names, "Average Baseline Variability")
          display_objectives <- cbind(display_objectives, blvs)
          colnames <- c(colnames, "Baseline Var p-val", "Baseline Var Significance Status")
        }

        # MAXIMIZE RT SEPARATION
        if ("rt" %in% input$objectives_new == TRUE && length(input$rt_input) == 2) {
          # in case there are already rts in all_drts, we should clear them and redo
          if ("all_drts" %in% colnames(vars$obj_data) == TRUE) {
            vars$obj_data <- vars$obj_data %>% dplyr::mutate(all_drts = NULL)
          }

          # now we have to calculate the drts
          drts <- list()
          for (i in 1:length(input$upload_data[,1])) {
            rts <- utils::read.csv(input$upload_rts$datapath)
            compounds <- input$rt_input
            first <- which(colnames(rts) == compounds[1])
            second <- which(colnames(rts) == compounds[2])
            diff <- abs(rts[i,second] - rts[i, first])
            drts <- c(drts, diff)
          }
          # add new column of drts to tibble
          vars$obj_data <- vars$obj_data %>% tibble::add_column(all_drts = drts)

          drts <- c()
          for (i in 1:length(vars$obj_data$all_drts)) {
            drts <- c(drts, vars$obj_data$all_drts[[i]])
          }

          # if any rts are NA, then we clear the drts tibble and throw an error message to user
          if (any(is.na(drts))) {
            vars$obj_data <- vars$obj_data %>% dplyr::mutate(all_drts = NULL)
            objectives_temp <- input$objectives_new
            index <- which(objectives_temp == "rt")
            objectives_temp <- objectives_temp[-index]
            shiny::updateCheckboxGroupInput(session, "objectives_new", selected = objectives_temp)
            shinyalert::shinyalert("Error", "Cannot compute retention time differences for selected compounds because one or more of the retention times do not exist.")
          }
          else {
            name <- "drts"
            all_names <- c(all_names, name)
            results <- cbind(results, drts)
            display_names <- c(display_names, "RT Separation")
            display_objectives <- cbind(display_objectives, drts)
            colnames <- c(colnames, "drts p-val", "drts Significance Status")
          }
        }

        # MAXIMIZE RT SEPARATION 2
        if ("rt2" %in% input$objectives_new == TRUE && length(input$rt_input2) == 2) {
          # in case there are already rts2 in all_drts2, we should clear them and redo
          if ("all_drts2" %in% colnames(vars$obj_data) == TRUE) {
            vars$obj_data <- vars$obj_data %>% dplyr::mutate(all_drts2 = NULL)
          }

          # now we have to calculate the drts2
          drts2 <- list()
          for (i in 1:length(input$upload_data[,1])) {
            rts <- utils::read.csv(input$upload_rts$datapath)
            compounds <- input$rt_input2
            first <- which(colnames(rts) == compounds[1])
            second <- which(colnames(rts) == compounds[2])
            diff <- abs(rts[i,second] - rts[i, first])
            drts2 <- c(drts2, diff)
          }
          # add new column of drts to tibble
          vars$obj_data <- vars$obj_data %>% tibble::add_column(all_drts2 = drts2)

          drts2 <- c()
          for (i in 1:length(vars$obj_data$all_drts2)) {
            drts2 <- c(drts2, vars$obj_data$all_drts2[[i]])
          }

          # if any rts are NA, then we clear the drts2 tibble and throw an error message to user
          if (any(is.na(drts2))) {
            vars$obj_data <- vars$obj_data %>% dplyr::mutate(all_drts2 = NULL)
            objectives_temp <- input$objectives_new
            index <- which(objectives_temp == "rt2")
            objectives_temp <- objectives_temp[-index]
            shiny::updateCheckboxGroupInput(session, "objectives_new", selected = objectives_temp)
            shinyalert::shinyalert("Error", "Cannot compute retention time differences for selected compounds because one or more of the retention times do not exist.")
          }
          else {
            name <- "drts2"
            all_names <- c(all_names, name)
            results <- cbind(results, drts2)
            display_names <- c(display_names, "RT Separation (2)")
            display_objectives <- cbind(display_objectives, drts2)
            colnames <- c(colnames, "drts2 p-val", "drts2 Significance Status")
          }
        }

        # MAXIMIZE RT SEPARATION 3
        if ("rt3" %in% input$objectives_new == TRUE && length(input$rt_input3) == 2) {
          # in case there are already rts3 in all_drts3 we should clear them and redo
          if ("all_drts3" %in% colnames(vars$obj_data) == TRUE) {
            vars$obj_data <- vars$obj_data %>% dplyr::mutate(all_drts3 = NULL)
          }

          # now we have to calculate the drts3
          drts3 <- list()
          for (i in 1:length(input$upload_data[,1])) {
            rts <- utils::read.csv(input$upload_rts$datapath)
            compounds <- input$rt_input3
            first <- which(colnames(rts) == compounds[1])
            second <- which(colnames(rts) == compounds[2])
            diff <- abs(rts[i,second] - rts[i, first])
            drts3 <- c(drts3, diff)
          }
          # add new column of drts to tibble
          vars$obj_data <- vars$obj_data %>% tibble::add_column(all_drts3 = drts3)

          drts3 <- c()
          for (i in 1:length(vars$obj_data$all_drts3)) {
            drts3 <- c(drts3, vars$obj_data$all_drts3[[i]])
          }

          # if any rts are NA, then we clear the drts3 tibble and throw an error message to user
          if (any(is.na(drts3))) {
            vars$obj_data <- vars$obj_data %>% dplyr::mutate(all_drts3 = NULL)
            objectives_temp <- input$objectives_new
            index <- which(objectives_temp == "rt3")
            objectives_temp <- objectives_temp[-index]
            shiny::updateCheckboxGroupInput(session, "objectives_new", selected = objectives_temp)
            shinyalert::shinyalert("Error", "Cannot compute retention time differences for selected compounds because one or more of the retention times do not exist.")
          }
          else {
            name <- "drts3"
            all_names <- c(all_names, name)
            results <- cbind(results, drts3)
            display_names <- c(display_names, "RT Separation (3)")
            display_objectives <- cbind(display_objectives, drts3)
            colnames <- c(colnames, "drts3 p-val", "drts3 Significance Status")
          }
        }

        # execute if statement in case any rt objectives were selected but only one rt was input; else proceed normally
        if ("rt" %in% input$objectives_new &&
            length(input$objectives_new) == 1 &&
            is.null(vars$obj_data$all_drts)) {  } # do nothing if rt is the only objective selected and differences cannot be computed because of NA vals
        else if ("rt2" %in% input$objectives_new &&
                 length(input$objectives_new) == 1 &&
                 is.null(vars$obj_data$all_drts2)) { }
        else if ("rt3" %in% input$objectives_new &&
                 length(input$objectives_new) == 1 &&
                 is.null(vars$obj_data$all_drts3)) { }
        else {
          results <- as.data.frame(results)
          # subtract the first column from results dataframe, which was all NA (as a placeholder earlier)
          results <- results[-1]
          # test for the case that there is only one method file input;
          # things get wonky and we have to make an adjustment later
          one_test <- length(rownames(results)) == 1

          results <- cbind(dat()[,1], results)
          all_names <- c("File Name", all_names)
          colnames(results) <- unlist(all_names)

          # delete first row of display_objectives, which is NA
          display_objectives <- display_objectives[,-1]
          display_objectives <- cbind(dat()[,1], display_objectives)
          display_objectives <- as.data.frame(display_objectives)
          colnames(display_objectives) <- display_names
          if (one_test) {
            rownames(display_objectives) <- 1
          }

          # output objective table
          output$objectives_tbl <- DT::renderDT(display_objectives,
                                                options = list(
                                                  dom = 'tip'
                                                  # scrollY = TRUE
                                                  # scrollCollapse = TRUE,
                                                  # pageLength = length(input$upload_data[,1]),
                                                  # ordering = FALSE
                                                ),
                                                # options = list(scrollX = TRUE, dom = 'ti')
                                                selection = 'none')

          output$caveat <- shiny::renderUI(htmltools::h5("*Note that values shown are averages (exception: RT separation)."))

          dwnld_results_obj(results) # to download all optimized parameters

          output$dwnld_results_obj_b <- shiny::renderUI({shiny::downloadButton("dwnld_results_obj", "Download Objective Data")})
        }
      }
    }
    else (
      shinyalert::shinyalert("Error", "Insufficient data to compute objectives. Potential error sources: (1) No uploaded screening data;(2) Heights not computed; (3) No objectives selection.", type = "error")
    )
  }
  )

  #------------download all objective data---------------
  output$dwnld_results_obj <- shiny::downloadHandler(
    filename = function() {
      paste("objective_results-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      utils::write.csv(dwnld_results_obj(), file, row.names = FALSE)
    }
  )


  #===============BBD=================BBD==============

  #----------Add factor for BBD-----------------
  shiny::observeEvent(input$add_factor_bbd, {
    if (input$factor_bbd != "" && input$low_val_bbd < input$mid_val_bbd && input$mid_val_bbd < input$hi_val_bbd) {

      bbd_temp = data.frame(Parameter = c(input$factor_bbd), Low = c(input$low_val_bbd), Mid = c(input$mid_val_bbd), High = c(input$hi_val_bbd))

      vars$bbd_data <- rbind(vars$bbd_data, bbd_temp)

      shiny::updateTextInput(session, "factor_bbd", value = "")
    }
    else {
      shinyalert::shinyalert("Oops!", "Please check that the parameter field is nonempty and the numeric values increase from low to high.", type = "error")
    }
  })

  #---------------Print table of BBD factors---------------
  output$fctr_tbl_bbd <- DT::renderDataTable(vars$bbd_data,
                                             rownames = FALSE,
                                             filter = "none",
                                             options = list(
                                               dom = 't',
                                               searching = FALSE,
                                               paging = FALSE
                                             ),
                                             editable = 'cell',
                                             selection = 'single')


  #---------------Edit data from BBD table-----------------
  shiny::observeEvent(input$fctr_tbl_bbd_cell_edit, {
    vars$bbd_data <<- DT::editData(data = vars$bbd_data,
                                   info = input$fctr_tbl_bbd_cell_edit,
                                   rownames = FALSE)
  })


  #--------------Delete row from BBD factors table---------------
  shiny::observeEvent(input$deleteRow_bbd, {
    if (!is.null(input$fctr_tbl_bbd_rows_selected)) {
      vars$bbd_data <- vars$bbd_data[-as.numeric(input$fctr_tbl_bbd_rows_selected),]
    }
  })

  #------------Make BBD----------------
  shiny::observeEvent(input$make_bbd, {
    if (nrow(vars$bbd_data) > 2) {
      bbd_tbl <- DoE.wrapper::bbd.design(nfactors = nrow(vars$bbd_data))
      bbd_tbl <- as.data.frame(bbd_tbl)
      for (i in 1:nrow(vars$bbd_data)) {
        low <- vars$bbd_data$Low[i]
        mid <- vars$bbd_data$Mid[i]
        hi <- vars$bbd_data$High[i]
        for (j in 1:length(bbd_tbl[,i])) {
          if (bbd_tbl[j,i] == -1) {bbd_tbl[j,i] <- low}
          else if (bbd_tbl[j,i] == 0) {bbd_tbl[j,i] <- mid}
          else if (bbd_tbl[j,i] == 1) {bbd_tbl[j,i] <- hi}
        }
      }
      colnames(bbd_tbl) <- vars$bbd_data$Parameter

      output$bbd_table <- DT::renderDataTable(bbd_tbl,
                                              options = list(
                                                dom = 'ltip'
                                              ))

      vars$bbd_tbl_dwnld <- bbd_tbl

      output$dwnld_bbd_b <- shiny::renderUI({shiny::downloadButton("dwnld_bbd", "Download BBD")})
    }
    else {
      shinyalert::shinyalert("Error", "There must be at least three factors to generate the Box Behnken Design.", type = "error")
    }
  })

  #-------------To download BBD-------------
  output$dwnld_bbd <- shiny::downloadHandler(
    filename = function() {
      paste("BBD-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      utils::write.csv(vars$bbd_tbl_dwnld, file, row.names = FALSE)
    }
  )

  #--------------Display uploaded BBD---------------
  data_uploaded_bbd <- shiny::reactive({
    file1 <- input$upload_bbd
    fulldf <- utils::read.table(file = file1$datapath, sep = ",", header = TRUE)
    #fulldf[,2:ncol(fulldf)]
  })

  # if the BBD that was uploaded does not meet the dimensions criteria (that is, number of rows of BBD is not
  # the same as the number of rows of method data), an error message is issued
  bbd_test <- shiny::reactive({
    shiny::req(input$upload_bbd)
    good_bbd <- length(data_uploaded_bbd()[,1]) == length(input$upload_data[,1])
    shinyFeedback::feedbackWarning("upload_bbd", !good_bbd, "Error: file format")
  })

  output$bbd_test <- shiny::renderText(bbd_test())

  output$BBD_actual <- DT::renderDataTable(
    data_uploaded_bbd(), options = list(scrollX = TRUE, dom = 'tip'), selection = 'none'
  )

  # when BBD is uploaded, we then have the clear to render the limiting factors input
  shiny::observe({shiny::req(input$upload_bbd)
    output$factors <- shiny::renderUI({shiny::selectInput("limiting_factors", "Which variables have experimental or physical ranges of validity?", colnames(data_uploaded_bbd()), multiple = TRUE)})
  })

  # default assign each limiting factor with the highs and lows from the BBD
  shiny::observe({shiny::req(input$limiting_factors)
    lows <- c()
    highs <- c()
    for (j in 1:length(input$limiting_factors)) {
      index <- which(colnames(data_uploaded_bbd()) == input$limiting_factors[j])
      min <- min(data_uploaded_bbd()[,index])
      max <- max(data_uploaded_bbd()[,index])
      lows <- c(lows, min)
      highs <- c(highs, max)
    }
    vars$opt_data <- data.frame(Parameter = c(input$limiting_factors), Low = c(lows), High = c(highs))
  })

  shiny::observe(
    if(is.null(input$limiting_factors)) {
      vars$opt_data <- data.frame(Parameter = c(), Low = c(), High = c())
    }
  )

  # the user can change the defauls highs and lows for the limiting factors
  shiny::observeEvent(input$limit_tbl_bbd_cell_edit, {
    vars$opt_data <- DT::editData(data = vars$opt_data,
                                  info = input$limit_tbl_bbd_cell_edit,
                                  rownames = FALSE)
  })

  output$limit_tbl_bbd <- DT::renderDataTable(vars$opt_data,
                                              rownames = FALSE,
                                              filter = "none",
                                              options = list(
                                                dom = 't',
                                                searching = FALSE,
                                                paging = FALSE
                                              ),
                                              editable = 'cell',
                                              selection = 'none')


  #============OPTIMIZE================OPTIMIZE==============

  #--------------Run the optimization algorithm-------------

  factor_num <- shiny::reactiveVal()
  shiny::observe(
    factor_num(input$limiting_factors)
  )
  obj <- shiny::reactiveVal()
  shiny::observe(
    obj(input$objectives_new)
  )

  shiny::observeEvent(input$bbd_b, {
    # must pass 4 conditions: screening data is uploaded, heights computed, objectives computed, and objectives selected
    if (!is.null(input$upload_data) &&
        !is.null(input$objectives_new) &&
        length(vars$obj_data$all_heights) == length(data_uploaded_bbd()[,1]) &&
        length(input$upload_data[,1]) == length(vars$obj_data$all_heights)) {

      results <- dwnld_results_obj()[-1]

      output$dwnld_results_bbd_b <- shiny::renderUI({shiny::downloadButton("dwnld_results_bbd", "Download Objective Data")})

      # for downloading objective data with bbd
      dwnld_results(cbind(data_uploaded_bbd(),results))

      # get optimal data from function
      items <- opt(objectives = input$objectives_new, bbd = data_uploaded_bbd(), results = results, lim_fac = factor_num(), valid_range_data = vars$opt_data)

      bad_vars <- items[[1]]
      obj_display <- items[[2]]
      optimal_display <- items[[3]]

      # make notice if Hessian is not satisfied
      if (!is.null(bad_vars)) {
        shinyjs::show("hessian_warning")
        if (length(bad_vars) > 1) {
          to_print <- paste(bad_vars, collapse = ", ")
          output$hessian_warning <- shiny::renderText(paste("Warning: Note that the parameters ", to_print, " do not satisfy the Hessian condition and the unbounded solution is not a true optimum. Be sure to enter a valid range for a true solution."))
        }
        else {
          output$hessian_warning <- shiny::renderText(paste("Warning: Note that the parameter ", bad_vars, " does not satisfy the Hessian condition and the unbounded solution is not a true optimum. Be sure to enter a valid range for a true solution."))
        }
      }
      else {
        shinyjs::hide("hessian_warning")
        output$hessian_warning <- NULL
      }

      output$obj_title <- shiny::renderUI({htmltools::h4("Objective Prediction at Optimum")})

      # display objective response to optimal
      output$objectives_sol <- DT::renderDataTable(obj_display,
                                                   options = list(scrollX = TRUE, dom = 'ti'),
                                                   selection = 'none')

      # display optimal solution
      output$optimal_output <- DT::renderDataTable(optimal_display,
                                                   options = list(scrollX = TRUE, dom = 'ti'),
                                                   selection = 'none'
      )

    }
    else {
      shinyalert::shinyalert("Error", "Insufficient data to compute optimization. Potential error sources: (1) No uploaded raw data; (2) Heights not computed; (3) No objectives selection; (4) No filled BBD table uploaded.", type = "error")
    }
  })

  # download all results
  output$dwnld_results_bbd <- shiny::downloadHandler(
    filename = function() {
      paste("all_results-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      utils::write.csv(dwnld_results(), file, row.names = FALSE)
    }
  )
}

# Notice: These data were produced by Battelle Savannah River Alliance, LLC under Contract No 89303321CEM000080 with the Department of Energy. During the period of commercialization or such other time period specified by DOE, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, and perform publicly and display publicly, by or on behalf of the Government. Subsequent to that period, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, distribute copies to the public, perform publicly and display publicly, and to permit others to do so. The specific term of the license can be identified by inquiry made to Contract or DOE. NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY DATA, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
# ©2024 Battelle Savannah River Alliance, LLC
