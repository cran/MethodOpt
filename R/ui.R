objectives <- c("Maximize Peak Height" = "height",
                "Minimize Peak Width" = "width",
                "Maximize Peak Area" = "area",
                "Minimize Baseline" = "bl",
                "Minimize Baseline Variability" = "blv",
                "Maximize Retention Time Separation" = "rt",
                "Maximize Retention Time Separation (2)" = "rt2",
                "Maximize Retention Time Separation (3)" = "rt3")#, "Maximize Peak Area", "Gaussian Peak Skew", "Maximize Peak Separation")

ui <- shiny::fluidPage(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  shiny::titlePanel("Multivariate Optimization Program"),
  #------------------FFD setup--------------------------
  shiny::tabsetPanel(
    shiny::tabPanel(
      "FFD Setup",
      shiny::sidebarLayout(position = "left",
                           shiny::sidebarPanel(htmltools::h3("Experiment Input"), # NOTE: 'htmltools::h3' is third level header.

                                 #factors and other input information to generate FFD table

                                 shiny::textInput(inputId = "factor", "Parameter", value = "", placeholder = "e.g. inlet temp", width = NULL),
                                 shiny::fluidRow(
                                   shiny::column(6, offset = 0,
                                          shiny::numericInput(inputId = "low_val", "Low", "0", step = 'none', width = "100px")
                                   ),
                                   shiny::column(6, offset = 0,
                                          shiny::numericInput(inputId = "hi_val", "High", "1", width = "100px")
                                   )
                                 ),
                                 shiny::fluidRow(
                                   shiny::column(5, offset = 2,
                                                 shiny::actionButton("add_factor", "Save Parameter")
                                   )
                                 ),
                                 shiny::numericInput(inputId = "res", "Resolution", "3", min = 3),
                                 shiny::textOutput("res_test"),
                                 htmltools::h4("Saved Parameters"),
                                 shiny::column(width = 12,
                                        DT::dataTableOutput("fctr_tbl"), style = "overflow-x: scroll;"
                                 ),
                                 shiny::actionButton("deleteRow", "Delete Row"),
                                 shiny::actionButton("make_ffd", "Generate FFD", icon = shiny::icon("table"))
                    ),

                    shiny::mainPanel(
                      shiny::tabsetPanel(
                        shiny::tabPanel(
                          "Full Factorial Design Table",
                          shiny::column(width = 12,
                                 DT::dataTableOutput(outputId = "ffd_table"), style = "overflow-x: scroll;"
                          ),
                          shiny::uiOutput("button1")
                        ),
                        shiny::tabPanel(
                          "FFD table -- Experimental Utility",
                          shiny::column(width = 12,
                                 DT::dataTableOutput(outputId = "ffd_table_exper"), style = "overflow-x: scroll;"
                          ),
                          shiny::uiOutput("button2")
                        )
                      )
                    )
      )
    ),

    #-----------------BBD setup-----------------
    shiny::tabPanel(
      "BBD Setup",
      shiny::sidebarLayout(position = "left",
                           shiny::sidebarPanel(htmltools::h3("BBD Input"),
                                               shiny::textInput(inputId = "factor_bbd", "Parameter", value = "", placeholder = "e.g. inlet temp", width = NULL),
                                               shiny::fluidRow(
                                                 shiny::column(4, offset = 0,
                                          shiny::numericInput(inputId = "low_val_bbd", "Low", "0", step = 'none', width = "100px")
                                   ),
                                   shiny::column(4, offset = 0,
                                                 shiny::numericInput(inputId = "mid_val_bbd", "Middle", "1", width = "100px")
                                   ),
                                   shiny::column(4, offset = 0,
                                                 shiny::numericInput(inputId = "hi_val_bbd", "High", "2", width = "100px")
                                   )
                                 ),
                                 shiny::fluidRow(
                                   shiny::column(5, offset = 2,
                                                 shiny::actionButton("add_factor_bbd", "Save Parameter")
                                   )
                                 ),
                                 htmltools::h4("Saved Parameters"),
                                 shiny::column(width = 12,
                                        DT::dataTableOutput("fctr_tbl_bbd"), style = "overflow-x: scroll;"
                                 ),
                                 shiny::actionButton("deleteRow_bbd", "Delete Row"),
                                 shiny::actionButton("make_bbd", "Generate BBD", icon = shiny::icon("table"))
                    ),
                    shiny::mainPanel(
                      shiny::tabPanel(
                        "Box-Behnken Design Table",
                        shiny::column(width = 12,
                               DT::dataTableOutput(outputId = "bbd_table"), style = "overflow-x: scroll;"
                        ),
                        shiny::uiOutput("dwnld_bbd_b")
                      )
                    )
      )
    ),
    #----------Analysis---------------------------
    shiny::tabPanel(
      "Data Analysis",
      htmltools::h5("Instructions: Upload experimental data below. Only files of type .csv are accepted."),
      shiny::fileInput("upload_data", "Upload Data", multiple = TRUE, accept = ".csv"),
      shiny::textOutput("file_test"),
      htmltools::tags$head(htmltools::tags$style("#file_test{color: red;
                                     font-size: 20px;
                                     font-style: italic;
                                     }"
      )
      ),
      # actionButton("restart", "Restart"),
      DT::dataTableOutput("files"),
      shiny::uiOutput("reorder"),
      shiny::tabsetPanel(
        shiny::tabPanel(
          "Plot",
          shiny::sidebarLayout(position = "left",
                               shiny::sidebarPanel(htmltools::h3("Plotting Options"), width = 3,
                                     htmltools::h5("Upload raw data to use plotting feature."),
                                     shiny::numericInput("row_skip", "Row skip:", 4, min = 0, step = 1, width = "50%"),
                                     shiny::textInput("xlabel", "x-axis label:", value = "time", placeholder = "e.g., time"),
                                     shiny::textInput("ylabel", "y-axis label:", value = "TIC", placeholder = "e.g., TIC"),
                                     shiny::checkboxInput("log", "Logarithmic scale", value = FALSE),
                                     shiny::checkboxInput("bg_sub", "Subtract baseline", value = FALSE),
                                     shiny::checkboxInput("overlay", "Overlay baseline", value = FALSE),
                                     shiny::numericInput("noise", "Background noise estimate", value = 0, min = 0, step = 100),
                                     htmltools::h5("Select file from table to render its plot.")
                        ),
                        shiny::mainPanel(
                          shiny::textOutput("plotname"),
                          shiny::plotOutput("plot1"),
                          shiny::uiOutput("button3"),
                          shinyjs::hidden(
                            shiny::div(id = "show_dwnld_b", style = "display:inline-block",
                                       shiny::actionButton("dwnld_all", "Download All")
                            )
                          ),
                          shinyBS::bsModal("dwnld_warning", "Download Warning", trigger = "dwnld_all",
                                           list(htmltools::h5("Mass downloads will be saved uniformly according to current settings in option bar. Are you sure you would like to proceed?")),
                                           shiny::downloadButton("mass_download", "Download All")
                          ),
                        )
          )
        ),
        shiny::tabPanel(
          "Identify Peaks",
          shiny::sidebarLayout(position = "left",
                               shiny::sidebarPanel(
                                 htmltools::h3("Identification Options"), width = 3,
                                 shiny::radioButtons("rt_select", "Retention times available?", choices = c("Yes" = "yes", "No" = "no")),
                                 shiny::fileInput("upload_rts", "Upload RTs", accept = ".csv"),
                                 shiny::uiOutput("rts_warning"),
                          htmltools::tags$head(htmltools::tags$style("#rts_warning{color: red;
                                     font-size: 15px;
                                     font-style: italic;
                                     }"
                          )
                          ),
                          htmltools::h5("(Note: Uploading retention times is suggested but not required.)"),
                          shiny::numericInput("peak_num", "Number of peaks to search for", 11, min = 1, max = 20, step = 1),
                          htmltools::h5("The \"SEARCH\" button applies to all uploaded data and will overwrite any existing data."),
                          shiny::actionButton("find_peaks", "SEARCH", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          shiny::uiOutput("instructions"),
                        ),
                        shiny::mainPanel(
                          shiny::fluidRow(htmltools::h4("Peak search options")),
                          shiny::fluidRow(
                            shiny::column(width = 4,
                                          shiny::checkboxInput("search_select", "Shrink search window?", value = FALSE)
                            ),
                            shiny::column(width = 4,
                                          shiny::numericInput("search_min", "Begin search at time:", value = "0", step = 1)
                            ),
                            shiny::column(width = 4,
                                   shiny::numericInput("search_max", "Begin search at time:", value = "50", step = 1)
                            )
                          ),
                          shiny::actionButton("regen_plot", "Search this plot"),
                          shiny::plotOutput("peak_plot"),
                          shiny::downloadButton("dwnld_peaks", label = "Download Plot"),
                          DT::DTOutput("master_data"),
                          shiny::actionButton("replace", "Delete & Find Next"),
                          shiny::br()
                        )
          )
        ),
        shiny::tabPanel(
          "Objectives",
          shiny::sidebarLayout(
            position = "left",
            shiny::sidebarPanel(
              htmltools::h3("Objective Calculation Options"), width = 3,
              shiny::checkboxGroupInput("objectives_new", "Objectives", objectives),
              shiny::uiOutput("rt_input_render")
              # actionButton("objectives_new_b", "Calculate Objectives", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            shiny::mainPanel(
              htmltools::h3("Objectives Results"),
              DT::DTOutput("objectives_tbl"),
              shiny::uiOutput("caveat"),
              shiny::uiOutput("dwnld_results_obj_b")
            )
          )
        ),
        shiny::tabPanel(
          "ANOVA",
          shiny::sidebarLayout(position = "left",
                               shiny::sidebarPanel(
                                 htmltools::h3("ANOVA Input"), width = 3,
                                 shiny::fileInput("upload_ffd", "Upload Full Factorial Design", multiple = FALSE, accept = ".csv"),
                                 shiny::textOutput("ffd_test"),
                                 shiny::actionButton("disp_FFD", "Display FFD", icon = shiny::icon("table")),
                                 htmltools::h6("Note: Uploaded FFD table should contain actual high and low data (i.e., not +/-1's). Be sure data is formatted so that the first row is parameter labels and there is no index column."),
                          shinyBS::bsModal("FFD_viewer", "Uploaded FFD", trigger = "disp_FFD",
                                           DT::dataTableOutput("FFD_actual")
                          ),
                          shiny::selectInput("alpha", "Alpha value", c("0.01" = "0.01", "0.005" = "0.005", "0.001" = "0.001")),
                          shiny::textOutput("percent_conf"),
                          shiny::actionButton("objectives_b", "Run ANOVA", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        ),
                        shiny::mainPanel(
                          htmltools::h3("Significant Parameters"),
                          DT::dataTableOutput("filled_ffd"),
                          shiny::uiOutput("dwnld_results_b"),
                          DT::dataTableOutput("bbd_info"),
                        )
          )
        ),
        shiny::tabPanel(
          "Optimization",
          shiny::sidebarLayout(position = "left",
                               shiny::sidebarPanel(
                                 htmltools::h3("Optimization Input"), width = 4,
                                 shiny::fileInput("upload_bbd", "Upload Box Behnken Desgin", multiple = FALSE, accept = ".csv"),
                                 shiny::textOutput("bbd_test"),
                                 shiny::actionButton("disp_BBD", "Display BBD", icon = shiny::icon("table")),
                                 htmltools::h6("Note: Uploaded BBD table should contain actual high and low data (i.e., not +/-1,0). Be sure data is formatted so that the first row is parameter labels and there is no index column."),
                          shinyBS::bsModal("BBD_viewer", "Box Behnken Design", trigger = "disp_BBD",
                                           DT::dataTableOutput("BBD_actual")
                          ),
                          htmltools::h4("Physical Limit Input"),
                          shiny::uiOutput("factors"),
                          shiny::column(width = 12,
                                 DT::dataTableOutput("limit_tbl_bbd"), style = "overflow-x: scroll;"
                          ),
                          shiny::actionButton("bbd_b", "Optimize", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        ),
                        shiny::mainPanel(
                          htmltools::h3("Results"),
                          DT::dataTableOutput("optimal_output"),
                          shiny::uiOutput("hessian_warning"),
                          htmltools::tags$head(htmltools::tags$style("#hessian_warning{color: red;
                                     font-size: 15px;
                                     font-style: italic;
                                     }"
                          )
                          ),
                          shiny::uiOutput("obj_title"),
                          DT::dataTableOutput("objectives_sol"),
                          shiny::uiOutput("dwnld_results_bbd_b"),
                          shiny::verbatimTextOutput("test")
                        )
          )
        )
      )
    )
  )
)

# Notice: These data were produced by Battelle Savannah River Alliance, LLC under Contract No 89303321CEM000080 with the Department of Energy. During the period of commercialization or such other time period specified by DOE, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, and perform publicly and display publicly, by or on behalf of the Government. Subsequent to that period, the Government is granted for itself and others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide license in this data to reproduce, prepare derivative works, distribute copies to the public, perform publicly and display publicly, and to permit others to do so. The specific term of the license can be identified by inquiry made to Contract or DOE. NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY DATA, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
# ©2024 Battelle Savannah River Alliance, LLC
