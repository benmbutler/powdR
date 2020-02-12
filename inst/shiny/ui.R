shiny::shinyUI(
  shiny::navbarPage("powdR",

             #################################
             ## TAB 1: REFERENCE LIBRARY BUILDER
             #################################
             shiny::tabPanel("Reference Library Builder",
                      shiny::fluidRow(
                        shiny::column(4, shiny::wellPanel(
                          shiny::h3("1. File upload"),
                          shiny::h4("XRPD data"),
                          shiny::helpText(shiny::withMathJax("Choose a .csv file containing the 2\\(\\theta\\) scale
                                    and the count intensities of all reference patterns.")),
                          shiny::div(style="display: inline-block;vertical-align:top; width: 300px;",
                              shiny::fileInput(inputId = "uploadXRD",
                                    label = NULL,
                                    multiple = FALSE,
                                    accept = ".csv")),
                          shiny::div(style="display: inline-block;vertical-align:center; width: 0px;",
                                     shinyWidgets::dropdownButton(
                                shiny::downloadLink(outputId = "download_xrd_eg",
                                             label = "Download an example .csv file"),
                                circle = FALSE, status = "danger", icon = shiny::icon("question"),
                                width = "300px", size = "sm"
                              )),
                          shiny::h4("Phase information"),
                          shiny::helpText("Upload a .csv file containing
                                    the id's, names and reference intensity ratios of
                                    all reference patterns."),
                          shiny::div(style="display: inline-block;vertical-align:top; width: 300px;",
                              shiny::fileInput(inputId = "uploadPHASE",
                                    label = NULL,
                                    multiple = FALSE,
                                    accept = ".csv")),
                          shiny::div(style="display: inline-block;vertical-align:center; width: 0px;",
                                     shinyWidgets::dropdownButton(
                                shiny::downloadLink(outputId = "download_phases_eg",
                                             label = "Download an example .csv file"),
                                circle = FALSE, status = "danger", icon = shiny::icon("question"),
                                width = "300px", size = "sm"
                              )),
                          shiny::tags$hr(),
                          shiny::div(style="display: inline-block;vertical-align:bottom; width: 100px;",
                              h3("2. Build")),
                          shiny::div(style="display: inline-block;vertical-align:bottom; width: 100px;",
                              shiny::actionButton("BuildLibButton", "Click to build library")),
                          shiny::tags$hr(),
                          shiny::h3("3. Download library"),
                          shiny::helpText("Provide a name for the new powdRlib object.
                                    This is what the library will be called if it is
                                   subsequently loaded into R (can be kept as the default
                                   'RefLib'):"),
                          shiny::textInput("name", label = NULL,
                                    "RefLib"),
                          shiny::downloadButton(outputId = "download_lib",
                                         label = "Download library as .Rdata binary file")
                        )),
                        shiny::column(8, shiny::wellPanel(
                          shiny::h3("Phases in your reference library"),
                          shiny::tags$hr(),
                          DT::dataTableOutput("minerals_table")
                        ))
                      ) # end fluidRow
             ),

             #################################
             ## TAB 2: REFERENCE LIBRARY PLOTTER
             #################################
             shiny::tabPanel("Reference Library Viewer",
                             shiny::fluidRow(
                               shiny::column(4, shiny::wellPanel(
                                 shiny::helpText("Choose a .Rdata reference library to load. Must be
                                    a powdRlib object created using either the powdRlib function,
                                    or via the Reference Library Builder in this application."),
                                 shiny::fileInput(inputId = "loadLIB_plotter",
                                    label = NULL,
                                    multiple = FALSE,
                                    accept = ".Rdata")
                        )),
                        shiny::column(4, shiny::wellPanel(
                          shiny::helpText("Select a wavelength to use when calculating d-spacings."),
                          shiny::selectInput(inputId = "selectWAVELENGTH",
                                      label = NULL,
                                      multiple = FALSE,
                                      choices = list("Cu (1.54056 Angstroms)" = "Cu",
                                                     "Co (1.78897 Angstroms)" = "Co"))
                        )),
                        shiny::column(4, shiny::wellPanel(
                          shiny::helpText("Choose phases from the library to plot."),
                          shiny::selectInput(inputId = "selectPHASES_plotter",
                                      label = NULL,
                                      choices = c("Please upload a reference library"),
                                      selected = "Please upload a reference library",
                                      multiple = TRUE,
                                      selectize = TRUE)
                        ))
                      ), # end fluidRow
                      shiny::fluidRow(
                        shiny::column(12, shiny::wellPanel(
                          plotly::plotlyOutput("lib_plot", width = "auto", height = 600),
                          shiny::tags$hr(),
                          DT::dataTableOutput("lib_table")
                        ))
                      )
             ),

             #################################
             ## TAB 3: REFERENCE LIBRARY EDITOR
             #################################
             shiny::tabPanel("Reference Library Editor",
                             shiny::fluidRow(
                               shiny::column(4, shiny::wellPanel(
                                 shiny::h3("1. Load a library"),
                                 shiny::helpText("Choose a .Rdata reference library to load. Must be
                                    a powdRlib object created using either the powdRlib function,
                                    or via the Reference Library Builder in this application."),
                                 shiny::div(style="display: inline-block;vertical-align:top; width: 300px;",
                                            shiny::fileInput(inputId = "loadLIB_editor",
                                        label = NULL,
                                        multiple = FALSE,
                                        accept = ".Rdata")),
                                 shiny::tags$hr(),
                                 shiny::h3("2. Select the subset mode"),
                                 shiny::selectInput(inputId = "selectMODE_editor",
                                      label = NULL,
                                      choices = c("keep", "remove"),
                                      multiple = FALSE),
                                 shiny::tags$hr(),
                                 shiny::h3("3. Select reference patterns to subset"),
                                 shiny::selectInput(inputId = "selectPHASES_editor",
                                      label = NULL,
                                      choices = c("Please upload a reference library"),
                                      selected = "Please upload a reference library",
                                      multiple = TRUE,
                                      selectize = TRUE),
                                 shiny::tags$hr(),
                                 shiny::div(style="display: inline-block;vertical-align:bottom; width: 120px;",
                              h3("4. Subset")),
                              shiny::div(style="display: inline-block;vertical-align:bottom; width: 100px;",
                              actionButton("SubsetLibButton", "Click to subset library")),
                              shiny::tags$hr(),
                              shiny::h3("5. Download library"),
                              shiny::helpText("Provide a name for the new powdRlib object.
                                    This is what the library will be called if it is
                                   subsequently loaded into R (can be kept as the default
                                   'RefLib'):"),
                              shiny::textInput("name_editor", label = NULL,
                                    "RefLib"),
                              shiny::downloadButton(outputId = "download_subset_lib",
                                         label = "Download library as .Rdata binary file")

                        )),
                        shiny::column(8, shiny::wellPanel(
                          shiny::h3("Phases in your subset reference library"),
                          shiny::tags$hr(),
                          DT::dataTableOutput("minerals_subset_table")
                        ))
                      ) #End fluid row

             ), #End tabpanel

             #################################
             ## TAB 3: Background Fitting
             #################################
             #tabPanel("Background Fitting",
            #          fluidRow(
            #            column(3, wellPanel(
            #              h3("1. Load a sample"),
            #              helpText("File must be .xy format (space separated)"),
            #              fileInput(inputId = "loadXYbkg",
            #                        label = NULL,
            #                        multiple = FALSE,
            #                        accept = c(".xy", ".XY")),
            #              h3("2. Background parameters"),
            #              helpText("lamda: 2nd derivative penalty for primary smoothing (default = 0.5)"),
            #              sliderInput("bkg_lambda",
            #                          label = NULL,
            #                          min = 0.1, max = 10,
            #                          value = 0.5, step = 0.1),
            #              helpText("hwi: half width of local windows (default = 25)"),
            #              sliderInput("bkg_hwi",
            #                          label = NULL,
            #                          min = 10, max = 100,
            #                          value = 25, step = 1),
            #              helpText("it: number of iterations in suppression loop (default = 50)"),
            #              sliderInput("bkg_it",
            #                          label = NULL,
            #                          min = 1, max = 200,
            #                          value = 50, step = 1),
            #             helpText("int: number of buckets to divide the data into (default = 1000)"),
            #              sliderInput("bkg_int",
            #                          label = NULL,
            #                          min = 10, max = 2000,
            #                          value = 1000, step = 10)
            #              )),
            #            column(9, wellPanel(
            #              div(style="display: inline-block;vertical-align:top; width: 200px;",
            #                  h4("Fitted background plot")),
            #              div(style="display: inline-block;vertical-align:top; width: 300px;",
            #                  dropdownButton(
            #                    sliderInput("bkg_x", "adjust the x-axis",
            #                                min = 4, max = 70,
            #                                value = c(4,70), step = 1),
            #                    sliderInput("bkg_y", "adjust the y-axis",
            #                                min = 0, max = 10000,
            #                                value = c(0,5000), step = 1),
            #                    circle = FALSE, status = "danger", icon = icon("sliders"),
            #                    width = "400px", size = "sm",
            #                    tooltip = tooltipOptions(title = "Click to adjust graph axes")
            #                  )),
            #              plotOutput("bkg_plot", width = "100%", height = "600px")
            #              ))
            #            ) # end fluidRow
            # ),

            #################################
            ## TAB 4: Full pattern summation
            #################################
            shiny::tabPanel("Full Pattern Summation",
                            shiny::fluidRow(
                              shiny::column(3, shiny::wellPanel(
                                shiny::h3("1. Load a sample for quantification"),
                                shiny::helpText("Must be .xy format (space separated)"),
                                shiny::div(style="display: inline-block;vertical-align:top; width: 225px;",
                                           shiny::fileInput(inputId = "loadXY",
                                                            label = NULL,
                                                            multiple = FALSE,
                                                            accept = c(".xy", ".XY"))),
                                shiny::div(style="display: inline-block;vertical-align:center; width: 0px;",
                                           shinyWidgets::dropdownButton(
                                             shiny::downloadLink(outputId = "download_soil_sand",
                                                                 label = "Sandstone_example.xy  "),
                                             shiny::downloadLink(outputId = "download_soil_lime",
                                                                 label = "Limestone_example.xy  "),
                                             shiny::downloadLink(outputId = "download_soil_granite",
                                                                 label = "Granite_example.xy  "),
                                             shiny::downloadLink(outputId = "download_rj_mix1",
                                                                 label = "rockjock_mix1.xy  "),
                                             shiny::downloadLink(outputId = "download_rj_mix2",
                                                                 label = "rockjock_mix2.xy  "),
                                             circle = FALSE, status = "danger", icon = shiny::icon("question"),
                                             width = "300px", size = "sm"
                                           )),
                                shiny::h3("2. Load a reference library"),
                                shiny::helpText("Must be a .Rdata powdRlib object"),
                                shiny::div(style="display: inline-block;vertical-align:top; width: 225px;",
                                           shiny::fileInput(inputId = "loadLIB",
                                                            label = NULL,
                                                            multiple = FALSE,
                                                            accept = ".Rdata")),
                                shiny::div(style="display: inline-block;vertical-align:center; width: 0px;",
                                           shinyWidgets::dropdownButton(
                                             shiny::downloadLink(outputId = "download_example_ref",
                                                                 label = "example_powdRlib.Rdata"),
                                             shiny::downloadLink(outputId = "download_rj_lib",
                                                                 label = "rockjock_powdRlib.Rdata"),
                                             circle = FALSE, status = "danger", icon = shiny::icon("question"),
                                             width = "300px", size = "sm"
                                           )),
                                shiny::h3("3. Select a solver"),
                                shiny::helpText("Choose the optimisation routine"),
                                shiny::selectInput(inputId = "selectSolver_fps",
                                                   label = NULL,
                                                   choices = c("BFGS", "Nelder-Mead", "CG", "L-BFGS-B", "NNLS"),
                                                   selected = "BFGS"),
                                shiny::uiOutput("selectOBJui_fps"),
                                shiny::tags$hr(),
                                shiny::h3("4. Select phases"),
                                #shiny::uiOutput("selectPHASESui"),
                                shiny::selectInput(inputId = "selectPHASES_fps",
                                                   choices = c(""),
                                                   label = NULL,
                                                   multiple = TRUE,
                                                   selectize = TRUE),
                                shiny::helpText("Choose an internal standard for peak alignment. If the manual alignment
                                   box below is ticked, then this internal standard is not used and instead
                                   the sample is aligned by the amount selected in the alignment slider."),
                                shiny::selectInput(inputId = "selectINT_fps",
                                                   label = NULL,
                                                   choices = c("")),
                                shiny::checkboxInput("std_conc_check_fps",
                                                     label = "Is the internal standard concentration known?",
                                                     value = FALSE),
                                shiny::uiOutput("std_conc_box_fps_ui"),
                                shiny::tags$hr(),
                                shiny::h3("5. Adjust fit parameters"),
                                shiny::helpText("Adjust the alignment parameter"),
                                shiny::checkboxInput("align_man_fps", label = "Manual alignment?",
                                                     value = FALSE),
                                shiny::sliderInput("align_fps", label = NULL, min = 0,
                                                   max = 0.5,
                                                   value = c(0.1)),
                                shiny::helpText("Adjust the grid-search shifting parameter"),
                                shiny::sliderInput("shift_fps", label = NULL, min = 0,
                                                   max = 0.1,
                                                   value = c(0)),
                                shiny::helpText(withMathJax("Adjust the 2\\(\\theta\\) range for
                                                         full pattern summation")),
                                shiny::sliderInput("tth_fps_slider", label = NULL,
                                                   min = 2, max = 75,
                                                   value = c(0, 100), step = 0.1),
                                shiny::helpText("Adjust the value below which trace phases
                                               are removed (weight %)"),
                                shiny::sliderInput("remove_trace_fps", label = NULL,
                                                   min = 0, max = 1,
                                                   value = 0, step = 0.01),
                                shiny::helpText("Select a wavelength to use when calculating d-spacings."),
                                shiny::selectInput(inputId = "selectWAVELENGTHfps",
                                                   label = NULL,
                                                   multiple = FALSE,
                                                   choices = list("Cu (1.54056 Angstroms)" = "Cu",
                                                                  "Co (1.78897 Angstroms)" = "Co"))
                              )),
                              shiny::column(9, shiny::wellPanel(
                                shiny::div(style="display: inline-block;vertical-align:bottom; width: 300px;",
                                           h3("6. Full pattern summation")),
                                shiny::div(style="display: inline-block;vertical-align:bottom; width: 300px;",
                                           shiny::actionButton("goButton_fps", "Click to start computation")),
                                shiny::tags$hr(),
                                shiny::h5("Once computation has finished, the results will be
                             tabulated and plotted below. Results can then be exported
                             using download buttons at the bottom of this page."),
                                shiny::tags$hr(),
                                DT::dataTableOutput("contents_fps"),
                                shiny::tags$hr(),
                                plotly::plotlyOutput("line_fps", width = "auto", height = 1000),
                                shiny::tags$hr(),
                                shiny::h3("7. Download computed fit"),
                                shiny::downloadButton(outputId = "download_fit",
                                                      label = "Download fitted patterns (.csv)"),
                                shiny::downloadButton(outputId = "download_mins",
                                                      label = "Download phase concentrations (.csv)"),
                                shiny::downloadButton(outputId = "download_fps",
                                                      label = "Download powdRfps object (.Rdata)")
                              ))
                            ) # end fluidRow
            ),


             #################################
             ## TAB 5:  Automated Full pattern summation
             #################################
            shiny::tabPanel("Automated Full Pattern Summation",
                            shiny::fluidRow(
                              shiny::column(3, shiny::wellPanel(
                                shiny::h3("1. Load a sample for quantification"),
                                shiny::helpText("Must be .xy format (space separated)"),
                                shiny::div(style="display: inline-block;vertical-align:top; width: 225px;",
                                           shiny::fileInput(inputId = "loadXYafps",
                                        label = NULL,
                                        multiple = FALSE,
                                        accept = c(".xy", ".XY"))),
                                shiny::div(style="display: inline-block;vertical-align:center; width: 0px;",
                                           shinyWidgets::dropdownButton(
                                             shiny::downloadLink(outputId = "download_soil_sand_afps",
                                             label = "Sandstone_example.xy  "),
                                             shiny::downloadLink(outputId = "download_soil_lime_afps",
                                             label = "Limestone_example.xy  "),
                                             shiny::downloadLink(outputId = "download_soil_granite_afps",
                                             label = "Granite_example.xy  "),
                                circle = FALSE, status = "danger", icon = shiny::icon("question"),
                                width = "300px", size = "sm"
                              )),
                              shiny::h3("2. Load a reference library"),
                              shiny::helpText("Must be a .Rdata powdRlib object"),
                              shiny::div(style="display: inline-block;vertical-align:top; width: 225px;",
                                         shiny::fileInput(inputId = "loadLIBafps",
                                        label = NULL,
                                        multiple = FALSE,
                                        accept = ".Rdata")),
                              shiny::div(style="display: inline-block;vertical-align:center; width: 0px;",
                                         shinyWidgets::dropdownButton(
                                           shiny::downloadLink(outputId = "download_example_ref_afps",
                                             label = "example_powdRlib.Rdata"),
                                circle = FALSE, status = "danger", icon = shiny::icon("question"),
                                width = "300px", size = "sm"
                              )),
                              shiny::h3("3. Select a solver"),
                              shiny::helpText("Choose the optimisation routine"),
                              shiny::selectInput(inputId = "selectSolver_afps",
                                      label = NULL,
                                      choices = c("BFGS", "Nelder-Mead", "CG", "L-BFGS-B")),
                              shiny::helpText("Choose the objective function to minimise"),
                              shiny::selectInput(inputId = "selectOBJ_afps",
                                      label = NULL,
                                      choices = c("Rwp", "R", "Delta")),
                              shiny::tags$hr(),
                              shiny::h3("4. Select phases"),
                              shiny::helpText("Choose an internal standard for peak alignment/limit of detection estimation."),
                              shiny::selectInput(inputId = "selectINT_afps",
                                      label = NULL,
                                      choices = c("")),
                              shiny::checkboxInput("std_conc_check_afps",
                                        label = "Is the internal standard concentration known?",
                                        value = FALSE),
                              shiny::uiOutput("std_conc_box_afps_ui"),
                              shiny::helpText("Choose which (if any) phases should be treated as amorphous."),
                              shiny::selectInput(inputId = "selectAMORPH_afps",
                                      label = NULL,
                                      choices = c(""),
                                      multiple = TRUE,
                                      selectize = TRUE),
                              shiny::checkboxInput("force_check_afps",
                                        label = "Force certain phases to be retained throughout?",
                                        value = FALSE),
                              shiny::uiOutput("force_afps_ui"),
                              shiny::tags$hr(),
                              shiny::h3("5. Adjust fit parameters"),
                              shiny::helpText("Adjust the alignment parameter"),
                              shiny::checkboxInput("align_man_afps", label = "Manual alignment?",
                                        value = FALSE),
                              shiny::sliderInput("align_afps", label = NULL, min = 0,
                                      max = 0.5,
                                      value = 0.1),
                              shiny::helpText("Adjust the grid-search shifting parameter"),
                              shiny::sliderInput("shift_afps", label = NULL, min = 0,
                                      max = 0.1,
                                      value = 0),
                              shiny::helpText(shiny::withMathJax("Adjust the 2\\(\\theta\\) range for
                                               full pattern summation")),
                              shiny::sliderInput("tth_afps", label = NULL,
                                      min = 2, max = 75,
                                      value = c(0, 100), step = 0.1),
                              shiny::helpText("Estimate the limit of detection (weight %) of the
                                    selected internal standard, from which all other LOD's
                                    are estimated."),
                              shiny::sliderInput("lod_afps", label = NULL,
                                      min = 0, max = 5,
                                      value = 0.1, step = 0.01),
                              shiny::helpText("Remove amorphous phases below this limit (weight %)"),
                              shiny::sliderInput("amorph_lod_afps", label = NULL,
                                      min = 0, max = 100,
                                      value = 2, step = 1),
                              shiny::helpText("Select a wavelength to use when calculating d-spacings."),
                              shiny::selectInput(inputId = "selectWAVELENGTHafps",
                                      label = NULL,
                                      multiple = FALSE,
                                      choices = list("Cu (1.54056 Angstroms)" = "Cu",
                                                     "Co (1.78897 Angstroms)" = "Co"))
                          )),
                          shiny::column(9, shiny::wellPanel(
                            shiny::div(style="display: inline-block;vertical-align:bottom; width: 400px;",
                                       shiny::h3("7. Automated full pattern summation")),
                            shiny::div(style="display: inline-block;vertical-align:bottom; width: 300px;",
                                       shiny::actionButton("goButton_afps", "Click to start computation")),
                            shiny::tags$hr(),
                            shiny::h5("Once computation has finished, the results will be
                             tabulated and plotted below. Results can then be exported
                             using download buttons at the bottom of this page."),
                            shiny::tags$hr(),
                          DT::dataTableOutput("contents_afps"),
                          shiny::tags$hr(),
                          plotly::plotlyOutput("line_afps", width = "auto", height = 1000),
                          tags$hr(),
                          shiny::h3("8. Download computed fit"),
                          shiny::downloadButton(outputId = "download_fit_afps",
                                         label = "Download fitted patterns (.csv)"),
                          shiny::downloadButton(outputId = "download_mins_afps",
                                         label = "Download phase concentrations (.csv)"),
                          shiny::downloadButton(outputId = "download_afps",
                                         label = "Download powdRafps object (.Rdata)")
                        ))
             ) # end fluidRow
             ),


             #################################
             ## TAB 6: RESULTS VIEWER
             #################################

            shiny::tabPanel("Results Viewer",

                            shiny::fluidRow(

                              shiny::column(4, shiny::wellPanel(
                                shiny::helpText("Choose a .Rdata file to load. Must be a
                                    powdRfps or powdRafps object created using the fps() or
                                    afps() functions. These objects can also be saved from the
                                    'Full pattern summation' or 'Automated full pattern summation'
                                    tabs of this application."),
                                shiny::fileInput(inputId = "loadRESULTS",
                                    label = NULL,
                                    multiple = FALSE,
                                    accept = ".Rdata")
                        )),
                        shiny::column(4, shiny::wellPanel(
                          shiny::helpText("Select a wavelength to use when calculating d-spacings."),
                          shiny::selectInput(inputId = "selectWAVELENGTHresults",
                                      label = NULL,
                                      multiple = FALSE,
                                      choices = list("Cu (1.54056 Angstroms)" = "Cu",
                                                     "Co (1.78897 Angstroms)" = "Co"))
                        )),
                        shiny::column(4, shiny::wellPanel(
                          shiny::helpText("Choose how the results are tabulated. If 'Grouped phases' is selected,
                                   the mineralogy is summarised according to the phase_name column, e.g.
                                   if more than one quartz pattern is used, these will be summed together."),
                          shiny::selectInput(inputId = "selectTABLE_viewer",
                                      label = NULL,
                                      choices = c("All phases", "Grouped phases"))
                        ))
                      ), # end fluidRow
                      shiny::fluidRow(
                        shiny::column(12, shiny::wellPanel(
                          DT::dataTableOutput("minerals_viewer_table"),
                          shiny::tags$hr(),
                          plotly::plotlyOutput("results_plot", width = "auto", height = 800)
                        ))
                      )
                      ),

            #################################
            ## TAB 7: RESULTS EDITOR
            #################################

            shiny::tabPanel("Results Editor",
                            shiny::fluidRow(
                              shiny::column(3, shiny::wellPanel(
                                shiny::h3("1. Load results to edit"),
                                shiny::helpText("Must be .Rdata powdRfps or powdRafps object"),
                                shiny::div(style="display: inline-block;vertical-align:top; width: 225px;",
                                           shiny::fileInput(inputId = "loadResults_editor",
                                       label = NULL,
                                       multiple = FALSE,
                                       accept = c(".Rdata"))),
                                shiny::h3("2. Load the reference library"),
                                shiny::helpText("Must be a .Rdata powdRlib object of the library used to
                                  produce the original results"),
                                shiny::div(style="display: inline-block;vertical-align:top; width: 225px;",
                                           shiny::fileInput(inputId = "loadLib_editor",
                                       label = NULL,
                                       multiple = FALSE,
                                       accept = ".Rdata")),
                                shiny::h3("3. Select a solver"),
                                shiny::helpText("Choose the optimisation routine"),
                                shiny::selectInput(inputId = "selectSolver_editor",
                                     label = NULL,
                                     choices = c("BFGS", "Nelder-Mead", "CG", "L-BFGS-B", "NNLS"),
                                     selected = "BFGS"),
                                shiny::helpText("Choose the objective function to minimise"),
                                shiny::selectInput(inputId = "selectOBJ_editor",
                                     label = NULL,
                                     choices = c("Rwp", "R", "Delta")),
                                shiny::tags$hr(),
                                shiny::h3("4. Select phases"),
                                shiny::helpText("Select phases to remove from the original analysis"),
                                shiny:: selectInput(inputId = "selectREMOVE_editor",
                                     label = NULL,
                                     multiple = TRUE,
                                     selectize = TRUE,
                                     choices = c("")),
                                shiny::helpText("Select phases from the library to add to the analysis"),
                                shiny::selectInput(inputId = "selectADD_editor",
                                     label = NULL,
                                     multiple = TRUE,
                                     selectize = TRUE,
                                     choices = c("")),
                                shiny::helpText("Select an internal standard"),
                                shiny::selectInput(inputId = "selectSTD_editor",
                                     label = NULL,
                                     multiple = FALSE,
                                     selectize = TRUE,
                                     choices = c("")),
                                shiny::checkboxInput("std_conc_check_editor",
                                       label = "Is the internal standard concentration known?",
                                       value = FALSE),
                                shiny::uiOutput("std_conc_box_editor_ui"),
                                shiny::tags$hr(),
                                shiny::h3("5. Adjust fit parameters"),
                                shiny::helpText("Adjust the alignment parameter"),
                                shiny::checkboxInput("align_man_editor", label = "Manual alignment?",
                                       value = FALSE),
                                shiny::uiOutput("align_editor_ui"),
                                shiny::helpText("Adjust the grid-search shifting parameter"),
                                shiny::sliderInput("shift_editor", label = NULL, min = 0,
                                     max = 0.1,
                                     value = c(0)),
                                shiny::helpText(shiny::withMathJax("Adjust the 2\\(\\theta\\) range for
                                              full pattern summation")),
                                shiny::sliderInput("tth_editor", label = NULL,
                                     min = 2, max = 75,
                                     value = c(0, 100), step = 0.1),
                                shiny::helpText("Select a wavelength to use when calculating d-spacings."),
                                shiny::selectInput(inputId = "selectWAVELENGTHeditor",
                                     label = NULL,
                                     multiple = FALSE,
                                     choices = list("Cu (1.54056 Angstroms)" = "Cu",
                                                    "Co (1.78897 Angstroms)" = "Co"))
                         )),
                         shiny::column(9, shiny::wellPanel(
                           shiny::h3("6. Full pattern summation"),
                           shiny::selectInput(inputId = "selectPLOTeditor",
                                     label = "Select whether the original results or new
                                     results are plotted",
                                     multiple = FALSE,
                                     choices = list("Original results",
                                                    "New results")),
                           shiny::div(style="display: inline-block;vertical-align:bottom; width: 225px;",
                                      shiny::actionButton("goButton_editor", "Click to Recompute results")),
                           shiny::div(style="display: inline-block;vertical-align:bottom; width: 325px;",
                                      shiny::helpText("Note: Computation may take several minutes.")),
                           shiny::tags$hr(),
                         DT::dataTableOutput("contents_editor"),
                         plotly::plotlyOutput("line_editor", width = "auto", height = 1000),
                         shiny::tags$hr(),
                         shiny::h3("7. Download computed fit"),
                         shiny::downloadButton(outputId = "download_fit_editor",
                                        label = "Download edited fitted patterns (.csv)"),
                         shiny::downloadButton(outputId = "download_mins_editor",
                                        label = "Download edited phase concentrations (.csv)"),
                         shiny::downloadButton(outputId = "download_editor",
                                        label = "Download edited powdRfps object (.Rdata)")
                         ))
                       ) # end fluidRow
                       ),


            #################################
            ## TAB 8: HELP
            #################################

            shiny::tabPanel("Help",
                            shiny::fluidRow(
                              shiny::column(12, align = "center",
                                            shiny::h1("Video tutorials"),
                                            shiny::helpText("Select an option to view a video tutorial for each tab of this
                                  Shiny application"),
                                            shiny::selectInput(inputId = "selectVIDEO",
                                     label = NULL,
                                     multiple = FALSE,
                                     choices = list("Reference Library Builder" = "RGEhe_hdDkM",
                                                 "Reference Library Viewer" = "9hVdSXk2uyw",
                                                 "Reference Library Editor" = "mNQC1TLXJWM",
                                                 "Full Pattern Summation" = "Xr1dHYo9PRg",
                                                 "Automated Full Pattern Summation" = "Sia6rGdpbkI",
                                                 "Results Viewer" = "XTK0r36ilM4",
                                                 "Results Editor" = "R7i8vJgVCvQ")),

                                     shiny::uiOutput("video")
                       )

                     ) # end fluidRow
            )

  ) # end navbarPage
)# end shinyUI
