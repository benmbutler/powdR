shinyUI(
  navbarPage("powdR",

             #################################
             ## TAB 1: REFERENCE LIBRARY BUILDER
             #################################
             tabPanel("Reference Library Builder",
                      fluidRow(
                        column(4, wellPanel(
                          h3("1. File upload"),
                          h4("XRPD data"),
                          helpText("Choose a .csv file containing the 2theta scale
                                    and the count intensities of all reference patterns."),
                          div(style="display: inline-block;vertical-align:top; width: 300px;",
                              fileInput(inputId = "uploadXRD",
                                    label = NULL,
                                    multiple = FALSE,
                                    accept = ".csv")),
                          div(style="display: inline-block;vertical-align:center; width: 0px;",
                              dropdownButton(
                                downloadLink(outputId = "download_xrd_eg",
                                             label = "Download an example .csv file"),
                                circle = FALSE, status = "danger", icon = icon("question"),
                                width = "300px", size = "sm"
                              )),
                          h4("Phase information"),
                          helpText("Upload a .csv file containing
                                    contains the id's, names and reference intensity ratios of
                                    all reference patterns."),
                          div(style="display: inline-block;vertical-align:top; width: 300px;",
                              fileInput(inputId = "uploadPHASE",
                                    label = NULL,
                                    multiple = FALSE,
                                    accept = ".csv")),
                          div(style="display: inline-block;vertical-align:center; width: 0px;",
                              dropdownButton(
                                downloadLink(outputId = "download_phases_eg",
                                             label = "Download an example .csv file"),
                                circle = FALSE, status = "danger", icon = icon("question"),
                                width = "300px", size = "sm"
                              )),
                          tags$hr(),
                          div(style="display: inline-block;vertical-align:bottom; width: 100px;",
                              h3("2. Build")),
                          div(style="display: inline-block;vertical-align:bottom; width: 100px;",
                              actionButton("BuildLibButton", "Click to build library")),
                          tags$hr(),
                          h3("3. Download library"),
                          helpText("Provide a name for the new library object.
                                    This is what the library will be called if it is
                                   subsequently loaded into R (can be kept as the default
                                   'RefLib'):"),
                          textInput("name", label = NULL,
                                    "RefLib"),
                          downloadButton(outputId = "download_lib",
                                         label = "Download library as .Rdata binary file")
                        )),
                        column(8, wellPanel(
                          h3("Phases in your reference library"),
                          tags$hr(),
                          dataTableOutput("minerals_table")
                        ))
                      ) # end fluidRow
             ),

             #################################
             ## TAB 2: REFERENCE LIBRARY PLOTTER
             #################################
             tabPanel("Reference Library Viewer",
                      fluidRow(
                        column(6, wellPanel(
                          helpText("Choose a .Rdata reference library to load. Must be
                                    a powdRlib object created using either the powdRlib function,
                                    or via the Reference Library Builder in this application."),
                          fileInput(inputId = "loadLIB_plotter",
                                    label = NULL,
                                    multiple = FALSE,
                                    accept = ".Rdata")
                        )),
                        column(6, wellPanel(
                          helpText("Choose phases from the library to plot."),
                          selectInput(inputId = "selectPHASES_plotter",
                                      label = NULL,
                                      choices = c("Please upload a reference library"),
                                      selected = "Please upload a reference library",
                                      multiple = TRUE,
                                      selectize = TRUE)
                        ))
                      ), # end fluidRow
                      fluidRow(
                        column(12, wellPanel(
                          plotlyOutput("lib_plot", width = "auto", height = 600)
                        ))
                      )
             ),

             #################################
             ## TAB 3: Background Fitting
             #################################
             tabPanel("Background fitting",
                      fluidRow(
                        column(3, wellPanel(
                          h3("1. Load a sample"),
                          helpText("File must be .xy format (space separated)"),
                          fileInput(inputId = "loadXYbkg",
                                    label = NULL,
                                    multiple = FALSE,
                                    accept = c(".xy", ".XY")),
                          h3("2. Background parameters"),
                          helpText("lamda: 2nd derivative penalty for primary smoothing (default = 0.5)"),
                          sliderInput("bkg_lambda",
                                      label = NULL,
                                      min = 0.1, max = 10,
                                      value = 0.5, step = 0.1),
                          helpText("hwi: half width of local windows (default = 25)"),
                          sliderInput("bkg_hwi",
                                      label = NULL,
                                      min = 10, max = 100,
                                      value = 25, step = 1),
                          helpText("it: number of iterations in suppression loop (default = 50)"),
                          sliderInput("bkg_it",
                                      label = NULL,
                                      min = 1, max = 200,
                                      value = 50, step = 1),
                          helpText("int: number of buckets to divide the data into (default = 1000)"),
                          sliderInput("bkg_int",
                                      label = NULL,
                                      min = 10, max = 2000,
                                      value = 1000, step = 10)
                          )),
                        column(9, wellPanel(
                          div(style="display: inline-block;vertical-align:top; width: 200px;",
                              h4("Fitted background plot")),
                          div(style="display: inline-block;vertical-align:top; width: 300px;",
                              dropdownButton(
                                sliderInput("bkg_x", "adjust the x-axis",
                                            min = 4, max = 70,
                                            value = c(4,70), step = 1),
                                sliderInput("bkg_y", "adjust the y-axis",
                                            min = 0, max = 10000,
                                            value = c(0,5000), step = 1),
                                circle = FALSE, status = "danger", icon = icon("sliders"),
                                width = "400px", size = "sm",
                                tooltip = tooltipOptions(title = "Click to adjust graph axes")
                              )),
                          plotOutput("bkg_plot", width = "100%", height = "600px")
                          ))
                        ) # end fluidRow
             ),

             #################################
             ## TAB 4: Full pattern summation
             #################################
             tabPanel("Full pattern summation",
                      fluidRow(
                        column(3, wellPanel(
                          h3("1. Load a sample for quantification"),
                          helpText("Must be .xy format (space separated)"),
                          div(style="display: inline-block;vertical-align:top; width: 225px;",
                          fileInput(inputId = "loadXY",
                                    label = NULL,
                                    multiple = FALSE,
                                    accept = c(".xy", ".XY"))),
                          div(style="display: inline-block;vertical-align:center; width: 0px;",
                              dropdownButton(
                                downloadLink(outputId = "download_soil_sand",
                                             label = "Sandstone_example.xy  "),
                                downloadLink(outputId = "download_soil_lime",
                                             label = "Limestone_example.xy  "),
                                downloadLink(outputId = "download_soil_granite",
                                             label = "Granite_example.xy  "),
                                circle = FALSE, status = "danger", icon = icon("question"),
                                width = "300px", size = "sm"
                              )),
                          h3("2. Load a reference library"),
                          helpText("Must be a .Rdata powdRlib object"),
                          div(style="display: inline-block;vertical-align:top; width: 225px;",
                              fileInput(inputId = "loadLIB",
                                    label = NULL,
                                    multiple = FALSE,
                                    accept = ".Rdata")),
                          div(style="display: inline-block;vertical-align:center; width: 0px;",
                              dropdownButton(
                            downloadLink(outputId = "download_example_ref",
                                         label = "example_powdRlib.Rdata"),
                            circle = FALSE, status = "danger", icon = icon("question"),
                            width = "300px", size = "sm"
                          )),
                          h3("3. Select a solver"),
                          helpText("Choose the optimisation routine"),
                          selectInput(inputId = "selectSolver",
                                      label = NULL,
                                      choices = c("BFGS", "Nelder-Mead", "CG", "NNLS")),
                          uiOutput("selectOBJui"),
                          tags$hr(),
                          h3("4. Select phases"),
                          uiOutput("selectPHASESui"),
                          helpText("Choose an internal standard for peak alignment."),
                          selectInput(inputId = "selectINT",
                                      label = NULL,
                                      choices = c("")),
                          tags$hr(),
                          h3("5. Adjust fit parameters"),
                          helpText("Adjust the alignment parameter"),
                          sliderInput("align", label = NULL, min = 0.01,
                                      max = 0.5,
                                      value = c(0.1)),
                          helpText(withMathJax("Adjust the 2\\(\\theta\\) range for
                                                         full pattern summation")),
                          sliderInput("tth", label = NULL,
                                      min = 2, max = 75,
                                      value = c(0, 100), step = 0.1)
                        )),
                        column(9, wellPanel(
                          div(style="display: inline-block;vertical-align:bottom; width: 300px;",
                              h3("6. Full pattern summation")),
                          div(style="display: inline-block;vertical-align:bottom; width: 300px;",
                              actionButton("goButton", "Click to start computation")),
                          tags$hr(),
                          h5("Once computation has finished, the results will be
                             tabulated and plotted below. Results can then be exported
                             using download buttons at the bottom of this page."),
                          tags$hr(),
                          dataTableOutput("contents"),
                          tags$hr(),
                          plotlyOutput("line", width = "auto", height = 1000),
                          tags$hr(),
                          h3("7. Download computed fit"),
                          downloadButton(outputId = "download_fit",
                                         label = "Download fitted patterns (.csv)"),
                          downloadButton(outputId = "download_mins",
                                         label = "Download phase concentrations (.csv)"),
                          downloadButton(outputId = "download_fps",
                                         label = "Download powdRfps object (.Rdata)")
                          ))
                        ) # end fluidRow
                        ),


             #################################
             ## TAB 5:  Automated Full pattern summation
             #################################
             tabPanel("Automated full pattern summation",
                      fluidRow(
                        column(3, wellPanel(
                          h3("1. Load a sample for quantification"),
                          helpText("Must be .xy format (space separated)"),
                          div(style="display: inline-block;vertical-align:top; width: 225px;",
                              fileInput(inputId = "loadXYafps",
                                        label = NULL,
                                        multiple = FALSE,
                                        accept = c(".xy", ".XY"))),
                          div(style="display: inline-block;vertical-align:center; width: 0px;",
                              dropdownButton(
                                downloadLink(outputId = "download_soil_sand_afps",
                                             label = "Sandstone_example.xy  "),
                                downloadLink(outputId = "download_soil_lime_afps",
                                             label = "Limestone_example.xy  "),
                                downloadLink(outputId = "download_soil_granite_afps",
                                             label = "Granite_example.xy  "),
                                circle = FALSE, status = "danger", icon = icon("question"),
                                width = "300px", size = "sm"
                              )),
                          h3("2. Load a reference library"),
                          helpText("Must be a .Rdata powdRlib object"),
                          div(style="display: inline-block;vertical-align:top; width: 225px;",
                              fileInput(inputId = "loadLIBafps",
                                        label = NULL,
                                        multiple = FALSE,
                                        accept = ".Rdata")),
                          div(style="display: inline-block;vertical-align:center; width: 0px;",
                              dropdownButton(
                                downloadLink(outputId = "download_example_ref_afps",
                                             label = "example_powdRlib.Rdata"),
                                circle = FALSE, status = "danger", icon = icon("question"),
                                width = "300px", size = "sm"
                              )),
                          h3("3. Select a solver"),
                          helpText("Choose the optimisation routine"),
                          selectInput(inputId = "selectSolver_afps",
                                      label = NULL,
                                      choices = c("BFGS", "Nelder-Mead", "CG")),
                          selectInput(inputId = "selectOBJ_afps",
                                      label = NULL,
                                      choices = c("Rwp", "R", "Delta")),
                          tags$hr(),
                          h3("4. Select phases"),
                          helpText("Choose an internal standard for peak alignment."),
                          selectInput(inputId = "selectINT_afps",
                                      label = NULL,
                                      choices = c("")),
                          helpText("Choose which (if any) phases should be treated as amorphous."),
                          selectInput(inputId = "selectAMORPH_afps",
                                      label = NULL,
                                      choices = c(""),
                                      multiple = TRUE,
                                      selectize = TRUE),
                          tags$hr(),
                          h3("5. Adjust fit parameters"),
                          helpText("Adjust the alignment parameter"),
                          sliderInput("align_afps", label = NULL, min = 0.01,
                                      max = 0.5,
                                      value = 0.1),
                          helpText("Adjust the shifting parameter"),
                          sliderInput("shift_afps", label = NULL, min = 0,
                                      max = 0.1,
                                      value = 0),
                          helpText(withMathJax("Adjust the 2\\(\\theta\\) range for
                                               full pattern summation")),
                          sliderInput("tth_afps", label = NULL,
                                      min = 2, max = 75,
                                      value = c(0, 100), step = 0.1),
                          helpText("Tune the limit of detection parameter for crystalline phases (lower values
                                   = lower limits of detection)"),
                          sliderInput("lod_afps", label = NULL,
                                      min = 0.01, max = 5,
                                      value = 0.3, step = 0.1),
                          helpText("Remove amorphous phases below this limit (percent)"),
                          sliderInput("amorph_lod_afps", label = NULL,
                                      min = 0, max = 100,
                                      value = 2, step = 1),
                          div(style="display: inline-block;vertical-align:center; width: 300px;",
                              h3("6. Background parameters")),
                          div(style="display: inline-block;vertical-align:center; width: 0px;",
                              dropdownButton(
                                helpText("Tune the background parameters"),
                                sliderInput("lambda", label = "lambda",
                                            min = 0.1, max = 10,
                                            value = 0.5, step = 0.1),
                                sliderInput("hwi", label = "hwi",
                                            min = 10, max = 100,
                                            value = 25, step = 1),
                                sliderInput("it", label = "it",
                                            min = 2, max = 75,
                                            value = 50, step = 1),
                                sliderInput("int", label = "int",
                                            min = 10, max = 2000,
                                            value = 1000, step = 10),
                                circle = FALSE, status = "danger", icon = icon("sliders"),
                                width = "300px", size = "sm"
                              )),
                          helpText("Use the dropdown box to tune the background parameters. The
                                   default setting are usually adequate.")
                          )),
                        column(9, wellPanel(
                          div(style="display: inline-block;vertical-align:bottom; width: 400px;",
                              h3("7. Automated full pattern summation")),
                          div(style="display: inline-block;vertical-align:bottom; width: 300px;",
                              actionButton("goButton_afps", "Click to start computation")),
                          tags$hr(),
                          h5("Once computation has finished, the results will be
                             tabulated and plotted below. Results can then be exported
                             using download buttons at the bottom of this page."),
                          tags$hr(),
                          dataTableOutput("contents_afps"),
                          tags$hr(),
                          plotlyOutput("line_afps", width = "auto", height = 1000),
                          tags$hr(),
                          h3("8. Download computed fit"),
                          downloadButton(outputId = "download_fit_afps",
                                         label = "Download fitted patterns (.csv)"),
                          downloadButton(outputId = "download_mins_afps",
                                         label = "Download phase concentrations (.csv)"),
                          downloadButton(outputId = "download_afps",
                                         label = "Download powdRafps object (.Rdata)")
                        ))
             ) # end fluidRow
             ),


             #################################
             ## TAB 6: RESULTS VIEWER
             #################################

             tabPanel("Results Viewer",

                      fluidRow(

                        column(6, wellPanel(
                          helpText("Choose a .Rdata file to load. Must be a
                                    powdRfps or powdRafps object created using the fps() or
                                    afps() function. These objects can also be saved from the
                                    'Full pattern summation' or 'Automated full pattern summation'
                                    tabs of this application."),
                          fileInput(inputId = "loadRESULTS",
                                    label = NULL,
                                    multiple = FALSE,
                                    accept = ".Rdata")
                        )),
                        column(6, wellPanel(
                          helpText("Choose how the results are tabulated. If 'Grouped phases' is selected,
                                   the mineralogy is summarised according to the phase_name column, e.g.
                                   if more than one quartz pattern is used, these will be summed together."),
                          selectInput(inputId = "selectTABLE_viewer",
                                      label = NULL,
                                      choices = c("All phases", "Grouped phases"))
                        ))
                      ), # end fluidRow
                      fluidRow(
                        column(12, wellPanel(
                          dataTableOutput("minerals_viewer_table"),
                          tags$hr(),
                          plotlyOutput("results_plot", width = "auto", height = 800)
                        ))
                      )
                      )


  ) # end navbarPage
)# end shinyUI
