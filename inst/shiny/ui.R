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
                          fileInput(inputId = "uploadXRD",
                                    label = "Choose a .csv file containing the 2theta scale
                                    and the count intensities of all reference patterns.",
                                    multiple = FALSE,
                                    accept = ".csv"),
                          h4("Phase information"),
                          fileInput(inputId = "uploadPHASE",
                                    label = "Upload a .csv file containing
                                    contains the id's, names and reference intensity ratios of
                                    all reference patterns.",
                                    multiple = FALSE,
                                    accept = ".csv"),
                          tags$hr(),
                          h3("2. Build"),
                          actionButton("BuildLibButton", "Click to build library"),
                          tags$hr(),
                          h3("3. Download library"),
                          textInput("name","Provide a name for the new library object.
                                    This is what the library will be called if it is
                                    subsequently loaded into R (can be kept as the default
                                    'RefLib'):",
                                    "RefLib"),
                          downloadButton(outputId = "download_lib",
                                         label = "Download library as .Rdata binary file"),
                          tags$hr(),
                          h3("Example files"),
                          downloadLink(outputId = "download_xrd_eg",
                                       label = "Download an example .csv file of the XRPD data"),
                          tags$br(),
                          downloadLink(outputId = "download_phases_eg",
                                       label = "Download an example .csv file of the
                                       phase information")
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
                          fileInput(inputId = "loadLIB_plotter",
                                    label = "Choose a .Rdata reference library to load. Must be
                                    a powdRlib object created using either the powdRlib function,
                                    or via the Reference Library Builder in this application.",
                                    multiple = FALSE,
                                    accept = ".Rdata")
                        )),
                        column(6, wellPanel(
                          selectInput(inputId = "selectPHASES_plotter",
                                      label = "Choose phases from the library to plot.",
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
             ## TAB 3: Full pattern summation
             #################################
             tabPanel("Full pattern summation",
                      fluidRow(
                        column(3, wellPanel(
                          h3("1. Load a sample for quantification"),
                          fileInput(inputId = "loadXY",
                                    label = "File must be .xy format (space separated)",
                                    multiple = FALSE,
                                    accept = c(".xy", ".XY")),
                          dropdownButton(
                            downloadLink(outputId = "download_soil_sand",
                                         label = "Sandstone_soil.xy  "),
                            downloadLink(outputId = "download_soil_lime",
                                         label = "Limestone_soil.xy  "),
                            downloadLink(outputId = "download_soil_granite",
                                         label = "Granite_soil.xy  "),
                            circle = FALSE, status = "danger", icon = icon("question"),
                            width = "300px", size = "xs",
                            tooltip = tooltipOptions(title = "Click for example .xy data")
                          ),
                          tags$hr(),
                          h3("2. Load a reference library"),
                          fileInput(inputId = "loadLIB",
                                    label = "Choose a .Rdata reference library to load",
                                    multiple = FALSE,
                                    accept = ".Rdata"),
                          dropdownButton(
                            downloadLink(outputId = "download_example_ref",
                                         label = "example_library.Rdata"),
                            circle = FALSE, status = "danger", icon = icon("question"),
                            width = "300px", size = "xs",
                            tooltip = tooltipOptions(title = "Click for example library")
                          ),
                          tags$hr(),
                          h3("3. Select a solver"),
                          selectInput(inputId = "selectSolver",
                                      label = "Choose the optimisation routine",
                                      choices = c("BFGS", "Nelder-Mead", "CG", "nnls")),
                          uiOutput("selectOBJui"),
                          h3("4. Select phases"),
                          uiOutput("selectPHASESui"),
                          selectInput(inputId = "selectINT",
                                      label = "Choose an internal standard for peak alignment.",
                                      choices = c("")),
                          tags$hr(),
                          h3("5. Adjust fit parameters"),
                          tags$hr(),
                          sliderInput("tth", withMathJax("Adjust the 2\\(\\theta\\) range for
                                                         full pattern summation"),
                                      min = 2, max = 75,
                                      value = c(0, 100), step = 0.1),
                          sliderInput("align", "Adjust the alignment parameter", min = 0.01,
                                      max = 0.5,
                                      value = c(0.1))
                        )),
                        column(9, wellPanel(
                          h3("6. Full pattern summation"),
                          actionButton("goButton", "Click to start computation"),
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
                                         label = "Download in powdRfps format (.Rdata)")
                          ))
                        ) # end fluidRow
                        ),

             #################################
             ## TAB 4: RESULTS VIEWER
             #################################

             tabPanel("Results Viewer",

                      fluidRow(

                        column(6, wellPanel(
                          fileInput(inputId = "loadRESULTS",
                                    label = "Choose a .Rdata results object to load. Must be a
                                    powdRfps object created using either the fps function, or
                                    exported from the full pattern summation tab of this
                                    application.",
                                    multiple = FALSE,
                                    accept = ".Rdata")
                        )),
                        column(6, wellPanel(
                          selectInput(inputId = "selectTABLE_viewer",
                                      label = "Choose how the results are tabulated",
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
