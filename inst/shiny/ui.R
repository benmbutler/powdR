shiny::shinyUI(
  shiny::navbarPage("powdR",

#################################
# TAB 1: REFERENCE LIBRARY BUILDER
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
                    shiny::helpText("Upload a .csv file containing the id's, names and reference
                                    intensity ratios of all reference patterns."),
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
                    shiny::textInput("name", label = NULL, "RefLib"),
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
                    shiny::textInput("name_editor", label = NULL, "RefLib"),
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

######################################
# TAB 4: Full pattern summation
#####################################

shiny::tabPanel("Full Pattern Summation",
                shiny::fluidRow(
                  shiny::column(3, shiny::wellPanel(
                    shiny::h3("1. Load samples for quantification"),
                    shiny::helpText("Must be .xy format (space separated)"),
                    shiny::div(style="display: inline-block;vertical-align:top; width: 225px;",
                               shiny::fileInput(inputId = "loadXY",
                                                label = NULL,
                                                multiple = FALSE,
                                                accept = c(".xy", ".XY"))),
                    shiny::div(style="display: inline-block;vertical-align:center; width: 0px;",
                               shinyWidgets::dropdownButton(
                                 shiny::downloadLink(outputId = "download_sandstone",
                                                     label = "sandstone_soil.xy  "),
                                 shiny::downloadLink(outputId = "download_limestone",
                                                     label = "limestone_soil.xy  "),
                                 shiny::downloadLink(outputId = "download_granite",
                                                     label = "granite_soil.xy  "),
                                 circle = FALSE, status = "danger", icon = shiny::icon("question"),
                                 width = "300px", size = "sm")),
                    shiny::h3("2. Load a reference library"),
                    shiny::helpText("Must be a .Rdata powdRlib object"),
                    shiny::div(style="display: inline-block;vertical-align:top; width: 225px;",
                               shiny::fileInput(inputId = "loadLIB",
                                                label = NULL,
                                                multiple = FALSE,
                                                accept = ".Rdata")),
                    shiny::div(style="display: inline-block;vertical-align:center; width: 0px;",
                               shinyWidgets::dropdownButton(
                                 shiny::downloadLink(outputId = "download_rockjock",
                                                     label = "rockjock_powdRlib.Rdata"),
                                 shiny::downloadLink(outputId = "download_mineral_library",
                                                     label = "example_powdRlib.Rdata"),
                                 circle = FALSE, status = "danger", icon = shiny::icon("question"),
                                 width = "300px", size = "sm")),
                    shiny::h3("3. Select mode"),
                    shiny::selectInput(inputId = "selectMode_fps",
                                       label = NULL,
                                       choices = c("Manual", "Automated"),
                                       selected = "Manual"),
                    shiny::h3("4. Select a solver"),
                    shiny::helpText("Select the optimisation routine"),
                    shiny::selectInput(inputId = "selectSolver_fps",
                                       label = NULL,
                                       choices = c("BFGS", "Nelder-Mead", "CG", "NNLS"),
                                       selected = "BFGS"),
                    shiny::helpText("Select the objective function to minimise"),
                    shiny::selectInput(inputId = "selectOBJ_fps",
                                       label = NULL,
                                       choices = c("Rwp", "R", "Delta"),
                                       selected = "Rwp"),
                    shiny::tags$hr(),
                    shiny::h3("5. Select phases"),
                    shiny::helpText("Select the phases to use in the fitting."),
                    shiny::selectInput(inputId = "selectPHASES_fps",
                                       choices = c("Upload a powdRlib object in section 2"),
                                       label = NULL,
                                       multiple = TRUE,
                                       selectize = TRUE,
                                       selected = "Upload a powdRlib object in section 2"),
                    shiny::helpText("Choose an internal standard that may be used for peak
                                    alignment or estimation of limits of detection."),
                    shiny::selectInput(inputId = "selectINT_fps",
                                       label = NULL,
                                       choices = c("")),
                    shiny::checkboxInput("std_conc_check_fps",
                                         label = "Is the internal standard concentration known?",
                                         value = FALSE),
                    shiny::uiOutput("std_conc_help_ui"),
                    shiny::uiOutput("std_conc_box_fps_ui"),
                    shiny::uiOutput("amorph_help_ui"),
                    shiny::uiOutput("selectAMORPH_ui"),
                    shiny::uiOutput("force_help_ui"),
                    shiny::uiOutput("selectFORCE_ui"),
                    shiny::tags$hr(),
                    shiny::h3("6. Adjust fit parameters"),
                    shiny::helpText("Adjust the alignment parameter"),
                    shiny::checkboxInput("align_man_fps", label = "Manual alignment?",
                                         value = FALSE),
                    shiny::sliderInput("align_fps", label = NULL, min = 0,
                                       max = 0.5, value = c(0.1)),
                    shiny::helpText("Adjust the shifting parameter"),
                    shiny::sliderInput("shift_fps", label = NULL, min = 0,
                                       max = 0.5, value = c(0)),
                    shiny::helpText(withMathJax("Adjust the 2\\(\\theta\\) range
                                                                   for full pattern summation")),
                    shiny::sliderInput("tth_fps_slide", label = NULL,
                                       min = 2, max = 75,
                                       value = c(0, 100), step = 0.1),
                    uiOutput("slide_help_ui"),
                    shiny::sliderInput("lod_slide", label = NULL,
                                       min = 0, max = 3,
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
                               h3("7. Full pattern summation")),
                    shiny::div(style="display: inline-block;vertical-align:bottom; width: 300px;",
                               shiny::actionButton("goButton_fps", "Click to start computation")),
                    shiny::tags$hr(),
                    shiny::h5("Once computation has finished, the results will be tabulated and
                              plotted below. Results can then be exported using download buttons at the
                              bottom of this page."),
                    shiny::tags$hr(),
                    DT::dataTableOutput("contents_fps"),
                    shiny::tags$hr(),
                    plotly::plotlyOutput("line_fps", width = "auto", height = 550),
                    shiny::tags$hr(),
                    shiny::h3("7. Download computed fit"),
                    shiny::downloadButton(outputId = "download_meas",
                                          label = "Download measured pattern (.xy)"),
                    shiny::downloadButton(outputId = "download_calc",
                                          label = "Download fitted pattern (.xy)"),
                    shiny::downloadButton(outputId = "download_concs",
                                          label = "Download concentrations (.csv)"),
                    shiny::downloadButton(outputId = "download_fps",
                                          label = "Download powdRfps object (.Rdata)")
                  ))
                ) # end fluidRow
),


#################################
## TAB 5: RESULTS VIEWER EDITOR
#################################

shiny::tabPanel("Results Viewer/Editor",
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
                               shiny::fileInput(inputId = "loadLIB_fps_editor",
                                                label = NULL,
                                                multiple = FALSE,
                                                accept = ".Rdata")),
                    shiny::h3("3. Select a solver"),
                    shiny::helpText("Choose the optimisation routine"),
                    shiny::selectInput(inputId = "selectSolver_editor",
                                       label = NULL,
                                       choices = c("BFGS", "Nelder-Mead", "CG", "NNLS"),
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
                    shiny::helpText("Adjust the shifting parameter"),
                    shiny::sliderInput("shift_editor", label = NULL, min = 0,
                                       max = 0.5,
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
                                       choices = list("Original results", "New results")),
                    shiny::div(style="display: inline-block;vertical-align:bottom; width: 225px;",
                               shiny::actionButton("goButton_editor", "Click to Recompute results")),
                    shiny::div(style="display: inline-block;vertical-align:bottom; width: 325px;",
                               shiny::helpText("Note: Computation may take several minutes.")),
                    shiny::tags$hr(),
                    DT::dataTableOutput("contents_editor"),
                    plotly::plotlyOutput("line_editor", width = "auto", height = 550),
                    shiny::tags$hr(),
                    shiny::h3("8. Download computed fit"),
                    shiny::downloadButton(outputId = "download_meas_editor",
                                          label = "Download edited measured pattern (.xy)"),
                    shiny::downloadButton(outputId = "download_calc_editor",
                                          label = "Download edited fitted pattern (.xy)"),
                    shiny::downloadButton(outputId = "download_mins_editor",
                                          label = "Download edited phase concentrations (.csv)"),
                    shiny::downloadButton(outputId = "download_editor",
                                          label = "Download edited powdRfps object (.Rdata)")
                    ))
                  ) # end fluidRow
                ),


#################################
## TAB 6: HELP
#################################

shiny::tabPanel("Help",
                shiny::fluidRow(
                  shiny::column(12, align = "center",
                                shiny::h1("Video tutorial"),
                                shiny::uiOutput("video")
                                )
                  ) # end fluidRow
                )
) # end navbarPage
)# end shinyUI
