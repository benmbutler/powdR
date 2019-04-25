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
                          helpText(withMathJax("Choose a .csv file containing the 2\\(\\theta\\) scale
                                    and the count intensities of all reference patterns.")),
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
                                    the id's, names and reference intensity ratios of
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
                          helpText("Provide a name for the new powdRlib object.
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
                        column(4, wellPanel(
                          helpText("Choose a .Rdata reference library to load. Must be
                                    a powdRlib object created using either the powdRlib function,
                                    or via the Reference Library Builder in this application."),
                          fileInput(inputId = "loadLIB_plotter",
                                    label = NULL,
                                    multiple = FALSE,
                                    accept = ".Rdata")
                        )),
                        column(4, wellPanel(
                          helpText("Select a wavelength to use when calculating d-spacings."),
                          selectInput(inputId = "selectWAVELENGTH",
                                      label = NULL,
                                      multiple = FALSE,
                                      choices = list("Cu (1.54056 Angstroms)" = "Cu",
                                                     "Co (1.78897 Angstroms)" = "Co"))
                        )),
                        column(4, wellPanel(
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
                          plotlyOutput("lib_plot", width = "auto", height = 600),
                          tags$hr(),
                          dataTableOutput("lib_table")
                        ))
                      )
             ),

             #################################
             ## TAB 3: REFERENCE LIBRARY EDITOR
             #################################
             tabPanel("Reference Library Editor",
                      fluidRow(
                        column(4, wellPanel(
                          h3("1. Load a library"),
                          helpText("Choose a .Rdata reference library to load. Must be
                                    a powdRlib object created using either the powdRlib function,
                                    or via the Reference Library Builder in this application."),
                          div(style="display: inline-block;vertical-align:top; width: 300px;",
                              fileInput(inputId = "loadLIB_editor",
                                        label = NULL,
                                        multiple = FALSE,
                                        accept = ".Rdata")),
                          tags$hr(),
                          h3("2. Select the subset mode"),
                          selectInput(inputId = "selectMODE_editor",
                                      label = NULL,
                                      choices = c("keep", "remove"),
                                      multiple = FALSE),
                          tags$hr(),
                          h3("3. Select reference patterns to subset"),
                          selectInput(inputId = "selectPHASES_editor",
                                      label = NULL,
                                      choices = c("Please upload a reference library"),
                                      selected = "Please upload a reference library",
                                      multiple = TRUE,
                                      selectize = TRUE),
                          tags$hr(),
                          div(style="display: inline-block;vertical-align:bottom; width: 120px;",
                              h3("4. Subset")),
                          div(style="display: inline-block;vertical-align:bottom; width: 100px;",
                              actionButton("SubsetLibButton", "Click to subset library")),
                          tags$hr(),
                          h3("5. Download library"),
                          helpText("Provide a name for the new powdRlib object.
                                    This is what the library will be called if it is
                                   subsequently loaded into R (can be kept as the default
                                   'RefLib'):"),
                          textInput("name_editor", label = NULL,
                                    "RefLib"),
                          downloadButton(outputId = "download_subset_lib",
                                         label = "Download library as .Rdata binary file")

                        )),
                        column(8, wellPanel(
                          h3("Phases in your subset reference library"),
                          tags$hr(),
                          dataTableOutput("minerals_subset_table")
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
             tabPanel("Full Pattern Summation",
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
                                downloadLink(outputId = "download_rj_mix1",
                                             label = "rockjock_mix1.xy  "),
                                downloadLink(outputId = "download_rj_mix2",
                                             label = "rockjock_mix2.xy  "),
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
                            downloadLink(outputId = "download_rj_lib",
                                         label = "rockjock_powdRlib.Rdata"),
                            circle = FALSE, status = "danger", icon = icon("question"),
                            width = "300px", size = "sm"
                          )),
                          h3("3. Select a solver"),
                          helpText("Choose the optimisation routine"),
                          selectInput(inputId = "selectSolver",
                                      label = NULL,
                                      choices = c("BFGS", "Nelder-Mead", "CG", "L-BFGS-B", "NNLS"),
                                      selected = "BFGS"),
                          uiOutput("selectOBJui"),
                          tags$hr(),
                          h3("4. Select phases"),
                          uiOutput("selectPHASESui"),
                          helpText("Choose an internal standard for peak alignment. If the manual alignment
                                   box below is ticked, then this internal standard is not used and instead
                                   the sample is aligned by the amount selected in the alignment slider."),
                          selectInput(inputId = "selectINT",
                                      label = NULL,
                                      choices = c("")),
                          checkboxInput("std_conc_check_fps",
                                        label = "Is the internal standard concentration known?",
                                        value = FALSE),
                          uiOutput("std_conc_box_fps_ui"),
                          tags$hr(),
                          h3("5. Adjust fit parameters"),
                          helpText("Adjust the alignment parameter"),
                          checkboxInput("align_man_fps", label = "Manual alignment?",
                                        value = FALSE),
                          sliderInput("align_fps", label = NULL, min = 0,
                                      max = 0.5,
                                      value = c(0.1)),
                          helpText("Adjust the grid-search shifting parameter"),
                          sliderInput("shift_fps", label = NULL, min = 0,
                                      max = 0.1,
                                      value = c(0)),
                          helpText(withMathJax("Adjust the 2\\(\\theta\\) range for
                                                         full pattern summation")),
                          sliderInput("tth", label = NULL,
                                      min = 2, max = 75,
                                      value = c(0, 100), step = 0.1),
                          helpText("Adjust the value below which trace phases
                                               are removed (weight %)"),
                          sliderInput("remove_trace", label = NULL,
                                      min = 0, max = 1,
                                      value = 0, step = 0.01),
                          helpText("Select a wavelength to use when calculating d-spacings."),
                          selectInput(inputId = "selectWAVELENGTHfps",
                                      label = NULL,
                                      multiple = FALSE,
                                      choices = list("Cu (1.54056 Angstroms)" = "Cu",
                                                     "Co (1.78897 Angstroms)" = "Co"))
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
             tabPanel("Automated Full Pattern Summation",
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
                                      choices = c("BFGS", "Nelder-Mead", "CG", "L-BFGS-B")),
                          helpText("Choose the objective function to minimise"),
                          selectInput(inputId = "selectOBJ_afps",
                                      label = NULL,
                                      choices = c("Rwp", "R", "Delta")),
                          tags$hr(),
                          h3("4. Select phases"),
                          helpText("Choose an internal standard for peak alignment/limit of detection estimation."),
                          selectInput(inputId = "selectINT_afps",
                                      label = NULL,
                                      choices = c("")),
                          checkboxInput("std_conc_check_afps",
                                        label = "Is the internal standard concentration known?",
                                        value = FALSE),
                          uiOutput("std_conc_box_afps_ui"),
                          helpText("Choose which (if any) phases should be treated as amorphous."),
                          selectInput(inputId = "selectAMORPH_afps",
                                      label = NULL,
                                      choices = c(""),
                                      multiple = TRUE,
                                      selectize = TRUE),
                          checkboxInput("force_check_afps",
                                        label = "Force certain phases to be retained throughout?",
                                        value = FALSE),
                          uiOutput("force_afps_ui"),
                          tags$hr(),
                          h3("5. Adjust fit parameters"),
                          helpText("Adjust the alignment parameter"),
                          checkboxInput("align_man_afps", label = "Manual alignment?",
                                        value = FALSE),
                          sliderInput("align_afps", label = NULL, min = 0,
                                      max = 0.5,
                                      value = 0.1),
                          helpText("Adjust the grid-search shifting parameter"),
                          sliderInput("shift_afps", label = NULL, min = 0,
                                      max = 0.1,
                                      value = 0),
                          helpText(withMathJax("Adjust the 2\\(\\theta\\) range for
                                               full pattern summation")),
                          sliderInput("tth_afps", label = NULL,
                                      min = 2, max = 75,
                                      value = c(0, 100), step = 0.1),
                          helpText("Estimate the limit of detection (weight %) of the
                                    selected internal standard, from which all other LOD's
                                    are estimated."),
                          sliderInput("lod_afps", label = NULL,
                                      min = 0, max = 5,
                                      value = 0.1, step = 0.01),
                          helpText("Remove amorphous phases below this limit (weight %)"),
                          sliderInput("amorph_lod_afps", label = NULL,
                                      min = 0, max = 100,
                                      value = 2, step = 1),
                          helpText("Select a wavelength to use when calculating d-spacings."),
                          selectInput(inputId = "selectWAVELENGTHafps",
                                      label = NULL,
                                      multiple = FALSE,
                                      choices = list("Cu (1.54056 Angstroms)" = "Cu",
                                                     "Co (1.78897 Angstroms)" = "Co"))
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

                        column(4, wellPanel(
                          helpText("Choose a .Rdata file to load. Must be a
                                    powdRfps or powdRafps object created using the fps() or
                                    afps() functions. These objects can also be saved from the
                                    'Full pattern summation' or 'Automated full pattern summation'
                                    tabs of this application."),
                          fileInput(inputId = "loadRESULTS",
                                    label = NULL,
                                    multiple = FALSE,
                                    accept = ".Rdata")
                        )),
                        column(4, wellPanel(
                          helpText("Select a wavelength to use when calculating d-spacings."),
                          selectInput(inputId = "selectWAVELENGTHresults",
                                      label = NULL,
                                      multiple = FALSE,
                                      choices = list("Cu (1.54056 Angstroms)" = "Cu",
                                                     "Co (1.78897 Angstroms)" = "Co"))
                        )),
                        column(4, wellPanel(
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
                      ),

            #################################
            ## TAB 7: RESULTS EDITOR
            #################################

            tabPanel("Results Editor",
                     fluidRow(
                       column(3, wellPanel(
                         h3("1. Load results to edit"),
                         helpText("Must be .Rdata powdRfps or powdRafps object"),
                         div(style="display: inline-block;vertical-align:top; width: 225px;",
                             fileInput(inputId = "loadResults_editor",
                                       label = NULL,
                                       multiple = FALSE,
                                       accept = c(".Rdata"))),
                         h3("2. Load the reference library"),
                         helpText("Must be a .Rdata powdRlib object of the library used to
                                  produce the original results"),
                         div(style="display: inline-block;vertical-align:top; width: 225px;",
                             fileInput(inputId = "loadLib_editor",
                                       label = NULL,
                                       multiple = FALSE,
                                       accept = ".Rdata")),
                         h3("3. Select a solver"),
                         helpText("Choose the optimisation routine"),
                         selectInput(inputId = "selectSolver_editor",
                                     label = NULL,
                                     choices = c("BFGS", "Nelder-Mead", "CG", "L-BFGS-B", "NNLS"),
                                     selected = "BFGS"),
                         helpText("Choose the objective function to minimise"),
                         selectInput(inputId = "selectOBJ_editor",
                                     label = NULL,
                                     choices = c("Rwp", "R", "Delta")),
                         tags$hr(),
                         h3("4. Select phases"),
                         helpText("Select phases to remove from the original analysis"),
                         selectInput(inputId = "selectREMOVE_editor",
                                     label = NULL,
                                     multiple = TRUE,
                                     selectize = TRUE,
                                     choices = c("")),
                         helpText("Select phases from the library to add to the analysis"),
                         selectInput(inputId = "selectADD_editor",
                                     label = NULL,
                                     multiple = TRUE,
                                     selectize = TRUE,
                                     choices = c("")),
                         helpText("Select an internal standard"),
                         selectInput(inputId = "selectSTD_editor",
                                     label = NULL,
                                     multiple = FALSE,
                                     selectize = TRUE,
                                     choices = c("")),
                         checkboxInput("std_conc_check_editor",
                                       label = "Is the internal standard concentration known?",
                                       value = FALSE),
                         uiOutput("std_conc_box_editor_ui"),
                         tags$hr(),
                         h3("5. Adjust fit parameters"),
                         helpText("Adjust the alignment parameter"),
                         checkboxInput("align_man_editor", label = "Manual alignment?",
                                       value = FALSE),
                         uiOutput("align_editor_ui"),
                         helpText("Adjust the grid-search shifting parameter"),
                         sliderInput("shift_editor", label = NULL, min = 0,
                                     max = 0.1,
                                     value = c(0)),
                         helpText(withMathJax("Adjust the 2\\(\\theta\\) range for
                                              full pattern summation")),
                         sliderInput("tth_editor", label = NULL,
                                     min = 2, max = 75,
                                     value = c(0, 100), step = 0.1),
                         helpText("Select a wavelength to use when calculating d-spacings."),
                         selectInput(inputId = "selectWAVELENGTHeditor",
                                     label = NULL,
                                     multiple = FALSE,
                                     choices = list("Cu (1.54056 Angstroms)" = "Cu",
                                                    "Co (1.78897 Angstroms)" = "Co"))
                         )),
                       column(9, wellPanel(
                         h3("6. Full pattern summation"),
                         selectInput(inputId = "selectPLOTeditor",
                                     label = "Select whether the original results or new
                                     results are plotted",
                                     multiple = FALSE,
                                     choices = list("Original results",
                                                    "New results")),
                         div(style="display: inline-block;vertical-align:bottom; width: 225px;",
                             actionButton("goButton_editor", "Click to Recompute results")),
                         div(style="display: inline-block;vertical-align:bottom; width: 325px;",
                             helpText("Note: Computation may take several minutes.")),
                         tags$hr(),
                         dataTableOutput("contents_editor"),
                         plotlyOutput("line_editor", width = "auto", height = 1000),
                         tags$hr(),
                         h3("7. Download computed fit"),
                         downloadButton(outputId = "download_fit_editor",
                                        label = "Download edited fitted patterns (.csv)"),
                         downloadButton(outputId = "download_mins_editor",
                                        label = "Download edited phase concentrations (.csv)"),
                         downloadButton(outputId = "download_editor",
                                        label = "Download edited powdRfps object (.Rdata)")
                         ))
                       ) # end fluidRow
                       ),


            #################################
            ## TAB 8: HELP
            #################################

            tabPanel("Help",
                     fluidRow(
                       column(12, align = "center",
                         h1("Video tutorials"),
                         helpText("Select an option to view a video tutorial for each tab of this
                                  Shiny application"),
                         selectInput(inputId = "selectVIDEO",
                                     label = NULL,
                                     multiple = FALSE,
                                     choices = list("Reference Library Builder" = "RGEhe_hdDkM",
                                                 "Reference Library Viewer" = "9hVdSXk2uyw",
                                                 "Reference Library Editor" = "mNQC1TLXJWM",
                                                 "Full Pattern Summation" = "Xr1dHYo9PRg",
                                                 "Automated Full Pattern Summation" = "Sia6rGdpbkI",
                                                 "Results Viewer" = "XTK0r36ilM4",
                                                 "Results Editor" = "R7i8vJgVCvQ")),

                         uiOutput("video")
                       )

                     ) # end fluidRow
            )

  ) # end navbarPage
)# end shinyUI
