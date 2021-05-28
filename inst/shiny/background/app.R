library(powdR)

ui <- shiny::navbarPage("powdR",

#################################
## TAB: Background Fitting
#################################
shiny::tabPanel("Background Fitting",
         shiny::fluidRow(
           shiny::column(3, shiny::wellPanel(
             shiny::h3("1. Load a sample"),
             shiny::helpText("File must be .xy format (space separated)"),
             shiny::fileInput(inputId = "loadXYbkg",
                              label = NULL,
                              multiple = FALSE,
                              accept = c(".xy", ".XY")),
             shiny::h3("2. Background parameters"),
             shiny::helpText("lamda: 2nd derivative penalty for primary smoothing (default = 0.5)"),
             shiny::sliderInput("bkg_lambda",
                                label = NULL,
                                min = 0.1, max = 10,
                                value = 0.5, step = 0.1),
             shiny::helpText("hwi: half width of local windows (default = 25)"),
             shiny::sliderInput("bkg_hwi",
                                label = NULL,
                                min = 10, max = 100,
                                value = 25, step = 1),
             shiny::helpText("it: number of iterations in suppression loop (default = 50)"),
             shiny::sliderInput("bkg_it",
                                label = NULL,
                                min = 1, max = 200,
                                value = 50, step = 1),
             shiny::helpText("int: number of buckets to divide the data into (default = 1000)"),
             shiny::sliderInput("bkg_int",
                                label = NULL,
                                min = 10,
                                max = 2000,
                                value = 1000,
                                step = 10)
             )),
           shiny::column(9, shiny::wellPanel(
             shiny::div(style="display: inline-block;vertical-align:top; width: 200px;",
                        shiny::h4("Fitted background plot")),
             shiny::div(style="display: inline-block;vertical-align:top; width: 300px;",
                        shinyWidgets::dropdownButton(
                          shiny::sliderInput("bkg_x",
                                             "adjust the x-axis",
                                             min = 4,
                                             max = 70,
                                             value = c(4,70),
                                             step = 1),
                          shiny::sliderInput("bkg_y",
                                             "adjust the y-axis",
                                             min = 0,
                                             max = 10000,
                                             value = c(0,5000),
                                             step = 1),
                          circle = FALSE,
                          status = "danger",
                          icon = icon("sliders"),
                          width = "400px",
                          size = "sm",
                          tooltip = shinyWidgets::tooltipOptions(title = "Click to adjust graph axes")
                   )),
             shiny::plotOutput("bkg_plot", width = "100%", height = "600px"),
             shiny::tags$hr(),
             shiny::h4("Add a nominal constant to avoid negative counts in background-subtracted data"),
             shiny::numericInput("constant_value", label = NULL, value = 0),
             shiny::h4("Download outputs"),
             shiny::downloadButton(outputId = "download_background",
                                   label = "Download background .xy file"),
             shiny::downloadButton(outputId = "download_background_sub",
                                   label = "Download background subtracted .xy file (with constant added)")
             ))
           ) # end fluidRow
         )

  ) # end navbarPage

server <- function(input, output, session) {

  #################################
  ## TAB: Background fitting
  #################################
  shiny::observe({
    #Load the .xy sample file
    filedata_bkg <- reactive({
      infile_bkg <- input$loadXYbkg
      if (is.null(infile_bkg)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      read.csv(infile_bkg$datapath, sep = " ", header = FALSE)
    })
    #Update the plot sliders
    if(!is.null(filedata_bkg())) {
      shiny::updateSliderInput(session = session, inputId = "bkg_x",
                               min = round(min(filedata_bkg()[1])),
                               max = round(max(filedata_bkg()[1])),
                               value = c(round(min(filedata_bkg()[1])),
                                         round(max(filedata_bkg()[1]))))

      shiny::updateSliderInput(session = session, inputId = "bkg_y",
                               min = round(min(filedata_bkg()[2])),
                               max = round(max(filedata_bkg()[2])),
                               value = c(round(min(filedata_bkg()[2])),
                                         round(max(filedata_bkg()[2]))))
    }

    #Fit background to data

    bkg_fit <- shiny::reactive({

      if (is.null(filedata_bkg())) {
        # User has not uploaded a file yet
        return(NULL)
      }

      bkg_out <- powdR::bkg(xrd = filedata_bkg(),
                            lambda = input$bkg_lambda,
                            hwi = input$bkg_hwi,
                            it = input$bkg_it,
                            int = input$bkg_int)


    })


    output$bkg_plot <- shiny::renderPlot({
      if (is.null(filedata_bkg())) {
        # User has not uploaded a file yet
        return(NULL)
      }

      #bkg_plot <- plot(bkg_fit())
      suppressWarnings(
        plot(bkg_fit()) +
          ggplot2::scale_x_continuous(limits = c(input$bkg_x[1], input$bkg_x[2])) +
          ggplot2::scale_y_continuous(limits = c(input$bkg_y[1], input$bkg_y[2]))
      )

      #ggplot(data.frame(bkg_fit()[c(1:3)])) +
      #geom_line(aes(x = tth,
      #              y = background))

      #bkg_plot +
      #scale_x_continuous(limits = c(input$bkg_x[1], input$bkg_x[2])) +
      #scale_y_continuous(limits = c(input$bkg_y[1], input$bkg_y[2]))


    })


    output$download_background <- shiny::downloadHandler(

      filename = function() {
        paste("background_", Sys.Date(), ".xy", sep="")
      },
      content = function(file) {
        write.table(data.frame(bkg_fit()[c(1,3)]),
                    file,
                    sep = " ",
                    col.names = FALSE,
                    row.names = FALSE)
      }
    )

    output$download_background_sub <- shiny::downloadHandler(

      filename = function() {
        paste("background_sub_", Sys.Date(), ".xy", sep="")
      },
      content = function(file) {
        write.table(data.frame(x = bkg_fit()$tth,
                               y = (bkg_fit()$counts - bkg_fit()$background) + input$constant_value),
                    file,
                    sep = " ",
                    col.names = FALSE,
                    row.names = FALSE)
      }
    )


  })




  session$onSessionEnded(function() {
    shiny::stopApp()
  })


} ## end  shinyServer

shiny::shinyApp(ui, server)
