## Server.R

shinyServer(function(input, output, session) {

  #################################
  ## TAB 1: Reference library builder
  #################################

  observe({
  #Load the xrd.csv file
  xrddata <- reactive({
    infile1 <- input$uploadXRD
    if (is.null(infile1)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile1$datapath, header = TRUE, stringsAsFactors = FALSE)
  })

  #Load the phase.csv file
  phasedata <- reactive({
    infile2 <- input$uploadPHASE
    if (is.null(infile2)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile2$datapath, header = TRUE, stringsAsFactors = FALSE)
  })

  #Download the library
  output$download_lib <- downloadHandler(

    filename = "xrd.Rdata",
    content = function(con) {
      assign(input$name, Dataset())

      save(list=input$name, file=con)
    }

  )

  #Create a powdRlib object
  Dataset <- eventReactive(input$BuildLibButton, {

    Dataset <- powdR::powdRlib(xrd_table = xrddata(),
                                   phases_table = phasedata())

  })

  #Tabulate the minerals in the library
    output$minerals_table <- shiny::renderDataTable({

     Dataset()[[3]]

    }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10))

  })


  #Downloads of example data
  output$download_xrd_eg <- downloadHandler(

    filename = function() {
      paste("xrd_example_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(minerals_xrd, file, sep = ",", col.names = TRUE, row.names = FALSE)
    }
  )

  output$download_phases_eg <- downloadHandler(

    filename = function() {
      paste("phase_info_example_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(minerals_phases, file, sep = ",", col.names = TRUE, row.names = FALSE)
    }
  )

  #################################
  ## TAB 2: Reference library plotter
  #################################

  observe({

  #Load library
  lib_plotter_load <- reactive({
    infile_lib_plotter <- input$loadLIB_plotter
    if (is.null(infile_lib_plotter)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    bar <- load(infile_lib_plotter$datapath)
    lpl <- get(bar)
    return(lpl)
  })

  #Make sure the class of the uploaded data is correct and
  #if it is, update the selectInput
  if(class(lib_plotter_load()) == "powdRlib") {
  updateSelectInput(session, "selectPHASES_plotter",
                    label = paste("Choose phases from the library to plot."),
                    choices = lib_plotter_load()[[3]][[1]],
                    selected = head(lib_plotter_load()[[3]][[1]], 1))
  }

  #Plot phases in the library
  output$lib_plot <- plotly::renderPlotly({

  if(class(lib_plotter_load()) == "powdRlib") {
    plot(lib_plotter_load(), patterns = input$selectPHASES_plotter, interactive = TRUE)
  } else {
   return(NULL)
  }

  })

  })


  #################################
  ## TAB 3: Background fitting
  #################################
  observe({
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
    updateSliderInput(session = session, inputId = "bkg_x",
                      min = min(filedata_bkg()[1]),
                      max = max(filedata_bkg()[1]),
                      value = c(min(filedata_bkg()[1]),
                                max(filedata_bkg()[1])))

    updateSliderInput(session = session, inputId = "bkg_y",
                      min = min(filedata_bkg()[2]),
                      max = max(filedata_bkg()[2]),
                      value = c(min(filedata_bkg()[2]),
                                max(filedata_bkg()[2])))
  }

  #Fit background to data

  output$bkg_plot <- renderPlot({
  if (is.null(filedata_bkg())) {
      # User has not uploaded a file yet
      return(NULL)
  }
  bkg_out <- bkg(xrd = filedata_bkg(),
                 lambda = input$bkg_lambda,
                 hwi = input$bkg_hwi,
                 it = input$bkg_it,
                 int = input$bkg_int)

  bkg_plot <- plot(bkg_out)

  bkg_plot +
  scale_x_continuous(limits = c(input$bkg_x[1], input$bkg_x[2])) +
  scale_y_continuous(limits = c(input$bkg_y[1], input$bkg_y[2]))


  })
  })

  #################################
  ## TAB 4: Full pattern fitting
  #################################

  #Downloads of example sandstone data
  output$download_soil_sand <- downloadHandler(

    filename = function() {
      paste("sandstone_example_", Sys.Date(), ".xy", sep="")
    },
    content = function(file) {
      write.table(soils$sandstone, file, sep = " ", col.names = FALSE, row.names = FALSE)
    }
  )

  #Downloads of example limestone data
  output$download_soil_lime <- downloadHandler(

    filename = function() {
      paste("limestone_example_", Sys.Date(), ".xy", sep="")
    },
    content = function(file) {
      write.table(soils$limestone, file, sep = " ", col.names = FALSE, row.names = FALSE)
    }
  )

  #Downloads of example granite data
  output$download_soil_granite <- downloadHandler(

    filename = function() {
      paste("granite_example_", Sys.Date(), ".xy", sep="")
    },
    content = function(file) {
      write.table(soils$granite, file, sep = " ", col.names = FALSE, row.names = FALSE)
    }
  )

  #Download an example reference library
  output$download_example_ref <- downloadHandler(

    filename = "example_library.Rdata",
    content = function(con) {
      assign("example_library", minerals)

      save(list="example_library", file=con)
    }
  )

  output$selectOBJui <- renderUI({
    if (input$selectSolver == "nnls") return(NULL)

    selectInput(inputId = "selectOBJ",
                label = "Choose the objective function to minimise",
                choices = c("Rwp", "R", "Delta"))
  })

  #Update the selectINPUT boxes in the full pattern fitting tab
  observe({

      x2 <- filedata3()
      x2 <- x2[[3]]

      output$selectPHASESui <- renderUI({
        if (input$selectSolver == "nnls") return(NULL)

        selectInput(inputId = "selectPHASES",
                    label = "Select the phases to use in the fitting.",
                    choices = paste0(x2[[2]], ": ", x2[[1]]),
                    selected = head(paste0(x2[[2]], ": ", x2[[1]]), 1),
                    multiple = TRUE,
                    selectize = TRUE)
      })


  })

  selectPHASESupdate <- reactive({

    x2b <- filedata3()
    x2b <- x2b[[3]]

    if(input$selectSolver == "nnls") {
      return(paste0(x2b[[2]], ": ", x2b[[1]]))
    } else {
    input$selectPHASES
    }

  })

  #Update the internal standard selectInput so that only phases selected for fitting
  #are available
  observe({

    scu <- as.character(selectPHASESupdate())

    updateSelectInput(session, "selectINT",
                      label = paste("Choose an internal standard for peak alignment."),
                      choices = scu,
                      selected = head(scu, 1))

  })


  #Load the .xy sample file
  filedata2 <- reactive({
    infile2 <- input$loadXY
    if (is.null(infile2)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile2$datapath, sep = " ", header = FALSE)
  })

  #If a library has been uploaded, then create a reactive library
  filedata3 <- reactive({
    infile3 <- input$loadLIB
    if (is.null(infile3)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    bar <- load(infile3$datapath)
    fd3 <- get(bar)
    return(fd3)
  })

  #Use the selected library to adjust the 2theta slider
  observe({

    if(!is.null(input$loadLIB)) {
      xrd_uploaded <- as.list(filedata3())
      updateSliderInput(session = session, inputId = "tth",
                        min = round(min(as.numeric(xrd_uploaded[[2]])) + input$align, 2),
                        max = round(max(as.numeric(xrd_uploaded[[2]])) - input$align, 2))
    }

  })

  #FULL PATTERN FITTING

  observe({

    fps_reactive <- eventReactive(input$goButton, {

      smpl <- filedata2()

        xrdlib2 <- as.list(filedata3())

        if (input$selectSolver %in% c("Nelder-Mead", "BFGS", "CG")) {

        fps_out <- powdR::fps(smpl = smpl, lib = xrdlib2, tth_fps = input$tth,
                                  std = gsub(".*: ", "", input$selectINT),
                                  refs = gsub(".*: ", "", input$selectPHASES),
                                  align = input$align,
                                  obj = input$selectOBJ,
                                  solver = input$selectSolver)
        } else {

        fps_out <- powdR::fps(smpl = smpl, lib = xrdlib2, tth_fps = input$tth,
                              std = gsub(".*: ", "", input$selectINT),
                              refs = xrdlib2$phases$phase_id,
                              align = input$align,
                              obj = input$selectOBJ,
                              solver = input$selectSolver)

        }

    })


    fps_out <- fps_reactive()

    output$contents <- renderDataTable({

      fps_table <- data.frame(fps_out["phases_summary"])
      names(fps_table) <- c("PHASE_NAME", "PHASE_PERCENT")
      fps_table$PHASE_PERCENT <- round(fps_table$PHASE_PERCENT, 2)

      fps_table

    }, options = list(lengthMenu = c(5, 10, 15), pageLength = 10))



    output$line <- plotly::renderPlotly({

      plot(fps_out, interactive = TRUE)

    })


    #Export the mineral concentrations to a .csv file
    minout <- fps_out
    minout <- data.frame(minout[[5]])
    names(minout) <- c("PHASE_ID", "PHASE_NAME", "PERCENT")

    output$download_mins <- downloadHandler(

      filename = function() {
        paste("minerals-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.table(minout, file, sep = ",", col.names = TRUE, row.names = FALSE)
      }
    )

    #Export the weighted patterns
    fitout <- fps_out
    fitout <- data.frame("TTH" = fitout[[1]],
                         "MEASURED" = fitout[[3]],
                         "FITTED" = fitout[[2]],
                         fitout[[8]])

    output$download_fit <- downloadHandler(

      filename = function() {
        paste("fit-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.table(fitout, file, sep = ",", col.names = TRUE, row.names = FALSE)
      }
    )

    #Download the whole fps output as .Rdata format
    output$download_fps <- downloadHandler(

      filename = "fps.Rdata",
      content = function(con) {
        assign("fps_output", fps_reactive())

        save(list="fps_output", file=con)
      }
    )

  })


  #################################
  ## TAB 5: Results plotter
  #################################

  observe({

    results_plotter_load <- reactive({
      infile_results_plotter <- input$loadRESULTS
      if (is.null(infile_results_plotter)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      bar <- load(infile_results_plotter$datapath)
      rpl <- get(bar)
      return(rpl)
    })

    output$minerals_viewer_table <- shiny::renderDataTable({

      if (!class(results_plotter_load()) == "powdRfps") {

        return(NULL)

      }

      if(input$selectTABLE_viewer == "All phases") {

        table_to_view <- results_plotter_load()[[5]]
        table_to_view$phase_percent <- round(table_to_view$phase_percent, 2)
        table_to_view

      } else {

        table_to_view <- results_plotter_load()[[6]]
        table_to_view$phase_percent <- round(table_to_view$phase_percent, 2)
        table_to_view
      }

    }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10))

    output$results_plot <- plotly::renderPlotly({

      if(class(results_plotter_load()) == "powdRfps") {
        plot(results_plotter_load(), interactive = TRUE)
      } else {
        return(NULL)
      }

    })

  })


  session$onSessionEnded(function() {
    stopApp()
  })

}) ## end  shinyServer
