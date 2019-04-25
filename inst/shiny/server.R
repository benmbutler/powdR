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


  #Create a powdRlib object
  Dataset <- eventReactive(input$BuildLibButton, {

    Dataset <- powdR::powdRlib(xrd_table = xrddata(),
                                   phases_table = phasedata())

  })


  #Download the library
  output$download_lib <- downloadHandler(

    filename = "my_powdRlib.Rdata",
    content = function(con) {
      assign(input$name, Dataset())

      save(list=input$name, file=con)
    }

  )

  #Tabulate the minerals in the library
    output$minerals_table <- shiny::renderDataTable({

     Dataset()[[3]]

    }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10))

  })


  observe({

  minerals_xrd <- powdR::minerals_xrd

  #Downloads of example data
  output$download_xrd_eg <- downloadHandler(

    filename = function() {
      paste("xrd_example_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(minerals_xrd, file, sep = ",", col.names = TRUE, row.names = FALSE)
    }
  )

  })


  observe({

  minerals_phases <- powdR::minerals_phases

  output$download_phases_eg <- downloadHandler(

    filename = function() {
      paste("phase_info_example_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(minerals_phases, file, sep = ",", col.names = TRUE, row.names = FALSE)
    }
  )

  })

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
                    choices = paste0(lib_plotter_load()[[3]][[2]], ": ", lib_plotter_load()[[3]][[1]]),
                    selected = head(paste0(lib_plotter_load()[[3]][[2]], ": ", lib_plotter_load()[[3]][[1]]), 1))
  }

  #Tabulate the minerals in the library
  output$lib_table <- shiny::renderDataTable({

    lib_plotter_load()[[3]]

  }, options = list(lengthMenu = c(5, 10, 25, 50), pageLength = 5))

  #Plot phases in the library
  output$lib_plot <- plotly::renderPlotly({

  #Subset the library based on the selection
  lib_sub <- lib_plotter_load()

  if (length(input$selectPHASES_plotter > 0)) {
  lib_sub$xrd <- lib_sub$xrd[which(names(lib_sub$xrd) %in% gsub(".*: ", "", input$selectPHASES_plotter))]
  lib_sub$phases <- lib_sub$phases[which(lib_sub$phases$phase_id %in% gsub(".*: ", "", input$selectPHASES_plotter)),]
  }


  if(class(lib_sub) == "powdRlib" & length(input$selectPHASES_plotter > 0)) {
    plot(lib_sub, wavelength = input$selectWAVELENGTH, interactive = TRUE)
  } else {
   return(NULL)
  }

  })

  })

  #################################
  ## TAB 2: Reference library Editor
  #################################

  observe({

    #Load library
    lib_editor_load <- reactive({
      infile_lib_editor <- input$loadLIB_editor
      if (is.null(infile_lib_editor)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      bar <- load(infile_lib_editor$datapath)
      lpl <- get(bar)
      return(lpl)
    })

    #Make sure the class of the uploaded data is correct and
    #if it is, update the selectInput
    if(class(lib_editor_load()) == "powdRlib") {
      updateSelectInput(session, "selectPHASES_editor",
                        label = paste("Select reference patterns to subset"),
                        choices = paste0(lib_editor_load()[[3]][[2]], ": ", lib_editor_load()[[3]][[1]]),
                        selected = head(paste0(lib_editor_load()[[3]][[2]], ": ", lib_editor_load()[[3]][[1]]), 1))
    }


    #Create a powdRlib object
    subset_lib <- eventReactive(input$SubsetLibButton, {

      subset_lib <- subset(lib_editor_load(),
                           refs = gsub(".*: ", "", input$selectPHASES_editor),
                           mode = input$selectMODE_editor)

    })


    #Download the library
    output$download_subset_lib <- downloadHandler(

      filename = "my_powdRlib.Rdata",
      content = function(con) {
        assign(input$name_editor, subset_lib())

        save(list=input$name_editor, file=con)
      }

    )

    #Tabulate the minerals in the library
    output$minerals_subset_table <- shiny::renderDataTable({

      subset_lib()[[3]]

    }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10))

  })


  #################################
  ## TAB 3: Background fitting
  #################################
  #observe({
  ##Load the .xy sample file
  #filedata_bkg <- reactive({
  #  infile_bkg <- input$loadXYbkg
  #  if (is.null(infile_bkg)) {
  #    # User has not uploaded a file yet
  #    return(NULL)
  #  }
  #  read.csv(infile_bkg$datapath, sep = " ", header = FALSE)
  #})

  #Update the plot sliders
  #if(!is.null(filedata_bkg())) {
  #  updateSliderInput(session = session, inputId = "bkg_x",
  #                    min = min(filedata_bkg()[1]),
  #                    max = max(filedata_bkg()[1]),
  #                    value = c(min(filedata_bkg()[1]),
  #                              max(filedata_bkg()[1])))

  # updateSliderInput(session = session, inputId = "bkg_y",
  #                    min = min(filedata_bkg()[2]),
  #                    max = max(filedata_bkg()[2]),
  #                    value = c(min(filedata_bkg()[2]),
  #                              max(filedata_bkg()[2])))
  #}

  #Fit background to data

  #output$bkg_plot <- renderPlot({
  #if (is.null(filedata_bkg())) {
      # User has not uploaded a file yet
  #    return(NULL)
  #}
  #bkg_out <- bkg(xrd = filedata_bkg(),
  #               lambda = input$bkg_lambda,
  #               hwi = input$bkg_hwi,
  #               it = input$bkg_it,
  #               int = input$bkg_int)

  #bkg_plot <- plot(bkg_out)

  #bkg_plot +
  #scale_x_continuous(limits = c(input$bkg_x[1], input$bkg_x[2])) +
  #scale_y_continuous(limits = c(input$bkg_y[1], input$bkg_y[2]))


  #})
  #})

  #################################
  ## TAB 4: Full pattern fitting
  #################################

  #Downloads of example sandstone data

  observe({

  soils <- powdR::soils

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

  output$download_rj_mix1 <- downloadHandler(

    filename = function() {
      paste("rockjock_mix1_", Sys.Date(), ".xy", sep="")
    },
    content = function(file) {
      write.table(rockjock_mixtures$Mix1, file, sep = " ", col.names = FALSE, row.names = FALSE)
    }
  )

  output$download_rj_mix2 <- downloadHandler(

    filename = function() {
      paste("rockjock_mix1_", Sys.Date(), ".xy", sep="")
    },
    content = function(file) {
      write.table(rockjock_mixtures$Mix2, file, sep = " ", col.names = FALSE, row.names = FALSE)
    }
  )

  })

  #Download an example reference library

  observe({

  minerals <- powdR::minerals

  output$download_example_ref <- downloadHandler(

    filename = "example_powdRlib.Rdata",
    content = function(con) {
      assign("example_powdRlib", minerals)

      save(list="example_powdRlib", file=con)
    }
  )

  #rockjock <- powdR::rockjock

  output$download_rj_lib <- downloadHandler(

    filename = "rockjock_powdRlib.Rdata",
    content = function(con) {
      assign("rockjock", rockjock)

      save(list="rockjock", file=con)
    }
  )

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

  output$selectOBJui <- renderUI({
    if (input$selectSolver == "NNLS") return(NULL)

    selectInput(inputId = "selectOBJ",
                label = "Choose the objective function to minimise",
                choices = c("Rwp", "R", "Delta"))
  })

  #Update the selectINPUT boxes in the full pattern fitting tab
  observe({

      x2 <- filedata3()
      x2 <- x2[[3]]

      output$selectPHASESui <- renderUI({
        if (input$selectSolver == "NNLS") return(NULL)

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

    if(input$selectSolver == "NNLS") {
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
                      label = paste("Choose an internal standard for peak alignment. If the manual alignment
                                   box below is ticked, then this internal standard is not used and instead
                                   the sample is aligned by the amount selected in the alignment slider."),
                      choices = scu,
                      selected = head(scu, 1))

  })

  observe({

    output$std_conc_box_fps_ui <- renderUI({
      if (input$std_conc_check_fps == FALSE) return(NULL)

      numericInput("std_conc_box_fps", label = "Define the internal standard concentration",
                  min = 0.01,
                  max = 99.99,
                  value = 20,
                  step = 0.01)

    })


  })


  #Use the selected library to adjust the 2theta slider
  observe({

    if(!is.null(input$loadLIB)) {
      xrd_uploaded <- as.list(filedata3())
      updateSliderInput(session = session, inputId = "tth",
                        min = round(min(as.numeric(xrd_uploaded[[2]])) + abs(input$align_fps), 2),
                        max = round(max(as.numeric(xrd_uploaded[[2]])) - abs(input$align_fps), 2),
                        value = c(round(min(as.numeric(xrd_uploaded[[2]])) + abs(input$align_fps), 2),
                                  round(max(as.numeric(xrd_uploaded[[2]])) - abs(input$align_fps), 2)))
    }

  })

  #Use the tickbox to adjust the alignment slider
  observe({

    if(input$align_man_fps == TRUE) {

      updateSliderInput(session = session, inputId = "align_fps",
                        min = -0.5,
                        max = 0.5,
                        value = 0)

    } else {

      updateSliderInput(session = session, inputId = "align_fps",
                        min = 0,
                        max = 0.5,
                        value = 0.1)

    }

  })

  #FULL PATTERN FITTING

  observe({

    fps_reactive <- eventReactive(input$goButton, {

      if(input$std_conc_check_fps == FALSE) {

        std_conc_fps <- NA

      } else {

        std_conc_fps <- input$std_conc_box_fps

      }

      smpl <- filedata2()

        xrdlib2 <- as.list(filedata3())

        if (input$selectSolver %in% c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B")) {

        fps_out <- powdR::fps(smpl = smpl, lib = xrdlib2, tth_fps = input$tth,
                                  std = gsub(".*: ", "", input$selectINT),
                                  std_conc = std_conc_fps,
                                  refs = gsub(".*: ", "", input$selectPHASES),
                                  align = input$align_fps,
                                  manual_align = input$align_man_fps,
                                  shift = input$shift_fps,
                                  obj = input$selectOBJ,
                                  solver = input$selectSolver,
                                  remove_trace = input$remove_trace)
        } else {

        fps_out <- powdR::fps(smpl = smpl, lib = xrdlib2, tth_fps = input$tth,
                              std = gsub(".*: ", "", input$selectINT),
                              std_conc = std_conc_fps,
                              refs = xrdlib2$phases$phase_id,
                              align = input$align_fps,
                              manual_align = input$align_man_fps,
                              shift = input$shift_fps,
                              obj = "Rwp",
                              solver = input$selectSolver,
                              remove_trace = input$remove_trace)

        }

    })


    fps_out <- fps_reactive()

    output$contents <- renderDataTable({

      fps_table <- data.frame(fps_out$phases)

      fps_table$phase_percent <- round(fps_table$phase_percent, 2)

      fps_table

    }, options = list(lengthMenu = c(5, 10, 15), pageLength = 10))



    output$line <- plotly::renderPlotly({

      plot(fps_out, wavelength = input$selectWAVELENGTHfps, interactive = TRUE)

    })


    #Export the mineral concentrations to a .csv file
    minout <- fps_out
    minout <- data.frame(minout$phases)

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

      filename = "fps_output.Rdata",
      content = function(con) {
        assign("fps_output", fps_reactive())

        save(list="fps_output", file=con)
      }
    )

  })


  #################################
  ## TAB 5: Automated full pattern fitting
  #################################

  observe({

  soils <- powdR::soils

  #Downloads of example sandstone data
  output$download_soil_sand_afps <- downloadHandler(

    filename = function() {
      paste("sandstone_example_", Sys.Date(), ".xy", sep="")
    },
    content = function(file) {
      write.table(soils$sandstone, file, sep = " ", col.names = FALSE, row.names = FALSE)
    }
  )

  #Downloads of example limestone data
  output$download_soil_lime_afps <- downloadHandler(

    filename = function() {
      paste("limestone_example_", Sys.Date(), ".xy", sep="")
    },
    content = function(file) {
      write.table(soils$limestone, file, sep = " ", col.names = FALSE, row.names = FALSE)
    }
  )

  #Downloads of example granite data
  output$download_soil_granite_afps <- downloadHandler(

    filename = function() {
      paste("granite_example_", Sys.Date(), ".xy", sep="")
    },
    content = function(file) {
      write.table(soils$granite, file, sep = " ", col.names = FALSE, row.names = FALSE)
    }
  )

  })

  #Download an example reference library

  observe({

  minerals <- powdR::minerals

  output$download_example_ref_afps <- downloadHandler(

    filename = "example_powdRlib.Rdata",
    content = function(con) {
      assign("example_powdRlib", minerals)

      save(list="example_powdRlib", file=con)
    }
  )

  })


  #Load the .xy sample file
  filedata2_afps <- reactive({
    infile2_afps <- input$loadXYafps
    if (is.null(infile2_afps)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile2_afps$datapath, sep = " ", header = FALSE)
  })

  #If a library has been uploaded, then create a reactive library

  filedata3_afps <- reactive({
    infile3_afps <- input$loadLIBafps
    if (is.null(infile3_afps)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    bar <- load(infile3_afps$datapath)
    fd3_afps <- get(bar)
    return(fd3_afps)
  })





  #update the selection of internal standard
  observe({

    x2_afps <- filedata3_afps()

    x2_afps <- x2_afps[[3]]

    updateSelectInput(session, "selectINT_afps",
                      label = paste("Choose an internal standard for peak alignment/limit of detection estimation."),
                      choices = paste0(x2_afps[[2]], ": ", x2_afps[[1]]),
                      selected = head(paste0(x2_afps[[2]], ": ", x2_afps[[1]]), 1)
                      )

    updateSelectInput(session, "selectAMORPH_afps",
                      label = paste("Choose which phases should be treated as amorphous."),
                      choices = paste0(x2_afps[[2]], ": ", x2_afps[[1]])
                      )

  })

  #Insert a numeric input box if the internal standard conc is known
  observe({

    output$std_conc_box_afps_ui <- renderUI({
      if (input$std_conc_check_afps == FALSE) return(NULL)

      numericInput("std_conc_box_afps", label = "Define the internal standard concentration",
                   min = 0.01,
                   max = 99.99,
                   value = 20,
                   step = 0.01)

    })


  })

  #Create a select input for force if the box is ticked
  observe({

    f_afps <- filedata3_afps()

    f_afps <- f_afps[[3]]

    output$force_afps_ui <- renderUI({
      if (input$force_check_afps == FALSE) return(NULL)

      selectInput(inputId = "force_afps",
                  label = "Select the phases to be retained.",
                  choices = paste0(f_afps[[2]], ": ", f_afps[[1]]),
                  selected = NULL,
                  multiple = TRUE,
                  selectize = TRUE)
    })


  })


  #Use the selected library to adjust the 2theta sliders
  observe({

    if(!is.null(input$loadLIBafps)) {
      xrd_uploaded_afps <- as.list(filedata3_afps())
      updateSliderInput(session = session, inputId = "tth_afps",
                        min = round(min(as.numeric(xrd_uploaded_afps[[2]])) + abs(input$align_afps), 2),
                        max = round(max(as.numeric(xrd_uploaded_afps[[2]])) - abs(input$align_afps), 2),
                        value = c(round(min(as.numeric(xrd_uploaded_afps[[2]])) + abs(input$align_afps), 2),
                                  round(max(as.numeric(xrd_uploaded_afps[[2]])) - abs(input$align_afps), 2)))
    }

  })


  #FULL PATTERN FITTING

  observe({

    afps_reactive <- eventReactive(input$goButton_afps, {

      #Define whether the internal standard is known or not
      if(input$std_conc_check_afps == FALSE) {

        std_conc_afps <- NA

      } else {

        std_conc_afps <- input$std_conc_box_afps

      }

      if(input$force_check_afps == FALSE) {

        force_afps_selected <- c()

      } else {

        force_afps_selected <- input$force_afps

      }

      smpl_afps <- filedata2_afps()

      xrdlib2_afps <- filedata3_afps()

        afps_out <- powdR::afps(lib = xrdlib2_afps, smpl = smpl_afps, tth_fps = input$tth_afps,
                              std = gsub(".*: ", "", input$selectINT_afps),
                              std_conc = std_conc_afps,
                              force = gsub(".*: ", "", force_afps_selected),
                              amorphous = gsub(".*: ", "", input$selectAMORPH_afps),
                              align = input$align_afps,
                              manual_align = input$align_man_fps,
                              shift = input$shift_afps,
                              obj = input$selectOBJ_afps,
                              solver = input$selectSolver_afps,
                              lod = input$lod_afps,
                              amorphous_lod = input$amorph_lod_afps)

    })


    afps_out <- afps_reactive()

    output$contents_afps <- renderDataTable({

      afps_table <- data.frame(afps_out$phases)

      afps_table$phase_percent <- round(afps_table$phase_percent, 2)

      afps_table

    }, options = list(lengthMenu = c(5, 10, 15), pageLength = 10))



    output$line_afps <- plotly::renderPlotly({

      plot(afps_out, wavelength = input$selectWAVELENGTHafps, interactive = TRUE)

    })


    #Export the mineral concentrations to a .csv file
    minout_afps <- afps_out
    minout_afps <- data.frame(minout_afps[["phases"]])


    output$download_mins_afps <- downloadHandler(

      filename = function() {
        paste("minerals-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.table(minout_afps, file, sep = ",", col.names = TRUE, row.names = FALSE)
      }
    )

    #Export the weighted patterns
    fitout_afps <- afps_out
    fitout_afps <- data.frame("TTH" = fitout_afps[["tth"]],
                         "MEASURED" = fitout_afps[["measured"]],
                         "FITTED" = fitout_afps[["fitted"]],
                         fitout_afps[["weighted_pure_patterns"]])

    output$download_fit_afps <- downloadHandler(

      filename = function() {
        paste("fit-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.table(fitout_afps, file, sep = ",", col.names = TRUE, row.names = FALSE)
      }
    )

    #Download the whole fps output as .Rdata format
    output$download_afps <- downloadHandler(

      filename = "afps_output.Rdata",
      content = function(con) {
        assign("afps_output", afps_reactive())

        save(list="afps_output", file=con)
      }
    )

  })


  #################################
  ## TAB 6: Results plotter
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

      if (!class(results_plotter_load()) %in% c("powdRafps", "powdRfps")) {

        return(NULL)

      }

      if(input$selectTABLE_viewer == "All phases") {

        table_to_view <- results_plotter_load()$phases
        table_to_view$phase_percent <- round(table_to_view$phase_percent, 2)
        table_to_view

      } else {

        table_to_view_index <- which(names(results_plotter_load()) %in% c("phases_summary", "phases_grouped"))
        table_to_view <- results_plotter_load()[[table_to_view_index]]
        table_to_view$phase_percent <- round(table_to_view$phase_percent, 2)
        table_to_view
      }

    }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10))

    output$results_plot <- plotly::renderPlotly({

      if(class(results_plotter_load()) %in% c("powdRfps", "powdRafps")) {
        plot(results_plotter_load(), wavelength = input$selectWAVELENGTHresults, interactive = TRUE)
      } else {
        return(NULL)
      }

    })

  })


  #######################
  #Results editor       #
  #######################

#Create a recompute button in new results is selected


  #Load the results
    results_editor_load <- reactive({
      infile_results_editor <- input$loadResults_editor
      if (is.null(infile_results_editor)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      bar <- load(infile_results_editor$datapath)
      rpl <- get(bar)
      return(rpl)
    })


  observe({


    if(input$selectPLOTeditor == "Original results") {

      if (!class(results_editor_load()) %in% c("powdRafps", "powdRfps")) {

        return(NULL)

      } else {

      output$contents_editor <- shiny::renderDataTable({

        table_to_view <- results_editor_load()$phases
        table_to_view$phase_percent <- round(table_to_view$phase_percent, 2)
        table_to_view

      }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10))


      output$line_editor <- plotly::renderPlotly({

        if(class(results_editor_load()) %in% c("powdRfps", "powdRafps")) {

          plot(results_editor_load(), wavelength = input$selectWAVELENGTHeditor, interactive = TRUE)

        } else {
          return(NULL)
        }

      })


      }

    }

  })

  observe({

  #If the thing uploaded is a powdRfps or powdRafps object then update the selection box
    if(class(results_editor_load()) %in% c("powdRfps", "powdRafps")) {
      updateSelectInput(session, "selectREMOVE_editor",
                        label = NULL,
                        choices = paste0(results_editor_load()[[5]][[2]], ": ", results_editor_load()[[5]][[1]]),
                        selected = NULL)
    }

  })

  observe({

    if(class(results_editor_load()) %in% c("powdRfps", "powdRafps")) {
      updateSliderInput(session = session, inputId = "tth_editor",
                        min = round(min(results_editor_load()$tth) + abs(input$align_editor), 2),
                        max = round(max(results_editor_load()$tth) - abs(input$align_editor), 2),
                        value = c(round(min(results_editor_load()$tth) + abs(input$align_editor), 2),
                                  round(max(results_editor_load()$tth) - abs(input$align_editor), 2)))
    }


  })


  #   #Load the results
  results_editor_load_lib <- reactive({
       infile_results_editor2 <- input$loadLib_editor
       if (is.null(infile_results_editor2)) {
         # User has not uploaded a file yet
         return(NULL)
       }
       bar2 <- load(infile_results_editor2$datapath)
       rpl2 <- get(bar2)
       return(rpl2)
     })


     observe({

     #If both items have been loaded correctly then update the selectADD box
     if(class(results_editor_load_lib()) == "powdRlib" &
        class(results_editor_load()) %in% c("powdRfps", "powdRafps")) {

       #The selection needs to include all of the phases in the library that are not already in the results
       phase_options <- results_editor_load_lib()[[3]][-which(results_editor_load_lib()[[3]][[1]] %in%
                                                              results_editor_load()[[5]][[1]]),]

       updateSelectInput(session, "selectADD_editor",
                         label = NULL,
                         choices = paste0(phase_options[[2]], ": ", phase_options[[1]]),
                         selected = NULL)

     }

     selectSTDupdate <- reactive({

       if(class(results_editor_load_lib()) == "powdRlib" &
          class(results_editor_load()) %in% c("powdRfps", "powdRafps")) {

         return(input$selectADD_editor)

       } else {

         return(c(""))

       }

     })

     removeSTDupdate <- reactive({

       if(class(results_editor_load_lib()) == "powdRlib" &
          class(results_editor_load()) %in% c("powdRfps", "powdRafps")) {

         return(input$selectREMOVE_editor)

       } else {

         return(c(""))

       }

     })


     observe({

     #Update the internal standard
     if(class(results_editor_load_lib()) == "powdRlib" &
        class(results_editor_load()) %in% c("powdRfps", "powdRafps")) {

       added_phases <- as.character(selectSTDupdate())
       removed_phases <- as.character(removeSTDupdate())

       #The selection needs to include all of the phases in the library that are not already in the results
       std_options <- c(paste0(results_editor_load()[[5]][[2]], ": ", results_editor_load()[[5]][[1]]),
                        added_phases)

       if(length(removed_phases) > 0) {

         std_options <- std_options[-which(std_options %in% removed_phases)]

       }

       std_options <- std_options[order(std_options)]

       updateSelectInput(session, "selectSTD_editor",
                         label = NULL,
                         choices = std_options,
                         selected = NULL)

     }

     })


     })



     #Add the uioutput if standard concentration is known
     output$std_conc_box_editor_ui <- renderUI({
       if (input$std_conc_check_editor == FALSE) return(NULL)

       numericInput("std_conc_box_editor", label = "Define the internal standard concentration",
                    min = 0.01,
                    max = 99.99,
                    value = 20,
                    step = 0.01)

     })

     observeEvent(input$align_man_editor,{

     output$align_editor_ui <- renderUI({

       if (input$align_man_editor == FALSE) return(sliderInput("align_editor", label = NULL, min = 0,
                                                               max = 0.5,
                                                               value = c(0.1)))

      sliderInput("align_editor", label = NULL, min = -0.5,
                     max = 0.5,
                     value = c(0))

     })

     })


  #FULL PATTERN FITTING

     observe({

       fps_reactive_editor <- eventReactive(input$goButton_editor, {

         if(input$std_conc_check_editor == FALSE) {

           std_conc_editor <- NA

         } else {

           std_conc_editor <- input$std_conc_box_editor

         }

         added_phases2 <- as.character(input$selectADD_editor)
         removed_phases2 <- as.character(input$selectREMOVE_editor)

         #The selection needs to include all of the phases in the library that are not already in the results
         phases2 <- c(paste0(results_editor_load()[[5]][[2]], ": ", results_editor_load()[[5]][[1]]),
                          added_phases2)

         if(length(removed_phases2) > 0) {

           phases2 <- phases2[-which(phases2 %in% removed_phases2)]

         }

         smpl_editor <- data.frame("tth" = results_editor_load()[[1]],
                            "counts" = results_editor_load()[[3]])

         xrdlib_editor <- results_editor_load_lib()

         fps_out <- powdR::fps(smpl = smpl_editor, lib = xrdlib_editor,
                               harmonise = TRUE,
                               std = gsub(".*: ", "", input$selectSTD_editor),
                               std_conc = std_conc_editor,
                               refs = gsub(".*: ", "", phases2),
                               align = input$align_editor,
                               tth_fps = input$tth_editor,
                               manual_align = input$align_man_editor,
                               shift = input$shift_editor,
                               obj = input$selectOBJ_editor,
                               solver = input$selectSolver_editor)

       })


     observe({


       fps_out_editor <- fps_reactive_editor()

       if (!input$selectPLOTeditor == "Original results") {

       output$contents_editor <- renderDataTable({

         fps_table_editor <- data.frame(fps_out_editor$phases)

         fps_table_editor$phase_percent <- round(fps_table_editor$phase_percent, 2)

         fps_table_editor

       }, options = list(lengthMenu = c(5, 10, 15), pageLength = 10))



       output$line_editor <- plotly::renderPlotly({

         plot(fps_out_editor, wavelength = input$selectWAVELENGTHeditor, interactive = TRUE)

       })

       }


     })


     observe({

     #Export the mineral concentrations to a .csv file
     minout_editor <- fps_reactive_editor()
     minout_editor <- data.frame(minout_editor[["phases"]])


     output$download_mins_editor <- downloadHandler(

       filename = function() {
         paste("minerals-", Sys.Date(), ".csv", sep="")
       },
       content = function(file) {
         write.table(minout_editor, file, sep = ",", col.names = TRUE, row.names = FALSE)
       }
     )

     #Export the weighted patterns
     fitout_editor <- fps_reactive_editor()
     fitout_editor <- data.frame("TTH" = fitout_editor[["tth"]],
                               "MEASURED" = fitout_editor[["measured"]],
                               "FITTED" = fitout_editor[["fitted"]],
                               fitout_editor[["weighted_pure_patterns"]])

     output$download_fit_editor <- downloadHandler(

       filename = function() {
         paste("fit-", Sys.Date(), ".csv", sep="")
       },
       content = function(file) {
         write.table(fitout_editor, file, sep = ",", col.names = TRUE, row.names = FALSE)
       }
     )

     #Download the whole fps output as .Rdata format
     output$download_editor <- downloadHandler(

       filename = "fps_editor_output.Rdata",
       content = function(con) {
         assign("fps_editor_output", fps_reactive_editor())

         save(list="fps_editor_output", file=con)
       }
     )

     })

     })

     #-----------------------
     #VIDEOS
     #-----------------------


     output$video <- renderUI({

         HTML(paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/', input$selectVIDEO, '" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))

     })


  session$onSessionEnded(function() {
    stopApp()
  })

}) ## end  shinyServer
