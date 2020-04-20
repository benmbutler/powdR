## Server.R

shiny::shinyServer(function(input, output, session) {

  #################################
  ## TAB 1: Reference library builder
  #################################
    shiny::observe({
      #Load the xrd.csv file
      xrddata <- shiny::reactive({
        infile1 <- input$uploadXRD
        if (is.null(infile1)) {
          # User has not uploaded a file yet
          return(NULL)
          }
        read.csv(infile1$datapath, header = TRUE, stringsAsFactors = FALSE)
        })

      #Load the phase.csv file
      phasedata <- shiny::reactive({
        infile2 <- input$uploadPHASE
        if (is.null(infile2)) {
          #User has not uploaded a file yet
          return(NULL)
          }
        read.csv(infile2$datapath, header = TRUE, stringsAsFactors = FALSE)
        })

      #Create a powdRlib object
      Dataset <- shiny::eventReactive(input$BuildLibButton, {
        Dataset <- powdR::powdRlib(xrd_table = xrddata(),
                                   phases_table = phasedata())
        })

      #Download the library
      output$download_lib <- shiny::downloadHandler(
        filename = "my_powdRlib.Rdata",
        content = function(con) {
          assign(input$name, Dataset())
          save(list=input$name, file=con)
          }
        )

      #Tabulate the minerals in the library
      output$minerals_table <- DT::renderDataTable({
        Dataset()[[3]]
        }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10))
      })

    shiny::observe({
      minerals_xrd <- powdR::minerals_xrd

      #Downloads of example data
      output$download_xrd_eg <- shiny::downloadHandler(
        filename = function() {
          paste("xrd_example_", Sys.Date(), ".csv", sep="")
          },
        content = function(file) {
          write.table(minerals_xrd, file, sep = ",", col.names = TRUE, row.names = FALSE)
          }
        )
      })

    shiny::observe({
      minerals_phases <- powdR::minerals_phases
      output$download_phases_eg <- shiny::downloadHandler(
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

  shiny::observe({

  #Load library
  lib_plotter_load <- shiny::reactive({
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
    shiny::updateSelectInput(session, "selectPHASES_plotter",
                             label = paste("Choose phases from the library to plot."),
                             choices = paste0(lib_plotter_load()[[3]][[2]], ": ",
                                              lib_plotter_load()[[3]][[1]]),
                             selected = head(paste0(lib_plotter_load()[[3]][[2]], ": ",
                                                    lib_plotter_load()[[3]][[1]]), 1))
    }

  #Tabulate the minerals in the library
  output$lib_table <- DT::renderDataTable({
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
    ## TAB 3: Reference library Editor
    #################################

    shiny::observe({

      #Load library
      lib_editor_load <- shiny::reactive({
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
        shiny::updateSelectInput(session, "selectPHASES_editor",
                                 label = paste("Select reference patterns to subset"),
                                 choices = paste0(lib_editor_load()[[3]][[2]], ": ",
                                                  lib_editor_load()[[3]][[1]]),
                                 selected = head(paste0(lib_editor_load()[[3]][[2]], ": ",
                                                        lib_editor_load()[[3]][[1]]), 1))
        }

      #Create a powdRlib object
      subset_lib <- shiny::eventReactive(input$SubsetLibButton, {
        subset_lib <- subset(lib_editor_load(),
                             refs = gsub(".*: ", "", input$selectPHASES_editor),
                             mode = input$selectMODE_editor)
        })

      #Download the library
      output$download_subset_lib <- shiny::downloadHandler(
        filename = "my_powdRlib.Rdata",
        content = function(con) {
          assign(input$name_editor, subset_lib())
          save(list=input$name_editor, file=con)
          }
        )

      #Tabulate the minerals in the library
      output$minerals_subset_table <- DT::renderDataTable({
        subset_lib()[[3]]
        }, options = list(lengthMenu = c(10, 25, 50), pageLength = 10))
      })

    ##################################
    # TAB 4: Full pattern fitting
    ##################################

    #-------------------------------
    #Examples
    #-------------------------------

    shiny::observe({
      soils <- powdR::soils
      output$download_soil_sand <- shiny::downloadHandler(
        filename = function() {
          paste("sandstone_example_", Sys.Date(), ".xy", sep="")
        },
        content = function(file) {
          write.table(soils$sandstone, file, sep = " ", col.names = FALSE, row.names = FALSE)
        }
      )

      #Downloads of example limestone data
      output$download_soil_lime <- shiny::downloadHandler(
        filename = function() {
          paste("limestone_example_", Sys.Date(), ".xy", sep="")
        },
        content = function(file) {
          write.table(soils$limestone, file, sep = " ", col.names = FALSE, row.names = FALSE)
        }
      )

      #Downloads of example granite data
      output$download_soil_granite <- shiny::downloadHandler(
        filename = function() {
          paste("granite_example_", Sys.Date(), ".xy", sep="")
        },
        content = function(file) {
          write.table(soils$granite, file, sep = " ", col.names = FALSE, row.names = FALSE)
        }
      )

      output$download_rj_mix1 <- shiny::downloadHandler(
        filename = function() {
          paste("rockjock_mix1_", Sys.Date(), ".xy", sep="")
        },
        content = function(file) {
          write.table(rockjock_mixtures$Mix1, file, sep = " ", col.names = FALSE, row.names = FALSE)
        }
      )

      output$download_rj_mix2 <- shiny::downloadHandler(
        filename = function() {
          paste("rockjock_mix2_", Sys.Date(), ".xy", sep="")
        },
        content = function(file) {
          write.table(rockjock_mixtures$Mix2, file, sep = " ", col.names = FALSE, row.names = FALSE)
        }
      )
    })

    #Download an example reference library
    shiny::observe({
      minerals <- powdR::minerals
      output$download_example_ref <- shiny::downloadHandler(
        filename = "example_powdRlib.Rdata",
        content = function(con) {
          assign("example_powdRlib", minerals)
          save(list="example_powdRlib", file=con)
        }
      )

      output$download_rj_lib <- shiny::downloadHandler(
        filename = "rockjock_powdRlib.Rdata",
        content = function(con) {
          assign("rockjock", rockjock)
          save(list="rockjock", file=con)
        }
      )
    })


    #---------------------------------------------------
    #Uploading data
    #---------------------------------------------------

    #Load the .xy sample file
    filedata2 <- shiny::reactive({
      infile2 <- input$loadXY
      if (is.null(infile2)) {
        #User has not uploaded a file yet
        return(NULL)
      }
      csv1 <- read.csv(infile2$datapath, sep = " ", header = FALSE)
      return(csv1)
    })

    #If a library has been uploaded, then create a reactive library
    filedata3 <- shiny::reactive({
      infile3 <- input$loadLIB
      if (is.null(infile3)) {
        #User has not uploaded a file yet
        return(NULL)
      }
      bar <- load(infile3$datapath)
      fd3 <- get(bar)
      return(fd3)
    })

    shiny::observe({

      #If automated mode is selected then update the selection box
      if (input$selectMode_fps == "Automated") {
        return(shiny::updateSelectInput(session, "selectSolver_fps",
                                        label = NULL,
                                        choices = c("BFGS", "Nelder-Mead", "CG"),
                                        selected = "BFGS"))
      }

      if (input$selectMode_fps == "Manual") {
        return(shiny::updateSelectInput(session, "selectSolver_fps",
                                        label = NULL,
                                        choices = c("BFGS", "Nelder-Mead", "CG", "NNLS"),
                                        selected = "BFGS"))
      }
    })


    output$amorph_help_ui <- shiny::renderUI({
      if (input$selectMode_fps == "Manual") return(NULL)
      return(shiny::helpText("Select which phases (if any) should be treated as amorphous"))
    })

    output$selectAMORPH_ui <- shiny::renderUI({
      if (input$selectMode_fps == "Manual") return(NULL)
      return(shiny::selectInput(inputId = "selectAMORPH",
                                label = NULL,
                                choices = c(),
                                multiple = TRUE,
                                selectize = TRUE))
    })

    output$force_help_ui <- shiny::renderUI({
      if (input$selectMode_fps == "Manual" | !class(filedata3()) == "powdRlib") return(NULL)

      return(shiny::helpText("Select the phases (if any) that should be forced to remain in the output"))
    })

    output$selectFORCE_ui <- shiny::renderUI({
      if (input$selectMode_fps == "Manual" | !class(filedata3()) == "powdRlib") return(NULL)
      refs_force <- list("PHASE_NAMES" = as.list(paste0(filedata3()[[3]][[2]])),
                         "PHASE IDs" = as.list(paste0(filedata3()[[3]][[2]], ": ", filedata3()[[3]][[1]])))

      #Order alphabetically
      refs_force[[1]] <- refs_force[[1]][order(unlist(refs_force[[1]]))]
      refs_force[[2]] <- refs_force[[2]][order(unlist(refs_force[[2]]))]

      return(shiny::selectInput("selectFORCE",
                                label = NULL,
                                choices = refs_force,
                                selected = NULL,
                                multiple = TRUE,
                                selectize = TRUE))
    })

    #Update the selectINPUT boxes in the full pattern fitting tab
    shiny::observe({
      if (class(filedata3()) == "powdRlib") {
        refs_choices <- list("PHASE_NAMES" = as.list(paste0(filedata3()[[3]][[2]])),
                             "PHASE IDs" = as.list(paste0(filedata3()[[3]][[2]], ": ", filedata3()[[3]][[1]])))
        #Order alphabetically
        refs_choices[[1]] <- refs_choices[[1]][order(unlist(refs_choices[[1]]))]
        refs_choices[[2]] <- refs_choices[[2]][order(unlist(refs_choices[[2]]))]

        return(shiny::updateSelectInput(session, "selectPHASES_fps",
                                        choices = refs_choices,
                                        selected = refs_choices[[1]][[1]]))
      }
    })

    shiny::observe({
      if (class(filedata3()) == "powdRlib") {

        int_choices <- paste0(filedata3()[[3]][[2]], ": ", filedata3()[[3]][[1]])


        return(shiny::updateSelectInput(session, "selectINT_fps",
                                        choices = int_choices,
                                        selected = int_choices[1]))
      }
    })

    #Update the internal standard selectInput so that only phases selected for fitting
    #are available
    # shiny::observe({
    #
    #   scu2 <- as.character(input$selectPHASES_fps)
    #   scu2 <- filedata3()[[3]][which(filedata3()[[3]][[1]] %in% sub(".*: ", "", scu2) |
    #                                    filedata3()[[3]][[2]] %in% sub(".*: ", "", scu2)), 1:2]
    #   scu2 <- paste0(scu2[[2]], ": ", scu2[[1]])
    #
    #   return(shiny::updateSelectInput(session, "selectINT_fps",
    #                                   label = NULL,
    #                                   choices = scu2,
    #                                   selected = head(scu2, 1)))
    # })
    #
    # shiny::observe({
    #
    #   scu <- as.character(input$selectPHASES_fps)
    #   scu <- filedata3()[[3]][which(filedata3()[[3]][[1]] %in% sub(".*: ", "", scu) |
    #                                   filedata3()[[3]][[2]] %in% sub(".*: ", "", scu)), 1:2]
    #   scu <- paste0(scu[[2]], ": ", scu[[1]])
    #
    #   return(shiny::updateSelectInput(session, "selectINT_fps",
    #                                   label = NULL,
    #                                   choices = scu,
    #                                   selected = head(scu, 1)))
    #
    # })


    # shiny::observe({
    #
    #   scu3 <- as.character(input$selectPHASES_fps)
    #   scu3 <- filedata3()[[3]][which(filedata3()[[3]][[1]] %in% sub(".*: ", "", scu3) |
    #                                    filedata3()[[3]][[2]] %in% sub(".*: ", "", scu3)), 1:2]
    #   scu3 <- paste0(scu3[[2]], ": ", scu3[[1]])
    #
    #   if (input$selectMode_fps == "Automated") {
    #     return(shiny::updateSelectInput(session, "selectAMORPH",
    #                                     label = NULL,
    #                                     choices = scu3,
    #                                     selected = NULL))
    #   }
    # })


    shiny::observe({

      amorph_choices <- paste0(filedata3()[[3]][[2]], ": ", filedata3()[[3]][[1]])

      if (input$selectMode_fps == "Automated") {
        return(shiny::updateSelectInput(session, "selectAMORPH",
                                        label = NULL,
                                        choices = amorph_choices,
                                        selected = NULL))
      }
    })

    shiny::observe({

      output$std_conc_help_ui <- shiny::renderUI({

        if (input$std_conc_check_fps == FALSE) return(NULL)

        return(shiny::helpText("Define the internal standard concentration"))
      })

    })

    shiny::observe({

      output$std_conc_box_fps_ui <- shiny::renderUI({

        if (input$std_conc_check_fps == FALSE) return(NULL)

        return(shiny::numericInput("std_conc_box_fps", label = NULL,
                                   min = 0.01,
                                   max = 99.99,
                                   value = 20,
                                   step = 0.01))
      })

    })

    #Use the selected library to adjust the 2theta slider
    shiny::observe({

      if (!is.null(input$loadLIB)) {
        return(shiny::updateSliderInput(session = session, inputId = "tth_fps_slide",
                                        min = round(min(as.numeric(filedata3()[[2]])) + abs(input$align_fps), 2),
                                        max = round(max(as.numeric(filedata3()[[2]])) - abs(input$align_fps), 2),
                                        value = c(round(min(as.numeric(filedata3()[[2]])) + abs(input$align_fps), 2),
                                                  round(max(as.numeric(filedata3()[[2]])) - abs(input$align_fps), 2))))
      }
    })

    #Modify the lod slider for manual and automated fitting
    shiny::observe({
      if (input$selectMode_fps == "Manual") {

        output$slide_help_ui <- shiny::renderUI({

          return(shiny::helpText("Adjust the slider to remove any trace phases from the output (%)."))

        })

      }

      if (input$selectMode_fps == "Automated") {

        output$slide_help_ui <- shiny::renderUI({

          return(shiny::helpText("Adjust the slider to represent the LOD of the internal standard (%)."))

        })

      }
    })

    #Use the tickbox to adjust the alignment slider
    shiny::observe({
      if (input$align_man_fps == TRUE) {
        return(shiny::updateSliderInput(session = session, inputId = "align_fps",
                                        min = -0.5,
                                        max = 0.5,
                                        value = 0))
      }

      if (input$align_man_fps == FALSE) {
        return(shiny::updateSliderInput(session = session, inputId = "align_fps",
                                        min = 0,
                                        max = 0.5,
                                        value = 0.1))
      }
    })

    #FULL PATTERN FITTING

    shiny::observe({



      fps_reactive <- shiny::eventReactive(input$goButton_fps, {

        if (input$std_conc_check_fps == FALSE) {
          std_conc_fps <- NA
        }

        if (input$std_conc_check_fps == TRUE) {
          std_conc_fps <- input$std_conc_box_fps
        }


        if (input$selectMode_fps == "Manual") {
          fps_out <- powdR::fps(lib = filedata3(),
                                smpl = filedata2(),
                                std = sub(".*: ", "", input$selectINT_fps),
                                std_conc = std_conc_fps,
                                refs = c(sub(".*: ", "", input$selectPHASES_fps),
                                         sub(".*: ", "", input$selectINT_fps)),
                                align = input$align_fps,
                                tth_fps = input$tth_fps_slide,
                                manual_align = input$align_man_fps,
                                shift = input$shift_fps,
                                obj = input$selectOBJ_fps,
                                solver = input$selectSolver_fps,
                                remove_trace = input$lod_slide)
          return(fps_out)
        }
        if (input$selectMode_fps == "Automated") {
          afps_out <- powdR::afps(lib = filedata3(),
                                  smpl = filedata2(),
                                  std = sub(".*: ", "", input$selectINT_fps),
                                  std_conc = std_conc_fps,
                                  refs = c(sub(".*: ", "", input$selectPHASES_fps),
                                           sub(".*: ", "", input$selectINT_fps),
                                           sub(".*: ", "", input$selectAMORPH)),
                                  force = sub(".*: ", "", input$selectFORCE),
                                  align = input$align_fps,
                                  tth_fps = input$tth_fps_slide,
                                  manual_align = input$align_man_fps,
                                  shift = input$shift_fps,
                                  obj = input$selectOBJ_fps,
                                  solver = input$selectSolver_fps,
                                  lod = input$lod_slide,
                                  amorphous = sub(".*: ", "", input$selectAMORPH))
          return(afps_out)
        }
      })

      fps_out <- fps_reactive()

      output$contents_fps <- DT::renderDataTable({
        fps_table <- data.frame(fps_out$phases)
        fps_table$phase_percent <- round(fps_table$phase_percent, 2)
        return(fps_table)
      }, options = list(lengthMenu = c(5, 10, 15), pageLength = 10))

      output$line_fps <- plotly::renderPlotly({
        return(plot(fps_out, wavelength = input$selectWAVELENGTHfps, interactive = TRUE))
      })

      #Export the mineral concentrations to a .csv file
      minout <- fps_out
      minout <- data.frame(minout$phases)
      output$download_mins <- shiny::downloadHandler(
        filename = function() {
          paste("minerals-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.table(minout, file, sep = ",", col.names = TRUE, row.names = FALSE)
        }
      )

      #Export the weighted patterns
      #fitout <- fps_out
      #fitout <- data.frame("TTH" = fitout[[1]],
      #                     "MEASURED" = fitout[[3]],
      #                     "FITTED" = fitout[[2]],
      #                     fitout[[8]])

      output$download_calc <- shiny::downloadHandler(
        filename = function() {
          paste("calculated_", Sys.Date(), ".xy", sep="")
        },
        content = function(file) {
          write.table(data.frame("X" = fps_reactive()$tth,
                                 "Y" = fps_reactive()$fitted), file, sep = " ", col.names = FALSE, row.names = FALSE)
        }
      )

      output$download_meas <- shiny::downloadHandler(
        filename = function() {
          paste("measured_", Sys.Date(), ".xy", sep="")
        },
        content = function(file) {
          write.table(data.frame("X" = fps_reactive()$tth,
                                 "Y" = fps_reactive()$measured), file, sep = " ", col.names = FALSE, row.names = FALSE)
        }
      )

      #Download the whole fps output as .Rdata format
      output$download_fps <- shiny::downloadHandler(
        filename = "fps_output.Rdata",
        content = function(con) {
          assign("fps_output", fps_reactive())
          save(list="fps_output", file=con)
        }
      )
    })


    #######################
    #TAB 5: Results editor
    #######################

    #Create a recompute button in new results is selected

    #Load the results
    results_editor_load <- shiny::reactive({
      infile_results_editor <- input$loadResults_editor
      if (is.null(infile_results_editor)) {
        #User has not uploaded a file yet
        return(NULL)
        }
      bar <- load(infile_results_editor$datapath)
      rpl <- get(bar)
      return(rpl)
      })

    shiny::observe({

      if(input$selectPLOTeditor == "Original results") {

        if (!class(results_editor_load()) %in% c("powdRafps", "powdRfps")) {

          return(NULL)

          } else {

            output$contents_editor <- DT::renderDataTable({

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

    shiny::observe({
      #If the thing uploaded is a powdRfps or powdRafps object then update the selection box
      if(class(results_editor_load()) %in% c("powdRfps", "powdRafps")) {
        shiny::updateSelectInput(session, "selectREMOVE_editor",
                                 label = NULL,
                                 choices = paste0(results_editor_load()[[5]][[2]], ": ", results_editor_load()[[5]][[1]]),
                                 selected = NULL)
        }
      })

    shiny::observe({
      if(class(results_editor_load()) %in% c("powdRfps", "powdRafps")) {
        shiny::updateSliderInput(session = session, inputId = "tth_editor",
                                 min = round(min(results_editor_load()$tth) + abs(input$align_editor), 2),
                                 max = round(max(results_editor_load()$tth) - abs(input$align_editor), 2),
                                 value = c(round(min(results_editor_load()$tth) + abs(input$align_editor), 2),
                                           round(max(results_editor_load()$tth) - abs(input$align_editor), 2)))
        }
      })

    #Load the results
    results_editor_load_lib <- shiny::reactive({
      infile_results_editor2 <- input$loadLib_editor
      if (is.null(infile_results_editor2)) {
        #User has not uploaded a file yet
        return(NULL)
        }
      bar2 <- load(infile_results_editor2$datapath)
      rpl2 <- get(bar2)
      return(rpl2)
      })

    shiny::observe({
      #If both items have been loaded correctly then update the selectADD box
      if(class(results_editor_load_lib()) == "powdRlib" &
         class(results_editor_load()) %in% c("powdRfps", "powdRafps")) {

        #The selection needs to include all of the phases in the library that are not already in the results
        phase_options <- results_editor_load_lib()[[3]][-which(results_editor_load_lib()[[3]][[1]] %in%
                                                                 results_editor_load()[[5]][[1]]),]

        shiny::updateSelectInput(session, "selectADD_editor",
                                 label = NULL,
                                 choices = paste0(phase_options[[2]], ": ", phase_options[[1]]),
                                 selected = NULL)
        }

      selectSTDupdate <- shiny::reactive({
        if(class(results_editor_load_lib()) == "powdRlib" &
           class(results_editor_load()) %in% c("powdRfps", "powdRafps")) {
          return(input$selectADD_editor)
          } else {
            return(c(""))
            }
        })

      removeSTDupdate <- shiny::reactive({
        if(class(results_editor_load_lib()) == "powdRlib" &
           class(results_editor_load()) %in% c("powdRfps", "powdRafps")) {
          return(input$selectREMOVE_editor)
          } else {
            return(c(""))
            }
        })

      shiny::observe({

        #Update the internal standard
        if(class(results_editor_load_lib()) == "powdRlib" &
           class(results_editor_load()) %in% c("powdRfps", "powdRafps")) {

          added_phases <- as.character(selectSTDupdate())
          removed_phases <- as.character(removeSTDupdate())

          #The selection needs to include all of the phases in the library that are not already in the results
          std_options <- c(paste0(results_editor_load()[[5]][[2]], ": ",
                                  results_editor_load()[[5]][[1]]),
                           added_phases)

          if(length(removed_phases) > 0) {
            std_options <- std_options[-which(std_options %in% removed_phases)]
            }

          std_options <- std_options[order(std_options)]

          shiny::updateSelectInput(session, "selectSTD_editor",
                                   label = NULL,
                                   choices = std_options,
                                   selected = NULL)
          }
        })
      })

    #Add the uioutput if standard concentration is known
    output$std_conc_box_editor_ui <- shiny::renderUI({
      if (input$std_conc_check_editor == FALSE) return(NULL)
      shiny::numericInput("std_conc_box_editor", label = "Define the internal standard concentration",
                          min = 0.01,
                          max = 99.99,
                          value = 20,
                          step = 0.01)
      })

    shiny::observeEvent(input$align_man_editor,{
      output$align_editor_ui <- shiny::renderUI({
        if (input$align_man_editor == FALSE) return(shiny::sliderInput("align_editor", label = NULL, min = 0,
                                                                       max = 0.5,
                                                                       value = c(0.1)))
        shiny::sliderInput("align_editor", label = NULL, min = -0.5,
                           max = 0.5,
                           value = c(0))
        })
      })

    #FULL PATTERN FITTING

    shiny::observe({
      fps_reactive_editor <- shiny::eventReactive(input$goButton_editor, {

        if(input$std_conc_check_editor == FALSE) {
          std_conc_editor <- NA
          } else {
            std_conc_editor <- input$std_conc_box_editor
            }
        added_phases2 <- as.character(input$selectADD_editor)
        removed_phases2 <- as.character(input$selectREMOVE_editor)

        #The selection needs to include all of the phases in the library that are not already in the results
        phases2 <- c(paste0(results_editor_load()[[5]][[2]], ": ",
                            results_editor_load()[[5]][[1]]),
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

      shiny::observe({
        fps_out_editor <- fps_reactive_editor()
        if (!input$selectPLOTeditor == "Original results") {
          output$contents_editor <- DT::renderDataTable({
            fps_table_editor <- data.frame(fps_out_editor$phases)
            fps_table_editor$phase_percent <- round(fps_table_editor$phase_percent, 2)
            fps_table_editor
            }, options = list(lengthMenu = c(5, 10, 15), pageLength = 10))

          output$line_editor <- plotly::renderPlotly({
            plot(fps_out_editor, wavelength = input$selectWAVELENGTHeditor, interactive = TRUE)
          })
          }
        })

      shiny::observe({
        #Export the mineral concentrations to a .csv file
        #minout_editor <- fps_reactive_editor()
        #minout_editor <- data.frame(minout_editor[["phases"]])

        output$download_mins_editor <- shiny::downloadHandler(
          filename = function() {
            paste("minerals_", Sys.Date(), ".csv", sep="")
            },
          content = function(file) {
            write.table(fps_reactive_editor()$phases, file, sep = ",", col.names = TRUE, row.names = FALSE)
            }
          )

        #Export the weighted patterns
        #fitout_editor <- fps_reactive_editor()
        #fitout_editor <- data.frame("TTH" = fitout_editor[["tth"]],
        #                            "MEASURED" = fitout_editor[["measured"]],
        #                            "FITTED" = fitout_editor[["fitted"]],
        #                            fitout_editor[["weighted_pure_patterns"]])

        output$download_meas_editor <- shiny::downloadHandler(
          filename = function() {
            paste("measured_", Sys.Date(), ".xy", sep="")
          },
          content = function(file) {
            write.table(data.frame("X" = fps_reactive_editor()$tth,
                                   "Y" = fps_reactive_editor()$measured), file, sep = " ", col.names = FALSE, row.names = FALSE)
          }
        )

        output$download_calc_editor <- shiny::downloadHandler(
          filename = function() {
            paste("calculated_", Sys.Date(), ".xy", sep="")
          },
          content = function(file) {
            write.table(data.frame("X" = fps_reactive_editor()$tth,
                                   "Y" = fps_reactive_editor()$measured), file, sep = " ", col.names = FALSE, row.names = FALSE)
          }
        )

        # output$download_fit_editor <- shiny::downloadHandler(
        #   filename = function() {
        #     paste("fit-", Sys.Date(), ".csv", sep="")
        #     },
        #   content = function(file) {
        #     write.table(fitout_editor, file, sep = ",", col.names = TRUE, row.names = FALSE)
        #     }
        #   )

        #Download the whole fps output as .Rdata format
        output$download_editor <- shiny::downloadHandler(
          filename = "fps_editor_output.Rdata",
          content = function(con) {
            assign("fps_editor_output", fps_reactive_editor())
            save(list="fps_editor_output", file=con)
            }
          )
        })
      })

    ############################
    #TAB 6: HELP
    ############################

      output$video <- shiny::renderUI({
        shiny::HTML(paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/', input$selectVIDEO,
                           '" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
        })

    session$onSessionEnded(function() {
      shiny::stopApp()
      })

    }) ## end  shinyServer
