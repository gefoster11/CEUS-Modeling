# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This application is meant to load CEUS video intensity data outputted from NarNar.
# Click on data points in the model to clean and reject outliers
# Once finished click the download button to save a .zip file containing the outputted action potential files

# Sets max file size to 800 Mb
options(#shiny.maxRequestSize = 800*1024^2,
  shiny.launch.browser = .rs.invokeShinyWindowExternal,
  scipen = 999)

# Package Dependency
packages = c("remotes",
             "shiny",
             "shinyBS",
             "bslib",
             "tidyverse",
             "zip",
             "thematic",
             "shinythemes",
             "DT",
             "rlang",
             "broom",
             "minpack.lm",
             "gt")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Package Dependency - Install plotly from github
if (!require("plotly", character.only = TRUE)) {
  remotes::install_github("ropensci/plotly")
  library("plotly", character.only = TRUE)
}

# Load functions
functions <- list.files("./functions/", full.names = TRUE, pattern = ".r")
functions %>% map(source)


# ---- Define UI for data upload app ----
ui <- fluidPage(
    #theme = shinytheme("flatly"),
    # ---- App title ----
    titlePanel("Contrast Enhanced Ultrasound Modeling"),
    
    # ---- Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # ---- Sidebar panel for inputs ----
        sidebarPanel(width = 3,
            
            # ---- Input: NarNar file ----
            fileInput("NarNar", "NarNar File",
                      accept = ".csv"),
            
            tags$hr(),
            tags$div(HTML("<p>Study Details</p><br>")),
            
            # ---- Input: ID ----
            selectizeInput('ID', "Participant ID", choices = NULL, multiple = FALSE, width = "90%", options = list(create = TRUE)),
            bsTooltip("ID", "Provide Participant ID", 
                      "right", options = list(container = "body")),

            # ---- Input: ROIs  ----
            selectizeInput('ROI', "ROI to analyze", 
                           choices = NULL,
                           multiple = FALSE, width = "90%", options = list(create = TRUE)),
            bsTooltip("ROI", "Choose the ROI to analyze", 
                      "right", options = list(container = "body")),

            # ---- Horizontal line ----
            tags$hr(),

            # ---- Parent card: Model Parameters ----
            card(
              card_header("Model Parameters"),
              
              layout_columns(
                col_widths = c(6, 6),
                fill = FALSE,
                
                # Left column: A Values stacked
                div(
                  card(numericInput("Astart", "A Start Value",value = NA, width = "80%")),
                  card(numericInput("ALower", "A Lower Limit", value = NA, width = "80%")),
                  card(numericInput("AUpper", "A Upper Limit", value = NA, width = "80%")),
                  card(numericInput("ATolerance", "A Tolerance", value = 5, min = 0, max = 10, width = "80%"))
                ),
                
                # Right column: B Values stacked
                div(
                  card(numericInput("Bstart", "B Start Value", value = 0.1, width = "80%")),
                  card(numericInput("BLower", "B Lower Limit", value = 0, width = "80%")),
                  card(numericInput("BUpper", "B Upper Limit", value = 10, width = "80%")),
                  card(numericInput("loess", "Loess Span", value = 0.8, min = 0.1, step = 0.1, max = 1, width = "80%")),
                  card(actionButton("save_fit", "Save Fit", width = "80%")),
                )
              )
            ),
            
            # ---- Horizontal line ----
            tags$hr(),
            
            # ---- Analysis Options ____
            checkboxInput("select_all_analysis", "Select all analysis options", FALSE),
            
            selectizeInput( #### Add new analysis options here
              "Analysis_Options", 
              "Select analysis options:", 
              list("AUC filtered" = "AUC",
                   "AUC predicted" = "AUCpredict",
                   "Peak dVI" = "PeakdVI",
                   "Peak filtered dVI" = "PeakdVIfilt",
                   "Maximum differential filtered" = "Maxdiff", 
                   "Maximum differential predicted" = "maxdiffpredict",
                   "Mean dVI" = "MeandVI",
                   "Mean filtered dVI" = "MeandVIfilt",
                   "Mean dVI predicted" = "MeandVIpredicted",
                   "Wash-in Model" = "Model"), 
              multiple = TRUE 
            ), 

            
            # ---- Horizontal line ----
            tags$hr(),
            
            actionButton("reset", "Reset", width = "100%"),
            bsTooltip("reset", "click to reset selected removed data",
                      "right", options = list(container = "body")),
            
            # ---- Horizontal line ----
            tags$hr(),
            
            downloadButton("save", "Download"),
            bsTooltip("save", "saves the output files", "right", 
                      options = list(container = "body")),
            
            # ---- Horizontal line ----
            tags$hr(),
            
            # ---- Reference ----
            tags$div(
              HTML("<p>2025; Created by Glen Foster</p><br>")),
        ),
        
        # ---- Main panel ----
        mainPanel(
            
            # ---- Output: Tabset w/ parsing table, selected data, and plot ----
            tabsetPanel(type = "tabs",
                        tabPanel("Data", 
                                    fluidRow(
                                      DT::dataTableOutput("data_in")  
                                    ),
                                 
                                 tags$hr(),
                                 
                                 #### ---- Input: Analysis Output
                                 selectizeInput( #### Add new analysis options here
                                   "selected_analysis", 
                                   "Select output data to display:", 
                                   list("Data and Fitted" = "Augmented", 
                                        "Predicted Model" = "Predicted", 
                                        "Parameters" = "Tidyed"), 
                                   multiple = FALSE 
                                 ),                                  
                                 
                                    fluidRow(
                                      DT::dataTableOutput("resultsTable")
                                    )
                                 ),
                        tabPanel("CEUS Washin Model",

                                 fluidRow(
                                   plotlyOutput("plot", height = "60vh")
                                 ),
                                 fluidRow(
                                   DT::dataTableOutput("model_params")  
                                 ),
                                 ),
                        )
            )
    )
)

# ---- Define server logic ----
server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
    # ---- initialize reactive values ----
    values <- reactiveValues(
        )
    
    # ---- Listen: AP data inputs ----
    loadlisten <- reactive({
        list(input$ID, input$NarNar$datapath)
    })
    
    # ---- Observe: data load - df ----
    observeEvent(loadlisten(), {
      req(input$ID, input$NarNar$datapath)
      
      #browser()
      
      tryCatch(
        {
          file <- input$NarNar$datapath
          ID <- input$ID
          
          # load locations
          df <- load_narnar(file, ID)
          
          # determine available ROIS and update input
          myROIs <- unique(df$ROI)
          updateSelectizeInput(session, "ROI", choices = myROIs, selected = myROIs[[1]])
          
          # Determine A Value start
          peakvalue <- peak_value(df$dVI)
          updateNumericInput(session, "Astart", value = peakvalue)
          
          # set Lower limit
          tolerance <- input$ATolerance
          updateNumericInput(session, "ALower", value = peakvalue*(1-(tolerance)/100))
          # set Upper limit
          updateNumericInput(session, "AUpper", value = peakvalue*(1+(tolerance)/100))
          
          values$df <- df
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
    })

    # ---- Output: Data Table ----
    output$data_in <- DT::renderDataTable({
      req(values$df)
      #browser() 
      df <- values$df
      ROIs <- input$ROI
        df %>%
        dplyr::filter(ROI %in% input$ROI) %>%
        DT::datatable(options = list(pageLength = 10),
                      filter = "top",
                      caption = "Input Data") %>%
        DT::formatRound(columns = c("Time", "VI", "dVI"), digits = 2)
        
    })
    
    
    # ---- Update model parameters when ROI changes ----
    observeEvent(input$ROI, {
      req(values$df)
      df <- isolate(values$df) %>% filter(ROI == input$ROI)   # or however you subset to ROI
      
      # Determine A Value start
      peakvalue <- peak_value(df$dVI)
      updateNumericInput(session, "Astart", value = peakvalue)
      
      # Determine A Value start
      peakvalue <- peak_value(df$dVI)
      updateNumericInput(session, "AStart", value = peakvalue)
      
      # set Lower limit
      tolerance <- isolate(input$ATolerance)
      updateNumericInput(session, "ALower", value = peakvalue * (1 - tolerance/100))
      
      # set Upper limit
      updateNumericInput(session, "AUpper", value = peakvalue * (1 + tolerance/100))
    })  
    
    
    
   # ---- Output: Create Plot ----
    output$plot <- renderPlotly({
      
      req(values$df)
      
      # Filter by selected ROI
      SelectedROI <- input$ROI
      temp <- values$df %>% filter(ROI %in% SelectedROI)
      
      # Get model fit
      fitted <- fit_model()
      
      # Start plot
      p <- plot_ly()
      
      # --- Predicted line (navy) ---
      if (!is.null(fitted) && "predicted" %in% names(fitted)) {
        p <- p %>%
          add_trace(
            data = fitted$predicted,
            x = ~Time,
            y = ~dVI_predict,
            type = "scatter",
            mode = "lines",
            line = list(color = "navy", width = 5),
            name = "Predicted",
            legendgroup = "Predicted",
            showlegend = TRUE,
            inherit = FALSE
          )
      }
      
      # --- Filtered line (goldenrod2) ---
      filtered_data <- temp %>%
        filter(!exclude) %>%
        mutate(dVI_filt = predict(loess(dVI ~ Time, span = as.numeric(input$loess), data = .)))
      
      p <- p %>%
        add_trace(
          data = filtered_data,
          x = ~Time,
          y = ~dVI_filt,
          type = "scatter",
          mode = "lines",
          line = list(color = "goldenrod2", width = 5),
          name = "Filtered",
          legendgroup = "Filtered",
          showlegend = TRUE,
          inherit = FALSE
        )
      
      # --- Data points ---
      # Map included/excluded manually
      temp_points <- temp %>%
        mutate(point_group = ifelse(exclude, "Excluded", "Included"))
      
      colors <- c("Included" = "goldenrod2", "Excluded" = "gray70")
      opacities <- c("Included" = 1, "Excluded" = 0.6)
      
      for(group in c("Included", "Excluded")) {
        group_data <- temp_points %>% filter(point_group == group)
        p <- p %>%
          add_trace(
            data = group_data,
            x = ~Time,
            y = ~dVI,
            type = "scatter",
            mode = "markers",
            marker = list(
              color = colors[group],
              opacity = opacities[group],
              size = 10,
              symbol = "circle",
              line = list(color = "black", width = 1.2)
            ),
            name = group,
            legendgroup = group,
            showlegend = TRUE,
            customdata = ~Interval_Index,
            text = ~paste("Interval Index", Interval_Index),
            inherit = FALSE
          )
      }
      
      # --- Layout ---
      p <- p %>%
        layout(
          xaxis = list(title = "Time (s)", zeroline = FALSE),
          yaxis = list(title = "Δ Video Intensity (AU)"),
          showlegend = TRUE,
          legend = list(
            orientation = "v",
            x = 1.02, y = 1,
            bordercolor = "black", borderwidth = 0.5
          ),
          clickmode = "event"
        )
      
      p
      
      
    })
    
    # ---- Observe: plot click ----
    # Toggle exclude flag on click
    observeEvent(event_data("plotly_click"), {
     
       click <- event_data("plotly_click")
      
      if (!is.null(click) && !is.null(click$customdata)) {
        idx <- click$customdata[[1]]
        selected_ROI <- input$ROI
        
        values$df <- values$df %>%
          mutate(exclude = ifelse(Interval_Index == idx & ROI == selected_ROI, !exclude, exclude))
        }
    })
  
  # ---- Reactive Modelling ----
    fit_model <- reactive({
      
      # ---- Ensure required inputs and data ----
      req(values$df)
      req(input$Astart, input$Bstart, input$ALower, input$BLower, input$AUpper, input$BUpper)
      
      SelectedROI <- input$ROI
      temp <- values$df %>%
        filter(ROI %in% SelectedROI, exclude == FALSE) %>%
        mutate(
          dVI_filt = predict(loess(dVI ~ Time, span = as.numeric(input$loess), data = .))
        )
      
      # ---- Update A limits based on tolerance ----
      tolerance <- input$ATolerance
      updateNumericInput(session, "ALower", value = input$Astart * (1 - tolerance/100))
      updateNumericInput(session, "AUpper", value = input$Astart * (1 + tolerance/100))
      
      # ---- Initialize outputs ----
      my_list <- list()
      
      # ---- Initialize empty tidy dataframe ----
      tidyed <- tibble(
        parameter = character(0),
        value     = numeric(0),
        std.error = numeric(0),
        t.value   = numeric(0),
        p.value   = numeric(0)
      )
      
      # ---- Wash-in model fit ----
      if("Model" %in% input$Analysis_Options){
        fitted <- tryCatch({
          washin_fit(
            temp,
            dVI  = "dVI",
            Time = "Time",
            A    = as.numeric(input$Astart),
            B    = as.numeric(input$Bstart),
            lower = c(A = as.numeric(input$ALower), B = as.numeric(input$BLower)),
            upper = c(A = as.numeric(input$AUpper), B = as.numeric(input$BUpper))
          )
        }, error = function(e) NULL)
        
        if(!is.null(fitted)){
          # --- Predicted values ---
          Time <- seq(0, 8, by = 0.1)
          predicted_df <- data.frame(Time, dVI_predict = predict(fitted, list(Time = Time)))
          
          # --- Augmented data for diagnostics ---
          augmented <- broom::augment(fitted, temp) %>%
            mutate(dVI_filt = predict(loess(dVI ~ Time, span = as.numeric(input$loess), data = temp)))
          
          # --- Tidy model parameters ---
          model_tidy <- broom::tidy(fitted) %>%
            mutate(
              parameter = term,
              value     = estimate,
              std.error = std.error,
              t.value   = statistic,
              p.value   = p.value
            ) %>%
            select(parameter, value, std.error, t.value, p.value)
          
          tidyed <- bind_rows(tidyed, model_tidy)
          
          # --- Add A*B product if applicable ---
          if(nrow(model_tidy) >= 2){
            tidyed <- bind_rows(
              tidyed, format_parameter("AB", model_tidy$value[1] * model_tidy$value[2])
              )
          }
          
          # --- Add R-squared ---
          r2 <- nlsLM_rsquared(augmented$dVI, augmented$`.fitted`)
          if(!is.null(r2)){
            tidyed <- bind_rows(
              tidyed, format_parameter("R-squared", r2)
              )
          }
          
          # ---- AUC Predicted if requested ----
          if("AUCpredict" %in% input$Analysis_Options){
            AUC <- AUC(predicted_df$Time ,predicted_df$dVI_predict)
            tidyed <- bind_rows(
              tidyed, format_parameter("AUC dVI_predict", AUC)
            )
          }
          
          # ---- Max Diff Predicted dvi if requested ----
          if("maxdiffpredict" %in% input$Analysis_Options){
            maxdiff <- max_diff(predicted_df$Time ,predicted_df$dVI_predict)
            tidyed <- bind_rows(
              tidyed, format_parameter("MaxDiff dVI_predict", maxdiff)
            )
          }
          
          # ---- Mean dVI Predicted if requested ----
          if("MeandVIpredicted" %in% input$Analysis_Options){
            mean_dvi <- mean(predicted_df$dVI_predict, na.rm = TRUE)
            tidyed <- bind_rows(
              tidyed, format_parameter("Mean dVI_predict", mean_dvi)
            )
          }
          
          # ---- Save model outputs ----
          my_list[["model"]]     <- fitted
          my_list[["predicted"]] <- predicted_df
          my_list[["augmented"]] <- augmented
        }
      }
      
      # ---- Mean dVI if requested ----
      if("MeandVI" %in% input$Analysis_Options){
        mean_dvi <- mean(temp$dVI, na.rm = TRUE)
        tidyed <- bind_rows(
          tidyed, format_parameter("Mean dVI", mean_dvi)
        )
      }
      
      # ---- Mean filtered dVI if requested ----
      if("MeandVIfilt" %in% input$Analysis_Options){
        mean_dvi <- mean(temp$dVI_filt, na.rm = TRUE)
        tidyed <- bind_rows(
          tidyed, format_parameter("Mean dVI_filt", mean_dvi)
        )
      }
      
      # ---- Peak dVI if requested ----
      if("PeakdVI" %in% input$Analysis_Options){
        peak_dvi <- peak_value(temp$dVI)
        tidyed <- bind_rows(
          tidyed, format_parameter("Peak dVI", peak_dvi)
        )
      }
      
      # ---- Peak filtered dVI if requested ----
      if("PeakdVIfilt" %in% input$Analysis_Options){
        peak_dvi <- peak_value(temp$dVI_filt)
        tidyed <- bind_rows(
          tidyed, format_parameter("Peak dVI_filt", peak_dvi)
        )
      }
      
      # ---- AUC if requested ----
      if("AUC" %in% input$Analysis_Options){
        AUC <- AUC(temp$Time ,temp$dVI_filt)
        tidyed <- bind_rows(
          tidyed, format_parameter("AUC dVI_filt", AUC)
        )
      }
      
      # ---- Max Diff filtered dvi if requested ----
      if("Maxdiff" %in% input$Analysis_Options){
        maxdiff <- max_diff(temp$Time ,temp$dVI_filt)
        tidyed <- bind_rows(
          tidyed, format_parameter("MaxDiff dVI_filt", maxdiff)
        )
      }
      
      # ---- Save tidyed dataframe ----
      my_list[["tidyed"]] <- tidyed
      
      # ---- Return list for plotting / table display ----
      my_list
    })

    
# easy button to set anaklysis options to all      
observeEvent(input$select_all_analysis, {
        if (input$select_all_analysis) {
          updateSelectizeInput(session, "Analysis_Options", 
                               selected = c("AUC", "AUCpredict", "PeakdVI", "PeakdVIfilt",
                                            "Maxdiff", "maxdiffpredict", "MeandVI", 
                                            "MeandVIfilt", "MeandVIpredicted", "Model"))
        } else {
          updateSelectizeInput(session, "Analysis_Options", selected = NULL)
        }
      })
    

output$model_params <- DT::renderDataTable({
  fitted <- fit_model()
  req(fitted)  # ensures 'fitted' is not NULL or invalid
  
  # Ensure fitted$tidy exists and is non-empty
  if (is.null(fitted$tidy) || nrow(fitted$tidy) == 0) {
    return(NULL)}
  
  fitted$tidy %>%
    DT::datatable(options = list(pageLength = 10),
                  filter = "top",
                  caption = "Output Parameters") %>%
        DT::formatRound(columns = "value", digits = 2) %>%
        DT::formatRound(columns = "std.error", digits = 2) %>%
        DT::formatRound(columns = "t.value", digits = 1) %>%
        DT::formatRound(columns = "p.value", digits = 4)
})


        # ---- Button observer → store current results ----
    observeEvent(input$save_fit, {
      # Stop if analysis options are empty
      req(input$Analysis_Options)   # <-- make sure the input ID matches your UI
      
      # Grab evaluated results
      fitted_val <- isolate(fit_model())
      req(fitted_val)
      
      # Label for this ROI
      run_id <- paste0(input$ROI)
      
      # Save result (overwrite any previous for this ROI)
      values$results_list[[run_id]] <- fitted_val
      
      # Notify user
      showNotification(
        paste("Results saved for ROI:", run_id),
        type = "message"
      )
    })



    # ---- Reactive table of all saved ROI results ----
    all_results <- reactive({
      req(values$results_list)
      
      temp <- values$results_list
      
      Augmented <- map_dfr(
        names(temp), 
        ~{ROI <- .x
          augmented <- temp[[ROI]]$augmented
          if (!is.null(augmented) && nrow(augmented) > 0) {
            augmented %>% mutate(ID = input$ID, ROI = ROI) %>% relocate(ID, ROI)
          } else {
            NULL
          }
        }
      )
      
      Predicted <- map_dfr(
        names(temp), 
        ~{ROI <- .x
        predicted <- temp[[ROI]]$predicted
        if (!is.null(predicted) && nrow(predicted) > 0) {
          predicted %>% mutate(ID = input$ID, ROI = ROI) %>% relocate(ID, ROI)
        } else {
          NULL
        }
        }
      )
      
      Tidyed <- map_dfr(
        names(temp), 
        ~{ROI <- .x
        tidyed <- temp[[ROI]]$tidyed
        if (!is.null(tidyed) && nrow(tidyed) > 0) {
          tidyed %>% mutate(ID = input$ID, ROI = ROI) %>% relocate(ID, ROI)
        } else {
          NULL
        }
        }
      )
      
      list("Augmented" = Augmented, "Predicted" = Predicted, "Tidyed" = Tidyed)
      
    })
  
  
  
      # ---- Render datatable ----
    output$resultsTable <- DT::renderDataTable({
      req(all_results())
      
      selected_data <- input$selected_analysis
      
      temp <- all_results()[[selected_data]]
      
      DT::datatable(
        temp,
        rownames = FALSE,
        extensions = "Buttons",
        filter = "top",  # enables column-wise filters
        options = list(
          dom = "Bfrtip",   # lowercase letters: f=global search, r=processing, t=table, i=info, p=paging
          buttons = c("csv"), 
          pageLength = -1,
          searching = TRUE   # optional, enables global search
        )
      )
    })


    # ---- Observe: reset ----
    observeEvent(input$reset, {
      #browser()
      # reset values exclude to FALSE
      SelectedROI <- input$ROI
      df <- values$df %>%
        mutate(exclude = ifelse(ROI == SelectedROI, FALSE, exclude))
      values$df <- df
      
      #reset model parameter start values
      # Determine A Value start
      peakvalue <- peak_value(df$dVI)
      updateNumericInput(session, "Astart", value = peakvalue)
      
      # set Lower limit
      tolerance <- input$ATolerance
      updateNumericInput(session, "ALower", value = peakvalue*(1-(tolerance)/100))
      # set Upper limit
      updateNumericInput(session, "AUpper", value = peakvalue*(1+(tolerance)/100))
      
    })
    
    
    # ---- Downloadable csv of selected dataset ----
output$save <- downloadHandler(
  filename = function() {
    paste0(tools::file_path_sans_ext(input$NarNar$name), "-CEUS.zip")
  },
  content = function(file) {
    # go to a temp dir to avoid permission issues
    owd <- setwd(tempdir())
    on.exit(setwd(owd))

    req(values$results_list)
    temp <- values$results_list

    Augmented <- map_dfr(
      names(temp),
      ~{
        ROI <- .x
        augmented <- temp[[ROI]]$augmented
        if (!is.null(augmented) && nrow(augmented) > 0) {
          augmented %>%
            mutate(ID = input$ID, ROI = ROI) %>%
            relocate(ID, ROI)
        } else {
          NULL
        }
      }
    )

    Predicted <- map_dfr(
      names(temp),
      ~{
        ROI <- .x
        predicted <- temp[[ROI]]$predicted
        if (!is.null(predicted) && nrow(predicted) > 0) {
          predicted %>%
            mutate(ID = input$ID, ROI = ROI) %>%
            relocate(ID, ROI)
        } else {
          NULL
        }
      }
    )

    Tidyed <- map_dfr(
      names(temp),
      ~{
        ROI <- .x
        tidyed <- temp[[ROI]]$tidyed
        if (!is.null(tidyed) && nrow(tidyed) > 0) {
          tidyed %>%
            mutate(ID = input$ID, ROI = ROI) %>%
            relocate(ID, ROI)
        } else {
          NULL
        }
      }
    )

    # filenames
    base <- tools::file_path_sans_ext(input$NarNar$name)
    fileName <- c(
      paste0(base, "-signals.csv"),
      paste0(base, "-predicted.csv"),
      paste0(base, "-parameters.csv")
    )

    # write CSVs
    write_csv(Augmented, fileName[1])
    write_csv(Predicted, fileName[2])
    write_csv(Tidyed, fileName[3])

    # zip them
    zip(file, fileName)
  }
)


}

# Create Shiny app ----
thematic::thematic_shiny()
shinyApp(ui, server)
