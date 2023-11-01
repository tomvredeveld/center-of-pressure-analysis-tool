######### Libraries ############
library(shiny)
library(data.table)
library(signal)
library(plotly)

# Version: 1.0.0
# Author: T. Vredeveld (GitHub: https://github.com/tomvredeveld)
# Version management can be found here: https://github.com/tomvredeveld/center-of-pressure-analysis-tool

########### UI #################
ui <- fluidPage(
  titlePanel("Center of Pressure Analysis Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV or TXT File", accept = c(".csv", ".txt")),
      numericInput(inputId = "input_timestamp_one",
                   label = "First timestamp to calculate CoP Parameters",
                   value = NA, min = 0),
      numericInput(inputId = "input_timestamp_two",
                   label = "Second timestamp to calculate CoP Parameters",
                   value = NA, min = 0),
      actionButton("go", "Calculate COP Parameters!", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions",
                 br(),
                 h2("Instructions"),
                 hr(),
                 p("Here, you will find some instructions to calculate Center of
                 Pressure parameters using this calculator."),
                 h3("Step 1: Upload data"),
                 hr(),
                 p("First, upload your data as a", code(".txt"), "or", code(".csv"),
                   "file on the left. 
                   Make sure the first three columns in the file are:"),
                 p(code("time"),
                   "containing a time series"),
                 p(code("copx"),
                   "medio-lateral direction COP data column and"),
                 p(code("copy"), "antero-posterior direction COP data column."),
                 p("The names of the columns do not need to match. Other 
                      columns can be within the file but will be ignored for the calculations. 
                      There is no need to clean up the data yourself."), 
                 h3("Step 2: Look at the signals"), 
                 hr(),
                 p("Use the tabs above: table, to have a quick look at the data and see if the data was uploaded correctly,
                      plotly, to inspect a medio-lateral (upper panel) or antero-posterior (lower pannel) signal."), 
                 h3("Step 3: Segment selection"),
                 hr(),
                 p("Hover over either the upper or lower panel of the plots and fill in the values as a 
                    timestamp the sidebar to the left to select a segment of the total signal. The app also
                   calculates the COP parameters if a non-existing timestamp was entered, for example due to a typo.
                   It then selects the nearest available point in time"),
                 h3("Step 4: Press Calculate"),
                 hr(),
                 p("Now you will find a range of COP parameters, reported at the COP parameters tab. 
                   At the tab Sway Area you may find plots that show the postural sway of the individual during the 
                   selected segment."),
        ),
        tabPanel("Table", 
                 br(), 
                 h2("Instructions"),
                 hr(),
                 p("Here you can find a table with the first 6 rows of your data, to see if the upload was succesful"),
                 h3("Table"), 
                 tableOutput("table")),
        tabPanel("Plotly", 
                 br(), 
                 h2("Instructions"),
                 hr(),
                 p("Beneath you will find a plot of your data, consisting of two panels,
                   the upper being the COP on the x-axis, or medio-lateral direction,
                   while the second panel shows the COP on the y-axis, or antero-posterior direction"), 
                 h3("Plot"),
                 hr(),
                 plotlyOutput("plot")),
        tabPanel("CoP Parameters", 
                 br(), 
                 h2("Instructions"),
                 hr(),
                 p("The table below shows the output of generic COP parameters of the selected segment from the two
                   timestamps you entered on the left. If no table is shown, please check your timestamps and make sure
                   these are values taken from the Plotly tab, using your mouse to hover over the signals."), 
                 h3("Table of COP parameters of the selected segment"),
                 tableOutput("cop_table")),
        tabPanel("Sway Area", 
                 br(), 
                 h2("Instructions"),
                 hr(),
                 p("Below, you may find two plots, the left being a 95% Predicted Ellipse Area, layed on top of the data
                   from the segment you selected using the timestamps taken from the Plotly tab you entered on the left.
                   The right plot shows pathlength on a wider coordinated figure including centered x and y lines.
                   If no figures are shown, please check your timestamps and make sure
                   these are values taken from the Plotly tab, using your mouse to hover over the signals."),
                 h3("Plot of Sway Area"),
                 hr(),
                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sway_area_1"), plotlyOutput("sway_area_2"))),
        tabPanel("About", 
                 br(),
                 h2("About"),
                 hr(),
                 p("Author: Tom Vredeveld"),
                 h3("Version"),
                 hr(),
                 p("1.0.0 – First public version"),
                 p("0.1.0 - Private beta"),
                 h3("COP Data filter"),
                 hr(),
                 p("The data are filtered at 10Hz, using a low-pass zero-lag 4th order Butterworth filter."),
                 h3("Disclaimer"),
                 hr(),
                 p("The author makes no representations or warranties of any kind,
                   express or implied, regarding the accuracy, reliability, 
                   or completeness of any information provided through this 
                   Center of Pressure Analysis Tool."),
                 h3("Privacy"),
                 hr(),
                 p("Please be aware that your data is sent to shinyapps.io as calculations are performed server-side.
                    Be careful with sensitive data! Otherwise, run the app locally from within RStudio
                   Head for instructions to:"),
                 a(href = "https://github.com/tomvredeveld/center-of-pressure-analysis-tool",
                   "https://github.com/tomvredeveld/center-of-pressure-analysis-tool"),
                 h3("Licence"),
                 hr(),
                 p("The code of this app is registered under a MIT licence at GitHub (url-to-be-updated)
                    and can be downloaded to run locally from a computer with R and RStudio. 
                    It is provided as a RShiny app, which is hosted for free at shinyapps.io,
                    the downside here being a limited bandwith. 
                    If you wish to frequently use this app, please be adviced to run it locally on your computer.
                    More info can be found here."),
                 a(href = "https://github.com/tomvredeveld/center-of-pressure-analysis-tool",
                   "https://github.com/tomvredeveld/center-of-pressure-analysis-tool")),
                 hr(),
      )
    )
  )
)

########### SERVER #################
server <- function(input, output) {
  
  ## prerequisites
  # 95% Predicted Area Ellipse calculations
  # Based off the works from P. Schuber and M. Kirchner 2014 (with permission translated to R)
  # Find code here: https://github.com/tomvredeveld/predicted-ellipse-area 
  # http://dx.doi.org/10.1016/j.gaitpost.2013.09.001
  pea <- function(copx, copy, probability = 0.95){
    
    # Set up libraries.
    library(PEIP)
    library(ggplot2)
    
    # Create list to export calculations to.
    pea <- list(area = NULL, eigenvectors = NULL, eigenvalues = NULL, plot = NULL)
    
    # Calculate inverse of chi-square cumulative distribution function, eigenvalues and area.
    chisquare <- chi2inv(probability, 2) 
    x <- copx[is.finite(copx)]
    y <- copy[is.finite(copy)]
    mx <- mean(x)
    my <- mean(y) 
    vec_val <- eigen(cov(matrix(c(x, y), ncol = 2)))
    val <- matrix(0, nrow = 2, ncol = 2)
    val[1,1] <- vec_val$values[2]
    val[2,2] <- vec_val$values[1] # place values at the right place.
    rotate  <- function(x, clockwise = TRUE) {
      if (clockwise) { t( apply(x, 2, rev))
      } else {apply( t(x),2, rev)} 
    } # Took this function from user: "bud.dugong" @ website: https://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r-by-90-degrees-clockwise
    vec <- rotate(vec_val$vectors, clockwise = FALSE) #rotate to match matlab vectors matrix
    pea$area <- pi * chisquare * prod(sqrt(svd(val)$d))
    pea$eigenvectors <- vec
    pea$eigenvalues <- val
    
    # Create Plot: dataframe
    df_xy <- data.frame(cbind(copx = copx, copy = copy))
    
    # Create Plot: ellipse
    N <- 100 # fixed number, higher creates a smoother ellipse.
    t <- seq(from = 0, to = 2*pi, length.out = N)
    ellipse <- sqrt(chisquare) * vec %*% sqrt(val) %*% 
      t(as.matrix(data.frame(cos_t = cos(t), sin_t = sin(t)))) + 
      kronecker(matrix(1, 1, N), c(mx, my))
    df_ellipse <- data.frame(t(ellipse))
    names(df_ellipse) <- c("x", "y")
    
    # Create Plot: minor and major axis
    ax1 <- sqrt(chisquare) * vec %*% sqrt(val) %*% as.matrix(rbind(c(-1, 1), c(0, 0))) + kronecker(matrix(1, 1, 2), c(mx, my))
    ax2 <- sqrt(chisquare) * vec %*% sqrt(val) %*% as.matrix(rbind(c(0,0), c(-1, 1))) + kronecker(matrix(1, 1, 2), c(mx, my))
    df_axis <- as.data.frame(rbind(t(ax1), c(NA, NA), t(ax2)))
    names(df_axis) <- c("x", "y")
    
    # Draw plot using ggplot2
    pea$plot1 <- ggplot()+
      geom_point(data = df_xy, aes(x = copx, y = copy), colour = "blue", shape = 3)+
      geom_path(data = df_ellipse, aes(x = x, y = y), colour = "red", linewidth = 0.2)+
      geom_path(data = df_axis, aes(x = x, y = y), colour = "red", linewidth = 0.2)+
      theme_classic()
    
    pea$plot2 <- ggplot()+
      geom_path(data = df_xy, aes(x = copx, y = copy), colour = "blue", linewidth = 0.2)+
      geom_path(data = df_ellipse, aes(x = x, y = y), colour = "red", linewidth = 0.2)+
      geom_path(data = df_axis, aes(x = x, y = y), colour = "red", linewidth = 0.2)+
      geom_hline(yintercept = 0, colour = "black", linewidth = 0.2)+
      geom_vline(xintercept = 0, colour = "black", linewidth = 0.2)+
      coord_cartesian(xlim = c(-30, 30), ylim = c(-30, 30))+
      theme_classic()
    
    # Return list with 4 elements.
    return(pea)
  } 
  
  ## Load the data by user input.
  data <- reactive({
    req(input$file)
    # Load data.
    df <- as.data.frame(fread(input$file$datapath, 
                              col.names = c("time", "copx", "copy"), 
                              skip = 1, 
                              select = c(1:3)))
    names(df) <- c("time", "copx", "copy")
    
    # Set butterworth filtering.
    fs_cut <- 10
    fs_wbb <- 40
    bf <- butter(2, (fs_cut / (fs_wbb / 2)), type = "low")
    df[, 2] <- filtfilt(bf, df[, 2])
    df[, 3] <- filtfilt(bf, df[, 3])
    
    return(df)
  })
  
  ## Quicklook table: provides a quick overview if upload/fread was successfully completed.
  output$table <- renderTable({
    quicklook <- head(data())
    return(quicklook)
  })
  
  ## Create a plot to inspect signal. 
  output$plot <- renderPlotly({
    # Plotly settings
    plot1 <- plot_ly(data(), x = ~time, y = ~copx, 
                     type = "scatter", 
                     mode = "lines",
                     hoverinfo = "x",
                     line = list(width = 1),
                     name = "Medio-Lateral COP") %>%
      layout(title = "Center of Pressure Signals & Selection Tool",
             xaxis = list(title = "Time"), #, hoverformat = ".2f"), 
             yaxis = list(title = "COP Medio-Lateral", range = c(-25, 25)),
             showlegend = TRUE)
    plot2 <- plot_ly(data(), x = ~time, y = ~copy, 
                     type = "scatter",
                     mode = "lines",
                     hoverinfo = "x",
                     line = list(width = 1),
                     name = "Antero-Posterior COP") %>%
      layout(title = "Center of Pressure Signals & Selection Tool",
             xaxis = list(title = "Time"), #, hoverformat = ".2f"), 
             yaxis = list(title = "COP Anterior-Posterior", range = c(-25, 25)),
             showlegend = TRUE)
    plot <- subplot(plot1, plot2, nrows = 2)
    return(plot)
  })
  
  # Set timestamp, based on data provided by user.
  timestamp_one <- eventReactive(input$go, {
    df <- data()
    return(which.min(abs(df[, 1] - input$input_timestamp_one)))})
    #match(input$input_timestamp_one, df[, 1])})
  
  # Set timestamp, based on data provided by user.
  timestamp_two <- eventReactive(input$go, { 
    df <- data()
    return(which.min(abs(df[, 1] - input$input_timestamp_two)))})
    #match(input$input_timestamp_two, df[, 1])})
  
  # Set analysis, based on input. 
  analysis <- eventReactive(input$go, { 
    analysis <- list(segment_pea = NULL, cop_table = NULL)
    
    # Take uploaded data
    df <- data()
    
    # Create segment based on timestamps 
    df_segment <- df[c(timestamp_one():timestamp_two()), ]
    
    # Calculate STD's of displacement
    std_copx <- sd(df_segment[, 2], na.rm = TRUE)
    std_copy <- sd(df_segment[, 3], na.rm = TRUE)
    
    # Calculate velocity of displacement
    mcopx <- df_segment[, 2] - mean(df_segment[, 2], na.rm = TRUE)
    mcopy <- df_segment[, 3] - mean(df_segment[, 3], na.rm = TRUE)
    time_diff <- diff(df_segment[, 1])
    mvelo_copx <- mean(abs(diff(mcopx)) / time_diff)
    mvelo_copy <- mean(abs(diff(mcopy)) / time_diff)
    
    # Calculate pathlength by summation of euclidian distances
    pathlength <- sum(sqrt((diff(mcopx)^2 + diff(mcopy)^2)))
    
    # Calculate 95% PEA
    segment_pea <- pea(df_segment[, 2], df_segment[, 3], probability = 0.95)
    
    # Put values in one table to return.
    cop_table <- as.data.frame(matrix(NA, nrow = 6, ncol = 3))
    names(cop_table) <- c("COP Parameter", "Value", "Units")
    cop_parameters <- c("standard deviation of displacement - COP medio-lateral", 
                        "standard deviation of displacement - COP antero-posterior",
                        "mean velocity COP medio-lateral", 
                        "mean velocity CoP antero-posterior",
                        "CoP Pathlength", "CoP 95% PEA")
    cop_table[, 1] <- cop_parameters
    cop_table[, 2] <- c(std_copx, std_copy, mvelo_copx, mvelo_copy, pathlength, segment_pea$area)
    cop_table[, 3] <- c("in centimeter", "in centimeter", "in centimeter per second", "in centimeter per second",
                        "in centimeters", "in cm2")
    
    # Add objects to a list.
    analysis$cop_table <- cop_table
    analysis$segment_pea <- segment_pea
    
    # Return element.
    return(analysis)
  })
  
  ## Output COP parameters table
  output$cop_table <- renderTable({
    analysis_data <- analysis()
    return(analysis_data$cop_table)
  })
  
  ## Output COP Sway plot 1
  output$sway_area_1 <- renderPlotly({
    analysis_data <- analysis()
    segment_pea <- analysis_data$segment_pea
    plot1 <- ggplotly(segment_pea$plot1)
    return(plot1)
  })
  
  ## Output COP Sway plot 2
  output$sway_area_2 <- renderPlotly({
    analysis_data <- analysis()
    segment_pea <- analysis_data$segment_pea
    plot2 <- ggplotly(segment_pea$plot2)
    return(plot2)
  })
}

shinyApp(ui, server)