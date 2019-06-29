library(shiny)
library(shinyjs)
library(ggplot2)
##library(devtools)
##devtools::install_git("https://github.com/highamm/FPBKPack2.git")
library(FPBKPack2)

## library(rsconnect)
## rsconnect::deployApp('~/Desktop/FPBKShiny/FPBKShinyApp')

## next steps:
## 1.) Test the shiny app using moose data
## 2.) Change automatic updating to a submit button


#detectionvar <- rbinom(40, 1, 0.6)
#predictor1 <- runif(40, 0, 1)
#predictor2 <- runif(40, -2, 2)
#radiocollartab <- cbind(detectionvar, predictor1, predictor2)
#radiocollardataset <- data.frame(radiocollartab)
#write.csv(radiocollardataset, "exampledetection.csv")

#exampledataset$stratvar <- c(rep("High", 23),
#  rep("Low", 17))
#write.csv(exampledataset, "exampledataset.csv", append = FALSE)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("GSPE Moose Prediction Application"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        conditionalPanel(condition = "input.tabs1==1",
        fileInput("file1", "Choose CSV File with Count Data",
          multiple = TRUE,
          accept = c("text/csv",
            "text/comma-separated-values,text/plain",
            ".csv")),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        # Input: Select separator ----
        radioButtons("sep", "Separator",
          choices = c(Comma = ",",
            Semicolon = ";",
            Tab = "\t"),
          selected = ","),
        
        # Input: Select quotes ----
    #    radioButtons("quote", "Quote",
    #      choices = c(None = "",
    #        "Double Quote" = '"',
    #        "Single Quote" = "'"),
    #      selected = '"'),
        
        # Horizontal line ----
    #    tags$hr(),
        
        # Input: Select number of rows to display ----
     #   radioButtons("disp", "Display",
    #      choices = c(Head = "head",
    #        All = "all"),
    #      selected = "head"),
        
        selectInput('resp', 'Select Response:', ""),
        checkboxGroupInput('preds', 'Select Predictors:', ""),
        selectInput('xcoords', 'Select X-coords:', ""),
        selectInput('ycoords', 'Select Y-coords:', ""),
        selectInput('strat', 'Select Stratification:', ""),
        selectInput('area', 'Select Area Column:', ""),
        numericInput('detection',
          'Enter Estimated Detection:', value = 1,
          min = 0, max = 1),
        numericInput('SEdetection',
          'Enter Standard Error for Detection:', value = 0,
          min = 0, max = Inf),
        radioButtons("latlon", "Latitude / Longitude?",
          choices = c(Yes = "Yes",
            No = "No"),
          selected = "2"),
        actionButton("go", "Submit"),
          
        
        actionButton("genreport", "Get Report"),
        actionButton("gendataframe", "Get Prediction Data")
        
        
      ),
        conditionalPanel(condition = "input.tabs1==2"),
        
        conditionalPanel(condition = "input.tabs1==3",
        fileInput("file2", "Choose CSV File with Radiocollar Data",
          multiple = TRUE,
          accept = c("text/csv",
            "text/comma-separated-values,text/plain",
            ".csv")),
        checkboxInput("headerdet", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons("sepdet", "Separator",
          choices = c(Comma = ",",
            Semicolon = ";",
            Tab = "\t"),
          selected = ","),
          
          selectInput('detectionresp',
            'Select Detection Column:', ""),
          checkboxGroupInput('detectionpreds',
            'Select Detection Predictors:', ""),
          actionButton("godetection", "Submit")
          
          
          
          )
      
        # Input: Select number of rows to display ----
     #   radioButtons("disp", "Display",
    #      choices = c(Head = "head",
    #        All = "all"),
    #      selected = "head"))
      ),
   
      # Show a plot of the generated distribution
     mainPanel(
       tabsetPanel(id = "tabs1",
         tabPanel("GSPE", value = 1, tableOutput("contents"),
         #  tableOutput("testsomething"),
           verbatimTextOutput("summary"),
           plotOutput("krigmap"),
           plotOutput("variogramplot")),
         tabPanel("Information", value = 2,
           h3("Basic Information"),
           "To run the application, use the following steps. There is also more detail about each step in the accompanying Vignette",
           tags$ol(
             tags$li("Upload data set from your local computer. The data set should be a .csv file and contain information on both the sampled and unsampled sites. The minimum that the data set should have are a column of observed counts, x-coordinates, y-coordinates, and any strata or other predictors."), 
             tags$li("If you do not have radiocollar data, then skip this step. If you do have radiocollar data, click the Radiocollar Sightability tab at the top center of the webpage. Upload your radiocollar data from a .csv file, which should, at minimum contain a column for whether or not each radiocollared animal was sighted. Choose the detection column and any relevant predictors for detection and then click the Submit button. If the informal output looks appropriate and there are not error messages, go back to the GSPE tab and continue with the remainder of the steps."),
             tags$li("Select the appropriate columns for the counts, coordinates, and predictors from the drop-down menus."), 
             tags$li("If you have an estimate for mean detection and a standard error, put in this estimate here. Note that this is separate from the sightability trials: if you've uploaded sightability data, then the model is already accounting for imperfect detection and these can be left as is! Uploading sightability data and putting in values for mean detection result in adjusting for sightability twice, which is not what we want. See the accompanying vignette for more information."),
             tags$li("Click the Submit button and verify that the information given in the informal R output seems reasonable."),
             tags$li("Click the Get Report button for an HTML report that can be saved as a PDF or the Get Prediction Data button for the data set in a that was originally uploaded with site-by-site predictions and a few other columns of information appended in a .csv format.")
           ),
           h3("Information about Stratification"),
           "Stratum can be included as a predictor in the spatial model by choosing the column with stratum in the Select Predictors part of the app. Doing so fits a a single spatial linear model, assuming that all sites have the same spatial structure with different means. Alternatively, stratum can be unchecked as a predictor but chosen in the Select Stratification option. Doing so fits a model for each stratum, allowing strata to have different covariance structures. The assumption we make with this model is that the strata are not cross-correlated. For more information about each of these options, see the accompanying Vignette."),
         tabPanel("Radiocollar Sightability", value = 3,
           tableOutput("radiocontents"),
           verbatimTextOutput("radiosummary"))
         )
     )
   )
)

server <- function(input, output, session) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
  
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
      header = input$header,
      sep = input$sep)#,
     # quote = input$quote)
    

    
 #   if(input$disp == "head") {
      return(head(df))
#    }
#    else {
#      return(df)
#    }
  
  })
  
  datare <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
      header = input$header,
      sep = input$sep)#,
     # quote = input$quote)
    
    updateSelectInput(session, inputId = 'resp', label = 'Response',
      choices = names(df))
    ##, selected = names(df)[1])
    updateCheckboxGroupInput(session, inputId = 'preds',
      label = "Predictors",
      choices = c("None", names(df)), selected = "None")
    updateSelectInput(session, inputId = 'xcoords',
      label = 'X-coordinate column',
      choices = names(df))
    updateSelectInput(session, inputId = 'ycoords',
      label = 'Y-coordinate column',
      choices = names(df))
    updateSelectInput(session, inputId = 'strat',
      label = 'Stratification column',
      choices = c("None", names(df)), selected = "None")
    updateSelectInput(session, inputId = 'area',
      label = 'Area column',
      choices = c("None", names(df)), selected = "None")
    
    return(df)
    
    ##lmobj <- lm(input$resp ~ 1, na.rm = TRUE, data = df)
    ##print(summary(lmobj))
  })
  
  df_test = reactive({
    df_new = datare()
    if(input$latlon == "Yes") {
      df_new$xcoordcol22 <- LLtoUTM(cm = base::mean(datare()[ ,input$xcoords]),
        lat = datare()[ ,input$ycoords],
        lon = datare()[ ,input$xcoords])$xy[ ,1]
      
      
    } else if (input$latlon == "No") {
      df_new$xcoordcol22 <- datare()$input$xcoords
    }
    df_new
  })
  
 # lmod <- reactive({ 
#    mod1 <- lm(input$resp ~ 1, data = values$df)
#  })
 # test <- c("fdafs", "dfaf")
#  test == "faaf"
  
 # testdf <- data.frame(x = c(1, 2), y = c(2, 3))
#  testdf[ ,"x"]
#  prednames <- list("x", "y")
#  testdf[ ,unlist(prednames)]

 # testsomething <- reactive({
#    req(input$file1)
    #radiocollardataout <- modelfitradiocollar()
#    datare()
    
#    sum(input$strat == "None")
#  })
  
#  output$testoutput <- renderTable({
#    print(testsomething())
#  })
  

  modelfit <- reactive({
    req(input$file1)
    #radiocollardataout <- modelfitradiocollar()

   datare()
   
   if (input$go == 0) {
     return()
   } else { 
     
     ## isolate wraps this so that it doesn't automatically
     ## update after submit is hit for the first time
    isolate(if (sum(input$strat == "None") >= 1) {
      
      formtouse <- as.formula(paste(input$resp, "~",
        paste(input$preds, collapse="+"), sep = ""))
      
      if (sum(input$preds == "None") >= 1) {
        # fit <- lm(datare()[ ,input$resp] ~ 1)
        
        if (sum(input$area == "None") >= 1) {
          fit <- slmfit(formula = as.formula(paste(input$resp, "~",
            1, sep = "")),
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = NULL,
            areacol = NULL)
          
        } else {
          fit <- slmfit(formula = as.formula(paste(input$resp, "~",
            1, sep = "")),
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = NULL,
            areacol = input$area)
        }
        
        predfit <- predict(fit, FPBKcol = NULL,
          detinfo = c(input$detection, input$SEdetection))
        
        predout <- FPBKoutput(pred_info = predfit,
          conf_level = c(0.80, 0.90, 0.95),
          get_krigmap = TRUE, get_sampdetails = TRUE,
          get_variogram = TRUE,
          nbreaks = 4,
          breakMethod = 'quantile', 
          pointsize = 3)
        
        
      } else {
        
        #   fit <- lm(as.formula(paste(input$resp, "~",
        #      paste(input$preds, collapse="+"), sep = "")), data = datare())
        
        if (sum(input$area == "None") >= 1) {
          fit <- slmfit(formula = formtouse,
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = NULL,
            areacol = NULL)
        } else {
          fit <- slmfit(formula = formtouse,
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = NULL,
            areacol = input$area)
        }
        
        predfit <- predict(fit, FPBKcol = NULL,
          detinfo = c(input$detection, input$SEdetection))
        
        predout <- FPBKoutput(pred_info = predfit,
          conf_level = c(0.80, 0.90, 0.95),
          get_krigmap = TRUE, get_sampdetails = TRUE,
          get_variogram = TRUE,
          nbreaks = 4,
          breakMethod = 'quantile', 
          pointsize = 3)
      }
    } else {
      
      formtouse <- as.formula(paste(input$resp, "~",
        paste(input$preds, collapse="+"), sep = ""))
      
      if (sum(input$preds == "None") >= 1) {
        
        if (sum(input$area == "None") >= 1) {
          multiobj <- multistrat(formula = as.formula(paste(input$resp, "~",
            1, sep = "")),
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = NULL,
            detinfo = c(input$detection, input$SEdetection),
            areacol = NULL,
            stratcol = input$strat)
        } else {
          multiobj <- multistrat(formula =
              as.formula(paste(input$resp, "~",
                1, sep = "")),
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = NULL,
            detinfo = c(input$detection, input$SEdetection),
            areacol = input$area,
            stratcol = input$strat)
        }
        
      } else {
        
        if (sum(input$area == "None") >= 1) {
          
          multiobj <- multistrat(formula = formtouse,
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = NULL,
            detinfo = c(input$detection, input$SEdetection),
            areacol = NULL,
            stratcol = input$strat)
        } else {
          multiobj <- multistrat(formula = formtouse,
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = NULL,
            detinfo = c(input$detection, input$SEdetection),
            areacol = input$area,
            stratcol = input$strat)
        }
      }
    })
   }
  })

  
  
  
  
  
  
  
  
  modelfit2 <- reactive({
    req(input$file1)
    req(input$file2)
    #radiocollardataout <- modelfitradiocollar()
    
    datare()
    
    if (input$godetection == 0 | input$go == 0) {
      return()
    } else {
 
    isolate(if (sum(input$strat == "None") >= 1) {
      
      formtouse <- as.formula(paste(input$resp, "~",
        paste(input$preds, collapse="+"), sep = ""))
      
      if (sum(input$preds == "None") >= 1) {
        # fit <- lm(datare()[ ,input$resp] ~ 1)
        
        if (sum(input$area == "None") >= 1) {
          fit <- slmfit(formula = as.formula(paste(input$resp, "~",
            1, sep = "")),
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = modelfitradiocollar(),
            areacol = NULL)
          
        } else {
          fit <- slmfit(formula = as.formula(paste(input$resp, "~",
            1, sep = "")),
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = modelfitradiocollar(),
            areacol = input$area)
        }
        
        predfit <- predict(fit, FPBKcol = NULL,
          detinfo = c(input$detection, input$SEdetection))
        
        predout <- FPBKoutput(pred_info = predfit,
          conf_level = c(0.80, 0.90, 0.95),
          get_krigmap = TRUE, get_sampdetails = TRUE,
          get_variogram = TRUE,
          nbreaks = 4,
          breakMethod = 'quantile', 
          pointsize = 3)
        
        
      } else {
        
        #   fit <- lm(as.formula(paste(input$resp, "~",
        #      paste(input$preds, collapse="+"), sep = "")), data = datare())
        
        if (sum(input$area == "None") >= 1) {
          fit <- slmfit(formula = formtouse,
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = modelfitradiocollar(),
            areacol = NULL)
        } else {
          fit <- slmfit(formula = formtouse,
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = modelfitradiocollar(),
            areacol = input$area)
        }
        
        predfit <- predict(fit, FPBKcol = NULL,
          detinfo = c(input$detection, input$SEdetection))
        
        predout <- FPBKoutput(pred_info = predfit,
          conf_level = c(0.80, 0.90, 0.95),
          get_krigmap = TRUE, get_sampdetails = TRUE,
          get_variogram = TRUE,
          nbreaks = 4,
          breakMethod = 'quantile', 
          pointsize = 3)
      }
    } else {
      
      formtouse <- as.formula(paste(input$resp, "~",
        paste(input$preds, collapse="+"), sep = ""))
      
      if (sum(input$preds == "None") >= 1) {
        
        if (sum(input$area == "None") >= 1) {
          multiobj <- multistrat(formula = as.formula(paste(input$resp, "~",
            1, sep = "")),
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = modelfitradiocollar(),
            detinfo = c(input$detection, input$SEdetection),
            areacol = NULL,
            stratcol = input$strat)
        } else {
          multiobj <- multistrat(formula =
              as.formula(paste(input$resp, "~",
                1, sep = "")),
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = modelfitradiocollar(),
            detinfo = c(input$detection, input$SEdetection),
            areacol = input$area,
            stratcol = input$strat)
        }
        
      } else {
        
        if (sum(input$area == "None") >= 1) {
          
          multiobj <- multistrat(formula = formtouse,
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = modelfitradiocollar(),
            detinfo = c(input$detection, input$SEdetection),
            areacol = NULL,
            stratcol = input$strat)
        } else {
          multiobj <- multistrat(formula = formtouse,
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            detectionobj = modelfitradiocollar(),
            detinfo = c(input$detection, input$SEdetection),
            areacol = input$area,
            stratcol = input$strat)
        }
      }
    }
    )}
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  filedata <- reactive({
    infile <- input$file2
    if (is.null(infile)){
      return(NULL)      
    }
    read.csv(infile$datapath)
  })
  
  output$summary <- renderPrint({
      if (is.null(filedata()) == TRUE) {
      print(modelfit())
      } else {
        print(modelfit2())
      }
    #  print(prednames)
  })
  
  output$krigmap <- renderPlot({
    if (is.null(filedata()) == TRUE) {
    modelfit()$krigmap
    } else {
      modelfit2()$krigmap
    }
  })
  
  output$variogramplot <- renderPlot({
    if (is.null(filedata()) == TRUE) {
    modelfit()$varplot
    } else {
      modelfit2()$varplot
    }
  })
  
  observeEvent(input$genreport, {
    if (is.null(filedata()) == TRUE) {
    get_reportdoc(modelfit())
    } else {
      get_reportdoc(modelfit2())
    }
   # session$sendCustomMessage(type = 'testmessage',
  #    message = 'Thank you for clicking')
  })
  
  observeEvent(input$gendataframe, {
    dout <- "~/Desktop/"
    if (is.null(filedata()) == TRUE) {
    write.csv(modelfit()$predvals, file = paste(dout,
      "dataset.csv", sep = ""))
    } else {
      write.csv(modelfit2()$predvals, file = paste(dout,
        "dataset.csv", sep = ""))
    }
  })

  output$radiocontents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file2)
    
    detectiondf <- read.csv(input$file2$datapath,
      header = input$headerdet,
      sep = input$sepdet)#,
    # quote = input$quote)
    
    return(head(detectiondf))

  })
  ## })
  
  dataradiocollar <- reactive({
    
    req(input$file2)
    
    detectiondf <- read.csv(input$file2$datapath,
      header = input$headerdet,
      sep = input$sepdet)#,
    # quote = input$quote)
    
    updateSelectInput(session, inputId = 'detectionresp',
      label = 'Detection Column',
      choices = names(detectiondf),
      selected = names(detectiondf)[1])
    updateCheckboxGroupInput(session, inputId = 'detectionpreds',
      label = "Detection Predictors",
      choices = c("None", names(detectiondf)), selected = "None")
  #  updateSelectInput(session, inputId = 'xcoords',
  #    label = 'X-coordinate column',
  #    choices = names(df))
  #  updateSelectInput(session, inputId = 'ycoords',
  #    label = 'Y-coordinate column',
  #    choices = names(df))
  #  updateSelectInput(session, inputId = 'strat',
  #    label = 'Stratification column',
  #    choices = c("None", names(df)), selected = "None")
    
    return(detectiondf)
    ##lmobj <- lm(input$resp ~ 1, na.rm = TRUE, data = df)
    ##print(summary(lmobj))
  })
  
modelfitradiocollar <- reactive({
    req(input$file2)
   # req(input$file1)
    
    dataradiocollar()
    if (input$godetection == 0) {
      return()
    } else {
    #modradiocollar <- lm(input$detectionresp ~ 1)
    isolate(if (sum(input$detectionpreds == "None") >= 1) {
      
    detmod <- get_detection(formula =
        as.formula(paste(input$detectionresp, "~", 1, sep = "")),
      data = dataradiocollar(),
      varmethod = "Delta")
    
   # valuesdet <- reactiveValues()
  #  valuesdet$a <- detmod
    
    } else {
      detectionform <- as.formula(paste(input$detectionresp, "~",
        paste(input$detectionpreds, collapse="+"), sep = ""))
      
      detmod <- get_detection(formula =
          detectionform,
        data = dataradiocollar(),
        varmethod = "Delta")
      
      return(detmod)
    }
    )}
  })

output$radiosummary <- renderPrint({
  
  print(modelfitradiocollar())
  #  print(prednames)
})
  

}

# Run the application 
shinyApp(ui = ui, server = server)

