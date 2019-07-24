library(shiny)
## library(shinyjs)
library(shinyFiles)
##library(ggplot2)
##library(devtools)
## devtools::install_git("https://github.com/highamm/FPBKPack2.git")
library(FPBKPack2)
##
## library(rsconnect)
## rsconnect::deployApp('~/Desktop/FPBKShiny/FPBKShinyApp')

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
          multiple = FALSE,
          accept = c("text/csv",
            "text/comma-separated-values,text/plain",
            ".csv")),
        
        # Horizontal line ----
      #  tags$hr(),
        
        # Input: Checkbox if file has header ----
      #  checkboxInput("header", "Header", TRUE),
        # Input: Select separator ----
        radioButtons("sep", "Separator",
          choices = c(Comma = ",",
            #Semicolon = ";",
            Tab = "\t"),
          selected = ","),
        

        # Horizontal line ----
    #    tags$hr(),
        
        
        selectInput('resp', 'Select Response:', "",
          width = '200px'),
        checkboxGroupInput('preds', 'Select Predictors:', "",
          width = '200px'),
        selectInput('xcoords', 'Select X-coords:', "",
          width = '200px'),
        selectInput('ycoords', 'Select Y-coords:', "",
          width = '200px'),
        selectInput('strat', 'Select Stratification:', "",
          width = '200px'),
        selectInput('area', 'Select Area Column:', "",
          width = '200px'),
        numericInput('detection',
          'Enter Estimated Detection:', value = 1,
          min = 0, max = 1,
          width = '200px'),
        numericInput('SEdetection',
          'Enter Standard Error for Detection:', value = 0,
          min = 0, max = Inf,
          width = '200px'),
        radioButtons("latlon", "Latitude / Longitude?",
          choices = c(Yes = "LatLon",
            No = "TM"),
          selected = "TM",
          width = '200px'),
        selectInput('predwtscol',
          'Select Column with Prediction Weights', "",
          width = '200px'),
        actionButton("go", "Submit"),
          
        
        actionButton("genreport", "Get Report"),
        actionButton("gendataframe", "Get Prediction Data")
        
        
      ),
        conditionalPanel(condition = "input.tabs1==2"),
        
        conditionalPanel(condition = "input.tabs1==3",
        fileInput("file2", "Choose CSV File with Radiocollar Data",
          multiple = FALSE,
          accept = c("text/csv",
            "text/comma-separated-values,text/plain",
            ".csv")),
       ## checkboxInput("headerdet", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons("sepdet", "Separator",
          choices = c(Comma = ",",
         ##   Semicolon = ";",
            Tab = "\t"),
          selected = ","),
          
          selectInput('detectionresp',
            'Select Detection Column:', ""),
          checkboxGroupInput('detectionpreds',
            'Select Detection Predictors:', ""),
          actionButton("godetection", "Submit")
          
          
          
          ),
        
        conditionalPanel(condition = "input.tabs1==4",
          fileInput(inputId = "shp", label = "Choose Shapefile", multiple = TRUE, accept = c('.shp', '.dbf','.sbn', '.sbx', '.shx', '.prj')),
          fileInput("predfile",
            "Choose File with Site by Site Predictions",
            multiple = FALSE,
            accept = c("text/csv",
              "text/comma-separated-values,text/plain",
              ".csv")),
          
          selectInput('shapeid',
            'Select Shape ID Column:', ""),
          selectInput('predid',
            'Select Prediction ID Column:', ""),
          selectInput('krigedpreds',
            'Select Column with Sitewise Predictions', ""),
    #      selectInput('latcoords',
    #        'Select Column with Latitude Coordinates', ""),
    #      selectInput('loncoords',
    #        'Select Column with Longitude Coordinates', ""),
          actionButton("goshapefile", "Submit")
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
             tags$li("Upload a data set from your local computer. The data set should be a .csv file and contain information on both the sampled and unsampled sites. The minimum that the data set should have are a column of observed counts, x-coordinates, y-coordinates, and any strata or other predictors."), 
             tags$li("If you do not have radiocollar data, then skip this step. If you do have radiocollar data, click the Radiocollar Sightability tab at the top center of the webpage. Upload your radiocollar data from a .csv file, which should, at minimum contain a column for whether or not each radiocollared animal was sighted. Choose the detection column and any relevant predictors for detection and then click the Submit button. If the informal output looks appropriate and there are not error messages, go back to the GSPE tab and continue with the remainder of the steps."),
             tags$li("Select the appropriate columns for the counts, coordinates, and predictors from the drop-down menus."), 
             tags$li("If you have an estimate for mean detection and a standard error, put in this estimate here. Note that this is separate from the sightability trials: if you've uploaded sightability data, then the model is already accounting for imperfect detection and these can be left as is! Uploading sightability data and putting in values for mean detection result in adjusting for sightability twice, which is not what we want. See the accompanying vignette for more information."),
             tags$li("Click the Submit button and verify that the information given in the informal R output seems reasonable."),
             tags$li("Click the Get Report button for an HTML report that can be saved as a PDF or the Get Prediction Data button for the data set in a that was originally uploaded with site-by-site predictions and a few other columns of information appended in a .csv format.")
           ),
           h3("Information about Stratification"),
           "Stratum can be included as a predictor in the spatial model by choosing the column with stratum in the Select Predictors part of the app. Doing so fits a a single spatial linear model, assuming that all sites have the same spatial structure with different means. Alternatively, stratum can be unchecked as a predictor but chosen in the Select Stratification option. Doing so fits a model for each stratum, allowing strata to have different covariance structures. The assumption we make with this model is that the strata are not cross-correlated."),
         tabPanel("Radiocollar Sightability", value = 3,
           tableOutput("radiocontents"),
           verbatimTextOutput("radiosummary")),
         
         tabPanel("Map with Shapefile", value = 4,
           tableOutput("predcontents"),
           plotOutput("shapecontents"))
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
      header = TRUE,
      sep = input$sep)#,
     # quote = input$quote)
    
      return(head(df))
  
  })
  
  datare <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
      header = TRUE,
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
    updateSelectInput(session, inputId = 'predwtscol',
      label = 'Prediction Weight Column',
      choices = c("None", names(df)), selected = "None")
    
    return(df)
  })


  modelfit <- reactive({
    req(input$file1)

   datare()
   
   if (input$go == 0) {
     return()
   } else { 
     
     ## isolate wraps this so that it doesn't automatically
     ## update after submit is hit for the first time
    isolate({
      
      if (sum(input$preds == "None") >= 1) {
        formtouse <- as.formula(paste(input$resp, "~",
          1, sep = ""))
      } else {
        formtouse <- as.formula(paste(input$resp, "~",
          paste(input$preds, collapse="+"), sep = ""))
      }
      
      if (sum(input$area == "None") >= 1) {
        areavar <- NULL 
      } else {
        areavar <- input$area
      }
      
      if (sum(input$predwtscol == "None") >= 1) {
        FPBKvar <- NULL
      } else {
        FPBKvar <- input$predwtscol
      }
      
      if (sum(input$strat == "None") >= 1) {
        
          fit <- slmfit(formula = formtouse,
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            coordtype = input$latlon,
            covestimates = c(NA, NA, NA),
            detectionobj = NULL,
            areacol = areavar)
        
          predfit <- predict(fit, FPBKcol = FPBKvar,
            detinfo = c(input$detection, input$SEdetection))
        
        predout <- FPBKoutput(pred_info = predfit,
          conf_level = c(0.80, 0.90, 0.95),
          get_krigmap = TRUE, get_sampdetails = TRUE,
          get_variogram = TRUE,
          pointsize = 3)
    
    } else {
      
          multiobj <- multistrat(formula = formtouse,
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            coordtype = input$latlon,
            detectionobj = NULL,
            detinfo = c(input$detection, input$SEdetection),
            areacol = areavar,
            FPBKcol = FPBKvar,
            stratcol = input$strat)
    }
      })
   }
  })

  
  
  
  modelfit2 <- reactive({
    req(input$file1)
    req(input$file2)
    
    datare()
    
    if (input$godetection == 0 | input$go == 0) {
      return()
    } else {
      
      isolate({
        
        if (sum(input$preds == "None") >= 1) {
          formtouse <- as.formula(paste(input$resp, "~",
            1, sep = "")) 
        } else {
          formtouse <- as.formula(paste(input$resp, "~",
            paste(input$preds, collapse="+"), sep = ""))
        }
        
        if (sum(input$area == "None") >= 1) {
          areavar <- NULL
        } else {
          areavar <- input$area
        }
        
        if (sum(input$predwtscol == "None") >= 1) {
          FPBKvar <- NULL
        } else {
          FPBKvar <- input$predwtscol
        }
        
        if (sum(input$strat == "None") >= 1) {
          
          fit <- slmfit(formula = formtouse,
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            coordtype = input$latlon,
            covestimates = c(NA, NA, NA),
            detectionobj = modelfitradiocollar(),
            areacol = areavar)
          
          
          predfit <- predict(fit, FPBKcol = FPBKvar,
            detinfo = c(input$detection, input$SEdetection))
          
          
          predout <- FPBKoutput(pred_info = predfit,
            conf_level = c(0.80, 0.90, 0.95),
            get_krigmap = TRUE, get_sampdetails = TRUE,
            get_variogram = TRUE,
            pointsize = 3)
          
        } else {
          
          multiobj <- multistrat(formula = formtouse,
            data = datare(),
            xcoordcol = input$xcoords,
            ycoordcol = input$ycoords,
            CorModel = "Exponential",
            estmethod = "REML",
            covestimates = c(NA, NA, NA),
            coordtype = input$latlon,
            detectionobj = modelfitradiocollar(),
            detinfo = c(input$detection, input$SEdetection),
            areacol = areavar,
            stratcol = input$strat)
        }
      })}
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
  })
  
  observeEvent(input$gendataframe, {
    ##dout <- "~/Desktop/"
    dout <- input$file1$datapath
    if (is.null(filedata()) == TRUE) {
      write.csv(modelfit()$predvals, file = "predictions.csv")#file = paste(dout,
       # "dataset.csv", sep = ""))
    } else {
      write.csv(modelfit2()$predvals, file = "predictions.csv")
      #file = paste(dout,
        #"dataset.csv", sep = ""))
    }
  })

  output$radiocontents <- renderTable({
    
    # input$file2 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file2)
    
    detectiondf <- read.csv(input$file2$datapath,
      header = TRUE,
      sep = input$sepdet)
    
    return(head(detectiondf))
  })

  
  output$predcontents <- renderTable({
    
    req(input$predfile)
    
    preddf <- read.csv(input$predfile$datapath,
      header = TRUE,
      sep = ",")
    
    return(head(preddf))
    
  })
  
  predre <- reactive({
    req(input$predfile)
    
    dfpred <- read.csv(input$predfile$datapath,
      header = TRUE,
      sep = ",")
    
    updateSelectInput(session, inputId = 'predid',
      label = 'ID in Prediction Data Set',
      choices = names(dfpred))
    updateSelectInput(session, inputId = 'krigedpreds',
      label = "Predictions",
      choices = names(dfpred))
    
    return(dfpred)
  })
  
   
  shapereact <- reactive({
    
    req(input$shp)
      
    # shpdf is a data.frame with the name, size, type and datapath of the uploaded files
    shpdf <- input$shp
    library(rgdal)
    library(sp)
    library(rgeos)
    library(broom)
 

    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename files
    for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], paste0(tempdirname, "/",
        shpdf$name[i]))
    }
    
    # Read shp
    map <- rgdal::readOGR(paste(tempdirname,
      shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep="/"))
    
    updateSelectInput(session, inputId = 'shapeid',
      label = 'ID in Shapefile',
      choices = names(map))
    return(map)
  })
  
  output$shapecontents <- renderPlot({
    
    predre()
    shapereact()
    
    if (input$goshapefile == 0) {
      return()
    } else {
      
    shapefort <- broom::tidy(shapereact(),
      region = input$shapeid)
    
    coorddf <- sp::SpatialPointsDataFrame(data = shapefort,
      coords = cbind(shapefort$long,
        shapefort$lat))
    sp::proj4string(coorddf) <- sp::proj4string(shapereact())
    
    trans.df <- sp::spTransform(coorddf,
      sp::CRS("+proj=longlat +datum=WGS84"))
    
    predictiondf <- predre()
    
    final.df <- suppressWarnings(merge(trans.df, predictiondf,
      by.x = "id", by.y = input$predid,
      all.x = TRUE, sort = TRUE))
    
    final.df2 <- final.df[order(final.df$order), ]

    final.df2@data$testx123 <- final.df2@coords[ ,1]
    final.df2@data$testy123 <- final.df2@coords[ ,2]
    
    krigpredvec <- final.df2@data[ ,input$krigedpreds]
   
   ggplot2::ggplot() +
      ggplot2::geom_polygon(data = final.df2@data,
        ggplot2::aes(x = testx123, y = testy123, group = id,
          fill = krigpredvec),
        colour = "darkgrey", alpha = 1) +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::scale_fill_viridis_c("Predictions") + ##, direction = -1,
      ##   labels = c("High", "Low", "NA"),
      ##  "Stratum") +
      ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
      ggplot2::ggtitle("Map of Site-wise Predictions") +
     ggplot2::coord_quickmap() +
     ggplot2::ggsave("Krigmap.png") ## saves in working directory by default
    }
  })

  
  dataradiocollar <- reactive({
    
    req(input$file2)
    
    detectiondf <- read.csv(input$file2$datapath,
      header = TRUE,
      sep = input$sepdet)
    
    updateSelectInput(session, inputId = 'detectionresp',
      label = 'Detection Column',
      choices = names(detectiondf),
      selected = names(detectiondf)[1])
    updateCheckboxGroupInput(session, inputId = 'detectionpreds',
      label = "Detection Predictors",
      choices = c("None", names(detectiondf)), selected = "None")
    
    return(detectiondf)
})
  
modelfitradiocollar <- reactive({
    req(input$file2)

    dataradiocollar()
    if (input$godetection == 0) {
      return()
    } else {
      
    isolate({
      
      if (sum(input$detectionpreds == "None") >= 1) {
        detectionform <- as.formula(paste(input$detectionresp, "~", 1, sep = ""))
      } else {
        detectionform <- as.formula(paste(input$detectionresp, "~",
          paste(input$detectionpreds, collapse="+"), sep = ""))
      }
      
    detmod <- get_detection(formula = detectionform,
      data = dataradiocollar(),
      varmethod = "Bootstrap")
    
    return(detmod)
      
    })}
  })

output$radiosummary <- renderPrint({
  print(modelfitradiocollar())
})
  
}

shinyApp(ui = ui, server = server)

