library(shinyFiles)
library(shiny)
library(shinydashboard)
library(datasets)
library(DT)
library(tidyr)
library(dplyr)
library(shinyjs)
library(ggrepel)
#library(ggbiplot)
library(stringr)
library(readr)
library(stringi)
library(softermax)
library(broom)
#library(growthcurve)
library(growthcurver)
library(tools)
library(ggplot2)
library(scales)
library("grofit")
library(reshape2)
library(purrr)
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  #define reactive values
  values <- reactiveValues(
    plots = list(),
    df = data.frame(),
    df_data = NULL,
    maindata = NULL     # the data frame used throughout the app
  ) 
    
  #Load data
  
  input_data <- reactive({
    req(input$file1)
    inFiles <- input$file1
    read_xml <- function(i){
      cvdata_df <- as.data.frame(read_softmax_xml(file =  i))
    }
    get_time <- function(j){
      data_time <- basename(j)
    }
    
    all_plates <- data.frame()
    
    if (is.null(inFiles))
      return(NULL)
    
    dat = lapply(inFiles$datapath, read_xml) 
    data_time<- lapply(inFiles$name, get_time)
    
    filelist <- mapply(cbind, dat, "data_time"=data_time, SIMPLIFY=F)
    all_plates = do.call("rbind.data.frame", filelist)
    all_plates
    
  })
  
  #modify data to df
  data <- reactive({ 
    #extract time form xml file
    cvdata_df <-input_data() %>% select(-Time) %>%
      separate(data_time, sep = " ",c("Date", "Type"))
    #extract date form file name
    cvdata_df$Type <-  sub("_XML.xml", "", cvdata_df$Type )
    
    cvdata_df <-cvdata_df %>%
      separate(Date, sep = "_",c("Date", "Time"))
    
    cvdata_df$Date <- stri_extract_last_regex(cvdata_df$Date, "\\d{8}")
    
    cvdata_df$Time<- format(strptime( cvdata_df$Time, format="%H%M%S"), format = "%H:%M:%S")
    
    cvdata_df$Date<- as.Date(cvdata_df$Date, format = "%Y%m%d")
    
    
    all_data <- cvdata_df %>%
      select( Well, Date, Time, Value, Type) %>%
      extract( Well, into = c("Layout", "col"), "^([A-Z])(\\d+)$") %>%
      mutate( col = as.numeric(col)) %>%
      group_by(Layout, col, Date, Time) %>%
      mutate(group_row = 1:n())%>%
      spread(col, Value) %>%
      select(-group_row) %>%
      ungroup()
    
    all_data <- all_data %>%
      mutate( timestamp=paste(Date, Time) %>%
                as.POSIXct( format="%Y-%m-%d %H:%M:%S"))
    #create time diffrence 
    all_data <- all_data %>%
      #select( -Time) %>%
      arrange(timestamp) %>%
      group_by( Type) %>%
      mutate(diff = timestamp - first(timestamp),
             time = as.numeric(diff, units = 'hours')) %>%
      ungroup() %>%
      select(-Date, -Time, -timestamp, -diff)
    
    str(all_data$timestamp)
    #change col names
    col_names <- c("Layout", "Type","col1","col2","col3","col4", "col5","col6","col7", "col8","col9","col10","col11","col12", "time")
    names(all_data) <- col_names
    
    all_data <- as.data.frame(all_data)
    # remove water outlayer
    if(!input$removewater){
      all_data
    }else{
      all_data <- all_data %>%
        #select(-col1, -col12) %>%
        filter(!Layout %in% c("A","H") )
    }
    #update names selection in Date tab
    names_sample <-colnames(all_data)
    names_sample <- c("NULL", names_sample[!names_sample %in% c("Layout","time","Type")])
    isolate({
      for(i in 1:10) {
        updateSelectInput(session, paste0('sam',i), choices=names_sample,selected = paste0('col',i+1))
        updateSelectInput(session, paste0('bla',i), choices=names_sample,selected ="col12")
      }})
    return(all_data)
    
    
  })# end data
    #https://stackoverflow.com/questions/42790422/how-do-i-coerce-contents-of-a-reactive-to-a-data-frame
    #https://stackoverflow.com/questions/29716868/r-shiny-how-to-get-an-reactive-data-frame-updated-each-time-pressing-an-actionb/30502250#30502250
    #https://stackoverflow.com/questions/45169876/observeevent-shiny-function-used-in-a-module-does-not-work 
  
  reval_df <- reactiveVal()
  observeEvent(data(), {
    reval_df(data())
  })
  
  
  ## take a colum choosed before and substract the blank - save as one column
  observeEvent(input$update, {
    df <- data()
    
    for (i in 1:10)
    {
      if(input[[paste0('sam',i)]]!='NULL' & input[[paste0('bla',i)]]!='NULL')
      {
        
        df[[input[[paste0('name',i)]]]] = df[[input[[paste0('sam',i)]]]]- df[[input[[paste0('bla',i)]]]]
      }
    }
    
    has.neg <- apply(df, 1, function(row) any(row < 0))
    
    if(has.neg == TRUE){
      output$n <- renderUI({HTML(paste0("ACHTUNG!!!! You have negative values in your samples! Go and repeat you experiment... #SHAME"))})
      
    }
    
    reval_df(df<- df%>% select(-(starts_with("col"))))
    
  })
  
  observeEvent(input$update, shinyjs::disable("removewater"))
  
  ## Download the data 
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(reval_df(), file)
    },
    contentType = "text/csv"
  )
  
  #render the dataframe 
  output$contents1 <- DT::renderDataTable({
    input$update
    datatable(reval_df(), options =  list( scrollX = T))
    
  } )
  
  #http://www.cyclismo.org/tutorial/R/linearLeastSquares.html
  #https://stackoverflow.com/questions/33748649/subset-dataframe-based-on-zoom-interaction
  #})
  
  observeEvent(input$update, {
    updateSelectInput(session, "replicates", choices=unique(reval_df()$Layout), selected = unique(reval_df()$Layout))
    
  })
  observeEvent(input$update, {
    updateSelectInput(session, "samples", choices=unique(colnames(reval_df())), selected = unique(colnames(reval_df())))
    
  })
  observeEvent(input$update, {
    updateSelectInput(session, "wells", choices = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12",
                                                  "B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12",
                                                  "C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12",
                                                  "D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12",
                                                  "E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12",
                                                  "F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12",
                                                  "G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12",
                                                  "H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12"), 
                                        selected = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12",
                                                   "B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12",
                                                   "C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12",
                                                   "D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12",
                                                   "E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12",
                                                   "F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12",
                                                   "G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12",
                                                   "H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12"))
    
  })
  
  
  observeEvent(input$reset, {
    shinyjs::reset("side-panel")
  })
  
  
  
})
