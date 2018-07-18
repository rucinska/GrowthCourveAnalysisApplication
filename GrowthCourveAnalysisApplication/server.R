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
  
 
  
  #http://www.cyclismo.org/tutorial/R/linearLeastSquares.html
  #https://stackoverflow.com/questions/33748649/subset-dataframe-based-on-zoom-interaction
  #})
  
  observeEvent(input$update, {
    updateSelectInput(session, "replicates", choices=unique(reval_df()$Layout), selected = unique(reval_df()$Layout))
    
  })
  
  
  observeEvent(input$update, {
    samples_list <- colnames(reval_df())
    samples_list <- samples_list[4:length(samples_list)]
    updateSelectInput(session, "samples", choices=samples_list, selected = samples_list)
    #updateSelectInput(session, "samples", choices=unique(colnames(reval_df())), selected = unique(colnames(reval_df())))
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
  
  #data modification based on user selection
  
  data_mod <- reactive({
    
    data <- reval_df() %>% gather(Sample, Value, -one_of("time", "Layout","Type"))
    
    data <- as.data.frame(data)
    
    data <- subset(data,
                   time >= input$time[1] & time <= input$time[2])
    print(str(data))
    
      if(is.null(input$samples))
      { data
      }else {
        data <- data %>%  filter(Sample %in% input$samples)
        }
      
      if(is.null(input$replicates))
      { data 
      }else {
        data <- data %>%  filter(Layout %in% input$replicates)
      }
    return(as.data.frame(data))
    print(data)
  })
  

  
  #render the dataframe 
  output$contents1 <- DT::renderDataTable({
    input$update
    datatable(reval_df(), options =  list( scrollX = T))
    
  } )
  
  ## Download the data 
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(reval_df(), file)
    },
    contentType = "text/csv"
  )
  
  
  ####PLOT the data - main GR plot#### 
  
  main_Plot <- eventReactive({ 
    input$update
    input$update_plot
  },{
    if(input$color_by == "Sample"){
    
      plot<-data_mod() %>% 
        ggplot( aes(x =  time , y = Value, color = factor(Sample))) +
        stat_summary(data = data_mod(), inherit.aes = TRUE,
                     fun.y = 'mean', fun.ymin = function(x) 0, geom = 'point', 
                     position=position_dodge()) +
        stat_summary(data =  data_mod(), inherit.aes = TRUE, 
                     fun.y = mean,
                     fun.ymin = function(y) mean(y) - sd(y), 
                     fun.ymax = function(y) mean(y) + sd(y),
                     geom ="pointrange",show.legend = FALSE) +
        ggtitle(input$title)  +labs( x = input$xlab, y = input$ylab) + ylim(input$minY,input$maxY)
      
      if(input$log){
        plot <- plot  +  scale_y_continuous(trans=log2_trans())#scale_y_continuous(trans="log",breaks = trans_breaks("log", function(x) exp(x)),
        #labels = trans_format("log", math_format(e^.x)))#
      }
      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') plot <- plot + facet_grid(facets)
      
      
      plot
    } else if (input$color_by == "Replicate") {
      
      plot<- data_mod() %>% 
        group_by(Sample) %>% 
        ggplot(aes(x =  time , y = Value, col = factor(Layout),shape = Sample, group=interaction(Layout, Sample))) +
        geom_point() +
        ggtitle(input$title) +labs( x = input$xlab, y = input$ylab) + ylim(input$minY,input$maxY)
      
      if(input$log){
        plot <- plot +  scale_y_continuous(trans=log2_trans())
      }  
      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') plot <- plot + facet_grid(facets)
      
      plot
    }
    
    plot +
      theme_bw() 
})
  
  output$plot <- renderPlot({main_Plot() })
  
  #### CALCULATE GROWTH RATE ####
  
  GrowthRate_calc <- eventReactive({ 
    input$update
    input$update_plot
  }, {
    if(input$color_by == "Sample"){
      
      df <-data_mod() %>% 
        select(-Layout, -Type) %>% 
        group_by(Sample) %>%
        mutate(id=1:n()) %>%
        spread(Sample, Value) %>% 
        select(-id)
      print(df)
      df[] <- lapply(df, abs)
      
      
      n <- 1    # keeps track of the current row in the output data frame
      num_analyses <- length(names(df)) - 1
      mydf <- data.frame(sample = character(num_analyses),
                         k = numeric(num_analyses),
                         n0  = numeric(num_analyses),
                         r = numeric(num_analyses),
                         t_mid = numeric(num_analyses),
                         t_gen = numeric(num_analyses),
                         auc_l = numeric(num_analyses),
                         auc_e = numeric(num_analyses),
                         sigma = numeric(num_analyses),
                         stringsAsFactors = FALSE)
      
      
      
      for (col_name in names(df)) {
        
        if (col_name != "time") {
          
          #This function finds the parameters that describe the input data’s growth. It does so by fitting the
          #logistic curve to your growth curve measurements.
          
          
          # Create a temporary data frame that contains just the time and current col
          d_loop <- df[, c("time", col_name)]
          #print(d_loop)
          #print(str(d_loop))
          gc_fit <- SummarizeGrowth(data_t = d_loop[, "time"],
                                    data_n = d_loop[, col_name],
                                    bg_correct = "none")
          mydf$sample[n] <- col_name
          mydf[n, 2:9] <- c(gc_fit$vals$k,
                            gc_fit$vals$n0,
                            gc_fit$vals$r,
                            gc_fit$vals$t_mid,
                            gc_fit$vals$t_gen,
                            gc_fit$vals$auc_l,
                            gc_fit$vals$auc_e,
                            gc_fit$vals$sigma)
          n <- n + 1
        }
      }
      mydf
      #https://stackoverflow.com/questions/1169539/linear-regression-and-group-by-in-r

      return(mydf)
    } else if (input$color_by == "Replicate") {
      

      df_rep <- data_mod() %>% select(-Type)

      d<- df_rep %>% 
        #gather(variable, value, -one_of("time","Layout")) %>% 
        unite(temp, Sample, Layout) %>% 
        group_by(temp) %>% 
        #mutate(id=1:n()) %>% 
        spread(temp, Value)
      print(d)
      d[] <- lapply(d, abs)
      
      num_analyses <- length(names(d)) - 1
      d_gc <- data.frame(sample = character(num_analyses),
                         k = numeric(num_analyses),
                         n0  = numeric(num_analyses),
                         r = numeric(num_analyses),
                         t_mid = numeric(num_analyses),
                         t_gen = numeric(num_analyses),
                         auc_l = numeric(num_analyses),
                         auc_e = numeric(num_analyses),
                         sigma = numeric(num_analyses),
                         stringsAsFactors = FALSE)
      
      rep_count <- length(unique(df_rep$Layout))
      col_count <- ncol(df_rep)-2
      par(mfcol = c(rep_count ,col_count))
      par(mar = c(0.25,0.25,0.25,0.25))
      y_lim_max <- max(d[,setdiff(names(d), "time")]) - min(d[,setdiff(names(d), "time")])
      trim_at_time <- input$time[2]
      n <- 1    # keeps track of the current row in the output data frame
      for (col_name in names(d)) {
        print(col_name)
        # Don't process the column called "time". 
        # It contains time and not absorbance data.
        if (col_name != "time") {
          
          # Create a temporary data frame that contains just the time and current col
          d_loop <- d[, c("time", col_name)]
          
          
          # Now, call Growthcurver to calculate the metrics using SummarizeGrowth
          gc_fit <- SummarizeGrowth(data_t = d_loop[, "time"], 
                                    data_n = d_loop[, col_name],
                                    
                                    bg_correct = "none")
          summary(gc_fit)
          plot(gc_fit)
          gc_fit$model
          # Now, add the metrics from this column to the next row (n) in the 
          # output data frame, and increment the row counter (n)
          d_gc$sample[n] <- col_name
          d_gc[n, 2:9] <- c(gc_fit$vals$k,
                            gc_fit$vals$n0,
                            gc_fit$vals$r,
                            gc_fit$vals$t_mid,
                            gc_fit$vals$t_gen,
                            gc_fit$vals$auc_l,
                            gc_fit$vals$auc_e,
                            gc_fit$vals$sigma)
          n <- n + 1
          
          # Finally, plot the raw data and the fitted curve
          # Here, I'll just print some of the data points to keep the file size smaller
          n_obs <- length(gc_fit$data$t)
          idx_to_plot <- 1:20 / 20 * n_obs
          plot<-plot(gc_fit$data$t[idx_to_plot], gc_fit$data$N[idx_to_plot], 
                     pch = 20, 
                     xlim = c(0, trim_at_time), 
                     ylim = c(0, y_lim_max),
                     cex = 0.6, xaxt = "n", yaxt = "n")
          text(x = 10, y = y_lim_max, labels = col_name, pos = 1)
          lines(gc_fit$data$t, predict(gc_fit$model), col = "red")
        }
      }
      d_gc
    }
  })
  
  
  output$summary1 <- DT::renderDataTable({
    
    datatable(GrowthRate_calc(),  options = list(scrollX = T))
  })
  
  ##################################
  
  
  
  
  
})
