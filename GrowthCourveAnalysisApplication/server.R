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
  
  # update select input - when Sample selected not possible to facet by Layout
  conditionalPanel(
    condition = "input.color_by == 'Sample'",     updateSelectInput(session, "facet_row",
                                                                    label ='Facet Row', 
                                                                    choices = c(None = '.',"Sample")
                                                                    
    ))
  
  conditionalPanel(
    condition = "input.color_by == 'Sample'",     updateSelectInput(session, "facet_row",
                                                                    label ='Facet Column', 
                                                                    choices = c(None = '.',"Sample")
                                                                    
    ))
  
  #data modification based on user selection
  
  data_mod <- reactive({
    
    data <- reval_df() %>% select(-Type) %>% gather(Sample, Value, -one_of("time", "Layout"))
    
    data <- as.data.frame(data)
    
    data <- subset(data,
                   time >= input$time[1] & time <= input$time[2])
    data <- data %>% mutate(Value = abs(Value))
    if(input$norm){
      # if(input$color_by == "Sample"){
      #   print(data)
      #     data <-data %>% group_by(Layout, Sample, time) %>%
      #       summarise(Value = mean(Value)) %>%
      #       mutate(Value = Value - first(Value))  
      #     print(str(data))
      # 
      #   } else if (input$color_by == "Replicate") {
      #     
          data <-data %>% group_by(Sample,Layout,time) %>%
            summarise(Value = mean(Value)) %>%
            mutate(Value = Value - first(Value))
          print(data)
    #}
     }
    
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
        ggtitle(input$title)  +labs( x = input$xlab, y = input$ylab) + ylim(input$minY,input$maxY) +
        stat_summary(data = data_mod(), inherit.aes = TRUE,
                     fun.y = 'mean', fun.ymin = function(x) 0, geom = 'point', 
                     position=position_dodge()) +
        stat_summary(data =  data_mod(), inherit.aes = TRUE, 
                     fun.y = mean,
                     fun.ymin = function(y) mean(y) - sd(y), 
                     fun.ymax = function(y) mean(y) + sd(y),
                     geom ="pointrange",show.legend = FALSE)
      
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
        select(-Layout) %>% 
        group_by(Sample) %>%
        mutate(id=1:n()) %>%
        spread(Sample, Value) %>% 
        select(-id)
     
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
      
      df_rep <- data_mod() 

      d<- df_rep %>% 
        #gather(variable, value, -one_of("time","Layout")) %>% 
        unite(temp, Sample, Layout) %>% 
        group_by(temp) %>% 
        #mutate(id=1:n()) %>% 
        spread(temp, Value)
     
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
          plot
        }
      }
      plot
      d_gc
    }
  })
  
  # Replication Growth rate fitting plot
  GrowthRate_repPLOT <- eventReactive({ 
    input$update
    input$update_plot
  }, {
    if (input$color_by == "Replicate") {
    
    df_rep <- data_mod() 
    
    d<- df_rep %>% 
      #gather(variable, value, -one_of("time","Layout")) %>% 
      unite(temp, Sample, Layout) %>% 
      group_by(temp) %>% 
      #mutate(id=1:n()) %>% 
      spread(temp, Value)
    
    d[] <- lapply(d, abs)
  
    num_analyses <- length(names(d)) - 1

    rep_count <- length(unique(df_rep$Layout))
    col_count <- length(unique(df_rep$Sample))
    
    par(mfcol = c(rep_count ,col_count))
    par(pin=c(0.9, 0.9), mar = c(0.25,0.25,0.25,0.25))
    
    
    y_lim_max <- max(d[,setdiff(names(d), "time")]) - min(d[,setdiff(names(d), "time")])
    trim_at_time <- max(df_rep$time)#input$time[2]
    n <- 1    # keeps track of the current row in the output data frame
    for (col_name in names(d)) {
      # Don't process the column called "time". 
      # It contains time and not absorbance data.
      if (col_name != "time") {
        
        # Create a temporary data frame that contains just the time and current col
        d_loop <- d[, c("time", col_name)]
        
        
        # Now, call Growthcurver to calculate the metrics using SummarizeGrowth
        gc_fit <- SummarizeGrowth(data_t = d_loop[, "time"], 
                                  data_n = d_loop[, col_name],
                                  bg_correct = "none")
       
        # Finally, plot the raw data and the fitted curve
        # Here, I'll just print some of the data points to keep the file size smaller
        n_obs <- length(gc_fit$data$t)
        idx_to_plot <- 1:20 / 20 * n_obs
        
        plot(gc_fit$data$t[idx_to_plot], gc_fit$data$N[idx_to_plot], 
                   pch = 20, 
                   xlim = c(0, trim_at_time), 
                   ylim = c(0, y_lim_max),
                   cex = 0.6, xaxt = "n", yaxt = "n")
        text(x = 10, y = y_lim_max, labels = col_name, pos = 1)
        lines(gc_fit$data$t, predict(gc_fit$model), col = "red")
      }
    }
    }
  })
  

  
  #print data in the table

  output$summary1 <- DT::renderDataTable({
    datatable(GrowthRate_calc(),  options = list(scrollX = T),callback = JS("table.on('click.dt', 'td', function() {
                                 Shiny.onInputChange('click', Math.random());});"))
    }) 

  
  #download the data
  output$downloadData_gr_cal <- downloadHandler(
    filename = "growth_rate_data.csv",
    content = function(file) {
      write.csv(GrowthRate_calc(), file)
    },
    contentType = "text/csv"
  )
  
  plotModal <- function() {
    modalDialog(
      size = "l",
      plotOutput("ptdist")
    )
  }
  observeEvent(input$click, {
    removeModal()
    showModal(plotModal())
  })
  output$ptdist <- renderPlot({
    GrowthRate_repPLOT()
  })
  
  ##################################
  ########### SAVE PLOT MAIN  ######
  
  
  # When the save button is clicked, add the plot to a list and clear the input
  observeEvent(input$save_plot_btn, {
    plot_name <- trimws(input$save_plot_name)
    
    if (plot_name %in% names(values$plots)) {
      showModal(
        modalDialog(
          "You already have a plot saved with the same name. Saving this plot will override the existing plot.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_plot_duplicate_confirm", "OK",
                         class = "btn-primary")
          ),
          size = "m"
        )
      )
    } else {
      save_plot()
    }
  })
  
  observeEvent(input$save_plot_duplicate_confirm, {
    save_plot()
    removeModal()
  })
  save_plot <- function() {
    shinyjs::show("save_plot_checkmark")
    values$plots[[trimws(input$save_plot_name)]] <- main_Plot() 
    updateTextInput(session, "save_plot_name", value = "")
    shinyjs::delay(
      1000,
      shinyjs::hide("save_plot_checkmark", anim = TRUE, animType = "fade")
    )
  }  
  # Disable the "save" button if the plot name input is empty
  observe({
    shinyjs::toggleState("save_plot_btn",
                         condition = nzchar(trimws(input$save_plot_name)))
    
  })   
  
  
  
  ###############################
  
  
  ##### Check your fit TAB ###############
  
  FitExpo <- eventReactive({ 
    input$update
    input$update_plot
  }, {
    if(input$color_by == "Sample"){
      
      df <-data_mod() %>% select(-Layout) %>% 
        group_by(Sample) %>%
        mutate(id=1:n()) %>% 
        spread(Sample, Value) %>%
        select(-id)
      
      print(df)
      #df[] <- lapply(df, abs)
      
      summG <- function(x) {SummarizeGrowth(df$time,x)}
      lapply(df[2:ncol(df)], summG)
      models.all <- lapply(df[2:ncol(df)], function(x) SummarizeGrowth(df$time, x))
      
      df.predicted.plate <- data.frame(time = df$time)
      for (i in names(df[2:ncol(df)])) 
      {df.predicted.plate[[i]] <- predict(models.all[[i]]$model)}
      
      models.all <- lapply(df[2:ncol(df)], function(x) SummarizeGrowth(df[!is.na(x), 1], x[!is.na(x)]))
      df.predicted.plate <- data.frame(time = df$time)
      for (i in names(df[2:ncol(df)])) 
      {df.predicted.plate[[i]] <- predict(models.all[[i]]$model, newdata = list(t = df$time))}
      
      
      melt1 <- melt(df, id.vars = "time", variable.name = "Sample", value.name = "od")
      melt2 <- melt(df.predicted.plate, id.vars = "time", variable.name = "Sample", value.name = "pred.od")
      df.final <- cbind(melt1, pred.od=melt2[,3])
      rm(melt1)
      rm(melt2)
      
      
      fit <- ggplot(df.final, aes(x=time, y=od, group = Sample, col = Sample)) + 
        geom_point(aes(), alpha=0.5) + 
        geom_line(aes(y=pred.od), color="red")  + 
        theme_bw() #+ facet_grid(sample ~ . )
      
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') fit <- fit + facet_grid(facets)
      fit
      
    } else if (input$color_by == "Replicate") {
      
      df_rep <- data_mod() #%>% select(-Type) 
      print(df_rep)
      
      d<- df_rep %>%
        #gather(variable, value, -one_of("time","Layout")) %>% 
        unite(temp, Sample, Layout) %>% 
        group_by(temp) %>% 
        mutate(id=1:n()) %>% 
        spread(temp, Value) %>%
        select(-id)
      print(d)
      #d[] <- lapply(d, abs)
      
      
      summG <- function(x) {SummarizeGrowth(d$time,x)}
      lapply(d[2:ncol(d)], summG)
      models.all <- lapply(d[2:ncol(d)], function(x) SummarizeGrowth(d$time, x))
      
      df.predicted.plate <- data.frame(time = d$time)
      for (i in names(d[2:ncol(d)])) 
      {df.predicted.plate[[i]] <- predict(models.all[[i]]$model)}
      
      models.all <- lapply(d[2:ncol(d)], function(x) SummarizeGrowth(d[!is.na(x), 1], x[!is.na(x)]))
      df.predicted.plate <- data.frame(time = d$time)
      for (i in names(d[2:ncol(d)])) 
      {df.predicted.plate[[i]] <- predict(models.all[[i]]$model, newdata = list(t = d$time))}
      
      
      melt1 <- melt(d, id.vars = "time", variable.name = "sample", value.name = "od")
      melt2 <- melt(df.predicted.plate, id.vars = "time", variable.name = "sample", value.name = "pred.od")
      df.final <- cbind(melt1, pred.od=melt2[,3])
      rm(melt1)
      rm(melt2)
      
      df_final <-df.final %>%  separate(sample, c("Sample", "Layout"), "_")
      
      fit1<- ggplot(df_final, aes(x=time, y=od, group = Layout,col = factor(Layout),shape = Sample)) + 
        geom_point(aes(), alpha=0.5) + 
        geom_line(aes(y=pred.od,group=interaction(Sample,Layout)), color="red")  + 
        theme_bw() #+ facet_grid(Layout~Sample)
      
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') fit1 <- fit1 + facet_grid(facets)
      fit1
    }
    
  })
  
  output$expo_fit <- renderPlot({FitExpo() })
  
  
  ##### save plot FIT EXPO ####
  # When the save button is clicked, add the plot to a list and clear the input
  observeEvent(input$save_plot_btn_fit, {
    plot_name <- trimws(input$save_plot_name_fit)
    
    if (plot_name %in% names(values$plots)) {
      showModal(
        modalDialog(
          "You already have a plot saved with the same name. Saving this plot will override the existing plot.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_plot_duplicate_confirm", "OK",
                         class = "btn-primary")
          ),
          size = "m"
        )
      )
    } else {
      save_plot_fit()
    }
  })
  
  observeEvent(input$save_plot_duplicate_confirm, {
    save_plot_fit()
    removeModal()
  })
  save_plot_fit <- function() {
    shinyjs::show("save_plot_checkmark_fit")
    values$plots[[trimws(input$save_plot_name_fit)]] <- FitExpo() 
    #values$plots[[trimws(input$save_plot_name)]] <- GR_Plot() 
    updateTextInput(session, "save_plot_name_fit", value = "")
    shinyjs::delay(
      1000,
      shinyjs::hide("save_plot_checkmark_fit", anim = TRUE, animType = "fade")
    )
  }
  
  # Disable the "save" button if the plot name input is empty
  observe({
    shinyjs::toggleState("save_plot_btn_fit",
                         condition = nzchar(trimws(input$save_plot_name_fit)))
    
  })  
  
  #####
  
  ############## Growth Rate Plot TAB #############
  
  GR_Plot <- eventReactive({ 
    input$update
    input$update_plot
  }, {
    if(input$color_by == "Sample"){
      ggplot(GrowthRate_calc(), aes(x = factor(sample), y = r, col = sample)) + 
        geom_point() +
        theme_bw()+
        ggtitle(input$title) +labs( x = input$xlab, y = input$ylab) + ylim(input$minY,input$maxY)
    } else if(input$color_by == "Replicate"){
      data_plot<- GrowthRate_calc() %>% group_by(sample = sub("_[A-Z]$", "", sample)) # %>% summarise(mean_r = mean(r), sd_r = sd(r))
      ggplot(data_plot, aes(x = factor(sample), y = r, col = sample)) + 
        geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
        geom_jitter(width = 0.2) + 
        theme_bw() +
        ggtitle(input$title) +labs( x = input$xlab, y = input$ylab) + ylim(input$minY,input$maxY)
    }
    
  })
  
  output$plot_GR <- renderPlot({GR_Plot() })
  
  
  ### save Growth Rate Plot #####
  
  observeEvent(input$save_plot_btn_gr, {
    plot_name <- trimws(input$save_plot_name_gr)
    
    if (plot_name %in% names(values$plots)) {
      showModal(
        modalDialog(
          "You already have a plot saved with the same name. Saving this plot will override the existing plot.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_plot_duplicate_confirm", "OK",
                         class = "btn-primary")
          ),
          size = "m"
        )
      )
    } else {
      save_plot_gr()
    }
  })
  
  observeEvent(input$save_plot_duplicate_confirm, {
    save_plot_gr()
    removeModal()
  })
  save_plot_gr <- function() {
    shinyjs::show("save_plot_checkmark_gr")
    values$plots[[trimws(input$save_plot_name_gr)]] <- GR_Plot() 
    updateTextInput(session, "save_plot_name_gr", value = "")
    shinyjs::delay(
      1000,
      shinyjs::hide("save_plot_checkmark_gr", anim = TRUE, animType = "fade")
    )
  }  
  
  ####################
  
  
  ############# Growth Rate Max OD ############
  od_max <- eventReactive({ 
    input$update
    input$update_plot
  }, {
    max_od_df<- data_mod() %>% 
      group_by(Sample) %>%
      filter(Value == max(Value))%>%
      select(Sample,time, Value)
    max_od_df
  })
  
  od_max_plot <- eventReactive({ 
    input$update
    input$update_plot
  }, {
    od_max_plot <- semi_join(x=data_mod(), y=od_max(), by=c("time","Sample")) %>%
      group_by(Sample) %>%
      #summarise(mean = mean(Value))%>%
      ggplot(aes(Sample, Value, color = factor(Sample))) + 
      geom_point(size = 0.2) + 
      stat_summary( inherit.aes = TRUE,
                    fun.y = 'mean', fun.ymin = function(x) 0, geom = 'point', 
                    position=position_dodge()) +
      stat_summary( inherit.aes = TRUE, 
                    fun.y = mean,
                    fun.ymin = function(y) mean(y) - sd(y), 
                    fun.ymax = function(y) mean(y) + sd(y),
                    geom ="pointrange",show.legend = FALSE) +
      theme_bw() +
      ggtitle(input$title) +labs( x = input$xlab, y = input$ylab) + ylim(input$minY,input$maxY)
    
    od_max_plot
  })
  
  output$plot_OD <- renderPlot({od_max_plot() })
  output$od_max_df <- DT::renderDataTable({
    datatable(od_max(), options =  list( scrollX = T))})
 
  ###Download datatable with max OD
  output$downloadData_OD <- downloadHandler(
    filename = "max_OD.csv",
    content = function(file) {
      write.csv(od_max(), file)
    },
    contentType = "text/csv"
  )
  
  ###Download max_od PLOT ##
  
  observeEvent(input$save_plot_btn_OD, {
    plot_name <- trimws(input$save_plot_name_OD)
    
    if (plot_name %in% names(values$plots)) {
      showModal(
        modalDialog(
          "You already have a plot saved with the same name. Saving this plot will override the existing plot.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_plot_duplicate_confirm", "OK",
                         class = "btn-primary")
          ),
          size = "m"
        )
      )
    } else {
      save_plot_OD()
    }
  })
  
  observeEvent(input$save_plot_duplicate_confirm, {
    save_plot_OD()
    removeModal()
  })
  save_plot_OD <- function() {
    shinyjs::show("save_plot_checkmark_OD")
    values$plots[[trimws(input$save_plot_name_OD)]] <- od_max_plot() 
    updateTextInput(session, "save_plot_name_OD", value = "")
    shinyjs::delay(
      1000,
      shinyjs::hide("save_plot_checkmark_OD", anim = TRUE, animType = "fade")
    )
  }  
  
  ####################
  
  
  
  
  ##################
  
  
  
  # ----------- Export tab -----------
  
  # Create a server variable that we can use in the UI for a conditionalPanel
  output$saved_plots_exist <- reactive({
    length(values$plots) > 0
  })
  outputOptions(output, 'saved_plots_exist', suspendWhenHidden = FALSE)
  
  countNoun <- function(num, noun) {
    if (num == 1) paste0(num, " ", noun)
    else paste0(num, " ", noun, "s")
  }
  
  output$export_btn_ui <- renderUI({
    btn_text <- paste0("Download ",
                       countNoun(length(input$plots_order), "plot"),
                       " (", countNoun(export_num_pages(), "page"), ")")
    downloadButton("export_btn", btn_text)
  })
  
  # Select the plots and the order of the plots to export
  output$plots_order_ui <- renderUI({
    selectizeInput("plots_order", "Plots to export (drag to reorder)",
                   choices = names(values$plots), selected = names(values$plots),
                   multiple = TRUE, options = list(
                     plugins = list('drag_drop','remove_button')))
  })
  
  # If no plots are chosen to export, don't show all the export options
  observe({
    shinyjs::toggle(selector = "#exporting_plots_options, #preview_plots_options",
                    condition = length(input$plots_order) > 0)
  })
  
  # Show a dropdown to select which page to show
  output$plots_select_page_ui <- renderUI({
    num_pages <- export_num_pages()
    
    # Try to remain on the same page even when the dropdown changes
    isolate({
      if (!is.null(input$plots_select_page) &&
          as.numeric(input$plots_select_page) <= num_pages) {
        selected <- input$plots_select_page
      } else {
        selected <- 1
      }
    })
    selectInput("plots_select_page", "Page to preview",
                choices = seq(num_pages), selected = selected)
  })
  
  # Calculate the number of pages to export
  export_num_pages <- reactive({
    if (input$export_multiple) {
      plots_per_page <- input$export_nrow * input$export_ncol
      pages <- ceiling(length(input$plots_order) / plots_per_page)
    } else {
      pages <- length(input$plots_order)
    }
    pages
  })
  
  # print a specific page of plots (either 1 plot/page or multiple rows/cols)
  export_print_page <- function(page) {
    page <- as.numeric(page)
    if (!input$export_multiple) {
      plot_name <- input$plots_order[page]
      values$plots[plot_name]
    } else {
      plots_per_page <- input$export_nrow * input$export_ncol
      idx_start <- (page - 1) * plots_per_page + 1
      idx_end <- min(length(input$plots_order), page * plots_per_page)
      if (idx_start > idx_end) {
        return()
      }
      plot_names <- input$plots_order[idx_start:idx_end]
      plots <- values$plots[plot_names]
      
      gridExtra::grid.arrange(
        grobs = plots,
        nrow = input$export_nrow,
        ncol = input$export_ncol,
        as.table = (input$export_arrangement == "byrow")
      )
    }
  }
  
  # Show a dropdown to select a plot to remove in the Export tab
  output$plots_remove_ui <- renderUI({
    selectInput("plots_remove", NULL, names(values$plots))
  })
  
  # Preview a plot in the Export tab
  output$plot_preview <- renderPlot({
    if (is.null(input$plots_select_page)) {
      return()
    }
    export_print_page(input$plots_select_page)
  },
  width = function() { plot_preview_width() },
  height = function() { plot_preview_height() })
  
  # Return the dimensions of the PDF page selected in the Export tab
  pdf_page_dim <- reactive({
    if (input$export_pdf_orientation == "landscape") {
      width <- 11
      height <- 8.5
    } else if (input$export_pdf_orientation == "portrait") {
      width <- 8.5
      height <- 11
    } else {
      width <- input$export_pdf_width
      height <- input$export_pdf_height
    }
    list(width = width, height = height)
  })
  
  # Calculate the dimensions of the plot preview
  plot_preview_dim <- reactive({
    # If it's PDF, the units are inches and default resolution is 72 px/inch
    if (input$export_file_type == "pdf") {
      width <- pdf_page_dim()$width * 72
      height <- pdf_page_dim()$height * 72
    } else {
      width <- input$export_file_width
      height <- input$export_file_height
    }
    
    # Keep the aspect ratio, but make the max dimensions 500
    ratio <- height/width
    if (ratio > 1) {
      height <- 500
      width <- height/ratio
    } else {
      width <- 500
      height <- ratio*width
    }
    
    list(width = width, height = height)
  })
  plot_preview_width <- reactive({
    plot_preview_dim()$width
  })
  plot_preview_height<- reactive({
    plot_preview_dim()$height
  })
  
  # Remove the currently selected plot from the saved plots list
  observeEvent(input$remove_plot_btn, {
    values$plots[[input$plots_remove]] <- NULL
  })
  
  # Determine the file name of the exported plots file.
  # If there's only one plot or using PDF, export it in its raw format.
  # Multiple plots in non-PDF format are zipped together.
  export_file_name <- reactive({
    if (export_num_pages() == 1 || input$export_file_type == "pdf") {
      paste0("export_plots", ".", input$export_file_type)
    } else {
      paste0("export_plots", ".zip")
    }
  })
  
  # Download the saved plots
  output$export_btn <- downloadHandler(
    filename = function() {
      export_file_name()
    },
    content = function(file) {
      tryCatch({
        file_type <- input$export_file_type
        
        # If saving as PDF, save all pages in one file
        if (file_type == "pdf") {
          width <- pdf_page_dim()$width
          height <- pdf_page_dim()$height
          
          file_names <- "export_plots.pdf"
          grDevices::pdf(file = file_names, width = width, height = height,
                         title = file_names, onefile = TRUE)
          
          if (!input$export_multiple) {
            plots <- values$plots[input$plots_order]
            invisible <- lapply(plots, print)
          } else {
            num_pages <- export_num_pages()
            for (page in seq(num_pages)) {
              suppressMessages(print(export_print_page(page)))
            }
          }
          
          grDevices::dev.off()
        }
        # If saving as raw images, save each page as a separate file
        else {
          num_pages <- export_num_pages()
          for (page in seq(num_pages)) {
            export_print_page(page)
          }
          file_names <- lapply(seq(num_pages), function(page) { 
            file_name <- paste0("export_plots_p", page, ".", file_type)
            export_params <- list(file_name,
                                  width = input$export_file_width,
                                  height = input$export_file_height)
            do.call(file_type, export_params)
            print(export_print_page(page))
            grDevices::dev.off()
            file_name
          })
          file_names <- unlist(file_names)
        }
        
        # If there's a single file, download the file. If multiple files, zip
        if (length(file_names) == 1) {
          file.copy(file_names, file, overwrite = TRUE)
        } else {
          zip(file, file_names)
        }
        
        # Remove the generated files so that we don't run out of disk space :)
        
        file.remove(file_names)
      },
      error = function(err) {
        stop(err$message)
      })
    }
  )
  
  
  
})
