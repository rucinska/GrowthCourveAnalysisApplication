#### GROWTH RATE APP #####
library(shinyFiles)
library(shiny)
library(ggplot2)
library(shinydashboard)
library(readxl)
library(plotly)

inline_ui <- function(tag) {
  div(style = "display: inline-block", tag)
}


shinyUI(
  
  fluidPage(
    shinyjs::useShinyjs(),
    dashboardPage(skin = "black",
                  #HEADER
                  dashboardHeader(title = "Calculate Growth Rate"),
                  #SLIDER
                  dashboardSidebar(
                    sidebarMenu(
                      
                      id = "tabs",
                      menuItem("Data", tabName = "data", icon = icon("table")),
                      menuItem("Plot", tabName = "plot", icon = icon("bar-chart-o")),
                      menuItem("Export", tabName = "export", icon = icon("export", lib = "glyphicon"))
                    )
                  ),
                  #BODY
                  dashboardBody(
                    tags$head(tags$style(HTML('.shiny-split-layout>div {overflow:visible}'))),
                    tags$style(HTML("
                                    
                                    .popover{
   background-color:#b94a48;
                                    border:none;
                                    border-radius:unset;
                                    min-width:100px;
                                    width:100%;
                                    max-width:400px;
                                    overflow-wrap:break-word;
                                    }
                                    .box.box-solid.box-primary>.box-header {
                                    color:#fff;
                                    background:#666666
                                    }
                                    
                                    .box.box-solid.box-primary{
                                    border-bottom-color:#666666;
                                    border-left-color:#666666;
                                    border-right-color:#666666;
                                    border-top-color:#666666;
                                    }
                                    
                                    ")),
                    
                    
                    tabItems(
                      tabItem(  
                        tabName = "data",    
                        fluidRow( 
                          box(width = 5, title = "Upload Data",
                              shinyjs::useShinyjs(),
                              div(
                                id = "side-panel",
                                # shinyDirButton("Btn_GetFolder", "Choose a folder" ,
                                #                title = "Please select a folder:",
                                #                buttonType = "default", class = NULL),
                                fileInput('file1', 'Choose Files', multiple = TRUE,
                                          accept = c("text/xml")),
                                hr(),
                                #option to remove first/last col and row
                                checkboxInput("removewater", "Remove water", value = TRUE),
                                #checkboxInput("mean_rep", "Take average for replicates", value = FALSE),
                                box(width = 12,title = "Assign columns",
                                    br(),
                                    
                                    column(
                                      width = 3,
                                      splitLayout(
                                        cellWidths = 100,
                                        cellArgs = list(style = "margin-top:-2em"),
                                        
                                        h5(textInput(inputId = "name1", label = "Name", value = "Sample1"),style="display: inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "sam1", label = "Sample 1",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "bla1", label = "Blank 1",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                                      ),
                                      splitLayout(
                                        cellWidths = 100,
                                        cellArgs = list(style = "white-space: nowrap;  margin-top: -5px;"),                                        
                                        
                                        h5(textInput(inputId = "name2", label = "Name", value = "Sample2"),style="display: inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "sam2", label = "Sample 2",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "bla2", label = "Blank 2",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                                      ),
                                      splitLayout(
                                        cellWidths = 100,
                                        cellArgs = list(style = "white-space: nowrap; margin-top: -5px;"),                                        
                                        
                                        h5(textInput(inputId = "name3", label = "Name", value = "Sample3"),style="display: inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "sam3", label = "Sample 3",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "bla3", label = "Blank 3",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                                      ),
                                      splitLayout(
                                        cellWidths = 100,
                                        cellArgs = list(style = "white-space: nowrap; margin-top: -5px;"),                                        
                                        
                                        
                                        h5(textInput(inputId = "name4", label = "Name", value = "Sample4"),style="display: inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "sam4", label = "Sample 4",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "bla4", label = "Blank 4",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                                      ),
                                      splitLayout(
                                        cellWidths = 100,
                                        cellArgs = list(style = "white-space: nowrap; margin-top: -5px;"),                                        
                                        
                                        h5(textInput(inputId = "name5", label = "Name", value = "Sample5"),style="display: inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "sam5", label = "Sample 5",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "bla5", label = "Blank 5",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                                      ),
                                      splitLayout(
                                        cellWidths = 100,
                                        cellArgs = list(style = "white-space: nowrap; margin-top: -5px;"),                                        
                                        
                                        h5(textInput(inputId = "name6", label = "Name", value = "Sample6"),style="display: inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "sam6", label = "Sample 6",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "bla6", label = "Blank 6",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                                      ),
                                      splitLayout(
                                        cellWidths = 100,
                                        cellArgs = list(style = "white-space: nowrap; margin-top: -5px;"),                                        
                                        
                                        h5(textInput(inputId = "name7", label = "Name", value = "Sample7"),style="display: inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "sam7", label = "Sample 7",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "bla7", label = "Blank 7",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                                      ),
                                      splitLayout(
                                        cellWidths = 100,
                                        cellArgs = list(style = "white-space: nowrap; margin-top: -5px;"),                                        
                                        
                                        h5(textInput(inputId = "name8", label = "Name", value = "Sample8"),style="display: inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "sam8", label = "Sample 8",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "bla8", label = "Blank 8",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                                      ),
                                      splitLayout(
                                        cellWidths = 100,
                                        cellArgs = list(style = "white-space: nowrap; margin-top: -5px;"),                                        
                                        
                                        h5(textInput(inputId = "name9", label = "Name", value = "Sample9"),style="display: inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "sam9", label = "Sample 9",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "bla9", label = "Blank 9",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                                      ),
                                      splitLayout(
                                        cellWidths = 100,
                                        cellArgs = list(style = "white-space: nowrap; margin-top: -5px;"),                                        
                                        
                                        h5(textInput(inputId = "name10", label = "Name", value = "Sample10"),style="display: inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "sam10", label = "Sample 10",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                                        h5(selectInput(inputId = "bla10", label = "Blank 10",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                                      )
                                      
                                    )#end column
                                ), #end box
                                
                                
                                #textInput("text1", "Sample1 Name", "" ),
                                
                                actionButton("update", "Update", class = "btn-primary",style='padding:4px; font-size:120%'),
                                actionButton("reset", "Reset", class = "btn-primary",style='padding:4px; font-size:120%')
                                
                                
                              )
                          ),
                          box(width = 7, title = "Data", solidHeader = TRUE,
                              
                              div(style = "white-space: nowrap;", 
                                  DT::dataTableOutput("contents1"),
                                  hr(),
                                  uiOutput(outputId = "n"),
                                  hr(),
                                  downloadButton("downloadData", "Download"))
                              
                          )
                          
                          
                          
                        ) # end FluidRow
                        
                      ),
                      
                      tabItem( tabName = "plot",
                               
                               fluidRow(
                                 box(width = 3, title = "Customise your plot", solidHeader = TRUE,
                                     column(12, 
                                            selectInput("color_by", "Color by", c("Sample","Replicate"), selected = "Replicate"),
                                            conditionalPanel(
                                              condition = "input.color_by == 'Replicate'",  selectInput("replicates", "Select replicates to display.",c(), multiple = TRUE)),
                                            conditionalPanel(
                                              condition = "input.color_by == 'Sample'",  selectInput("samples", "Select samples to display.",c(), multiple = TRUE))),
                                     # conditionalPanel(
                                     #   condition = "input.tabs_id == c('plot1','expo')",  checkboxInput("log", "Log y axes", value = FALSE)
                                     #                    ),
                                     conditionalPanel(
                                       condition = ("input.tabs_id == 'expo'|| input.tabs_id == 'plot1'|| input.tabs_id == 'zoom'"), 
                                       checkboxInput("log", "Log y axes", value = FALSE),
                                       checkboxInput("well", "Delete particular well", value = FALSE),
                                       checkboxInput("norm", "Normalise data to zero", value = FALSE)
                                     ),
                                     
                                     
                                     
                                     conditionalPanel(
                                       condition = "input.well == true",  selectInput("wells", "Select wells to display.",c(), multiple = TRUE)),
                                     
                                     
                                     textInput('title', 'Plot Title'),
                                     textInput('xlab', 'X axis label'),
                                     conditionalPanel(
                                                     condition = "input.tabs_id != 'gr'",
                                                        textInput('ylab', 'Y axis label', value = "OD")
                                     ),
                                     #conditionalPanel(
                                       #condition = "input.tabs_id == 'gr'",
                                       numericInput(inputId = "minY", label = "Minimum y value",value =  0),
                                       numericInput(inputId = "maxY", label = "Maximum y value", value = 1),
                                     #),
                                     conditionalPanel(
                                       condition = ("input.tabs_id == 'expo'|| input.tabs_id == 'plot1'|| input.tabs_id == 'zoom'"),
                                     
                                     selectInput('facet_row', 'Facet Row', c(None = '.',"Sample", "Layout")),
                                     selectInput('facet_col', 'Facet Column', c(None = '.', "Sample", "Layout")),
                                     
                                     sliderInput("time", "Time", value = c(0, 100), min = 0, max = 100)
                                     ),
                                     actionButton("update_plot", "Update", class = "btn-primary",style='padding:4px; font-size:120%')
                                 ),
                                 
                                 
                                 box(width = 9, title = "Plot", solidHeader = TRUE, column(
                                   12,
                                   tabsetPanel(id ="tabs_id", type = "tabs", 
                                               tabPanel("Plot", value="plot1",
                                                        plotOutput('plot'),
                                                        div(
                                                          id = "save_plot_area",
                                                          inline_ui(
                                                            textInput("save_plot_name", NULL, "",
                                                                      placeholder = "Enter plot name to save")
                                                          ),
                                                          actionButton("save_plot_btn", "Save plot", icon = icon("star")),
                                                          shinyjs::hidden(
                                                            span(
                                                              id = "save_plot_checkmark",
                                                              icon("check")
                                                            )
                                                          )
                                                        ),
                                                        conditionalPanel(condition = "input.update_plot" , 
                                                                         h4("Growth Curve Analysis"),
                                                        div(style = "white-space: nowrap;", 
                                                            DT::dataTableOutput("summary1")),
                                                            hr(),
                                                            uiOutput(outputId = "n_gr"),
                                                            hr(),
                                                            downloadButton("downloadData_gr_cal", "Download"))),
                                               
                                               tabPanel("Check your fit",value="expo",
                                                        plotOutput('expo_fit', height=700),
                                                        div(
                                                          id = "save_plot_area",
                                                          inline_ui(
                                                            textInput("save_plot_name_fit", NULL, "",
                                                                      placeholder = "Enter plot name to save")
                                                          ),
                                                          actionButton("save_plot_btn_fit", "Save plot", icon = icon("star")),
                                                          shinyjs::hidden(
                                                            span(
                                                              id = "save_plot_checkmark_fit",
                                                              icon("check")
                                                            )
                                                          )
                                                        )
                                                        
                                                        #verbatimTextOutput("info")
                                                        #DT::dataTableOutput("plot_brushed_points")
                                               ),
                                               
                                               tabPanel("Growth Rate Plot",value="gr",
                                                        plotOutput('plot_GR', height=700),
                                                        div(
                                                          id = "save_plot_area",
                                                          inline_ui(
                                                            textInput("save_plot_name_gr", NULL, "",
                                                                      placeholder = "Enter plot name to save")
                                                          ),
                                                          actionButton("save_plot_btn_gr", "Save plot", icon = icon("star")),
                                                          shinyjs::hidden(
                                                            span(
                                                              id = "save_plot_checkmark_gr",
                                                              icon("check")
                                                            )
                                                          )
                                                        ),
                                                        plotOutput('test', height=700)
                                                        #verbatimTextOutput("info")
                                                        #DT::dataTableOutput("plot_brushed_points")
                                               ),
                                               tabPanel("Growth Rate Max OD",value="OD",
                                                        fluidRow(
                                                          column(7,plotOutput('plot_OD', height=700),
                                                                 div(
                                                                   id = "save_plot_area",
                                                                   inline_ui(
                                                                     textInput("save_plot_name_OD", NULL, "",
                                                                               placeholder = "Enter plot name to save")
                                                                   ),
                                                                   actionButton("save_plot_btn_OD", "Save plot", icon = icon("star")),
                                                                   shinyjs::hidden(
                                                                     span(
                                                                       id = "save_plot_checkmark_OD",
                                                                       icon("check")
                                                                     )
                                                                   )
                                                                 ) ),
                                                          column(5, DT::dataTableOutput("od_max_df"),
                                                                 hr(),
                                                                 downloadButton("downloadData_OD", "Download"))
                                                        )
                                                        
                                                        #plotOutput('OD', height=700)
                                                        #verbatimTextOutput("info")
                                                        #DT::dataTableOutput("plot_brushed_points")
                                               ),
                                               
                                               tabPanel("Growth Rate ZOOM",value="zoom",
                                                        fluidRow(
                                                          column(8, plotOutput('zoom_plot', height = 300,
                                                                               brush = brushOpts(
                                                                                 id = "zoom_plot_brush",
                                                                                 resetOnNew = TRUE
                                                                               ))),
                                                          column(width = 4,  plotOutput("zoom_plot_2", height = 300),
                                                                 div(
                                                                   id = "save_plot_area",
                                                                   inline_ui(
                                                                     textInput("save_plot_name_zoom2", NULL, "",
                                                                               placeholder = "Enter plot name to save")
                                                                   ),
                                                                   actionButton("save_plot_btn_zoom2", "Save plot", icon = icon("star")),
                                                                   shinyjs::hidden(
                                                                     span(
                                                                       id = "save_plot_checkmark_zoom2",
                                                                       icon("check")
                                                                     )
                                                                   )
                                                                 ),
                                                                 actionButton("AddToTable","Add to table", class = "btn-primary",style='padding:4px; font-size:120%')
                                                          ),
                                                          hr(),
                                                          fluidRow(
                                                            conditionalPanel(condition = "input.update_plot" , h4("Growth Curve Analysis")),
                                                            
                                                            column(width =12, 
                                                                   
                                                                   DT::dataTableOutput("info"),
                                                                   hr(),
                                                                   numericInput("Delete", "Delete row:", 1, step = 1),
                                                                   actionButton("Go", "Delete!"),
                                                                   hr(),
                                                                   downloadButton("downloadData_GR", "Download"))
                                                            
                                                          ),
                                                          hr(),
                                                          fluidRow(
                                                            conditionalPanel(condition = "input.update_plot" , h4("Growth Rate Comparison Plot")),
                                                            
                                                            column(width =12, plotOutput("zoom_plot_GR", height = 300),
                                                                   div(
                                                                     id = "save_plot_area",
                                                                     inline_ui(
                                                                       textInput("save_plot_name_zoomGR", NULL, "",
                                                                                 placeholder = "Enter plot name to save")
                                                                     ),
                                                                     actionButton("save_plot_btn_zoomGR", "Save plot", icon = icon("star")),
                                                                     shinyjs::hidden(
                                                                       span(
                                                                         id = "save_plot_checkmark_zoomGR",
                                                                         icon("check")
                                                                       )
                                                                     )
                                                                   )
                                                            )
                                                          )
                                                        )
                                               ),
                                               
                                               tabPanel("Growth Rate vol2",value="gr2",
                                                        fluidRow(
                                                          column(12, plotOutput('plotGR_v2', height = 300,
                                                                                brush = brushOpts(
                                                                                  id = "plot2_brush",
                                                                                  resetOnNew = TRUE)),
                                                                 div(
                                                                   id = "save_plot_area",
                                                                   inline_ui(
                                                                     textInput("save_plot_name5", NULL, "",
                                                                               placeholder = "Enter plot name to save")
                                                                   ),
                                                                   actionButton("save_plot_btn5", "Save plot", icon = icon("star")),
                                                                   shinyjs::hidden(
                                                                     span(
                                                                       id = "save_plot_checkmark5",
                                                                       icon("check")
                                                                     )
                                                                   )
                                                                   
                                                                 ),
                                                                 fluidRow(
                                                                   # column(width = 6, plotOutput("plotGR_v3", height = 300),
                                                                   #        div(
                                                                   #          id = "save_plot_area",
                                                                   #          inline_ui(
                                                                   #            textInput("save_plot_name_gr3", NULL, "",
                                                                   #                      placeholder = "Enter plot name to save")
                                                                   #          ),
                                                                   #          actionButton("save_plot_btn_gr3", "Save plot", icon = icon("star")),
                                                                   #          shinyjs::hidden(
                                                                   #            span(
                                                                   #              id = "save_plot_checkmark_gr3",
                                                                   #              icon("check")
                                                                   #            )
                                                                   #          )
                                                                   #        )
                                                                   # ),
                                                                   column(width =12, DT::dataTableOutput("tableGR_v2"))
                                                                 )
                                                          )
                                                        )
                                                        # fluidRow(column(12, plotOutput('plotGR_v2', height = 300,
                                                        #                                brush = brushOpts(
                                                        #                                  id = "plot2_brush",
                                                        #                                  resetOnNew = TRUE
                                                        #                                ))),
                                                        #          fluidRow(column(width = 6, plotOutput("plotGR_v3", height = 300),
                                                        #                          column(width =6, DT::dataTableOutput("tableGR_v2"))
                                                        #          ))
                                                        # ),
                                                        
                                                        #verbatimTextOutput("info")
                                                        #DT::dataTableOutput("plot_brushed_points")
                                               )
                                               
                                               # plotOutput('plot', brush=brushOpts("plot_brush",resetOnNew=T)),
                                               # div(
                                               #   id = "save_plot_area",
                                               #   inline_ui(
                                               #     textInput("save_plot_name", NULL, "",
                                               #               placeholder = "Enter plot name to save")
                                               #   ),
                                               #   actionButton("save_plot_btn", "Save plot", icon = icon("star")),
                                               #   shinyjs::hidden(
                                               #     span(
                                               #       id = "save_plot_checkmark",
                                               #       icon("check")
                                               #     )
                                               #   )
                                               # ),
                                               # conditionalPanel(condition = "input.update_plot" , h4("Growth Curve Analysis")),
                                               # 
                                               # DT::dataTableOutput("summary1"),
                                               # #verbatimTextOutput("brush_info"),
                                               # #wellPanel(width=9,h4("Select points to calculate Growth Rate:"),DT::dataTableOutput("plot_brushed_points")),
                                               # 
                                               # conditionalPanel(condition = "input.update_plot" , h4("Growth Rate Plot")),
                                               # plotOutput('plot_GR')
                                               # 
                                   ))) #end box
                                 
                               )
                      ), #end tabItem
                      tabItem( tabName = "export",
                               fluidRow(      conditionalPanel(
                                 condition = "!output.saved_plots_exist",
                                 h2("You do not have any saved plots to export")
                               ),
                               conditionalPanel(
                                 condition = "output.saved_plots_exist",
                                 fluidRow(
                                   column(
                                     4,
                                     h2("Export Options"),
                                     div(
                                       id = "exporting_plots_options",
                                       selectInput("export_file_type", "File type",
                                                   c("PDF" = "pdf", "JPEG" = "jpeg", "PNG" = "png")),
                                       conditionalPanel(
                                         condition = "input.export_file_type == 'pdf'",
                                         selectInput("export_pdf_orientation", "Page orientation",
                                                     c("Portrait (8.5\" x 11\")" = "portrait",
                                                       "Landscape (11\" x 8.5\")" = "landscape",
                                                       "Custom dimensions" = "custom")
                                         ),
                                         conditionalPanel(
                                           condition = "input.export_pdf_orientation == 'custom'",
                                           numericInput("export_pdf_width", "Page width (inches)",
                                                        value = 8.5, min = 1, max = 50, step = 0.5),
                                           numericInput("export_pdf_height", "Page height (inches)",
                                                        value = 11, min = 1, max = 50, step = 0.5)
                                         )
                                       ),
                                       conditionalPanel(
                                         condition = "input.export_file_type != 'pdf'",
                                         numericInput("export_file_width", "Image width (pixels)",
                                                      value = 480, min = 100, max = 2000),
                                         numericInput("export_file_height", "Image height (pixels)",
                                                      value = 480, min = 100, max = 2000)
                                       ),
                                       checkboxInput("export_multiple", "Multiple plots per page"),
                                       conditionalPanel(
                                         condition = "input.export_multiple",
                                         selectInput("export_arrangement", NULL,
                                                     c("Arrange plots by row" = "byrow",
                                                       "Arrange plots by column" = "bycol")),
                                         numericInput("export_nrow", "Rows per page",
                                                      value = 1, min = 1, max = 20),
                                         numericInput("export_ncol", "Columns per page",
                                                      value = 1, min = 1, max = 20)
                                         
                                       ),
                                       uiOutput("export_btn_ui")
                                     )
                                   ),
                                   column(
                                     8,
                                     h2("Preview"),
                                     strong("Remove plot"), br(),
                                     inline_ui(uiOutput("plots_remove_ui")),
                                     actionButton("remove_plot_btn", "Remove"),
                                     uiOutput("plots_order_ui"),
                                     div(
                                       id = "preview_plots_options",
                                       uiOutput("plots_select_page_ui"),
                                       plotOutput("plot_preview", height = "auto")
                                     )
                                   )
                                 )
                               )))
                    )
                    )
                    )
                  ))
