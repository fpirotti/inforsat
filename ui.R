# Define UI for application that draws a histogram
#,
library(shinydashboardPlus)
library(shinyWidgets)
##required for ui
tt <- table(images.lut$tile)
tiles.labels = sprintf("%s (%s)", names(tt) , tt ) 
tt <- names(tt)
names(tt)<- tiles.labels

ui <-  dashboardPage(
  #skin = "black",updateBox
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#000000"),
  header = dashboardHeader( title = HTML("InforSAT"),
                            leftUi = tagList(
                              
                              actionBttn("showHelpPanel", "Help", 
                                         icon("book", verify_fa=F), size="sm", 
                                         style = "simple" ),
                              
                             
                              dropdown(  
                                div(id="legendPlaceholder", style=" margin:0 -10px;") ,
                                 
                                style = "simple", 
                                icon = icon("table"),  size = "sm",
                                width = "225px",  label = "Layers",
                                animate = animateOptions(
                                  enter = animations$fading_entrances$fadeIn,
                                  exit = animations$fading_exits$fadeOut,
                                  duration=0.5
                                )
                              ), 
                              actionBttn("showAnalysisPanel", "Multi-temporal Plot", icon = icon("chart-line", verify_fa=F), size="sm", 
                                         style = "simple" ),
                              actionBttn("showNotificationPanel", "Logs&Notifications", size="sm", style = "simple" ),
                              
                              
                              dropdownButton(
                                circle = F, status = "primary",
                                icon = icon("download"), width = "300px",
                                
                                tooltip = tooltipOptions(title = "Click to see download options"),
                                div( title="Download vector file with the areas that were drawn in the map as polygons. ", 
                                     downloadButton("scaricaPoligoni", "Polygons")
                                ),
                                div( title="Download raster of index ", 
                                     downloadButton("scaricaIndice", "Index Raster")
                                ),
                                div( title="Downloads all overlays (composite, index, cloud cover...) in PNG format ", 
                                     actionButton("scaricaImmagini", "Canvas images")
                                ), 
                                div( title="Download an excel file with all values extracted from pixels inside the areas drawn in the map from all the images at different dates. ", 
                                     downloadButton("scaricaTabellaValori", "Index Values Table") 
                                )
                              )
                              
                            ) ), 
  sidebar=dashboardSidebar (
    
    collapsed = FALSE,
    minified = F,
    # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
    #                   label = "Search..."),
    div(title="Select what image you want to render, in parenthesis are the number of available dates for that image tile", 
        selectInput(inputId = 'tile',
                label = 'Choose tile', 
                choices = c("", tt) ) ),
    
    div(title="Select what image date you want to render", 
    selectInput(inputId = 'dayselected',
                label = 'Choose Image Date', 
                choices = NULL ) 
    ),
    
    selectInput(inputId = 'resampling',
                label = 'Resampling method',
                choices=c("near","bilinear","cubic","cubicspline","lanczos","average","mode","max") ,
                  selected="bilinear"
    ),
    
    div(title="<b>Fixed</b> fixes the scale range of the index map so that it does not change depending on min max values of the created index map.
       <br> <b>Auto</b> calculates an ideal scale range using 10/90 percentiles of frequency histogram",
        switchInput( 
          inputId = "freezeScale", offStatus="success",
          label = "Color&nbsp;Scale",
          offLabel = "Auto",
          onLabel = "Fixed", 
          value=F  , handleWidth=230
        ) 
    ),
    
    div(actionBttn(inputId = "aggiorna", label = "Redraw Index Map", icon=icon("recycle"), 
               style="simple", color = "primary", size="sm"), title="<b>Index map is only updated on demand</b> because it is created on the fly from the sensor imagery
        So make sure that you redraw it if you zoom or pan your map." ),
 
    div(style="  margin:5px; padding:5px; border-top:solid 2px white; ", 
      div(style="font-weight: 700;", "Multi-temporal Analysis"),
      div("Parallelize over Multiple Cores"),
      fluidRow(  title="Use parallel computing ",

      column(5, 
             div(    
                  switchInput("parallel", label = "<i class=\"fa fa-bars fa-rotate-90\"></i>", onLabel = "Yes", offLabel = "No",   value = FALSE) 
                  ) 
             ),
      column(7, title="Number of Cores (max 4 for guests, 32 for admin)",    numericInput("nCores", NULL, 2, 1, 4, 1) )  
    ),  
       div(title="If areas have been added by user, this tool extracts and plots multi-temporal data from all available images.", 
           actionButton("calcola",  "Index Values in Areas") )
      )
   
  ),
  
  controlbar = dashboardControlbar( width = 500,
    controlbarMenu(
      id = "cb_menu",
      controlbarItem(
        "Index Panel",
        selectizeInput(
          "indici",
          width = "100%",
          label = "Index",
          choices = radio2expression,
          selected = radio2expression[["NDVI - Normalized Difference Vegetation Index"]]
        ) , 
        div( textInput("indici_formula", label = "Formula", placeholder = "custom index", ) , title="You can define your index here NOT YET ACTIVE!!"),
        div("Please use the following band names when creating your formula: B01,B02,...,B8A,...B12. NOT B09 and B01, these are not yet implemented")

      ),
      
      controlbarItem(
         "Color Composition",
        selectizeInput(
          "composite",
          label = "Band combinations",
          choices = processingComposite,
          selected = character(0)
        ),
        
        #div(title="dddd",
            plotlyOutput(outputId = "bandHistogram")
       # ),
        
        
      ),
      controlbarItem(
        "Filters",
        knobInput(
          inputId = "mskCld",
          label = "Filtra Nuvole", 
          min=0, max=100, value=0
        ),
        
        knobInput(
          inputId = "mskSnw",
          label = "Filtra Neve", 
          min=0, max=100, value=0
        )
      ),
      controlbarItem(
        "Infolog",
        div(id="infolog", style="width:100%; height:500px; overflow-y:auto; padding:5px;")
      )
    )
     
    
  ),
  dashboardBody ( 
    
    chooseSliderSkin("Modern"),
    setSliderColor(c("DarkSlateGray","DarkSlateGray"), c(1, 2)),
    shinyalert::useShinyalert(force=T), 
    shinyjs::useShinyjs(),
    
    tags$link(rel = "stylesheet", type = "text/css", href = "mycss2.css?v=sdfgdefsd"),
    
    tags$link(rel="stylesheet", type="text/css", href="jquery.qtip.min.css"),
    
    tags$head(tags$script(src="myfuncts.js?v=3dsddf")) , 
    tags$head(tags$script(src="html2canvas.min.js")) , 
    
    tags$head(tags$script(  src="jquery.qtip.min.js")),
    
     
    
    jqui_draggable(  
      column(12, style="displan:none; position:absolute;z-index:999999999; width:calc( 100vw - 60px ); top:5px; left:10px;", 
          box( id= "myBoxAnalytics",
                                   title = HTML(paste0(icon("chart-line"), " Multi-temporal Analytics                ")), 
                                   closable = TRUE,  width = NULL,
                                   collapsed = F,
                                   enable_label = T, 
                                   label_text = "",
                                   label_status = "danger",
                                   status = "warning",
                                   solidHeader = TRUE,
                                   collapsible = TRUE, 
                    #               dygraphOutput("dygraph"),
                                   fluidRow(
                                     column(3, div(title="Scale X axis as dates or as groups.", switchInput("xPlotAxis", onLabel="Text", offLabel = "Date",   width="100%"  ) ),
                                            selectInput("plotType", NULL, c("Boxplot+Mean/SD", "Line (mean)+SD"), "Line (mean)"),   
                                            actionBttn("redrawAnalysisPlot", "RePlot", icon=icon("recycle"), 
                                                          size="sm", style = "simple", color = "primary")),
                                     column(3,   sliderInput("cloudsInPlot", "CLOUD tolerance:",
                                               min = 0, max = 100, value = 100)),
                                     column(3,   sliderInput("snowInPlot", "SNOW tolerance:",
                                                             min = 0, max = 100, value = 100  )),
                                     column(3,   pickerInput("datesInPlot", "Dates", multiple=T, 
                                                             choices=NULL, options = pickerOptions(liveSearch=T, size=10, selectedTextFormat="count" ) ))
                                   ),
                                  addSpinner( plotlyOutput("graph1", width="calc( 100vw - 110px )" ) )
          ) 
      ),
      options = list(
        cursor = "move",
        zIndex = 99999999999,
        stack = ".container",
        opacity = 0.5,
        handle = ".box-header"
      )
    ),
    
    leaflet::leafletOutput("mymap"),
    
  )
  
)
