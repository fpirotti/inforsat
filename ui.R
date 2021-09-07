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
                              actionBttn("showAnalysisPanel", "Analysis Panel", size="sm", style = "simple" ),
                              actionBttn("showNotificationPanel", "Logs&Notifications", size="sm", style = "simple" )
                            ) ), 
  sidebar=dashboardSidebar (
    
    collapsed = FALSE,
    minified = F,
    # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
    #                   label = "Search..."),
    selectInput(inputId = 'tile',
                label = 'Choose tile', 
                choices = c("", tt) ),
    selectInput(inputId = 'dayselected',
                label = 'Choose Image Date', 
                choices = NULL ),
    
    selectInput(inputId = 'resampling',
                label = 'Resampling method (GDAL)',
                choices=c("near","bilinear","cubic","cubicspline","lanczos","average","mode","max") 
    ),
    
    div(title="Fix scale so that it does not change depending on min max values. Blocca ad una scala lineare i valori dell'indice, utile per eseguire confronti",
        switchInput( 
          inputId = "freezeScale", offStatus="success",
          label = "Color&nbsp;Scale",
          offLabel = "Fixed",
          onLabel = "Auto", 
          value=T  , handleWidth=230
        ) 
    ),
    
    actionBttn(inputId = "aggiorna", label = "Redraw", icon=icon("recycle"), 
               style="simple", color = "primary", size="sm"),
    actionButton("calcola", "Process areas for PLOT") ,
    fluidRow( style="border:1px dotted white; margin:1px;", title="Use parallel computing ",
      column(6, 
             div(   style="display:inline-block",
                  switchInput("parallel", label = "<i class=\"fa fa-bars fa-rotate-90\"></i>", onLabel = "Yes", offLabel = "No",   value = FALSE) 
                  ) 
             ),
      div( title="Number of Cores (max 12)",  column(6,  numericInput("nCores", NULL, 4, 1, 12, 1) ) )
    ), 
    div( title="Download vector file with the areas that were drawn in the map as polygons. ", 
       downloadButton("scaricaPoligoni", "Polygons")
       ),
    #downloadButton("scaricaIndice", "Raster Indice"),
    div( title="Download an excel file with all values extracted from pixels inside the areas drawn in the map from all the images at different dates. ", 
         downloadButton("scaricaTabellaValori", "Index Values Table") 
         ) 
  ),
  
  controlbar = dashboardControlbar( width = 500,
    controlbarMenu(
      id = "cb_menu",
      controlbarItem(
        "Indici",
        selectizeInput(
          "indici",
          width = "100%",
          label = "Indici",
          choices = radio2expression,
          selected = character(0)
        ) , 
        div( textInput("indici_formula", label = "Formula", placeholder = "custom index", ) , title="You can define your index here NOT YET ACTIVE!!"),
        div("Please use the following band names when creating your formula: B01,B02,...,B8A,...B12")

      ),
      
      controlbarItem(
         "Composite",
        selectizeInput(
          "composite",
          label = "Combinazioni bande",
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
    
    tags$link(rel = "stylesheet", type = "text/css", href = "mycss2.css?v=c00df"),
    tags$head(tags$script(src="myfuncts.js?v=3c")) , 
    
    jqui_draggable(  
      column(12, style="displan:none; position:absolute;z-index:999999999; width:calc( 100vw - 60px ); top:5px; left:10px;", 
          box( id= "myBoxAnalytics",
                                   title = HTML(paste0(icon("gears"), " Analytics                ")), 
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
                                     column(2, div(title="Scale X axis as dates or as groups.", switchInput("xPlotAxis", onLabel="Text", offLabel = "Date",   width="100%"  ) ),
                                               actionBttn("redrawAnalysisPlot", "RePlot", icon=icon("recycle"), 
                                                          size="sm", style = "simple", color = "primary")),
                                     column(3,   sliderInput("cloudsInPlot", "CLOUD tolerance:",
                                               min = 0, max = 100, value = 100)),
                                     column(3,   sliderInput("snowInPlot", "SNOW tolerance:",
                                                             min = 0, max = 100, value = 100  )),
                                     column(4,   pickerInput("datesInPlot", "Dates", multiple=T, 
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
