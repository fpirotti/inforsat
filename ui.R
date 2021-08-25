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
  #skin = "black",
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#000000"),
  header = dashboardHeader( title = HTML("<b>InforSAT</b>") ), 
  sidebar=dashboardSidebar (
    
    collapsed = FALSE,
    minified = F,
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    selectInput(inputId = 'tile',
                label = 'Choose tile', 
                choices = c("", tt) ),
    dropdown( 
      fluidRow(
        column(6,   
               selectInput(inputId = 'dayselected',
                           label = 'Choose Image by list', 
                           choices = NULL ),
              
               div(title="Blocca ad una scala lineare dell'indice, utile per eseguire confronti", 
         
                  selectInput(inputId = 'resampling',
                               label = 'Resampling method (GDAL)',
                               choices=c("near","bilinear","cubic","cubicspline","lanczos","average","mode","max") 
                   ),
                   materialSwitch( 
                     inputId = "freezeScale",
                     label = "Scala fissa",
                     status = "primary",
                     value=T,
                     right = F, inline = T
                   ),
                   materialSwitch( 
                     inputId = "spyGlass",
                     label = "SpyGlass",
                     status = "primary",
                     right = F, inline = T
                   )
                 )
            ),
        column(6, 
               actionBttn(inputId = "aggiorna", label = "Aggiorna la mappa", icon=icon("reload"), 
                          style="simple", color = "primary", size="sm"),
               div(id="legendPlaceholder", style=" margin:0 -10px;") 
                
               )
      ),   
      style = "bordered", 
      icon = icon("table"),
      width = "650px", 
      label = "Layers",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeIn,
        exit = animations$fading_exits$fadeOut,
        duration=0.5
      )
    ),
    # box( id= "LayersBox",
    #      title = HTML(paste0(icon("table"), " Layers                ")), 
    #      closable = F,  width = NULL,
    #      collapsed = T,
    #      enable_label = F, 
    #      label_text = "",
    #      #label_status = "danger",
    #      status = "warning",
    #      solidHeader = TRUE,
    #      collapsible = TRUE
    # ) ,
    
    # dropdown(   
    #   style = "bordered", 
    #   icon = icon("gears"),
    #   label = "Zonal Statistics",
    #   animate = animateOptions(
    #     enter = animations$fading_entrances$fadeIn,
    #     exit = animations$fading_exits$fadeOut,
    #     duration=0.5
    #   ),
    #   
    #   
    #   pickerInput(
    #     inputId = "Id083",
    #     label = "Statistics to calculate",
    #     choices = c("Average", "Median", "Standard deviation", "25th-75th percentile", "Min-Max"),
    #     selected = c("Average", "Median", "Standard deviation", "25th-75th percentile", "Min-Max"),
    #     multiple = TRUE
    #   ),
    # 
    # ),
    actionButton("calcola", "Source PLOT") ,
    div( title="Download vector file with the areas that were drawn in the map as polygons. ", 
       downloadButton("scaricaPoligoni", "Polygons")
       ),
    #downloadButton("scaricaIndice", "Raster Indice"),
    div( title="Download an excel file with all values extracted from pixels inside the areas drawn in the map from all the images at different dates. ", 
         downloadButton("scaricaTabellaValori", "Index Values Table") 
         )
    
    # dropdown(
    #   actionButton("aggiorna", "Aggiorna la mappa"),
    #   div(id="legendPlaceholder", style=" margin:0 -10px;"),
    #   hr(),
    #   style = "bordered", icon = icon("table"),
    #    label = "Layers",
    #   animate = animateOptions(
    #     enter = animations$fading_entrances$fadeIn,
    #     exit = animations$fading_exits$fadeOut,
    #     duration=0.5
    #   )
    # )
    
   
  ),
  
  controlbar = dashboardControlbar( width = 500,
    controlbarMenu(
      id = "cb_menu",
      controlbarItem(
        "Indici",
        selectizeInput(
          "indici",
          label = "Indici",
          choices = radio2expression,
          selected = character(0)
        ) 
        

      ),
      
      controlbarItem(
         "Composite",
        selectizeInput(
          "composite",
          label = "Combinazioni bande",
          choices = processingComposite,
          selected = character(0)
        ),
        
        div(title="dddd",
            plotlyOutput(outputId = "bandHistogram")
        ),
        
        
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
    
    tags$link(rel = "stylesheet", type = "text/css", href = "mycss2.css?v=cfcd"),
    tags$head(tags$script(src="myfuncts.js?v=3c")) ,
    # Application title
    #theme = "solar_bootstrap.css",
    # div( title=sprintf(
    #   "Scegli giorno (%d immagini disponibili)",
    #   length(images.lut$dates)
    # ), id="topMostSlider",
    #   style = "margin:0 0; position:relative; display:inline-block; width:100%;   padding:0px 0px",
    #   # shinyWidgets::sliderTextInput(
    #   #   "dayselected",
    #   #   width = "100%",
    #   #   NULL,
    #   #   choices = as.character(images.lut$date),
    #   #   grid = TRUE
    #   # )
    # 
    # 
    # ),
    
    
    jqui_draggable( 
      # fixedPanel(top = 5, left=280,  draggable = T, 
      #               style="z-index:99999; width:250px",
      #               
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
                                     column(2, switchInput("xPlotAxis", "as Date", size="sm"  ) ),
                                     column(3,   sliderInput("cloudsInPlot", "CLOUD tolerance:",
                                               min = 0, max = 100, value = 100)),
                                     column(3,   sliderInput("snowInPlot", "SNOW tolerance:",
                                                             min = 0, max = 100, value = 100  )),
                                     column(4,   pickerInput("datesInPlot", "Dates", multiple=T, 
                                                             choices=NULL, options = list(liveSearch=T) ))
                                   ),
                                  addSpinner(plotlyOutput("graph1" ))
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
