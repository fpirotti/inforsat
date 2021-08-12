# Define UI for application that draws a histogram
#,
library(shinydashboardPlus)
ui <-  dashboardPage(
  header = dashboardHeader( ), 
  sidebar=dashboardSidebar (
    
    collapsed = FALSE,
    minified = F,
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    dropdown(
      
      tags$h3("List of Input"),
      
      pickerInput(inputId = 'xcol2',
                  label = 'X Variable',
                  choices = names(iris),
                  options = list(`style` = "btn-info")),
      
      style = "bordered", icon = icon("gear"),
       width = "400px", label = "Tools",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInLeftBig,
        exit = animations$fading_exits$fadeOutRightBig
      )
    ),
    
    dropdown(
      div(id="legendPlaceholder", style="padding:5px; margin-top:-10px;"),
      style = "bordered", icon = icon("gear"),
       label = "Layers",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInLeftBig,
        exit = animations$fading_exits$fadeOutRightBig
      )
    )
    
   
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
        ), 
        
        div(title="Blocca ad una scala lineare dell'indice, utile per eseguire confronti", materialSwitch( 
          inputId = "freezeScale",
          label = "Scala fissa",
          status = "primary",
          right = F
        ) ),
        
        # div(title="dddd", numericInput( 
        #   inputId = "resolution",
        #   label = "Screen Resolution", 
        #   min=1, max=3, value = 1
        # ) ),

        img(id="legendIndex", style="width:100%;"),
        
        actionButton("aggiorna", "Aggiorna la mappa"),
        actionButton("calcola", "Calcola il grafico") ,
        downloadButton("scaricaPoligoni", "Scarica Poligoni"),
        downloadButton("scaricaIndice", "Raster Indice")
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
    shinyalert::useShinyalert(force=T), 
    shinyjs::useShinyjs(),
    
    
    tags$link(rel = "stylesheet", type = "text/css", href = "mycss2.css?v=ffd"),
    # Application title
    #theme = "solar_bootstrap.css",
    div( title=sprintf(
      "Scegli giorno (%d immagini disponibili)",
      length(images.lut$dates)
    ), id="topMostSlider",
      style = "margin:0 0; position:relative; width:100%;   padding:0px 0px",
      shinyWidgets::sliderTextInput(
        "dayselected",
        width = "100%",
        NULL,
        choices = as.character(images.lut$dates),
        grid = TRUE
      )
    ),
    
    
    jqui_draggable( 
      # fixedPanel(top = 5, left=280,  draggable = T, 
      #               style="z-index:99999; width:250px",
      #               
      column(12, style="position:absolute;z-index:999999999; width:calc( 100vw - 200px ); top:5px; left:90px;", 
          box( id= "myBox",
                                   title = HTML(paste0(icon("gears"), " Analytics                ")), 
                                   closable = TRUE,  width = NULL,
                                   collapsed = F,
                                   enable_label = T, 
                                   label_text = "",
                                   label_status = "danger",
                                   status = "warning",
                                   solidHeader = TRUE,
                                   collapsible = TRUE, 
                                   plotlyOutput("graph1" )
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
