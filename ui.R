# Define UI for application that draws a histogram
#,
library(shinydashboardPlus)
##required for ui
tt <- table(images.lut$tile)
tiles.labels = sprintf("%s (%s)", names(tt) , tt )
tt <- names(tt)
names(tt)<- tiles.labels
ui <-  dashboardPage(
  #skin = "black",
  
  header = dashboardHeader( title = "InforSAT" ), 
  sidebar=dashboardSidebar (
    
    collapsed = FALSE,
    minified = F,
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    dropdown( 
       
      selectInput(inputId = 'tile',
                  label = 'Choose tile', 
                  choices = tt ),
      
      selectInput(inputId = 'date',
                  label = 'Choose Image by list', 
                  choices = images.lut %>% filter(tile == tt[[1]]) %>% select(date) ),
      
      airDatepickerInput(  
        inputId = "datePicker", 
        label ="Choose image by calendar",
        range = c( first(as.Date(images.lut$date)), Sys.Date()),
        value = last(as.Date(images.lut$date)), 
        todayButton=T,
        addon=NULL,
        #position = "bottom right", 
        startView = last(as.Date(images.lut$date)),
        highlightedDates = as.Date(images.lut$date),
        disabledDates = 
          as.Date( setdiff( as.character( seq(first(as.Date(images.lut$date)), Sys.Date(), by="days") ) , as.character(as.Date(images.lut$date))  ) )
      ),
      style = "bordered", icon = icon("gear"),
       width = "400px", label = "Tools",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeIn,
        exit = animations$fading_exits$fadeOut,
        duration=0.5
      )
    ),
    box( id= "LayersBox",
         title = HTML(paste0(icon("table"), " Layers                ")), 
         closable = F,  width = NULL,
         collapsed = T,
         enable_label = F, 
         label_text = "",
         #label_status = "danger",
         status = "warning",
         solidHeader = TRUE,
         collapsible = TRUE,      
         actionBttn(inputId = "aggiorna", label = "Aggiorna la mappa", icon=icon("reload"), 
                    style="simple", color = "primary", size="sm"),
         div(id="legendPlaceholder", style=" margin:0 -10px;") 
    ) ,
    box( id= "LayersBox2",
         title = HTML(paste0(icon("gears"), " Layers      ")), 
         closable = F,  width = NULL,
         collapsed = T,
         enable_label = F,  
         background = "black",
         #label_status = "danger",
         #status = "primary",
         solidHeader = TRUE,
         collapsible = TRUE,
         div(title="Blocca ad una scala lineare dell'indice, utile per eseguire confronti", materialSwitch( 
           inputId = "freezeScale",
           label = "Scala fissa",
           status = "primary",
           right = F
         ) ),
           
         img(id="legendIndex", style="width:100%;"),
         
         actionButton("calcola", "Calcola il grafico") ,
         downloadButton("scaricaPoligoni", "Scarica Poligoni"),
         downloadButton("scaricaIndice", "Raster Indice")
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
    shinyalert::useShinyalert(force=T), 
    shinyjs::useShinyjs(),
    
    
    tags$link(rel = "stylesheet", type = "text/css", href = "mycss2.css?v=fvcfcfd"),
    tags$head(tags$script(src="myfuncts.js?v=3ccxd")) ,
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
      column(12, style="displan:none; position:absolute;z-index:999999999; width:calc( 100vw - 200px ); top:5px; left:90px;", 
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
