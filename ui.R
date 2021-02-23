# Define UI for application that draws a histogram
#,
ui <- dashboardPage(
  dashboardHeader(   ),
  dashboardSidebar(
    selectizeInput(
      "indici",
      label = "Indici",
      choices = radio2expression,
      selected = character(0)
    ),
    div(title="Blocca la scala dell'indice, utile per eseguire confronti", materialSwitch( 
      inputId = "freezeScale",
      label = "Fissa scala",
      status = "primary",
      right = F
    ) ),
    selectizeInput(
      "composite",
      label = "Combinazioni bande (da SNAP)",
      choices = processingComposite,
      selected = character(0)
    ),
    
    materialSwitch(
      inputId = "mskCld",
      label = "Filtra Nuvole",
      status = "primary",
      right = F
    ),
    
    materialSwitch(
      inputId = "mskSnow",
      label = "Filtra Neve",
      status = "primary",
      right = F
    ),
    
    img(id="legendIndex", style="width:100%;"),
     
    actionButton("aggiorna", "Aggiorna la mappa"),
    actionButton("calcola", "Calcola il grafico") ,
    downloadButton("scaricaPoligoni", "Scarica Poligoni"),
    downloadButton("scaricaIndice", "Raster Indice")
    
  ),
  dashboardBody( 
    shinyalert::useShinyalert(), 
    shinyjs::useShinyjs(),
    
    
    tags$link(rel = "stylesheet", type = "text/css", href = "mycss2.css?v=fshf"),
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
      div(style="position:absolute;z-index:999999999; width:calc( 100vw - 300px ); top:5px; left:290px;", 
       shinydashboardPlus::boxPlus(
        title = HTML(paste0(icon("gears"), " Analytics                ")), 
        closable = TRUE,  width="100%",
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