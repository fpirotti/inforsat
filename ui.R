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
    
     
    actionButton("aggiorna", "Aggiorna la mappa"),
    actionButton("calcola", "Calcola il grafico") ,
    downloadButton("scaricaPoligoni", "Scarica Poligoni")
    
  ),
  dashboardBody( 
    shinyalert::useShinyalert(), 
    shinyjs::useShinyjs(),
    
    
    # Application title
    #theme = "solar_bootstrap.css",
    div( title=sprintf(
      "Scegli giorno (%d immagini disponibili)",
      length(images.lut$dates)
    ), id="topMostSlider",
      style = "margin:0px 15px; position:relative; z-index:9999; padding:5px 15px",
      shinyWidgets::sliderTextInput(
        "dayselected",
        width = "100%",
        NULL,
        choices = as.character(images.lut$dates),
        grid = TRUE
      )
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "mycss2.css?v=daeks"),
    
    leaflet::leafletOutput("mymap"),
    
    jqui_draggable( 
      shinydashboardPlus::boxPlus(
        title = HTML(paste0(icon("gears"), " Analytics")), 
        closable = TRUE, 
        collapsed = T,
        enable_label = T,
        label_text = "",
        label_status = "danger",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE, 
        plotlyOutput("graph1" , width=400, height = 500)
      ),
      options = list(
        cursor = "move",
        zIndex = 99999999999,
        stack = "#topMostSlider",
        opacity = 0.5,
        handle = ".box-header" ,  
        stop= "function() {
          alert();
        }"
      )
    )
  )
  
)