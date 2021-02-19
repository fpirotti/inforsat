# Define UI for application that draws a histogram
#,
ui <- fluidPage(
  # Set up shinyalert
  shinyalert::useShinyalert(), 
  # Set up shinyjs mostra bottoni
  shinyjs::useShinyjs(),
  # Application title
  theme = "solar_bootstrap.css",
  shiny::titlePanel("Infortrac"),
  tags$link(rel="stylesheet", type="text/css", href="mycss.css?v=1"), 
  

  # Show a plot of the generated distribution
  shiny::mainPanel(
    leaflet::leafletOutput("mymap", width=1860, height=900 )
  ),
  
  #Pannello header con titolo
  shiny::fixedPanel(id="topanel",
             draggable = FALSE, 
             top = 0, 
             bottom = "auto",
             width = 1860, 
             height = 30, 
             
             div(id="titolo",style="display: inline-block",strong("TESAF - Progetto InForTrac")),
             div(id="info_contatti",actionButton("info",strong("informazioni"))),
             div(id="info_contatti",actionButton("guida",strong("guida"))),
             div(id="info_contatti",actionButton("contatti",strong("contatti"))),
  ), 
  
  
  #Pannello mobile principale con bottone mostra/nascondi        
  shiny::absolutePanel(id = "mainpanel1", 
                
                #class = "panel-default", 
                fixed = TRUE,
                draggable = TRUE, 
                top = 55, 
                left = 250, 
                right = "auto", 
                bottom = "auto",
                width = 80, 
                height = 40, 
               # includeCSS("www/mycss.css"),  
                
                actionButton('showBtn1','',height = 50, icon = icon("gear")),
                
                
                shinyjs::hidden(
                  shiny::absolutePanel(id = "controlli1", 
                                fixed = FALSE,
                                draggable = FALSE,
                                top = 10,
                                left = 30,
                                height = "auto",
                                width = 390,
                                

                                #coordinates
                                #div(style="display: inline-block;vertical-align:top; width: 150px;",                
                                #    p("Coordinate:"),
                                #    textOutput("Clon"),
                                #    textOutput("Clat"),
                                #    br()
                                #),
                                
                                shiny::br(),
                                #zoomcheck
                                div(style="display: inline-block;vertical-align:top; width: 100px;",              
                                    htmlOutput("Zoom_check"),
                                    br()
                                    
                                ),
                                shiny::br(),
                                
                                # Copy the chunk below to make a group of checkboxes
                                shiny::radioButtons("radio", label = "Indici", 
                                             choices = list("NDVI" = 1, "RGI" = 2, "NDMI" = 3, "NIR"=4, "RGB" = 5),
                                             selected = 1,inline = TRUE), 
                                
                                fluidRow(
                                  column(6, materialSwitch(
                                      inputId = "mskCld",
                                      label = "Filtra Nuvole", 
                                      status = "primary",
                                      right = F
                                    ) ), column(6,
                                    materialSwitch(
                                      inputId = "mskSnow",
                                      label = "Filtra Neve", 
                                      status = "primary",
                                      right = TRUE
                                    )
                                    )),
                                # shiny::br(),
                                # shiny::p("Scegli le date:"),
                                # 
                                # shiny::dateRangeInput("daterange", "", language="it",
                                #                start  = min(images.lut$dates),
                                #                end    = NULL,
                                #                min    = min(images.lut$dates),
                                #                #format = "mm/dd/yy",
                                #                separator = " - "),
                                
                                shiny::p(sprintf("Giorno da visualizzare:", length(images.lut$dates) ) ),
                                
                                shinyWidgets::sliderTextInput("dayselected",
                                                "",
                                                choices=as.character(images.lut$dates),
                                                grid=TRUE
                                                #min = as.Date("2019-01-01"),
                                                #max = as.Date("2019-01-01"),
                                                #value = as.Date("2018-01-01"),
                                                #timeFormat="%F"
                                ),
                                
                                shiny::br(),
                                shiny::br(),
                                
                                
                                shiny::sliderInput("opac", "Opacità",
                                            min = 0.0, max = 1.0, value = 1, ticks=FALSE
                                ),
                              
                                shiny::br(),
                                shiny::p("Giorno visualizzato:"),
                                shiny::span(strong(textOutput("selected_var"),style = "color:Black")),
                                
                                shiny::br(),
                                #crea un output testo 
                                #verbatimTextOutput("indices"),
                                shiny::div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton("aggiorna", "Aggiorna la mappa")),
                                
                                #textOutput("Polygon"),
                                shiny::div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton("calcola", "Calcola il grafico")),
                                shiny::p(" ")
                  )
                ),
  ),
  
  
  
  #Pannello mobile principale con bottone mostra/nascondi        
  shiny::absolutePanel(id = "mainpanel2", 
                
                #class = "panel-default", 
                fixed = TRUE,
                draggable = TRUE, 
                top = 55, 
                left = 637, 
                right = "auto", 
                bottom = "auto",
                width = 80, 
                height = 40, 
                #includeCSS("www/mycss.css"),  
                
                actionButton('showBtn2','',height = 50, icon = icon("chart-line")),
                
                
                shinyjs::hidden( #zoomcheck
                  shiny::absolutePanel(id = "risultati", 
                                fixed = FALSE,
                                draggable = FALSE,
                                top = 10,
                                left = 30,
                                height = "355",
                                width = "420",
                               # includeCSS("www/mycss.css"),
                                
                                shiny::div(style="display: inline-block;vertical-align:top;margin-top:4px;width: 80px;margin-left:-35px","Risultati"),
                                
                                shiny::div(style="display: inline-block;vertical-align:top; width: 100px;",downloadButton("downloadData", "Scarica i dati")),
                                
                                shiny::div(style="display: inline-block;vertical-align:top; width: 100px;margin-left:15px",downloadButton("downloadPlot", "Scarica il grafico")),
                                
                                shiny::plotOutput("plot1", click = "plot_click"),
                                
                                shiny::verbatimTextOutput("info")
                  )
                ),
  ),

)

