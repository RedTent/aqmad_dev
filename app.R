###########################################################
# AqMaD RShiny interface                                  #
#                                                         #
# Auteurs: Lilith Kramer                                  #
#                                                         #
#                                                         #
#                                                         #
# Datum : 2019-04-23                                      #
# Bedrijf: Deltares                                       #
# Licentie: GNU General Public License                    #
#                                                         #           
# Contact : Gerben van Geest                              #
# Email : gerben.vangeest@deltares.nl                     #
#                                                         #
########################################################### 


##=PREPARATION========

##==legend=====

## COMMENTS
## ##      = comment
## #       = outcommented code
## #!#     = should be checked if location of script changes 
## #?#     = to do
## ##==    = block separator

## OBJECTS
## fnXXX   = filename
## dirXXX  = directory (path) / foldername
## oXXX    = loaded object
## dfXXX   = dataframe      
## dtXXX   = datatable 

##==install packages and open libraries ====

# install.packages("shiny")          ## shiny
# install.packages("shinydashboard") ## layout for shiny
# install.packages("data.table")     ## working with data.frames the data.table way
# install.packages("ggplot2")        ## plotting
# install.packages("openxlsx")       ## open excel files
# install.packages("digest")         ## needed for dev version ggplot2
# install.packages("devtools")
# devtools::install_github('hadley/ggplot2')
# install.packages("leaflet")
# install.packages("plotly")
# install.packages("lubridate")
# install.packages("tidyverse")
# install.packages("rgdal")
# install.packages("shinyjs")
# install.packages("rmapshaper")
# install.packages("shinycssloaders")
# install.packages("plyr")
# install.packages("V8")
# install.packages("htmlwidgets")
# install.packages("mapview")
# install.packages("XML")

library(shiny)         
library(shinydashboard)
library(leaflet)
library(plotly)
library(lubridate)
library(shinyjs)
library(shinycssloaders)
library(plyr)
library(rgdal)
library(V8)
library(htmlwidgets)
library(mapview)
library(XML)



##==settings=======

options(shiny.display.mode="showcase")
options(scipen = 999)  ## no scientific numbering
options(shiny.reactlog = T)
options(shiny.maxRequestSize = 30*1024^2) ## maximum upload size is now 30Mb, standard for R shiny is 5Mb. 
## rshiny shinyapps.io max limit size = 32MB. 

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page



##== load functions ====

source('AqMaD_inleesfuncties.R')
source('AqMaD_plotFunctions.R')
source('AqMaD_rekenkern.R')



##==set paths ========

#!# directories
## no directories yet 

#!# files
path_KRWtypeRef <- paste("data/AqMaD referentietypes.csv")
path_parametergroepen <- paste("data/AqMaD_Indeling-parameters-in-groepen.csv")



##== set variables =======

variables = reactiveValues(soortgroep = NULL, soortgroepnaam = NULL, watertype = NULL)



##==load files=========

## no files to load yet



##== set functions=====

## no functions yet



##==USER INTERFACE==================

header  <- dashboardHeader(title = "AqMaD")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "side_tabs",
              menuItem(text = "Startpagina", tabName = "start", icon = icon("home")),
              menuItem(text = "Data inlezen", tabName = "inlezen", icon = icon("upload")),
              menuItem(text = "Abiotiek bekijken", tabName = "abiotiek", icon = icon("list-alt", lib = "glyphicon")),
              menuItem(text = "Z-waardes bekijken", tabName = "z_waardes", icon = icon("cogs")),
              menuItem(text = "Achtergrondinformatie", tabName = "info", icon = icon("info"))
  ))
## rows have a grid width of 12, so a box with width = 4, takes up one third of the space
## tops will be lined out, bottoms not
## heights are in pixels.. 

body    <- dashboardBody(
  
  tabItems(
    
    ##===startpagina==============
    tabItem(tabName = "start",
            fluidRow(
              box(title = "Welkom bij de webapplicatie van AqMaD",
                  solidHeader = T,
                  status = "success",
                  collapsible = F,
                  p("AqMaD is bedoeld ter ondersteuning van een ecologische watersysteemanalyse. Met deze applicatie is het mogelijk om een indicatie te krijgen van de abiotische condities aan de hand van data van de soortensamenstelling."),
                  p("Navigeren door deze applicatie werkt via de tabs aan de linkerzijde van dit scherm."),
                  p("Deze applicatie is gebouwd door Deltares en gefinancierd door de STOWA."),
                  img(src='stowa_logo.png'),
                  img(src='deltares_logo.png'),
                  width = 12))),

    ##== data inlezen pagina==============
    tabItem(tabName = "inlezen", 
            fluidRow(
              box(title = "Data inlezen",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = F,
                  width = 12,
                  p("Het inlezen van de data gaat als volgt:"), 
                    tags$ol(tags$li("Selecteer uw soortgroep"),
                            tags$li("Selecteer stilstaand of stromend water"),
                            tags$li("Upload uw huidige opnames"),
                            tags$li("Upload uw gewenste referentiesoortensamenstelling")),
                  p("Op de pagina 'Achtergrondinformatie' zijn voorbeelden van de benodigde bestandsformaten terug te vinden. Tevens zijn deze in de handleiding beschreven."),
                  p("De onderstaande boxen zijn te openen en sluiten met een klik op het '+'-teken. Dit teken vindt u aan de rechterbovenzijde van de boxen.")),
              
              ##==== reset applicatie ========
              box(title = "Herstart applicatie",
                  solidHeader = T,
                  status = "warning",
                  collapsible = T,
                  collapsed = T,
                  width = 12,
                  p("Druk op de knop om de applicatie opnieuw te starten. Hierbij gaat alle informatie die tot nu toe ingeladen was of weergegeven werd, verloren."),
                  useShinyjs(),                                           # Include shinyjs in the UI
                  extendShinyjs(text = jsResetCode),                      # Add the js code to the page
                  actionButton("reset_button", "Herstart applicatie")
                  ),
              
              
              ##==== ecologische soortgroep kiezen =====
              box(title = "1. Selecteer uw soortgroep",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = T,
                  width = 12,
                  p("Klik op één van de onderstaande soortgroepen om de ecologische soortgroep waar u mee wilt gaan werken te selecteren."),
                  # p("De berekening van Z-waardes voor de soortgroep macrofauna verschilt van die van de andere soortgroepen. Meer informatie over de berekening van de Z-waardes is terug te vinden op de achtergrondinformatiepagina."),
                  radioButtons(inputId = "rb_ecologische_groep", 
                               label = NULL, 
                               choices = c("Diatomeeen", "Macrofyten", "Macrofauna", "Vissen"))),
              
              ##==== watertype kiezen =====
              box(title = "2. Selecteer stilstaand of stromend water",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = T,
                  width = 12,
                  p("Selecteer hier uw watertype. Op basis van deze keuze worden de resultaten gepresenteerd volgens de ESF-systematiek van resp. stilstaand of stromend water."),
                  radioButtons(inputId = "rb_watertype", 
                               label = NULL, 
                               choices = c("Stilstaand water", "Stromend water"))),
              
              ##==== inlezen opname data ========
              box(title = "3. Upload uw huidige opnames",
                  solidHeader = T, 
                  status = "success",
                  collapsible = T,
                  collapsed = T,
                  width = 12,
                  p("Upload hier uw huidige bestand met monitordata van één van de soortgroepen. Voor een omschrijving van het benodigde format zie de handleiding op de pagina 'Achtergrondinformatie'. De maximale bestandsgrootte is 30MB."),
                  uiOutput("huidigInvoerSoortenBestand_ui"),
                  tabBox(id = "logHuidigInvoerSoortenBestand",
                         title = "",
                         side = "left", 
                         width = 12,
                         selected = "Logbestand",
                         tabPanel("Logbestand", 
                                  p("Met onderstaande knop kunnen de logmeldingen geëxporteerd worden."),
                                  conditionalPanel("output.huidigFouten", downloadButton("downloadHuidigLogbestand", "Download logbestand")),
                                  tags$hr(),
                                  verbatimTextOutput("huidigFouten") %>% withSpinner(type = 3, color = "#01A65A", color.background = "#ffffff"),
                                  verbatimTextOutput("huidigWaarschuwingen") %>% withSpinner(type = 3, color = "#01A65A", color.background = "#ffffff")),
                         tabPanel("Ingelezen bestand",
                                  p("Indien het bestand goed is ingelezen, kunt u in de onderstaande tabel de eerste paar regels van het ingelezen bestand bekijken."),
                                  div(style = 'overflow-x: scroll', tableOutput("huidigInvoerSoortenTabel") %>% withSpinner(color = "#01A65A", color.background = "#ffffff")
                                  )))),
              
              ##==== (inlezen) doeldata ========
              box(title = "4. Upload uw gewenste referentiesoortensamenstelling",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = T,
                  width = 12,
                  p("Upload hier uw soortenlijst voor de gewenste (referentie)condities. Voor diatomeeën en macrofauna zijn – naast een soortenlijst – ook aantallen per soort nodig. Voor macrofyten is het ook mogelijk om gebruik te maken van soortenlijsten per KRW-watertype, die gebaseerd zijn op de KRW-maatlatten. De soortenlijsten per KRW-type zijn reeds opgenomen in de webapplicatie; AqMaD hanteert hiervoor het KRW-watertype dat is aangegeven in de invoerfile met huidige opnames. Als u voor referentiewaardes op basis van het KRW-watertype kiest, dan kunt u niet tegelijkertijd gebruik maken van een eigen invoerbestand."),
                  radioButtons(inputId = "sel_bron_doel", 
                               label = NULL,
                               choices = c("KRW", "Eigen referentie")),
                  uiOutput("refInvoerSoortenBestand_ui"),
                  tabBox(id = "logRefInvoerSoortenBestand",
                           title = "",
                           side = "left", 
                           width = 12,
                           selected = "Logbestand",
                           tabPanel("Logbestand", 
                                    p("Met onderstaande knop kunnen de logmeldingen geëxporteerd worden."),
                                    conditionalPanel("output.refFouten", downloadButton("downloadReferentieLogbestand", "Download logbestand")),
                                    tags$hr(),
                                    verbatimTextOutput("refFouten") %>% withSpinner(type = 3, color = "#01A65A", color.background = "#ffffff"),
                                    p(""),
                                    verbatimTextOutput("refWaarschuwingen") %>% withSpinner(type = 3, color = "#01A65A", color.background = "#ffffff")),
                           tabPanel("Ingelezen bestand",
                                    p("Indien het bestand goed is ingelezen, kunt u in de onderstaande tabel de eerste paar regels van het ingelezen bestand bekijken."),
                                    div(style = 'overflow-x: scroll', tableOutput("refInvoerSoortenTabel") %>% withSpinner(color = "#01A65A", color.background = "#ffffff"))))))),
          
    ##========= abiotiek bekijken pagina ============================
    tabItem(tabName = "abiotiek",
            fluidRow(
              box(title = "Abiotiek bekijken", 
                  textOutput("selected_soortgroep_abio"), 
                  width = 12,
                  solidHeader = T, 
                  # background = "green",
                  status = "success"),
              tabBox(title = NULL,
                     width = 12,
                     side = "left",
                     selected = "Huidige monitordata",
                     tabPanel("Huidige monitordata",  
                              p("Na het invoeren van data in box '3. Upload uw huidige opnames' op het tabblad 'Data inlezen' wordt hier de (berekende) abiotiek gepresenteerd die hoort bij de huidige opnames met monitordata."),
                              p("Wanneer een groot aantal parameters gepresenteerd wordt, dan kunnen de grafieken en tabellen naar rechts tot buiten het paginabereik ‘doorlopen’. Deze gegevens worden zichtbaar door de schuifbalk (onder de figuren en tabellen) naar rechts te verplaatsen."),
                              p("De kans bestaat dat niet alle parameters voor alle opnames zijn berekend. Er moeten namelijk tenminste vier soorten in een opname voorkomen met informatie voor de desbetreffende parameter, voordat de statistieken berekend kunnen worden."),
                              p("Bij het laden van grotere bestanden kan het enkele minuten duren voor de data hier zichtbaar wordt."),
                              tags$hr(),
                              p("Met onderstaande knoppen kunnen alle achterliggende data als csv-bestand geëxporteerd worden."),
                              downloadButton("downloadHuidigAbiotiek", "Download abiotische data"),
                              downloadButton("downloadHuidigAbiotiekStatistiek", "Download statistiek abiotische data"),
                              tags$hr(),
                              uiOutput("huidigAbiotiekLocatie"),
                              uiOutput("huidigAbiotiekJaar"),
                              uiOutput("huidigAbiotiekParametergroep"),
                              conditionalPanel(condition = "input.rb_ecologische_groep == 'Macrofyten'", 
                              uiOutput("huidigAbiotiekPtype")),
                              tags$hr(),
                              conditionalPanel(condition = "input.rb_ecologische_groep == 'Macrofauna'",
                                               p("Voor de legenda met de eenheden van de parameters wordt verwezen naar Tabel 2 in de bijlage van de handleiding.")),
                              div(style = 'overflow-x: scroll', plotOutput("huidigAbiotiekPlots") %>% withSpinner(type = 3, color = "#01A65A", color.background = "#ffffff")),
                              tags$hr(),
                              div(style = 'overflow-x: scroll', tableOutput("huidigAbiotiekStatistiekTabel") %>% withSpinner(type = 3, color = "#01A65A", color.background = "#ffffff"))
                              ),
                     tabPanel("Gewenste (referentie) condities",
                       p("Na het invoeren van data in box '4. Upload uw gewenste referentiesoortensamenstelling' op het tabblad 'Data inlezen' wordt hier de (berekende) abiotiek gepresenteerd die hoort bij deze gewenste condities."),
                       p("Wanneer een groot aantal parameters gepresenteerd wordt, dan kunnen de grafieken en tabellen naar rechts tot buiten het paginabereik ‘doorlopen’. Deze gegevens worden zichtbaar door de schuifbalk (onder de figuren en tabellen) naar rechts te verplaatsen."),
                       p("De kans bestaat dat niet alle parameters voor alle opnames zijn berekend. Er moeten namelijk tenminste vier soorten in een opname voorkomen met informatie voor de desbetreffende parameter, voordat de statistieken berekend kunnen worden."),
                       p("Bij het laden van grotere bestanden kan het enkele minuten duren voor de data hier zichtbaar wordt."),
                       tags$hr(),
                       p("Met onderstaande knoppen kunnen alle achterliggende data als csv-bestand geëxporteerd worden."),
                       downloadButton("downloadRefAbiotiek", "Download abiotische data"),
                       downloadButton("downloadRefAbiotiekStatistiek", "Download statistiek abiotische data"),
                       tags$hr(),
                       uiOutput("refAbiotiekLocatie"),
                       uiOutput("refAbiotiekParametergroep"),
                       conditionalPanel(condition = "input.rb_ecologische_groep == 'Macrofyten'", 
                                        uiOutput("refAbiotiekPtype")),
                       tags$hr(),
                       conditionalPanel(condition = "input.rb_ecologische_groep == 'Macrofauna'",
                                        p("Voor de legenda met de eenheden van de parameters wordt verwezen naar Tabel 2 in de bijlage van de handleiding.")),
                       div(style = 'overflow-x: scroll', plotOutput("refAbiotiekPlots") %>% withSpinner(type = 3, color = "#01A65A", color.background = "#ffffff")),
                       tags$hr(),
                       div(style = 'overflow-x: scroll', tableOutput("refAbiotiekStatistiekTabel") %>% withSpinner(type = 3, color = "#01A65A", color.background = "#ffffff"))
              )))),
                
                         

    #== z waardes bekijken pagina==============
    tabItem(tabName = "z_waardes", 
            fluidRow(
              box(title = "Z-waardes bekijken", 
                  textOutput("selected_soortgroep_z"), 
                  width = 12,
                  solidHeader = T, 
                  # background = "green",
                  status = "success"),
              tabBox(id = "z_waardes_tabbox",
                     title = "",
                     side = "left", 
                     selected = "Overzicht",
                     width = 12,
                     tabPanel("Overzicht", 
                              
                              p("Dit tabblad bevat het overzicht van de Z-waardes wanneer op de pagina 'Data inlezen' zowel de huidige monitordata als de referentiedata ingevoerd zijn. De kleuren in de figuur geven het waardeoordeel over de hoogte van de Z-waardes voor de betreffende soortgroep; in Tabel 4.3 van de handleiding staat de klassengrenzen van de Z-waardes weergegeven voor de waardeoordelen. Via het dropdown menu kunnen locatiecode en jaar geselecteerd worden. Tevens kunnen alle Z-waardes worden gedownload."),
                              tags$hr(),
                              p("Met de onderstaande knop kunnen alle berekende Z-waardes als csv-bestand geexporteerd worden."),
                              downloadButton("downloadZwaardes", "Download alle berekende Z-waardes"),
                              tags$hr(),
                              uiOutput("zwaardenOverzichtLocaties"),
                              uiOutput("zwaardenOverzichtJaar"),
                              uiOutput("zwaardenOverzichtPtype"),
                              tags$hr(),
                              plotOutput("zWaardenTabel", height = "1000px", width = "500px") %>% withSpinner(type = 3, color = "#01A65A", color.background = "#ffffff")),
                     tabPanel("Presentatie Z-waardes op kaart", 
                              p("Dit tabblad presenteert de uitkomsten van de Z-waardes op een kaart. De kleuren in de figuur geven het waardeoordeel over de hoogte van de Z-waardes voor de betreffende soortgroep; in Tabel 4.3 van de handleiding staat de klassengrenzen van de Z-waardes weergegeven voor de waardeoordelen. Via het dropdown menu kunnen het jaar van bemonstering en de parameter geselecteerd worden; downloaden van de Z-waardes is mogelijk via het tabblad “Overzicht”."),
                              tags$hr(),
                              p("Met de onderstaande knop kan de kaart als html-bestand geexporteerd worden."),
                              downloadButton("zwaarden_map", "download Z-waarden kaart"),
                              tags$hr(),
                              uiOutput("zwaardenRuimteJaar"),
                              uiOutput("zwaardenRuimteParameter"),
                              uiOutput("zwaardenRuimtePtype"),
                              leafletOutput("zWaardenLocaties") %>% withSpinner(type = 3, color = "#01A65A", color.background = "#ffffff")
                              ),
                     tabPanel("Tijdreeks Z-waardes", 
                              p("Dit tabblad presenteert tijdreeksen van dezelfde monsterlocatie in een figuur. Deze figuren bieden inzicht in de trend over jaren, en levert antwoord op vragen als: indiceert de soortensamenstelling een verbetering voor bepaalde parameters na uitvoer van maatregelen? En wat is de autonome trend? "),
                              p("De kleuren in de figuur geven het waardeoordeel over de hoogte van de Z-waardes voor de betreffende soortgroep; in Tabel 4.3 van de handleiding staat de klassengrenzen van de Z-waardes weergegeven voor de waardeoordelen. Via het dropdown menu kunnen de locatie en de ecologische sleutelfactoren geselecteerd worden; downloaden van de Z-waardes is mogelijk via het tabblad 'Overzicht'."),
                              tags$hr(),
                              uiOutput("zwaardenTijdLocatie"),
                              uiOutput("zwaardenTijdParametergroep"),
                              uiOutput("zwaardenTijdPtype"),
                              div(style = 'overflow-x: scroll', plotOutput("zTijdWaardePlot", height = "auto") %>% withSpinner(type = 3, color = "#01A65A", color.background = "#ffffff")))))),
    
    ##====achtergrondinformatie pagina ====
    tabItem(tabName = "info",
            fluidRow(
              box(title = "Download achtergrondinformatie",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = F,
                  width = 12,
                  h3("Handleiding AqMaD"),
                  downloadButton("handleiding_aqmad", "Download handleiding voor AqMaD"),
                  h3("Diatomeeen"),
                  downloadButton("soortgroep_diatomeeen", "Download achtergronddocument diatomeeen"),
                  p(" "),
                  downloadButton("soortgroep_diatomeeen_habitatpref", "Download habitat- en milieupreferenties diatomeeen"),
                  p(" "),
                  downloadButton("monitordata_diatomeeen_voorbeeld", "Download voorbeeld monitordata diatomeeen"),
                  p(" "),
                  downloadButton("referentiedata_diatomeeen_voorbeeld", "Download voorbeeld referentiedata diatomeeen"),
                  h3("Macrofauna"),
                  downloadButton("soortgroep_macrofauna", "Download achtergronddocument macrofauna Verdonschot (2011)"),
                  p(" "),
                  downloadButton("soortgroep_macrofauna_2", "Download achtergronddocument macrofauna Verberk et al. (2012)"),
                  p(" "),
                  downloadButton("soortgroep_macrofauna_habitatpref", "Download habitat- en milieupreferenties macrofauna"),
                  p(" "),
                  downloadButton("monitordata_macrofauna_voorbeeld", "Download voorbeeld monitordata macrofauna"),
                  p(" "),
                  downloadButton("referentiedata_macrofauna_voorbeeld_csv", "Download voorbeeld referentiedata macrofauna (csv)"),
                  p(" "),
                  downloadButton("referentiedata_macrofauna_voorbeeld_txt", "Download voorbeeld referentiedata macrofauna (txt)"),
                  h3("Macrofyten"),
                  downloadButton("soortgroep_macrofyten", "Download achtergronddocument macrofyten"),
                  p(" "),
                  downloadButton("soortgroep_macrofyten_2", "Download achtergronddocument macrofyten (bijlagen)"),
                  p(" "),
                  downloadButton("indeling_planten", "Download indeling waterplanten en oeverplanten"),
                  p(" "),
                  downloadButton("soortgroep_macrofyten_habitatpref", "Download habitat- en milieupreferenties macrofyten"),
                  p(" "),
                  downloadButton("monitordata_planten_voorbeeld", "Download voorbeeld monitordata macrofyten"),
                  p(" "),
                  downloadButton("referentiedata_planten_voorbeeld", "Download voorbeeld referentiedata macrofyten"),
                  h3("Vissen"),
                  downloadButton("soortgroep_vissen", "Download notitie vissen"),
                  p(" "),
                  downloadButton("soortgroep_vissen_habitatpref", "Download habitat- en milieupreferenties vissen"),
                  p(" "),
                  downloadButton("monitordata_vissen_voorbeeld", "Download voorbeeld monitordata vissen"),
                  p(" "),
                  downloadButton("referentiedata_vissen_voorbeeld", "Download voorbeeld referentiedata vissen"),
                  h3("R-code"),
                  p("De R-code voor deze tool is op aanvraag beschikbaar bij Lilith Kramer (Deltares) via:"),
                  img(src='contact.png')
                  )))))
    

   
ui <- dashboardPage(skin = "green", header, sidebar, body,  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), tags$style(".leaflet-top {z-index:999!important;}"))) ## tags$head etc is noodzakelijk om de zoombuttons van leaflet achter de dropdown box te krijgen. 
# ui <- dashboardPage(header, sidebar, body,  tags$head(tags$style(".leaflet-top {z-index:999!important;}"))) ## tags$head etc is noodzakelijk om de zoombuttons van leaflet achter de dropdown box te krijgen. 


##==SET SERVER=========


server <- function(input, output, session) {
  
  
  ##========= reset shiny app ==================
  observeEvent(input$reset_button, {js$reset()}) 
  
  ##========= toekennen Variabelen ==================
  observeEvent(input$rb_ecologische_groep, {
    
    if(input$rb_ecologische_groep == 'Diatomeeen'){
    variables$soortgroep = "Diatomeeen"
    variables$soortgroepnaam = c("DIATM", "DIAB", "Diatomeeen")
    } else if(input$rb_ecologische_groep == 'Macrofyten'){
    variables$soortgroep = "Macrofyten"
    variables$soortgroepnaam = c('MAFY','MACFT','MACFY','mafy','macft','macfy','MFYT', 'Macrofyten')
    } else if(input$rb_ecologische_groep == 'Macrofauna'){
    variables$soortgroep = "Macrofauna_V2"
    variables$soortgroepnaam = c('Macrofauna','MFA')
    #} else if(input$rb_ecologische_groep == 'Macrofauna'){
    #  variables$soortgroep = "Macrofauna"
    #  variables$soortgroepnaam = c("MACEV", "MFA$", "MFAT", "Macrofauna")
    } else if(input$rb_ecologische_groep == 'Vissen'){
    variables$soortgroep = "Vissen"
    variables$soortgroepnaam = c("VISSN", "Vissen")
    }})
  
  observeEvent(input$rb_watertype, {
    if(input$rb_watertype == "Stilstaand water"){
      variables$watertype = "stilstaand"
    } else if(input$rb_watertype == "Stromend water"){
      variables$watertype = "stromend"
    }})
      
  
  ##========= weergave soortgroep ==================
  output$selected_soortgroep_abio <- renderText({req(input$rb_ecologische_groep); paste("Geselecteerde soortgroep: ", input$rb_ecologische_groep, sep = "")})
  output$selected_soortgroep_z    <- renderText({req(input$rb_ecologische_groep); paste("Geselecteerde soortgroep: ", input$rb_ecologische_groep, sep = "")})
  
  ##========= aanpassing radiobuttons bij referentiekeuze op basis van soortgroep ==================
  observe({
    
    req(input$rb_ecologische_groep)
    
    x <- input$rb_ecologische_groep
    
    if(x == "Macrofyten") {y <- c("KRW", "Eigen referentie")}
    if(x == "Diatomeeen" | x == "Macrofauna" | x == "Vissen") {y <- c("Eigen referentie")}
    
    # Can also set the label and select items
    updateRadioButtons(session, "sel_bron_doel",
                       label = NULL,
                       choices = y,
                       selected = y[1]
    )
  })
  
  
  
  output$huidigInvoerSoortenBestand_ui <- renderUI({
    input$reset_button
    fileInput("huidigInvoerSoortenBestand", label = NULL,
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"),
            buttonLabel = "Kies bestand",
            placeholder = "Er is nog geen bestand geselecteerd.")
  })
  
  output$refInvoerSoortenBestand_ui <- renderUI({
    input$reset_button
    fileInput("refInvoerSoortenBestand", 
              label = NULL,
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".txt",
                         ".xls",
                         ".xlsx"),
              buttonLabel = "Kies eigen referentiebestand",
              placeholder = "Er is nog geen bestand geselecteerd.")
  })
  
  
  ##==========make mapping================
  mapping <- reactive({
    req(variables$soortgroep)
    parametergroepenbestand <- paste(path_parametergroepen, sep = "")
    dfPAR <- read.table(parametergroepenbestand, header = T, sep = ";")
    dfPAR_MAP <- unique(dfPAR[dfPAR$soortgroep == variables$soortgroep, c("kolomnaam", "parameter_eenheid_kort")])
  })

  ##==huidig soorten ======  
  huidigSoortenPrep <- reactive({
    req(input$huidigInvoerSoortenBestand, variables$soortgroep, variables$soortgroepnaam)
    monsterlist <- inlezenDawaco(input$huidigInvoerSoortenBestand$datapath, variables$soortgroep, variables$soortgroepnaam)
    controlelist <- ControleMonsterData(monsterlist[[1]])
    # controlelist_samples <- controlelist[[1]]
    vertaallist <- VertaalSoorten(controlelist[[1]], variables$soortgroep, 'data/')
    waarschuwingen <- c(monsterlist[[2]], controlelist[[2]], vertaallist[[2]])
    fouten <- c(controlelist[[3]])
    return(list(lijst = vertaallist[[1]], waarschuwingen = waarschuwingen, fouten = fouten))
  })
  
  huidigSoorten <- reactive({
    req(huidigSoortenPrep())
    if(length(huidigSoortenPrep()[["lijst"]])==0){
      return(NULL)
    }
    if(length(huidigSoortenPrep()[["lijst"]])>0){
      return(huidigSoortenPrep())
    }
 })
  
  ##===huidig abiotiek============
  huidigAbiotiek <- reactive({
    req(huidigSoorten(), variables$soortgroep)
    return(abiotieklist <- BerekenAbiotiek(huidigSoorten()$lijst, variables$soortgroep, 'data/'))
  })
  
  ##===huidig abiotiek statistiek====
  huidigAbiotiekStatistiek <- reactive({
    req(huidigAbiotiek(), variables$soortgroep)
    statistieklist_huidig <- BerekenenStatistiekWaarden(huidigAbiotiek()[[1]], variables$soortgroep)
    return(statistieklist_huidig)
  })
  
  ##===huidig abiotiek gegroepeerd====
  huidigAbiotiekGroep <- reactive({
    req(huidigAbiotiek(), variables$soortgroep, variables$watertype)
    abiotiekdf <- bereidAbiotiekPlotData(huidigAbiotiek()[[1]], sg = variables$soortgroep)
    abiotiekgroepdf <- groepeerAbiotiekPlotData(abiotiekdf, sg = variables$soortgroep, wt = variables$watertype)
    return(abiotiekgroepdf)
  })
  
  
  ##==ref soorten======
  refSoorten <- reactive({
    req(input$sel_bron_doel)
    
    if(input$sel_bron_doel == 'KRW'){
      req(huidigAbiotiekGroep(), variables$soortgroep, variables$soortgroepnaam)
      reftypes_from_file <- as.vector(unique(huidigAbiotiekGroep()$reftype))
      krwDoelsoorten <- inlezenKRWrefDoelsoorten(path_KRWtypeRef, reftypes = reftypes_from_file, variables$soortgroep, variables$soortgroepnaam)
      controlelist <- ControleKRWData(krwDoelsoorten[[1]])
      controlelist_samples <- controlelist[[1]]
      waarschuwingen <- c(krwDoelsoorten[[2]],controlelist[[2]])
      fouten <- c(controlelist[[3]])
      return(list(lijst = controlelist_samples, waarschuwingen = waarschuwingen, fouten = fouten))
    }
    
    if(input$sel_bron_doel == 'Eigen referentie'){
      req(input$refInvoerSoortenBestand, variables$soortgroep, variables$soortgroepnaam)
      eigenDoelsoorten <- inlezenDoelsoorten(input$refInvoerSoortenBestand$datapath, variables$soortgroep, soortgroepnaam = variables$soortgroepnaam)
      controlelist = ControleDoelsoortData(eigenDoelsoorten[[1]])
      controlelist_samples <- controlelist[[1]]
      waarschuwingen <- c(eigenDoelsoorten[[2]], controlelist[[2]])
      fouten <- c(controlelist[[3]])
      return(list(lijst = controlelist_samples, waarschuwingen = waarschuwingen, fouten = fouten))
    }
  })
  
  ##===ref abiotiek statistiek====
  refAbiotiekStatistiek <- reactive({
      req(refSoorten(), variables$soortgroep)
      vertaallist        <- VertaalSoorten(refSoorten()$lijst, variables$soortgroep, 'data/')
      abiotieklist       <- BerekenAbiotiek(vertaallist[[1]], variables$soortgroep, 'data/')
      statistieklist_ref <- BerekenenStatistiekWaarden(abiotieklist[[1]], variables$soortgroep)
      return(statistieklist_ref)  
    })

  ##==zwaarden======
  zWaarden <- reactive({
    if(input$sel_bron_doel == 'KRW'){
      req(huidigAbiotiekStatistiek(), refAbiotiekStatistiek())
      ## calc z values
      matched  <- MatchMonsterKRWReferentie(huidigAbiotiekStatistiek()[[1]], refAbiotiekStatistiek()[[1]])
      zwaarden <- BerekenZWaarden(matched[[1]])
      return(zwaarden)  
    }
    if(input$sel_bron_doel == 'Eigen referentie'){
      req(huidigAbiotiekStatistiek(), refAbiotiekStatistiek())
      ## calc z values
      matched  <- MatchMonsterReferentie(huidigAbiotiekStatistiek()[[1]], refAbiotiekStatistiek()[[1]])
      zwaarden <- BerekenZWaarden(matched[[1]])
      return(zwaarden)                              
    }
  })
  
  ##==ref abiotiek================
  refAbiotiek <- reactive({
    req(input$sel_bron_doel)
    if(input$sel_bron_doel == 'KRW'){
      req(refSoorten(), variables$soortgroep)
      vertaallist  <- VertaalSoorten(refSoorten()$lijst, variables$soortgroep, 'data/')
      abiotieklist <- BerekenAbiotiek(vertaallist[[1]], variables$soortgroep, 'data/')
      return(abiotieklist)  
    }
    if(input$sel_bron_doel == 'Eigen referentie'){
      req(refSoorten(), variables$soortgroep)
      vertaallist  <- VertaalSoorten(refSoorten()$lijst, variables$soortgroep, 'data/')
      abiotieklist <- BerekenAbiotiek(vertaallist[[1]], variables$soortgroep, 'data/')
      return(abiotieklist)                              
    }
  })
  
  ##===ref abiotiek gegroepeerd===============
  refAbiotiekGroep <- reactive({
    req(input$sel_bron_doel)
    if(input$sel_bron_doel == 'KRW'){
      req(refAbiotiek(), variables$soortgroep, variables$watertype)
      doelAbiotiekDf <- bereidRefKRWAbiotiekPlotData(refAbiotiek()[[1]], sg = variables$soortgroep)
      doelAbiotiekGroepDf <- groepeerAbiotiekPlotData(doelAbiotiekDf, sg = variables$soortgroep, wt = variables$watertype)
      return(doelAbiotiekGroepDf)
      
    } else {
    req(refAbiotiek(),variables$soortgroep, variables$watertype)
    doelAbiotiekDf <- bereidRefAbiotiekPlotData(refAbiotiek()[[1]], sg = variables$soortgroep)
    doelAbiotiekGroepDf <- groepeerAbiotiekPlotData(doelAbiotiekDf, sg = variables$soortgroep, wt = variables$watertype)
    return(doelAbiotiekGroepDf)
    }
  })
  
  
  ## output 
  outputHuidigAbiotiekStatistiek <- reactive({
    req(huidigAbiotiekStatistiek(), variables$soortgroep)
    huidigAbioStat <- bereidStatistiekDataHuidig(huidigAbiotiekStatistiek()[[1]][[1]], sg = variables$soortgroep)
  }) 
  
  outputRefAbiotiekStatistiek <- reactive({
    req(refAbiotiekStatistiek(), variables$soortgroep, input$sel_bron_doel)
    refAbioStat <- bereidStatistiekDataRef(refAbiotiekStatistiek()[[1]][[1]], sg = variables$soortgroep, optie = input$sel_bron_doel)
  }) 
  
  
  
  
##==UI OPBOUW=========================================
  
  ##==dropdown box huidig abiotiek locatie==============
  output$huidigAbiotiekLocatie <- renderUI({
    req(huidigAbiotiekGroep())
    choice_of_locaties <- unique(na.omit(huidigAbiotiekGroep()$locname))
    selectInput("huidigAbiotiekLocatie_ui", "Locatie:", choice_of_locaties)
  })
  
  ##==dropdown box huidig abiotiek jaar=================
  output$huidigAbiotiekJaar <- renderUI({
    req(huidigAbiotiekGroep(), input$huidigAbiotiekLocatie_ui)
    choice_of_jaren <- sort(unique(na.omit(format(huidigAbiotiekGroep()[huidigAbiotiekGroep()$locname == input$huidigAbiotiekLocatie_ui, "datesmp"], "%Y"))))
    selectInput("huidigAbiotiekJaar_ui", "Jaar:", choice_of_jaren)
  })
  
  ##==dropdown box huidig abiotiek parametergroep=======
  output$huidigAbiotiekParametergroep <- renderUI({
    req(huidigAbiotiekGroep(), input$huidigAbiotiekLocatie_ui, input$huidigAbiotiekJaar_ui)
    choice_of_parametergroep <- unique(na.omit(huidigAbiotiekGroep()[huidigAbiotiekGroep()$locname == input$huidigAbiotiekLocatie_ui & 
                                                                       lubridate::year(huidigAbiotiekGroep()$datesmp) == input$huidigAbiotiekJaar_ui, "parametergroep"]))
    selectInput("huidigAbiotiekParametergroep_ui", "Ecologische sleutelfactor:", choice_of_parametergroep)
  })
  
  ##==dropdown box huidig abiotiek macrofyten ptype=====
  output$huidigAbiotiekPtype <- renderUI({
    req(huidigAbiotiekGroep(), input$huidigAbiotiekLocatie_ui, input$huidigAbiotiekJaar_ui, input$huidigAbiotiekParametergroep_ui)
    choice_of_ptype <- unique(na.omit(huidigAbiotiekGroep()[huidigAbiotiekGroep()$locname == input$huidigAbiotiekLocatie_ui & 
                                                            lubridate::year(huidigAbiotiekGroep()$datesmp) == input$huidigAbiotiekJaar_ui &
                                                            huidigAbiotiekGroep()$parametergroep == input$huidigAbiotiekParametergroep_ui, "type"]))
    selectInput("huidigAbiotiekPtype_ui", "Planttype:", choice_of_ptype)
  })
  
  ##==dropdown box ref abiotiek locatie==============
  output$refAbiotiekLocatie <- renderUI({
    if(input$sel_bron_doel == 'KRW'){
      req(refAbiotiekGroep())
      choice_of_reftype <- unique(na.omit(refAbiotiekGroep()$reftype))
      selectInput("refAbiotiekLocatie_ui", "Referentie type:", choice_of_reftype)
    } else{
    req(refAbiotiekGroep())
    choice_of_locaties <- unique(na.omit(refAbiotiekGroep()$locname))
    selectInput("refAbiotiekLocatie_ui", "Locatie:", choice_of_locaties)}
  })
  
  ##==dropdown box ref abiotiek parametergroep=======
  output$refAbiotiekParametergroep <- renderUI({
    if(input$sel_bron_doel == 'KRW'){
      req(refAbiotiekGroep())
      choice_of_parametergroep <- unique(na.omit(refAbiotiekGroep()[refAbiotiekGroep()$reftype == input$refAbiotiekLocatie_ui, "parametergroep"]))
      selectInput("refAbiotiekParametergroep_ui", "Ecologische sleutelfactor:", choice_of_parametergroep)
    } else{
    req(refAbiotiekGroep())
    choice_of_parametergroep <- unique(na.omit(refAbiotiekGroep()[refAbiotiekGroep()$locname == input$refAbiotiekLocatie_ui, "parametergroep"]))
    selectInput("refAbiotiekParametergroep_ui", "Ecologische sleutelfactor:", choice_of_parametergroep)}
  })
  
  ##==dropdown box ref abiotiek macrofyten ptype=====
  output$refAbiotiekPtype <- renderUI({
    if(input$sel_bron_doel == 'KRW'){
      req(refAbiotiekGroep())
      choice_of_ptype <- unique(na.omit(refAbiotiekGroep()[refAbiotiekGroep()$reftype == input$refAbiotiekLocatie_ui & 
                                                             refAbiotiekGroep()$parametergroep == input$refAbiotiekParametergroep_ui, "type"]))
      selectInput("refAbiotiekPtype_ui", "Planttype:", choice_of_ptype)
  } else{
    req(refAbiotiekGroep())
    choice_of_ptype <- unique(na.omit(refAbiotiekGroep()[refAbiotiekGroep()$locname == input$refAbiotiekLocatie_ui & 
                                                         refAbiotiekGroep()$parametergroep == input$refAbiotiekParametergroep_ui, "type"]))
    selectInput("refAbiotiekPtype_ui", "Planttype:", choice_of_ptype)}
  })
  
  ##==dropdown box zwaarden overzicht locatie==============
  output$zwaardenOverzichtLocaties <- renderUI({
    req(zPlotDf())
    choice_of_locaties <- unique(na.omit(zPlotDf()$locname))
    selectInput("zwaardenOverzichtLocatie_ui", "Locatie:", choice_of_locaties)
  })
  
  ##==dropdown box zwaarden overzicht jaar==============
  output$zwaardenOverzichtJaar <- renderUI({
    req(zPlotDf())
    choice_of_jaren <- sort(unique(na.omit(zPlotDf()[zPlotDf()$locname == input$zwaardenOverzichtLocatie_ui, "jaar"])))
    selectInput("zwaardenOverzichtJaar_ui", "Jaar:", choice_of_jaren)
  })
  
  ##==dropdown box zwaarden overzicht macrofyten ptype==============
  output$zwaardenOverzichtPtype <- renderUI({
    req(zPlotDf())
    choice_of_ptype <- unique(na.omit(zPlotDf()[zPlotDf()$locname == input$zwaardenOverzichtLocatie_ui &
                                                zPlotDf()$jaar == input$zwaardenOverzichtJaar_ui, "soorttype"]))
    selectInput("zwaardenOverzichtPtype_ui", "Soorttype:", choice_of_ptype)
  })
  
  ##==dropdown box zwaarden ruimte jaar==============
  output$zwaardenRuimteJaar <- renderUI({
    req(zPlotDf())
    choice_of_jaren <- sort(unique(na.omit(zPlotDf()$jaar)))
    selectInput("zwaardenRuimteJaar_ui", "Jaar:", choice_of_jaren)
  })
  
  ##==dropdown box zwaarden ruimte parameter==============
  output$zwaardenRuimteParameter <- renderUI({
    req(zPlotDf())
    choice_of_parameter <- unique(na.omit(zPlotDf()[zPlotDf()$jaar == input$zwaardenRuimteJaar_ui, "Parameter"]))
    selectInput("zwaardenRuimteParameter_ui", "Parameter:", choice_of_parameter)
  })

  ##==dropdown box zwaarden ruimte macrofyten ptype==============
  output$zwaardenRuimtePtype <- renderUI({
    req(zPlotDf())
    choice_of_ptype <- unique(na.omit(zPlotDf()[zPlotDf()$jaar == input$zwaardenRuimteJaar_ui &
                                      zPlotDf()$Parameter == input$zwaardenRuimteParameter_ui, "soorttype"]))
    selectInput("zwaardenRuimtePtype_ui", "Type:", choice_of_ptype)
  })
  
  ##==dropdown box zwaarden tijd locatie==============
  output$zwaardenTijdLocatie <- renderUI({
    req(zPlotDf())
    choice_of_locatie <- sort(unique(na.omit(zPlotDf()$locname)))
    selectInput("zwaardenTijdLocatie_ui", "Locatie:", choice_of_locatie)
  })
  
  
  ##==dropdown box zwaarden tijd parametergroep=======
  output$zwaardenTijdParametergroep <- renderUI({
    req(zPlotDf(), input$zwaardenTijdLocatie_ui)
    choice_of_parametergroep <- unique(na.omit(zPlotDf()[zPlotDf()$locname == input$zwaardenTijdLocatie_ui, "parametergroep"]))
    selectInput("zwaardenTijdParametergroep_ui", "Ecologische sleutelfactor:", choice_of_parametergroep)
  })
  
  ##==dropdown box zwaarden tijd ptype=======
  output$zwaardenTijdPtype <- renderUI({
    req(zPlotDf(), input$zwaardenTijdLocatie_ui)
    choice_of_ptype <- unique(na.omit(zPlotDf()[zPlotDf()$locname == input$zwaardenTijdLocatie_ui &
                                                zPlotDf()$parametergroep == input$zwaardenTijdParametergroep_ui, "soorttype"]))
    selectInput("zwaardenTijdPtype_ui", "Soorttype:", choice_of_ptype)
  })
  
  ##==fouten huidige abiotiek==========================
  output$huidigFouten <- renderText({
    req(huidigSoortenPrep()[["fouten"]])
    ifelse(any(huidigSoortenPrep()[["fouten"]] == 1), "Het aangeboden bestand is niet ingelezen. Bekijk de onderstaande meldingen om de reden te achterhalen.", "Het aangeboden bestand is goed ingelezen.")
  })
  
  ##==fouten ref abiotiek==============================
  output$refFouten <- renderText({
    if(input$sel_bron_doel == 'KRW'){tekst <- "Er wordt gebruik gemaakt van de KRW referentiewaardes."}
    if(input$sel_bron_doel == 'Eigen referentie'){
      req(refSoorten()$fouten)
      ifelse(any(refSoorten()$fouten == 1), tekst <- "Het aangeboden bestand is niet ingelezen. Bekijk de onderstaande meldingen om de reden te achterhalen.", tekst <- "Het aangeboden bestand is goed ingelezen.")}
    tekst
    })
  
  ##==waarschuwing huidige abiotiek========================
  output$huidigWaarschuwingen <- renderText({
    req(huidigSoortenPrep()[["waarschuwingen"]])
    if(length(huidigSoortenPrep()$waarschuwingen) > 0){
      wrsch <- paste(as.vector(unlist(huidigSoortenPrep()[["waarschuwingen"]])), collapse = "\n")
    }
    ifelse(length(huidigSoortenPrep()[["waarschuwingen"]]) > 0, wrsch, "Er zijn geen waarschuwingen ontstaan bij het inlezen.")
  })
  
  ##==waarschuwingen ref abiotiek=======================
  output$refWaarschuwingen <- renderText({
    if(input$sel_bron_doel == 'KRW'){tekst <- NULL}
    if(input$sel_bron_doel == 'Eigen referentie'){
    req(refSoorten()$waarschuwingen)
    if(length(refSoorten()$waarschuwingen) > 0){
      wrsch <- paste(as.vector(unlist(refSoorten()[["waarschuwingen"]])), collapse = "\n")
    }
    ifelse(length(refSoorten()$waarschuwingen) > 0, tekst <- wrsch, tekst <- "Er zijn geen waarschuwingen ontstaan bij het inlezen.")
    }
    tekst
    })
  
  ##==tabel huidige abiotiek==================
  output$huidigInvoerSoortenTabel <- renderTable({
    req(huidigSoorten())
    df <- as.data.frame(huidigSoorten()$lijst[[1]])
    df$datesmp <- as.character(df$datesmp)
    if(nrow(df)>10){df <- df[1:10, ]}
    return(df)
  })
  
  ##===tabel ref abiotiek====================
  output$refInvoerSoortenTabel <- renderTable({
    req(length(refSoorten()[["lijst"]])>0)
    #if(input$sel_bron_doel == 'KRW'){NULL}
    #if(input$sel_bron_doel == 'Eigen referentie'){
    df <- as.data.frame(refSoorten()$lijst[[1]])
    if(nrow(df)>10){df <- df[1:10, ]}
    return(df)
    #}
  })
  
  ##==tabel statistiek abiotiek=====
  huidigSelectionDf <- reactive({
    req(huidigAbiotiekStatistiek())
    library(lubridate)
    
    df <- data.frame(locatie = character(), entry = numeric(), stringsAsFactors = F)
    for(i in 1:length(huidigAbiotiekStatistiek()$Data$Sample_data)){
      # i <- 1
      df[i, "locatie"] <- as.character(huidigAbiotiekStatistiek()$Data$Sample_data[[i]]$locname)
      df[i, "jaar"]    <- year(huidigAbiotiekStatistiek()$Data$Sample_data[[1]]$datesmp)
      df[i, "entry"]  <- i
    }
    return(df)
  })
  
  output$huidigAbiotiekStatistiekTabel <- renderTable({
    
    if(input$rb_ecologische_groep == "Macrofyten"){
    req(variables$soortgroep, huidigAbiotiekStatistiek(), huidigSelectionDf(),  input$huidigAbiotiekLocatie_ui, input$huidigAbiotiekJaar_ui, input$huidigAbiotiekPtype_ui)
    output_stat  <- huidigAbiotiekStatistiek()$Data$Sample_data[[huidigSelectionDf()[huidigSelectionDf()$locatie == input$huidigAbiotiekLocatie_ui, "entry"][1]]]$Statistic_results[[input$huidigAbiotiekPtype_ui]]
    kolommen     <- colnames(output_stat)[which(colnames(output_stat)!= "Statistiek")]
    output_stat2 <- apply(output_stat[, kolommen], 2, FUN = function(x) {round(as.numeric(as.character(x)), 2)})
    output_stat3 <- data.frame(Statistiek = output_stat[, "Statistiek"], output_stat2)
    colnames(output_stat3) <- mapvalues(colnames(output_stat3), 
                             from = c("Statistiek", as.vector(as.character(mapping()$kolomnaam))),
                             to = c("Statistiek", as.vector(as.character(mapping()$parameter_eenheid_kort))))
          }
    
    if(input$rb_ecologische_groep != "Macrofyten"){ 
      req(variables$soortgroep, huidigAbiotiekStatistiek(), huidigSelectionDf(),  input$huidigAbiotiekLocatie_ui, input$huidigAbiotiekJaar_ui)
    output_stat <- huidigAbiotiekStatistiek()$Data$Sample_data[[huidigSelectionDf()[huidigSelectionDf()$locatie == input$huidigAbiotiekLocatie_ui, "entry"][1]]]$Statistic_results[[1]]
    kolommen     <- colnames(output_stat)[which(colnames(output_stat)!= "Statistiek")]
    output_stat2 <- apply(output_stat[, kolommen], 2, FUN = function(x) {round(as.numeric(as.character(x)), 2)})
    output_stat3 <- data.frame(Statistiek = output_stat[, "Statistiek"], output_stat2)
    colnames(output_stat3) <- mapvalues(colnames(output_stat3), 
                                        from = c("Statistiek", as.vector(as.character(mapping()$kolomnaam))),
                                        to = c("Statistiek", as.vector(as.character(mapping()$parameter_eenheid_kort))))
    }
    return(output_stat3)
    })
 
  
  refSelectionDf <- reactive({
    if(input$sel_bron_doel == 'KRW'){
      req(refAbiotiekStatistiek())
      library(lubridate)
      
      df <- data.frame(reftype = character(), entry = numeric(), stringsAsFactors = F)
      for(i in 1:length(refAbiotiekStatistiek()$Data$Sample_data)){
        # i <- 1
        df[i, "reftype"] <- as.character(refAbiotiekStatistiek()$Data$Sample_data[[i]]$reftype)
        df[i, "entry"]  <- i
    }} else {
    req(refAbiotiekStatistiek())
    library(lubridate)
    
    df <- data.frame(locatie = character(), entry = numeric(), stringsAsFactors = F)
    for(i in 1:length(refAbiotiekStatistiek()$Data$Sample_data)){
      # i <- 1
      df[i, "locatie"] <- as.character(refAbiotiekStatistiek()$Data$Sample_data[[i]]$locname)
      df[i, "entry"]  <- i
    }}
    return(df)
  })
  
  
  refStatistiekTabel <- reactive({
    
  if(input$rb_ecologische_groep == "Macrofyten"){
    if(input$sel_bron_doel == 'KRW'){
      req(variables$soortgroep, refAbiotiekStatistiek(), refSelectionDf(),  input$refAbiotiekLocatie_ui, input$refAbiotiekPtype_ui)
      output_stat <- refAbiotiekStatistiek()$Data$Sample_data[[refSelectionDf()[refSelectionDf()$reftype == input$refAbiotiekLocatie_ui, "entry"]]]$Statistic_results[[input$refAbiotiekPtype_ui]]
      kolommen     <- colnames(output_stat)[which(colnames(output_stat)!= "Statistiek")]
      output_stat2 <- apply(output_stat[, kolommen], 2, FUN = function(x) {round(as.numeric(as.character(x)), 2)})
      output_stat3 <- data.frame(Statistiek = output_stat[, "Statistiek"], output_stat2)
      colnames(output_stat3) <- mapvalues(colnames(output_stat3), 
                                          from = c("Statistiek", as.vector(as.character(mapping()$kolomnaam))),
                                          to = c("Statistiek", as.vector(as.character(mapping()$parameter_eenheid_kort))))
      
    } else {
      req(variables$soortgroep, refAbiotiekStatistiek(), refSelectionDf(),  input$refAbiotiekLocatie_ui, input$refAbiotiekPtype_ui)
      output_stat <- refAbiotiekStatistiek()$Data$Sample_data[[refSelectionDf()[refSelectionDf()$locatie == input$refAbiotiekLocatie_ui, "entry"]]]$Statistic_results[[input$refAbiotiekPtype_ui]]
      kolommen     <- colnames(output_stat)[which(colnames(output_stat)!= "Statistiek")]
      output_stat2 <- apply(output_stat[, kolommen], 2, FUN = function(x) {round(as.numeric(as.character(x)), 2)})
      output_stat3 <- data.frame(Statistiek = output_stat[, "Statistiek"], output_stat2)
      colnames(output_stat3) <- mapvalues(colnames(output_stat3), 
                                          from = c("Statistiek", as.vector(as.character(mapping()$kolomnaam))),
                                          to = c("Statistiek", as.vector(as.character(mapping()$parameter_eenheid_kort))))
      
    }}
  
  if(input$rb_ecologische_groep != "Macrofyten"){ 
    req(variables$soortgroep, refAbiotiekStatistiek(), refSelectionDf(),  input$refAbiotiekLocatie_ui)
    output_stat <- refAbiotiekStatistiek()$Data$Sample_data[[refSelectionDf()[refSelectionDf()$locatie == input$refAbiotiekLocatie_ui, "entry"]]]$Statistic_results[[1]]
    kolommen     <- colnames(output_stat)[which(colnames(output_stat)!= "Statistiek")]
    output_stat2 <- apply(output_stat[, kolommen], 2, FUN = function(x) {round(as.numeric(as.character(x)), 2)})
    output_stat3 <- data.frame(Statistiek = output_stat[, "Statistiek"], output_stat2)
    colnames(output_stat3) <- mapvalues(colnames(output_stat3), 
                                        from = c("Statistiek", as.vector(as.character(mapping()$kolomnaam))),
                                        to = c("Statistiek", as.vector(as.character(mapping()$parameter_eenheid_kort))))
  }
  
  return(output_stat3)
  
  })
  
  output$refAbiotiekStatistiekTabel <- renderTable({
    refStatistiekTabel()
  })
  

  ##===plots huidig abiotiek=============
  huidigAbiotiekPlotPrep <- reactive({
    
    if(input$rb_ecologische_groep == 'Macrofyten'){
      
      req(huidigAbiotiekGroep(), input$huidigAbiotiekLocatie_ui, input$huidigAbiotiekJaar_ui, input$huidigAbiotiekParametergroep_ui, input$huidigAbiotiekPtype_ui)
      validate(need(nrow(huidigAbiotiekGroep()[huidigAbiotiekGroep()$locname == input$huidigAbiotiekLocatie_ui &
                                               lubridate::year(huidigAbiotiekGroep()$datesmp) == input$huidigAbiotiekJaar_ui &
                                               huidigAbiotiekGroep()$parametergroep == input$huidigAbiotiekParametergroep_ui &
                                               huidigAbiotiekGroep()$type == input$huidigAbiotiekPtype_ui, ]) > 0, message = FALSE))
      
      plotAbio <- plotAbiotiek(huidigAbiotiekGroep(), jaar = input$huidigAbiotiekJaar_ui, locatie = input$huidigAbiotiekLocatie_ui, pg = input$huidigAbiotiekParametergroep_ui, ptype = input$huidigAbiotiekPtype_ui, sg = variables$soortgroep)
    
    }
    
    if(input$rb_ecologische_groep != 'Macrofyten'){
      req(huidigAbiotiekGroep(), input$huidigAbiotiekLocatie_ui, input$huidigAbiotiekJaar_ui, input$huidigAbiotiekParametergroep_ui)
      validate(need(nrow(huidigAbiotiekGroep()[huidigAbiotiekGroep()$locname == input$huidigAbiotiekLocatie_ui &
                                                 lubridate::year(huidigAbiotiekGroep()$datesmp) == input$huidigAbiotiekJaar_ui &
                                                 huidigAbiotiekGroep()$parametergroep == input$huidigAbiotiekParametergroep_ui, ]) > 0, message = FALSE))
      
      plotAbio <- plotAbiotiek(huidigAbiotiekGroep(), jaar = input$huidigAbiotiekJaar_ui, locatie = input$huidigAbiotiekLocatie_ui, pg = input$huidigAbiotiekParametergroep_ui, sg = variables$soortgroep)} 

    return(plotAbio)
    })

  wi <- reactive({
    req(huidigAbiotiekPlotPrep())
    gg_facet_ncol(huidigAbiotiekPlotPrep())})
  
  output$huidigAbiotiekPlots <- renderPlot({
    req(huidigAbiotiekPlotPrep())
    huidigAbiotiekPlotPrep()}, height = 300, width = function(){wi()*150 + 100})
  
  
  ##==== download data prep ====
  
  downloadAbiotiekHuidigRuw <- reactive({
    
    if(variables$soortgroep != "Macrofyten"){
      selcols <- c("loccode", "locX", "locY", "datesmp", "soortgroep", "species_name", "reftype", "parametergroep", "parameter", "parameter_eenheid", "waarde")
      namecols <- c("locatiecode", "xcoor", "ycoor", "datum", "soortgroep", "soortnaam", "KRW_referentietype", "parametergroe", "parameter", "parameter_eenheid", "waarde")
    
    dfDL <- huidigAbiotiekGroep()[, selcols]
    colnames(dfDL) <- namecols
    }
    if(variables$soortgroep == "Macrofyten"){
      selcols <- c("loccode", "locX", "locY", "datesmp", "soortgroep", "type", "species_name", "reftype", "parametergroep", "parameter", "parameter_eenheid", "waarde")
      namecols <- c("locatiecode", "xcoor", "ycoor", "datum", "soortgroep", "type", "soortnaam", "KRW_referentietype", "parametergroe", "parameter", "parameter_eenheid", "waarde")
      
      dfDL <- huidigAbiotiekGroep()[, selcols]
      colnames(dfDL) <- namecols
    }
    
    return(dfDL)
    
  })
 
  
  downloadAbiotiekRefRuw <- reactive({
    
    
    if(input$sel_bron_doel == "KRW"){
      
      if(variables$soortgroep != "Macrofyten"){
        selcols <- c("reftype", "soortgroep", "species_name", "parametergroep", "parameter", "parameter_eenheid", "waarde")
        namecols <- c("KRW_referentietype", "soortgroep", "soortnaam", "parametergroep", "parameter", "parameter_eenheid", "waarde")
        
        dfDL <- refAbiotiekGroep()[, selcols]
        colnames(dfDL) <- namecols
      }
      if(variables$soortgroep == "Macrofyten"){
        selcols <- c("reftype", "soortgroep", "type", "species_name", "parametergroep", "parameter", "parameter_eenheid", "waarde")
        namecols <- c("KRW_referentietype", "soortgroep", "type", "soortnaam", "parametergroep", "parameter", "parameter_eenheid", "waarde")
        
        dfDL <- refAbiotiekGroep()[, selcols]
        colnames(dfDL) <- namecols
      }
    } else{
      
      if(variables$soortgroep != "Macrofyten"){
        selcols <- c("locname", "soortgroep", "species_name", "parametergroep", "parameter", "parameter_eenheid", "waarde")
        namecols <- c("locatiecode", "soortgroep", "soortnaam", "parametergroep", "parameter", "parameter_eenheid", "waarde")
        
        dfDL <- refAbiotiekGroep()[, selcols]
        colnames(dfDL) <- namecols
      }
      if(variables$soortgroep == "Macrofyten"){
        selcols <- c("locname", "soortgroep", "type", "species_name", "parametergroep", "parameter", "parameter_eenheid", "waarde")
        namecols <- c("locatiecode", "soortgroep", "type", "soortnaam", "parametergroep", "parameter", "parameter_eenheid", "waarde")
        
        dfDL <- refAbiotiekGroep()[, selcols]
        colnames(dfDL) <- namecols
      }
    }
    return(dfDL)
    
  })
  
  zPlotDfOutput <- reactive({
    req(zPlotDf())
    
    # df.plotz$soortgroep <- "Macrofyten"
    # dfZout <- df.plotz
    
    dfZout <- zPlotDf()
    dfZout$soortgroep <- variables$soortgroep
    
    if(variables$soortgroep != "Macrofyten"){
    selcols <- c("locname", "locX", "locY", "jaar", "soortgroep", "Parameter", "Zwaarde", "klasse")
    namecols <- c("locatiecode", "xcoordinaat", "ycoordinaat", "jaar", "soortgroep", "parameter", "z_waarde", "klasse")
    dfDL <- dfZout[, selcols]
    colnames(dfDL) <- namecols
    }
    
    if(variables$soortgroep == "Macrofyten"){
      selcols <- c("locname", "locX", "locY", "jaar", "soortgroep", "soorttype", "Parameter", "Zwaarde", "klasse")
      namecols <- c("locatiecode", "xcoordinaat", "ycoordinaat", "jaar", "soortgroep", "type", "parameter", "z_waarde", "klasse")
      dfDL <- dfZout[, selcols]
      colnames(dfDL) <- namecols
    }
    return(dfDL)
  })
  
  
  
  ##===plots ref abiotiek=============
  refAbiotiekPlotPrep <- reactive({
    if(input$rb_ecologische_groep == 'Macrofyten'){
      if(input$sel_bron_doel == 'KRW'){
        req(refAbiotiekGroep(), input$refAbiotiekLocatie_ui, input$refAbiotiekParametergroep_ui, input$refAbiotiekPtype_ui)
        validate(need(nrow(refAbiotiekGroep()[refAbiotiekGroep()$reftype == input$refAbiotiekLocatie_ui &
                                                refAbiotiekGroep()$parametergroep == input$refAbiotiekParametergroep_ui &
                                                refAbiotiekGroep()$type == input$refAbiotiekPtype_ui, ]) > 0, message = FALSE))
        plotAbio <- plotAbiotiek(refAbiotiekGroep(), refrtype = input$refAbiotiekLocatie_ui, pg = input$refAbiotiekParametergroep_ui, ptype = input$refAbiotiekPtype_ui, sg = variables$soortgroep)
      } else {
      req(refAbiotiekGroep(), input$refAbiotiekLocatie_ui, input$refAbiotiekParametergroep_ui, input$refAbiotiekPtype_ui)
      validate(need(nrow(refAbiotiekGroep()[refAbiotiekGroep()$locname == input$refAbiotiekLocatie_ui &
                                            refAbiotiekGroep()$parametergroep == input$refAbiotiekParametergroep_ui &
                                            refAbiotiekGroep()$type == input$refAbiotiekPtype_ui, ]) > 0, message = FALSE))
      plotAbio <- plotAbiotiek(refAbiotiekGroep(), locatie = input$refAbiotiekLocatie_ui, pg = input$refAbiotiekParametergroep_ui, ptype = input$refAbiotiekPtype_ui, sg = variables$soortgroep)
    }}
    if(input$rb_ecologische_groep != 'Macrofyten'){
      req(refAbiotiekGroep(), input$refAbiotiekLocatie_ui, input$refAbiotiekParametergroep_ui)
      validate(need(nrow(refAbiotiekGroep()[refAbiotiekGroep()$locname == input$refAbiotiekLocatie_ui &
                                            refAbiotiekGroep()$parametergroep == input$refAbiotiekParametergroep_ui, ]) > 0, message = FALSE))
      plotAbio <- plotAbiotiek(refAbiotiekGroep(), locatie = input$refAbiotiekLocatie_ui, pg = input$refAbiotiekParametergroep_ui, sg = variables$soortgroep)} 
    
    return(plotAbio)
  })
  
  refWidth <- reactive({
    req(refAbiotiekPlotPrep())
    gg_facet_ncol(refAbiotiekPlotPrep())})
  
  output$refAbiotiekPlots <- renderPlot({
    req(refAbiotiekPlotPrep())
    refAbiotiekPlotPrep()}, height = 300, width = function(){refWidth()*150 + 100})
  
  
  
  ##===zwaardeplots voorbereiding==========
  zPlotDf <- reactive({
    req(zWaarden())
    zwaarden <- bereidZplotData(zWaarden())
    zwaarden2 <- groepeerZplotData(zwaarden, sg = variables$soortgroep, wt = variables$watertype)
    return(zwaarden2)
  })
  
  
 
  ##===zwaarde overzicht==========
  output$zWaardenTabel <- renderPlot({
    req(zPlotDf(), input$zwaardenOverzichtLocatie_ui, input$zwaardenOverzichtJaar_ui, input$zwaardenOverzichtPtype_ui)
    plotZwaarden(zdf = zPlotDf(), locaties = input$zwaardenOverzichtLocatie_ui, jaartal = input$zwaardenOverzichtJaar_ui, ptype = input$zwaardenOverzichtPtype_ui)
  })
  
  
  
  ##===zwaarde tijd==========
  zTijdWaardePlotPrep <- reactive({
    req(zPlotDf(), input$zwaardenTijdLocatie_ui, input$zwaardenTijdParametergroep_ui, input$zwaardenTijdPtype_ui)
    plotZtijdseries(zdf = zPlotDf(), locaties = input$zwaardenTijdLocatie_ui, pg = input$zwaardenTijdParametergroep_ui, ptype = input$zwaardenTijdPtype_ui)
  })
  
  he <- reactive({req(zTijdWaardePlotPrep())
                  gg_facet_nrow(zTijdWaardePlotPrep())})
  
  output$zTijdWaardePlot <- renderPlot({
    req(zTijdWaardePlotPrep(), he())
    zTijdWaardePlotPrep()}, height = function(){he()*130 + 80}, width = 900)
  
  
  
  ##===zwaarde ruimte==========
  output$zWaardenLocaties <- renderLeaflet({
    req(zPlotDf(), input$zwaardenRuimteJaar_ui, input$zwaardenRuimteParameter_ui, input$zwaardenRuimtePtype_ui)
    plotZwaardeLocaties(zdf = zPlotDf(), zjaar = input$zwaardenRuimteJaar_ui, zpar = input$zwaardenRuimteParameter_ui, ptype = input$zwaardenRuimtePtype_ui)
  })
  
  zWaardenLocatiesDownload <- reactive({
    req(zPlotDf(), input$zwaardenRuimteJaar_ui, input$zwaardenRuimteParameter_ui, input$zwaardenRuimtePtype_ui)
    plotZwaardeLocaties(zdf = zPlotDf(), zjaar = input$zwaardenRuimteJaar_ui, zpar = input$zwaardenRuimteParameter_ui, ptype = input$zwaardenRuimtePtype_ui)
    
  })
  
  

##=downloads======================

  
  ##== download logbestanden ====
  huidigLog <- renderText({
    req(huidigSoortenPrep()[["fouten"]], huidigSoortenPrep()[["waarschuwingen"]])
    ifelse(any(huidigSoortenPrep()[["fouten"]] == 1), fout <- "Het aangeboden bestand is niet ingelezen. Bekijk de onderstaande meldingen om de reden te achterhalen.", fout <- "Het aangeboden bestand is goed ingelezen.")
    if(length(huidigSoortenPrep()$waarschuwingen) > 0){
      wrsch <- paste(as.vector(unlist(huidigSoortenPrep()[["waarschuwingen"]])), collapse = "\n")
    }
    ifelse(length(huidigSoortenPrep()[["waarschuwingen"]]) > 0, waarschuwing <- wrsch, waarschuwing <- "Er zijn geen waarschuwingen ontstaan bij het inlezen.")
    output <- paste(fout, waarschuwing, sep ="\n")
    return(output)
    })

  
  referentieLog <- renderText({
    req(input$sel_bron_doel, refSoorten()$fouten, refSoorten()$waarschuwingen)
    
    ## KRW case
    if(input$sel_bron_doel == 'KRW'){
      KRW <- "Er wordt gebruik gemaakt van de KRW referentiewaardes."
      fouten <- NULL
      waarschuwingen <- NULL}
    
    if(input$sel_bron_doel == 'Eigen referentie'){
      ## krw
      KRW <- NULL
      ## fouten
      ifelse(any(refSoorten()$fouten == 1), fouten <- "Het aangeboden bestand is niet ingelezen. Bekijk de onderstaande meldingen om de reden te achterhalen.", fouten <- "Het aangeboden bestand is goed ingelezen.")
      ## waarschuwingen
      if(length(refSoorten()$waarschuwingen) > 0){
        wrsch <- paste(as.vector(unlist(refSoorten()[["waarschuwingen"]])), collapse = "\n")
      }
      ifelse(length(refSoorten()$waarschuwingen) > 0, waarschuwingen <- wrsch, waarschuwingen <- "Er zijn geen waarschuwingen ontstaan bij het inlezen.")
    }
    output <- paste(KRW, fouten, waarschuwingen, sep = "\n")
  })
  
  
output$downloadHuidigLogbestand <- downloadHandler(
  filename = function(){paste(gsub(".txt", "", input$huidigInvoerSoortenBestand), "_log.txt", sep = "")},
  content = function(file){
    validate(
      need(!is.null(huidigLog()), "Laad eerst een dataset.")
    )
    write.table(huidigLog(), file = file, sep = "\n", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    write.csv},
  contentType = "text/csv"
)  

output$downloadReferentieLogbestand <- downloadHandler(
  filename = function(){paste(gsub(".txt", "", input$refInvoerSoortenBestand), "_log.txt", sep = "")},
  content = function(file){
    write.table(referentieLog(), file = file, sep = "\n", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    write.csv},
  contentType = "text/csv"
)  


  ## == download data ====
#output$zwaarden_map <- downloadHandler(
#    filename = function(){paste(input$zwaardenRuimteJaar_ui, "_", input$zwaardenRuimteParameter_ui, "_", input$zwaardenRuimtePtype_ui, ".png", sep = "")},
#    content = function(file){
#      mapshot(zWaardenLocatiesDownload(), file = file)
#    }
#  )

output$zwaarden_map <- downloadHandler(
  filename = function(){paste(input$zwaardenRuimteJaar_ui, "_", input$zwaardenRuimteParameter_ui, "_", input$zwaardenRuimtePtype_ui, ".html", sep = "")},
  content = function(file){
    saveWidget(widget = zWaardenLocatiesDownload(), file = file)
  }
)

output$downloadHuidigAbiotiek <- downloadHandler(
  filename = function(){paste(gsub(".txt", "", input$huidigInvoerSoortenBestand), ".csv", sep = "")},
  content = function(file){ write.table(downloadAbiotiekHuidigRuw(), file, sep = ";", dec = ".", row.names = FALSE)
    write.csv},
  contentType = "text/csv"
  )

output$downloadHuidigAbiotiekStatistiek <- downloadHandler(
 filename = function(){paste(gsub(".txt", "", input$huidigInvoerSoortenBestand), "_statistiek.csv", sep = "")},
 content = function(file){ write.table(outputHuidigAbiotiekStatistiek(), file, sep = ";", dec = ".", row.names = FALSE)
   write.csv},
 contentType = "text/csv"
)
 
output$downloadRefAbiotiek <- downloadHandler(
  filename = function(){paste("Abiotiek_referentie_", Sys.Date(), ".csv", sep = "")},
  content = function(file){ write.table(downloadAbiotiekRefRuw(), file, sep = ";", dec = ".", row.names = FALSE)},
  contentType = "text/csv"
)

output$downloadRefAbiotiekStatistiek <- downloadHandler(
  filename = function(){paste("Abiotiek_referentie_", Sys.Date(), "_statistiek.csv", sep = "")},
  content = function(file){ write.table(outputRefAbiotiekStatistiek(), file, sep = ";", dec = ".", row.names = FALSE)
    write.csv},
  contentType = "text/csv"
)

output$downloadZwaardes <- downloadHandler(
  filename = function(){paste("Zwaardes", Sys.Date(), ".csv", sep = "")},
  content = function(file){ write.table(zPlotDfOutput(), file, sep = ";", dec = ".", row.names = FALSE)},
  contentType = "text/csv"
)

## == download achtergronddocumenten ====
output$handleiding_aqmad <- downloadHandler(
  filename = function(){paste("Van Geest et al 2019 - Handleiding webapplicatie AqMaD.pdf")},
  content <- function(file){file.copy("documentatie/Van Geest et al 2019 - Handleiding webapplicatie AqMaD.pdf", file)},
  contentType = "application/pdf"
)

output$indeling_planten <- downloadHandler(
  filename = function(){paste("indeling_planten.csv")},
  content <- function(file){file.copy("data/AqMaD_Macrofyten_v1_1_compartiment.csv", file)},
  contentType = "text/csv"
)

output$soortgroep_macrofyten <- downloadHandler(
  filename = function(){paste("Jaarsma 2016 Vergelijking indicatiewaarde waterplanten - hoofdrapport.pdf")},
  content <- function(file){file.copy("documentatie/Jaarsma 2016 Vergelijking indicatiewaarde waterplanten - hoofdrapport.pdf", file)},
  contentType = "application/pdf"
)

output$soortgroep_macrofyten_2 <- downloadHandler(
  filename = function(){paste("Jaarsma 2016 Vergelijking indicatiewaarde waterplanten - bijlagen.pdf")},
  content <- function(file){file.copy("documentatie/Jaarsma 2016 Vergelijking indicatiewaarde waterplanten - bijlagen.pdf", file)},
  contentType = "application/pdf"
)

output$soortgroep_vissen <- downloadHandler(
  filename = function(){paste("Buijse_2016.pdf")},
  content <- function(file){file.copy("documentatie/Buijse 2016 - Notitie AqMad vissen.docx", file)},
  contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
)

output$soortgroep_macrofauna <- downloadHandler(
  filename = function(){paste("Verdonschot_2011.pdf")},
  content <- function(file){file.copy("documentatie/Verdonschot 2011 Interne rapportage kennisregels macrofauna AqMaD versie 5 juli.pdf", file)},
  contentType = "application/pdf"
)

output$soortgroep_macrofauna_2 <- downloadHandler(
  filename = function(){paste("Verberketal_2012.pdf")},
  content <- function(file){file.copy("documentatie/Verberk 2012 Habitat en milieupreferenties macrofauna.pdf", file)},
  contentType = "application/pdf"
)


output$soortgroep_diatomeeen <- downloadHandler(
  filename = function(){paste("VanDam_2010.pdf")},
  content <- function(file){file.copy("documentatie/Van Dam 2010 Diatomeeen.pdf", file)},
  contentType = "application/pdf"
)

## == download voorbeelddata ====
output$monitordata_diatomeeen_voorbeeld <- downloadHandler(
  filename = function(){paste("Monitordata_diatomeeen_voorbeeld.txt")},
  content <- function(file){file.copy("voorbeelddata/Monitordata_diatomeeen_voorbeeld.txt", file)},
  contentType = "text/plain"
)

output$referentiedata_diatomeeen_voorbeeld <- downloadHandler(
  filename = function(){paste("Referentiedata_diatomeeen_voorbeeld.csv")},
  content <- function(file){file.copy("voorbeelddata/Referentiedata_diatomeeen_voorbeeld.csv", file)},
  contentType = "text/csv"
)

output$monitordata_planten_voorbeeld <- downloadHandler(
  filename = function(){paste("Monitordata_macrofyten_voorbeeld.txt")},
  content <- function(file){file.copy("voorbeelddata/Monitordata_macrofyten_voorbeeld.txt", file)},
  contentType = "text/plain"
)

output$referentiedata_planten_voorbeeld <- downloadHandler(
  filename = function(){paste("Referentiedata_macrofyten_voorbeeld.csv")},
  content <- function(file){file.copy("voorbeelddata/Referentiedata_macrofyten_voorbeeld.csv", file)},
  contentType = "text/csv"
)

output$monitordata_macrofauna_voorbeeld <- downloadHandler(
  filename = function(){paste("Monitordata_macrofauna_voorbeeld.txt")},
  content <- function(file){file.copy("voorbeelddata/Monitordata_macrofauna_voorbeeld.txt", file)},
  contentType = "text/plain"
)

output$referentiedata_macrofauna_voorbeeld_csv <- downloadHandler(
  filename = function(){paste("Referentiedata_macrofauna_voorbeeld.csv")},
  content <- function(file){file.copy("voorbeelddata/Referentiedata_macrofauna_voorbeeld.csv", file)},
  contentType = "text/csv"
)

output$referentiedata_macrofauna_voorbeeld_txt <- downloadHandler(
  filename = function(){paste("Referentiedata_macrofauna_voorbeeld.txt")},
  content <- function(file){file.copy("voorbeelddata/Referentiedata_macrofauna_voorbeeld.txt", file)},
  contentType = "text/plain"
)

output$monitordata_vissen_voorbeeld <- downloadHandler(
  filename = function(){paste("Monitordata_vissen_voorbeeld.txt")},
  content <- function(file){file.copy("voorbeelddata/Monitordata_vissen_voorbeeld.txt", file)},
  contentType = "text/plain"
)

output$referentiedata_vissen_voorbeeld <- downloadHandler(
  filename = function(){paste("Referentiedata_vissen_voorbeeld.csv")},
  content <- function(file){file.copy("voorbeelddata/Referentiedata_vissen_voorbeeld.csv", file)},
  contentType = "text/csv"
)

##== download habitat- en milieupreferenties ====
output$soortgroep_diatomeeen_habitatpref <- downloadHandler(
  filename = function(){paste("Van Dam 2010 Dataset milieupreferenties Diatomeeen.xlsx")},
  content <- function(file){file.copy("documentatie/Van Dam 2010 Dataset milieupreferenties Diatomeeen.xlsx", file)},
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$soortgroep_macrofauna_habitatpref <- downloadHandler(
  filename = function(){paste("Verberk Dataset macrofauna milieupreferenties wew_themanummer23.xlsx")},
  content <- function(file){file.copy("documentatie/Verberk Dataset macrofauna milieupreferenties wew_themanummer23.xlsx", file)},
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$soortgroep_macrofyten_habitatpref <- downloadHandler(
  filename = function(){paste("Riegman 2008 Milieupreferenties water- en oeverplanten.xlsx")},
  content <- function(file){file.copy("documentatie/Riegman 2008 Milieupreferenties water- en oeverplanten.xlsx", file)},
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$soortgroep_vissen_habitatpref <- downloadHandler(
  filename = function(){paste("Buijse 2016 Dataset habitat- en milieupreferenties vissen.xlsx")},
  content <- function(file){file.copy("documentatie/Buijse 2016 Dataset habitat- en milieupreferenties vissen.xlsx", file)},
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

}

shinyApp(ui, server)
