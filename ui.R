#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(plotly)
library(shinythemes)



# Define UI for application that draws a histogram
shinyUI( navbarPage(strong("SOM APLICADO AL FÚTBOL"), theme=shinytheme("united"), collapsible = F,
                      
                    
                                        tabPanel("Introducción",
                                                 fluidPage(
                                                 fluidRow(p(h2("Introducción")), hr(),
                                                          p("En este trabajo intentaremos agrupar jugadores de las cinco principales ligas del fútbol 
                                                            europeo aplicando un mapa autoorganizado (SOM) a datos de la temporada 2019/2020."),
                                                          p("Un esquema de lo que se ha hecho puede ser el siguiente:"),
                                                          tags$head(
                                                            tags$style(HTML("hr {border-top: 1px solid #ffa533;}")))
                                                 ),
                                                 fluidRow(wellPanel(tags$div(
                                                   tags$ul(
                                                     tags$li(h3("SOM y Clustering"))
                                                   ),
                                                   p("El objetivo es ver si con una serie de datos estadísticos pueden agruparse
                                                                              los jugadores según su similitud en el juego."),
                                                   p("Usando diferentes formas de representación se analizan los resultados obtenidos.")
                                                   ,
                                                   tags$ul(
                                                     tags$li(h3("PCA"))
                                                   ),
                                                   p("Se estudia el uso de esta técnica para cuando se usan muchas variables. Se comparan los resultados de la aplicación
                                                                              del SOM a datos 'crudos', y a datos reducidos mediante el PCA, en base a una tolerancia."),
                                                   tags$ul(
                                                     tags$li(h3("DATOS"))
                                                   ),
                                                   p('Los datos se han descargado de la página'),
                                                   p(h4(a("fbref.com", href="https://fbref.com/en/")))
                                                   
                                                 )
                                                 )),
                                                 fluidRow(h4("Autores:"),
                                                          wellPanel(tags$div(tags$ul(
                                                            tags$li("Nofre Sanmartín Vich"),
                                                            tags$li("Llorenç Sancho Barrios"),
                                                            tags$li("Carlos Roger De La Resurrección")
                                                            )))
                                                   
                                                 )
                                                 )
                                                 
                                                 
                                                 ),
                                        navbarMenu("Presentación de datos",
                                                   tabPanel("Pase",dataTableOutput('tabla1')),
                                                   tabPanel("Tiros",dataTableOutput('tabla2')),
                                                   tabPanel("Acciones defensivas",dataTableOutput('tabla3')),
                                                   tabPanel("Posesión",dataTableOutput('tabla4'))
                                                 
                                                 ),
                                        tabPanel("Passing",
                                                 tabsetPanel(tabPanel("SOM",
                                                                       fluidRow(width=12,
                                                                               column(4, wellPanel(sliderInput("it","Número iteraciones",500,1500,1000),
                                                                                      sliderInput("grid","Dimensión del mallado",2,7,c(4,5))
                                                                                      )),
                                                                               column(4, plotOutput("grafSOM1")
                                                                                      ),
                                                                               column(4,plotOutput("grafSOM2")
                                                                                      )
                                                                               ),
                                                                       fluidRow(width=12, 
                                                                                column(4),
                                                                                column(8,plotOutput("grafdendo"))
                                                                                
                                                                                )
                                                                       ),
                                                            tabPanel("Cluster",
                                                                     fluidRow(width=12,
                                                                              column(4, wellPanel(uiOutput("sliderNumClus")
                                                                                      )),
                                                                              column(8, plotOutput("grafdendo2")
                                                                                      )
                                                                              ),
                                                                       fluidRow(width=12,
                                                                                column(4
                                                                                      ),
                                                                                column(4, plotOutput("grafcluster1")
                                                                                      ),
                                                                                column(4, plotOutput("grafcluster2")
                                                                                       )
                                                                                
                                                                                ),
                                                                       fluidRow(width=12, hr(),
                                                                                column(4, wellPanel(uiOutput("desplegableGrupos")
                                                                                      )),
                                                                                column(7,  dataTableOutput("tablaCluster")
                                                                                      )
                                                                                
                                                                                ),
                                                                       fluidRow(width=12, hr(),
                                                                                column(4, wellPanel(selectInput("queCol", "Que columna quieres visualizar",
                                                                                                      choices=c('Acierto total'=1, 'Acierto pase corto'=2, 'Acierto pase medio'=3, 'Acierto pase largo'=4,
                                                                                                                'Asistencias esperadas'=5, 'Pases intentados total'=6, 'Distancia pase'=7, 'Distancia vertical'=8,
                                                                                                                'Pases intentados cortos'=9, 'Pases intentados medios'=10, 'Pases intentados largos'=11,
                                                                                                                'Asistencias'=12, 'Pases clave'=13, 'Pases último tercio'=14, 'Pases al área'=15, 'Centros al área'=16,
                                                                                                                'Pases progresivos'=17))
                                                                                )),
                                                                                column(8,  plotOutput("boxWiskie")
                                                                                )
                                                                                
                                                                                
                                                                       )
                                                                     ),
                                                            tabPanel("SOM selectivo",
                                                                     wellPanel(fluidRow(width=12,
                                                                              column(4, uiOutput("selectGrup")
                                                                                     ),
                                                                              column(4, sliderInput("itRed","Número iteraciones",500,1500,1000)
                                                                                     ),
                                                                              column(4, sliderInput("gridRed","Dimensión del mallado",2,7,c(2,3))
                                                                                    )
                                                                              )),
                                                                     fluidRow(width=12,hr(),
                                                                              column(6, plotOutput("grafSOMRed1")
                                                                                     ),
                                                                              column(6, plotOutput("grafSOMRed2")
                                                                                     )
                                                                              ),
                                                                     fluidRow(width = 12, hr(),
                                                                              column(4, wellPanel(uiOutput("sliderNumClusRed")
                                                                                     )),
                                                                              column(8, plotOutput("grafdendoRed2")
                                                                                     )
                                                                              ),
                                                                     fluidRow(width=12,
                                                                              column(4
                                                                                    ),
                                                                              column(4, plotOutput("grafclusterRed1")
                                                                                    ),
                                                                              column(4, plotOutput("grafclusterRed2")
                                                                                    )
                                                                              
                                                                              ),
                                                                     fluidRow(width=12,hr(),
                                                                              column(4, wellPanel(uiOutput("desplegableGruposRed")
                                                                                    )),
                                                                              column(7,  dataTableOutput("tablaClusterRed")
                                                                                    )
                                                                              ),
                                                                     fluidRow(width=12, hr(),
                                                                              column(4, wellPanel(selectInput("queColRed", "Que columna quieres visualizar",
                                                                                                    choices=c('Acierto total'=1, 'Acierto pase corto'=2, 'Acierto pase medio'=3, 'Acierto pase largo'=4,
                                                                                                              'Asistencias esperadas'=5, 'Pases intentados total'=6, 'Distancia pase'=7, 'Distancia vertical'=8,
                                                                                                              'Pases intentados cortos'=9, 'Pases intentados medios'=10, 'Pases intentados largos'=11,
                                                                                                              'Asistencias'=12, 'Pases clave'=13, 'Pases último tercio'=14, 'Pases al área'=15, 'Centros al área'=16,
                                                                                                              'Pases progresivos'=17))
                                                                                    )),
                                                                              column(8,  plotOutput("boxWiskieRed")
                                                                                    )
                                                                              )
                                                                     
                                                                     
                                                                     
                                                              
                                                            ))
                                                              ),
                                        tabPanel("Total",
                                                 tabsetPanel(tabPanel("SOM",
                                                                       fluidRow(width=12,
                                                                                column(4, wellPanel(sliderInput("itTotal","Número iteraciones",500,1500,1000),
                                                                                                  sliderInput("gridTotal","Dimensión del mallado",2,7,c(4,5))
                                                                                       )),
                                                                                column(4, plotOutput("grafSOMTotal1")
                                                                                       ),
                                                                                column(4,plotOutput("grafSOMTotal2")
                                                                                       )
                                                                               ),
                                                                      fluidRow(width=12, 
                                                                                column(4
                                                                                       ),
                                                                                column(8,plotOutput("grafdendoTotal")
                                                                                       )
                                                                       
                                                                              )
                                                                      ),
                                                              tabPanel("Cluster",
                                                                       fluidRow(width=12, 
                                                                                column(4, wellPanel(uiOutput("sliderNumClusTotal")
                                                                                )),
                                                                                column(8, plotOutput("grafdendoTotal2")
                                                                                )
                                                                       ),
                                                                       fluidRow(width=12,
                                                                                column(4
                                                                                ),
                                                                                column(4, plotOutput("grafclusterTotal1")
                                                                                ),
                                                                                column(4, plotOutput("grafclusterTotal2")
                                                                                )
                                                                                
                                                                       ),
                                                                       fluidRow(width=12,hr(),
                                                                                column(4, wellPanel(uiOutput("desplegableGruposTotal")
                                                                                )),
                                                                                column(7,  dataTableOutput("tablaClusterTotal")
                                                                                )
                                                                                
                                                                       ),
                                                                       fluidRow(width=12,hr(),
                                                                                column(4, wellPanel(uiOutput("selectorColumnaTotal")
                                                                                )),
                                                                                column(8, plotOutput("grafboxWiskieTotal")
                                                                                )
                                                                                
                                                                       )
                                                              )
                                                              )
                                                ),
                                        tabPanel("PCA",
                                                 tabsetPanel(tabPanel("Precisión",
                                                                      fluidRow(width=12,
                                                                               column(4, wellPanel(numericInput("tol", "tolerancia", 0.9, min = 0, max = 1, step = 0.01)
                                                                                      )),
                                                                               column(8, plotOutput("varAcum")
                                                                                      )
                                                                               
                                                                      ),
                                                                      fluidRow(width=12,hr(),
                                                                               column(4, wellPanel(uiOutput("selectorCont")
                                                                                      )),
                                                                               column(8, plotOutput("grafCont"))
                                                                               )
                                                                      
                                                   
                                                                      ),
                                                             tabPanel("SOM",
                                                                      fluidRow(width=12,
                                                                               column(4, wellPanel(sliderInput("itPCA","Número iteraciones",500,1500,1000),
                                                                                      sliderInput("gridPCA","DimensiÃ³n del mallado",2,7,c(4,5))
                                                                               )),
                                                                               column(4, plotOutput("grafSOMPCA1")
                                                                               ),
                                                                               column(4,plotOutput("grafSOMPCA2")
                                                                               )
                                                                      ),
                                                                      fluidRow(width=12, 
                                                                               column(4
                                                                               ),
                                                                               column(8,plotOutput("grafdendoPCA")
                                                                               )
                                                                              )
                                                                     ),
                                                             tabPanel("Cluster",
                                                                      fluidRow(width=12,
                                                                               column(4, wellPanel(uiOutput("sliderNumClusPCA")
                                                                               )),
                                                                               column(8, plotOutput("grafdendoPCA2")
                                                                               )
                                                                      ),
                                                                      fluidRow(width=12,
                                                                               column(4
                                                                               ),
                                                                               column(4, plotOutput("grafclusterPCA1")
                                                                               ),
                                                                               column(4, plotOutput("grafclusterPCA2")
                                                                               )
                                                                               
                                                                      ),
                                                                      fluidRow(width=12,hr(),
                                                                               column(4, wellPanel(uiOutput("desplegableGruposPCA")
                                                                               )),
                                                                               column(7,  dataTableOutput("tablaClusterPCA")
                                                                               )
                                                                               
                                                                      ),
                                                                      fluidRow(width=12,hr(),
                                                                               column(4, wellPanel(uiOutput("selectorColumnaPCA")
                                                                                      )),
                                                                               column(7, plotOutput("grafboxWiskiePCA")
                                                                                      )
                                                                               
                                                                               )
                                                                      ),
                                                             tabPanel("Comparativa",
                                                                    fluidRow(width = 12,
                                                                             column(4, wellPanel(selectInput("tipoGrupo", "Sobre qué grupos quieres elegir", choices = c("Total", "PCA")),
                                                                                    uiOutput("selComp")
                                                                                    )),
                                                                             column(7, dataTableOutput("tablaComp"),
                                                                                    hr(),
                                                                                    tableOutput("tablaComp2"))
                                                                             
                                                                             
                                                                             )
                                                                      
                                                                      
                                                                      
                                                                      )
                                                                  
                                                           
                                                 
                                                          )
                                                    ),
                                        tabPanel("Puzzle",
                                                 tabsetPanel(
                                                   tabPanel("SOM",
                                                            wellPanel(fluidRow(width = 12,
                                                                               column(4,checkboxGroupInput("queDat","Con qué datos quieres trabajar", 
                                                                                                  choices = c("Pase" = 1, "Tiro" = 2, "Acciones defensivas" = 3, "PosesiÃ³n" = 4),
                                                                                                              selected = 1)),
                                                                               column(4,sliderInput("itPuzzle","Número iteraciones",500,1500,1000)),
                                                                               column(4,sliderInput("gridPuzzle","Dimensión del mallado",2,7,c(4,5)))
                                                                     
                                                           
                                                                    
                                                                     
                                                                     
                                                                     )),
                                                            fluidRow(width = 12, hr(),
                                                                     column(6, plotOutput("grafSOMPuzzle1")
                                                                     ),
                                                                     column(6,plotOutput("grafSOMPuzzle2")
                                                                     )
                                                                     ),
                                                            fluidRow(width=12, 
                                                                     column(2
                                                                     ),
                                                                     column(8,plotOutput("grafdendoPuzzle")
                                                                     )
                                                                     )
                                                            
                                                            
                                                            
                                                            
                                                            ),
                                                   tabPanel("Cluster",
                                                            fluidRow(width=12,
                                                                     column(4, wellPanel(uiOutput("sliderNumClusPuzzle")
                                                                     )),
                                                                     column(8, plotOutput("grafdendoPuzzle2")
                                                                     )
                                                            ),
                                                            fluidRow(width=12,
                                                                     column(4
                                                                     ),
                                                                     column(4, plotOutput("grafclusterPuzzle1")
                                                                     ),
                                                                     column(4, plotOutput("grafclusterPuzzle2")
                                                                     )
                                                                     
                                                            ),
                                                            fluidRow(width=12,hr(),
                                                                     column(4, wellPanel(uiOutput("desplegableGruposPuzzle")
                                                                     )),
                                                                     column(7,  dataTableOutput("tablaClusterPuzzle")
                                                                     )
                                                                     
                                                            ),
                                                            fluidRow(width=12,hr(),
                                                                     column(4, wellPanel(uiOutput("selectorColumnaPuzzle")
                                                                     )),
                                                                     column(7, plotOutput("grafboxWiskiePuzzle")
                                                                     )
                                                                     
                                                            )
                                                   )
                                                   
                                                   
                                                 
                                                   
                                                   
                                                   
                                                   )
                                                 )
                    
                                                   
                                      
    )
    
    )

