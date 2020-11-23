#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




library(shiny)
library(DT)
library(kohonen)
library(plotly)
library(ggplot2)
library(factoextra)

set.seed(999)

source(file.path("Scripts" ,"datCompleto.R"), local = T)
source(file.path("Scripts" ,"scriptTotalyPCA.R"), local = T)

degradadoazul <- function(n){
  return(rgb(0,0.4,1,alpha=seq(0,1,1/n)))}




shinyServer(function(input, output) {

    
    ###########################################
    #Dades
  
  
 
  
    output$tabla1 <- renderDataTable(datCompleto, rownames = FALSE, extensions = 'Buttons', filter = 'bottom' )
    
    output$tabla2 <- renderDataTable(datShooting, rownames = FALSE, extensions = 'Buttons', filter = 'bottom' )
    
    output$tabla3 <- renderDataTable(datDefensive, rownames = FALSE, extensions = 'Buttons', filter = 'bottom' )
    
    output$tabla4 <- renderDataTable(datPossesion, rownames = FALSE, extensions = 'Buttons', filter = 'bottom' )
    
    
    ##########################################
    
    
    ########################################
    carte <- reactive({
        carte <- som(datSOMscaled, grid=somgrid(input$grid[1],input$grid[2], "hexagonal"), input$it)
        carte
    })

    output$grafSOM1 <- renderPlot({

        grafSOM1 <-plot(carte(),shape='straight')
    })
    
    output$grafSOM2 <- renderPlot({
      
      grafSOM2 <-plot(carte(), type = "dist.neighbours", shape = "straight")
    })
    
    output$grafdendo <- renderPlot({
      
      grafdendo <-plot(cah(), xlab="", ylab="", sub="")
    })
    
    ########################################################
    
    nb <- reactive({
        
        table(carte()$unit.classif)
        
    })
    
    
    dc <- reactive({
        
        dist(getCodes(carte()))
        
    })

    cah <- reactive({
       cah <-  hclust(dc(), method = "ward.D2", members = nb())
    })
    
    output$grafdendo2 <- renderPlot({
      
      graf <- plot(cah(),ylab="",xlab = "", sub="")
      rect.hclust(cah(), input$nclus)
      graf
    })
    
    numMaxClus <- reactive({
      numMaxClus<-15
      if(input$grid[1]*input$grid[2]-1 < numMaxClus)
        numMaxClus<-input$grid[1]*input$grid[2]-1
      numMaxClus
    })
    
    output$sliderNumClus <- renderUI({
      sliderInput("nclus","Número clusters",2,numMaxClus(),9, step = 1)
    })
   
    
    
    grupos <- reactive({
        grupos <- cutree(cah(), k=input$nclus)
    })
    
    
    datCluster <- reactive({
      datCluster <- datCompleto
      datCluster <- select(datCluster, c("Player","team", "Pos"))
      datCluster$grup <- grupos()[carte()$unit.classif]
      datCluster
    })
    
    
    
    colfijo <- c("steelblue1", "sienna1", "yellowgreen", "red", "yellow", "purple", "grey", "brown", "darkslategray1", "indianred1", "wheat1", "tan1", "forestgreen","maroon1","lightsteelblue4")
    
    
    output$legend <- renderText({
      
      paste("Grupo", i, sep=' ')
    })
    
    output$grafcluster1 <- renderPlot({
        plot(carte(), type = "mapping", bgcol = colfijo[1:input$nclus][grupos()], shape = "straight")
        add.cluster.boundaries(carte(), clustering = grupos())
        
    })
    
    output$grafcluster2 <- renderPlot({
      plot(carte(), type = "dist.neighbours", shape = "straight")
      add.cluster.boundaries(carte(), clustering = grupos())
      
    })

    
    output$tablaCluster <- renderDataTable({
        subset(datCluster(), grup==as.numeric(input$queGrupo))
       
            })

    output$desplegableGrupos <- renderUI({

        selectInput("queGrupo", "Que grupo quieres visualizar",
                    choices=1:input$nclus)
    })
    
   
    output$plotBox <- renderDataTable({
      subset(datCluster(), grup==as.numeric(input$queGrupo))
      
    })
    
    
    
    lista <- reactive({
      
      datPruebaSOM <-datSOM
      datPruebaSOM$grup <- grupos()[carte()$unit.classif]
      
      lista <-list()
      lst <- list()
      
      for(j in 1:ncol(datSOM)){
        for (i in 1:input$nclus){
          lst[[i]]<- subset(datPruebaSOM,grup==i)[,j]
        }
        lista[[j]]<-lst
      }
      
      lista
      
    })
    
    
    
    dibuix <- reactive({
      as.numeric(input$queCol)
    })
    

    
    output$boxWiskie <- renderPlot({
      
      par(mfrow=c(1,2))
      boxplot(lista()[[dibuix()]], main = colnames(datSOM)[dibuix()],
              xlab='groups', col='orange', border = 'brown')
      
      plot(carte(), type = "property", property = getCodes(carte())[,dibuix()], palette = degradadoazul ,main = colnames(datSOM)[dibuix()], shape = "straight")
      add.cluster.boundaries(carte(), clustering = grupos())
    })
    ###############################################
    #SoM d'alguns grups
    
    
    
    
    output$selectGrup <- renderUI({
      checkboxGroupInput("selGrup", "Elige que grupos te gustaria separar", choices=1:input$nclus, selected=1)
    })
      
    
    datRed <- reactive({
      datAux <- datSOM
      datAux$grup <- grupos()[carte()$unit.classif]
      datAux <- subset(datAux, grup %in% input$selGrup)
      datAux<- select(datAux, -c("grup"))
      datAux <- scale(datAux, center=T, scale=T)
      datAux
      
    })
    
    
    datFrameRed <- reactive({
      datAux <- datSOM
      datAux$grup <- grupos()[carte()$unit.classif]
      datAux <- subset(datAux, grup %in% input$selGrup)
      datAux<- select(datAux, -c("grup"))
      datAux
      
    })
    
    
    
    
    
    carteRed <- reactive({
      som(datRed(), grid=somgrid(input$gridRed[1],input$gridRed[2], "hexagonal"), input$itRed)
    })

    output$grafSOMRed1 <- renderPlot({

      plot(carteRed(),shape='straight')
    })

    output$grafSOMRed2 <- renderPlot({

      plot(carteRed(), type = "dist.neighbours", shape = "straight")
    })
    
   
   
    
    nbRed <- reactive({

      table(carteRed()$unit.classif)

    })


    dcRed <- reactive({

      dist(getCodes(carteRed()))

    })

    cahRed <- reactive({
      hclust(dcRed(), method = "ward.D2", members = nbRed())
    })

    output$grafdendoRed2 <- renderPlot({

      plot(cahRed(),ylab="",xlab = "", sub="")
      rect.hclust(cahRed(), input$nclusRed)

    })

    numMaxClusRed <- reactive({
      numMaxClus<-15
      if(input$gridRed[1]*input$gridRed[2]-1 < numMaxClus)
        numMaxClus<-input$gridRed[1]*input$gridRed[2]-1
      numMaxClus
    })

    output$sliderNumClusRed <- renderUI({
      sliderInput("nclusRed","Número clusters",2,numMaxClusRed(),3, step = 1)
    })



    gruposRed <- reactive({
        cutree(cahRed(), k=input$nclusRed)
    })


    



    # colfijo <- c("steelblue1", "sienna1", "yellowgreen", "red", "yellow", "purple", "grey", "brown", "darkslategray1", "indianred1", "wheat1", "tan1", "forestgreen","maroon1","lightsteelblue4")


    # output$legend <- renderText({
    # 
    #   paste("Grupo", i, sep=' ')
    # })

    output$grafclusterRed1 <- renderPlot({
      plot(carteRed(), type = "mapping", bgcol = colfijo[1:input$nclusRed][gruposRed()], shape = "straight")
      add.cluster.boundaries(carteRed(), clustering = gruposRed())

    })

    output$grafclusterRed2 <- renderPlot({
      plot(carteRed(), type = "dist.neighbours", shape = "straight")
      add.cluster.boundaries(carteRed(), clustering = gruposRed())

    })

    
    datClusterRed <- reactive({
      datCluster <- datCluster()
      datCluster <- subset(datCluster, grup %in% input$selGrup)
      datCluster$grupNou <- gruposRed()[carteRed()$unit.classif]
      datCluster
    })

    output$tablaClusterRed <- renderDataTable({
      subset(datClusterRed(), grupNou==as.numeric(input$queGrupoRed))

    })

    output$desplegableGruposRed <- renderUI({

      selectInput("queGrupoRed", "Que grupo quieres visualizar",
                  choices=1:input$nclusRed)
    })





    listaRed <- reactive({

      datPruebaSOM <-datFrameRed()
      datPruebaSOM$grup <- gruposRed()[carteRed()$unit.classif]

      lista <-list()
      lst <- list()

      for(j in 1:ncol(datFrameRed())){
        for (i in 1:input$nclusRed){
          lst[[i]]<- subset(datPruebaSOM,grup==i)[,j]
        }
        lista[[j]]<-lst
      }

      lista

    })



    dibuixRed <- reactive({
      as.numeric(input$queColRed)
    })



    output$boxWiskieRed <- renderPlot({

      par(mfrow=c(1,2))
      boxplot(listaRed()[[dibuixRed()]], main = colnames(datFrameRed())[dibuixRed()],
              xlab='groups', col='orange', border = 'brown')

      plot(carteRed(), type = "property", property = getCodes(carteRed())[,dibuixRed()], palette = degradadoazul ,main = colnames(datFrameRed())[dibuixRed()], shape = "straight")
      add.cluster.boundaries(carteRed(), clustering = gruposRed())
    })

    
    
    
    
    
    
 #######################################################################3
    ############################################################################################################
    #Som Total
    
    
    carteTotal <- reactive({
      som(datSOMTotalscaled, grid=somgrid(input$gridTotal[1],input$gridTotal[2], "hexagonal"), input$itTotal)
      
    })
    
    output$grafSOMTotal1 <- renderPlot({
      
        plot(carteTotal(),shape='straight')
    })
    
    output$grafSOMTotal2 <- renderPlot({
        plot(carteTotal(), type = "dist.neighbours", shape = "straight")
    })
    
    
    
    
    ##########################
    ########################################################
    
    nbTotal <- reactive({

      table(carteTotal()$unit.classif)

    })


    dcTotal <- reactive({

      dist(getCodes(carteTotal()))

    })

    cahTotal <- reactive({
      hclust(dcTotal(), method = "ward.D2", members = nbTotal())
    })

    output$grafdendoTotal <- renderPlot({

      plot(cahTotal(), xlab ="", ylab = "", sub = "")
    })

    numMaxClusTotal <- reactive({
      numMaxClus<-15
      if(input$gridTotal[1]*input$gridTotal[2]-1 < numMaxClus)
        numMaxClus<-input$gridTotal[1]*input$gridTotal[2]-1
      numMaxClus
    })

    output$sliderNumClusTotal <- renderUI({
      sliderInput("nclusTotal","Número clusters",2,numMaxClusTotal(),9, step = 1)
    })



    gruposTotal <- reactive({
      cutree(cahTotal(), k=input$nclusTotal)
    })
    
    output$grafdendoTotal2 <- renderPlot({
      
      graf <- plot(cahTotal(),ylab="",xlab = "", sub="")
      rect.hclust(cahTotal(), input$nclusTotal)
      graf
    })

    datClusterTotal <- reactive({
      datCluster <- datTotal
      datCluster <- select(datCluster, c("Player","team", "Pos"))
      datCluster$grup <- gruposTotal()[carteTotal()$unit.classif]
      datCluster
    })



    output$grafclusterTotal1 <- renderPlot({
      plot(carteTotal(), type = "mapping", bgcol = colfijo[1:input$nclusTotal][gruposTotal()], shape = "straight")
      add.cluster.boundaries(carteTotal(), clustering = gruposTotal())

    })

    output$grafclusterTotal2 <- renderPlot({
      plot(carteTotal(), type = "dist.neighbours", shape = "straight")
      add.cluster.boundaries(carteTotal(), clustering = gruposTotal())

    })


    output$tablaClusterTotal <- renderDataTable({
      subset(datClusterTotal(), grup==as.numeric(input$queGrupoTotal))

    })

    output$desplegableGruposTotal <- renderUI({

      selectInput("queGrupoTotal", "Que grupo quieres visualizar",
                  choices=1:input$nclusTotal)
    })


    output$plotBoxTotal <- renderDataTable({
      subset(datClusterTotal(), grup==as.numeric(input$queGrupoTotal))

    })



    listaTotal <- reactive({
      
      datPruebaSOM <-datSOMTotal
      datPruebaSOM$grup <- gruposTotal()[carteTotal()$unit.classif]
      
      lista <-list()
      lst <- list()
      
      for(j in 1:ncol(datSOMTotal)){
        for (i in 1:input$nclusTotal){
          lst[[i]]<- subset(datPruebaSOM,grup==i)[,j]
        }
        lista[[j]]<-lst
      }
      
      lista
      
    })
    
    
    output$selectorColumnaTotal <- renderUI({
      selectInput("queColTotal", "Que columna quieres visualizar",
                  choices=colnames(datSOMTotal))
    })
    
    
    
    dibuixTotal <- reactive({
      match(input$queColTotal, colnames(datSOMTotal))
    })
    
    
    
    
    output$grafboxWiskieTotal <- renderPlot({
      
      par(mfrow=c(1,2))
      boxplot(listaTotal()[[dibuixTotal()]], main = colnames(datSOMTotal)[dibuixTotal()],
              xlab='groups', col='orange', border = 'brown')
      
      plot(carteTotal(), type = "property", property = getCodes(carteTotal())[,dibuixTotal()], palette = degradadoazul ,main = colnames(datSOMTotal)[dibuixTotal()], shape = "straight")
      add.cluster.boundaries(carteTotal(), clustering = gruposTotal())
    })
    ###################################################
    
    ##################################################
    #PCA
    
    
    #######################33
    
    numdim <- reactive({
      ind <- ifelse(varCum >= input$tol, 1, 0)
      num <-match(1,ind)
    })
    
    output$varAcum <- renderPlot({
      barplot(varCum, space=0, col="coral",xlab="Dimensiones" ,ylab="Varianza relativa acumulada")
      #lines(0:length(varRel),input$tol*ones(c(length(varRel)+1,1)), col="blue")
      lines(0:length(varRel),matrix(input$tol,length(varRel)+1,1), col="blue")
      axis(1,at=numdim()-0.5,labels = as.character(numdim()))
    })
    
    
    output$selectorCont <- renderUI({
      selectInput("queDim", "De que dimension quieres ver la contribucion", choices = 1:numdim())
    })
    
    output$grafCont <- renderPlot({
      fviz_contrib(PCA, choice = "ind", axes=as.numeric(input$queDim), top=10)
    })
    ####################
    
    
    
    
    datPCA <- reactive({
      PCA$x[,1:numdim()]
    })
    
    
    
    
    
    cartePCA <- reactive({
      som(datPCA(), grid=somgrid(input$gridPCA[1],input$gridPCA[2], "hexagonal"), input$itPCA)
      
    })
    
    output$grafSOMPCA1 <- renderPlot({
      
      plot(cartePCA(),shape='straight')
    })
    
    output$grafSOMPCA2 <- renderPlot({
      plot(cartePCA(), type = "dist.neighbours", shape = "straight")
    })
    
    
    
    ########################################################
    
    nbPCA <- reactive({
      
      table(cartePCA()$unit.classif)
      
    })
    
    
    dcPCA <- reactive({
      
      dist(getCodes(cartePCA()))
      
    })
    
    cahPCA <- reactive({
      hclust(dcPCA(), method = "ward.D2", members = nbPCA())
    })
    
    output$grafdendoPCA <- renderPlot({
      
      plot(cahPCA(),ylab="",xlab = "", sub="")
    })
    
    numMaxClusPCA <- reactive({
      numMaxClus<-15
      if(input$gridPCA[1]*input$gridPCA[2]-1 < numMaxClus)
        numMaxClus<-input$gridPCA[1]*input$gridPCA[2]-1
      numMaxClus
    })
    
    output$sliderNumClusPCA <- renderUI({
      sliderInput("nclusPCA","Número clusters",2,numMaxClusPCA(),9, step = 1)
    })
    
    
    
    gruposPCA <- reactive({
      cutree(cahPCA(), k=input$nclusPCA)
    })
    
    output$grafdendoPCA2 <- renderPlot({
      
      graf <- plot(cahPCA(),ylab="",xlab = "", sub="")
      rect.hclust(cahPCA(), input$nclusPCA)
      graf
    })
    
    datClusterPCA <- reactive({
      datCluster <- datTotal
      datCluster <- select(datCluster, c("Player","team", "Pos"))
      datCluster$grup <- gruposPCA()[cartePCA()$unit.classif]
      datCluster
    })
    
    
    
    output$grafclusterPCA1 <- renderPlot({
      plot(cartePCA(), type = "mapping", bgcol = colfijo[1:input$nclusPCA][gruposPCA()], shape = "straight")
      add.cluster.boundaries(cartePCA(), clustering = gruposPCA())
      
    })
    
    output$grafclusterPCA2 <- renderPlot({
      plot(cartePCA(), type = "dist.neighbours", shape = "straight")
      add.cluster.boundaries(cartePCA(), clustering = gruposPCA())
      
    })
    
    
    output$tablaClusterPCA <- renderDataTable({
      subset(datClusterPCA(), grup==as.numeric(input$queGrupoPCA))
      
    })
    
    output$desplegableGruposPCA <- renderUI({
      
      selectInput("queGrupoPCA", "Que grupo quieres visualizar",
                  choices=1:input$nclusPCA)
    })
    
    
    output$plotBoxPCA <- renderDataTable({
      subset(datClusterPCA(), grup==as.numeric(input$queGrupoPCA))
      
    })
    
    
    listaPCA <- reactive({
      
      datPruebaSOM <-datSOMTotal
      datPruebaSOM$grup <- gruposPCA()[cartePCA()$unit.classif]
      
      lista <-list()
      lst <- list()
      
      for(j in 1:ncol(datSOMTotal)){
        for (i in 1:input$nclusPCA){
          lst[[i]]<- subset(datPruebaSOM,grup==i)[,j]
        }
        lista[[j]]<-lst
      }
      
      lista
      
    })
    
    
    output$selectorColumnaPCA <- renderUI({
      selectInput("queColPCA", "Que grupo quieres visualizar",
                  choices=colnames(datSOMTotal))
    })
    
    
    
    dibuixPCA <- reactive({
      match(input$queColPCA, colnames(datSOMTotal))
    })
    
    
    
    
    output$grafboxWiskiePCA <- renderPlot({
      
      boxplot(listaPCA()[[dibuixPCA()]], main = colnames(datSOMTotal)[dibuixPCA()],
              xlab='groups', col='orange', border = 'brown')
    })
    
    
    ###############################################
    #Comparativa                                  #
    ###############################################
    
    output$selComp <- renderUI({
      switch(input$tipoGrupo,
             "Total" = selectInput("quegrupComp", "Que grupo de Total quieres visualizar", choices= 1:input$nclusTotal),
             "PCA" = selectInput("quegrupComp", "Que grupo de PCA quieres visualizar", choices= 1:input$nclusPCA)


             )
      # selectInput("quegrupComp", "Que grupo de Total quieres visualizar", choices= 1:input$nclusTotal)

    })

    
    datClusterComp <- reactive({
      datCluster <- datTotal
      datCluster <- select(datCluster, c("Player","team", "Pos"))
      datCluster$grupTotal <- gruposTotal()[carteTotal()$unit.classif]
      datCluster$grupPCA <- gruposPCA()[cartePCA()$unit.classif]
      datCluster
    })
    
    
    
    
    
    output$tablaComp <- renderDataTable({
      switch(input$tipoGrupo,
             "Total" = subset(datClusterComp(), grupTotal == input$quegrupComp),
             "PCA" = subset(datClusterComp(), grupPCA == input$quegrupComp)
             
      )
      # subset(datClusterComp(), grupTotal == input$quegrupComp)
    })
    
    
    datComparatica <- reactive({
      
      switch (input$tipoGrupo,
        "Total" = {matriu <- matrix(0,  1 ,input$nclusTotal+1)
                for (i in 1:as.numeric(input$nclusTotal))
                {
                  aux<-subset(datClusterComp(), grupTotal == i)
                  grupoMax <- as.numeric(names(sort(summary(as.factor(aux$grupPCA)), decreasing=T)[1]))
                  matriu[i] <- sum(aux$grupPCA != grupoMax)
                }
                matriu[input$nclusTotal+1] = sum(matriu)
                matriu<-data.frame("Diferencia respecto grupos PCA", matriu)
                names(matriu)<-c("Grupos Total", 1:input$nclusTotal, "Global")
        },
        "PCA" = {matriu <- matrix(0,  1 ,input$nclusPCA+1)
                    for (i in 1:as.numeric(input$nclusPCA))
                    {
                      aux<-subset(datClusterComp(), grupPCA == i)
                      grupoMax <- as.numeric(names(sort(summary(as.factor(aux$grupTotal)), decreasing=T)[1]))
                      matriu[i] <- sum(aux$grupTotal != grupoMax)
                    }
                    matriu[input$nclusPCA+1] = sum(matriu)
                    matriu<-data.frame("Diferencia respecto grupos Total", matriu)
                    names(matriu)<-c("Grupos PCA", 1:input$nclusPCA, "Global")
                  }
      )
      
      # matriu <- matrix(0,  1 ,input$nclusTotal)
      # for (i in 1:as.numeric(input$nclusTotal))
      # {
      # aux<-subset(datClusterComp(), grupTotal == i)
      # grupoMax <- as.numeric(names(sort(summary(as.factor(aux$grupPCA)), decreasing=T)[1]))
      # matriu[i] <- sum(aux$grupPCA != grupoMax)
      # 
      # }

      matriu

    })
    
    output$tablaComp2 <- renderTable({
       datComparatica()
      })
    
    
    
    #######################################
    #Puzzle                               #
    #######################################
    
    datPuzzle <- reactive({
      dat <- data.frame(matrix(0, nrow(datTotal),1))
      if(1 %in% as.numeric(input$queDat))
        dat<- cbind(dat, select(datPassing2,-(1:5),  -c(ncol(datPassing2)-1, ncol(datPassing2))))
      
      if(2 %in% as.numeric(input$queDat))
        dat<- cbind(dat, select(datShooting,-(1:5),  -c(ncol(datShooting)-1, ncol(datShooting))))
      
      if(3 %in% as.numeric(input$queDat))
        dat<- cbind(dat, select(datDefensive,-(1:5),  -c(ncol(datDefensive)-1, ncol(datDefensive))))
      
      if(4 %in% as.numeric(input$queDat))
        dat<- cbind(dat, select(datPossesion,-(1:5),  -c(ncol(datPossesion)-1, ncol(datPossesion))))
      
      dat<-select(dat, -1)
      dat
    })
    
    ########
    
    datSOMPuzzle <- reactive({
      scale(datPuzzle())
    })
    
    cartePuzzle <- reactive({
      som(datSOMPuzzle(), grid=somgrid(input$gridPuzzle[1],input$gridPuzzle[2], "hexagonal"), input$itPuzzle)
      
    })
    
    output$grafSOMPuzzle1 <- renderPlot({
      
      plot(cartePuzzle(),shape='straight')
    })
    
    output$grafSOMPuzzle2 <- renderPlot({
      plot(cartePuzzle(), type = "dist.neighbours", shape = "straight")
    })
    
    
    ##################
    
    nbPuzzle <- reactive({
      
      table(cartePuzzle()$unit.classif)
      
    })
    
    
    dcPuzzle <- reactive({
      
      dist(getCodes(cartePuzzle()))
      
    })
    
    cahPuzzle <- reactive({
      hclust(dcPuzzle(), method = "ward.D2", members = nbPuzzle())
    })
    
    output$grafdendoPuzzle <- renderPlot({
      
      plot(cahPuzzle(),ylab="",xlab = "", sub="")
    })
    
    numMaxClusPuzzle <- reactive({
      numMaxClus<-15
      if(input$gridPuzzle[1]*input$gridPuzzle[2]-1 < numMaxClus)
        numMaxClus<-input$gridPuzzle[1]*input$gridPuzzle[2]-1
      numMaxClus
    })
    
    output$sliderNumClusPuzzle <- renderUI({
      sliderInput("nclusPuzzle","Número clusters",2,numMaxClusPuzzle(),9, step = 1)
    })
    
    
    
    gruposPuzzle <- reactive({
      cutree(cahPuzzle(), k=input$nclusPuzzle)
    })
    
    output$grafdendoPuzzle2 <- renderPlot({
      
      graf <- plot(cahPuzzle(),ylab="",xlab = "", sub="")
      rect.hclust(cahPuzzle(), input$nclusPuzzle)
      graf
    })
    
    datClusterPuzzle <- reactive({
      datCluster <- datTotal
      datCluster <- select(datCluster, c("Player","team", "Pos"))
      datCluster$grup <- gruposPuzzle()[cartePuzzle()$unit.classif]
      datCluster
    })
    
    
    
    output$grafclusterPuzzle1 <- renderPlot({
      plot(cartePuzzle(), type = "mapping", bgcol = colfijo[1:input$nclusPuzzle][gruposPuzzle()], shape = "straight")
      add.cluster.boundaries(cartePuzzle(), clustering = gruposPuzzle())
      
    })
    
    output$grafclusterPuzzle2 <- renderPlot({
      plot(cartePuzzle(), type = "dist.neighbours", shape = "straight")
      add.cluster.boundaries(cartePuzzle(), clustering = gruposPuzzle())
      
    })
    
    
    output$tablaClusterPuzzle <- renderDataTable({
      subset(datClusterPuzzle(), grup==as.numeric(input$queGrupoPuzzle))
      
    })
    
    output$desplegableGruposPuzzle <- renderUI({
      
      selectInput("queGrupoPuzzle", "Que grupo quieres visualizar",
                  choices=1:input$nclusPuzzle)
    })
    
    
    output$plotBoxPuzzle <- renderDataTable({
      subset(datClusterPuzzle(), grup==as.numeric(input$queGrupoPuzzle))
      
    })
    
    
    listaPuzzle <- reactive({
      
      datPruebaSOM <-datPuzzle()
      datPruebaSOM$grup <- gruposPuzzle()[cartePuzzle()$unit.classif]
      
      lista <-list()
      lst <- list()
      
      for(j in 1:ncol(datPuzzle())){
        for (i in 1:input$nclusPuzzle){
          lst[[i]]<- subset(datPruebaSOM,grup==i)[,j]
        }
        lista[[j]]<-lst
      }
      
      lista
      
    })
    
    
    output$selectorColumnaPuzzle <- renderUI({
      selectInput("queColPuzzle", "Que grupo quieres visualizar",
                  choices=colnames(datPuzzle()))
    })
    
    
    
    dibuixPuzzle <- reactive({
      match(input$queColPuzzle, colnames(datPuzzle()))
    })
    
    
    
    
    output$grafboxWiskiePuzzle <- renderPlot({
      par(mfrow=c(1,2))
      boxplot(listaPuzzle()[[dibuixPuzzle()]], main = colnames(datPuzzle())[dibuixPuzzle()],
              xlab='groups', col='orange', border = 'brown')
      
      plot(cartePuzzle(), type = "property", property = getCodes(cartePuzzle())[,dibuixPuzzle()], palette = degradadoazul ,main = colnames(datPuzzle())[dibuixPuzzle()], shape = "straight")
      add.cluster.boundaries(cartePuzzle(), clustering = gruposPuzzle())
      
    })
    
    
    
    
    
    
    
                                       
})
