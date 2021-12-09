library(shiny)
library(move)
library(ggplot2)
library(shinyWidgets)
library(colourpicker)
library(htmltools)

## ToDo?: 
# Improve the default settings of the saved plot!
# make zoom-in tool
# give possibility to select/unselect all individuals with one click
# give option to only display one individual, ie, when another is selected, the previous one is unselected
# if variable is factor, give color palette options?
# make multipanel with aspect ratio==1, look into cowplot pkg


shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Plot track(s) colored by attribute"),
    fluidRow(
      column(3,uiOutput(ns('uiAttributeL'))),
      column(3,selectInput(ns("panels"),"Choose display mode",choices=c("Single panel","Multipanel"), selected="Single panel",multiple=F)),
      column(2,colourInput(ns("colmin"), "Select colour: low", "blue")),
      column(2,colourInput(ns("colmid"), "mid", "yellow")),
      column(2,colourInput(ns("colmax"), "high", "red"))),
    uiOutput(ns('uiIndivL')),
    span(textOutput(ns("warningtext")),
    plotOutput(ns("plot")), style="color:red"),
    downloadButton(ns('savePlot'), 'Save Plot')
  )
}

shinyModuleConfiguration <- function(id, input) {
  ns <- NS(id)
  
  configuration <- list()
  
  configuration
}

shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  current <- reactiveVal(data)
  output$uiAttributeL <- renderUI({
    selectInput(ns("attributeL"), "Select attribute", choices=colnames(data@data))})
    
  output$uiIndivL <- renderUI({
    # checkboxGroupInput(ns("indivL"), "Select individuals", choices=namesIndiv(data), selected=namesIndiv(data)[1], inline=TRUE)
    checkboxGroupButtons(ns("indivL"), "Select individuals",size="sm", choices=namesIndiv(data), selected=namesIndiv(data)[1],status="default",checkIcon = list(
      # yes = icon("check-square"), no = icon("square-o")))
      yes = icon("ok",lib = "glyphicon"))) #, no = icon("remove",lib = "glyphicon")
  })
  
  output$plot <- renderPlot({
    mDF <- data.frame(long=coordinates(data)[,1],lat=coordinates(data)[,2],attribute=data@data[,input$attributeL], indiv=trackId(data))
    mDF <- mDF[mDF$indiv %in% c(input$indivL),]
    if(input$panels=="Single panel"){
      output$warningtext <- NULL
      if(is.numeric(mDF[, "attribute"])){ 
        minattr <- min(mDF[, "attribute"],na.rm=T)
        maxattr <- max(mDF[, "attribute"],na.rm=T)
        mpt <- (minattr+maxattr)/2
        ggplot(mDF) + geom_path(aes(x=long, y=lat, colour=attribute, group=indiv))+
          scale_colour_gradient2(low=input$colmin, mid=input$colmid, high=input$colmax, midpoint=mpt, name=input$attributeL
                                 # , breaks=round(seq(minattr,maxattr,length.out=3),2),labels=round(seq(minattr,maxattr,length.out=3),2)
          )+
          coord_fixed()+
          labs(x ="Longitude", y = "Latitude")+ #title=input$attributeL,
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
      }else{ 
        ggplot(mDF) + geom_path(aes(x=long, y=lat, colour=attribute, group=indiv))+
          scale_color_discrete(name=input$attributeL)+
          coord_fixed()+
          labs(x ="Longitude", y = "Latitude")+ #title=input$attributeL,
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
      }
    }else if(input$panels=="Multipanel"){
      output$warningtext <- renderText({"WARNING: Aspect ratio of plots is distorted and not 1/1"})
      if(nrow(mDF)==0){ ## if plot is empty
        ggplot(mDF)+labs( x ="Longitude", y = "Latitude")+ #title=input$attributeL,
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
      }else{
        if(is.numeric(mDF[, "attribute"])){ 
          minattr <- min(mDF[, "attribute"],na.rm=T)
          maxattr <- max(mDF[, "attribute"],na.rm=T)
          mpt <- (minattr+maxattr)/2
          ggplot(mDF) + geom_path(aes(x=long, y=lat, colour=attribute, group=indiv))+
            facet_wrap(~indiv, scales="free")+
            scale_colour_gradient2(low=input$colmin, mid=input$colmid, high=input$colmax, midpoint=mpt, name=input$attributeL
                                   # , breaks=round(seq(minattr,maxattr,length.out=3),2),labels=round(seq(minattr,maxattr,length.out=3),2)
            )+
            # coord_fixed()+
            labs( x ="Longitude", y = "Latitude")+ #title=input$attributeL,
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
        }else{ 
          ggplot(mDF) + geom_path(aes(x=long, y=lat, colour=attribute, group=indiv))+
            facet_wrap(~indiv, scales="free")+
            scale_color_discrete(name=input$attributeL)+
            # coord_fixed()+
            labs( x ="Longitude", y = "Latitude")+ #title=input$attributeL,
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
        }
      }
    }
  })     

  ### save plot ###
  output$savePlot <- downloadHandler(
    filename = "ColorSegmentsByAttributePlot.png",
    content = function(file) {
      ggsave(file, device = "png")
    }
  )
  return(reactive({ current() }))
}

