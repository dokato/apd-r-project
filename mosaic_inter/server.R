library(shiny)
library(ggplot2)

load("../apd.Rda")
apddata$rok <- as.numeric(format(apddata$data,"%Y"))
apddata <- apddata[!is.na(apddata$rok),]

fnam <- vector(mode="list", length=4)
names(fnam) <- c("ast", "fiz", "in", "zfbm")
fnam[["ast"]] <- "Astronomia"
fnam[["fiz"]] <- "Fizyka"
fnam[["in"]] <- "Inż. Nanostruktur"
fnam[["zfbm"]] <- "Zastosowania fizyki w biologii i medycynie"

colors = c("#1ebaff","#f69d1f","#e60000","#5bb300","#851994")

shinyServer(function(input, output) {
  
  formulaText <- reactive(function() {
    if (input$year!=0){
      paste("Rok: ", input$year)
    }
    else{
      "Ogółem"
    }
  })
  
  output$caption <- renderText(function() {
    formulaText()
  })
    
  output$ygPlot <- renderPlot(function() {
    # check for the input variable
    if (input$year != 0) {
      plotdata <- apddata %>% filter(rok==input$year)
    }
    else{
      plotdata <- apddata;
    }
    tabyg <- table(plotdata$gender, plotdata$field)

    rownames(tabyg) <- c("Kobiety", "Mężczyźni")
    cn <- colnames(tabyg)
    for(i in 1:length(cn)){
      cn[i] <- fnam[[cn[i]]]
    }
    colnames(tabyg) <- cn
    p <- mosaicplot(tabyg, las=1, col=colors, main=NA)
  })
  
})