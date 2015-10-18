library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel(h1("Wykres mozaikowy kierunku studiów w zależnosci od płci")),
  
  sidebarPanel(
    selectInput("year", "Rok:",
                list("Ogółem" = 0, 
                     "2009" = 2009, "2010" = 2010, "2011" = 2011, 
                     "2012" = 2012, "2013" = 2013, "2014" = 2014,
                     "2015" = 2015))
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h2(textOutput("caption")),
    
    plotOutput("ygPlot")
  )
))