library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(strong(h1("Prace dyplomowe na Wydziale Fizyki UW!")) 
             ),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(h3("Instrukcja" ),
                 p("Wybierz kierunek, dla którego chcesz zobaczyć wykres"),
                 br(),
                 selectInput("select", label = h3("Wybierz tu"), 
                             choices = list("Fizyka" = "fiz", "Astronomia" = "ast",
                                            "Inzynieria nanostruktur" = "in",  "Zastosowania fizyki w biologii i w medycynie"="zfbm",
                             "Wszystkie"="dane"),
                             selected = "dane")),
                 
    
    # Show a plot of the generated distribution
    mainPanel(strong(h2("Liczba prac w zależności od płci w latach 2010-2015",align="center")),
              textOutput("text1"),
              plotOutput("myPlot")
                           
                           )
  )
))