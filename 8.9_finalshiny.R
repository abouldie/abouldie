
library(shiny)
library(fpp3)
library(shinydashboard)
library(shinydashboardPlus)
library(fdaACF)
library(plotly)
library(shinyWidgets)
data("aus_arrivals")
autoplot(aus_arrivals)
install.packages("shinythemes")
library(shinythemes)
install.packages("formattable")
library(formattable)
data("aus_arrivals")
ausfitUK <- aus_arrivals %>% filter(Origin == "UK") %>%
  model(
    naive = NAIVE(Arrivals),
    snaive = SNAIVE(Arrivals),
    mean = MEAN(Arrivals),
    drift = RW(Arrivals~ drift()))
ausfitUKfc <- ausfitUK %>% forecast(h = "5 years")
ausfitUKfc %>% autoplot(aus_arrivals, level = NULL)+ labs(title = "UK->AUS 5 year forecast")

ausfitJapan <- aus_arrivals %>% filter(Origin == "Japan") %>%
  model(
    naive = NAIVE(Arrivals),
    snaive = SNAIVE(Arrivals),
    mean = MEAN(Arrivals),
    drift = RW(Arrivals~ drift()))
ausfitJapanfc <- ausfitJapan %>% forecast(h = "5 years")
ausfitJapanfc %>% autoplot(aus_arrivals, level = NULL)+ labs(title = "Japan->AUS 5 year forecast")

ausfitNZ <- aus_arrivals %>% filter(Origin == "NZ") %>%
  model(
    naive = NAIVE(Arrivals),
    snaive = SNAIVE(Arrivals),
    mean = MEAN(Arrivals),
    drift = RW(Arrivals~ drift()))
ausfitNZfc <- ausfitNZ %>% forecast(h = "5 years")
ausfitNZfc %>% autoplot(aus_arrivals, level = NULL)+labs(title = "NZ->AUS 5 year forecast")

ausfitUS <- aus_arrivals %>% filter(Origin == "US") %>%
  model(
    naive = NAIVE(Arrivals),
    snaive = SNAIVE(Arrivals),
    mean = MEAN(Arrivals),
    drift = RW(Arrivals~ drift()))
ausfitUSfc <- ausfitUS %>% forecast(h = "5 years")
ausfitUSfc %>% autoplot(aus_arrivals, level = NULL)+labs(title = "US->AUS 5 year forecast")


beet <- aus_arrivals %>%
  model(
    AAN = ETS(Arrivals ~ error("A") + trend("A") + season("N"))
  )
beet1 <- beet %>% forecast(h = "3 years") %>% autoplot(aus_arrivals, level = NULL)

root <- aus_arrivals %>%
  model(
    additive = ETS(Arrivals ~ error("A")+trend("A")+season("A")),
    multiplicative = ETS(Arrivals ~ error("M")+trend("A")+season("M"))
  )
rootfc <- root %>% forecast(h = "3 years")

foot <- aus_arrivals %>% filter(Origin == "NZ") %>%
  model(
    arima = ARIMA(Arrivals)
  )
foot2 <- foot %>% forecast(h = "5 years")
foot2 %>% autoplot(aus_arrivals, level = NULL)


hand <- aus_arrivals %>% filter(Origin == "UK") %>%
  model(
    arima = ARIMA(Arrivals)
  )
hand2 <- hand %>% forecast(h = "5 years")
hand2 %>% autoplot(aus_arrivals, level = NULL)


wrist <- aus_arrivals %>% filter(Origin == "Japan") %>%
  model(
    arima = ARIMA(Arrivals)
  )
wrist2 <- wrist %>% forecast(h = "5 years")
wrist2 %>% autoplot(aus_arrivals, level = NULL)


knee <- aus_arrivals %>% filter(Origin == "US") %>%
  model(
    arima = ARIMA(Arrivals)
  )
knee2 <- knee %>% forecast(h = "5 years")
knee2 %>% autoplot(aus_arrivals, level = NULL)



install.packages('rsconnect')
library(rsconnect)
# rsconnect::setAccountInfo(name='abouldie',
#                           token='9C4733FDD96F187743CFCF0AB10F2507',
#                           secret='<SECRET>')

ui <- dashboardPage(
  dashboardHeader(title = "Angelee's App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Instructions!", tabName = "ins"),
      menuItem("Time Series Plot", tabName = "tsplot"),
      menuItem("Choose a Plot", tabName = "choose"),
      menuItem("Interactive Plots", tabName = "myfeat"),
      menuItem("Simple Models", tabName = "fc"),
      menuItem("Exponential Smoothing", tabName = "smooth"),
      menuItem("Arima's", tabName = "arimatab"),
      menuItem("Plan a Trip", tabName = "trip")
    )
  ),
  dashboardBody( shinytheme("journal"),
    tabItems(
      tabItem(tabName = "ins",
              box(background = "light-blue",
              tags$h1("Welcome!"),
              tags$h2("Here is how to utilize my app:"),
              tags$p("There are several tabs on the sidebar menu. They all analyze AUS flights in different ways!
                     You will first be able to view a full time series plot (collapsible, just for fun).
                     Next you can select one plot at a time for viewing.
                     The Interactive Plot tab allows you to scroll and click
                     across the data to view precise data points! 
                     Some simple forecasting and exponential smoothing are included
                     after that. The last tab allows for viewing of arima models
                     (everyone's favorite!)"),
              tags$h3("Thanks for visiting!")),
              tags$img(src = "https://www.thewanderinglens.com/wp-content/uploads/2016/01/header.jpg")),
      
      tabItem(tabName = "tsplot",
              box(
                title = "Full Time Series Plot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              plotOutput("plotall"),
              uiOutput("uio1"))),
      
      tabItem(tabName = "choose",
              box(
              selectInput("sel1", label = "Select a plot", 
                          choices = c("Seasonality", "Autocorrelation",
                                      "Decomposition"))), 
              box(plotOutput("plotchoose"),
              uiOutput("uio2")),
              box(tags$p("Decomposition refers to breaking down a time series into 
                     several components. We can see how the graphic depicts different aspects
                     of all of the arrivals, including: trend, seasonal, and random.
                    Seasonality allows for us to invision
                     seasonal patterns more easily. Through the seasonality graphic, 
                    we can see what quarters of the year bring in the most amount of 
                    flights from the different origins. For people in the UK, it is much
                    more common for them to fly to AUS in Q1 and Q4, which makes sense becasue
                    those are the summer months in AUS. Overall, it seems that all 4 origins
                    have the least amount of flights to AUS in Q2."))
      ),
      tabItem(tabName = "myfeat",
             selectInput("sel2", label = "Choose Origin",
                         choices = c("UK", "US", "Japan", "NZ")),
             plotlyOutput("plotlast"),
             uiOutput("uio3")),
      
      tabItem(tabName = "fc",
      awesomeRadio(
        inputId = "fcid",
        label = "Select a Country",
        choices = c("Japan", "US", "UK", "NZ"),
        selected = "NZ"
      ),
      plotOutput("fcplot"),
      uiOutput("fcplot1")),
      
      tabItem(tabName = "smooth",
              box(setBackgroundColor(color = "#B0E0E6", shinydashboard = TRUE),
              plotOutput("holtplot")),
              box(
                plotOutput("hwplot"))
       ),
      
      tabItem(tabName = "arimatab",
              tags$h2("ARIMA"),
              tags$h3("AR: autoregressive - I: integrated - MA: moving average"),
              box(
                radioGroupButtons(
                  inputId = "selarima",
                  label = "Select to create a model",
                  choices = c("Japan", "NZ", "US", "UK"),
                  selected = NULL
                ),
                plotOutput("arimaplot1"),
                uiOutput("arimaplot")
                
              )),
      tabItem("trip",
              tags$h2("Let's Plan a trip! First step: book a flight"),
              box(
              tags$a(href = "https://www.expedia.com/Flights-Search?leg1=from%3AKnoxville%2C%20TN%20%28TYS-McGhee%20Tyson%29%2Cto%3ASydney%2C%20NSW%20%28SYD-Kingsford%20Smith%20Intl.%29%2Cdeparture%3A11%2F13%2F2022TANYT&leg2=from%3ASydney%2C%20NSW%20%28SYD-Kingsford%20Smith%20Intl.%29%2Cto%3AKnoxville%2C%20TN%20%28TYS-McGhee%20Tyson%29%2Cdeparture%3A11%2F18%2F2022TANYT&mode=search&options=carrier%3A%2A%2Ccabinclass%3A%2Cmaxhops%3A1%2Cnopenalty%3AN&pageId=0&passengers=adults%3A1%2Cchildren%3A0%2Cinfantinlap%3AN&trip=roundtrip",
                     tags$text("CLICK HERE TO CHECK FLIGHTS FROM TYS -> AUS"))),
              tags$image(src = "https://preview.redd.it/g5unfqbro8h01.jpg?auto=webp&s=7dd7098540bc7981416803974eb5cd5db0a4c360", 
                         height = "300", 
                         width = "650"))
              
              ))) 
    




server <- function(input, output, session) {
  
  
  
  output$uio1 <- renderUI({
    renderPlot({
      autoplot(aus_arrivals)
    })
  })
  
  output$uio2 <- renderUI({
    if (input$sel1 == "Seasonality") {
      renderPlot({
        gg_season(aus_arrivals)
      })
      
    }
    
    else 
      if (input$sel1 == "Autocorrelation") {
        renderPlot({
          aus_arrivals %>% ACF() %>%  autoplot()
        })
      }
    
    
    else 
      if (input$sel1 == "Decomposition") {
        renderPlot({
          aus_arrivals %>% model(classical_decomposition(Arrivals, type = "additive")) %>%
            components() %>% autoplot()
        })
      }
    
    
  })
  
  output$uio3 <- renderUI({
    if (input$sel2 == "Japan") {
      renderPlotly({
        aus_arrivals %>%
          filter(Origin == "Japan") %>% autoplot(Arrivals)
      })
    }
    else 
      if (input$sel2 == "US") {
        renderPlotly({
          aus_arrivals %>% filter(Origin == "US") %>% autoplot()
        })
      }
    
    else 
      if (input$sel2 == "UK") {
        renderPlotly({
          aus_arrivals %>% filter(Origin == "UK") %>% autoplot()
        })
        
      }
    else 
      if (input$sel2 == "NZ") {
        renderPlotly({
          aus_arrivals %>% filter(Origin == "NZ") %>% autoplot()
        })
      }
    
  })
  
  output$fcplot1 <- renderUI({ 
    if (input$fcid == "NZ"){
      renderPlot({
        ausfitNZfc %>% autoplot(aus_arrivals, 
                                level = NULL)+labs(title = "NZ->AUS 5 year forecast")
        
      })
    }
    else
      if (input$fcid == "Japan"){
        renderPlot({
          ausfitJapanfc %>% autoplot(aus_arrivals, 
                                     level = NULL)+ labs(title = "Japan->AUS 5 year forecast")
        })
      }
    else
      if (input$fcid == "UK"){
        renderPlot({
          ausfitUKfc %>% autoplot(aus_arrivals,
                                  level = NULL)+ labs(title = "UK->AUS 5 year forecast")
          
        })
      }
    else 
      if (input$fcid == "US"){
        renderPlot({
          ausfitUSfc %>% autoplot(aus_arrivals, 
                                  level = NULL)+labs(title = "US->AUS 5 year forecast")
          
        })
      }
  })
  
  output$holtplot <- renderPlot({
    beet %>% forecast(h = "3 years") %>% autoplot(aus_arrivals, level = NULL)+ labs(title = "Holt Method")
  })
  
  output$hwplot <- renderPlot({
    rootfc %>% autoplot(aus_arrivals, level = NULL)+labs(title = "Holt-Winters Method (captures seasonality)")
  })
  
  output$arimaplot <- renderUI({
    if (input$selarima == "Japan"){
      renderPlot({
        wrist <- aus_arrivals %>% filter(Origin == "Japan") %>% 
          model(
            arima = ARIMA(Arrivals)
          )
        wrist2 <- wrist %>% forecast(h = "5 years")
        wrist2 %>% autoplot(aus_arrivals, level = NULL)
      })
    }
    else if (input$selarima == "US"){
      renderPlot({
        knee <- aus_arrivals %>% filter(Origin == "US") %>% 
          model(
            arima = ARIMA(Arrivals)
          )
        knee2 <- knee %>% forecast(h = "5 years")
        knee2 %>% autoplot(aus_arrivals, level = NULL)
      })
    }
    else if (input$selarima == "UK"){ 
      renderPlot({
        hand <- aus_arrivals %>% filter(Origin == "UK") %>% 
          model(
            arima = ARIMA(Arrivals)
          )
        hand2 <- hand %>% forecast(h = "5 years")
        hand2 %>% autoplot(aus_arrivals, level = NULL)
        
      })
    }
    else if (input$selarima == "NZ"){
      renderPlot({
        foot <- aus_arrivals %>% filter(Origin == "NZ") %>% 
          model(
            arima = ARIMA(Arrivals)
          )
        foot2 <- foot %>% forecast(h = "5 years")
        foot2 %>% autoplot(aus_arrivals, level = NULL)
      })
    }
  })
  
  
}
    

shinyApp(ui, server)


