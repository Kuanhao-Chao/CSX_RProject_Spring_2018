library(shiny)
library(leaflet)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# variable
taipei.region <- c("中正區", "大同區", "中山區", "松山區", "大安區", "萬華區", "信義區", "士林區", "北投區", "內湖區", "南港區", "文山區")
taipei.house.feature <- c("辦公商業大樓","廠辦","店面(店鋪)","工廠","公寓(5樓含以下無電梯)","華廈(10層含以下有電梯)","其他","套房(1房1廳1衛)","透天厝","住宅大樓(11層含以上有電梯)")
taipei.house.usage <- c("住","商","工","住商","住工")

ui <- fluidPage(
  # start UI
  titlePanel("房價查詢、預估平台"),
  navbarPage("功能",
             tabPanel("歷史租屋價查詢",
                      sidebarLayout(
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          selectInput("select.region",
                                      h3("選擇地區:"),
                                      choices = taipei.region),
                          selectInput("select.usage",
                                      h3("租屋用途:"),
                                      choices = taipei.house.usage),
                          h3("房子特徵:"),
                          fluidRow(
                            column(6, align="center", radioButtons("radio.land", h4("土地"),
                                                                   choices = list("是" = 1, "否" = 2),
                                                                   selected = 1)),
                            column(6, align="center",  radioButtons("radio.building", h4("建物"),
                                                                    choices = list("是" = 1, "否" = 2),
                                                                    selected = 1)),
                            column(6, align="center",radioButtons("radio.car.park", h4("車位"),
                                                                  choices = list("是" = 1, "否" = 2),
                                                                  selected = 1)),
                            column(6, align="center", selectInput("select.function", h4("型態"),
                                                                  choices = taipei.house.feature))
                          ),
                          
                          sliderInput("slide.age", h3("屋齡:"),  
                                      min = 0, max = 120, value = c(40, 80)),
                          
                          sliderInput("slide.area.size", h3("面積(平方公尺):"),  
                                      min = 0, max = 100, value = c(40, 80)),
                          
                          h3("租金範圍:"),
                          fluidRow(
                            column(6, numericInput("slide.low.price", h4("最低:"), value = 1)),
                            column(6, numericInput("slide.high.price", h4("最高:"), value = 50))
                          ),
                          
                          fluidRow(
                            column(11, align="center", actionButton("recalc", "確認"))
                          )
                          #selectInput("select.age",
                          #            h3("屋齡:"),
                          #            choices = taipei.house.age),
                          # Input: Numeric entry for number of obs to view ----
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          # Output: Verbatim text for data summary ----
                          
                          #verbatimTextOutput("summary"),
                          
                          # Output: HTML table with requested number of observations ----
                          #tableOutput("view"),
                          checkboxGroupInput("checkGroup.facility", h4("附近設施"),
                                             choices = list("捷運站" = 1, 
                                                            "公車站" = 2, 
                                                            "便利商店" = 3,
                                                            "公園" = 4),
                                             selected = 1,
                                             inlin = TRUE),
                          #textOutput("select.region_output"),
                          #verbatimTextOutput("summary"),
                          leafletOutput("mymap", width = "100%", height = 450),
                          p(),
                          actionButton("recalc", "New points")
                        )
                      )
             ),
             tabPanel("預測租屋價查詢",
                      verbatimTextOutput("summary")
             ),
             navbarMenu("More",
                        tabPanel("Table",
                                 DT::dataTableOutput("table")
                        )
             )
  )
)


server <- function(input, output, session) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  real.estate.data <- read.csv("real_estate_ready.CSV")
  #convenient.store <- read.csv("")
  
  datasetInput <- reactive({
    switch(input)
  })
  
  output$summary <- renderPrint({
    dataset <- real_estate_ready
    summary(dataset)
  })
  #get.data <- c(
  a <- reactive({
    a <- input$select.region
  })
  
  #output$select.region_output <- renderPrint({a()})
  
  #b <- input$select.usage 
  #c <- input$radio.land
  #d <- input$radio.building
  #e <- input$radio.car.park
  #f <- input$select.function
  #g <- input$slide.age
  #h <- input$slide.area.size
  #i <- input$slide.low.price
  #j <- input$slide.high.price
  #k <- input$checkGroup.facility
  #l<- input$checkGroup.facility
  
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = real.estate.data$section,
           "pressure" = real.estate.data$address,
           "cars" = cars)
  })
  
  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$caption
  })
  
  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
  
}

shinyApp(ui, server)