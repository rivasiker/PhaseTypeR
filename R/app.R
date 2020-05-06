library(shiny)
library(shinyMatrix)
library(expm)
library(phasty)

m <- matrix(0.2, 3, 3)

ui <- fluidPage(
  titlePanel('shinyMatrix: Simple App'),
  inputPanel(
    tags$h4("Data"),

    actionButton('add', 'add'),
    actionButton('remove', 'remove'),
    matrixInput('sample', value <- m),

    radioButtons(inputId = 'wh_plot', choices = c('d', 'p', 'q'),
                 label = 'Which plot')


  ),
  mainPanel(
    conditionalPanel(
      condition = "input.wh_plot == 'd'", plotOutput("plot1")),
    conditionalPanel(
      condition = "input.wh_plot == 'p'", plotOutput("plot2")),
    conditionalPanel(
      condition = "input.wh_plot == 'q'", plotOutput("plot3")),
    plotOutput("scatter"),
    verbatimTextOutput('test')
  )
)


# Define server logic to plot various variables against mpg ----
server <- function(input, output, session) {

  data <- reactive(phase_type(input$sample, round_zero = 6))

  max_x <- reactive(qphtype(0.95, data()))

  pas <- reactive({
    if(class(data()) == 'disc_phase_type'){
      pas <- 1
    } else if (class(data()) == 'cont_phase_type'){
      pas = 0.01 * max_x()
    }
  })



  x <- reactive({
    if (input$wh_plot == 'd'){
      x = seq(0, max_x(), pas())
      } else if (input$wh_plot == 'p'){
        x = seq(0, max_x(), pas())
      } else if (input$wh_plot == 'q'){
        x = seq(0.03, 0.97, 0.01)
      }
  })

  output$plot1 <- renderPlot({
    plot(x(), dphtype(x(), data()))
    })
  output$plot2 <- renderPlot({
    plot(x(), pphtype(x(), data()))
  })
  output$plot3 <- renderPlot({
    plot(qphtype(x(), data()), x())
  })

  output$test <- renderPrint(summary(data()))

  observeEvent(input$add, {
    updateMatrixInput(session, "sample",
                      value = rbind(cbind(input$sample, 0), 0))
    })
  observeEvent(input$remove, {
    updateMatrixInput(session, "sample",
                      value = input$sample[1:(nrow(input$sample)-1), 1:(ncol(input$sample)-1)])
    })
}

shinyApp(ui, server)
