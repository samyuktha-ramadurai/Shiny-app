library(deSolve)
library(shiny)

# Defining the differential equations
lotka_volterra <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dxdt = alpha * x - beta * x * y
    dydt = delta * x * y - gamma * y
    return(list(c(dxdt, dydt)))
  })
}

# Defining the UI
ui <- fluidPage(
  titlePanel("Lotka-Volterra Predator-Prey Dynamics"),

  sidebarLayout(
    sidebarPanel(
      numericInput("alpha", "Prey growth rate (alpha):", 1),
      numericInput("beta", "Predator-prey interaction rate (beta):", 0.1),
      numericInput("gamma", "Predator death rate (gamma):", 1),
      numericInput("delta", "Predator reproduction rate (delta):", 0.1),
      numericInput("x0", "Initial prey population x(0):", 40),
      numericInput("y0", "Initial predator population y(0):", 9),
      numericInput("time", "Time range:", 100, min = 1, step = 1)
    ),

    mainPanel(
      plotOutput("solutionPlot")
    )
  )
)

# Defining the server
server <- function(input, output) {

  # Reactive expression to solve the Lotka-Volterra system
  solve_system <- reactive({
    # Parameters
    parameters <- c(alpha = input$alpha, beta = input$beta, gamma = input$gamma, delta = input$delta)

    # Initial state
    state <- c(x = input$x0, y = input$y0)

    # Time span
    time <- seq(0, input$time, by = 0.1)

    # Solving the system of ODEs
    out <- ode(y = state, times = time, func = lotka_volterra, parms = parameters)

    # Converting the output to a data frame for plotting
    data.frame(Time = out[, 1], Prey = out[, 2], Predator = out[, 3])
  })

  # Plotting the solution
  output$solutionPlot <- renderPlot({
    result <- solve_system()

    plot(result$Time, result$Prey, type = "l", col = "blue", lwd = 2,
         xlab = "Time", ylab = "Population", ylim = range(c(result$Prey, result$Predator)))
    lines(result$Time, result$Predator, col = "red", lwd = 2)
    legend("topright", legend = c("Prey (x)", "Predator (y)"), col = c("blue", "red"), lwd = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
