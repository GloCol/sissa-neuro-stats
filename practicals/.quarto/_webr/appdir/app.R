library(shiny)

ui <- fluidPage(
  titlePanel("Maximizing Projected Variance"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("theta", "Projection Vector Angle (°):", min = 0, max = 180, value = 20, step = 2),
      helpText("The red line shows the chosen unit vector. The histogram displays the distribution of data points projected onto this line.")
    ),
    mainPanel(
      plotOutput("pcaPlot", height = "400px")
    )
  )
)

server <- function(input, output) {
  set.seed(123)
  x_raw <- rnorm(70, mean = 0, sd = 1.8)
  y_raw <- 0.7 * x_raw + rnorm(70, mean = 0, sd = 0.7)
  X <- cbind(x_raw - mean(x_raw), y_raw - mean(y_raw))

  output$pcaPlot <- renderPlot({
    rad <- input$theta * pi / 180
    u <- c(cos(rad), sin(rad))
    projections <- as.vector(X %*% u)
    var_proj <- var(projections)

    par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

    # Scatter plot with projection line
    plot(X[, 1], X[, 2], pch = 19, col = rgb(0.1, 0.4, 0.8, 0.7),
         xlim = c(-5, 5), ylim = c(-5, 5), asp = 1,
         xlab = "Neuron 1 (centered)", ylab = "Neuron 2 (centered)",
         main = "Original Data Space")
    abline(0, tan(rad), col = "firebrick", lwd = 2.5)
    arrows(0, 0, 3 * cos(rad), 3 * sin(rad), col = "firebrick", lwd = 3, length = 0.1)
    grid()

    # Histogram of projections
    hist(projections, breaks = seq(-6, 6, by = 0.8), col = "darkorange", border = "white",
         xlim = c(-6, 6), ylim = c(0, 35),
         xlab = "Projected Coordinates",
         main = paste("Projected Variance:", round(var_proj, 3)))
    abline(v = mean(projections), col = "black", lty = 2, lwd = 2)
  })
}

shinyApp(ui, server)
