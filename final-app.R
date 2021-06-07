library(shiny)

dist <- c("Gamma", "Beta", "Binomial")
blues <- c("Light Blue", "Sky Blue", "Midnight Blue")
greens <- c("Light Green", "Olive Green", "Dark Green")
purples <- c("Light Plum", "Medium Purple", "Dark Purple")

ui <- fluidPage(
    headerPanel("Final Project"),
    sidebarLayout(
        sidebarPanel(
            selectInput("Distribution", label = "Choose Distribution Type:", dist),
            conditionalPanel(condition = "input.Distribution == 'Gamma'",
                             selectInput("bluetype", "Customize Color:", blues),
                             sliderInput("alpha", "Shape Parameter (a):", min = 0.1, max = 100, value = 2),
                             sliderInput("beta", "Rate Parameter (b):", min = 0.1, max = 100, value = 2)),
            conditionalPanel(condition = "input.Distribution == 'Beta'",
                             selectInput("greentype", "Customize Color:", greens),
                             sliderInput("kvalue", "K Parameter:", min = 0.1, max = 100, value = 2),
                             sliderInput("theta", "Theta Parameter:", min = 0.1, max = 100, value = 2)),
            conditionalPanel(condition = "input.Distribution == 'Binomial'",
                             selectInput("purpletype", "Customize Color:", purples),
                             sliderInput("nvalue", "N:", min = 5, max = 100, value = 10),
                             sliderInput("prob", "Probability of Success (p):", min = 0, max = 1, value = 0.1)),
            sliderInput("n", label = "Sample Size:", min = 1, max = 100, value = 2),
        ),
        mainPanel(
            plotOutput("histplot")
        )
    )
)

server <- function(input, output, session) {
    
    output$histplot <- renderPlot({
        
        typedist <- input$Distribution
        n <- input$n
        a <- input$alpha
        b <- input$beta
        k <- input$kvalue
        t <- input$theta
        nval <- input$nvalue
        p <- input$prob
        
        if (input$bluetype == "Light Blue") { bluetype = "lightblue1" } 
        else if (input$bluetype == "Sky Blue") { bluetype = "deepskyblue2" } 
        else if (input$bluetype == "Midnight Blue") { bluetype = "midnightblue" }
        
        if (input$greentype == "Light Green") { greentype = "darkseagreen3" } 
        else if (input$greentype == "Olive Green") { greentype = "darkolivegreen4" } 
        else if (input$greentype == "Dark Green") { greentype = "darkgreen" }
        
        if (input$purpletype == "Light Plum") { purpletype = "plum1" } 
        else if (input$purpletype == "Medium Purple") { purpletype = "mediumpurple2" } 
        else if (input$purpletype == "Dark Purple") { purpletype = "purple4" }
        
        if (typedist == "Gamma") { 
            means <- replicate(1e4, mean(rgamma(n, shape = a, rate = b)))
            hist(means, breaks = 25, col = bluetype, main = "Histogram of Sample Means: Gamma Distribution")
            grid()
            box()
        } else if (typedist == "Beta") {
            means <- replicate(1e4, mean(rbeta(n, shape1 = k, shape2 = t, ncp = 0)))
            hist(means, breaks = 25, col = greentype, main = "Histogram of Sample Means: Beta Distribution")
            grid() 
            box()
        } else if (typedist == "Binomial") {
            means <- replicate(1e4, mean(rbinom(n, size = nval, prob = p)))
            hist(means, breaks = 25, col = purpletype, main = "Histogram of Sample Means: Binomial Distribution")
            grid()
            box()
        }
    }, 
    res = 90)
}

shinyApp(ui, server)

