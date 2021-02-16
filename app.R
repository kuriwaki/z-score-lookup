#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Z-score to probability"),

    tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("z",
                        "Z-score",
                        min = -2.5,
                        max = 2.5,
                        step=0.01,
                        ticks=TRUE,
                        value = 1.96),
            withMathJax(),
            p("$P(X ≤ z) =$"),
            textOutput("prob")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$prob <- renderPrint({pnorm(input$z)})

    library(ggplot2)
    library(gridExtra)

    # manually save colors
    col1 <- "#3B429F"
    col2 <- "#76BED0"
    col3 <- "#F55D3E"

    # set parameters for normal dist
    # (keeping standard normal for simplicity)
    mu <- 0
    sd <- 1

    output$plot <- renderPlot({

    # useful "shader" function taken from: https://t-redactyl.io/blog/2016/03/creating-plots-in-r-using-ggplot2-part-9-function-plots.html
        funcShaded <- function(x) {
            y <- dnorm(x, mean = 0, sd = 1)
            y[x > input$z] <- NA
            return(y)
        }

    p1 <- ggplot(data.frame(x = c(-20, 20)), aes(x = x)) +
        stat_function(fun=funcShaded, geom="area", fill=col2, alpha=0.6) +
        stat_function(fun = dnorm, args = list(mu, sd), color=col1, size = 1.4) +
        ggtitle("PDF") +
        labs(x="", y="") +
        theme_bw() +
        scale_x_continuous(limits = c(-5, 5), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 0.5), expand = c(0, 0)) +
        geom_vline(xintercept=input$z, lty=2, size=1.2, color=col3) +
        annotate("text", x=ifelse(input$z<0, input$z - 0.4, input$z + 0.4),
                 y=dnorm(input$z, 0, 1) + 0.05, label="Z",
                 parse=TRUE, size=5, color=col3) +
        theme(axis.line = element_line(size=1, colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(size = 20, family = "Tahoma", face = "bold"),
              text=element_text(family="Tahoma"),
              axis.text.x=element_text(colour="black", size = 11),
              axis.text.y=element_text(colour="black", size = 11))

    p2 <- ggplot(data.frame(x = c(-20, 20)), aes(x = x)) +
        annotate("segment", x=input$z, xend=input$z,
                 y=0, yend=pnorm(input$z), color=col3, lty=2, size=1.4) +
        annotate("segment", x=-5, xend=input$z,
                 y=pnorm(input$z), yend=pnorm(input$z), color=col2, lty=2, size=1.4) +
        stat_function(fun = pnorm, args = list(mu, sd), color=col1, size = 1.4) +
        ggtitle("CDF") +
        labs(x="", y="") +
        theme_bw() +
        scale_x_continuous(limits = c(-5, 5), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 1.14), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), expand = c(0, 0)) +
        annotate("text", x=input$z + 0.4,
                 y=pnorm(input$z, 0, 1) - 0.1, label="Z",
                 parse=TRUE, size=5, color=col3) +
        annotate("text", x=-3, y=(pnorm(input$z) + 0.05), label=("'P(X' <= Z ~ ')'"),
                 parse=TRUE, size=5, color=col2) +
        theme(axis.line = element_line(size=1, colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(size = 20, family = "Tahoma", face = "bold"),
              text=element_text(family="Tahoma"),
              axis.text.x=element_text(colour="black", size = 11),
              axis.text.y=element_text(colour="black", size = 11))

    grid.arrange(p1, p2, nrow=1)

    })
}

# Run the application
shinyApp(ui = ui, server = server)


