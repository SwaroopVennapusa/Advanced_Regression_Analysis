
# Interactive demo (with R Shiny) of the ROC curve
# Adapted from source: https://gist.github.com/topepo/d179feeba12dd8bfdaaee75c7d5fcbd0

library(shiny)
library(ggplot2)
library(dplyr)
library(yardstick)

theme_set(theme_bw())

n <- 1000

set.seed(124254)
prob_data <- 
  tibble(
    #prob = c(1 - rbeta(n, 1, 3), rbeta(n, 1.5, 4)),
    prob = c(rnorm(n, .7, .15), rnorm(n, .3, .15)),
    class = factor(rep(c("event", "non-event"), each = n))
  )

wdth <- 0.04
cut_seq <- seq(0, 1, by = wdth)
midpoints <- cut_seq + 0.5 * diff(cut_seq)[1]

freq_data <- 
  prob_data %>% 
  mutate(binned = cut(prob, breaks = cut_seq, include.lowest = TRUE)) %>% 
  group_by(class, binned) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(prob_value = midpoints[as.integer(binned)])

p_hist <- 
  ggplot(freq_data, aes(x = prob_value, y = n, fill = class)) + 
  geom_bar(stat = "identity", position = "identity", alpha = .4) + 
  theme_bw() + 
  theme(legend.position = "top") + 
  labs(x = "Probability of the Event", y = "Count") + 
  scale_fill_manual("legend", values = c("event" = "orange", "non-event" = "blue"))

curve <- yardstick::roc_curve(prob_data, class, prob) 
p_roc <- autoplot(curve)

ui <-
  pageWithSidebar(
    headerPanel("ROC Curves"),
    sidebarPanel(
      sliderInput(
        "cut_point",
        label = "Probability Threshold for an Event",
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.01
      )
    ),
    # sidebarPanel
    mainPanel(
      plotOutput("hist"),
      HTML("<br><br><br>"),
      plotOutput("roc")
    ) # mainpanel
  ) # pageWithSidebar

server <-
  function(input, output) {
    
    cut_stats <- reactive({
      cut_point <- input$cut_point
      curve %>% 
        filter(is.finite(.threshold)) %>% 
        mutate(delta = abs(.threshold - cut_point)) %>% 
        arrange(delta) %>% 
        slice(1)
    })
    
    
    output$hist <- renderPlot({
      info <- cut_stats()
      p_hist + geom_vline(xintercept = info$.threshold[1], col = "black")
    }) # hist
    
    output$roc <- renderPlot({
      info <- cut_stats()
      p_roc + 
        geom_point(data = info, aes(x = 1 - specificity, y = sensitivity), 
                   cex = 2, 
                   col = "red") + 
        ggtitle(
          paste0(
            "threshold = ", round(info$.threshold[1], 3),
            ", sensitivity = ", round(info$sensitivity[1], 3),
            ", specificity = ", round(info$specificity[1], 3)
          )
        )
    }) # roc
    
  } # function

shinyApp(ui, server)