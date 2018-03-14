#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#


library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(theme = shinytheme('paper'),
  HTML('Simulating pi'),
  tabPanel(
    "What is this?",
    fluidPage(
      fluidRow(
        column(8,
    withMathJax(includeMarkdown('main_page_text.Rmd')),
    
    h4("In case you're interested, here are some other apps I've created: "),
    p(a("Budget App",
        href = "https://bgstieber.shinyapps.io/budget_shiny_app/"),
      "- an application that automates some of the tedious data
analysis involved in analyzing your checking account."
    ),
    p(a("KDE Bandwidth Demonstration",
        href = "https://bgstieber.shinyapps.io/kdeshinyapp/"),
      "- an application that visualizes how playing with the bandwidth
for univariate kernel density estimation impacts the density plot."
    ),
    p(a("Probability Squares",
        href = "https://bgstieber.shinyapps.io/probabilitysquares/"),
      "- a (mostly toy) application that demonstrates one way of visualizing
probabilities, likelihoods, or group memberships.")
),
column(4)
           ))),
  
  tabPanel("Buffon's Needle",
           fluidPage(
             fluidRow(
               column(3,
                      h3("Buffon's Needle Simulation"),
                      numericInput('buffon_trials',
                                   'Enter # of Needles to Toss',
                                   value = 100,
                                   min = 1),
                      p("Try some different options for the number
                        of needles to toss, and note how the approximation
                        for pi improves or degrades."),
                      p("You'll also notice that sometimes a smaller
                        number of trials will yield a closer approximation
                        than a larger number of trials. This simulation is
                        based on generating random numbers, and although
                        in general we would expect a larger simulation to 
                        yield better results, the randomness within this process
                        may sometimes yield interesting results."),
                      p("In the 18th century, French philosopher Georges-Louis 
                        Leclerc, Comte de Buffon determined that you can 
                        approximate pi by dropping needles on a grid of parallel 
                        lines and calculating the probability that they will 
                        cross a line. 
                        The probability is directly related to pi.")
               ),
               column(6,
                      plotOutput('buffon_plot'),
                      p("In the plot above, we visualize the results ",
                        "of tossing the needles. Red needles are those that
                        crossed one of the parallel lines, while blue
                        needles are ones that stayed within the lines.")
               ),
               column(3,
                      h3("Simulation Report"),
                      textOutput('b_n_trials'),
                      textOutput('b_n_approx'),
                      textOutput('b_n_perc'), 
                      textOutput('b_n_perc_cross'),
                      plotOutput('buffon_progress_plot'),
                      radioButtons('buff_legend',
                                   'Show or Hide Legend in Trend Plot',
                                   choices = c('Show', 'Hide')))
             ))),
  
  tabPanel("Using a Quarter Circle",
           fluidPage(
             fluidRow(
               column(3,
                      h3('Quarter Circle Monte Carlo'),
                      numericInput('circle_trials',
                                   'Enter # of Dots to Draw',
                                   value = 100,
                                   min = 1),
                      p("Try some different options for the number
                        of needles to toss, and note how the approximation
                        for pi improves or degrades."),
                      p("You'll also notice that sometimes a smaller
                        number of trials will yield a closer approximation
                        than a larger number of trials. This simulation is
                        based on generating random numbers, and although
                        in general we would expect a larger simulation to 
                        yield better results, the randomness within this process
                        may sometimes yield interesting results.")
               ),
               column(6,
                      plotOutput('quarter_circle_plot'),
                      p("In the plot above, we visualize the results ",
                        "of selecting points within the quarter square. ",
                        "Red points are those that fell within the quarter",
                        "circle, and blue points are those that were outside",
                        "of it.")
               ),
               column(3,
                      h3('Simulation Report'),
                      textOutput('q_c_trials'),
                      textOutput('q_c_approx'),
                      textOutput('q_c_perc'), 
                      textOutput('q_c_perc_cross'),
                      plotOutput('q_c_progress_plot'),
                      radioButtons('q_c_legend',
                                   'Show or Hide Legend in Trend Plot',
                                   choices = c('Show', 'Hide')))
             )))
))