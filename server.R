library(shiny) 

generate_segments <- function(n){
  theta <- runif(n, 0, pi)
  x1 <- runif(n, 5, 15)
  y1 <- runif(n, 5, 15)
  h_c_t <- 0.5 * cos(theta)
  h_s_t <- 0.5 * sin(theta)
  
  data.frame(x_0 = x1 - h_c_t,
             y_0 = y1 - h_s_t,
             
             x_1 = x1 + h_c_t,
             y_1 = y1 + h_s_t)
  
}

max_and_min <- function(data){
  data.frame(max_x = max(c(data$x_0, data$x_1)),
             min_x = min(c(data$x_0, data$x_1)),
             max_y = max(c(data$y_0, data$y_1)),
             min_y = min(c(data$y_0, data$y_1)))
}

gen_buffon_base_plot <- function(max_min_data){
  # generate plot
  plot(NA, 
       xlim = c(max_min_data$min_x, max_min_data$max_x),
       ylim = c(max_min_data$min_y, max_min_data$max_y),
       xlab = '',
       ylab = '',
       xaxt = 'n',
       yaxt = 'n')
  # draw line segments at integers
  abline(h = floor(max_min_data$min_y):ceiling(max_min_data$max_y),
         lty = 'dashed')
}

draw_buffon_segments <- function(simulated_data){
  
  gen_buffon_base_plot(max_and_min(simulated_data))
  
  
  col_code <- ifelse(
    as.integer(simulated_data[,2]) == as.integer(simulated_data[,4]),
    'blue', 'red'
  )
  
  segments(
    simulated_data[,1],
    simulated_data[,2],
    simulated_data[,3],
    simulated_data[,4],
    col = col_code
  )
}

total_buffon_sim <- function(simulated_data){
  # # generate data
  # 
  # approximate pi
  n <- nrow(simulated_data)
  crosses <- sum(
    as.integer(simulated_data$y_0) != as.integer(simulated_data$y_1)
  )
  approx_pi <- 2 * (n / crosses)
  
  draw_buffon_segments(simulated_data)
  title(paste('After', scales::comma(n), 
              'trials, our approximation for pi is:',
              round(approx_pi, 5)))
  
  
}

buff_sim_report_plot <- function(simulated_data, show_legend = 'Show'){
  
  x <- 1:nrow(simulated_data)
  
  crosses <- as.integer(simulated_data$y_0) != as.integer(simulated_data$y_1)
  
  y <- 2 * (x / cumsum(crosses))
  

  plot(x, y,
       xlab = 'Cumulative Number of Trials',
       ylab = expression(paste('Approximation of ', pi)),
       main = 'Trend in Approximation of pi\nby Simulation Number',
       type = 'l',
       lwd = 1.2)
  abline(h = pi, col = 'red', lty = 2)
  if(show_legend == 'Show'){
    legend('bottomleft',
           col = c('black', 'red'),
           legend = c('Approximation Trend',
                      expression(pi)),
           lty = c(1,1),
           bty = 'n')
  }
  

}


num_trials <- function(n){
  paste('Number of Trials:', scales::comma(n))
}



buff_approx <- function(simulated_data){
  n <- nrow(simulated_data)
  crosses <- sum(
    as.integer(simulated_data$y_0) != as.integer(simulated_data$y_1)
  )
  approx_pi <- 2 * (n / crosses)
  
  paste('Approximation of pi:', round(approx_pi, 5))
  
}

buff_perc_error <- function(simulated_data){
  n <- nrow(simulated_data)
  crosses <- sum(
    as.integer(simulated_data$y_0) != as.integer(simulated_data$y_1)
  )
  approx_pi <- 2 * (n / crosses)
  percent_error <- abs((pi - approx_pi) / pi)
  
  paste('Percent Error:', scales::percent(percent_error))
}

buff_perc_cross <- function(simulated_data){
  n <- nrow(simulated_data)
  crosses <- sum(
    as.integer(simulated_data$y_0) != as.integer(simulated_data$y_1)
  )
  
  paste('About', scales::percent(crosses/n),
        'of needles crossed a parallel line. We would expect about',
        scales::percent(2 / pi),
        'of needles to cross')
}


generate_semi_circle <- function(n){
  x <- runif(n)
  y <- runif(n)
  in_circle <- (sqrt(x^2 + y^2) <= 1)
  
  data.frame(x, y, in_circle)
}

semi_circle_plot <- function(sim_data){
  
  n <- nrow(sim_data)
  
  approx_pi <- 4 * mean(sim_data[,3])
  
  percent_error <- abs(pi - approx_pi) / pi
  
  caption <-  paste('After', scales::comma(n), 
                          'trials, our approximation for pi is:',
                          round(approx_pi, 5))
    
    
    
  plot(sim_data[,-3],
       col = ifelse(sim_data[,3], 'red', 'blue'),
       main = caption,
       xlim = 0:1,
       ylim = 0:1)
  curve(sqrt(1 - x^2), add = TRUE, lty = 'dashed')
}

circ_sim_report_plot <- function(simulated_data, show_legend = 'Show'){
  
  x <- 1:nrow(simulated_data)
  
  fall_within <- simulated_data[,3]
  
  y <- 4 * (cumsum(fall_within) / x)
  
  
  plot(x, y,
       xlab = 'Cumulative Number of Trials',
       ylab = expression(paste('Approximation of ', pi)),
       main = 'Trend in Approximation of pi\nby Simulation Number',
       type = 'l',
       lwd = 1.2)
  abline(h = pi, col = 'red', lty = 2)
  if(show_legend == 'Show'){
    legend('bottomleft',
           col = c('black', 'red'),
           legend = c('Approximation Trend',
                      expression(pi)),
           lty = c(1,1),
           bty = 'n')
  }
  
  
}

circ_approx <- function(sim_data){
  paste('Approximation of pi:', round(4 * mean(sim_data[,3]), 5))
}

circ_perc_error <- function(sim_data){
  approx_pi <- 4 * mean(sim_data[,3])
  percent_error <- abs((pi - approx_pi) / pi)
  paste('Percent Error:', scales::percent(percent_error))
  
}

circ_perc_in <- function(sim_data){
  
  percent_in <- mean(sim_data[,3])
  paste('About', scales::percent(percent_in), 
        'of dots fell within the quarter circle. We would expect about',
        scales::percent(pi/4), 'of dots to fall within.')
  
  
}

shinyServer(function(input, output) {
  
  output$ex1 <- renderUI({
    withMathJax(helpText('Dynamic output 1:  $$\\alpha^2$$'))
  })

  # buffon
   sim_data <- reactive({generate_segments(input$buffon_trials)})
   output$buffon_plot <- renderPlot(total_buffon_sim(sim_data()))
   output$buffon_progress_plot <- renderPlot(
     buff_sim_report_plot(sim_data(),input$buff_legend))
   
   output$b_n_trials <- renderText(num_trials(input$buffon_trials))
   output$b_n_approx <- renderText(buff_approx(sim_data()))
   output$b_n_perc <- renderText(buff_perc_error(sim_data()))
   output$b_n_perc_cross <- renderText(buff_perc_cross(sim_data()))
   
   circle_sim_data <- reactive({generate_semi_circle(input$circle_trials)})
   
   output$quarter_circle_plot <- renderPlot(semi_circle_plot(circle_sim_data()))
   output$q_c_progress_plot <- renderPlot(circ_sim_report_plot(
     circle_sim_data(), input$q_c_legend
   ))
   
   output$q_c_trials <- renderText(num_trials(input$circle_trials))
   output$q_c_approx <- renderText(circ_approx(circle_sim_data()))
   output$q_c_perc <- renderText(circ_perc_error(circle_sim_data()))
   output$q_c_perc_cross <- renderText(circ_perc_in(circle_sim_data()))
   
})
