library(shiny)
library(tidyverse)


## Ongoing Issues

# This is a piece of coursework for my MSc, please judge it accordingly. Updates may
# be slow or non-existent as a result.

# Reducing the hits and misses and/or false alarms and correct rejections causes
# an error that I currently don't know how to fix. I consulted the TquanT version
# and that also has an error (with a swanky custom error message).

# I struggled to get working user-defined functions to work, hence the repeated
# code and giant for loop. I know for loops can be stressful on the old computer, and 
# can possibly go wrong and get you stuck infinitely. That's why iterations are 
# locked at an upper limit of 10k and an empty data frame is defined prior to the 
# initiation of the loop. Hopefully this minimises the risk.

# none of my testing has run up against these problems thus far- feel free to
# test it and email problems/fixes to me or fix it yourself.

# It is ugly.



# Define UI for Signal Detection Calculator that mimics the
# functionality of the faceresearch SDA (DeBruine, n.d.)

ui <- fluidPage(
  
  # Application title (smaller and centred that titlePanel)
  HTML('<h2><center>Signal Detection Calculator</center></h2>'),
  
  # Sidebar with a slider input for number of bins 
  
  fluidRow(column(width = 3, offset = 0,
                  # user inputs defined and positioned
                  wellPanel(numericInput(inputId = "hits", # in panel together for tidyness
                                         label =  "Hits", 
                                         min = 0, 
                                         value = 155),
                            numericInput(inputId = "misses",
                                         label = "Misses",
                                         min = 0,
                                         value = 55),
                            numericInput(inputId = "false_alarms",
                                         label = "False Alarms", 
                                         min = 0, 
                                         value = 44), 
                            numericInput(inputId = "correct_rejections",
                                         label = "Correct Rejections", 
                                         min = 0, 
                                         value = 145), 
                            numericInput(inputId = "bs", 
                                         label = "Bootstrap Iterations", 
                                         min = 0, value = 100),
                            textInput(inputId = "plot_title",
                                      label = "Graph Title:"),
                            textInput(inputId = "plot_subtitle",
                                      label = "Graph Subtitle:",
                                      value = ""))),
           column(width = 8, offset = 0,  
                  # graph and stats output defined and positioned
                  plotOutput(outputId = "d_plot"))),
  
  fluidRow(column(width = 4, offset = 1,
                  htmlOutput(outputId = "tab1")),
           column(width = 4, offset = 2,
                  htmlOutput(outputId = "tab2"))),
  
  fluidRow(column(width = 5, offset = 1,
                  tableOutput(outputId = "stats_out")),
           column(width = 5, offset = 1,
                  tableOutput(outputId = "probs_out"))),
  
  fluidRow(column(width = 10, offset = 0,
                  wellPanel(HTML(

'<h4>Quick Notes:</h3>
<ul> 
<li> Ph = probability of a hit, Pm = probability of a miss, Pf = probability of a false alarm and Pcr = probability of a correct rejection.
<li> Probabilities are corrected if they are equal to 0 or 1. <br> 
<li> Values for d SD, Beta SD, z and p (comparing d to 0) are calculated from simulated data. <br>
<li> This app is adapted from various sources and the metrics generated from  it are subject to certain assumptions. 
<li> For more information see commentary in the source code. <br> <br>

<center> Made with: 
<a href = "https://www.rstudio.com/"> <img src = "rstudio.png" alt = "RStudio" height = 55 width = 50> </a>
<a href = "https://www.rstudio.com/products/shiny/"> <img src = "shiny.png" alt = "Shiny" height = 47 width = 44> </a>
<a href = "https://www.tidyverse.org/"> <img src = "tidyverse.png" alt = "Tidyverse" height = 47 width = 44> </a>
<a href = "http://faceresearch.org/res/stats/sda"> <img src = "kinfaces.png" alt = "KINSHIP Project SDA" height = 49 width = 46> </a> </center>')))))
  


## Define server logic required to calculate the stats and generate the graphs

server <- function(input, output) {
  
  output$tab1 <- renderText({
    paste("<b><u>Model Statistics</u></b>")
  })
  
  output$tab2 <- renderText({
    paste("<b><u>Probability Table</u></b>")
  })
  
  output$stats_out <- renderTable({
    
    yes <- input$hits + input$misses
    no <- input$false_alarms + input$correct_rejections
    
    h_rate <- input$hits / yes
    
    if (h_rate == 0) 
    {h_rate <- 0 + 1/(2 * yes)}
    else if (h_rate == 1)
    {h_rate <- 1 - 1/(2 * yes)}
    else 
    {h_rate <- input$hits / yes}
    
    f_rate <- input$false_alarms / no
    
    if (f_rate == 0) 
    {f_rate <- 0 + 1/(2 * no)} 
    else if (f_rate == 1)
    {f_rate <- 1 - 1/(2 * no)} 
    else 
    {f_rate <- input$false_alarms / no}
    
    # if == 1, make it 1 - an arbitrary little amount
    # if == 0 make it 0 + an arbitrary little amount
    # (Lee, 2013; Wickens, 2001)
    
    # correction specified here comes from the source code of TquanT SDA 
    # app (TquanT, 2017).
    
    zh_rate <- qnorm(h_rate)
    zf_rate <- qnorm(f_rate)
    d <- zh_rate - zf_rate
    beta <- exp(-d * 0.5 * (zh_rate + zf_rate))
    
    # Probability of hit and false alarm, as well as d' and Beta should be 
    # reported as per the Wicken's Elementary Signal Detection Theory book.
    # Probability of miss and correct rejection easy to work out from these.
    
    # Equal Variance Gaussian Model assumptions (wickens, 2001):
    # 1) that the prob distributions of noise and signal + noise are Gaussian. 
    #    This is a reasonable assumption for most types of detection experiments
    #    which should sample across large numbers of trials. Furthermore, this
    #    type of distribution is very familiar to many researchers because of
    #    its ubiquity.
    # 2) That the mean of the noise and the noise + signal distributions 
    #    are equal. Although these are rarely to be worried about as the
    #    noise and noise + signal variables can be given any centre or spread,
    #    what really matters is the relationship to each other. We cannot know
    #    the absolute values. This does not limit the predictive power of the 
    #    model.
    # 3) The variance of the two distributions are equal. This is the one most
    #    likely to be violated and may lead you to form incorrect conclusions
    #    based on a model that uses equal variance, where d' = the difference
    #    between the means, where the mean is no longer the peak of the 
    #    probability distributions of noise and noise + signal distributions.
    #    There are unequal-variance models, but they are beyond my current 
    #    understanding.
    
    # Criterion B chosen because the tool which this is based upon uses that     ## needs more information here
    # statistic, (Wickens, 2001).
    
    # Resampled data to obtain z and p values, comparing d' to 0 (the mean 
    # of the noise distribution, meaning that the observed results of the 
    # experiment for the signal + noise trials do not differ from those of
    # the noise trials.)
    
    # simulation based upon 1) lessons from the research cycle course (Barr and DeBruine, 2017).
    # and tuition received from Dr DeBruine as part of my dissertation project work.
    
    # empty data frame for simulation of hits and false alarms
    sim_stats <- data.frame("sim_hits" = numeric(input$bs),
                            "sim_misses" = numeric(input$bs),
                            "sim_false_alarms" = numeric(input$bs),
                            "sim_correct_rejections" = numeric(input$bs),
                            "sim_h_rate" = numeric(input$bs),
                            "sim_f_rate" = numeric(input$bs),
                            "sim_d" = numeric(input$bs),
                            "sim_beta" = numeric(input$bs))
    
    for (i in input$bs)
      
      {
      # simulate hits
      sim_hits <- sum(sample(c(0,1), yes, replace=T, prob=c(1 - input$hits/yes, input$hits/yes)))
      
      # calculate misses
      sim_misses <- yes - sim_hits
      
      # simulate false alarms
      sim_false_alarms <- sum(sample(c(0,1), no, replace=T, prob=c(1 - input$false_alarms/no, input$false_alarms/no)))
      
      # calculate correct rejections
      sim_correct_rejections <- no - sim_false_alarms
      
      # simulated hit rate for each iteration- with corrections
      sim_h_rate <- sim_hits / (sim_hits + sim_misses)
      
      if (sim_h_rate == 0) 
      {sim_h_rate <- 0 + 1/(2 * (sim_hits + sim_misses))}
      else if (sim_h_rate == 1)
      {sim_h_rate <- 1 - 1/(2 * (sim_hits + sim_misses))}
      else 
      {sim_h_rate <- sim_hits / (sim_hits + sim_misses)}
      
      # simulated false alarm rate for each iteration- with corrections
      sim_f_rate <- sim_false_alarms/(sim_false_alarms + sim_correct_rejections)
      
      if (sim_f_rate == 0) 
      {sim_f_rate <- 0 + 1/(2 * (sim_false_alarms + sim_correct_rejections))} 
      else if (sim_f_rate == 1)
      {sim_f_rate <- 1 - 1/(2 * (sim_false_alarms + sim_correct_rejections))} 
      else 
      {sim_f_rate <- sim_false_alarms / (sim_false_alarms + sim_correct_rejections)}
      
      # calculate d' and Beta
      
      sim_zh_rate <- qnorm(sim_h_rate)
      sim_zf_rate <- qnorm(sim_f_rate)
      
      sim_d <- sim_zh_rate - sim_zf_rate
      sim_beta <- exp(-sim_d * 0.5 * (sim_zh_rate + sim_zf_rate))
      
      # generate a data.frame with hit and false alarms
      sim_stats$sim_hits[i] <- sim_hits
      sim_stats$sim_misses[i] <- sim_misses
      sim_stats$sim_false_alarms[i] <- sim_false_alarms
      sim_stats$sim_correct_rejections[i] <- sim_correct_rejections
      sim_stats$sim_h_rate[i] <- sim_h_rate
      sim_stats$sim_f_rate[i] <- sim_f_rate
      sim_stats$sim_d[i] <- sim_d
      sim_stats$sim_beta[i] <- sim_beta  
      
    }
    
    # Random samples were drawn from a choice of 0 or 1, with the same proportion as 
    # indicated by user input with the same probabilites of being drawn. This was done
    # a number of times as indicated by the user through number of iterations.
    
    # These were used to create the simulated number of hits, misses, false alarms and
    # correct rejections needed to calculate the d' and the Beta statistics. Using these,
    # the standard deviations across all iterations were used, and the standardised z and p
    # (given normal distribution assumptions) were calculated from these.
    
    return(tibble("d'" = d, 
                  # discriminability
                  "d' SD" = sd(sim_stats$sim_d), 
                  # sd of discriminability from the sim_stats
                  "Beta" = beta, 
                  # decision criterion Beta
                  "Beta SD"= sd(sim_stats$sim_hits), 
                  # sd of beta from the sim_stats
                  "z" = d - mean(sim_stats$sim_d)/sd(sim_stats$sim_d), 
                  # how far is the d from the simulated mean of d, in standardized units?
                  # z = (our observation d' - est. pop mean d')/ est. pop sd
                  # Dolinar (2014)
                  "p" = 2*pnorm(-abs(d - mean(sim_stats$sim_d)/sd(sim_stats$sim_d)))))
                  # p = pnorm(-abs(z)), Black (2015)

 
    }, align = "c")
  
  output$probs_out <- renderTable({
    
    yes <- input$hits + input$misses
    no <- input$false_alarms + input$correct_rejections
    
    h_rate <- input$hits / yes
    
    if (h_rate == 0) 
    {h_rate <- 0 + 1/(2 * yes)}
    else if (h_rate == 1)
    {h_rate <- 1 - 1/(2 * yes)}
    else 
    {h_rate <- input$hits / yes}
    
    f_rate <- input$false_alarms / no
    
    if (f_rate == 0) 
    {f_rate <- 0 + 1/(2 * no)} 
    else if (f_rate == 1)
    {f_rate <- 1 - 1/(2 * no)} 
    else 
    {f_rate <- input$false_alarms / no}
    
    return(tibble(" " = c("Correct", "Incorrect"),
                  "Yes Response" = c(h_rate, 1 - h_rate),
                  "No Response" = c(f_rate, 1 - f_rate)))
    # hit and false alarm rates, should be reported as standard with d' and Beta 
    # as  h anf f report on real data, but d' and Beta are theoretical models 
    # statistics(Wickens, 2001)))
    
  })
  
  output$d_plot <- renderPlot({
    
    # calculate the metrics again... because my shiny function skills are bad
    
    yes <- input$hits + input$misses
    no <- input$false_alarms + input$correct_rejections
    
    h_rate <- input$hits / yes
    
    if (h_rate == 0) 
    {h_rate <- 0 + 1/(2 * yes)}
    else if (h_rate == 1)
    {h_rate <- 1 - 1/(2 * yes)}
    else 
    {h_rate <- input$hits / yes}
    
    f_rate <- input$false_alarms / no
    
    if (f_rate == 0) 
    {f_rate <- 0 + 1/(2 * no)} 
    else if (f_rate == 1)
    {f_rate <- 1 - 1/(2 * no)} 
    else 
    {f_rate <- input$false_alarms / no}
    
    zh_rate <- qnorm(h_rate)
    zf_rate <- qnorm(f_rate)
    d <- zh_rate - zf_rate
    beta <- exp(-d * 0.5 * (zh_rate + zf_rate))
    
    fa <- seq(-7.5, 10, length = 1e4)
    d_fa <- dnorm(fa)
    
    hr <- seq(-7.5, 10, length = 1e4)
    d_hr <- dnorm(hr, mean = d, sd = 1)
    
    custom_theme <- theme_bw() %+replace%
      theme(legend.position = "bottom")
    
    ggplot() +
      geom_line(aes(hr, d_hr, colour = "dark green"), size = 1) +
      geom_line(aes(fa, d_fa, colour = "dark red"), size = 1) +
      scale_x_continuous(limits = c(-7.5,10)) +
      geom_vline(aes(xintercept = beta), linetype = "dashed") +
      labs(title = input$plot_title,
           subtitle = input$plot_subtitle,
           x = "",
           y = "Probability Density Function") +
      scale_color_manual(name = "Distribution:",
                         labels = c( "Signal + Noise", "Noise"),
                         values = c("dark green", "dark red")) +
      geom_text(aes(x = beta, label = paste("Beta = ", round(beta, 2)), y = 0.44, angle = 90, vjust = 1.2)) +
      theme_set(custom_theme) +
      ylim(0, 0.46)
    
    },
    
    height = 570)
  
  }  

# Run it
shinyApp(ui = ui, server = server)



##### References #####

# Barr D, DeBruine LM (2017) The Research Cycle. Available at: 
# https://rgup.gitlab.io/research_cycle/index.html [Accessed March 12, 2018]

# Black K. (2015) 10. Calculating p Values - R Tutorial. Available at: 
# http://www.cyclismo.org/tutorial/R/pValues.html#calculating-a-single-p-value-from-a-normal-distribution 
# [Accessed March 10, 2018].

# Chang, W., Cheng, J., Allaire, J., Xie, Y. and McPherson, J. (2017) shiny: Web Application Framework
# for R. R package version 1.0.5. https://CRAN.R-project.org/package=shiny

# DeBruine LM (n.d.) Face Research -Researchers -Stats -Signal Detection Analyser. Available at: 
# http://faceresearch.org/res/stats/sda [Accessed January 8, 2018].

# Dolinar S. (2014) Calculating Z-Scores using R. Available at: 
# http://stats.seandolinar.com/calculating-z-scores-with-r/ 
# [Accessed March 10, 2018].

# Wickham, H. (2017) tidyverse: Easily Install and Load the 'Tidyverse'. R package version 1.2.1.
# https://CRAN.R-project.org/package=tidyverse

# Lee K. (2013) SDT d prime calculation &amp; other tips. Available at: 
# http://www.kangleelab.com/sdt-d-prime-calculation---other-tips.html 
# [Accessed March 22, 2018].

# R Core Team (2018). R: A language and environment for statistical computing. R Foundation for Statistical
# Computing, Vienna, Austria. URL https://www.R-project.org/.

# TquanT (2017) Signal detection. Available at: 
# https://r.tquant.eu/GrazApps/Group7_SignalDetection/ 
# [Accessed March 22, 2018].
  
 # Wickens, T.D. (2001) Elementary Signal Detection Theory. Oxford University Press. New York.
