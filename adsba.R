library(shiny)
library(shinydashboard)        
library(nortest)
library(mvnormtest)
library(MASS)
library(cluster)

# PREAMBLE begins
# READ (FOR REPORTS) https://shiny.rstudio.com/articles/generating-reports.html

# PREAMBLE ends

ui <- fluidPage(
  
  navbarPage(title = "ADSBA",
             tabPanel("DataSets", 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose CSV File", accept=c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv')),
                          radioButtons("indata", "Choice:", choices = c("full", "cols")),
                          selectInput("cols", "Choose the variable", choices = "", selected = " ", multiple = TRUE), 
                          downloadButton('report')
                          ), 
                        mainPanel(tableOutput("tab1"))
                        )
             ), 
             
             navbarMenu("Descriptive Data Analysis",
                        tabPanel("Summary Statistics",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("cols1", "Choose Variable:", choices = "", selected = " ", multiple = TRUE)
                              
                            ), 
                            mainPanel(
                              fluidRow(
                                p("Summary Statistics"),
                                div(
                                  verbatimTextOutput("summar")
                                )
                              )
                            )
                          )
                        ), 
                        tabPanel("Frequency Tables",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("cols2", "Choose Varibale 1:", choices = "", selected = " ", multiple = TRUE),
                              selectInput("cols3", "Choose Varibale 2:", choices = "", selected = " ", multiple = TRUE)
                            ), 
                            mainPanel(
                              fluidRow(
                                p("Frequency Tables"),
                                div(
                                  verbatimTextOutput("freq_tables")
                                )
                              )
                            )
                          )
                          
                        ), 
                        
                        tabPanel("Cross Tabulation",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols4", "Choose Varibale 1:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols5", "Choose Varibale 2:", choices = "", selected = " ", multiple = TRUE),
                                     hr(),
                                     helpText("For details visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Chi-squared_test", "Karl Pearson Chisquare Test"),
                                     hr()
                                   ), 
                                   mainPanel(
                                     fluidRow(
                                       h3("Chisquare Test"),
                                        verbatimTextOutput("chi_t")
                                  )
                                   )
                                 )
                                 
                        ),
                        tabPanel("Plots",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("plotoption", "Choose the Option:", choices = c("Histogram", "BarPlot", "Scatter", "Pie" )),
                                     selectInput("cols6", "Choose Varibale 1:", choices = "", selected = " ", multiple = TRUE),
                                     textInput("xaxisname", "Write X Axis Name"),
                                     textInput("yaxisname", "Write Y Axis Name"),
                                     textInput("title", "Write Title For the Graph")
                                   ), 
                                   mainPanel(
                                     fluidRow(
                                       plotOutput("plot")
                                       )
                                   )
                                 )
                                 
                        )
                      ),
             
             navbarMenu("Predictive Data Analysis",
                        
                        tabPanel("Statistical Tests", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols7", "Choose Varibale 1:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols8", "Choose Varibale 2:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("normaltest", "Select Method:", choices = c("A-DTest", "Shapiro", "KS", "MV-Shapiro")),
                                     hr(),
                                     helpText("For more details visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Anderson%E2%80%93Darling_test", "Anderson–Darling test"), br(),
                                     a(href="https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test", "Shapiro–Wilk test"), br(),
                                     a(href="https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test", "Kolmogorov–Smirnov test"), br(),
                                     
                                     hr()
                                   ), 
                                   mainPanel(
                                     fluidRow(
                                      div(
                                        plotOutput("qqp")
                                      ),
                                      div(
                                        verbatimTextOutput("normtest")
                                      )
                                     )
                                   )
                                   
                                 )
                                 
                        ),
                        
                        tabPanel("Correlation", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols9", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols10", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("cormethod", "Select Method:", choices = c("KarlPearson", "Spearman", "Kendals")),
                                     hr(),
                                     helpText("For Details Visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient", "Karl Pearson Correlation Test"),
                                     hr()
                                   ), 
                                   mainPanel(
                                     h3("Correlation"),
                                     verbatimTextOutput("cor_t")
                                   )
                                   
                                 )
                                 
                        ),
                        tabPanel("Regression & ANOVA", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols11", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols12", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("regmethod", "Select Method:", choices = c("Fit", "Summary", "ANOVA")), 
                                     hr(),
                                     helpText("For more information please visit"),
                                     a(href="https://en.wikipedia.org/wiki/Simple_linear_regression", "Simple Linear Regression"),
                                     hr()
                                   ), 
                                   mainPanel(
                                     fluidRow(
                                       div(
                                         verbatimTextOutput("regout")
                                       ),
                                       div(
                                         plotOutput("regplot")
                                       )
                                     )
                                   )
                                   
                                 )
                                 
                        ),
                        
                        tabPanel("MANOVA", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols13", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols14", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("manmethod", "Choose Method:", choices = c("Fit", "Summary")),
                                     hr(),
                                     helpText("For more information please visit"),
                                     a(href="https://en.wikipedia.org/wiki/Multivariate_analysis_of_variance", "MANOVA"),
                                     hr(),
                                     helpText("Right now MANOVA supports only two dependent varibales"),
                                     hr()
                                   ), 
                                   mainPanel(
                                     fluidRow(
                                       div(
                                         verbatimTextOutput("manovaout")
                                       ),
                                       div(
                                         plotOutput("manovaplot")
                                       )
                                     )
                                   )
                                   
                                 )
                                 
                        )
                        
             ),         
            
             navbarMenu("Exploratory Data Analysis", 
                      tabPanel("Discriminant Analysis",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput('dafactvar', "Choose the Factor:", choices = "", selected = ""),
                            selectInput('daNumvar1', "Choose the Vector1:", choices = "", selected = ""),
                            selectInput('daNumvar2', "Choose the Vector2:", choices = "", selected = ""),
                            selectInput('daNumtvar3', "Choose the Vector3:", choices = "", selected = ""),
                            hr(), 
                            helpText("Supports only 3 vectors (Vector: varibale at right hand side of the equation). Implements only LDA at presents. For details visit:"),
                            a(href="https://en.wikipedia.org/wiki/Linear_discriminant_analysis", "Discriminant Analysis"),
                            hr()
                          ),
                          mainPanel(
                            fluidRow(
                              div(
                                verbatimTextOutput("daoutput")
                              ),
                              div(
                                plotOutput("daplot")
                              )
                            )
                          )
                        )
                        
                      ),
                      
                      tabPanel("Reliability Analysis",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("relalpha", "Choose the variables:", choices = "", selected = "", multiple = TRUE),
                                hr(),
                                helpText("For more details visit:"),
                                a(href="https://en.wikipedia.org/wiki/Cronbach%27s_alpha", "Cronbach's alpha"),
                                hr()
                                ),
                              mainPanel(
                                div(
                                  verbatimTextOutput("reloutput")
                                )
                              )
                            )
                            
                      ),
                      
                      tabPanel("Factor Analysis",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput("fadataset", "Choose the Variables:", choices = "", selected = "", multiple = TRUE),
                                   textInput("nf", "Mention No. of Factors", value = "2", width = NULL, placeholder = "Write here number of factors"), 
                                   hr(),
                                   helpText("For details visit:"),
                                   a(href="https://en.wikipedia.org/wiki/Factor_analysis", "Factor Analysis"),
                                   hr()
                                 ),
                                 mainPanel(
                                   div(
                                     verbatimTextOutput("faoutput")
                                   ), 
                                   div(
                                     plotOutput("faplot")
                                   )
                                 )
                               )
                               
                      ),
                      
                      tabPanel("Cluster Analysis",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput("cavars", "Choose Variables:", choices = "", selected = "", multiple = TRUE),
                                   textInput("nc", "Number of Clusters:", value = "2", placeholder = "Choose No. Clusters"), 
                                   radioButtons("showcl", "Show Individuals by Clusters:", choices = c("ShowClus", "NoClus")),
                                   hr(),
                                   helpText("For more details visit:"),
                                   a(href="https://en.wikipedia.org/wiki/Cluster_analysis", "Cluster analysis"),
                                   hr()
                                 ),
                                 mainPanel(
                                   div(
                                     verbatimTextOutput("caoutput")
                                   ), 
                                   div(
                                     plotOutput("caplot")
                                   )
                                 )
                               )
                      )
             ), 
             
             # tabPanel("REPORTS", 
             #          sidebarLayout(
             #            sidebarPanel("Reports"), 
             #            mainPanel(tableOutput("pdfview"))
             #          )
             # ),        
             
             tabPanel("Contact", 
                      sidebarLayout(
                        sidebarPanel(
                          "Information to contact"
                        ), 
                        mainPanel(htmlOutput("text1"))
                      )
              )
          )
      ) 



server <- function(input, output, session) {
  
  # for DATASET TAB
  
  data_input <- reactive({
    infile <- input$file1
    req(infile)
    data.frame(read.csv(infile$datapath)) 
  })
  
  observeEvent(input$file1,{
    updateSelectInput(session,
                        inputId = "cols",
                        choices = names(data_input()))
    }
  )
  
  
 output$tab1 <- renderTable(
   {
     df <- data_input()
     if (input$indata == "full"){
       print(df)
     } else {
       print(df[input$cols])
     }
     
    }
 )

# summary tab
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols1", choices = names(data_input()))
   }
 )
 
 summ <- reactive({
   var1 <- data_input()[,input$cols1]
 
   su <- summary(var1)
   return(su)
 })
 
 output$summar <- renderPrint({
   summ()
   })
 
 # frequency tab
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols2", choices = names(data_input()))
   updateSelectInput(session, inputId = "cols3", choices = names(data_input()))
 }
 )
 
 freq <- reactive({
   var1 <- data_input()[,input$cols2]
   var2 <- data_input()[,input$cols3]
   fre <- table(var1, var2)
   return(fre)
 })
 
 output$freq_tables <- renderPrint({
   freq()
 })
 
 # Cross tabulation
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols4", choices = names(data_input()))
   updateSelectInput(session, inputId = "cols5", choices = names(data_input()))
 }
 )

cross <- reactive({
   var1 <- data_input()[,input$cols4]
   var2 <- data_input()[,input$cols5]
   
   cro <- chisq.test(var1, var2)
   return(cro)
 })
 
 output$chi_t <- renderPrint({
   cross()
   
 })
 
 # Plots 
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols6", choices = names(data_input()))
  }
 )
 
 output$plot <- renderPlot({
   df <- data_input()
   if(input$plotoption == "Histogram"){
     hist(df[, input$cols6], freq = FALSE, xlab = input$xaxisname, ylab = input$yaxisname, main = input$title); lines(density(df[, input$cols6]), col = "red", lwd = 1.5)
   } else if(input$plotoption == "BarPlot"){
     barplot(df[, input$cols6], xlab = input$xaxisname, ylab = input$yaxisname, main = input$title)
   } else if(input$plotoption == "Scatter"){
     scatter.smooth(df[, input$cols6], xlab = input$xaxisname, ylab = input$yaxisname, main = input$title)
   } else {
     pie(table(df[, input$cols6]))
   }
 })
 
 # Statistical Tests
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols7", choices = names(data_input()))
   updateSelectInput(session, inputId = "cols8", choices = names(data_input()))
 }
 )
 
 output$qqp <- renderPlot({
   df <- data_input()
   qqnorm(df[, input$cols7]);qqline(df[, input$cols7])
 })
 
 adt <- reactive({
   df <- data_input()
   var <- df[, input$cols7]
   ad <- ad.test(var)
   return(ad)
 })
 
 sht <- reactive({
   df <- data_input()
   var <- df[, input$cols7]
   sh <- shapiro.test(var)
   return(sh)
 })
 
 kst <- reactive({
   df <- data_input()
   var1 <- df[, input$cols7]
   var2 <- df[, input$cols8]
   ks <- ks.test(var1, var2)
   return(ks)
 })
 
 mvst <- reactive({
   df <- data_input()
   var1 <- df[, input$cols7]
   var2 <- df[, input$cols8]
   return(mshapiro.test(t(as.data.frame(var1, var2))))
 })
 
 output$normtest <- renderPrint({
   
   if(input$normaltest == "A-DTest"){
       print(adt())
   } else if(input$normaltest == "Shapiro"){
     print(sht())
   } else if(input$normaltest == "KS"){
     print(kst())
   } else if(input$normaltest == "MV-Shapiro"){
     print(mvst())
   }
   
   }
 
 )
 # correlation & regression 
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols9", choices = names(data_input()))
   updateSelectInput(session, inputId = "cols10", choices = names(data_input()))
 }
 )
 
 cortest <- reactive({
   var1 <- data_input()[,input$cols9]
   var2 <- data_input()[,input$cols10]
   if (input$cormethod == "KarlPearson"){
     return(cor.test(var1, var2, method = "pearson"))
   } else if(input$cormethod == "Spearman"){
     return(cor.test(var1, var2, method="spearman"))
   } else {
     return(cor.test(var1, var2, method="kendall"))
   }
  }
  )

 output$cor_t <- renderPrint({
   cortest()
 })
 
 # Regression
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols11", choices = names(data_input()))
   updateSelectInput(session, inputId = "cols12", choices = names(data_input()))
 }
 )
 
 reganal <- reactive({
   df <- data_input()
   var1 <- df[, input$cols11]
   var2 <- df[, input$cols12]
   rego <- lm(var1 ~ var2, data = df)
   return(list(fit = rego, fitsum = summary(rego), anov = anova(rego)))
   
 })
 
 output$regout <- renderPrint({
   if (input$regmethod == "Fit"){
     reganal()$fit
   } else if(input$regmethod == "Summary"){
     reganal()$fitsum
   } else if(input$regmethod == "ANOVA"){
     reganal()$anov
   }
 })
 
 output$regplot <- renderPlot({
   df <- data_input()
   var1 <- df[, input$cols11]
   var2 <- df[, input$cols12]
   plot(var1, var2); abline(lm(var1 ~ var2, data = df), col = "red", lwd=2)
 })
 
 # MANOVA
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cols13", choices = names(data_input()))
   updateSelectInput(session, inputId = "cols14", choices = names(data_input()))
 }
 )
 
 manovaform <- reactive({
   df <- data_input()
   #formula <- as.formula(paste(cbind(df[, input$cols13]), '~', df[, input$cols14])) 
   manform <- as.formula(paste("cbind(unlist(rbind(df[, input$cols13]))[1:length(df[, input$cols14])], unlist(rbind(df[, input$cols13]))[length(df[, input$cols14])+1:length(df[, input$cols14])*2])", "~", "df[, input$cols14]"))
   return(manform)
 })
 
 manovaanal <- reactive({
   df <- data_input()
   manout <- manova(manovaform(), data = df)
   return(manout)
 })
 
 output$manovaout <- renderPrint({
 if(input$manmethod=="Fit"){
   manovaanal()
 } else if(input$manmethod == "Summary"){
   summary(manovaanal())
 }
   
 })
 
output$manovaplot <- renderPlot({
  df <- data_input()
  var1 <- df[, input$cols13]
  var2 <- df[, input$cols14]
  plot(data.frame(var1, var2))
}) 
 
 
# Exploratory 
 # Disctiminant Analysi s
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "dafactvar", choices = names(data_input()))
   updateSelectInput(session, inputId = "daNumvar1", choices = names(data_input()))
   updateSelectInput(session, inputId = "daNumvar2", choices = names(data_input()))
   updateSelectInput(session, inputId = "daNumtvar3", choices = names(data_input()))
 }
 )
 
 daout <- reactive({
   df <- data_input()
   var1 <- df[, input$dafactvar]
   var2 <- df[, input$daNumvar1]
   var3 <- df[, input$daNumvar2]
   var4 <- df[, input$daNumtvar3]
   daformula <- as.formula(paste("var1", "~", "var2", "+", "var3", "+", "var4"))
   fit <- lda(daformula, data = df)
   return(fit)
   
 })
 
 output$daoutput <- renderPrint({
   daout()
 })
 
 output$daplot <- renderPlot({
   plot(daout(), dimen=1, type="both")
 })
 
 # Reliability analysis 
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "relalpha", choices = names(data_input()))
  }
 )
 
 relout <- reactive({
   df <- data_input()
   out <- alpha(df[, input$relalpha])
   return(out)
   
 })

 output$reloutput <- renderPrint({
   relout()
 })
 
 # FActor analysis 
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "fadataset", choices = names(data_input()))
 }
 )
 
 faout <- reactive({
   df <- data_input()
   # out <- factanal(matrix(unlist(list(df[, input$fadataset])), dim(df)[1], length(input$fadataset)), input$nf)
   out <- fa(df[, input$fadataset], input$nf)
   return(out)
 })
 
 output$faoutput <- renderPrint({
   faout()
 })

 output$faplot <- renderPlot({
   plot(faout())
 })
 
 # Cluster analysis 
 
 observeEvent(input$file1, {
   updateSelectInput(session, inputId = "cavars", choices = names(data_input()))
 }
 )
 
 caout <- reactive({
   df <- data_input()
   out <- kmeans(df[,input$cavars], input$nc)
   return(out)
 })
 
 output$caoutput <- renderPrint({
   df <- data_input()
   if(input$showcl == "NoClus"){
     caout()
   } else if(input$showcl == "ShowClus"){
     out <- cbind(1:dim(df)[1], caout()$cluster)
     colnames(out) <- c("individuals", "Cluster")
     print(out)
   }
   
 })
 
 output$caplot <- renderPlot({
   df <- data_input()
   clusplot(df, caout()$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
 })
 
 
 # Contact Information 
 
 output$text1 <- renderText({
   str1 <- paste("Dr. M. Kamakshaiah") 
   str2 <- paste("kamakshaiah.m@gmail.com") 
   str3 <- paste("+919177573730")
   #str4 <- paste("166, Vayushakti Nagar, Dammaiguda, Hyderabad, Telangana State, India 500083")
   HTML(paste(str1, str2, str3, sep = '<br/>'))
 })
  
}



shinyApp(ui, server)
