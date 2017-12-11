#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#----------------
#below are gloabal variables and settings
library(shiny)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(scales)
library(RColorBrewer)
#-----
#content in global.R
dataset <- readRDS("dataset.RData")

#----------------
#start value
select_start_data <- 4
select_end_month <- 6
threshold_i <- 0.08
#------------------------
#total

total_numbers <- dataset %>%
    filter(month(issue_d) %in% c(select_start_data:select_end_month)) %>%
    summarise(sum_loan = sum(loan_amnt),num_loan = n())

total_numbers_display <- total_numbers %>%
    transmute(`Total Loan Amount` = str_c("$",round(sum_loan/1000000),"M"),
              `Total Number of Loan` = format(num_loan,big.mark = ","))
#text_out_put <- tibble(a = colnames(total_numbers_display),b = total_numbers_display[1,])
total_text <- ggplot(data = tibble(x = c(0,1),y = c(0,1))) +
    geom_text(mapping = aes(x = x ,y = y,label = str_c(colnames(total_numbers_display),as.character(total_numbers_display),sep = ":\n")),
              size = 10,
              data = tibble(x = 0.5,y =c(0.4,0.7)))+
    coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
    theme_void()


#--------------------------------
#status bar YTD
YTD_data <- dataset %>%
    filter(month(issue_d) %in% c(select_start_data:select_end_month)) %>%
    group_by(loan_status) %>%
    summarise(num_loan = n()) %>%
    mutate(pct_loan = num_loan/sum(num_loan)) 

YTD_data_disply <- YTD_data %>%
    mutate(pct_dispaly = str_c(round(pct_loan*100,2),"%"))

#---------------------
#line chart data
track_plot_data <- dataset %>%
    filter(month(issue_d) %in% c(select_start_data:select_end_month)) %>%
    group_by(issue_d,loan_status) %>%
    summarise(n_notes = n()) %>%
    mutate(ttl_notes = sum(n_notes),
           pct = n_notes/ttl_notes,
           plot_date = ymd(str_replace(issue_d,"[1-9]{2}$","01"))) %>%
    ungroup()
#----------------------
#track bar chart
plot_data <- dataset %>%
    filter(month(issue_d) %in% c(select_start_data:select_end_month)) %>%
    group_by(issue_d,loan_status) %>%
    summarise(num_notes = n()) %>%
    filter(loan_status == "Fully Paid" | loan_status == "Charged Off") %>%
    spread(key = loan_status, value = num_notes) %>%
    ungroup() %>%
    mutate(ratio = .[[3]]/.[[2]],
           type = ifelse(ratio < threshold_i, "good", "bad"),
           plot_date = ymd(str_replace(issue_d,"[1-9]{2}$","01"))) 

track_bar_app <- plot_data %>%
    ggplot(mapping = aes(x = plot_date, y = ratio)) +
    geom_col(mapping = aes(fill = type)) +
    #aesthetics below
    scale_y_continuous(labels = percent) +
    theme_light() +
    theme(legend.position="none") +
    labs(x =  "", y = "", title = "Monthly charge-off/fully-paid ratio") +
    scale_x_date(date_labels = "%b %Y",date_breaks = "1 month") +
    #add extra info
    geom_hline(yintercept = threshold_i, size = 1) +
    geom_text(mapping = aes(x = min(plot_data$plot_date), y = threshold_i - 0.002,label = str_c("threshold: ",threshold_i*100,"%")),hjust = 0.8) +
    geom_text(mapping = aes(y = ratio + 0.002,label = str_c(signif(ratio*100,digits = 2),"%"))) +
    scale_fill_manual(values = c(good = "skyblue3", bad = "firebrick3")) 
#------------------------------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Lending Club Dashboard"),
   # Sidebar with a slider input for number of bins 
   fluidRow(
       column(4,align="center",
              plotOutput("table")),
       column(8,
              selectInput(inputId = "select_display",
                          label = "Display Method:",
                          choices = c("percent","count"),
                          selected = "count"),
              plotOutput("status_bar"))
   ),
   fluidRow(
       column(5,
              plotOutput("loan_line_chart")),
       column(2,
              checkboxGroupInput(inputId = "tracked_type",
                                 label = "Displayed Type(s):",
                                 choices = c("Fully Paid","Current","In Grace Period","Late (16-30 days)","Late (31-120 days)","Charged Off"),
                                 selected = c("Fully Paid","Current"))),
       column(5,
              plotOutput("track_bar"))
   )
)
#-------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {
    #--------------------------------------------
    #total num output
    output$table <- renderPlot({total_text})
    
    #----------------------------------
    #status bar YTD
    YTD_display_method <- reactive({input$select_display})
    output$status_bar <- renderPlot({
        if(YTD_display_method() == "count"){
            YTD_data_disply %>% 
                ggplot(mapping = aes(x = loan_status,y = num_loan)) +
                geom_col(fill = "skyblue3") +
                #aethetics
                theme_light() +
                scale_y_continuous(labels = comma) +
                #coord_flip() +
                #theme(axis.text.x = element_text(size = rel(1.2),angle = 30, hjust = 1)) +
                #extra info
                geom_text(mapping = aes(label = format(num_loan,big.mark = ",")),vjust = -0.3) +
                labs(x = "Loan Status", y = "Number of Notes")
        } else if(YTD_display_method() == "percent"){
            YTD_data_disply %>% 
                ggplot(mapping = aes(x = loan_status,y = pct_loan)) +
                geom_col(fill = "skyblue3") +
                #aethetics
                theme_light() +
                scale_y_continuous(labels = percent) +
                #coord_flip() +
                #theme(axis.text.x = element_text(size = rel(1.2),angle = 30, hjust = 1)) +
                #extra info
                geom_text(mapping = aes(label = pct_dispaly,vjust = -0.3)) +
                labs(x = "Loan Status", y = "Percentage")
        }
    })
    #---------------------------
    #line chart
    tracked_loan_type <- reactive({input$tracked_type})
    track_plot_data_specific <- reactive({track_plot_data %>%
        filter(loan_status %in% tracked_loan_type())})
    
    output$loan_line_chart <- renderPlot({
        track_plot_data_specific() %>% 
            ggplot(mapping = aes(x = plot_date, y = n_notes)) +
            geom_line(mapping = aes(color = loan_status),size = 1) +
            geom_point() +
            #aesthetics
            scale_x_date(date_labels = "%b %Y",date_breaks = "1 month") +
            scale_y_continuous(labels = comma) +
            theme_light() +
            guides(color = guide_legend(title="Loan Status")) +
            scale_colour_brewer(palette = "Set1") +
            theme(legend.position="top") +
            #layer of info
            geom_text(mapping = aes(y = n_notes,label = format(n_notes,big.mark = ",")),vjust="inward",hjust="inward") +
            labs(x = "Month", y = "Number of Notes")
    })
    #--------------------------------
    #track bar chart
    output$track_bar <- renderPlot({
        track_bar_app
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

