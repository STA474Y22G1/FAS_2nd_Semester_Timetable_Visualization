## app.R ##

## Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(DT)
library(randomcoloR)
library(readr)
library(hrbrthemes)
library(viridis)


## Data sets

##Data for Overview Tab
# Bar Plot
overview_data1 <- read.csv("Data/overview_data1.csv")
overview_data1$Day <- ordered(overview_data1$Day, c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

# Line Plot
overview_data2 <- read_csv("Data/overview_data2.csv")
overview_data2$Day <- factor(overview_data2$Day, levels = c("Monday", "Tuesday", "Wednesday", 
                                                          "Thursday", "Friday", "Saturday", "Sunday")) 

# KPIs
academic_kpi_data <- read.csv("Data/academic_kpi_data.csv")


## Data for Course view Tab
courseViewData <- read_csv("Data/courseViewData.csv")


## Data for Lecturer view Tab
#Plot 1 Data
lecturer_data<-read.csv("Data/lecturer_data.csv") %>% rename(`Lecturer in Charge`=Lecturer.in.Charge)

lecturer_data$Starting.Time<-substring(lecturer_data$Starting.Time, 1,5)
lecturer_data$Ending.Time<-substring(lecturer_data$Ending.Time, 1,5)
lecturer_data$Starting.time <- as.POSIXct(lecturer_data$Starting.Time, format = "%H:%M")
lecturer_data$Ending.time<- as.POSIXct(lecturer_data$Ending.Time, format = "%H:%M")

#Plot 2 Data
lecturer_stat_data<-read_csv("Data/lecturer_stat_data.csv")


## Data for Lecture Hall View Tab
availabilty_data <- read_csv("Data/availability_data.csv")
lecture_hall_data <- read_csv("Data/lecture_hall_data.csv")
# Lecture halls character vector
lecture_halls_names <- unique(availabilty_data$Location)
availabilty_data$Day <- ordered(availabilty_data$Day,
                                c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))


# to remove select all option in picker Input function 
my_css <- "
.bs-select-all {
  display: none;
}
.bs-deselect-all {
  width: 100%;
}
"
# Creating the color palette
set.seed(34) # Set random seed
color_palette <- distinctColorPalette(34)
color_palette



# User Interface
ui <- dashboardPage(skin="purple",
  dashboardHeader(title="FAS Timetable" ),
  dashboardSidebar(
    sidebarMenu(id = "sidebarid",
      menuItem("Overview", tabName = "Overview",
               icon = icon(name = "eye-open", lib="glyphicon")),
      conditionalPanel(
        'input.sidebarid == "Overview"',
        selectInput("option", label = h5("Select Department"),
                    choices = sort(unique(overview_data1$Department)))),
     
      
      menuItem("Course View", tabName = "course_view",
               icon = icon(name = "book", lib="glyphicon")),
      
      menuItem("Lecturer View", tabName = "Lecturer_View",
               icon = icon(name = "user", lib="glyphicon")),
      
      menuItem("Lecture Hall View", tabName = "Lecture_Hall_View",
               icon = icon(name = "home", lib="glyphicon")),
      
      menuItem("About", tabName = "About",
               icon = icon(name = "search", lib="glyphicon"))
      
    )),
  dashboardBody(tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Calibri", Times, "Calibri", serif;
        font-weight: bold;
        font-size: 28px;
      }
    '))),
    tabItems(
      # Overview tab
      tabItem(tabName = "Overview", 
             fluidRow( 
                #kp1
                valueBoxOutput("overview_kpi_1", width=3),
                
                #kpi2
                valueBoxOutput("overview_kpi_2", width=3),
                
                #kpi3
                valueBoxOutput("overview_kpi_3", width=3), 
                
                #kpi4
                valueBoxOutput("overview_kpi_4", width=3)),
              
              fluidRow(
                (box(plotlyOutput("overview_plot1", height=450),width=6)),
                box(plotlyOutput("overview_plot2", height=450),width=6))
      ),
      
      # Course view tab
      tabItem(tabName = "course_view",
              fluidRow(
                box(width = 3,
                    height = 300,
                    
                    ## user input 1 degree
                    selectInput(
                      inputId = "Degree.Type",
                      label = h4("Select Degree"), 
                      choices = unique(courseViewData$Degree.Type)),
                    
                    ## user input 2 year
                    selectInput(
                      inputId = "Academic.Year",
                      label = h4("Select Academic Year"), 
                      choices = NULL),
                    
                    ## user input 3 subject
                    tags$head(tags$style(HTML(my_css))),
                    
                    pickerInput(
                      inputId = "Subject.Code",
                      label = h4("Select Subject/s"), 
                      choices = NULL,
                      options = list(`actions-box` = TRUE, size = 5),
                      multiple = TRUE
                    )
                    
                ),
                
                # 1st visualization
                box(plotlyOutput("course_view_plot1", height = 270), width = 9, height = 300),
                
                # Course finder table
                box(dataTableOutput("CourseData"), width = 12)),
              
              
      ),
      
      # Lecturer view tab
      tabItem(tabName="Lecturer_View",
              fluidRow(
                (box(width=3,height=220,
                     selectInput("Day", label = h4("Select Day (Only for Lecturer Availability)"),
                                 choices = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
                     selectInput("Department", label = h4("Select Department"),
                                 choices = sort(unique(lecturer_data$Department))))),
                box(plotlyOutput("lec.view_plot1", height = 200),width=9,height=220)
                
              ),
              fluidRow(
                box(plotlyOutput("lec.view_plot2", height = 380), width = 12)
              )),
      
      
      
      # Lecture hall view tab
      tabItem(tabName = "Lecture_Hall_View",
              # Treemap 
              box(plotlyOutput("hall_view_plot1", height = 573)),
              
              fluidRow(
                box(height = 100,
                    selectInput("opt", label = h4("Select Lecture Hall"),
                                choices = lecture_halls_names)
                ),
                
                # KPI 1  
                # Number of courses per week
                valueBoxOutput("hall_view_kpi1", width = 3),
                
                # KPI 2
                valueBoxOutput("hall_view_kpi2", width = 3),
                
                # Heatmap
                box(plotOutput("hall_view_plot2", height = 355, width = 470))
        )
      ),
      ##About tab
      tabItem(tabName = "About",
              tags$head(
        tags$style(HTML("
            code {
                display:block;
                padding:9.5px;
                margin:0 0 15px;
                margin-top:10px;
                font-size:20px;
                line-height:20px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#F5F5F5;
                border:1px solid rgba(0,0,0,0.15);
                border-radius:4px; 
                font-family:monospace;
            }"))),
     
        h4(strong("Created by")),
        p("- Sadrushi Dissanayake"),
        p("- Thisaakhya Jayakody"),
        p("- Lakna Perera"),
        p("- Menasha Senanayaka"),
        p("- Kalani Siriwardena"),
        p("- Trishika Wickramarathne"),
        p("Under the supervision of",
            tags$a(href="https://thiyanga.netlify.app/","Dr. Thiyanga S. Talagala.")),
            h4(strong("Code")),
            p("The code used to construct this dashboard can be found on",tags$a(href="https://github.com/STA474Y22G1/FAS_2nd_Semester_Timetable_Visualization", "GitHub.")),
            h4(strong("Data")),
            p("Data for this dashboard was collected from the 12 departments operating under the Faculty of Applied Sciences of the University of Sri Jayewardenepura."),
            h4(strong("Update")),
            p("The data used is as per the second semester timetables finalized as of 23rd December 2022 by the Dean's office.")
          
        )
      ))) 
    
  


# Server Function
server <- function(input, output, session) {
  
  ## Overview Tab
  
  # KPIs
  # KPI 1
  # Number of lectures for 1st year
  output$overview_kpi_1 <- renderValueBox({
    option_kpi1 <- academic_kpi_data %>%
      filter(Department == input$option & Academic.Year == 1) 
    
    valueBox(value = option_kpi1$Course.Count,
             subtitle = "Courses offered in 1st Year",
             color = "light-blue")
  })
  
  
  # KPI 2
  # Number of lectures for 2nd year
  output$overview_kpi_2 <- renderValueBox({
    option_kpi2 <- academic_kpi_data %>%
      filter(Department == input$option & Academic.Year == 2) 
    
    valueBox(value = option_kpi2$Course.Count,
             subtitle = "Courses offered in 2nd Year",
             color = "light-blue")
  })
  
  # KPI 3
  # Number of lectures for 3rd year
  output$overview_kpi_3 <- renderValueBox({
    option_kpi3 <- academic_kpi_data %>%
      filter(Department == input$option & Academic.Year == 3) 
    
    valueBox(value = option_kpi3$Course.Count,
             subtitle = "Courses offered in 3rd Year",
             color = "light-blue")
  })
  
  # KPI 4
  # Number of lectures for 4th year
  output$overview_kpi_4 <- renderValueBox({
    option_kpi4 <- academic_kpi_data %>%
      filter(Department == input$option & Academic.Year == 4) 
    
    valueBox(value = option_kpi4$Course.Count,
             subtitle = "Courses offered in 4th Year",
             color = "light-blue")
  })
  
  # Bar Plot
  output$overview_plot1<- renderPlotly({
    overview_data1 %>% filter(Department==input$option) %>%
      plot_ly(x = ~Day, y = ~Total.Number.of.Lectures, type = 'bar',
              marker = list(color = '#CF1A7A')) %>%
      layout(
        title= "Distribution of Lectures by Day",
        xaxis = list(title = "Day",tickangle=-45,categoryorder = "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") ,
        yaxis = list(title = "Total Number of Lectures"),font = list(color = 'black')
      )
  })
  
  # Line Plot
  output$overview_plot2<- renderPlotly({
    overview_data2%>%filter(Department==input$option) %>% group_by(Day) %>%
      plot_ly(x = ~`Time Slot`, y = ~`Total Number of Lectures`, 
              type = 'scatter', color = ~Day, mode="lines+markers") %>%
      layout(title = "Number of Lectures by Time Slot",
             xaxis = list(title = "Time Slot", tickangle = -45),
             yaxis = list(title = "Number of Lectures"), 
             hovermode = "x unified",font = list(color = 'black'))
  })
  
  
  ## Course View Tab
  
  # updating filters
  degree <- reactive({
    req(input$Degree.Type)
    filter(courseViewData, Degree.Type == input$Degree.Type)
  })
  
  year <- reactive({
    req(input$Academic.Year)
    filter(degree(), Academic.Year == input$Academic.Year)
  })
  
  subject <- reactive({
    
    # message to display when subject not selected
    validate(need(input$Subject.Code != "", "Please Select a Subject/s"))
    
    req(input$Subject.Code)
    filter(year(), Subject.Code %in% input$Subject.Code)
  })
  
  # observing event to update next filter
  observeEvent(degree(), {
    updateSelectInput(session, "Academic.Year", 
                      choices = sort(unique(degree()$Academic.Year)), selected = 1)
  })
  
  observeEvent(year(), {
    updatePickerInput(session, "Subject.Code", 
                      choices = sort(unique(year()$Subject.Code)), selected = c("STA","FST"))
  })
  
  # course view Plot1
      output$course_view_plot1 <- renderPlotly({
    
      plot1 <- subject() %>% ggplot(aes(label1 = Course, 
                                      label2 = Lecture.Time)) + 
      geom_linerange(aes(x = Starting.Time, xmin = Starting.Time, 
                         xmax = Ending.Time, y = Day, color = Subject.Code), 
                     linewidth = 2, position = position_dodge(0.5)) +
      geom_point(aes(Starting.Time, Day, color = Subject.Code), position = position_dodge(0.5)) +
      geom_point(aes(Ending.Time, Day, color = Subject.Code), position = position_dodge(0.5)) +
      scale_x_datetime(name = "Time", date_labels = "%H:%M", date_breaks = "1 hour") +
      theme_bw() +
      labs(title = "Lecture Times", color = "Subject") +
      theme(axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = 0.5)) 
    
    # interactive plot
    ggplotly(plot1, tooltip = c("label1", "label2"))
    
  })
  
  # course finder
  # Data set for course finder
  course <- reactive({
    subject() %>% filter(Lecture.Type == "Lecture") %>% 
      select(Department, Stream, Day, Lecture.Time,  Course.Code, Course.Title, 
             Lecturer.in.charge, Location) 
  })
  
  
  # Course finder table
  output$CourseData <- renderDataTable(course(),
                                       options = list(paging = TRUE,
                                                      pageLength = 5,
                                                      dom = 'ftip'),
                                       colnames = c('Lecture Time' = 'Lecture.Time',
                                                    'Course Code' = 'Course.Code',
                                                    'Course Title' = 'Course.Title',
                                                    'Lecturer in Charge' = 'Lecturer.in.charge'),
                                       caption = htmltools::tags$caption(
                                         style = 'caption-side: top; text-align: Center; font-style: normal; 
                                         color: black; font-family: arial; font-size: 1.8rem;',
                                         htmltools::em('Course Finder'))
  )
  
  
  ## Lecturer View Tab
  
  data <- reactive({
    filter(lecturer_data,
           Department == input$Department,
           Day == input$Day)
  })
  
  # Plot 1
  output$lec.view_plot1<- renderPlotly({
    plot_ly(data() , color = I("gray58")) %>%
      add_segments(x = ~Starting.time, xend = ~Ending.time, y = ~`Lecturer in Charge`, yend = ~`Lecturer in Charge`, showlegend = FALSE) %>%
      add_markers(x = ~Starting.time, y = ~`Lecturer in Charge`, name = "Starting Time", color = I("violetred1"),
                  hovertemplate=paste("Course :", data()$Course,
                                      "<br> Starting Time :", data()$Starting.Time,
                                      "<br> Location :" , data()$Location)) %>%
      add_markers(x = ~Ending.time, y = ~`Lecturer in Charge`, name = "Ending Time", color = I("dodgerblue3"),
                  hovertemplate=paste("Course :", data()$Course,
                                      "<br> Ending Time :", data()$Ending.Time,
                                      "<br> Location :" , data()$Location)) %>%
      layout(
        title = "Lecturer Availability",
        xaxis = list(title = "Lecture Time",tickangle=-45,categoryorder = "category ascending", type = 'date',tickformat = "%H:%M ",
                     rangebreaks=
                       list(bounds=list(18, 8),
                            pattern="hour"),dtick=60*60*1000),
        yaxis = list(title = "Lecturer in Charge"),
        margin = list(l = 65), legend=list(x =0.95, y= 0.95),font = list(color = 'black')
      )
  })
  
  # Plot 2
  output$lec.view_plot2 <- renderPlotly({
    lecturer_stat_data %>% filter(Department==input$Department) %>%
      plot_ly(x = ~`Lecturer in Charge`, y = ~`Total Number of Courses`, name = 'Courses', type = 'scatter', mode = 'lines+markers', line = list(color ="#FF1764"), marker = list(color ="#FF1764")) %>%
      add_trace(y = ~`Total Lecture Hours`, name = 'Lecture Hours', mode = 'lines+markers', line = list(color ="#51f1e3"), marker = list(color ="#51f1e3")) %>%
      layout(hovermode = "x unified") %>%
      layout(legend = list (x =0.95, y= 0.95, title=list(text='Total Number of '))) %>%
      layout(title=list(text="Lecturer Drivers"),
             xaxis = list(title = "Lecturer", tickangle=-45) ,
             yaxis = list(title = "Total Count"),font = list(color = 'black'))
  })
  
  
  ## Lecture Hall View Tab
  
  # Treemap
  output$hall_view_plot1 <- renderPlotly({
      plot_ly(
      data = lecture_hall_data,
      type="treemap",
      source = "treemapplot",
      labels = ~ `Lecture Hall`,
      parents = ~ "",
      values = ~ `Seating Capacity`,
      domain = list(column = 0),
      textinfo = "label+value",
      colors = color_palette) %>%
      layout(title = "Lecture Hall Capacity",
             font = list(color = 'black'), margin=list(l=0, r=0, b=0, t=40))
      
  })
  
  # Heatmap
  output$hall_view_plot2 <- renderPlot({
    
    availabilty_data %>%
      filter(Location == input$opt) %>%
      mutate(Day = as.factor(Day)) %>%
      ggplot(aes(TimeSlot, Day, fill = Availability)) +
      geom_tile(color = "white", lwd = 1.0, linetype = 1) + 
      coord_fixed() +
      scale_fill_continuous(breaks = 0:1, labels = c("Vacant", "Occupied")) +
      coord_fixed() +
      labs(title = "Lecture Hall Availability")+
      theme(plot.title = element_text(hjust = 0.5,size=18), axis.text.x = element_text(angle = 90)) 
  }) 
  
  # KPI 1
  # Number of courses per week
  output$hall_view_kpi1 <- renderValueBox({
    option_kpi1 <- lecture_hall_data %>%
      filter(`Lecture Hall` == input$opt) 
    
    valueBox(value = tags$p(paste0(option_kpi1$`Lecture count`), style = "font-size: 40%;"),
             subtitle = "Number of lectures per week",
             # color = "blue")
             color = "teal")
    
  })
  
  # KPI 2
  # Busiest Days of the Lecture Hall
  output$hall_view_kpi2 <- renderValueBox({
    option_kpi2 <- lecture_hall_data %>%
      filter(`Lecture Hall` == input$opt) 
    
    valueBox(value = tags$p(paste0(option_kpi2$`Bussiest Day`), style = "font-size: 40%;"),
             subtitle = "Busiest Day/s of the Lecture Hall",
             color = "olive")
    
  })
}

shinyApp(ui, server)
