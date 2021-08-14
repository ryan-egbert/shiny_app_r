##########################################
####   Shiny ui                       ####
##########################################
library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
library(markdown)

# ------------------
# Main title section
# ------------------

ui <- navbarPage(
  "College Major Recommender",
  theme = shinytheme("flatly"),
  tabPanel(
    "Home",
    # App title ----
    titlePanel(div(
      windowTitle = "College Major Recommendation",
      img(src = "bg.png", width="100%", class="bg"),
    )),
    
    tags$br(),
    
    ################################################
    #### Panel: Explore                         ####
    ################################################
                
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Explore",

        # ------------------
        # Major Category & Majors
        # ------------------

        sidebarLayout(
          sidebarPanel(
            h3("Majors in Major Category"),
            tags$br(),
            selectInput(
              inputId="majorcattable",
              label="Select a major category to view",
              choices = list("Agriculture & Natural Resources", "Arts", "Biology & Life Science",
                             "Business", "Communications & Journalism","Computer Technology","Education",
                             "Engineering","Health","Humanities & Liberal Arts","Industrial Arts & Consumer Services",
                             "Law & Public Policy","Mathematics","Physical Sciences","Psychology & Social Work","Social Science"),
              selected = "Agriculture & Natural Resourcces"
            )
          ),

          mainPanel(
            tableOutput("majorcat"),
            tags$br(),
            tags$br(),
          )
        ),
        tags$hr(),

        # ------------------
        # Salary Overview & Majors
        # ------------------

        sidebarLayout(
          sidebarPanel(

            h3("Salary Overview by Major"),
            tags$br(),
            setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
            sliderInput(
              inputId="SalaryRange",
              label = "Choose desired salary range",
              min = 35000,
              max = 125000,
              value = c(35000, 125000)
            ),

            actionButton(inputId="actionDT", label="Filter", class = "btn btn-warning"),
          ),
          mainPanel(
            h3("Browse All"),
            tags$br(),
            dataTableOutput("salaryTable"),
            tags$br(),
            tags$br(),
          )
        ),
        
        # ------------------
        # Salary by Employment Rate & Majors
        # ------------------
        
        sidebarLayout(
          sidebarPanel(
            h3("Median Salary and Employment Rate by Major"),
            tags$br(),
            radioButtons(
              inputId="majorcatradio",
              label = "Select Major Category",
              choices = list("Agriculture & Natural Resources", "Arts", "Biology & Life Science",
                             "Business", "Communications & Journalism","Computer Technology","Education",
                             "Engineering","Health","Humanities & Liberal Arts",
                             "Industrial Arts & Consumer Services","Law & Public Policy",
                             "Mathematics","Physical Sciences","Psychology & Social Work","Social Science"),
              selected = "Agriculture & Natural Resources"
            ),
            tags$hr()
            
          ),
          mainPanel(
            # h3("Average Salary and Employment Rate for Each Major"),
            plotlyOutput(outputId = "majorcatsalaryPlot", height=700),
            tags$br(),
            tags$br()
          )
        ),
        
        tags$hr(),
      ),
      
      
      ################################################
      #### Panel: Compare                         ####
      ################################################
      
      tabPanel(
        "Compare",
        
        # --------------------
        # density plot section
        # --------------------
        
        sidebarLayout(
          sidebarPanel(
            h3("Major Category Comparison"),
            tags$br(),
            selectInput(
              inputId="cat1",
              label = "Select two categories to compare:",
              choices = c(
                "Agriculture & Natural Resources",
                "Arts",
                "Biology & Life Science",
                "Business",
                "Communications & Journalism",
                "Computer Technology",
                "Education",
                "Engineering",
                "Health",
                "Humanities & Liberal Arts",
                "Industrial Arts & Consumer Services",
                "Law & Public Policy",
                "Mathematics",
                "Physical Sciences",
                "Psychology & Social Work",
                "Social Science"
              ),
              selected = "Agriculture & Natural Resources"
            ),
            selectInput(
              inputId="cat2",
              label = " ",
              choices = c(
                "Agriculture & Natural Resources",
                "Arts",
                "Biology & Life Science",
                "Business",
                "Communications & Journalism",
                "Computer Technology",
                "Education",
                "Engineering",
                "Health",
                "Humanities & Liberal Arts",
                "Industrial Arts & Consumer Services",
                "Law & Public Policy",
                "Mathematics",
                "Physical Sciences",
                "Psychology & Social Work",
                "Social Science"
              ),
              selected = "Agriculture & Natural Resources"
            ),
            actionButton(
              inputId="compare",
              label="Compare"
            )
          ),
          mainPanel(
            h3("Salary Comparison"),
            plotlyOutput(outputId = "salary"),
            tags$br(),
            h3("Employment Rate Comparison"),
            plotlyOutput(outputId = "employmentRate"),
            tags$br(),
            h3("Gender Comparison"),
            plotOutput(outputId = "gender")
          )
        ),
        tags$hr(),
      ),
      
      
      ################################################
      #### Panel: Recommend                       ####
      ################################################
      
      tabPanel(
        "Recommend",
        h3("College Major Recommendation", align = "center"),
        br(),
        div(style = "display:vertical-align:center;center-align",
            fluidRow(
              column(3,
                selectInput(
                  inputId="fav1",
                  label = "Select Favorite High School Course",
                  choices = c(
                    "Arts",
                    "Biology",
                    "Business",
                    "Chemistry",
                    "English",
                    "Environmental Science",
                    "Foreign Language",
                    "Math",
                    "Physical Education",
                    "Physics",
                    "Social Science",
                    "Technology"
                  ),
                  selected = "Arts",
                  width = 400
                ),
              ),
              column(3,
                selectInput(
                  inputId="fav2",
                  label="Select Second Favorite High School Course",
                  choices = c(
                    "Arts",
                    "Biology",
                    "Business",
                    "Chemistry",
                    "English",
                    "Environmental Science",
                    "Foreign Language",
                    "Math",
                    "Physical Education",
                    "Physics",
                    "Social Science",
                    "Technology"
                  ),
                  selected = "Biology",
                  width = 400
                )
              ),
              column(3,
                 selectInput(
                   inputId="leastFav",
                   "Select Least Favorite High School Course",
                   choices = c(
                     "Arts",
                     "Business",
                     "English",
                     "Foreign Language",
                     "Math",
                     "Physical Education",
                     "Science",
                     "Social Science",
                     "Technology"
                   ),
                   selected = "English",
                   width = 400
                 )
               ),
              column(2, offset=1,
                  tags$br(),
                  actionButton(
                    inputId="recommend",
                    label="Recommend",
                    class = "btn btn-warning btn-sm"
                  )
              )
            )),
        
        tags$br(),
        tags$br(),
        
        htmlOutput(outputId="recommendation"),
        tableOutput(outputId="rec_table"),

        tags$br(),
        
        tags$br(),
        
        h3("Salary Overview by Major"),
        tags$br(),
        setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
        sliderInput(
          inputId="SalaryRange2",
          label = "Choose desired salary range",
          min = 35000,
          max = 125000,
          value = c(35000, 125000)
        ),
        
        actionButton(inputId="rec_salary_button", label="Filter", class = "btn btn-warning"),
        
        plotlyOutput(outputId="rec_salary", height=600),
        
        tags$br(),
      )
    )
  ),
  
  
  
  ################################################
  #### Panel: Documentation                   ####
  ################################################
  
  # tabPanel("Documentation",
  #          fluidPage(htmlOutput("doc"))),
  
  ################################################
  #### Panel: About                           ####
  ################################################
  tabPanel("About",
           fluidPage(includeHTML("./about.html")))
  # )
)

##########################################
####   Main Libraries                 ####
##########################################
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(knitr)
library(kableExtra)
library(ggthemes)
library(gghighlight)
library(plotly)
library(scales)
library(ggrepel)
library(h2o)
library(caret)
library(rsconnect)

library(rsconnect)


library(shinythemes)

##########################################
####   Load datasets                  ####
##########################################

data_aa = read.csv("./data/all-ages.csv")
data_rg = read.csv("./data/recent-grads.csv")

data = read.table("./data/class-list.csv", header=T, sep=",",
                  colClasses=c("numeric", "factor", "factor", "factor",
                               "factor", "factor"))

names(data) = c("id", "major", "y", "fav1", "fav2", "least")

data$id = NULL
data$major = NULL

##########################################
####   Data Preprocessing             ####
##########################################
dummies = dummyVars(y ~ ., data=data)
ex = data.frame(predict(dummies, newdata=data))

names(ex) = gsub("\\.", "", names(ex))
data = cbind(data$y, ex)
names(data)[1] = "y"

rm(dummies, ex)

descrCorr = cor(data[,2:ncol(data)])
highCorr = sum(abs(descrCorr[upper.tri(descrCorr)]) > 0.85)

highCorrDescr = findCorrelation(descrCorr, cutoff=0.85)
filteredDescr = data[,2:ncol(data)][,-highCorrDescr]

data = cbind(data$y, filteredDescr)
names(data)[1] = "y"

rm(descrCorr, filteredDescr, highCorr, highCorrDescr)

y = data$y
data = cbind(rep(1, nrow(data)), data[2:ncol(data)])
names(data)[1] = "ones"

comboInfo = findLinearCombos(data)

data = data[, -comboInfo$remove]
data$ones = NULL
data = cbind(y, data)

rm(y, comboInfo)

##########################################
####   Train model                    ####
##########################################

# predictors = names(data[2:ncol(data)])
# response = "y"
# data_h2o = as.h2o(data)
# fit = h2o.randomForest(
#   x=predictors, 
#   y=response, 
#   training_frame=data_h2o, 
#   seed=999, 
#   ntrees=100,
#   max_depth = 5)
# 
h2o.init()
h2o.removeAll(timeout_secs = 0, retained_elements = c())
fit = h2o.loadModel("models/DRF_model_R_1628545796401_12")

##########################################
####   Shiny server                   ####
##########################################

server <- function(session, input, output) {
  # ----------------
  # Explore panel
  # ----------------
  
  output$majorcat = renderTable({
    data_aa %>%
      filter(Major_category == input$majorcattable) %>%
      mutate(Employment_Rate = paste(round((Employed / Total) * 100, 2), "%")) %>%
      select(Major, "Total Number of Enrolled Students"=Total, "Employment Rate"=Employment_Rate)
    
    # kable(
    #   "html",
    #   col.names = c(
    #     "Major",
    #     "Number of Students",
    #     "Employment Rate"
    #   )
    # ) %>%
    # kable_styling(c("striped", "hover"), full_width = T)
  },
  striped=T)
  
  salaryFilter = eventReactive(input$actionDT, {
    minIncome = input$SalaryRange[1]
    maxIncome = input$SalaryRange[2]
    data_aa %>%
      filter(Median < maxIncome, Median > minIncome) %>%
      arrange(desc(Median)) %>%
      select("Major Category"=Major_category, Major, "Median Salary"=Median) 
  })
  
  output$salaryTable = renderDataTable(salaryFilter())
  
  output$majorcatsalaryPlot = renderPlotly({
    data = data_aa %>%
      mutate(Employment_Rate = round((Employed / Total) * 100, 2))
    
    input$majorcatradio
    
    p = ggplot(
        data,
        aes(x=Employment_Rate, y=Median, label=Major)
      ) +
      geom_point() +
      gghighlight(
        data$Major_category==input$majorcatradio,
        label_key=F,
        unhighlighted_colour=alpha("grey", 0.7)) +
      xlab("Employment Rate") +
      ylab("Median Salary") +
      theme(legend.title = element_blank(), legend.position="none")
    
    p
  })
  
  # ----------------
  # Compare panel
  # ----------------
  
  compareSalary = eventReactive(input$compare, {
    data_aa %>%
      filter(Major_category == input$cat1 | Major_category == input$cat2) %>%
      group_by(Major_category) %>%
      summarise(med_salary=mean(Median))
  })
  
  compareEmploymentRate = eventReactive(input$compare, {
    data_aa %>%
      mutate(Employment_Rate = (Employed / Total) * 100) %>%
      filter(Major_category == input$cat1 | Major_category == input$cat2) %>%
      group_by(Major_category) %>%
      summarise(med_empl_rate=mean(Employment_Rate))
  })
  
  compareGender = eventReactive(input$compare, {
    data_rg %>%
      filter(Major_category == input$cat1 | Major_category == input$cat2) %>%
      group_by(Major_category) %>%
      summarise(total_m=sum(Men), total_f=sum(Women))
  })
  
  output$salary = renderPlotly({
    p = ggplot(
        compareSalary(),
        aes(x=Major_category, y=med_salary, fill=Major_category)
      ) +
      geom_bar(stat="identity", position="stack") +
      theme(
        legend.title = element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
      geom_text(aes(label=paste0("$", round(med_salary, 2))), position=position_stack(vjust=0.9)) +
      coord_flip()
    
    p
  })
  
  output$employmentRate = renderPlotly({
    p = ggplot(
      compareEmploymentRate(),
      aes(x=Major_category, y=med_empl_rate, fill=Major_category)
    ) +
      geom_bar(stat="identity") +
      theme(
        legend.title = element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
      geom_text(aes(label=paste0(round(med_empl_rate, 2), "%")), nudge_y = -5) +
      coord_flip()
    
    p
  })
  
  output$gender = renderPlot({
    
    label_m = paste("Total Men:", comma_format()(compareGender()$total_m), sep="\n")
    label_f = paste("Total Women:", comma_format()(compareGender()$total_f), sep="\n")
    p = ggplot(compareGender()) +
      geom_segment(aes(x=0, xend=1, y=total_m, yend=total_f, color=Major_category), size=0.75) +
      theme(
            panel.background = element_blank(), 
            panel.grid=element_blank(), 
            axis.ticks=element_blank(), 
            axis.text=element_blank(), 
            panel.border=element_blank()) +
      xlab("") + ylab("Total Students") +
      xlim(-0.25, 1.25) + ylim(0, max(compareGender()$total_m, compareGender()$total_f)) +
      geom_text_repel(label=label_m, aes(x=-0.25, y=total_m)) + 
      geom_text_repel(label=label_f, aes(x=1.2, y=total_f))
    
    p
  })
  
  # ----------------
  # Recommend panel
  # ----------------
  
  recommendMajorCat = eventReactive(input$recommend, {
    fav1 = input$fav1
    fav2 = input$fav2
    least = input$leastFav
    
    d = data[1,2:ncol(data)]
    
    for (i in 1:ncol(d)) {
      d[1,i] = 0
    }
    
    if (!is.na(match(paste("fav1", fav1, sep=""), names(d)))) {
      d[1,match(paste("fav1", fav1, sep=""), names(d))] = 1
    }
    
    if (!is.na(match(paste("fav2", fav2, sep=""), names(d)))) {
      d[1,match(paste("fav2", fav2, sep=""), names(d))] = 1
    }
    
    if (!is.na(match(paste("least", least, sep=""), names(d)))) {
      d[1,match(paste("least", least, sep=""), names(d))] = 1
    }
    
    as.character(as.data.frame(h2o.predict(fit, as.h2o(d)))$predict)
  })
  
  output$recommendation = renderText({
    paste("<div style='text-align:center'>We recommend that you choose", "<h2><font color=\"#FF0000\"><b>", recommendMajorCat(), "</b></font></h2></div><br><br>")
  })
  
  observeEvent(input$recommend, {
    output$rec_table = renderTable({
      data_aa %>%
        filter(Major_category == recommendMajorCat()) %>%
        mutate(Employment_Rate = round((Employed / Total) * 100, 2)) %>%
        select("Major"=Major, "Median Salary"=Median, "Employment Rate"=Employment_Rate)
    }, striped=T)
  })
  
  recommendSalary = eventReactive(input$rec_salary_button, {
    minIncome = input$SalaryRange2[1]
    maxIncome = input$SalaryRange2[2]
    data_aa %>%
      filter(Major_category == recommendMajorCat(), Median > minIncome, Median < maxIncome)
  })
  
  output$rec_salary = renderPlotly({
    print(recommendSalary())
    p = ggplot(
      recommendSalary(),
      aes(x=Major, y=Median, fill=Major)
    ) + geom_bar(stat="identity", position="stack") +
      theme(
        legend.title = element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
      geom_text(aes(label=paste0("$", round(Median, 2))), position=position_stack(vjust=0.9)) +
      coord_flip()
  })
}

shinyApp(ui=ui, server=server)



