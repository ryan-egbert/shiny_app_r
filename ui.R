##########################################
####   Shiny ui                       ####
##########################################
library(shinyWidgets)
library(shiny)
library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
# ------------------
# Main title section
# ------------------

ui <- navbarPage(
  "Project Name",
  theme = shinytheme("flatly"),
  tabPanel(
    "Insert Title",
    # App title ----
    titlePanel(div(
      windowTitle = "GraduatEmploymentSG",
      img(src = "sg0.jpg", width = "100%", class = "bg"),
    )),
    
    tags$br(),
    
        ##########################################
        ####  Panel: Main>Summary             ####
        ##########################################
            ################################################
            #### Panel: Main>Summary>Tables & Pie Chart ####
            ################################################
                
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Explore",
        ################################################
        #### Panel: Main>Explore>Tables & Pie Chart ####
        ################################################

        # ------------------
        # ranking $ pie chart section
        # ------------------

        sidebarLayout(
          sidebarPanel(
            h3("Major Category"),
            tags$br(),
            selectInput(
              inputId="majorcattable",
              label="Select a major category to view",
              choices = list("Agriculture & Natural Resources", "Arts", "Biology & Life Science",
                             "Business", "Communications & Journalism","Computer Technology","Education",
                             "Engineering","Health","Humanities & Liberal Arts","Industrial Arts & Consumer Service Tech",
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


        sidebarLayout(
          sidebarPanel(
            # ------------------
            # Data overview filters
            # ------------------

            h3("Salary Overview"),
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
        
        sidebarLayout(
          sidebarPanel(
            # ------------------
            # Data overview filters
            # ------------------
            h3("Scatterplot Panel"),
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
            h3("Average Salary by Major Category type"),
            plotlyOutput(outputId = "majorcatsalaryPlot"),
            tags$br(),
            tags$br()
          )
        ),
        
        tags$hr(),
      ),
      
      
      ################################################
      #### Panel: Main>Plots                      ####
      ################################################
      
      tabPanel(
        "Compare",
        
        # --------------------
        # density plot section
        # --------------------
        
        sidebarLayout(
          sidebarPanel(
            h3("Major Category vs. Salary"),
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
      #### Panel: Main>Details                    ####
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
                  label = "Select Favorite HS Course",
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
                  label="Select Second Favorite HS Course",
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
                  selected = "",
                  width = 400
                )
              ),
              column(3,
                 selectInput(
                   inputId="leastFav",
                   "Select Least Favorite HS Course",
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
                   selected = "",
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
        tags$hr(),
        tags$br(),
        
        htmlOutput(outputId="recommendation"),
        tableOutput(outputId="rec_table"),

        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br(),
        
        tags$br(),
        
        h3("Salary Overview"),
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
        
        plotlyOutput(outputId="rec_salary"),
        
        tags$br(),
        
        plotOutput(outputId="salary_range")
      )
    )
  ),
  
  
  
  ################################################
  #### Panel: Documentation                   ####
  ################################################
  
  tabPanel("Documentation",
           fluidPage(htmlOutput("doc"))),
  
  ################################################
  #### Panel: About                           ####
  ################################################
  tabPanel("About",
           fluidPage(htmlOutput("abo")))
)
