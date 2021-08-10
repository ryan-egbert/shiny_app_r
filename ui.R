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
  
  tabPanel("Documentation",
           fluidPage(htmlOutput("doc"))),
  
  ################################################
  #### Panel: About                           ####
  ################################################
  tabPanel("About",
           fluidPage(includeMarkdown("about.rmd")))
)
