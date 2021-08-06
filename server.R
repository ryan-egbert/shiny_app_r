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

library(rsconnect)
library(shinythemes)

##########################################
####   Attaching datasets             ####
##########################################

data_aa = read.csv("data/all-ages.csv")
data_rg = read.csv("data/recent-grads.csv")

#data <- read.csv("data/employment_data.csv")

# data <- readRDS("data/employment_data.rds")
# data_g <- readRDS("data/graduates_by_institutions.rds")
# 
# 
# ## Setting datatables view
# 
# opts <- list(
#   language = list(url = "//cdn.datatables.net/plug-ins/1.10.19/i18n/English.json"),
#   pageLength = 30,
#   searchHighlight = TRUE,
#   orderClasses = TRUE,
#   columnDefs = list(list(
#     targets = c(1, 6), searchable = FALSE
#   ))
# )


##########################################
####   Shiny server                   ####
##########################################

server <- function(session, input, output) {
  ################################################
  #### Panel: Main>Summary>Tables & Pie Chart ####
  ################################################
  
  # ----------------
  # Explore section
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
      select("Major Category"=Major_category, Major, "Median Salary"=Median)
  })
  
  output$salaryTable = renderDataTable(salaryFilter())
  
  output$majorcatsalaryPlot = renderPlotly({
    data = data_aa %>%
      mutate(Employment_Rate = round((Employed / Total) * 100, 2))
    
    input$majorcatradio
    
    p = ggplot(
        data,
        aes(x=Employment_Rate, y=Median)
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
      geom_text(aes(label=paste0(round(med_empl_rate, 2), "%")), nudge_y = -5)
    
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
  
  output$recommendation = renderText({
    paste("We recommend that you choose", "<font color=\"#FF0000\"><b>", input$fav1, "</b></font>", "because it's the best")
  })
  
  output$rec_table = renderTable({
    data_aa %>%
      filter(Major_category == input$fav1) %>%
      mutate(Employment_Rate = round((Employed / Total) * 100, 2)) %>%
      select("Major"=Major, "Median Salary"=Median, "Employment Rate"=Employment_Rate)
  }, striped=T)
  
  recommendSalary = eventReactive(input$rec_salary_button, {
    minIncome = input$SalaryRange2[1]
    maxIncome = input$SalaryRange2[2]
    data_aa %>%
      filter(Major_category == input$fav1, Median > minIncome, Median < maxIncome)
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
  
  # output$datahead <- renderPrint({
  #   data %>%
  #     filter(year == input$checkYear) %>%
  #     group_by(university) %>%
  #     select(university,
  #            employment_rate_overall,
  #            gross_monthly_median) %>%
  #     summarise_all(funs(mean)) %>%
  #     mutate_if(is.numeric, round, 0) %>%
  #     arrange(desc(employment_rate_overall)) %>%
  #     #summarise_at(.vars = names(.)[8], list(min, max)) %>%
  #     kable(
  #       "html",
  #       col.names = c(
  #         "University",
  #         "Employment Rate Overall (Avg %)",
  #         "Gross Monthly Median Income (Avg)"
  #       )
  #     ) %>%
  #     kable_styling(c("striped", "hover"), full_width = T)
  #   
  # })
  # 
  # # ----------------
  # # pie plot section
  # # ----------------
  # 
  # output$piePlot <- renderPlot({
  #   colmap <-
  #     c(
  #       "#bdb2ff",
  #       # NUS
  #       "#ffc6ff",
  #       # NTU
  #       "#fffffc",
  #       # SMU
  #       "#33658A",
  #       # SIT
  #       "#3a506b",
  #       # SUTD
  #       "#577590",
  #       # SUSS
  #       "#43aa8b",
  #       # NIE
  #       "#90be6d",
  #       # SP
  #       "#f9c74f",
  #       # NP
  #       "#f8961e",
  #       # TP
  #       "#f3722c",
  #       # NAYANG POLY
  #       "#f94144",
  #       # RP
  #       "#ffadad",
  #       # NAFA DEG
  #       "#ffd6a5",
  #       # LAS DEG
  #       "#fdffb6",
  #       # NAFA DIP
  #       "#caffbf",
  #       # NAFA DEG
  #       "#a8dadc"  # ITE
  #     )
  #   
  #   data_g %>%
  #     filter(year == input$checkYear) %>%
  #     group_by(university) %>%
  #     tally(graduates) %>%
  #     ggplot(aes(x = "", y = n, fill = university)) +
  #     geom_bar(
  #       stat = "identity",
  #       width = 1,
  #       color = "black",
  #       size = 1
  #     ) +
  #     theme_void() +
  #     theme(legend.position = "right",
  #           plot.title = element_text(hjust = 0.5, size = 14)) +
  #     coord_polar("y", start = 0) +
  #     scale_fill_manual(values = c(colmap)) +
  #     labs(title = "Universities by number of graduates")
  #   
  # })
  # 
  # # ------------------
  # # data table section
  # # ------------------
  # 
  # # filter the checkgroup input:
  # 
  # yearGroup <- reactive({
  #   input$actionDT
  #   isolate(return(data[data$year %in% input$checkYearGroup, ]))
  # })
  # 
  # 
  # filtered_DT <- reactive({
  #   input$actionDT
  #   isolate({
  #     minIncome <- input$incomeRange[1]
  #     maxIncome <- input$incomeRange[2]
  #     minEmploy <- input$employRange[1]
  #     maxEmploy <- input$employRange[2]
  #   })
  #   
  #   yearGroup() %>%
  #     filter(basic_monthly_median > minIncome,
  #            basic_monthly_median < maxIncome) %>%
  #     filter(employment_rate_ft_perm > minEmploy,
  #            employment_rate_ft_perm < maxEmploy) %>%
  #     select(1, 2, 3, 4, 6, 8)
  # })
  # 
  # # render DT:
  # 
  # output$myTable <- renderDataTable({
  #   filtered_DT() %>%
  #     datatable(
  #       .,
  #       rownames = FALSE,
  #       class = "table",
  #       options = list(pageLength = 10, scrollX = T),
  #       colnames = c(
  #         "Year",
  #         "University",
  #         "School",
  #         "Degree",
  #         "Fulltime Employment Rate",
  #         "Basic Monthly Salary(Median)"
  #       )
  #     )
  #   
  # })
  # 
  # 
  # ################################################
  # #### Panel: Main>Plots                      ####
  # ################################################
  # 
  # # --------------------
  # # density plot section
  # # --------------------
  # 
  # # filter the checkgroup input:
  # 
  # dent <-  reactive({
  #   return(data[data$university %in% input$checkGroup, ])
  #   
  # })
  # 
  # # render density plot
  # 
  # output$densityPlot <- renderPlotly({
  #   colmap <- c("#2c3e50",
  #               "#e67e22",
  #               "#f1c40f",
  #               "#e74c3c",
  #               "#F97F51",
  #               "#27ae60")
  #   
  #   ggplotly(
  #     ggplot(data = dent(), aes_string(x = input$selectvar)) +
  #       geom_density(aes(fill = university), size = 1, alpha=0.75) +
  #       theme(legend.position = "bottom") + labs(x = input$selectvar) +
  #       scale_fill_manual(values = colmap) +
  #       theme_hc() +
  #       theme(
  #         legend.title = element_blank(),
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_blank()
  #       )
  #   ) %>% layout(legend = list(orientation = "h",
  #                              y = 0, x = 0))
  #   
  # })
  # 
  # 
  # # ----------------
  # # bar plot section
  # # ----------------
  # 
  # # filter the input and group data:
  # 
  # output$uniPlot <- renderPlotly({
  #   monthly <- data %>%
  #     filter(university == input$radio) %>%
  #     group_by(school) %>%
  #     summarise_at(.vars = names(.)[7:8], .funs = c(mean = "mean"))
  #   
  #   # render bar plot
  #   
  #   colmap <-
  #     c(
  #       "#2c3e50",
  #       "#e67e22",
  #       "#f1c40f",
  #       "#e74c3c",
  #       "#F97F51",
  #       "#27ae60",
  #       "#2980b9",
  #       "#86BBD8",
  #       "#8e44ad",
  #       "#95a5a6",
  #       "#f39c12",
  #       "#d35400",
  #       "#c0392b",
  #       "#bdc3c7",
  #       "#D6A2E8",
  #       "#25CCF7",
  #       "#16a085"
  #     )
  #   
  #   p <- monthly %>%
  #     data.frame() %>%
  #     ggplot(.,
  #            aes(
  #              x = reorder(school, basic_monthly_median_mean),
  #              y = basic_monthly_median_mean,
  #              fill = school
  #            )) +
  #     geom_bar(
  #       stat = "identity",
  #       width = 0.5,
  #       color = "black",
  #       size = 1
  #     ) +
  #     scale_fill_manual(values = colmap) +
  #     theme_hc() +
  #     theme(
  #       legend.title = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.title.y = element_blank()
  #     )
  #   
  #   p <- ggplotly(p + coord_flip(), tooltip = ("basic_monthly_median_mean"))
  #   hide_legend(p)
  #   
  # })
  # 
  # # ----------------
  # # box plot section
  # # ----------------
  # 
  # 
  # 
  # 
  # # filter the checkgroup input:
  # 
  # uniMedian <-  reactive({
  #   return(data[data$university%in%input$checkGroupbox, ])
  # })
  # 
  # # render box plot
  # 
  # output$boxPlot <- renderPlotly({
  #   
  #   colmap <- c("#2c3e50",
  #               "#e67e22",
  #               "#f1c40f",
  #               "#e74c3c",
  #               "#F97F51",
  #               "#27ae60")
  #   
  #   p <-
  #     ggplot(data = uniMedian(),
  #            aes(x = university, y = basic_monthly_median, fill = university)) +
  #     geom_boxplot(color = "black",
  #                  size = 1,
  #                  width = 0.3) +
  #     scale_fill_manual(values = colmap) +
  #     theme_hc() +
  #     theme(
  #       legend.title = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.title.y = element_blank()
  #     )
  #   
  #   p <- ggplotly(p + coord_flip(), tooltip = ("basic_monthly_median"))
  #   hide_legend(p)
  #   
  # })
  # 
  # 
  # # ----------------
  # # scatter plot section
  # # ----------------
  # 
  # output$scatPlot <- renderPlotly({
  #   colmap <- c("#2c3e50",
  #               "#e67e22",
  #               "#f1c40f",
  #               "#e74c3c",
  #               "#F97F51",
  #               "#27ae60")
  #   
  #   data <- data %>% filter(year=="2018")
  #   
  #   p <-
  #     ggplot(
  #       data,
  #       aes(
  #         x = basic_monthly_median,
  #         y = employment_rate_ft_perm,
  #         color = university,
  #         shape = as.factor(year)
  #       )
  #     ) +
  #     geom_point(size = 3, alpha = 0.7) +
  #     scale_colour_manual(values = colmap) +
  #     theme_hc() +
  #     theme(
  #       legend.title = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.title.y = element_blank()
  #     )
  #   
  #   ggplotly(
  #     p,
  #     tooltip = c(
  #       "year",
  #       "basic_monthly_median",
  #       "university",
  #       "employment_rate_ft_perm"
  #     ),
  #     height = 800
  #   )
  #   
  # })
  # 
  # 
  # ################################################
  # #### Panel: Main>Details                    ####
  # ################################################
  # 
  # 
  # observeEvent(
  #   input$detailUniversity,
  #   updateSelectInput(
  #     session,
  #     "detailSchool",
  #     "Select School",
  #     choices = unique(data$school[data$university == input$detailUniversity])
  #   )
  # )
  # observeEvent(
  #   input$detailSchool,
  #   updateSelectInput(
  #     session,
  #     "detailMajor",
  #     "Select Program",
  #     choices = unique(data$degree[data$school == input$detailSchool &
  #                                    data$university == input$detailUniversity])
  #   )
  # )
  # 
  # detailTB <- eventReactive(input$detailMajor,
  #                           {
  #                             data %>%
  #                               filter(
  #                                 school == input$detailSchool &
  #                                   university == input$detailUniversity &
  #                                   degree == input$detailMajor
  #                               ) %>%
  #                               select(c(
  #                                 "year",
  #                                 "basic_monthly_median",
  #                                 "employment_rate_ft_perm"
  #                               ))
  #                             
  #                           })
  # 
  # output$detailTable <- renderPrint({
  #   input$detailFilter
  #   
  #   isolate({
  #     detailTB() %>%
  #       data.frame() %>%
  #       kable("html",
  #             col.names = c("Year", "Median Montly Income",
  #                           "Fulltime Employment Rate")) %>%
  #       kable_styling(c("striped", "hover"), full_width = F)
  #   })
  # })
  # 
  # # median income plot:
  # 
  # output$detailPlot <- renderPlot({
  #   input$detailFilter
  #   
  #   isolate({
  #     ggplot(detailTB(), aes(x = year, y = basic_monthly_median)) +
  #       geom_smooth(
  #         mapping = aes(linetype = "r2"),
  #         method = "lm",
  #         formula = y ~ x + log(x),
  #         se = FALSE,
  #         color = "#bdd5ea",
  #         linetype = "dashed",
  #         size = 2,
  #         alpha = 0.5
  #       ) +
  #       geom_line(aes(y = basic_monthly_median),
  #                 size = 2,
  #                 color = "#2c3e50") +
  #       geom_point(
  #         aes(x = year, y = basic_monthly_median),
  #         size = 7,
  #         shape = 21,
  #         colour = "white",
  #         fill = "#fca311",
  #         stroke = 5
  #       ) +
  #       
  #       theme_hc() +
  #       theme(
  #         legend.title = element_blank(),
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_blank()
  #       )
  #   })
  #   
  # })
  # 
  # output$detailPlotem <- renderPlot({
  #   input$detailFilter
  #   
  #   isolate({
  #     ggplot(detailTB(), aes(x = year, y = employment_rate_ft_perm)) +
  #       geom_smooth(
  #         mapping = aes(linetype = "r2"),
  #         method = "lm",
  #         formula = y ~ x + log(x),
  #         se = FALSE,
  #         color = "#bdd5ea",
  #         linetype = "dashed",
  #         size = 2,
  #         alpha = 0.5
  #       ) +
  #       geom_line(aes(y = employment_rate_ft_perm),
  #                 size = 2,
  #                 color = "#2c3e50") +
  #       geom_point(
  #         aes(x = year, y = employment_rate_ft_perm),
  #         size = 7,
  #         shape = 21,
  #         colour = "white",
  #         fill = "#fca311",
  #         stroke = 5
  #       ) +
  #       
  #       theme_hc() +
  #       theme(
  #         legend.title = element_blank(),
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_blank()
  #       )
  #   })
    
  # })
  # 
  # ################################################
  # #### Panel: Documentation                   ####
  # ################################################
  # 
  # getPageDoc <- function() {
  #   return(includeHTML("gesrmarkdown.html"))
  # }
  # output$doc <- renderUI({
  #   getPageDoc()
  # })
  # 
  # 
  # ################################################
  # #### Panel: About                           ####
  # ################################################
  # 
  # getPageAbo <- function() {
  #   return(includeHTML("about.html"))
  # }
  # output$abo <- renderUI({
  #   getPageAbo()
  # })
  
  
}
