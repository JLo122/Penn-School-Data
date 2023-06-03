### Data 4010 Final_test
## Jinery Lora
## April 17, 2023
## link for Final Project : https://jlopassionprojects.shinyapps.io/Final_test/
## Further down you'll find server and UI

library(tidyverse)
library(RColorBrewer)
library(scales)
library(shiny)
library(rsconnect)

############## LEA dataset
lea <- read.csv("LEA_data.csv")
lea.untouched <- read.csv("LEA_data.csv")
#ccd_fre_reeduced_lunch_eligible
#exp_total
#prof_teach_salary
# rev_total take a look at highest and lowest!
# enroll_allgrades_total
# crdc_fteteach_absent teachers chronically absent
# crdc_stu_absent_f_total chronically absent fem students
# crdc_stu_absent_m_total chronically absent male students



##### Cleaning and Filtering Data

lea <- lea %>% 
  group_by(schoolname) %>% #added all the suspensions the school had!
  mutate(suspension_tot  = sum(safesch_oss_academic,  safesch_oss_conduct,   safesch_oss_substance,
                               safesch_oss_tobacco,   safesch_oss_violence,  safesch_oss_weapon ))


lea.sm <- lea[, c('X', #selecting these variables to look into
                  'schoolname',
                  'county',
                  'enroll_allgrades_total',
                  'prof_teach_salary',
                  'exp_total',
                  'ccd_free_reduced_lunch_eligible',
                  'rev_total',
                  'suspension_tot')]

##recode col names!
colnames(lea.sm)[4] <- "enroll_tot"
colnames(lea.sm)[6] <- "expenditures_tot"
colnames(lea.sm)[7] <- "free_reduced_lunch"
colnames(lea.sm)[8] <- "revenue_tot"

lea.sm <- lea.sm %>% 
  group_by(schoolname) %>% #how much revenue per child
  mutate(rev_perchild  = revenue_tot/enroll_tot)


enroll.bycounty <- lea.sm %>% 
  group_by(county) %>% 
  summarise(county_enrol = sum(enroll_tot)) ####  THIS ADDED ALL enrolled STUDENTS by COUNTY

# need to convert free_reduced_lunch & enroll_tot variable to num
lea.sm$free_reduced_lunch <- as.numeric(lea.sm$free_reduced_lunch)
lea.sm$enroll_tot <- as.numeric(lea.sm$enroll_tot)


##### GGPLOTs
# Define the palette
my_palette <- brewer.pal(n = 5, name = "Dark2")

# Set the color for Philadelphia to #A569BD
my_palette[2] <- "#A569BD"

#compare teach salary
lea.salary <- lea.sm %>% 
  group_by(county) %>% 
  summarise(avg_salary = mean(prof_teach_salary))#filtering and manipulating data for graph

ggplot(data = lea.salary) +
  geom_col(mapping = aes(x = reorder(county, -avg_salary), y = avg_salary, fill = ifelse(county == "Philadelphia", "#A569BD", "other"))) +
  scale_fill_manual(values = my_palette, guide = "none") +
  labs(title = "", x = "Pennsylvania Counties", y = "Average Salary of Classroom Teacher") +
  ylim(0, 100000) +
  theme_minimal()


#compare revenue
lea.revenue <- lea.sm %>% 
  group_by(county) %>% 
  summarise(revenue_sum = sum(revenue_tot))#filtering and manipulating data for graph

ggplot(data = lea.revenue) +
  geom_col(mapping = aes(x = reorder(county, -revenue_sum), y = revenue_sum, fill = ifelse(county == "Philadelphia", "#A569BD", "other"))) +
  labs(title = "", x = "Pennsylvania Counties", y = "Total Revenue") +
  scale_fill_manual(values = my_palette, guide = "none") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# student enrollment
ggplot(enroll.bycounty) +
  geom_col(mapping = aes(y = county_enrol, x = reorder(county, -county_enrol), fill = ifelse(county == "Philadelphia", "#A569BD", "other"))) +
  theme(legend.position = "none") +
  labs(title = "", x = "Pennsylvania Counties", y = "Student Enrollment") +
  scale_fill_manual(values = my_palette, guide = "none") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# searching for correlations
# free reduced lunch and suspensions with revenue_tot

lea.sm <- lea.sm %>% 
  mutate(suspension_rate = (suspension_tot/enroll_tot))#filtering and manipulating data for graph

ggplot(lea.sm) +
  geom_point(mapping = aes(y = suspension_rate, x = revenue_tot)) +
  geom_smooth(mapping = aes(y = suspension_rate, x = revenue_tot)) +
  scale_x_log10()



# free reduced lunch and suspensions with county
lea.grouped2 <- lea.sm %>% 
  group_by(county) %>% 
  summarise(freereduced_lunchtot = sum(free_reduced_lunch),
            suspension_rate = (sum(suspension_tot)/sum(enroll_tot))*100)#filtering and manipulating data for graph

ggplot(lea.grouped2) +
  geom_point(mapping = aes(y = suspension_rate,
                           x = freereduced_lunchtot,
                           fill = ifelse(county == "Philadelphia", "#A569BD", "other"))) +
  geom_smooth(mapping = aes(y = suspension_rate,
                            x = freereduced_lunchtot)) +
  scale_fill_manual(values = my_palette, guide = "none") +
  labs(title = "", 
       y = "Suspension Rate, %", 
       x = "Free or Reduced Lunch Eligibility") +
  scale_x_continuous(labels = comma) +
  theme_minimal()

lea.sm <- lea.sm %>% 
  group_by(schoolname) %>% #how much revenue per child
  mutate(rev_perchild  = revenue_tot/enroll_tot)#filtering and manipulating data for graph

child.rev.lunch <- lea.sm %>% #filtering and manipulating data for graph
  select(county, free_reduced_lunch, rev_perchild, schoolname) %>% 
  group_by(county) %>% 
  summarise(reduced_lunch_tot = sum(free_reduced_lunch), rev_perchild_tot = sum(rev_perchild))

ggplot(child.rev.lunch) + 
  geom_point(mapping = aes(x = rev_perchild_tot, y = reduced_lunch_tot)) +
  geom_smooth(aes(x = rev_perchild_tot, y = reduced_lunch_tot)) +
  scale_x_log10()

# free reduced lunch with prof_teach_salary
child.lunch.teach.salary <- merge(child.rev.lunch, lea.salary)

ggplot(child.lunch.teach.salary) +
  geom_point(mapping = aes(y = reduced_lunch_tot, x = avg_salary, color = ifelse(county == "Philadelphia", "Philadelphia", "Other counties"))) +
  geom_smooth(mapping = aes(y = reduced_lunch_tot, x = avg_salary)) +
  scale_color_manual(values = c("#A569BD", "#1B9E77"), guide = "none") +
  labs(title = , x = "Average Teacher Salary", y = "Students Eligible for Reduced or Free Lunch") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  theme(legend.position = "none") 



####### server

server <- function(input, output){
  schoolrev.input <- reactive({
    if(any(lea.sm$county %in% c(input$school.revenue1, input$school.revenue2)))
    {
      # Define the palette
      my_palette <- brewer.pal(n = 5, name = "Dark2")
      
      # Set the color for Philadelphia to #A569BD
      my_palette[2] <- "#A569BD"
      
      lea.revenue <- lea.sm %>% 
        filter(county %in% c(input$school.revenue1, input$school.revenue2)) %>% 
        group_by(county) %>% 
        summarise(revenue_sum = sum(revenue_tot))
      return(lea.revenue)
    }
    
  })
  
  output$revenue.plot <- renderPlot({ #this plot shows one or more county revenue
    ggplot(data = schoolrev.input()) +
      geom_col(mapping = aes(x = reorder(county, -revenue_sum), y = revenue_sum, fill = ifelse(county == "Philadelphia", "#A569BD", "other"))) +
      labs(title = "Total Revenue for Pennsylvania Schools by County", x = "Pennsylvania Counties", y = "Total Revenue") +
      scale_fill_manual(values = my_palette, guide = "none") +
      scale_y_continuous(labels = comma) +
      theme_minimal()
  })
  
  teachsal.input <- reactive({
    if(any(lea.sm$county %in% c(input$teacher.salary1, input$teacher.salary2)))
    {
      # Define the palette
      my_palette <- brewer.pal(n = 5, name = "Dark2")
      
      # Set the color for Philadelphia to #A569BD
      my_palette[2] <- "#A569BD"
      
      lea.salary <- lea.sm %>% 
        filter(county %in% c(input$teacher.salary1, input$teacher.salary2)) %>% 
        group_by(county) %>% 
        summarise(avg_salary = mean(prof_teach_salary))
      return(lea.salary)
    }
    
  })
  
  
  output$teach.salary <- renderPlot({ #average teacher salary for each county
    ggplot(data = teachsal.input()) +
      geom_col(mapping = aes(x = reorder(county, -avg_salary), y = avg_salary, fill = ifelse(county == "Philadelphia", "#A569BD", "other"))) +
      scale_fill_manual(values = my_palette, guide = "none") +
      labs(title = "Average Salary for Pennsylvania Teachers by County", x = "Pennsylvania Counties", y = "Average Salary of Classroom Teacher") +
      ylim(0, 100000) +
      theme_minimal()
  })
  output$sus.rev <- renderPlot({ #this plot shows the rate of suspension to revenue
    ggplot(lea.sm) +
      geom_point(mapping = aes(y = suspension_rate, x = revenue_tot, color = ifelse(county == "Philadelphia", "Philadelphia", "Other counties"))) +
      scale_color_manual(values = c("#A569BD", "#1B9E77"), guide = "none") +
      geom_smooth(mapping = aes(y = suspension_rate, x = revenue_tot)) +
      scale_x_log10() +
      labs(title = "Student Suspension Rate Compared to County School Revenue",
           x = "Total Revenue",
           y = "Student Suspension Rate") +
      theme_minimal() +
      scale_x_log10(labels = comma)+
      scale_y_continuous(labels = comma) +
      theme(legend.position = "none")
  })
  
  studentenrol.input <- reactive({
    if(any(lea.sm$county %in% c(input$student.enrol1, input$student.enrol2)))
    {
      # Define the palette
      my_palette <- brewer.pal(n = 5, name = "Dark2")
      
      # Set the color for Philadelphia
      my_palette[2] <- "#A569BD"
      
      enroll.bycounty <- lea.sm %>% 
        filter(county %in% c(input$student.enrol1, input$student.enrol2)) %>% 
        group_by(county) %>% 
        summarise(county_enrol = sum(enroll_tot))
      return(enroll.bycounty)
    }
    
  })
  output$student.enrol <- renderPlot({
    # plot of student enrollment
    ggplot(studentenrol.input()) +
      geom_col(mapping = aes(y = county_enrol, x = reorder(county, -county_enrol), fill = ifelse(county == "Philadelphia", "#A569BD", "other"))) +
      theme(legend.position = "none") +
      labs(title = "Student Enrollment Per Pennsylvania County", x = "Pennsylvania Counties", y = "Student Enrollment") +
      scale_fill_manual(values = my_palette, guide = "none") +
      scale_y_continuous(labels = comma) +
      theme_minimal()
    
  })
  output$lunch.revchild <- renderPlot({
    lea.sm <- lea.sm %>% 
      group_by(schoolname) %>% #how much revenue per child
      mutate(rev_perchild  = revenue_tot/enroll_tot)
    child.rev.lunch <- lea.sm %>% 
      select(county, free_reduced_lunch, rev_perchild, schoolname) %>% 
      group_by(county) %>% 
      summarise(reduced_lunch_tot = sum(free_reduced_lunch), rev_perchild_tot = sum(rev_perchild))
    
    ggplot(child.rev.lunch) + 
      geom_point(mapping = aes(x = rev_perchild_tot, y = reduced_lunch_tot, color = ifelse(county == "Philadelphia", "Philadelphia", "Other counties"))) +
      geom_smooth(aes(x = rev_perchild_tot, y = reduced_lunch_tot)) +
      scale_color_manual(values = c("#A569BD", "#1B9E77"), guide = "none") +
      scale_x_log10() +
      labs(title = "Total Students eligible for Reduced/Free lunch Compared to Revenue per Student", x = "Total Revenue per Student", y = "Students Eligible for Reduced or Free Lunch") +
      theme_minimal() +
      scale_y_continuous(labels = comma) +
      scale_x_log10(labels = comma) +
      theme(legend.position = "none") 
    
    
  })
  
  output$teach.student <- renderPlot({
    # free reduced lunch with prof_teach_salary
    child.lunch.teach.salary <- merge(child.rev.lunch, lea.salary)
    
    ggplot(child.lunch.teach.salary) +
      geom_point(mapping = aes(y = reduced_lunch_tot, x = avg_salary, color = ifelse(county == "Philadelphia", "Philadelphia", "Other counties"))) +
      geom_smooth(mapping = aes(y = reduced_lunch_tot, x = avg_salary)) +
      scale_color_manual(values = c("#A569BD", "#1B9E77"), guide = "none") +
      labs(title = "Total Students eligible for Reduced/Free lunch Compared to Teacher Salary",
           x = "Average Teacher Salary",
           y = "Students Eligible for Reduced or Free Lunch") +
      theme_minimal() +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma) +
      theme(legend.position = "none") 
  })
  
}

####### UI

ui <- shinyUI(fluidPage(
  theme = shinythemes::shinytheme("yeti"),#set up app theme
  navbarPage(
    "Pennsylvania School Data 2018", #title of my tab bar
    tabPanel(
      "Welcome", #title of tab 1
      headerPanel(" "),
      h2("Jinery Lora's Data Analytics Final"), #title page
      h4("For my final project I am presenting data collected by Research for 
         Action of the Pennsylvania academic years starting 2006 to 2018. 
         This data holds information of the state, federal and Local Education Agency (LEA) level. 
         This data holds information for schools in 67 Pennsylvanian counties. 
         One thing to note is that Philadelphia is only recorded once and the 
         data had over 130,000 students enrolled, at some point, coming second to Allegheny county(with several observations).
         Some of the variables I found interesting were school revenue, teacher salary, student enrollment and suspensions. 
         Provided with this information I wanted to see if variables such as revenue and teacher salary correlates with student suspensions, their free lunch eligibility, etc.
         Since lack of funding or lower paid teachers might reflect students' environment and can beome expressed through their grades and behavior.
         In the following tab I have provided visuals to determine for yourself if you can find a relationship."),
      br(),
      h4("When clicking on \"Exploring the Data\" you'll be lead through how to 
         use my widgets and my thoughts on this prelimenary analysis.")
    ),#welcome page,first tab
    tabPanel(
      "Exploring the Data",#title of tab 2
      mainPanel(br(),
                helpText("For each graph Philadelphia is in green."),
                tabsetPanel(
                  type = "tabs",
                  tabPanel(
                    "County School Finance",
                    br(),
                    h4("Within this tab you'll find financial information for revenue and teacher salary."),
                    helpText("Below you are given the option to select 1 or 
                              more Pennsylvania counties to select, view, and 
                              compare their total revenue."),
                    selectInput("school.revenue1", #drop down 1 for school rev
                                "County:",
                                lea.sm$county,
                                selected = "Philadelphia"),
                    selectInput("school.revenue2", #drop down 2 for school rev
                                "County:",
                                lea.sm$county,
                                multiple = T),
                    plotOutput("revenue.plot"),
                    br(),
                    helpText("Below in this second graph you are given the option to select 1 or 
                              more Pennsylvania counties to select, view, and 
                              compare teacher salary."),
                    selectInput("teacher.salary1", #drop down 1 for teach sal
                                "County:",
                                lea.sm$county,
                                selected = "Philadelphia"),
                    selectInput("teacher.salary2", #drop down 2 for teach sal
                                "County:",
                                lea.sm$county,
                                multiple = T),
                    plotOutput("teach.salary"),
                    br(),
                    h4("We can see from these graphs that Philadelphia has one of the highest revenues and teacher salary.")
                  ),#first sub panel
                  tabPanel(
                    "Student Enrollment",
                    br(),
                    h4("Within this tab you'll find information for student enrollment."),
                    helpText("Below you are given the option to select 1 or 
                              more Pennsylvania counties to select, view, and 
                              compare student enrollment."),
                    selectInput("student.enrol1", #drop down 1 for student enrol
                                "County:",
                                lea.sm$county,
                                selected = "Philadelphia"),
                    selectInput("student.enrol2", #drop down 2 for student enrol
                                "County:",
                                lea.sm$county,
                                multiple = T),
                    plotOutput("student.enrol"),
                    br(),
                    h4("We can see here that Allgeheny and Philadelphia had the highest student enrollment.")
                  ),# 2nd sub panel
                  tabPanel(
                    "More Student Infographics",
                    br(),
                    h4("Here I try to find a relationship between these variables."),
                    plotOutput("sus.rev"),
                    br(),
                    h4("Here I decided to find a relationship between rate of 
                       suspension and school revenue. I cannot see a positive or
                       negative relationship between the two. Perhaps looking  
                       into suspensions rates higher that 20% and searching for 
                       a pattern there could yield better results."),
                    plotOutput("lunch.revchild"),
                    br(),
                    h4("For our second graph we can see a potentional 
                       relationship between revenue per student and enrolled 
                       students eligibility for reduced lunch. We can see after 
                       $100,000 in revenue eligibility goes up. 
                       This could be because more money is needed in these 
                       institutions, but more data analysis is needed."),
                    plotOutput("teach.student"),
                    br(),
                    h4("In this last graph I expected to see lower teacher salary
                       with more students needing reduced lunch prices, but it 
                       appears to be very weak.")
                  )# 3rd sub panel
                ))
    )#second top tab
  )#nav bar
)#fluid page
)#shiny UI
shinyApp(ui = ui, server = server) 