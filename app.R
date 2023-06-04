library(shiny)
library(shiny.semantic)
library(lubridate)
library(aws.s3)
library(aws.ec2metadata)
library(tidyr)
library(dplyr)
library(shinyjs)
library(toastui)
library(shinyWidgets)
library(RColorBrewer)
library(glue)
library(stringr)
library(emayili)
library(shinybusy)
library(shinyalert)
library(kableExtra)
library(highcharter)
#library(plyr)


deets <- aws.s3::s3readRDS('deets.rds', bucket = 'kumonsvv')
employeeList <- aws.s3::s3readRDS('employeeList.rds', bucket = 'kumonsvv') %>% distinct()
sched1 <- aws.s3::s3readRDS('schedule.rds', bucket = 'kumonsvv') #%>% filter(as.Date(start) >= (today() %m+% days(-1)))
pass1 <- aws.s3::s3readRDS('adminPass.rds', bucket = 'kumonsvv')
timer <- aws.s3::s3readRDS('timer.rds', bucket = 'kumonsvv') %>%
  filter(day(checkin) == day(today())) %>% filter(month(checkin) == month(today())) %>%
  filter(year(checkin) == year(today()))


smtp <- gmail(username = 'springvalleyvillage_tx@ikumon.com', password = aws.s3::s3readRDS('emailDeetsKumon.rds', bucket = 'kumonsvv'))

n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#pie(rep(1,n), col=sample(col_vector, n))

cal_props1 <- employeeList %>% select(name = employee) %>% mutate(id = seq(1, n(), 1),
                                                                 color = '#000',
                                                                 bgColor = col_vector[1:nrow(employeeList)],
                                                                 borderColor = 'black')

cal_props2 <- deets %>% select(name = students)%>% mutate(id = seq(1, n(), 1),
                                                         color = '#000',
                                                         bgColor = col_vector[1:nrow(deets)],
                                                         borderColor = 'black')


kumonMath <- data.frame(level = c('6A', '5A', '4A', '3A', '2A', 'A', 'B',
                                  'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 
                                  'M', 'N', 'O', 'X'))

kumonReading <- data.frame(level = c('7A', '6A', '5A', '4A', '3A', '2A', 'AI', 'AII', 'BI', 'BII',
                                  'CI', 'CII', 'DI', 'DII', 'EI','EII',  'FI', 'FII', 'GI', 'GII',
                                  'HI','HII',  'I1', 'I2', 'J', 'K', 'L'))

kumonSheets <- data.frame(sheets = seq(1, 200, 1))

date1 <- wday(today()) 

date1[date1 == 1] <- 'Sunday'
date1[date1 == 2] <- 'Monday'
date1[date1 == 3] <- 'Tuesday'
date1[date1 == 4] <- 'Wednesday'
date1[date1 == 5] <- 'Thursday'
date1[date1 == 6] <- 'Friday'
date1[date1 == 7] <- 'Saturday'
date1 <- paste0(date1, ', ', month.name[month(today())], ' ', day(today()), ', ', year(today()))
grades <- c('None', 'Pre-K', 'K', '1st', '2nd', '3rd', '4th', '5th', '6th', '7th', '8th', '9th', '10th', '11th', '12th')



ui <- shinyUI(
  semanticPage(
    tags$br(),
    add_busy_bar(color = "blue"),
    div(
      class = "ui grid",
      div(
        style = "text-align: center;width: 100%;",
        #class = 'center',
      
        segment(
          class = 'raised segment',
          #form(
            img(src = './images/kumon-logo-white1_full.jpeg'),
            
            h2(style="text-align: center;",'Kumon Math and Reading Center of Houston - Spring Valley Village')
         #   )
          )
        ),
      shinyjs::useShinyjs(),
      tabset(tabs = 
               list(
                 list(menu = 'Student Sign-In', 
                    content = div(
                     # class = "one column row",
                      # Form Input
                      
                        segment(
                          form(
                           # h2(class = "ui dividing header", "Student Sign-In"),
                            fields(
                              class = 'four',
                            
                              field(
                                tags$label("Student"),
                                dropdown_input("student", unique(deets$students), value = deets$students[1], type = "search selection single")
                              ),
                              field(
                                tags$label("Subject"),
                                textOutput('subject')#,
                                #text_input('subject', value = '', placeholder = 'Subject')#,
                                #dropdown_input("subject", c('Math', 'Reading', 'Both'), value = "Both", type = "search selection single")
                              ),
                              field(
                                tags$label("Virtual or In-Center?"),
                                textOutput('virtualHome')
                                # text_input("phone", value = "",  placeholder = "Phone")
                              ),
                              field(
                                tags$label("Parent(s)"),
                                textOutput('parent')#,
                                #text_input('subject', value = '', placeholder = 'Subject')#,
                                #dropdown_input("subject", c('Math', 'Reading', 'Both'), value = "Both", type = "search selection single")
                              )
                              ),
                            fields(
                              class = 'four',
                              field(
                                tags$label("E-Mail"),
                                textOutput('email')
                                #text_input("email", value = "", type = "email", placeholder = "E-Mail")
                              ),
                              field(
                                tags$label("Phone"),
                                textOutput('phone')
                               # text_input("phone", value = "",  placeholder = "Phone")
                              ),
                              field(
                                tags$label("Grade"),
                                textOutput('grade')
                                # text_input("phone", value = "",  placeholder = "Phone")
                              ),
                              field(
                                tags$label("Birthday"),
                                textOutput('birthday')
                                # text_input("phone", value = "",  placeholder = "Phone")
                              ),
                              field(
                                tags$label("Scheduled Sessions"),
                                textOutput('sessions')
                                # text_input("phone", value = "",  placeholder = "Phone")
                              )
                              ),

                            field(
                              tags$label(date1)
                              # calendar(
                              #   "calendar_ex",
                              #   placeholder = 'Check-In Date',
                              #   value = today(),
                              #   min = today(),
                              #   max = today()
                              # )
                            ),
                            field(
                              action_button('checkin', 'Check-in', icon = icon('share')
                                )
                              ),
              
                            
                              textOutput('mathLabel'),
                              fields(
                               class = 'three',
                               field(
                                  dropdown_input("mathLevel", kumonMath$level, value = kumonMath$level[1], type = "search selection single")
                                ),
                                field(
                                  dropdown_input("mathNumber1", kumonSheets$sheets, value = 1, type = "search selection single")
                                ),
                                field(
                                  dropdown_input("mathNumber2", kumonSheets$sheets, value = 1, type = "search selection single")
                                )
                              ),
                              textOutput('readingLabel'),
                              fields(
                                class = 'three',
                               field(
                                dropdown_input("readingLevel", kumonReading$level, value = kumonReading$level[1], type = "search selection single")
                                ),
                               field(
                                dropdown_input("readingNumber1", kumonSheets$sheets, value = 1, type = "search selection single")
                                ),
                               field(
                                dropdown_input("readingNumber2", kumonSheets$sheets, value = 1, type = "search selection single")
                               )
                              
                              ),
                           field(
                           tags$label("Send checkout email?"),
                           #br(),
                           checkboxInput('sendCheckout', '', value = FALSE, width = NULL
               
                              )
                            ),
                            field(
                              action_button('checkout', 'Check-out', icon = icon('reply'))
                            )
                            
                          )
                        
                      )
                    )
                 
               ),
          list(menu = 'Student Remaining Time',
               content = div(
                 field(
                   htmlOutput("student_table")
                 )
               )
               ),
          list(menu = 'New Student Sign-Up', 
               content = div(
                #class = "one column row",
                segment(
                  form(
                    field(
                      tags$label('Admin Password'),
                      passwordInput('adminPassword2', '', value = '')
                    ),
                   # h2(class = "ui dividing header", "New Student Sign-Up"),
                    fields(
                      class = 'four',
                      field(
                        tags$label("Student"),
                        text_input("student1", value = "",  placeholder = "Student First and Last Name")
                      ),
                      field(
                        tags$label('Subject'),
                        dropdown_input("subject1", c('Math', 'Reading', 'Both'), value = "Math", type = "search selection single")
                      ),
                      field(
                       tags$label('Virtual or In-center?'),
                       dropdown_input("virtualOrHome", c('Virtual', 'Center'), value = 'Virtual', type = "search selection single")
                      ),
                      field(
                        tags$label("Parent(s)"),
                        text_input("parent1", value = "",  placeholder = "Parent(s) First and Last Names")
                      )
                    ),
                    fields(
                      class = 'four',
                      field(
                        tags$label("E-Mail"),
                        text_input("email1", value = "", type = "email", placeholder = "E-Mail")
                      ),
                      field(
                        tags$label("Phone"),
                        text_input("phone1", value = "",  placeholder = "Phone")
                      ),
                      field(
                        tags$label("Grade"),
                        dropdown_input("grade1", grades, value = grades[1], type = "search selection single")
                      ),
                      field(
                        tags$label("Birthday"),
                        shiny.semantic::calendar(
                          "birthday",
                          placeholder = 'Birthday',
                          value = today(),
                          max = today()
                        )
                      )
                      ),
                    fields(
                      class = 'four',
                      field(
                        tags$label("First Attendance Day of Week"),
                        dropdown_input("attendDay1", choices = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'),
                                       value = "Monday", type = "search selection single")
                      ),
                      field(
                        tags$label("First Day Time"),
                        airDatepickerInput(
                          inputId = "attendTime1",
                          label = "",
                          timepicker = TRUE,
                          onlyTimepicker = TRUE
                        )
                      ),
                      field(
                        tags$label("Second Attendance Day of Week"),
                        dropdown_input("attendDay2", choices = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'),
                                       value = "Wednesday", type = "search selection single")
                      ),
                      field(
                        tags$label("Second Day Time"),
                        airDatepickerInput(
                          inputId = "attendTime2",
                          label = "",
                          timepicker = TRUE,
                          onlyTimepicker = TRUE
                        )
                      )
                    ),
      
                    field(
                      action_button('newStudent', 'Add student', icon = icon('share'))
                    )
                  )
                )
               )
          
          ),
          list(menu = 'Student Schedule',
               content = div(
                 segment(
                   form(
                     tags$h3('Schedule'),
                     
                     field(
                       
                       uiOutput('studentSchedule')
                       
                       
                     )
                   )
                 )
               )
          ),
          list(menu = 'Employees', 
               content = div(
                 #class = "one column row",
                 segment(
                   form(
                     
                     fields(
                       class = 'three',
                       field(
                         tags$label("Employee"),
                         dropdown_input("employee1", unique(employeeList$employee), value = employeeList$employee[1], type = "search selection single")
                       ),
                       field(
                         tags$label("E-Mail"),
                         textOutput('email3')
                         #text_input("email", value = "", type = "email", placeholder = "E-Mail")
                       ),
                       field(
                         tags$label("Phone"),
                         textOutput('phone3')
                         # text_input("phone", value = "",  placeholder = "Phone")
                       )
                     ),
                     field(
                       action_button('checkin1', 'Check-in', icon = icon('share')),
                       action_button('checkout1', 'Check-out', icon = icon('reply')),
                       tags$label("Hours next payroll period"),
                    
                       textOutput('timeWorked'),
                       tags$label("Payroll must be processed on Wednesday to pay on Friday, so hours worked after Wednesday will accrue to next weeks paycheck.")
                       
                     )
                   )
                 )
               )
          ),
          list(menu = 'New Employee', 
               content = div(
                 #class = "one column row",
                 segment(
                   form(
                     field(
                       tags$label('Admin Password'),
                       passwordInput('adminPassword1', '', value = '')
                     ),
                     fields(
                       class = 'three',
                       field(
                         tags$label("Employee"),
                         text_input("employee2", value = "",  placeholder = "Employee First and Last Name")
                       ),
                       field(
                         tags$label("E-Mail"),
                         text_input("email2", value = "", type = "email", placeholder = "E-Mail")
                       ),
                       field(
                         tags$label("Phone"),
                         text_input("phone2", value = "",  placeholder = "Phone")
                       ),
                       field(
                         tags$label("Add Employee"),
                         
                         action_button('addEmployee', label = '', icon = icon('share'))
                         
                       )
                     )
                   )
                 )
               )
          ),
         
          list(menu = 'Employee Scheduling', 
               content = div(
                 #class = "one column row",
                 segment(
                   form(
                     field(
                       tags$label('Admin Password'),
                       passwordInput('adminPassword', '', value = '')
                     ),
                     tags$h3('Add Shift'),
                     
                     fields(
                       class = 'four',
                       field(
                         tags$label("Employee"),
                         dropdown_input("employee3", unique(employeeList$employee),
                                        value = employeeList$employee[nrow(employeeList)], type = "search selection single")
                       ),
                       field(
                         tags$label('Date'),
                         shiny.semantic::calendar(
                           'scheduleDate',
                           value = today(),
                           placeholder = today(),
                           type = "date"
                         )
                         ),
                       field(
                         tags$label('Start Time'),
                         airDatepickerInput(
                           inputId = "scheduleTime1",
                           label = "",
                           timepicker = TRUE,
                           onlyTimepicker = TRUE
                         )
                         ),
                         # shiny.semantic::calendar(
                         #   'scheduleTime1',
                         #   value = NULL,
                         #   placeholder = NULL,
                         #   type = "time"
                         # ),
                       field(
                         tags$label('End Time'),
                         airDatepickerInput(
                           inputId = "scheduleTime2",
                           label = "",
                           timepicker = TRUE,
                           onlyTimepicker = TRUE
                         )
                       )
                         ),
                     field(
                       action_button('addShift', 'Add To Work Schedule', icon = icon('share'))
                     ),
                     tags$h3('Remove Shift'),
                     fields(
                       class = 'two',
                       field(
                         
                         dropdown_input("removeItem", unique(sched1$body[as.Date(sched1$start) >= today()]),
                                        value = sched1$body[as.Date(sched1$start) >= today()][1], type = "search selection single")
                         ),
                       field(
                       action_button('removeShift', 'Remove From Work Schedule', icon = icon('reply'))
                       )
                       
                     ),
                     field(
                       action_button('sendShift', 'Email to Employees', icon = icon('share'))
                     ),
                     tags$h3('Schedule'),
                     
                     field(
                       
                       uiOutput('schedule')
                       
                       
                     )
                   )
                 )
               )
          ),
          list(
            menu = 'Analytics',
            content = div(
              segment(
                form(
                  highchartOutput('studentCheckins')
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  
  observe({
    if(input$adminPassword ==  pass1){
      shinyjs::enable('addShift')
      shinyjs::enable('removeShift')
      shinyjs::enable('sendShift')
      
    } else {
      shinyjs::disable('addShift')
      shinyjs::disable('removeShift')
      shinyjs::disable('sendShift')
      
    }
  })
  
  observeEvent(input$sendShift, {
    sched <- values$sched1 %>% filter(start >= today()) %>% arrange(start)
    if(nrow(sched) > 0){
    
      plyr::ldply(unique(word(sched$title, 1, sep = fixed(' 2'))), function(x) {
        y <- values$employeeList %>% filter(employee == x) 
        name1 <- word(x, 1)
        z <- sched %>% filter(grepl(x, title))
        z$start <- as.character(format(as.POSIXct(z$start), '%m/%d/%Y %l:%M %p'))
        z$end <- as.character(str_squish(format(as.POSIXct(z$end), '%l:%M %p')))
        z$start <- paste0(z$start,'-',z$end)
        z1 <- str_c(z$start, collapse = '<br>')
        
        txt1 <- glue::glue("<p>Hi {name1}!  I've got you on the schedule for: <br><br>{z1}<br><br>Let me know if this works for you or if I need to make an adjustment.<br> Thanks!<br><br>Linh Davis, Center Director<br>Kumon Spring Valley Village<br>Phone: (713) 930-1277<br>Email: springvalleyvillage_tx@ikumon.com</p>")
        
        msg <- envelope() %>%
          from("springvalleyvillage_tx@ikumon.com") %>%
          to(y$email[1]) %>% subject(glue::glue('Upcoming Work Schedule'))%>% 
          html(txt1) 
        
        smtp(msg, verbose = T)
        
        data.frame()
      })

        title1 <- glue::glue('Work Schedule Sent')
        
        shinyalert(title = as.character(title1), type= 'success')
        
        
      
    }
    
  })
  
  observe({
    if(input$adminPassword1 ==  pass1){
      shinyjs::enable('addEmployee')
    } else {
      shinyjs::disable('addEmployee')
    }
  })
  
  observe({
    if(input$adminPassword2 ==  pass1){
      shinyjs::enable('newStudent')
    } else {
      shinyjs::disable('newStudent')
    }
  })
  
  
  values$deets <- deets
  values$employeeList <- employeeList
  values$sched1 <- sched1
  
  observe( print(input$scheduleTime1))
  
  observeEvent(input$addShift, {
   
    minute1 <- as.character(minute(input$scheduleTime1))
    minute2 <- as.character(minute(input$scheduleTime2))
    if(nchar(minute1) == 1){
      minute1 <- paste0('0', minute1)
    }
    
    if(nchar(minute2) == 1){
      minute2 <- paste0('0', minute2)
    }

    
    values$sched1 <- values$sched1 %>% dplyr::bind_rows(
      (data.frame(calendarId = cal_props1$id[cal_props1$name == input$employee3], title = paste0(input$employee3, ' ', input$scheduleDate), body = paste0(input$employee3, ' ', input$scheduleDate),
               start = paste0(input$scheduleDate,' ', hour(input$scheduleTime1), ':',minute1, ':00'),
               end = paste0(input$scheduleDate, ' ', hour(input$scheduleTime2), ':', minute2, ':00'),
               category = 'time')
      )
      
      
    )
    shinyalert(title = 'Added to Schedule', type= 'success')
    
    
    aws.s3::s3saveRDS(values$sched1, 'schedule.rds', bucket = 'kumonsvv')
    
    update_dropdown_input(session, 'removeItem', choices = unique(values$sched1$body[as.Date(values$sched1$start) >= today()]))
    
  })
  
  observeEvent(input$removeShift, {
    
    values$sched1 <- values$sched1 %>% filter(!body %in% input$removeItem)
    
    aws.s3::s3saveRDS(values$sched1, 'schedule.rds', bucket = 'kumonsvv')
    
    update_dropdown_input(session, 'removeItem', choices = unique(values$sched1$body[as.Date(values$sched1$start) >= today()]))
    shinyalert(title = 'Removed from Schedule', type= 'success')
    
  })
  
  observe({
    
    #sched <- values$sched1
    
    output$schedule <- renderUI(
      toastui::calendar(values$sched1, view = "week", defaultDate = Sys.Date(), useCreationPopup = F,
                      isReadOnly = T, navigation = T) %>% 
      cal_week_options(
        startDayOfWeek = 1,
        workweek = TRUE
      ) %>% 
      cal_props(cal_props1)
    )
  })
  
  observe({
    
    
    
    
    studentSched <- values$deets
    
    studentSched <- data.frame(days1 = c(seq(-5,6,1))) %>% mutate(date1 = today() %m+% days(days1)) %>%
      mutate(wday = weekdays(date1)) %>% left_join(
        studentSched %>% left_join(cal_props2 %>% select(calendarId = id, students = name)) %>% rename(title = students) %>%
          rename(wday = attendDay1)
      ) %>% filter(!is.na(calendarId)) %>% 
      mutate(start = paste0(date1, ' ', attendTime1), 
             end = if_else(subject == 'Both', as.character(as.POSIXct(start)+3600),
                           as.character(as.POSIXct(start)+1800)),
             category = 'time', body = title) %>% 
      select(calendarId, title, body, start, end, category) %>%
      dplyr::bind_rows(
        data.frame(days1 = c(seq(-5,6,1))) %>% mutate(date1 = today() %m+% days(days1)) %>%
          mutate(wday = weekdays(date1)) %>% left_join(
            studentSched %>% left_join(cal_props2 %>% select(calendarId = id, students = name)) %>% rename(title = students) %>%
              rename(wday = attendDay2)
          ) %>% filter(!is.na(calendarId)) %>% 
          mutate(start = paste0(date1, ' ', attendTime2), 
                 end = if_else(subject == 'Both', as.character(as.POSIXct(start)+3600),
                               as.character(as.POSIXct(start)+1800)),
                 category = 'time', body = title) %>% 
          select(calendarId, title, body, start, end, category)
      )
    
    output$studentSchedule <- renderUI(
      toastui::calendar(studentSched, view = "week", defaultDate = Sys.Date(), useCreationPopup = F,
                        isReadOnly = T, navigation = T) %>% 
        cal_week_options(
          startDayOfWeek = 1,
          workweek = TRUE
        ) %>% 
        cal_props(cal_props2)
    )
    
  })
  
  
  observe({
   # updateTextInput(session, 'email', value = values$deets$email[values$deets$students == input$student])
  
    #updateTextInput(session, 'phone', value = values$deets$phone[values$deets$students == input$student])
    output$phone <- renderText( values$deets$phone[values$deets$students == input$student])
    output$email <- renderText( values$deets$email[values$deets$students == input$student])
    
    output$subject <- renderText( values$deets$subject[values$deets$students == input$student])
    output$grade <- renderText( values$deets$grade[values$deets$students == input$student])
    output$parent <- renderText( values$deets$parent[values$deets$students == input$student])
    
    output$birthday <- renderText( as.character(values$deets$birthday[values$deets$students == input$student]))
    output$virtualHome <- renderText( as.character(values$deets$virtualHome[values$deets$students == input$student]))
    output$sessions <- renderText(
      as.character(paste0(values$deets$attendDay1[values$deets$students == input$student], ' ', 
                          values$deets$attendTime1[values$deets$students == input$student], '-', 
                          values$deets$attendDay2[values$deets$students == input$student], ' ', 
                          values$deets$attendTime2[values$deets$students == input$student])
    ))
    
    output$mathLabel <- renderText('Math take-home sheets')
    output$readingLabel <- renderText('Reading take-home sheets')
  })
  
  observe({
    # updateTextInput(session, 'email', value = values$deets$email[values$deets$students == input$student])
    
    #updateTextInput(session, 'phone', value = values$deets$phone[values$deets$students == input$student])
    output$phone3 <- renderText( values$employeeList$phone[values$employeeList$employee == input$employee1])
    output$email3 <- renderText( values$employeeList$email[values$employeeList$employee == input$employee1])
    
  
  })
  
  observe({
    req(values$deets)
    if(values$deets$subject[values$deets$students == input$student] == 'Math'){
      shinyjs::show('mathLabel')
      
      shinyjs::show('mathLevel')
      shinyjs::show('mathNumber1')
      shinyjs::show('mathNumber2')
      shinyjs::hide('readingLabel')
      
      shinyjs::hide('readingLevel')
      shinyjs::hide('readingNumber1')
      shinyjs::hide('readingNumber2')
      
    } else if(values$deets$subject[values$deets$students == input$student] == 'Reading'){
      shinyjs::hide('mathLabel')
      
       shinyjs::hide('mathLevel')
      shinyjs::hide('mathNumber1')
      shinyjs::hide('mathNumber2')
      
      shinyjs::show('readingLabel')
      
      shinyjs::show('readingLevel')
      shinyjs::show('readingNumber1')
      shinyjs::show('readingNumber2')
      
    } else {
      shinyjs::show('mathLabel')
      
      shinyjs::show('mathLevel')
      shinyjs::show('mathNumber1')
      shinyjs::show('mathNumber2')
      shinyjs::show('readingLabel')
      
      shinyjs::show('readingLevel')
      shinyjs::show('readingNumber1')
      shinyjs::show('readingNumber2')
      
    }
  })
  
  observeEvent(input$newStudent, {
    chk1 <- data.frame(students = input$student1, phone = input$phone1, email = input$email1, subject = input$subject1,
                       virtualHome = input$virtualOrHome,
                       parent = input$parent1,
                       grade = input$grade1,
                       birthday = as.Date(input$birthday),
                       attendDay1 = input$attendDay1,
                       attendDay2 = input$attendDay2,
                       attendTime1 = paste0(hour(input$attendTime1),':',minute(input$attendTime1), ':00'),
                       attendTime2 = paste0(hour(input$attendTime2),':',minute(input$attendTime2), ':00')
                       )
    deets <- aws.s3::s3readRDS('deets.rds', bucket = 'kumonsvv') %>%
      dplyr::bind_rows(chk1)
    
    aws.s3::s3saveRDS(deets, 'deets.rds', bucket = 'kumonsvv')
    
    values$deets <- deets
    update_dropdown_input(session, 'student', choices = unique(deets$students))
    title1 <- glue::glue('{input$student1} Added')
    shinyalert(title = as.character(title1), type= 'success')
    
  })
  
  observeEvent(input$addEmployee, {
    chk1 <- data.frame(employee = input$employee2, phone = input$phone2, email = input$email2)
    chk1 <- aws.s3::s3readRDS('employeeList.rds', bucket = 'kumonsvv') %>%
      anti_join(chk1 %>% select(employee)) %>%
      dplyr::bind_rows(chk1) %>% distinct()
    
    aws.s3::s3saveRDS(chk1, 'employeeList.rds', bucket = 'kumonsvv')
    
    values$employeeList <- chk1
    update_dropdown_input(session, 'employee1', choices = unique(chk1$employee))
    update_dropdown_input(session, 'employee3', choices = unique(chk1$employee))
    title1 <- glue::glue('{input$employee2} Added')
    
    shinyalert(title = as.character(title1), type= 'success')
    
  })
  
  observeEvent(input$student, {
    
    update_action_button(session, 'checkin', icon = icon('share'))
    update_action_button(session, 'checkout', icon = icon('reply'))
  })
  
  observeEvent(input$employee1, {
    
    update_action_button(session, 'checkin1', icon = icon('share'))
    update_action_button(session, 'checkout1', icon = icon('reply'))
  })
  
  values$timer <- timer
  
  next_whole <- lubridate::ceiling_date(Sys.time(), "minute")
  #print(format(Sys.time(), "%H:%M:%S"))
  go_singal <- reactiveVal(FALSE)
  
  first_check <- observe({
    invalidateLater(1000)
    req(next_whole - Sys.time() < 0)
    go_singal(TRUE)
    first_check$destroy()
    
  })
  
  

    
    output$student_table <-renderText({
      if(!go_singal()){
        time1 <- timer %>%
          mutate(remTime = round(as.numeric(endTime - Sys.time()), 0))
        
        timer %>% rename(Student = students, Subject = subject) %>%
          mutate(remTime = round(as.numeric(endTime - Sys.time()), 0),
                 endTime = format(endTime, '%l:%M %p')) %>%
          select(Student, Subject, `End Time` = endTime, remTime) %>%
          rename(`Remaining Minutes` = remTime) %>%
          knitr::kable("html", row.names = F) %>%
          kable_styling("striped", full_width = T)%>%
          row_spec(which(time1$remTime >= 15), bold = T, color = "white", background = "green")%>%
          
          row_spec(which(time1$remTime < 15 & time1$remTime >= 5), bold = T, color = "white", background = "orange")%>%
          row_spec(which(time1$remTime < 5), bold = T, color = "white", background = "red")
      } else {
        
        invalidateLater(30*1000, session)
        if(nrow(values$timer) > 0){
          time1 <- values$timer %>%
            mutate(remTime = round(as.numeric(endTime - Sys.time()), 0))
          
          values$timer %>% rename(Student = students, Subject = subject) %>%
            mutate(remTime = round(as.numeric(endTime - Sys.time()), 0),
                   endTime = format(endTime, '%l:%M %p')) %>%
            select(Student, Subject, `End Time` = endTime, remTime) %>%
            rename(`Remaining Minutes` = remTime) %>%
            knitr::kable("html", row.names = F) %>%
            kable_styling("striped", full_width = T)%>%
            row_spec(which(time1$remTime >= 15), bold = T, color = "white", background = "green")%>%
            
            row_spec(which(time1$remTime < 15 & time1$remTime >= 5), bold = T, color = "white", background = "orange")%>%
            row_spec(which(time1$remTime < 5), bold = T, color = "white", background = "red")
        }
      }
    })

  
  observeEvent(input$checkin, {
    update_action_button(session, 'checkin', icon = icon('thumbs up outline'))
    
    chk1 <- values$deets %>% filter(students == input$student) %>% mutate(checkin = Sys.time()) %>%
      mutate(checkout = Sys.time(), 
             mathLevel = as.character(NA),
             mathNumber1 = as.character(NA),
             mathNumber2 = as.character(NA),
             readingLevel = as.character(NA),
             readingNumber1 = as.character(NA),
             readingNumber2 = as.character(NA))
    
    values$timer <- values$timer %>% dplyr::bind_rows(
      chk1 %>% select(students, checkin, subject) %>%
        mutate(totalTime = if_else(subject == 'Both', 60, 30)) %>%
        mutate(endTime = checkin %m+% minutes(totalTime))
    )
    
    aws.s3::s3saveRDS(values$timer, 'timer.rds', bucket = 'kumonsvv')
    
    
    loginDeets <- aws.s3::s3readRDS('loginDeets.rds', bucket = 'kumonsvv') %>%
      dplyr::bind_rows(chk1)
    
    aws.s3::s3saveRDS(loginDeets, 'loginDeets.rds', bucket = 'kumonsvv')
    
    title1 <- glue::glue('{input$student} Checked In')
    
    shinyalert(title = as.character(title1), type= 'success')
    
  })
  
  observeEvent(input$checkout, {
    update_action_button(session, 'checkout', icon = icon('thumbs up outline'))
    loginDeets <- aws.s3::s3readRDS('loginDeets.rds', bucket = 'kumonsvv') %>%
      mutate(checkout = if_else(students == input$student & day(checkin) == day(today()), Sys.time(), checkout),
             mathLevel = if_else(students == input$student & day(checkin) == day(today()) & !subject %in% 'Reading', input$mathLevel, mathLevel),
             mathNumber1 = if_else(students == input$student & day(checkin) == day(today()) & !subject %in% 'Reading', input$mathNumber1, mathNumber1),
             mathNumber2 = if_else(students == input$student & day(checkin) == day(today()) & !subject %in% 'Reading', input$mathNumber2, mathNumber2),
             readingLevel = if_else(students == input$student & day(checkin) == day(today()) & !subject %in% 'Math', input$readingLevel, readingLevel),
             readingNumber1 = if_else(students == input$student & day(checkin) == day(today()) & !subject %in% 'Math', input$readingNumber1, readingNumber1),
             readingNumber2 = if_else(students == input$student & day(checkin) == day(today()) & !subject %in% 'Math', input$readingNumber2, readingNumber2)
             )
    
    values$timer <- values$timer %>% filter(!students %in% input$student)
    aws.s3::s3saveRDS(values$timer, 'timer.rds', bucket = 'kumonsvv')
    
    login1 <- loginDeets %>% filter(students == input$student & day(checkin) == day(today()))
    
    name1 <- word(login1$students[1], 1)
    if(login1$subject[1] == 'Both'){
      subj1 <- glue::glue('Math Level {login1$mathLevel[1]} {login1$mathNumber1[1]}-{login1$mathNumber2[1]} and Reading Level {login1$readingLevel[1]} {login1$readingNumber1[1]}-{login1$readingNumber2[1]}')
    } else if(login1$subject[1] == 'Math'){
      subj1 <- glue::glue('Math Level {login1$mathLevel[1]} {login1$mathNumber1[1]}-{login1$mathNumber2[1]}')
    } else {
      subj1 <- glue::glue('Reading Level {login1$readingLevel[1]} {login1$readingNumber1[1]}-{login1$readingNumber2[1]}')
    }
    
    txt1 <- glue::glue("<p>Thank you for visiting us today! {name1} should be taking home {subj1}.  We look forward to seeing ya'll next time.<br><br>Kumon Spring Valley Village<br>Phone: (713) 930-1277<br>Email: springvalleyvillage_tx@ikumon.com</p>")
    
    msg <- envelope() %>%
      from("springvalleyvillage_tx@ikumon.com") %>%
      to(login1$email[1]) %>% subject(glue::glue('{login1$students[1]}-{as.Date(login1$checkin[1])} Kumon visit'))%>% 
      html(txt1) 
    if(input$sendCheckout == T){
    
      smtp(msg, verbose = T)
    }
    
    aws.s3::s3saveRDS(loginDeets, 'loginDeets.rds', bucket = 'kumonsvv')
    
    title1 <- glue::glue('{input$student} Checked Out')
    
    shinyalert(title = as.character(title1), type= 'success')
    
  })
  
  
  observeEvent(input$checkin1, {
    update_action_button(session, 'checkin1', icon = icon('thumbs up outline'))
    
    chk1 <- values$employeeList %>% filter(employee == input$employee1) %>% mutate(checkin = Sys.time()) %>%
      mutate(checkout = Sys.time())
    
    loginDeetsEmployee <- aws.s3::s3readRDS('loginDeetsEmployee.rds', bucket = 'kumonsvv') %>%
      dplyr::bind_rows(chk1) %>% distinct()
    
    aws.s3::s3saveRDS(loginDeetsEmployee, 'loginDeetsEmployee.rds', bucket = 'kumonsvv')
    title1 <- glue::glue('{input$employee1} Checked In')
    
    shinyalert(title = as.character(title1), type= 'success')
  })
  
  observe(
    {
      time1 <- aws.s3::s3readRDS('loginDeetsEmployee.rds', bucket = 'kumonsvv') 
      time1$dayOfWeek <- wday(time1$checkin)
      time1$wk <- week(time1$checkin)
      time1$yr <- year(time1$checkin)
      
      if(wday(today()) >= 5){
        wk1 <- week(today())+1
      }
      
      time1 <- time1 %>% filter((wk == wk1 & yr == max(yr) & dayOfWeek < 5)|
                         (wk == (wk1-1) & yr == max(yr) & dayOfWeek >= 5)) %>%
        subset(select = -c(dayOfWeek, wk, yr))%>%
        filter(employee == input$employee1) %>% mutate(time1 = checkout-checkin) %>% 
        summarise(time = sum(as.numeric(time1))) %>% ungroup() %>%
        mutate(time = paste0(round(time, 2), ' hours'))
      
      output$timeWorked <- renderText(time1$time)
      
    }
  )
  
  observeEvent(input$checkout1, {
    update_action_button(session, 'checkout1', icon = icon('thumbs up outline'))
    loginDeetsEmployee <- aws.s3::s3readRDS('loginDeetsEmployee.rds', bucket = 'kumonsvv') %>%
       mutate(checkout = if_else(employee == input$employee1 & day(checkin) == day(today()), Sys.time(), checkout)
      )
    
    aws.s3::s3saveRDS(loginDeetsEmployee, 'loginDeetsEmployee.rds', bucket = 'kumonsvv')
    
    time1 <- aws.s3::s3readRDS('loginDeetsEmployee.rds', bucket = 'kumonsvv') %>%
      filter(employee == input$employee1) %>% mutate(time1 = checkout-checkin) %>% 
      mutate(wk = week(checkin), yr = year(checkin)) %>% filter(wk == week(today())) %>% 
      filter(yr == year(today())) %>% summarise(time = sum(as.numeric(time1))) %>% ungroup() %>%
      mutate(time = paste0(round(time, 2), ' hours'))
    
    output$timeWorked <- renderText(time1$time)
    
    title1 <- glue::glue('{input$employee1} Checked Out')
    
    shinyalert(title = as.character(title1), type= 'success')
  })
  
  output$studentCheckins <- renderHighchart(
    highchart() %>% 
      hc_add_series(aws.s3::s3readRDS('loginDeets.rds', bucket = 'kumonsvv')  %>% 
                      mutate(count = if_else(subject == 'Both', 2, 1)) %>%
                      mutate(checkin = as.Date(checkin))%>% 
                      select(students, count, checkin) %>% distinct(), type = 'column',
                    hcaes(x = checkin, y = count, group = students), stacking = 'normal') %>%
      hc_xAxis(type = 'datetime',
               labels = list(style = list(fontFamily = 'Roboto', fontSize = '9pt'))) %>%
      hc_title(text = 'Student check-in tracker', align = 'left',
               style = list(fontFamily = 'Roboto', fontSize = '12pt')) %>%
      hc_yAxis(title = list(text = 'Subjects checked-in', style = list(fontFamily = 'Roboto', fontSize = '9pt'))) %>%
      hc_legend(verticalAlign = 'top', align = 'right', itemStyle = list(fontFamily = 'Roboto', fontSize = '8pt'))
  )
  
})

shiny::shinyApp(ui, server)