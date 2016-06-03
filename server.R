library(shiny)
library(magrittr)
library(grDevices)
library(DT)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(sqldf)
library(digest)
library(DBI)
# library(RMySQL)
library(RSQLite)
require(RH2)


source("helper.R")

shinyServer(function(input, output) {
  
  ## Data Upload ##
  
  output$contents <- DT::renderDataTable({

    inFile <- input$file1
    tableName <- input$text
    
    if (is.null(inFile)) return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    
    table <- read.csv(inFile$datapath, header = input$header,
                      sep = input$sep, quote = input$quote)
    # 
    # RSQLite::dbWriteTable(conn = db, name = tableName, value = table,
    #                       row.names = FALSE, overwrite = TRUE, append = FALSE)
    
  })
  
  observeEvent(input$submit, {

      # Connect to the database
      db <- dbConnect(SQLite(), dbname= 'Achievement')
      
      inFile <- input$file1
      tableName <- input$text
      
      if (is.null(inFile)) return(NULL)
      
      read.csv(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
      
      table <- read.csv(inFile$datapath, header = input$header,
                        sep = input$sep, quote = input$quote)
      
      # Write the table
      RSQLite::dbWriteTable(conn = db, name = tableName, value = table,
                            row.names = FALSE, overwrite = TRUE, append = FALSE)
      
      # Disconnect from the database
      dbDisconnect(db)
  })
  
  # Pass/Fail function
  table_desc_all <- reactive({
    
    validate(
      need(try(input$sum != All), "Please make a selection")
    )
    

    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$sum)

    DATA <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    DATA %>%
      mutate(Level = factor(Level)) %>%
      group_by(Level, Student) %>%
      summarise(MeanTotal = round(mean(Composite.Score, na.rm=TRUE),2)) %>%
      mutate(Decision = ifelse(round(MeanTotal) >= 80, "Yes", "No")) %>%
      mutate(Pass = ifelse(round(MeanTotal) >= 80, "Yes", 'No')) %>%
      mutate(Fail = ifelse(round(MeanTotal) >= 80, 'No', "Yes")) %>%
      group_by(Level) %>%
      summarise(Pass = sum(length(which(Pass == "Yes"))),
                Fail = sum(length(which(Fail == "Yes"))))
    
  })
  
  # Test descriptives, all
  descriptives_all <- reactive({
    
    validate(
      need(try(input$sum != All), "Please make a selection")
    )

    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$sum)
    
    DATA <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    
    
    DATA %>%
      summarise(
        'N' = length(unique(Student)),
        'Evaluation' = round(mean(Teacher.Evaluation, na.rm = TRUE),2),
        'Course Grade' = round(mean(Class.Score, na.rm = TRUE),2),
        'Final Exam' = round(mean(Assessment.Score, na.rm = TRUE),2),
        'Total' = round(mean(Composite.Score, na.rm = TRUE),2)
      ) %>%
      t() %>%
      as.data.frame() -> Mean
    
    DATA %>%
      summarise(
        'N' = length(unique(Student)),
        'Evaluation' = round(sd(Teacher.Evaluation, na.rm = TRUE),2),
        'Course Grade' = round(sd(Class.Score, na.rm = TRUE),2),
        'Final Exam' = round(sd(Assessment.Score, na.rm = TRUE),2),
        'Total' = round(sd(Composite.Score, na.rm = TRUE),2)
      ) %>%
      t() %>%
      as.data.frame() -> SD
    
    Measure <- data.frame('Measure' = c('N', 'Evaluation', 'Course Grade', 'Final Exam', 'Total'))
    
    Desc <- bind_cols(Measure, Mean, SD)
    
    colnames(Desc)[2:3] <- c('Mean', 'SD')
    
    Desc 
    
  })
  
  # Descriptives by level
  descriptives_level <- reactive({
    
    validate(
      need(try(input$sum != All), "Please make a selection")
    )

    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$sum)

    DATA <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    DATA %>%
      # group_by(Level) %>%
      filter(Level == input$level) %>%
      summarise(
        'N' = length(unique(Student)),
        'Evaluation' = round(mean(Teacher.Evaluation, na.rm = TRUE),2),
        'Course Grade' = round(mean(Class.Score, na.rm = TRUE),2),
        'Final Exam' = round(mean(Assessment.Score, na.rm = TRUE),2),
        'Total' = round(mean(Composite.Score, na.rm = TRUE),2)
      ) %>%
      t() %>%
      as.data.frame() -> Mean
    
    DATA %>%
      # group_by(Level) %>%
      filter(Level == input$level) %>%
      summarise(
        'N' = length(unique(Student)),
        'Evaluation' = round(sd(Teacher.Evaluation, na.rm = TRUE),2),
        'Course Grade' = round(sd(Class.Score, na.rm = TRUE),2),
        'Final Exam' = round(sd(Assessment.Score, na.rm = TRUE),2),
        'Total' = round(sd(Composite.Score, na.rm = TRUE),2)
      ) %>%
      t() %>%
      as.data.frame() -> SD
    
    Measure <- data.frame('Measure' = c('N', 'Evaluation', 'Course Grade', 'Final Exam', 'Total'))
    
    Desc <- bind_cols(Measure, Mean, SD)
    
    colnames(Desc)[2:3] <- c('Mean', 'SD')
    
    Desc 
    
  })
  
  plot_desc_all <- reactive({
    
    validate(
      need(try(input$sum != All), "Please make a selection")
    )
    
    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$sum)

    DATA <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    DATA %>%
      mutate(Level = factor(Level)) %>%
      group_by(Level, Student) %>%
      summarise(MeanTotal = round(mean(Composite.Score, na.rm=TRUE),2)) %>%
      mutate(Decision = ifelse(round(MeanTotal) >= 80, "Pass", "Fail")) %>%
      mutate(Pass = ifelse(round(MeanTotal) >= 80, "Yes", 'No')) %>%
      mutate(Fail = ifelse(round(MeanTotal) >= 80, 'No', "Yes")) %>%
      group_by(Level) %>%
      ggplot(aes(x=Decision, fill=Decision))+
      geom_bar()+
      facet_wrap(~Level)+
      theme_bw()+
      theme(text=element_text(size=18)) +
      theme(plot.background = element_rect(fill="transparent", colour=NA),
            panel.background = element_rect(fill="transparent", colour=NA),
            legend.position = 'none') +
      ylab("Count")
  })
  
  plot_dist_all <- reactive({
    
    validate(
      need(try(input$sum != All), "Please make a selection")
    )

    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$sum)

    DATA <- dbGetQuery(db, query)
    dbDisconnect(db)
        
    DATA %>%
      gather(Measure, Score, c(5:8)) %>%
      ggplot(aes(x = Score)) + 
      geom_area(aes(y = (..count..), colour=Measure, fill=Measure), stat='bin', binwidth = 5, position = "stack", alpha = .5) + 
      geom_vline(xintercept = 80) +
      scale_x_continuous(limits = c(0, 100), breaks=c(20, 40, 60, 80, 100)) +
      theme_bw() +
      theme(text=element_text(size=18)) +
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.position = 'none')+
      # ggtitle("Score Distributions") +
      facet_wrap(~Measure) +
      ylab("Count")
    
  })
  
  plot_dist <- reactive({
    
    validate(
      need(try(input$sum != All), "Please make a selection")
    )

    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$sum)

    DATA <- dbGetQuery(db, query)
    dbDisconnect(db)
        
    DATA %>%
      group_by(Level) %>%
      filter(Level == input$level) %>%
      gather(Measure, Score, c(5:8)) %>%
      ggplot(aes(x = Score)) + 
      geom_area(aes(y = (..count..), colour=Measure, fill=Measure), stat='bin', binwidth = 5, position = "stack", alpha = .5) + 
      geom_vline(xintercept = 80) +
      scale_x_continuous(limits = c(0, 100), breaks=c(20, 40, 60, 80, 100)) +
      theme_bw() +
      theme(text=element_text(size=18)) +
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.position = 'none')+
      # ggtitle("Score Distributions") +
      facet_wrap(~Measure) +
      ylab("Count")
    
  })
  
  
  
  
  # Distribution plot output
  output$dist <- renderPlot({
    
    validate(
      need(try(input$level != All), "Please make a selection")
    )
    
    switch (input$level,
            
            'All' = plot_dist_all(),
            '6' = plot_dist(),
            '5' = plot_dist(),
            '4' = plot_dist(),
            '3' = plot_dist(),
            '2' = plot_dist(),
            '1' = plot_dist()
    )
    
  }, bg="transparent")
  
  # Decision plot output
  output$desc <- renderPlot({
    
    validate(
      need(try(input$level != All), "Please make a selection")
    )
    
    switch (input$level,
            
            'All' = plot_desc_all()
            # 'Six' = plot_dist(),
            # 'Five' = plot_dist(),
            # 'Four' = plot_dist(),
            # 'Three' = plot_dist(),
            # 'Two' = plot_dist(),
            # 'One' = plot_dist()
    )
    
  }, bg="transparent")
  
  # Progression tale
  output$progressionTable <- DT::renderDataTable(
    

    switch (input$level,
  
            'All' = DT::datatable(
              descriptives_all(),
              rownames = FALSE,
              caption = "Descriptive statistics",
              options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE)
            ) %>%
              formatStyle(names(descriptives_all()), fontWeight = 'bold'),
            
            '6' = DT::datatable(
              descriptives_level(),
              rownames = FALSE,
              caption = "Descriptive statistics",
              options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE)
            ) %>%
              formatStyle(names(descriptives_level()), fontWeight = 'bold'),
            
            '5' = DT::datatable(
              descriptives_level(),
              rownames = FALSE,
              caption = "Descriptive statistics",
              options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE)
            ) %>%
              formatStyle(names(descriptives_level()), fontWeight = 'bold'),
            
            '4' = DT::datatable(
              descriptives_level(),
              rownames = FALSE,
              caption = "Descriptive statistics",
              options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE)
            ) %>%
              formatStyle(names(descriptives_level()), fontWeight = 'bold'),
            
            '3' = DT::datatable(
              descriptives_level(),
              rownames = FALSE,
              caption = "Descriptive statistics",
              options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE)
            ) %>%
              formatStyle(names(descriptives_level()), fontWeight = 'bold'),
            
            '2' = DT::datatable(
              descriptives_level(),
              rownames = FALSE,
              caption = "Descriptive statistics",
              options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE)
            ) %>%
              formatStyle(names(descriptives_level()), fontWeight = 'bold'),
            
            '1' = DT::datatable(
              descriptives_level(),
              rownames = FALSE,
              caption = "Descriptive statistics",
              options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE)
            ) %>%
              formatStyle(names(descriptives_level()), fontWeight = 'bold')
            
            
    ))
  
  # Pass/Fail table
  output$passfailTable <- DT::renderDataTable(
    
    switch (input$level,
            
            'All' = DT::datatable(
              table_desc_all(),
              rownames = FALSE,
              caption = "Pass/Fail Decisions by level",
              options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE)
            ) %>%
              formatStyle(names(table_desc_all()), fontWeight = 'bold')
    )
  )
  
  output$course <- renderUI({
    
    if (input$courseLevels == "Make a choice") {
      return("Please select a level")
    } 
    
    switch(input$courseLevel,
           
           'Grammar' = selectInput("dynamic", "Select Test", choices = c("Make a choice", gramT), selected = "Make a choice"),
           'Reading' = selectInput("dynamic", "Select Test", choices = c("Make a choice",readT), selected = "Make a choice"),
           'Writing Adv' = selectInput("dynamics", "Select Test", choices = c("Make a choice",writT), selected = "Make a choice"),
           'Writing Int' = selectInput("dynamics", "Select Test", choices = c("Make a choice",writT), selected = "Make a choice"),
           'Writing Basic' = selectInput("dynamics", "Select Test", choices = c("Make a choice",writT), selected = "Make a choice"),
           'Listening/Speaking' = selectInput("dynamic", "Select Test", c("Make a choice",choices = lstspT), selected = "Make a choice")
    )
  })
  
  output$courseS <- renderUI({

    if (input$courseLevels == "Make a choice") {
      return("Please select a level")
    } 

    switch(input$courseLevels,
           
           'Grammar' = selectInput("dynamics", "Select Test", choices = c("Make a choice", gramT), selected = "Make a choice"),
           'Reading' = selectInput("dynamics", "Select Test", choices = c("Make a choice",readT), selected = "Make a choice"),
           'Writing Adv' = selectInput("dynamics", "Select Test", choices = c("Make a choice",writT), selected = "Make a choice"),
           'Writing Int' = selectInput("dynamics", "Select Test", choices = c("Make a choice",writT), selected = "Make a choice"),
           'Writing Basic' = selectInput("dynamics", "Select Test", choices = c("Make a choice",writT), selected = "Make a choice"),
           'Listening/Speaking' = selectInput("dynamics", "Select Test", c("Make a choice",choices = lstspT), selected = "Make a choice")
    )
  })
  
  
  # Begin functions and outputs for Test summaries
  MODE <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  MEAN <- function(x){
    round(mean(x, na.rm = TRUE),2)
  }
  
  STDEV <- function(x){
    round(sd(x, na.rm = TRUE),2)
  }
  
  MEDIAN <- function(x){
    round(median(x, na.rm = TRUE),2)
  }
  
  MIN <- function(x){
    round(min(x, na.rm = TRUE),2)
  }
  
  MAX <- function(x){
    round(max(x, na.rm = TRUE),2)
  }
  
  SUM <- function(x){
    round(sum(x, na.rm = TRUE),2)
  }
  
  level_skill_summary <- reactive({
    
    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$sum)
    
    DATA <- dbGetQuery(db, query)

    Alphaquery <- sprintf("SELECT * FROM %s", input$dynamic)
    
    Alpha_data <- dbGetQuery(db, Alphaquery)
    dbDisconnect(db)
    
    DATA %>%
      filter(form_name == input$dynamic) %>%
      summarise('Mean Score' = round(MEAN(Assessment.Score),2),
                'Median Score' = round(MEDIAN(Assessment.Score),2),
                'Standard Deviation' = round(STDEV(Assessment.Score),2),
                'Minimum Score' = round(MIN(Assessment.Score),2),
                'Maximum Score' = round(MAX(Assessment.Score),2)) -> RawScore
    
    Alpha_data %>%
      select(-c(1:5)) %>%
      psychometric::alpha() %>%
      round(2) -> Alpha_raw

    Alpha_raw <- data.frame('Alpha' = Alpha_raw)
    
    Summary <- bind_cols(RawScore, Alpha_raw)
    Summary
    
  })
  
  level_skill_stemplot <- reactive({
    
    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$sum)

    DATA <- dbGetQuery(db, query)
    dbDisconnect(db)
    

    DATA %>%
      filter(form_name == input$dynamic) %>%
      filter(!is.na(Assessment.Score)) %>%
      mutate(Assessment.Score = round(Assessment.Score)) %$%
      capture.output(stem(Assessment.Score, scale = 1)) %>%
      data.frame(.,rr=1:length(.)) %>%
      ggplot() + 
      geom_text(aes(x=rr, y=0, label=.), size = 6, fontface = 'bold', hjust=0) +
      coord_flip()+ theme_classic() +
      scale_x_discrete(breaks=NULL) +
      scale_y_discrete(breaks=NULL, limits=c(0,1)) +
      theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          axis.line=element_blank())
  })
  
  # Begin writing summary
  level_write_summary_adv <- reactive({
    
    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$dynamics)
    
    DATA <- dbGetQuery(db, query)
    dbDisconnect(db)    
    
    ## Tidy up data
    
    DATA %>%
      tbl_df() %>%
      select(-contains('_R3'), -contains('Level'), 
             -contains('Skill'), -contains('Section'),
             -contains('Name'), -contains('Course'),
             -contains('Cover_Letter'), -contains('Major_Assignment'),
             -contains('Choices'), -contains('Num_missing')) -> data
    
    data %>%
      gather('key', 'value', -ID) %>%
      group_by(ID) %>%
      summarise(Total = MEAN(value)) %>%
      summarise('Measure' = 'Total',
                'Mean Score' = MEAN(Total),
                'Median Score' = MEDIAN(Total),
                'Standard Deviation' = STDEV(Total),
                'Minimum Score' = MIN(Total),
                'Maximum Score' = MAX(Total))-> WriteTotRaw
    
    data %>%
      select(contains('CD'), contains('ID')) %>%
      gather('key', 'value', -ID) %>%
      group_by(ID) %>%
      summarise(Total = MEAN(value)) %>%
      summarise('Measure' = 'Content',
                'Mean Score' = MEAN(Total),
                'Median Score' = MEDIAN(Total),
                'Standard Deviation' = STDEV(Total),
                'Minimum Score' = MIN(Total),
                'Maximum Score' = MAX(Total))-> ContRaw
    
    data %>%
      select(contains('ORG'), contains('ID')) %>%
      gather('key', 'value', -ID) %>%
      group_by(ID) %>%
      summarise(Total = MEAN(value)) %>%
      summarise('Measure' = 'Organization',
                'Mean Score' = MEAN(Total),
                'Median Score' = MEDIAN(Total),
                'Standard Deviation' = STDEV(Total),
                'Minimum Score' = MIN(Total),
                'Maximum Score' = MAX(Total))-> OrgRaw
    
    data %>%
      select(contains('LU'), contains('ID')) %>%
      gather('key', 'value', -ID) %>%
      group_by(ID) %>%
      summarise(Total = MEAN(value)) %>%
      summarise('Measure' = 'Language Use',
                'Mean Score' = MEAN(Total),
                'Median Score' = MEDIAN(Total),
                'Standard Deviation' = STDEV(Total),
                'Minimum Score' = MIN(Total),
                'Maximum Score' = MAX(Total))-> LuRaw
    
    data %>%
      select(contains('SU'), contains('ID')) %>%
      gather('key', 'value', -ID) %>%
      group_by(ID) %>%
      summarise(Total = MEAN(value)) %>%
      summarise('Measure' = 'Source Use',
                'Mean Score' = MEAN(Total),
                'Median Score' = MEDIAN(Total),
                'Standard Deviation' = STDEV(Total),
                'Minimum Score' = MIN(Total),
                'Maximum Score' = MAX(Total))-> SuRaw
    
    
    WriteScore <- bind_rows(WriteTotRaw, ContRaw, OrgRaw, LuRaw, SuRaw)
    WriteScore
    
  })
  
  level_write_summary_int <- reactive({
    
    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$dynamics)
    
    DATA <- dbGetQuery(db, query)
    dbDisconnect(db)    
    
    ## Tidy up data
    DATA %>%
      tbl_df() %>%
      select(-contains('_R3'), -contains('Level'), 
             -contains('Skill'), -contains('Section'),
             -contains('Name'), -contains('Course'),
             -contains('_AVE'), -contains('_tot')) -> data
    
    data %>%
      select(-contains('T2_')) %>%
      gather('key', 'value', -ID) %>%
      group_by(ID) %>%
      summarise(Total = MEAN(value)) %>%
      summarise('Measure' = 'Task 1 Total',
                'Mean Score' = MEAN(Total),
                'Median Score' = MEDIAN(Total),
                'Standard Deviation' = STDEV(Total),
                'Minimum Score' = MIN(Total),
                'Maximum Score' = MAX(Total)) -> t1WriteTotRaw
    
    data %>%
      select(-contains('T2_')) %>%
      select(contains('Cont'), contains('ID')) %>%
      gather('key', 'value', -ID) %>%
      group_by(ID) %>%
      summarise(Total = MEAN(value)) %>%
      summarise('Measure' = 'Task 1 Content',
                'Mean Score' = MEAN(Total),
                'Median Score' = MEDIAN(Total),
                'Standard Deviation' = STDEV(Total),
                'Minimum Score' = MIN(Total),
                'Maximum Score' = MAX(Total))-> t1ContRaw
    
    data %>%
      select(-contains('T2_')) %>%
      select(contains('Org'), contains('ID')) %>%
      gather('key', 'value', -ID) %>%
      group_by(ID) %>%
      summarise(Total = MEAN(value)) %>%
      summarise('Measure' = ' Task 1 Organization',
                'Mean Score' = MEAN(Total),
                'Median Score' = MEDIAN(Total),
                'Standard Deviation' = STDEV(Total),
                'Minimum Score' = MIN(Total),
                'Maximum Score' = MAX(Total))-> t1OrgRaw
    
    data %>%
      select(-contains('T2_')) %>%
      select(contains('Lu'), contains('ID')) %>%
      gather('key', 'value', -ID) %>%
      group_by(ID) %>%
      summarise(Total = MEAN(value)) %>%
      summarise('Measure' = 'Task 1 Language Use',
                'Mean Score' = MEAN(Total),
                'Median Score' = MEDIAN(Total),
                'Standard Deviation' = STDEV(Total),
                'Minimum Score' = MIN(Total),
                'Maximum Score' = MAX(Total))-> t1LuRaw
    
    data %>%
      select(-contains('T1_')) %>%
      gather('key', 'value', -ID) %>%
      group_by(ID) %>%
      summarise(Total = MEAN(value)) %>%
      summarise('Measure' = 'Task 2 Total',
                'Mean Score' = MEAN(Total),
                'Median Score' = MEDIAN(Total),
                'Standard Deviation' = STDEV(Total),
                'Minimum Score' = MIN(Total),
                'Maximum Score' = MAX(Total)) -> t2WriteTotRaw
    
    data %>%
      select(-contains('T1_')) %>%
      select(contains('Cont'), contains('ID')) %>%
      gather('key', 'value', -ID) %>%
      group_by(ID) %>%
      summarise(Total = MEAN(value)) %>%
      summarise('Measure' = 'Task 2 Content',
                'Mean Score' = MEAN(Total),
                'Median Score' = MEDIAN(Total),
                'Standard Deviation' = STDEV(Total),
                'Minimum Score' = MIN(Total),
                'Maximum Score' = MAX(Total)) -> t2Cont
    
    data %>%
      select(-contains('T1_')) %>%
      select(contains('Org'), contains('ID')) %>%
      gather('key', 'value', -ID) %>%
      group_by(ID) %>%
      summarise(Total = MEAN(value)) %>%
      summarise('Measure' = 'Task 2 Organization',
                'Mean Score' = MEAN(Total),
                'Median Score' = MEDIAN(Total),
                'Standard Deviation' = STDEV(Total),
                'Minimum Score' = MIN(Total),
                'Maximum Score' = MAX(Total)) -> t2Org
    
    data %>%
      select(-contains('T1_')) %>%
      select(contains('Lu'), contains('ID')) %>%
      gather('key', 'value', -ID) %>%
      group_by(ID) %>%
      summarise(Total = MEAN(value)) %>%
      summarise('Measure' = 'Task 2 Language Use',
                'Mean Score' = MEAN(Total),
                'Median Score' = MEDIAN(Total),
                'Standard Deviation' = STDEV(Total),
                'Minimum Score' = MIN(Total),
                'Maximum Score' = MAX(Total)) -> t2Lu
    
    
    
    WriteScore <- bind_rows(t1WriteTotRaw, t1ContRaw, t1OrgRaw, t1LuRaw, t2WriteTotRaw, t2Cont, t2Org, t2Lu)
    WriteScore
    
  })
  
  output$testsumTable <- DT::renderDataTable({
    
    switch(input$courseLevel,
      
           'Grammar' = DT::datatable(
             level_skill_summary(),
             rownames = FALSE,
             options = list(searching = FALSE, lengthChange = FALSE, paging = FALSE)
           ) %>%
             formatStyle(names(level_skill_summary()), fontWeight = 'bold'),
           
           'Reading' = DT::datatable(
             level_skill_summary(),
             rownames = FALSE,
             options = list(searching = FALSE, lengthChange = FALSE, paging = FALSE)
           ) %>%
             formatStyle(names(level_skill_summary()), fontWeight = 'bold'),
           
           'Writing Adv' = DT::datatable(
             level_write_summary_adv(),
             rownames = TRUE,
             options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE), filter = 'none'
           ) %>%
             formatStyle(names(level_write_summary_adv()), fontWeight = 'bold'),
           
           'Writing Int' = DT::datatable(
             level_write_summary_int(),
             rownames = TRUE,
             options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE), filter = 'none'
           ) %>%
             formatStyle(names(level_write_summary_int()), fontWeight = 'bold')
    )
  })
  
  output$stem <- renderPlot({

    switch(input$courseLevel,
           
           'Grammar' = level_skill_stemplot(),
           'Reading' = level_skill_stemplot(),
           'Writing Basic' = level_skill_stemplot(),
           'Writing Int' = level_skill_stemplot(),
           'Writing Adv' = level_skill_stemplot()
    )
  }, bg="transparent")
  
  # Begin item analysis
  
  # phi <- function(x){
  #   
  #   # phi = Pit - Pi*Pt / sqrt(Pi*Qi*Pt*Qt)
  #   
  #   # Pi = proportion who answered item correct
  #   # Qi = proportion who answered item incorrect
  #   # Pt = proportion who passed test
  #   # Qt = proportion who failed test
  #   # Pit = proportion who answered item correct and passed the test
  #   # Brown and Hudson (2002), Criterion-referenced language testing p. 126
  #   
  #   x %>%
  #     select(-c(1:5)) %>%
  #     mutate_each(funs(.[1:length(.)]/MAX(.))) %>%
  #     summarise_each(funs(MEAN)) -> Pi
  #   
  #   Qi <- 1 - Pi
  #   
  #   x %>%
  #     select(-c(1,3,4,5)) %>%
  #     mutate(Total_raw = rowSums(.[2:length(.)])) %>%
  #     summarise(Pt = length(which(Total_raw/MAX(Total_raw) >= .80))/length(.)) -> Pt
  #   
  #   Qt <- 1 - Pt
  #   
  #   x %>%
  #     select(-c(1:5)) %>%
  #     mutate(Total_raw = rowSums(.[2:length(.)])) %>%
  #     mutate(Total_scaled = Total_raw/MAX(Total_raw)*100) %>%
  #     summarise_each(funs(length(which(.[1:length(.)]/MAX(.) >= .80 & Total_scaled >= 80))/length(.))) %>%
  #     round(2)  %>%
  #     select(-contains('Total'))-> Pit
  #   
  # 
  #   step1 <- Pit - as.data.frame(apply(Pi, 2, function(X) X*Pt))
  #   step2 <- Pi*Qi
  #   step3 <- Pt*Qt
  #   step4 <- sqrt(as.data.frame(apply(step2,2,function(X) X*step3)))
  #   Phi <- as.data.frame(apply(step1, 2, function(X) X/step4))
  #   colnames(Phi) <- colnames(Pi)
  #   Phi
  # }
  
  
  # Bindex <- function(x){
  #   x %>%
  #     select(-c(1:5)) %>%
  #     summarise_each(funs(MEAN(.[2:length(.)]/.[1]))) %>%
  #     select(-contains('Total')) -> Pi
  #   
  #   x %>%
  #     filter(Name != 'Max Score') %>%
  #     summarise(Pt = length(which(Total_scaled >= 80))/length(Total_scaled)) -> Pt
  #   
  #   Qt <- 1 - Pt
  #   
  #   x %>%
  #     select(-c(1:5)) %>%
  #     summarise_each(funs(length(which(.[-1] == .[1] & Total_scaled >= 80))/length(.[-1]))) %>%
  #     round(2) %>%
  #     select(-contains('Total'))-> Pit
  #   
  #   step1 <- as.data.frame(apply(Pi, 2, function(X) X*Pt))
  #   step2 <- Pit-step1
  #   step3 <- Pt*Qt
  # 
  #   bindex <- as.data.frame(apply(step2, 2, function(X) X/step3))
  #   colnames(bindex) <- colnames(Pi)
  #   return(bindex)
  # }
  
  # Agree <- function(x){
  #   x %>%
  #     select(-c(1:5)) %>%
  #     summarise_each(funs(length(which(.[-1] == .[1] & Total_scaled >= 80))/length(.[-1]))) %>%
  #     round(2) %>%
  #     select(-contains('Total'))-> Pit
  #   
  #   x %>%
  #     select(-c(1:5)) %>%
  #     summarise_each(funs(length(which(.[-1] != .[1] & Total_scaled < 80))/length(.[-1]))) %>%
  #     round(2) %>%
  #     select(-contains('Total'))-> Fit
  #   
  #   agree <- Pit + Fit
  #   return(agree)
  # }
  

  item_analysis_data <- reactive({

    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$dynamics)
    
    DATA <- dbGetQuery(db, query)
    dbDisconnect(db)
    

    # Weighted item difficulty
    DATA %>%
      select(-c(1:5)) %>%
      summarise_each(funs(MEAN(.[2:length(.)]/.[1]))) -> Difficulty

    # # Item phi
    # DATA %>%
    #   mutate(Total_raw = rowSums(.[6:length(.)], na.rm = TRUE)) %>%
    #   mutate(Total_scaled = round((Total_raw/Total_raw[1])*100,2)) %>%
    #   phi() %>%
    #   round(2) -> ItemPhi
    

    # #B index  
    # DATA %>%
    #   mutate(Total_raw = rowSums(.[6:length(.)], na.rm = TRUE)) %>%
    #   mutate(Total_scaled = round((Total_raw/Total_raw[1])*100,2)) %>%
    #   Bindex() %>%
    #   round(2) -> ItemB
    # 
    # # Item Agreement
    # DATA %>%
    #   mutate(Total_raw = rowSums(.[6:length(.)], na.rm = TRUE)) %>%
    #   mutate(Total_scaled = round((Total_raw/Total_raw[1])*100,2)) %>%
    #   Agree() %>%
    #   round(2) -> ItemAg
    
    # Corrected item total
    DATA %>%
      filter(Name != 'Max Score') %>%
      select(-c(1:5)) %>%
      psychometric::item.exam() %$%
      round(Item.Tot.woi,2) %>%
      as.data.frame() -> ItemCor
    
    rownames(ItemCor) <- colnames(DATA[6:length(names(DATA))])
    colnames(ItemCor) <- '1'
    ItemCor <- data.frame(t(ItemCor))
    
      
    ## Binding difficulty and discrimination
    # Index <- bind_rows(Difficulty, ItemPhi, ItemCor)
    # Index <- t(Index)
    # colnames(Index) <- c('Difficulty', 'Item Phi', 'R-Drop')
    # is.na(Index) <- sapply(Index, is.infinite)
    # Index
    
    Index <- bind_rows(Difficulty, ItemCor)
    Index <- t(Index)
    colnames(Index) <- c('Difficulty',  'R-Drop')
    is.na(Index) <- sapply(Index, is.infinite)
    Index
    
  })
  
  
  # Begin writing rater analysis
  
  COR <- function(x){
    round(cor(x, use = 'complete.obs'),2)
  }
  
  write_rater_adv <- reactive({
    
    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$dynamics)
    
    DATA <- dbGetQuery(db, query)
    dbDisconnect(db)    
    
    ## Tidy up data
    
    DATA %>%
      tbl_df() %>%
      select(-contains('_R3'), -contains('Level'), 
             -contains('Skill'), -contains('Section'),
             -contains('Name'), -contains('Course'),
             -contains('Cover_Letter'), -contains('Major_Assignment'),
             -contains('Choices'), -contains('Num_missing'), 
             -contains('_AVE')) -> data


    ## Begin correlation tables
    
    data %>%
      gather('key', 'value', -ID) %>%
      group_by(key) %>%
      summarise(Mean = MEAN(value), SD = STDEV(value))-> RaterMeans
    
    # Content Development
    data %>%
      select(contains('CD')) %>%
      COR() %>%
      round(2) %>%
      as.data.frame() %>%
      slice(2) %>%
      t() -> ContCor
    
    # Organization
    data %>%
      select(contains('Org')) %>%
      COR() %>%
      round(2) %>%
      as.data.frame() %>%
      slice(2) %>%
      t() -> OrgCor
    
    # Language Use
    data %>%
      select(contains('Lu')) %>%
      COR() %>%
      round(2) %>%
      as.data.frame() %>%
      slice(2) %>%
      t() -> LangCor
    
    # Source Use
    data %>%
      select(contains('Su')) %>%
      COR() %>%
      round(2) %>%
      as.data.frame() %>%
      slice(2) %>%
      t() -> SuCor
    
    
    # Combine categories and raters & correlate raters
    
    Datacor <- rbind(ContCor, OrgCor, LangCor, SuCor)
    
    Datacor %>%
      as.data.frame() %>%
      transmute(Correlation = ifelse(V1 == 1.00, NA, V1)) %>%
      as.data.frame() -> Datacor
    
    CorTab <- bind_cols(RaterMeans, Datacor)
    colnames(CorTab)[1] <- 'Category'
    CorTab
    
  })
  

  write_rater_int <- reactive({
    
    
    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$dynamics)
    
    DATA <- dbGetQuery(db, query)
    dbDisconnect(db)    
    
    ## Tidy up data
    
    DATA %>%
      tbl_df() %>%
      select(-contains('_R3'), -contains('Level'), 
             -contains('Skill'), -contains('Section'),
             -contains('Name'), -contains('Course'), 
             -contains('_AVE'), -contains('tot')) -> data
    
    
    ## Begin correlation tables
    
    data %>%
      select(-contains('T2')) %>%
      gather('key', 'value', -ID) %>%
      group_by(key) %>%
      summarise(Mean = MEAN(value), SD = STDEV(value))-> T1RaterMeans
    
    # Content Development
    data %>%
      select(-contains('T2')) %>%
      select(contains('Cont')) %>%
      COR() %>%
      round(2) %>%
      as.data.frame() %>%
      slice(2) %>%
      t() -> T1ContCor
    
    # Organization
    data %>%
      select(-contains('T2')) %>%
      select(contains('Org')) %>%
      COR() %>%
      round(2) %>%
      as.data.frame() %>%
      slice(2) %>%
      t() -> T1OrgCor
    
    # Language Use
    data %>%
      select(-contains('T2')) %>%
      select(contains('Lu')) %>%
      COR() %>%
      round(2) %>%
      as.data.frame() %>%
      slice(2) %>%
      t() -> T1LangCor
    
    data %>%
      select(-contains('T1')) %>%
      gather('key', 'value', -ID) %>%
      group_by(key) %>%
      summarise(Mean = MEAN(value), SD = STDEV(value))-> T2RaterMeans
    
    # Content Development
    data %>%
      select(-contains('T1')) %>%
      select(contains('Cont')) %>%
      COR() %>%
      round(2) %>%
      as.data.frame() %>%
      slice(2) %>%
      t() -> T2ContCor
    
    # Organization
    data %>%
      select(-contains('T1')) %>%
      select(contains('Org')) %>%
      COR() %>%
      round(2) %>%
      as.data.frame() %>%
      slice(2) %>%
      t() -> T2OrgCor
    
    # Language Use
    data %>%
      select(-contains('T1')) %>%
      select(contains('Lu')) %>%
      COR() %>%
      round(2) %>%
      as.data.frame() %>%
      slice(2) %>%
      t() -> T2LangCor
    
  
    # Combine categories and raters & correlate raters
    
    T1Datacor <- rbind(T1ContCor, T1OrgCor, T1LangCor)
    
    T1Datacor %>%
      as.data.frame() %>%
      transmute(Correlation = ifelse(V1 == 1.00, NA, V1)) %>%
      as.data.frame() -> T1Datacor
    
    T1CorTab <- bind_cols(T1RaterMeans, T1Datacor)
    colnames(T1CorTab)[1] <- 'Task/Category'
    
    T2Datacor <- rbind(T2ContCor, T2OrgCor, T2LangCor)
    
    T2Datacor %>%
      as.data.frame() %>%
      transmute(Correlation = ifelse(V1 == 1.00, NA, V1)) %>%
      as.data.frame() -> T2Datacor
    
    T2CorTab <- bind_cols(T2RaterMeans, T2Datacor)
    colnames(T2CorTab)[1] <- 'Task/Category'
    
    CorTab <- bind_rows(T1CorTab, T2CorTab)
 
    
  })
 
  brksdiff <- c(.59, .69, .89)
  clrsdiff <- c('rgb(255, 69, 0)', 'rgb(255, 215, 0)', 'rgb(34, 139, 34)', 'rgb(255, 215, 0)')
  
  brksdisc <- c(-1, 0, .19)
  clrsdisc <- c('rgb(255, 69, 0)', 'rgb(255, 69, 0)', 'rgb(255, 215, 0)', 'rgb(34, 139, 34)')
  
  brkscor <- c(-1, .0000000001, .64, .90)
  clrscor <- c('rgb(105, 105, 105)', 'rgb(105, 105, 105)', 'rgb(255, 69, 0)', 'rgb(34, 139, 34)', 'rgb(255, 215, 0)')
  
  
  output$item_analysisTable <- DT::renderDataTable({

        switch(input$courseLevels,
           
            'Grammar' = DT::datatable(
              item_analysis_data(),
              rownames = TRUE,
              options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE), filter = 'none'
            ) %>%
             formatStyle(names(item_analysis_data()), fontWeight = 'bold') %>%
             formatStyle(names(item_analysis_data()[1]), backgroundColor = styleInterval(brksdiff, clrsdiff)) %>%
             formatStyle(names(item_analysis_data()[2]), backgroundColor = styleInterval(brksdisc, clrsdisc)) %>%
             formatStyle(names(item_analysis_data()[3]), backgroundColor = styleInterval(brksdisc, clrsdisc)),
           
           'Reading' = DT::datatable(
             item_analysis_data(),
             rownames = TRUE,
             options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE), filter = 'none'
           ) %>%
             formatStyle(names(item_analysis_data()), fontWeight = 'bold') %>%
             formatStyle(names(item_analysis_data()[1]), backgroundColor = styleInterval(brksdiff, clrsdiff)) %>%
             formatStyle(names(item_analysis_data()[2]), backgroundColor = styleInterval(brksdisc, clrsdisc)) %>%
             formatStyle(names(item_analysis_data()[3]), backgroundColor = styleInterval(brksdisc, clrsdisc)),
           
           'Writing Adv' = DT::datatable(
             write_rater_adv(),
             rownames = TRUE,
             options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE), filter = 'none'
           ) %>%
             formatStyle(names(write_rater_adv()), fontWeight = 'bold') %>%
             formatStyle(names(write_rater_adv()[4]), backgroundColor = styleInterval(brkscor, clrscor)),
           
           'Writing Int' = DT::datatable(
             write_rater_int(),
             rownames = TRUE,
             options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE), filter = 'none'
           ) %>%
             formatStyle(names(write_rater_int()), fontWeight = 'bold') %>%
             formatStyle(names(write_rater_int()[4]), backgroundColor = styleInterval(brkscor, clrscor))
           )
    
    
    
  })
  
  # Begin objective summaries
  
  obj_cor_adv <- reactive({
    
    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$dynamics)
    
    DATA <- dbGetQuery(db, query)
    dbDisconnect(db) 
    
    DATA%>%
      select(contains('AVE')) %>%
      COR() -> OBJ_corTable
    
    upper<-OBJ_corTable
    upper[upper.tri(OBJ_corTable, diag = FALSE)]<-""
    upper<-as.data.frame(upper)
    upper
  })
  
  
  obj_receptive <- reactive({
  
    db <- dbConnect(SQLite(), 'Achievement')
    query <- sprintf("SELECT * FROM %s", input$dynamics)
    
    DATA <- dbGetQuery(db, query)
    dbDisconnect(db) 
    
    DATA %>%
      select(-c(1:5)) %>%
      filter(!is.na(.[1])) %>%
      # mutate_each(funs((.[1:length(.)]/.[1]))) Used when 'Max Score' was the first row
      mutate_each(funs(.[1:length(.)]/MAX(.))) %>%
      gather(key, value) -> SLO
    
    SLO %>%
      mutate(SLO = str_sub(key, start = str_length(key)-3)) %>%
      group_by(SLO) %>%
      summarise('N Test takers' = length(key)/length(unique(key)),
                'N Items' = length(unique(key)),
                 Mean = MEAN(value),
                SD = STDEV(value)) -> SLOsum
    
  })


  output$OBJ_analysisTable <- DT::renderDataTable({
    if (is.null(input$courseLevels))
      return()
    
  switch(input$courseLevels,
         
  'Reading' = DT::datatable(
    obj_receptive(),
    rownames = TRUE,
    options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE), filter = 'none'
  ) %>%
    formatStyle(names(obj_receptive()), fontWeight = 'bold'),
  
  'Grammar' = DT::datatable(
    obj_receptive(),
    rownames = TRUE,
    options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE), filter = 'none'
  ) %>%
    formatStyle(names(obj_receptive()), fontWeight = 'bold'),
  

  'Writing Adv' = DT::datatable(
    obj_cor_adv(),
    rownames = TRUE,
    options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE), filter = 'none'
  ) %>%
    formatStyle(names(obj_cor_adv()), fontWeight = 'bold') %>%
    formatStyle(names(obj_cor_adv()), backgroundColor = styleInterval(brkscor, clrscor))
  )
  })
  
  
  # Download Buttons
  
  # NOT USED CURRENTLY
  # output$downloadBtnResp <- downloadHandler(
  #   filename = function() { 
  #     paste0(input$dynamic, 'ItemResponses.csv', sep = '_')
  #   },
  #   content = function(file) {
  #     write.csv(responses_data(), file, row.names = FALSE)
  #   }
  # )
  
  output$downloadBtnAna <- downloadHandler(
    filename = function() { 
      paste0(input$dynamics, '_ItemAnalysis.csv', sep = '')
    },
    content = function(file) {
      write.csv(item_analysis_data(), file, row.names = TRUE)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste(input$dynamic, 'stemPlot.png', sep = '_')},
    content = function(file) {
      ggsave(file, plot = level_skill_stemplot(), device = 'png', width = 6, height = 6, units = 'in')
      }
  )
  
  output$downloadSum <- downloadHandler(
    filename = function() { 
      paste0(input$sum,'.csv', sep = '')
    },
    content = function(file) {
      write.csv(level_skill_summary(), file, row.names = TRUE)
    }
  )
  
  
  output$downloadAdvWrite <- downloadHandler(
    filename = function() { 
      paste0(input$sum,'.csv', sep = '')
    },
    content = function(file) {
      write.csv(level_write_summary_adv(), file, row.names = TRUE)
    }
  )
  
  
  level_write_summary_adv
  
  output$downloadRaterAna <- downloadHandler(
    filename = function() { 
      paste0(input$dynamics, '_RaterAnalysis.csv', sep = '')
    },
    content = function(file) {
      write.csv(write_rater_adv(), file, row.names = TRUE)
    }
  )
  
  output$downloadrubricOBJ <- downloadHandler(
    filename = function() { 
      paste0(input$dynamics,'_rubric_OBJsum.csv', sep = '')
    },
    content = function(file) {
      write.csv(obj_cor_adv(), file, row.names = TRUE)
    }
  )
  
  output$downloadOBJ <- downloadHandler(
    filename = function() { 
      paste0(input$dynamics,'_OBJsum.csv', sep = '')
    },
    content = function(file) {
      write.csv(obj_receptive(), file, row.names = TRUE)
    }
  )
  

})