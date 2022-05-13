# Data entry for projectile point analysis

library(shiny)
library(tidyverse)
library(magrittr)
library(rhandsontable)
# library(DataEditR)
library(DT)
library(rio)
library(here)

i_am("ProjAnalysis/app.R")

inputOptions = import(here("ProjAnalysis/InputOptions.xlsx"),
                      setclass = 'tibble') %>% 
  separate_rows(options,sep = ";") %>% 
  mutate_all(trimws) %>% 
  chop(options)

jscode = '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#add").click();
}});'

# load from tmp
fntmp = here("ProjAnalysis/tmp/current.Rds")
if(file.exists(fntmp)){
  prior = readRDS(fntmp)
} else {
  prior = tibbe()
}

# analysisDownload <- 
#     import('https://docs.google.com/spreadsheets/d/1rJuFW9zCX4szd5r4nO7a_3BcE9kbqh2ZMhlZ7AG_fwo/edit#gid=1861978005',
#            setclass = 'tibble')

# ui ----
ui <- fluidPage(
  
  # Application title
  titlePanel("Projectile Point Analysis"),
  
  # javascript
  tags$head(tags$script(HTML(jscode))),
  
  
  sidebarLayout( 
    sidebarPanel(
      # h2("load data"),
      # shiny::fileInput('file1', 'Choose file to import',
      #                  multiple = FALSE),
      # actionButton('load','load'),
      h2("Add data"),
      textInput('ID','ID',
                placeholder = "proj-number-artifact"),
      selectizeInput('variableUI',"variable",
                     inputOptions$key %>% 
                       setNames(inputOptions$inputName)),
      uiOutput('valueUI'),
      checkboxInput('check','Check if observation is uncertain'),
      tagAppendAttributes(
        actionButton('add','Add'),
        `data-proxy-click` = "add"
      ),
      wellPanel(
      textOutput('fileName1'),
      textOutput('fileName2')
      )
    ),
    mainPanel(
      # h3("Editing is not working"),
      actionButton('deleteAll','delete all'),
      actionButton('deleteRow','delete row'),
      actionButton('save','save'),
      DT::DTOutput('table1')
      # dataOutputUI("output-1"),
      # dataEditUI("edit-1")
    )
  )
  
)

# Server ----
server <- function(input, output, session) {
  input <<- input
  
  # new = reactiveVal(tibble())
  # analysis <- reactiveVal(analysisDownload)
  analysis <- reactiveVal(prior)
  
  
  # table output ----
  prntTbl = function(){
    output$table1 = DT::renderDT({
      DT::datatable(analysis(),rownames = F)
    })
  }
  prntTbl()
  
  # userFile <- shiny::reactive({
  #     # If no file is selected, don't do anything
  #     shiny::validate(shiny::need(input$file1, message = FALSE))
  #     input$file1
  # })
  
  # observeEvent(input$load,{
  
  # print("loaded data")
  # })
  # analysis <- shiny::reactive({
  #     rio::import(userFile()$datapath, setclass = 'tibble')
  # })
  
  # Define ui inputs ----
  output$valueUI = renderUI({
    req(input$variableUI)
    type = inputOptions %>% 
      dplyr::filter(stringr::str_detect(key,input$variableUI)) %>% 
      dplyr::pull(type)
    qs = 
      inputOptions %>% 
      filter(key == input$variableUI) %>% 
      pull(options) %>%
      unlist()
    if(type == "text"){
      r = shiny::textInput('value','value')
    } else if(type == "numeric"){
      r = shiny::numericInput('value','value',0)
    } else if(type == "list"){
      r = shiny::selectizeInput('value','value',qs, multiple = T)
    } else {
      stop("error in input type")
    }
  })
  
  # file names to copy ----
  output$fileName1 = shiny::renderText({
    req(input$ID)
    paste0(input$ID,".png")
  })
  output$fileName2 = shiny::renderText({
    req(input$ID)
    paste0(input$ID,"b.png")
  })
  
  # buttons ----
  observeEvent(input$add,{
    req(input$variableUI)
    if(input$check == T){
      value = paste0(input$value,'?')
      # reset checkbox
      updateCheckboxInput(session = session,
                          inputId = 'check',
                          label =  'Check if observation is uncertain',
                          value = F)
    } else value = input$value
    value = paste(value, collapse = "; ")
    
    new = tibble(
      ID = input$ID,
      variable = input$variableUI,
      value = value,
      date = as.character(as.Date(Sys.time())),
      source = "RJB"
    )
    print(new)
    new = bind_rows(new,analysis()) %>% distinct_all()
    analysis <<- reactiveVal(new)
    prntTbl()
    
    # save results
    analysis() %>% saveRDS(here("ProjAnalysis/tmp/current.Rds"))
    
    # reset select input
    updateSelectizeInput(session = session,
                         inputId = 'variableUI',
                         label = "variable",
                         choices = inputOptions$key %>% 
                     setNames(inputOptions$inputName),
                     selected = "")
  })
  
  # delete everything -- including prior saved data
  observeEvent(input$deleteAll,{
    showModal(modalDialog(
      title = h1("Warning!", style = "color: red;"),
      "This will delete all data including everything saved in prior sessions!",
      footer = tagList(
        actionButton('deleteAllMod','delete', style = "color: red;"),
        actionButton('cancel','cancel')
      )
    ))
  })
  
  observeEvent(input$cancel,removeModal())
  observeEvent(input$deleteAllMod,{
    print('working?')
    analysis <<- reactiveVal(tibble())
    prntTbl()
    # save results
    analysis() %>% saveRDS(here("ProjAnalysis/tmp/current.Rds"))
    removeModal()
  })
  
  observeEvent(input$deleteRow,{
    
    indx = input$table1_rows_selected
    
    # save deleted just in case
    removed = analysis() %>% 
      slice(indx)
    fntmp = here("ProjAnalysis/tmp/deleted.Rds")
    if(file.exists(fntmp)){
      old = readRDS(fntmp)
    } else {
      old = tibble()
    }
    removed = bind_rows(old, removed) %>% 
      distinct_all()
    saveRDS(removed,fntmp)
    
    new = analysis() %>% 
      slice(-indx)
    
    analysis <<- reactiveVal(new)
    prntTbl()
    
    # save results
    analysis() %>% saveRDS(here("ProjAnalysis/tmp/current.Rds"))
  })
  
  
  
  
  # output$table1 = renderDT(datatable(tibble(a = 1),rownames = F))
  # edited <- dataEditServer("edit-1",
  #                          data = analysis()
  # )
  # dataOutputServer("output-1",
  #                  data = edited(),
  #                  write_args = list(row.names = F)
  # )
  
  # save ----
  observeEvent(input$save,{
    print("saving")
    fn = here(glue::glue(
      "data/ProjectilePointAnalysis-{as.Date(Sys.time())}.xlsx"
      # "data/test-{as.Date(Sys.time())}.xlsx"
    ))
    i = 0
    while(isTRUE(file.exists(fn))){
      i = i + 1
      fn %<>% 
        gsub("v\\d+.xlsx|.xlsx",paste0("v",i,".xlsx"),.)
      if(i > 99) stop("limited to 99 files in one day. Why so many files?")
    }
    rio::export(analysis(),fn)
    browseURL(fn)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
