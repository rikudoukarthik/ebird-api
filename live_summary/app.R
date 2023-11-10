library(shiny)
library(curl)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(glue)
library(writexl)
library(rebird)


source("token.R") # Get an eBird API token and assign it to object myebirdtoken
source("scripts/functions.R")

# for sort order of species
# ebd <- ebirdtaxonomy(key = myebirdtoken) %>%
#   rename(ENGLISH.NAME = comName) %>%
#   mutate(SORT = 1:n()) %>% 
#   dplyr::select(ENGLISH.NAME, SORT)
# write_csv(ebd, "eBirdTaxonomy.csv")
ebd <- read_csv("eBirdTaxonomy.csv")


# Define UI for app ----

ui <- fluidPage(
  
  # App title 
  titlePanel("Hello eBirder!"),
  
  # Single vertical layout (no sidebar) with input and output definitions 
  verticalLayout(
    
    # Input: Date selection (value always yyyy-mm-dd, even if display format different)
    helpText(h4("Select the start and end dates of interest")),
    
    dateInput(inputId = "event_date_start", 
              label = "Start date", 
              value = today()),
    
    dateInput(inputId = "event_date_end", 
              label = "End date", 
              value = today()),
    
    # Input: Region (admin unit) code
    helpText(h4("Select the administrative unit code for the region of interest")),
    helpText("Summary will include selection and all admin. units one level below selection"),
    selectizeInput(inputId = "region_code", 
                   choices = c("Choose one" = "", 
                               get_admin_codes("IN")),
                   selected = "", label = "Admin. unit"), 
    
    # Input: Event code for save file name
    helpText(h4("Provide a short event code (for file downloads)")),
    textInput(inputId = "event_code", 
              value = glue("ABCD_{today() %>% year()}"), label = NULL), 
    
    # Input: download button 
    helpText(h4("Download your eBird summary!")),
    downloadButton("downloadData", "Download",
                   label = "Summary"),
    
  )
  
)


# Define server logic ----

server <- function(input, output) {
  
    # dates (single or multi)
    event_date <- reactive ({
      if (input$event_date_start == input$event_date_end) {
        input$event_date_start
      } else {
        seq(input$event_date_start, input$event_date_end, by = "days")
      }
    })
    
    region_info <- reactive ({
      get_admin_names(input$region_code)
    })
    
    
    # get list of species
    spec_list_adm1 <- reactive ({
      get_admin_codes(input$region_code, hi_arch = FALSE) %>%
        gen_spec_list(dates = event_date())
    })
    
    spec_list_adm2 <- reactive ({
      get_admin_codes(input$region_code, hi_arch = TRUE) %>%
        gen_spec_list(dates = event_date()) %>%
        filter(REGION != input$region_code)
    })

    # get participation stats
    
    basic_summary_adm1 <- reactive ({
      get_admin_codes(input$region_code, hi_arch = FALSE) %>%
        gen_part_summ(dates = event_date(), list_spec = spec_list_adm1())
    })
    
    basic_summary_adm2 <- reactive ({
      get_admin_codes(input$region_code, hi_arch = TRUE) %>%
        gen_part_summ(dates = event_date(), list_spec = spec_list_adm2()) %>%
        filter(REGION != input$region_code)
    })
    
    
    # Downloadable .xlsx of selected dataset 
    output$downloadData <- downloadHandler(
      
      filename = function(){glue("{input$event_code}_summary.xlsx")},
      content = function(file) {
        
        withProgress(message = "Calculating summaries", value = 0, {
          
          Sys.sleep(1)
          
          data3 <- spec_list_adm1()    
          incProgress(0.25)
          data1 <- basic_summary_adm1()
          incProgress(0.25)
          data4 <- spec_list_adm2()    
          incProgress(0.25)
          data2 <- basic_summary_adm2()    
          incProgress(0.25)

          write_xlsx(list("Summary (overall)" = data1,
                          "Summary (subregions)" = data2,
                          "Species list (overall)" = data3,
                          "Species list (subregions)" = data4), 
                     file)
          
        })
        
      }
      
    )

}

# Define app ----

shinyApp(ui = ui, server = server)

