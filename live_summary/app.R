library(shiny)
library(curl)
library(tidyverse)
library(lubridate)
library(glue)
library(writexl)
library(rebird)

# Get an eBird API token and assign it to object myebirdtoken
source("token.R")
source("scripts/functions.R")

# for sort order of species
# ebd <- ebirdtaxonomy(key = myebirdtoken) %>%
#   rename(ENGLISH.NAME = comName) %>%
#   mutate(SORT = 1:n()) %>% 
#   dplyr::select(ENGLISH.NAME, SORT)
# write_csv(ebd, "eBirdTaxonomy.csv")
ebd <- read_csv("eBirdTaxonomy.csv")


# Define UI for app that draws a histogram ----

ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello eBirder!"),
  
  # Sidebar layout with input and output definitions ----
  verticalLayout(
    
    # Input: Date selection (value always yyyy-mm-dd, even if display format different) ----
    helpText(h4("Select the date of your event")),
    
    dateInput(inputId = "event_date", 
              label = "Event date", 
              value = today()),
    
    # Input: Region (admin unit) code ----
    helpText(h4("Select the admin unit code for the region of interest")),
    helpText("Summary will include selection and all admin units one level below selection"),
    selectizeInput(inputId = "region_code", 
                   choices = c("Choose one" = "", 
                               get_admin_codes("IN")),
                   selected = "", label = "Admin unit"), 
    
    # Input: Event code for save file name ----
    helpText(h4("Provide a short event code (for file downloads)")),
    textInput(inputId = "event_code", 
              value = glue("ABCD_{today() %>% year()}"), label = NULL), 
    
    # Input: download button ----
    helpText(h4("Download your eBird summary!")),
    downloadButton("downloadData", "Download",
                   label = "Summary"),
    
  )
  
)


# Define server logic required to draw a histogram ----

server <- function(input, output) {
  
  region_info <- reactive ({
    get_admin_names(input$region_code)
  })
  
    
  # get list of species
  spec_list_adm1 <- reactive ({
    get_admin_codes(input$region_code, hi_arch = FALSE) %>% 
      map(~ write_spec_tally(.x, input$event_date)) %>% 
      list_c() %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("REGION", "ENGLISH.NAME")) %>% 
      left_join(ebd) %>% 
      arrange(REGION, SORT) %>% 
      left_join(region_info()) %>% 
      dplyr::select(REGION, REGION.NAME, ENGLISH.NAME)
  })
  
  spec_list_adm2 <- reactive ({
    get_admin_codes(input$region_code, hi_arch = TRUE) %>% 
      map(~ write_spec_tally(.x, input$event_date)) %>% 
      list_c() %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("REGION", "ENGLISH.NAME")) %>% 
      filter(REGION != input$region_code) %>% 
      left_join(ebd) %>% 
      arrange(REGION, SORT) %>% 
      left_join(region_info()) %>% 
      dplyr::select(REGION, REGION.NAME, ENGLISH.NAME)
  })
  
  # get participation stats
  
  basic_summary_adm1 <- reactive ({
    get_admin_codes(input$region_code, hi_arch = FALSE) %>% 
      map(~ write_obs_tally(.x, input$event_date)) %>% 
      list_c() %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("REGION", "OBSERVERS", "CHECKLISTS", "SPECIES")) %>% 
      mutate(across(c(everything(), -REGION), ~ as.integer(.))) %>% 
      arrange(desc(OBSERVERS), desc(SPECIES)) %>% 
      left_join(region_info()) %>% 
      relocate(REGION, REGION.NAME)
  })
  
  basic_summary_adm2 <- reactive ({
    get_admin_codes(input$region_code, hi_arch = TRUE) %>% 
      map(~ write_obs_tally(.x, input$event_date)) %>% 
      list_c() %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("REGION", "OBSERVERS", "CHECKLISTS", "SPECIES")) %>% 
      mutate(across(c(everything(), -REGION), ~ as.integer(.))) %>% 
      arrange(desc(OBSERVERS), desc(SPECIES)) %>% 
      filter(REGION != input$region_code) %>% 
      left_join(region_info()) %>% 
      relocate(REGION, REGION.NAME)
  })
  
  
  
  # Reactive value for selected dataset (filter, summarise) ----

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(

    filename = function(){glue("{input$event_code}_summary.xlsx")},
    content = function(file) {
      write_xlsx(list("Summary (overall)" = basic_summary_adm1(),
                      "Summary (subregions)" = basic_summary_adm2(),
                      "Species list (overall)" = spec_list_adm1(),
                      "Species list (subregions)" = spec_list_adm2()), 
                 file)
    }

  )
  
}

# Define app ----

shinyApp(ui = ui, server = server)

