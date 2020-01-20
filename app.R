# Libraries
library(shiny);library(shinydashboard);library(tidyverse);
library(collaborator);library(jsonlite)

auth <- readRDS("~/authorship_app/auth.rds")

# Build UI
ui <- fluidPage(titlePanel("STARSurg RECON Authorship"),
                tabsetPanel(type = "tabs", 
                            
                            # Tab 1: Selection (collaborators can search by name, orcid, or hospital - it pulls names (from orcid) + hospital + role in project).
                            # They can download pdf certificate auto-generated using markdown with their name / centre / role on it (based on what they select in tab 1).
                            # https://www.linkedin.com/pulse/shiny-app-r-integrating-filter-multiple-dynamic-conditions-lee-rock
                            # https://shiny.rstudio.com/articles/selectize.html
                            tabPanel("RECON Certificate",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectizeInput(inputId = "orcid",
                                                        label = "ORCID (Enter First)",
                                                        choices = auth$orcid,
                                                        selected = NULL, multiple = FALSE),
                                         # username as well
                                         selectInput("hospital",
                                                     "Hospital",
                                                     choices = ""),
                                         selectInput("team",
                                                     "Speciality Team",
                                                     choices = ""),
                                         selectInput("role",
                                                     "Role",
                                                     choices = ""),
                                         downloadButton("report", "Generate Certificate")),
                                     mainPanel(DT::dataTableOutput("table")))),
                                               
                            # Tab 2:  Shows how their name will be presented on the authorship list + shows full authorship  list.
                            tabPanel("RECON Authorship List",
                                     htmlOutput("auth_list"))))

server <- function(input, output, session){
  
  observe({updateSelectInput(session,
                             "hospital",
                             choices = filter(auth, orcid==input$orcid) %>% pull(hospital) %>% unique())})
  observe({updateSelectInput(session,
                             "team",
                             choices = filter(auth, orcid==input$orcid)  %>% pull(team) %>% unique() %>% na.omit())})
  observe({updateSelectInput(session,
                             "role",
                             choices = filter(auth, orcid==input$orcid) %>% pull(role) %>% unique())})
    
     reactiveDf <- reactive({
       if(input$orcid == ""){auth}
       
       shiny::req(input$hospital)
       if(input$orcid != ""){auth %>% filter(orcid==input$orcid)}
     })
                     
      output$table <- DT::renderDataTable(reactiveDf())
    
      output$auth_list <- renderUI({includeHTML(rmarkdown::render("report_auth.Rmd",
                                                                  params = list(orcid = input$orcid)))})
      
      # https://shiny.rstudio.com/articles/generating-reports.html
      output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "RECON_certificate.pdf",
        content = function(file) {
          
          # Copy the report file to a temporary directory before processing it, in case we don't have 
          # write permissions to the current working dir (which can happen when deployed).
          if(input$role=="Hospital Lead"){
            tempReport <- file.path(tempdir(), "recon_cert_hl.Rmd")
            file.copy("recon_cert_hl.Rmd", tempReport, overwrite = TRUE)}else{
              tempReport <- file.path(tempdir(), "recon_cert_dc.Rmd")
              file.copy("recon_cert_dc.Rmd", tempReport, overwrite = TRUE)}
          
          # Set up parameters to pass to Rmd document
          params2 <- list(orcid = input$orcid,
                          role = input$role,
                          hospital = input$hospital,
                          team = input$team)
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, output_file = file,
                            params = params2,
                            envir = new.env(parent = globalenv()))
          })
        
}

shinyApp(ui, server)