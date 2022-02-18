#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(readxl)
library(plyr)
library(dplyr)
library(htmltools)
library(MITREShiny)


sheet_names <- excel_sheets("./data/sjp_data_catalog.xlsx")
catalog <- lapply(sheet_names, function(X) read_excel("./data/sjp_data_catalog.xlsx", sheet = X))
names(catalog) <- sheet_names

list_of_tags <- catalog$'list of tags'
tag_types <- unique(list_of_tags$'Tag Type')
  

# Define UI for application that draws a histogram

ui <- navbarPage(
  title = "Social Justice Platform Data Catalog",
  ## Main tab ----
  tabPanel(
    "Resources",
    
    sidebarLayout(
      sidebarPanel(
        textInput(
          inputId = "search",
          label = NULL,
          value = "Search"
        ),
        
        lapply(tag_types, function(t) {
          checkboxGroupInput(
            inputId = gsub(" ", "_", t),
            label = h4(t),
            selected = NULL,
            choiceNames = list_of_tags$Tags[list_of_tags$'Tag Type' == t],
            choiceValues = list_of_tags$Tags[list_of_tags$'Tag Type' == t]
          )
        }),
        
        fluidRow(
          column(
            width = 12,
            align="center",
            actionButton(
              inputId = "go",
              label = "Go"
            ),
            
            actionButton(
              inputId = "clear",
              label = "Clear Selections"
            )
          )
        )
      ),
      
      mainPanel(
        uiOutput(outputId = "sources_output")
      )
    )
  ),
  
  ## About tab ----
  tabPanel(
    "About",
  ),
  
  ## Insights tab ----
  tabPanel(
    "Insights",
  ),
  
  ## Shopping cart
  tabPanel(
    "Shopping Cart",
    
    fluidRow(
      column(width = 2),
      
      column(
        width = 8,
        fluidRow(
          column(
            width = 6,
            h2("Saved Resources")
          ),

          column(
            width = 6,
            align = "right",
            actionButton(
              inputId = "clear_cart",
              label = "Remove All"
            )
          )
        ),
        
        # tags$table(
        #   width = "100%",
        #   tags$thead(
        #     tags$tr(
        #       tags$th(
        #         align = "left",
        #         valign = "bottom",
        #         h2("Saved Resources")
        #       ),
        #       tags$th(
        #         align = "right",
        #         valign = "bottom",
        #         actionButton(
        #           inputId = "clear_cart",
        #           label = "Remove All"
        #         )
        #       )
        #     )
        #   )
        # ),
        
        hr(),
        
        uiOutput(outputId = "shopping_cart")
      ),
        
      column(width = 2)
    )
  )
)



server <- function(input, output, session) {
  # List of selected tags
  selected_tags <- reactiveVal(list())
  
  # Get all resources across all tabs
  datasets <- catalog$datasets
  repos <- catalog$'data repositories'
  methods <- catalog$methodologies
  tables <- catalog$tables
  tools <- catalog$tools
  full_catalog <- rbind.fill(datasets, repos, methods, tables, tools)
  
  # Start with all resources selected
  selected_rscs <- reactiveVal(full_catalog)
  
  # List of saved resources (i.e. shopping cart)
  shopping_list <- reactiveVal(list())
  
  observeEvent(input$go, {
    # Update selected tags on button press
    selected_tags(lapply(tag_types, function(t) {input[[gsub(" ", "_", t)]]}))
    selected_tags(setNames(selected_tags(), tag_types))
  })
  
  observeEvent(input$clear, {
    # Reset all of the checkboxes
    for (t in tag_types) {
      updateCheckboxGroupInput(
        session, 
        inputId = gsub(" ", "_", t), 
        selected = FALSE
      )
    }
    
    # Clear the list of selected tags
    selected_tags(list())
  })
  
  output$sources_output <- renderUI({
    # Get the list of selected tags
    select_tags <- selected_tags()
    
    tmp_catalog <- full_catalog
    
    # If tags have been selected, how only the sources for the selected tags
    if (!is.null(unlist(select_tags))) {
      # Filter according to each type of tag
      for (tag_type in names(select_tags)) {
        if (!is.null(select_tags[[tag_type]])) {
          # Get user-selected tags for this tag type
          get_select <- select_tags[[tag_type]]

          # Remove any entries that don't align with the selected tags'
          rmv_indices <- c()
          for (i in 1:nrow(tmp_catalog)) {
            # Get tags for i-th entry
            i_tags <- strsplit(tmp_catalog[i,]$Tags, ";")[[1]]
            i_tags <- trimws(i_tags)
            # If i-th resource is not of the selected tag types, remove it from catalog
            if (length(intersect(get_select, i_tags)) == 0) {
              rmv_indices <- c(rmv_indices, i)
            } 
          }
          tmp_catalog <- tmp_catalog[-rmv_indices,]
        }
      }
      
      selected_rscs(tmp_catalog)
    }
    
    # Define all the collpase panels for each of the filtered resources
    collapseArgs <- lapply(1:nrow(tmp_catalog), function(i) {
      bsCollapsePanel(
        title = tmp_catalog[i, "Name"],
        value = paste0("rsc_", i),
        HTML(paste0("<p><b>Decription:</b> ", tmp_catalog[i, "Description"], "</p>")),
        HTML(paste0("<p><b>Tags:</b> ", tmp_catalog[i, "Tags"], "</p>")),
        HTML(paste0("<p><b>URL:</b> <a href=", tmp_catalog[i, "Link"], ">", tmp_catalog[i, "Link"], "</a></p>")),
        fluidRow(
          column(
            width = 12,
            align = "right",
            actionButton(
              inputId = paste0("add_to_cart_rsc_", i), 
              label = icon("cart-plus")
            )
          )
        )
        
      )
    })
    
    # Additional bsCollapse arguments
    collapseArgs[["multiple"]] <- TRUE
    collapseArgs[["open"]] <- unlist(lapply(1:nrow(tmp_catalog), function(i) {paste0("rsc_", i)}))
    
    do.call(bsCollapse, collapseArgs)

  })
  
  lapply(1:nrow(full_catalog), function(i) {
    # Individual "Add to Cart" buttons
    observeEvent(input[[paste0("add_to_cart_rsc_", i)]], {
      tmp <- shopping_list()
      
      rsc_name <- selected_rscs()[i, "Name"]
      rsc_url <- selected_rscs()[i, "Link"]
      
      tmp[[rsc_name]] <- rsc_url
      shopping_list(tmp)
    })
    
    # Individual "Remove from Cart" buttons
    observeEvent(input[[paste0("rmv_cart_rsc_", i)]], {
      tmp <- shopping_list()
      tmp <- tmp[-i]
      shopping_list(tmp)
    })
  })
  
  
  output$shopping_cart <- renderUI({
    if (length(shopping_list()) > 0) {
      lapply(1:length(shopping_list()), function(i) {
        rsc_name <- names(shopping_list())[i]
        rsc_url <- shopping_list()[[names(shopping_list())[i]]]
        
        fluidRow(
          column(
            width = 4,
            align = "left",
            rsc_name
          ),
          
          column(
            width = 5,
            align = "left",
            HTML("<a href=", rsc_url, ">", rsc_url, "</a>")
          ),
          
          column(
            width = 1,
            align = "right",
            actionButton(
              inputId = paste0("rmv_cart_rsc_", i),
              label = icon("trash")
            )
          )
        )
      })
    }
    
  })
  
  observeEvent(input$clear_cart, {
    shopping_list(list())
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
