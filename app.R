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


# suporting functions
source("utility.R")

# Data folder
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Social Justice Platform WSs - Data Catalog")

# Catalog data
path_to_catalog <- file.path(data_folder, "sjp_data_catalog.xlsx")
sheet_names <- excel_sheets(path_to_catalog)
catalog <- lapply(sheet_names, function(X) read_excel(path_to_catalog, sheet = X))
names(catalog) <- sheet_names

list_of_tags <<- catalog$'list of tags'
tag_types <- unique(list_of_tags$'Tag Type')
  

# Define UI for application that draws a histogram

ui <- navbarPage(
  title = "Social Justice Platform Data Catalog",
  ## Main tab ----
  tabPanel(
    "Resources",
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        h6("Search Keywords"),
        fluidRow(
          column(
            width=9,
            textInput(
              inputId = "search",
              label = NULL,
              placeholder = "Search"
            )
          ),
          column(
            width=3,
            actionButton(
              inputId = "go",
              label = "Go"
            )
          )
        ),
        
        
        hr(),
        
        h6("Filter by Tags"),
        lapply(tag_types, function(t) {
          tags <- list_of_tags[list_of_tags$'Tag Type' == t, ]
          subcats <- sort(unique(tags$Subcategory), na.last = TRUE)
            
          # Display checkbox groups for each subcategory within each tag type
          div(
            # Overarching tag types
            style = 'padding-top: 5px',
            actionLink(
              inputId = paste0("label_", gsub(" ", "_", t)), 
              label = t, 
              icon("caret-right")
            ),
            
            # Show/hide the info within each tag type
            conditionalPanel(
              paste0("input.label_", gsub(" ", "_", t)," % 2 == 1"),
              
              if (all(is.na(subcats))) {
                # If there are no subcategories, just show the checkbox group
                div(
                  style = 'padding: 5px 0px 0px 1em',
                  checkboxGroupInput(
                    inputId = gsub(" ", "_", t),
                    label = NULL,
                    selected = NULL,
                    choiceNames = sort(list_of_tags$Tags[which(list_of_tags$'Tag Type' == t)]),
                    choiceValues = sort(list_of_tags$Tags[which(list_of_tags$'Tag Type' == t)])
                  )
                )
                
              } else {
                # Show subcategories within each tag type
                div(
                  style = 'padding-left: 1em', 
                  lapply(subcats, function(s) {
                    if (is.na(s)) {
                      # Assign any tags without a subcategory to "Other"
                      div(
                        style = 'padding-top: 5px',
                        actionLink(
                          inputId = paste("label", gsub(" ", "_", t), "Other", sep = "_"),
                          label = "Other",
                          icon("caret-right")
                        ),
                        conditionalPanel(
                          paste0("input.label_", gsub(" ", "_", t), "_Other", " % 2 == 1"),
                          div(
                            style = 'padding: 5px 0px 0px 1em',
                            checkboxGroupInput(
                              inputId = paste(gsub(" ", "_", t), "Other", sep = "--"),
                              label = NULL,
                              selected = NULL,
                              choiceNames = sort(tags$Tags[is.na(tags$'Subcategory')]),
                              choiceValues = sort(tags$Tags[is.na(tags$'Subcategory')])
                            )
                          )
                        )
                      )
                    } else {
                      # Otherwise group tags together by their designated subcategory
                      div(
                        style = 'padding-top: 5px',
                        actionLink(
                          inputId = paste("label", gsub(" ", "_", t), gsub(" ", "_", s), sep = "_"),
                          label = s,
                          icon("caret-right")
                        ),
                        conditionalPanel(
                          paste0("input.label_", gsub(" ", "_", t), "_", gsub(" ", "_", s), " % 2 == 1"),
                          div(
                            style = 'padding: 5px 0px 0px 1em',
                            checkboxGroupInput(
                              inputId = paste(gsub(" ", "_", t), gsub(" ", "_", s), sep = "--"),
                              label = NULL,
                              selected = NULL,
                              choiceNames = sort(tags$Tags[which(tags$'Subcategory' == s)]),
                              choiceValues = sort(tags$Tags[which(tags$'Subcategory' == s)])
                            )
                          )
                        )
                      )
                    }
                    
                    
                  })
                )
              }
            )
          )
        }),
        
        fluidRow(
          column(
            width = 12,
            align="center",
            
            br(),
            
            actionButton(
              inputId = "filter",
              label = "Filter"
            ),
            
            actionButton(
              inputId = "clear",
              label = "Clear Selections"
            )
          )
        )
      ),
      
      mainPanel(
        width = 9,
        div(
          align = "right",
          "Sort by: ",
          pickerInput(
            inputId = "sort_by",
            label = NULL,
            choices = c("Alphabetical", "Year: Oldest to Newest", "Year: Newest to Oldest"),
            selected = "Alphabetical",
            inline = TRUE
          ),
          "Show per page: ",
          pickerInput(
            inputId = "sort_by",
            label = NULL,
            choices = c(5, 10, 20, "All"),
            selected = 10,
            inline = TRUE
          ),
        ),
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
            ),
            dropdownButton(
              inputId = "export",
              label = "Export...",
              circle = FALSE,
              inline = TRUE,
              actionLink(
                inputId = "export_to_csv",
                label = "To CSV"
              ),
              actionLink(
                inputId = "export_to_pdf",
                label = "To PDF"
              )
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
  full_catalog <- full_catalog[order(full_catalog$Name), ]  #start off ordered alphabetically because that's what the default selection is
  colnames(full_catalog) <- unlist(lapply(colnames(full_catalog), function(x) {gsub(" ", "_", x)})) #changing column names so that there are no spaces
  
  # Start with all resources selected
  selected_rscs <- reactiveVal(full_catalog)
  
  # List of saved resources (i.e. shopping cart)
  shopping_list <- reactiveVal(list())
  
  # Toggle the carets on the checkbox menu labels
  lapply(tag_types, function(t) {
    # Labels for tag types
    observeEvent(input[[paste("label", gsub(" ", "_", t), sep = "_")]], {
      if (input[[paste("label", gsub(" ", "_", t), sep = "_")]] %% 2 == 1) {
        updateActionLink(
          session = session,
          inputId = paste("label", gsub(" ", "_", t), sep = "_"),
          icon = icon("caret-down")
        )
      } else {
        updateActionLink(
          session = session,
          inputId = paste("label", gsub(" ", "_", t), sep = "_"),
          icon = icon("caret-right")
        )
      }
    })
    
    # Labels for any subcategories
    subcats <- unique(list_of_tags[list_of_tags$'Tag Type' == t, ]$Subcategory)
    
    # If there are no subcategories, don't need to do anything more, so just checking for tag types with subcategories
    if (!all(is.na(subcats))) {
      lapply(subcats, function(s) {
        if (is.na(s)) {
          s <- "Other"
        }
        
        observeEvent(input[[paste("label", gsub(" ", "_", t), gsub(" ", "_", s), sep = "_")]], {
          if (input[[paste("label", gsub(" ", "_", t), gsub(" ", "_", s), sep = "_")]] %% 2 == 1) {
            updateActionLink(
              session = session,
              inputId = paste("label", gsub(" ", "_", t), gsub(" ", "_", s), sep = "_"),
              icon = icon("caret-down")
            )
          } else {
            updateActionLink(
              session = session,
              inputId = paste("label", gsub(" ", "_", t), gsub(" ", "_", s), sep = "_"),
              icon = icon("caret-right")
            )
          }
        })
      })
    } 
  })
    
  observeEvent(input$go, {
    search_terms <- input$search
    
  })
  
  observeEvent(input$filter, {
    # Update selected tags on button press
    selected_tags(
      lapply(tag_types, function(t) {
        # Want to group tags from the subcategories together to pass to selected_tags
        subcats <- unique(list_of_tags[list_of_tags$'Tag Type' == t, ]$Subcategory)
        select_tags_type <- c()
        if (all(is.na(subcats))) {
          # If there are no subcategories, checkbox group ID is just the 
          select_tags_type <- c(select_tags_type, input[[gsub(" ", "_", t)]])
        } else {
          for (s in subcats) {
            if (is.na(s)) {
              s <- "Other"
            }
            
            select_tags_type <- c(select_tags_type, input[[paste(gsub(" ", "_", t), gsub(" ", "_", s), sep = "--")]])
          }
        }
        
        return(select_tags_type)
      })
    )
    
    # Update names of the list to be each of the tag types
    selected_tags(setNames(selected_tags(), tag_types))
    
    # Making a local instance for easy reference
    select_tags <- selected_tags()
    
    tmp_catalog <- full_catalog
    
    # If tags have been selected, show only the sources for the selected tags
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
    }
    
    if (input$sort_by == "Alphabetical") {
      tmp_catalog <- tmp_catalog[order(tmp_catalog$Name), ]
    } else if (input$sort_by == "Year: Oldest to Newest") {
      # tmp <- tmp %>% arrange(Years_Available)
      tmp_catalog <- tmp_catalog[order(tmp_catalog$Years_Available, na.last=TRUE), ]
    } else {#if input$sort_by == "Year: Newest to Oldest"
      tmp_catalog <- tmp_catalog[order(tmp_catalog$Years_Available, na.last=TRUE, decreasing=TRUE), ]
    }
    
    selected_rscs(tmp_catalog)
  })
  
  observeEvent(input$clear, {
    # Reset all of the checkboxes
    for (t in tag_types) {
      subcats <- unique(list_of_tags[list_of_tags$'Tag Type' == t, ]$Subcategory)
      
      if (all(is.na(subcats))) {
        updateCheckboxGroupInput(
          session, 
          inputId = gsub(" ", "_", t), 
          selected = FALSE
        )
      } else {
        for (s in subcats) {
          if (is.na(s)) {
            s <- "Other"
          }
          
          updateCheckboxGroupInput(
            session, 
            inputId = paste(gsub(" ", "_", t), gsub(" ", "_", s), sep = "--"), 
            selected = FALSE
          )
        }
      }
    }
    
    # Clear the list of selected tags
    selected_tags(list())
    # Set selected_rscs back to the full catalog
    selected_rscs(full_catalog)
    # Reset the sort method to "Alphabetical"
    updatePickerInput(
      session,
      "sort_by",
      selected = "Alphabetical"
    )
  })
  
  output$sources_output <- renderUI({
    tmp_catalog <<- selected_rscs()
    
    if (nrow(tmp_catalog) > 0) {
      # Define all the collpase panels for each of the filtered resources
      collapseArgs <- lapply(1:nrow(tmp_catalog), function(i) {
        bsCollapsePanel(
          title = tmp_catalog[i, "Name"],
          value = paste0("rsc_", i),
          HTML(gen_rsc_info(tmp_catalog[i,])),
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
      
    } else {
      fluidRow(
        column(
          width = 12, 
          align = "center",
          HTML("<i>No resources match the criteria from your search.</i>")
        )
      )
    }

  })
  
  lapply(1:nrow(full_catalog), function(i) {
    # Individual "Add to Cart" buttons
    observeEvent(input[[paste0("add_to_cart_rsc_", i)]], {
      tmp <<- shopping_list()
      rsc_name <- selected_rscs()[i, "Name"]
      tmp[[rsc_name]] <<- selected_rscs()[i, ]
      shopping_list(tmp)
    })
    
    # Individual "Expand" buttons in cart
    observeEvent(input[[paste0("expand_cart_rsc_", i)]], {
      
    })
    
    # Individual "Remove from Cart" buttons
    observeEvent(input[[paste0("rmv_cart_rsc_", i)]], {
      tmp <- shopping_list()
      tmp <- tmp[-i]
      shopping_list(tmp)
    })
  })
  
  observeEvent(input$sort_by, {
    tmp <- selected_rscs()
    
    if (input$sort_by == "Alphabetical") {
      tmp <- tmp[order(tmp$Name, na.last=TRUE), ]
    } else if (input$sort_by == "Year: Oldest to Newest") {
      # tmp <- tmp %>% arrange(Years_Available)
      tmp <- tmp[order(tmp$Years_Available, na.last=TRUE), ]
    } else {#if input$sort_by == "Year: Newest to Oldest"
      tmp <- tmp[order(tmp$Years_Available, na.last=TRUE, decreasing=TRUE), ]
    }
    
    selected_rscs(tmp)
  })
  
  
  output$shopping_cart <- renderUI({
    if (length(shopping_list()) > 0) {
      lapply(1:length(shopping_list()), function(i) {
        rsc_name <- names(shopping_list())[i]
        rsc_info <- shopping_list()[[rsc_name]]
        rsc_type <- strsplit(rsc_info$Tags, ";")[[1]][1]
        rsc_url <- rsc_info$Link
        
        div(
          fluidRow(
            column(
              width = 4,
              align = "left",
              rsc_name
            ),
            
            column(
              width = 2,
              align = "left",
              rsc_type
            ),
            
            column(
              width = 4,
              align = "left",
              HTML("<a href=", rsc_url, ">", rsc_url, "</a>")
            ),
            
            column(
              width = 2,
              align = "right",
              actionButton(
                inputId = paste0("expand_cart_rsc_", i),
                label = "Expand"
              ),
              actionButton(
                inputId = paste0("rmv_cart_rsc_", i),
                label = icon("trash")
              )
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              
              conditionalPanel(
                paste0("input.expand_cart_rsc_", i," % 2 == 1"),
                HTML(gen_rsc_info(rsc_info)),
              )
              
            )
          )
        )
      })
    }
    
  })
  
  observeEvent(input$clear_cart, {
    shopping_list(list())
  })
  
  observeEvent(input$export_to_csv, {
    if (length(shopping_list()) > 0) {
      col_names <- names(shopping_list()[[names(shopping_list())[1]]])
      cart_df <- data.frame(matrix(ncol=length(col_names), nrow=0))
      colnames(cart_df) <- col_names
      
      for (n in names(shopping_list())){
        cart_df[nrow(cart_df)+1, ] <- shopping_list()[[n]]
      }
      
      write.csv(x = cart_df, file = choose.files(default=paste0("SavedResources_SJPCatalog_", Sys.Date(), ".csv"), caption="Save file", multi=FALSE))
    }
    
  })
  
  observeEvent(input$export_to_pdf, {
    print(shopping_list())
  })
  
  # output$export_to_pdf <- downloadHandler(
  #   filename = paste0("SavedResourcesReport_SJPCatalog_", Sys.Date(), ".pdf"),
  #   content = function(file) {
  #     # # Copy the report file to a temporary directory before processing it, in
  #     # # case we don't have write permissions to the current working dir (which
  #     # # can happen when deployed).
  #     # tmp_report <- file.path(tempdir(), "report.Rmd")
  #     # file.copy("report.Rmd", tmp_report, overwrite = TRUE)
  #     # 
  #     # # Set up parameters to pass to Rmd document
  #     # params <- list()
  #     # 
  #     # # Knit the document, passing in the `params` list, and eval it in a
  #     # # child of the global environment (this isolates the code in the document
  #     # # from the code in this app).
  #     # rmarkdown::render(tempReport, output_file = file,
  #     #                   params = params,
  #     #                   envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  

}

# Run the application 
shinyApp(ui = ui, server = server)
