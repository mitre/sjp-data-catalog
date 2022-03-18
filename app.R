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
library(rmarkdown)
# library(shinysky)


# Data and Utility definitions ----

# Suporting functions
source("utility.R")

# Data folder
data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Social Justice Platform WSs - Data Catalog")

# Catalog data
path_to_catalog <- file.path(data_folder, "sjp_data_catalog.xlsx")
sheet_names <- excel_sheets(path_to_catalog)
ignore_sheets <- c("guidelines", "Copewell Indicators", "to review", "mitch_milestone_goals") #sheets to ignore
catalog <<- lapply(sheet_names, function(X) {
    if (!(X %in% ignore_sheets)) {
      read_excel(path_to_catalog, sheet = X) 
    }
  })
names(catalog) <<- sheet_names

# Tags info
list_of_tags <<- catalog$'list of tags'
tag_types <- unique(list_of_tags$'Tag Type')

  


# UI ----
ui <- MITREnavbarPage(
  title = "Social Justice Platform Data Catalog",
  ## Main tab ----
  tabPanel(
    "Search Catalog",
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        h6("Search Keywords"),
        splitLayout(
          cellWidths = c("85%", "15%"), 
          # Example of search bar recommendations (library shinysky not compatible with current R version)
          # textInput.typeahead(
          #   id="thti"
          #   ,placeholder="type 'name' or '2'"
          #   ,local=data.frame(name=c("name1","name2"),info=c("info1","info2"))
          #   ,valueKey = "name"
          #   ,tokens=c(1,2)
          #   ,template = HTML("<p class='repo-language'>{{info}}</p> 
          #   <p class='repo-name'>{{name}}</p> 
          #   <p class='repo-description'>You need to learn more CSS to customize this further</p>")
          # ),
          textInput(
            inputId = "search",
            label = NULL,
            placeholder = "Search",
          ),
          actionButton(
            inputId = "go",
            label = "Go",
            class="btn-primary"
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
              condition = paste0("input.label_", gsub(" ", "_", t)," % 2 == 1"),
              
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
                          condition = paste0("input.label_", gsub(" ", "_", t), "_Other", " % 2 == 1"),
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
                          condition = paste0("input.label_", gsub(" ", "_", t), "_", gsub(" ", "_", s), " % 2 == 1"),
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
              label = "Filter",
              class="btn-primary"
            ),
            
            actionButton(
              inputId = "clear",
              label = "Clear Selections",
              class="btn-secondary"
            )
          )
        )
      ),
      
      mainPanel(
        width = 9,
        
        fluidRow(
          column(
            width = 6,
            align = "left",
            uiOutput(outputId = "num_results")
          ),
          column(
            width = 6,
            align = "right",
            "Sort by: ",
            pickerInput(
              inputId = "sort_by",
              label = NULL,
              choices = c("Alphabetical", "Year: Oldest to Newest", "Year: Newest to Oldest"),
              selected = "Alphabetical",
              inline = TRUE
            ),
            HTML("&nbsp&nbspShow per page: "),
            pickerInput(
              inputId = "show_per_page",
              label = NULL,
              choices = c(5, 10, 20, "All"),
              selected = 10,
              inline = TRUE
            )
          )
        ),
        
        # div(
        #   align = "right",
        #   "Sort by: ",
        #   pickerInput(
        #     inputId = "sort_by",
        #     label = NULL,
        #     choices = c("Alphabetical", "Year: Oldest to Newest", "Year: Newest to Oldest"),
        #     selected = "Alphabetical",
        #     inline = TRUE
        #   ),
        #   HTML("&nbsp&nbspShow per page: "),
        #   pickerInput(
        #     inputId = "show_per_page",
        #     label = NULL,
        #     choices = c(5, 10, 20, "All"),
        #     selected = 10,
        #     inline = TRUE
        #   ),
        # ),
        
        uiOutput(outputId = "sources_output"),
        conditionalPanel(
          condition = "!output.no_matches",
          fluidRow(
            column(
              width = 12,
              align = "center",
              actionButton(
                inputId = "prev_page",
                label = icon("angle-left"),
                class = "btn-secondary"
              ),
              uiOutput(outputId = "page_number", inline=TRUE),
              actionButton(
                inputId = "next_page",
                label = icon("angle-right"),
                class = "btn-secondary"
              )
            )
          )
        )
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
    "Saved Resources",
    
    fluidRow(
      column(width = 2),
      
      column(
        width = 8,
        
        fluidRow(
          column(
            width = 6,
            div(
              style = "display: inline-block; padding-top: 8px",
              h2("Saved Resources")
            )
          ),

          column(
            width = 6,
            align = "right",
            div(
              style = "display: inline-block; padding-top: 25px",
              actionButton(
                inputId = "clear_cart",
                label = "Remove All",
                class="btn-danger"
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
          )
        ),
        
        hr(),
        
        uiOutput(outputId = "shopping_cart")
      ),
        
      column(width = 2)
    )
  )
)

# Helper functions ----
get_num_pages <- function(df, per_page) {
  if (nrow(df) %% per_page == 0) {
    total_pages <- nrow(df) %/% per_page  #default show_per_page=10
  } else {
    total_pages <- nrow(df) %/% per_page + 1  #default show_per_page=10
  }
  return(total_pages)
}

sort_by_criteria <- function(df, criteria) {
  if (criteria == "Alphabetical") {
    df <- df[order(df$Name), ]
  } else if (criteria == "Year: Oldest to Newest") {
    df <- df[order(df$Years_Available, na.last=TRUE), ]
  } else {#if criteria == "Year: Newest to Oldest"
    df <- df[order(df$Years_Available, na.last=TRUE, decreasing=TRUE), ]
  }
  return(df)
}


# Server ----
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
  full_catalog <- sort_by_criteria(full_catalog, "Alphabetical") #start off ordered alphabetically because that's what the default selection is
  colnames(full_catalog) <- unlist(lapply(colnames(full_catalog), function(x) {gsub(" ", "_", x)})) #changing column names so that there are no spaces
  
  # Similarity matrix between all resources
  catalog_features <<- feature_vectors(full_catalog, list_of_tags$Tags, catalog$`methodology--data`)
  rsc_sim <<- sim_matrix(catalog_features, sim_measure="cosine")
  
  all_sims <- rsc_sim[lower.tri(rsc_sim)]
  # sim_thresh <- quantile(all_sims)[["75%"]]
  sim_thresh <- mean(all_sims) + 2 * sd(all_sims)
  
  shared_tags <<- shared_features(catalog_features, "tags", total_tags=length(list_of_tags$Tags))
  shared_methods <<- shared_features(catalog_features, "methodologies", total_methods=nrow(tmp_catalog[grepl("Methodology", tmp_catalog$Tags), ]))
  
  # Current page
  current_page <- reactiveVal(1)
  # Total number of pages to view
  total_pages <- reactiveVal(get_num_pages(full_catalog, 10))  #default show_per_page=10
  
  # Initialize server-side variable that conditional panel can access from UI-side
  no_matches <- reactiveVal(FALSE)
  output$no_matches <- eventReactive(no_matches(), {
    return(no_matches())
  })
  outputOptions(output, "no_matches", suspendWhenHidden = FALSE)  #need this to access variable from UI
  
  # Start with all resources selected
  selected_rscs <- reactiveVal(full_catalog)
  
  # Default is to show first 10 resources
  page_rscs <- reactiveVal(full_catalog[1:10, ])
  
  # List of saved resources (i.e. shopping cart)
  shopping_list <- reactiveVal(list())
  
  
  
  # Display number of total results and which ones are currently being shown
  output$num_results <- renderUI({
    if (input$show_per_page == "All") {
      div(
        style = 'padding-top: 32px',
        HTML(paste0("<i>Showing all of ", nrow(selected_rscs())," results</i>"))
      )
    } else {
      start_i <- (strtoi(input$show_per_page) * (current_page() - 1) + 1)
      end_i <- min((strtoi(input$show_per_page) * current_page()), nrow(selected_rscs()))
      div(
        style = 'padding-top: 32px',
        HTML(paste0("<i>Showing ", start_i,"-", end_i," of ", nrow(selected_rscs())," results</i>"))
      )
    }
  })
  
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
    # Look for resources which have any fields that include a partial match to the search term(s)
    if (input$search != "") {
      search_terms <- input$search
      include_terms <- t(matrix(grepl(search_terms, t(full_catalog), ignore.case = TRUE), ncol=nrow(full_catalog)))
      rsc_index <- unlist(lapply(1:nrow(include_terms), function(i) {any(include_terms[i, ])}))
      tmp_catalog <- full_catalog[rsc_index, ]

      tmp_catalog <- sort_by_criteria(tmp_catalog, input$sort_by)
      
      selected_rscs(tmp_catalog)
      
      # Reset to first page
      current_page(1)
    }
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
    
    tmp_catalog <- sort_by_criteria(tmp_catalog, input$sort_by)
    
    selected_rscs(tmp_catalog)
    
    # Reset to first page when filters change
    current_page(1)
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
    
    # Reset the search bar
    updateTextInput(
      session,
      inputId = "search",
      value = ""
    )
    
    # Clear the list of selected tags
    selected_tags(list())
    # Set selected_rscs back to the full catalog
    selected_rscs(sort_by_criteria(full_catalog, input$sort_by))
    
    # Reset to first page
    current_page(1)
  })
  
  output$sources_output <- renderUI({
    tmp_catalog <<- selected_rscs()
    
    if (nrow(tmp_catalog) > 0) {
      no_matches(FALSE)
      
      # Get resources to be shown on current page
      if (input$show_per_page != "All") {
        # Need to recompute how many pages there should be every time output changes
        total_pages(get_num_pages(selected_rscs(), strtoi(input$show_per_page)))
        
        start_i <- (strtoi(input$show_per_page) * (current_page() - 1) + 1)
        end_i <- min((strtoi(input$show_per_page) * current_page()), nrow(tmp_catalog))
        indices <- start_i:end_i
        page_rscs(tmp_catalog[indices, ])
        
      } else {
        total_pages(1)
        page_rscs(tmp_catalog)
      }
      
      # Define all the collapse panels for each of the filtered resources
      collapseArgs <- lapply(1:nrow(page_rscs()), function(i) {
        bsCollapsePanel(
          title = page_rscs()[i, "Name"],
          value = paste0("rsc_", i),
          HTML(gen_rsc_info(page_rscs()[i,])),
          fluidRow(
            column(
              width = 6,
              align = 'left',
              style = "padding-top: 1em; margin-bottom: -1em;",
              materialSwitch(
                inputId = paste0('show_more_rscs_', i),
                label = NULL,
                value = FALSE,
                status = "primary",
                inline = TRUE,
              ),
              "Show me similar resources"
            ),
            
            column(
              width = 6,
              align = "right",
              actionButton(
                inputId = paste0("add_to_cart_rsc_", i), 
                label = "Save",
                icon = icon("heart"),
                class = "btn-primary"
              )
            )
          ),
          conditionalPanel(
            condition = paste0("input.show_more_rscs_", i),
            br(),
            # "Placeholder for resource recommendations",
            uiOutput(outputId = paste0("rsc_recs_", i)),
            br()
          )
        )
      })
      
      # Additional bsCollapse arguments
      collapseArgs[["multiple"]] <- TRUE
      collapseArgs[["open"]] <- unlist(lapply(1:nrow(page_rscs()), function(i) {paste0("rsc_", i)}))
      
      do.call(bsCollapse, collapseArgs)
      
    } else {
      no_matches(TRUE)
      fluidRow(
        column(
          width = 12, 
          align = "center",
          HTML("<i>No resources match the criteria from your search.</i>")
        )
      )
    }

  })
  
  # Display what page user is currently viewing
  output$page_number <- renderUI({
    HTML(paste("Page", current_page(), "of", total_pages()))
  })
  
  observeEvent(input$prev_page, {
    page <- max(1, current_page()-1)
    current_page(page)
  })
  
  observeEvent(input$next_page, {
    page <- min(current_page()+1, total_pages())
    current_page(page)
  })
  
  # Functions for each of the resource-specific buttons
  # Need to apply function up to nrow(full_catalog) because can't access length of selected_rscs() outside of render/observe
  lapply(1:nrow(full_catalog), function(i) {
    # Show source recommendations
    output[[paste0("rsc_recs_", i)]] <- renderUI({
      # Get most similar resources (i.e. all with similarity greater than pre-set threshold)
      rsc_name <- page_rscs()[i, "Name"]
      recs <- c(rsc_sim[rsc_name, ])[order(-unlist(rsc_sim[rsc_name, ]))]
      n_recs_show <- sum(recs > sim_thresh, na.rm=TRUE)
      recs <- recs[1:n_recs_show]
      
      # Colors for displaying the recommended source boxes
      bkgd_color <- "#f0f7ff"
      bord_color <- "#005B94"  #MITRE blue
      
      if (n_recs_show == 0) {
        # Let user know if there are no sources to recommend
        HTML("<i>No recommended resources to show.</i>")
        
      } else {
        # Create a horizontal scrolling div with all recommended resources
        recs_list <- lapply(1:length(recs), function(j) {
          n_shared_tags <- shared_tags[rsc_name, names(recs[j])]
          n_shared_methods <- shared_methods[rsc_name, names(recs[j])]
          
          desc <- full_catalog[full_catalog["Name"] == names(recs[j]), ][["Description"]]
          disp_str <- paste0("<b>", names(recs[j]), "</b><p style='width: 375px; overflow:hidden; text-overflow: ellipsis; display: -webkit-box; -webkit-line-clamp: 2; line-clamp: 2; -webkit-box-orient: vertical;'>", desc, "</p><p>")
          
          if (n_shared_tags > 0) {
            if (n_shared_tags == 1) {
              disp_str <- paste0(disp_str, n_shared_tags, " shared tag; ")
            } else {
              disp_str <- paste0(disp_str, n_shared_tags, " shared tags; ")
            }
          }
          
          if (n_shared_methods > 0) {
            if (n_shared_methods == 1) {
              disp_str <- paste0(disp_str, n_shared_methods, " shared methodology; ")
            } else {
              disp_str <- paste0(disp_str, n_shared_methods, " shared methodologies; ")
            }
          }
          
          disp_str <- paste0(substr(disp_str, 0, nchar(disp_str)-2), "</p>")
          
          div(
            style = paste0("position: relative; flex: 0 0 375px; padding: 10px; margin: 0px 5px; background-color: ", bkgd_color,"; border-radius: 5px; border-style: solid; border-width: 1px; border-color: ", bord_color),
            fluidRow(
              style = "margin-bottom: 2.5em;",
              column(
                width = 12,
                HTML(disp_str)
              )
            ),
            # br(),
            # br(),
            fluidRow(
              style = "position: absolute; bottom: 10px; right: 10px;",
              column(
                width = 12,
                align = "right",
                actionButton(inputId = paste0("go_to_rec_", i, j), label = "Take me here!", class = "btn-secondary"),
                actionButton(inputId = paste0("save_rec_", i, j), label = icon("heart"), class = "btn-primary")
              )
            )
          )
        })
        
        fluidRow(
          style = "padding: 5px; margin-bottom: -1.5em",
          column(
            width = 12,
            div(
              style = "width: 100%; overflow-x: auto; display:inline-flex;",
              recs_list
            )
          )
        )
      }
    })
    
    # "Save" buttons
    observeEvent(input[[paste0("add_to_cart_rsc_", i)]], {
      tmp <<- shopping_list()
      rsc_name <- page_rscs()[i, "Name"]
      tmp[[rsc_name]] <<- page_rscs()[i, ]
      shopping_list(tmp)
    })
    
    # "Remove from Cart" buttons
    observeEvent(input[[paste0("rmv_cart_rsc_", i)]], {
      tmp <- shopping_list()
      tmp <- tmp[-i]
      shopping_list(tmp)
    })
  })
  
  observeEvent(input$sort_by, {
    tmp <- selected_rscs()
    
    # Sort the resources output according to user input
    tmp <- sort_by_criteria(tmp, input$sort_by)
    
    selected_rscs(tmp)
    
    # Reset to first page
    current_page(1)
  })
  
  observeEvent(input$show_per_page, {
    if (input$show_per_page != "All") {
      # Compute how many pages there should be based on user input
      total_pages(get_num_pages(selected_rscs(), strtoi(input$show_per_page)))
    } else {
      total_pages(1)
    }
    # Go back to first page if changing the number of resources per page
    current_page(1)
  })
  
  
  output$shopping_cart <- renderUI({
    if (length(shopping_list()) > 0) {
      # Headers
      div(
        # Generate line items for each of the saved resources
        lapply(1:length(shopping_list()), function(i) {
          rsc_name <- names(shopping_list())[i]
          rsc_info <- shopping_list()[[rsc_name]]
          rsc_type <- strsplit(rsc_info$Tags, ";")[[1]][1]
          rsc_url <- rsc_info$Link
          
          # if (i %% 2 == 1) {
          #   bkgd_color <- "#dceafc"
          # } else {
          #   bkgd_color <- "#ffffff"
          # }
          
          # bord_color <- "#0D2F4F"  #MITRE navy blue
          
          bkgd_color <- "#f0f7ff"
          bord_color <- "#005B94"  #MITRE blue
          # bord_color <- "#bfddff"
          
          div(
            # Basic resource info
            fluidRow(
              style = paste0("padding-top: 7px; padding-bottom: 7px; margin-top: 5px; border-radius: 5px; border-style: solid; border-width: 1px; border-color: ", bord_color, "; background-color: ", bkgd_color),
              column(
                width = 4,
                style = "padding-top: 5px",
                rsc_name
              ),
              
              column(
                width = 2,
                style = "padding-top: 5px",
                rsc_type
              ),
              
              column(
                width = 4,
                style = "padding-top: 5px",
                HTML("<a href=", rsc_url, ">", rsc_url, "</a>")
              ),
              
              # Resource-specific buttons
              column(
                width = 2,
                align = "right",
                actionButton(
                  inputId = paste0("expand_cart_rsc_", i),
                  label = "Expand",
                  class="btn-secondary"
                ),
                actionButton(
                  inputId = paste0("rmv_cart_rsc_", i),
                  label = icon("trash"),
                  class="btn-primary"
                )
              )
            ),
            
            # More detailed resource info revealed by the Expand button
            fluidRow(
              column(
                width = 12,
                
                conditionalPanel(
                  condition = paste0("input.expand_cart_rsc_", i," % 2 == 1"),
                  style = paste0("padding: 10px; background-color: #ffffff; margin: -0.25em -1.1em 0px -1.1em; border-radius: 0px 0px 5px 5px; border-style: solid; border-width: 1px; border-color: ", bord_color),
                  HTML(gen_rsc_info(rsc_info))
                )
                
              )
            )
          )
        })
      )
    } else {
      fluidRow(
        column(
          width = 12, 
          align = "center",
          HTML("<br><i>Your currently have no saved resources.</i>")
        )
      )
    }
    
  })
  
  observeEvent(input$clear_cart, {
    if (length(shopping_list()) > 0) {
      # Take user to modal to make sure they actually want to clear their cart
      showModal(
        modalDialog(
          HTML("<p style=\"font-size: 15px\">Are you sure you want to clear your list? This will remove all saved resources.</p>"),
          footer = div(
            align = "center", 
            actionButton("clear_no", label="No, take me back", class="btn-secondary"),
            actionButton("clear_yes", label = "Yes, clear all", class="btn-primary")
          ),
          easyClose = FALSE
        )
      )
    }
  })
  
  # Changed mind--do nothing
  observeEvent(input$clear_no, {
    removeModal()
  })
  
  # Button that will execute clear cart once user has confirmed this is actually what they want
  observeEvent(input$clear_yes, {
    removeModal()
    shopping_list(list())
  })
  
  
  # Exports a dataframe of all saved resources and their associated fields to a csv
  observeEvent(input$export_to_csv, {
    if (length(shopping_list()) > 0) {
      col_names <- names(shopping_list()[[names(shopping_list())[1]]])
      cart_df <- data.frame(matrix(ncol=length(col_names), nrow=0))
      colnames(cart_df) <- col_names
      
      for (n in names(shopping_list())){
        cart_df[nrow(cart_df)+1, ] <- shopping_list()[[n]]
      }
      
      write.csv(x = cart_df, file = choose.files(default=paste0("SavedResources_SJPCatalog_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"), caption="Save file", multi=FALSE))
      
      showModal(
        modalDialog(
          HTML("Successfully downloaded a CSV with all saved resources!"),
          footer = NULL,
          easyClose = TRUE
        )
      )
    } 
  })
  
  # Exports a PDF report with the list of saved resources as well as some other info
  observeEvent(input$export_to_pdf, {
    if (length(shopping_list()) > 0) {
      filename = paste0("SavedResourcesReport_SJPCatalog_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf")
      
      tmp_report <- file.path(tempdir(), "report.Rmd")
      file.copy("www/report.Rmd", tmp_report, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = length(selected_rscs()))
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tmp_report, 
        output_file = filename,
        output_dir = normalizePath("./test"),
        params = params,
        envir = new.env(parent = globalenv())
      )
      
      showModal(
        modalDialog(
          HTML("Successfully downloaded a report with all saved resources!"),
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
