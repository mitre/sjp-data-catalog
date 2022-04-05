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
library(bsplus)
library(rintrojs)
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

# Get all resources across all tabs
datasets <- catalog$datasets
repos <- catalog$`data repositories`
methods <- catalog$methodologies
tables <- catalog$tables
tools <- catalog$tools
full_catalog <- rbind.fill(datasets, repos, methods, tables, tools)
full_catalog <- sort_by_criteria(full_catalog, "Alphabetical") #start off ordered alphabetically because that's what the default selection is
colnames(full_catalog) <- unlist(lapply(colnames(full_catalog), function(x) {gsub(" ", "_", x)})) #changing column names so that there are no spaces

# Get the data sources contained in the methodologies
methods_data <- catalog$`methodology--data`

# Generate features vectors and compute all similarity values
catalog_features <<- feature_vectors(full_catalog, list_of_tags$Tags, catalog$`methodology--data`)
all_sim_values <- get_all_sims(catalog_features, sim_measure="cosine", total_tags=length(list_of_tags$Tags), total_methods=nrow(full_catalog[grepl("Methodology", full_catalog$Tags), ]))

# Similarity matrix between all resources
rsc_sim <<- all_sim_values$sim_matrix
all_sims <- rsc_sim[lower.tri(rsc_sim)]
# sim_thresh <- quantile(all_sims)[["75%"]]
# sim_thresh <- mean(all_sims) + 1.64 * sd(all_sims)
sim_thresh <- quantile(all_sims, 0.95)

# Number of shared tags and methodologies
shared_tags <<- all_sim_values$tags
shared_methods <<- all_sim_values$methods

# Get most similar resources (i.e. all with similarity greater than pre-set threshold)
all_rsc_recs <- list()
num_recs <- c()
for (i in 1:nrow(full_catalog)) {
  rsc_name <- full_catalog[i, "Name"]
  recs <- c(rsc_sim[rsc_name, ])[order(-unlist(rsc_sim[rsc_name, ]))]
  n_recs_show <- sum(recs > sim_thresh, na.rm=TRUE)
  if (n_recs_show == 0) {
    all_rsc_recs[[rsc_name]] <- c()
    num_recs <- c(num_recs, 0)
  } else {
    all_rsc_recs[[rsc_name]] <- recs[1:n_recs_show]
    num_recs <- c(num_recs, n_recs_show)
  }
}
max_num_recs <- max(num_recs) #for generating buttons in the server

# Get min and max years across catalog
years_avail <<- full_catalog[order(full_catalog$Years_Available, na.last=TRUE, decreasing=FALSE), "Years_Available"]
years_avail <<- years_avail[!(is.na(years_avail)) & years_avail != "N/A" & !(grepl("Varies", years_avail))]  #get values that aren't missing or actual text input (e.g. "N/A", "Varies")
min_year <- strtoi(substr(years_avail[1], 1, 4))  #pull first 4 characters (YYYY) of first element in years_avail
end_yrs <- unlist(lapply(years_avail, function(y) {
  strtoi(substr(tail(strsplit(tail(trimws(strsplit(y, ",")[[1]]), n=1), "-")[[1]], n=1), 1, 4))
}))  #get all of the ending years of each entry
max_year <- max(end_yrs)  #take the max of all ending years to get overall max year

# Get all available geographic levels
geo_levels <- c("National", "State", "County", "City", "Zip Code", "ZCTA", "Census Tract", "Census Block")

  


# UI ----
ui <- MITREnavbarPage(
  title = "Social Justice Platform Data Catalog",
  id = "navbar_tabs",
  header = includeCSS("www/style.css"),

  ## Main tab ----
  tabPanel(
    title = "Search Catalog",
    value = "search_tab",
    
    # To activate the use of popovers in your page
    use_bs_popover(),
    
    # To activate the rintrojs tutorial package
    introjsUI(),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        HTML("<h6>Search Keywords"),
        shiny_iconlink() %>%
          bs_embed_popover(
            title = NULL,
            content = "This feature will search through the entire catalog for matches to the keywords.",
            html = TRUE,
            placement = "right",
            trigger = "hover",
            options = list(container = "body")),
        HTML("</h6>"),
        
        splitLayout(
          cellWidths = c("85%", "15%"), 

          textInput(
            inputId = "search",
            label = NULL,
            placeholder = "Search",
          ),
          actionButton(
            inputId = "go",
            label = "Go",
            class = "btn-primary"
          )
        ),
        
        hr(),
        
        # # Buttons for enacting filters and for clearing any existing selections
        # fluidRow(
        #   column(
        #     width = 12,
        #     style = "border-style: solid",
        #     align = "center",
        #     
        #     br(),
        #     
        #     actionButton(
        #       inputId = "filter",
        #       label = "Filter",
        #       class = "btn-primary"
        #     ),
        #     
        #     actionButton(
        #       inputId = "clear",
        #       label = "Clear Selections",
        #       class = "btn-secondary"
        #     )
        #   )
        # ),
        
        h6("Filter by Tags"),
        lapply(tag_types, function(t) {
          tags <- list_of_tags[list_of_tags$'Tag Type' == t, ]
          subcats <- sort(unique(tags$Subcategory), na.last = TRUE)
            
          # Display checkbox groups for each subcategory within each tag type
          div(
            # Overarching tag types
            class = 'tag_group_label',
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
                  class = 'tag_chkbox',
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
                  class = 'tag_subcat', 
                  lapply(subcats, function(s) {
                    if (is.na(s)) {
                      # Assign any tags without a subcategory to "Other"
                      div(
                        class = 'tag_group_label',
                        actionLink(
                          inputId = paste("label", gsub(" ", "_", t), "Other", sep = "_"),
                          label = "Other",
                          icon("caret-right")
                        ),
                        conditionalPanel(
                          condition = paste0("input.label_", gsub(" ", "_", t), "_Other", " % 2 == 1"),
                          div(
                            class = 'tag_chkbox',
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
                        class = 'tag_group_label',
                        actionLink(
                          inputId = paste("label", gsub(" ", "_", t), gsub(" ", "_", s), sep = "_"),
                          label = s,
                          icon("caret-right")
                        ),
                        conditionalPanel(
                          condition = paste0("input.label_", gsub(" ", "_", t), "_", gsub(" ", "_", s), " % 2 == 1"),
                          div(
                            class = 'tag_chkbox',
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
        
        hr(),
        
        h6("Filter by Year"),
        
        sliderInput(
          inputId = "filter_year", 
          label = NULL, 
          sep = "",
          min = min_year, 
          max = max_year, 
          step = 1,
          value = c(min_year, max_year)
        ),
        
        checkboxInput(
          inputId = "filter_year_na",
          label = div(
            "Include resources without designated years",
            shiny_iconlink(class = "help_btn") %>%
            bs_embed_popover(
              title = NULL,
              content = "Not all resources have year availability (e.g. Repositories, Methodologies, Tools). Check this box to include resources without years in the filtered results.",
              HTML = TRUE,
              trigger = "hover",
              options = list(container = "body"))
          ),
          value = TRUE
        ),
        
        hr(),
        
        HTML("<h6>Filter by Geographic Level"),
        shiny_iconlink() %>%
          bs_embed_popover(
            title = NULL,
            content = "Filter here by the most common geographic levels. You can use the search bar if you are looking for a more specific level.",
            html = TRUE,
            placement = "right",
            trigger = "hover",
            options = list(container = "body")),
        HTML("</h6>"),
        
        div(
          class = 'geo_lvl_chkbox',
          checkboxGroupInput(
            inputId = "filter_geo_lvls",
            label = NULL,
            selected = NULL,
            choiceNames = geo_levels,
            choiceValues = geo_levels
          )
        ),
        
        # Buttons for enacting filters and for clearing any existing selections
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
          # Show the number of resources displayed on the current page
          column(
            width = 6,
            align = "left",
            uiOutput(outputId = "num_results")
          ),
          # Drop downs for sorting resources and how many resources to show per page
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
        
        # Main output showing resource cards
        uiOutput(outputId = "sources_output"),
        
        # Forward and back page buttons -- only visible if there are resources to show
        conditionalPanel(
          condition = "!output.no_matches",
          br(),
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
    title = "About",
    value = "about_tab"
  ),
  
  ## Insights tab ----
  tabPanel(
    title = "Insights",
    value = "insights_tab"
  ),
  
  ## Shopping cart ----
  tabPanel(
    title = uiOutput("saved_res_title"),
    value = "saved_res_tab",
    
    fluidRow(
      # Left-side padding
      column(width = 2),
      
      column(
        width = 8,

        fluidRow(
          column(
            width = 6,
            div(
              class = "saved_res_header",
              h2("Saved Resources")
            )
          ),

          # Buttons for clearing cart and for exporting cart to various file types
          column(
            width = 6,
            align = "right",
            div(
              class = "saved_res_btns",
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
        
        # All cart contents
        uiOutput(outputId = "shopping_cart")
      ),
      
      # Right-side padding
      column(width = 2)
    )
  )
)




# Server ----
server <- function(input, output, session) {
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
  
  # Keep track of what solo recommended resource card is open
  rec_open <- reactiveVal("")
  
  # Keep track of whether a new item has been added to the cart since the last time the cart was viewed
  save_clicked <- reactiveVal(FALSE)
  
  
  
  # Update the "Saved Resources" navbar title whenever someone adds a resource to their cart
  output$saved_res_title <- renderUI({
    # if (input$navbar_tabs == "saved_res_tab") {
    #   "Saved Resources"
    # } else {
    #   if (save_clicked()) {
    #     "(Saved Resources)" 
    #   } else {
    #     "Saved Resources"
    #   }
    # }
    
    if (save_clicked()) {
      "(Saved Resources)" 
    } else {
      "Saved Resources"
    }
    
  })
  
  observeEvent(input$navbar_tabs, {
    if (input$navbar_tabs == "saved_res_tab") {
      # Reset save_clicked every time the user views their cart
      save_clicked(FALSE)
    }
  })
  
  # Display number of total results and which ones are currently being shown
  output$num_results <- renderUI({
    if (input$show_per_page == "All") {
      div(
        class = 'showing_n_res',
        HTML(paste0("<i>Showing all of ", nrow(selected_rscs())," results</i>"))
      )
    } else {
      start_i <- (strtoi(input$show_per_page) * (current_page() - 1) + 1)
      end_i <- min((strtoi(input$show_per_page) * current_page()), nrow(selected_rscs()))
      div(
        class = 'showing_n_res',
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
        
        # Toggle the carets on the checkbox menu labels
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
  
  # "Go" button for the search bar
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
  
  # Button for filtering according to selected checkboxes
  observeEvent(input$filter, {
    tmp_catalog <- full_catalog
    
    # Update selected tags on button press
    selected_tags <-
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
    
    
    # Update names of the list to be each of the tag types
    selected_tags <- setNames(selected_tags, tag_types)
    
    # Get sources with the selected tags
    if (!is.null(unlist(selected_tags))) {
      # Filter according to each type of tag
      for (tag_type in names(selected_tags)) {
        if (!is.null(selected_tags[[tag_type]])) {
          # Get user-selected tags for this tag type
          get_select <- selected_tags[[tag_type]]
          
          # Remove any entries that don't align with the selected tags
          keep_indices <- c()
          for (i in 1:nrow(tmp_catalog)) {
            # Get tags for i-th entry
            i_tags <- strsplit(tmp_catalog[i, "Tags"], ";")[[1]]
            i_tags <- trimws(i_tags)
            # If i-th resource is not of the selected tag types, remove it from catalog
            if (length(intersect(get_select, i_tags)) > 0) {
              keep_indices <- c(keep_indices, i)
            } 
          }
          tmp_catalog <- tmp_catalog[keep_indices,]
        }
      }
    }
    
    # Further filter by year
    if (input$filter_year[1] != min_year || input$filter_year[2] != max_year || !input$filter_year_na) {
      selected_years <<- seq.int(input$filter_year[1], input$filter_year[2])
      
      keep_indices <- c()
      for (i in 1:nrow(tmp_catalog)) {
        i_yr_val <- tmp_catalog[i, "Years_Available"]
        # Ignore if year data is missing or has string inputs (e.g. "N/A", "Varies")
        if (!(is.na(i_yr_val)) & i_yr_val != "N/A" & !(grepl("Varies", i_yr_val))) {
          # Remove any characters between parentheses and remove any trailing whitespace
          i_yr_str <- trimws(gsub("\\(.*\\)", "", strsplit(i_yr_val, ",")[[1]]))
          # Figure out of selected years overlaps with years available of selected resources
          i_yrs <- unlist(lapply(i_yr_str, function(y) {
            rng <- strtoi(strsplit(y, "-")[[1]])
            if (length(rng) == 2) {
              seq.int(rng[1], rng[2])
            } else {#length(rng) == 1
              rng[1]
            }
          }))
          if (length(intersect(selected_years, i_yrs)) > 0) {
            keep_indices <- c(keep_indices, i)
          }
        } else {
          if (input$filter_year_na) {
            keep_indices <- c(keep_indices, i)
          }
        }
      }
      tmp_catalog <- tmp_catalog[keep_indices, ]
    }
    
    # Further filter by geographic level
    if (!is.null(input$filter_geo_lvls)) {
      selected_levels <- input$filter_geo_lvls
      
      keep_indices <- c()
      for (i in 1:nrow(tmp_catalog)) {
        if (!is.na(tmp_catalog[i, "Levels"])) {
          i_lvls <- trimws(strsplit(tmp_catalog[i, "Levels"], ";")[[1]])
          
          if (any(unlist(lapply(selected_levels, function(x) {x %in% i_lvls})))) {
            keep_indices <- c(keep_indices, i)
          }
        }
      }
      tmp_catalog <- tmp_catalog[keep_indices, ]
    }
    
    # Sort the selected resources according to user input
    tmp_catalog <- sort_by_criteria(tmp_catalog, input$sort_by)

    # Update reactive
    selected_rscs(tmp_catalog)
    
    # Reset to first page when filters change
    current_page(1)
  })
  
  # Button for clearing all sidebar inputs
  observeEvent(input$clear, {
    # Reset all of the tags checkboxes
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
    
    # Reset the years slider
    updateSliderInput(
      session,
      inputId = "filter_year",
      value = c(min_year, max_year)
    )
    
    # Reset the geographic levels 
    updateCheckboxGroupInput(
      session, 
      inputId = "filter_geo_lvls", 
      selected = FALSE
    )
    
    # Reset the search bar
    updateTextInput(
      session,
      inputId = "search",
      value = ""
    )

    # Set selected_rscs back to the full catalog
    selected_rscs(sort_by_criteria(full_catalog, input$sort_by))
    
    # Reset to first page
    current_page(1)
  })
  
  # Main output showing resource cards
  output$sources_output <- renderUI({
    tmp_catalog <<- selected_rscs()
    
    if (nrow(tmp_catalog) > 0) {
      # If there are resources to show...
      no_matches(FALSE)
      
      # Get resources to be shown on current page
      if (input$show_per_page != "All") {
        # Need to recompute how many pages there should be every time output changes
        total_pages(get_num_pages(selected_rscs(), strtoi(input$show_per_page)))
        
        # Figure out which resources to display given the current page the user is on
        start_i <- (strtoi(input$show_per_page) * (current_page() - 1) + 1)
        end_i <- min((strtoi(input$show_per_page) * current_page()), nrow(tmp_catalog))
        indices <- start_i:end_i
        page_rscs(tmp_catalog[indices, ])
        
      } else {
        # Show all resources on one page
        total_pages(1)
        page_rscs(tmp_catalog)
      }
      
      # Define all the collapse panels for each of the filtered resources
      collapseArgs <- lapply(1:nrow(page_rscs()), function(i) {
        # Get name of resource
        res_name <- page_rscs()[i, "Name"]
        # Get type of resource
        res_type <- strsplit(page_rscs()[i, "Tags"], ";")[[1]][1]

        if (res_type == "Data Methodology") {
          # Get list of data sources used in methodology
          used_data <- methods_data[methods_data$'Methodology Name' == res_name, ]
          used_data <- used_data[order(used_data$'Dataset Name'), ]
          
          # Generate collapse specifically for methodology
          bsCollapsePanel(
            title = res_name,
            value = paste0("rsc_", i),
            # Show resource info
            HTML(gen_rsc_info(page_rscs()[i,])),
            
            # If resource is a methodology, then add extra section for viewing individual contributing data sources
            fluidRow(
              column(
                width = 12,
                actionLink(
                  inputId = paste0("view_methods_data_", i), 
                  label = "View data sources used by this methodology", 
                  icon("caret-right")
                ),
                
                # Show/hide methodology's data sources
                conditionalPanel(
                  condition = paste0("input.view_methods_data_", i, " % 2 == 1"),
                  
                  # Generate a row with a "Take me here!" button for each data source
                  lapply(1:nrow(used_data), function(j) {
                    if (used_data[j, "Status"] == "Available") {
                      fluidRow(
                        class = "method_data_row",
                        column(
                          width = 3,
                          HTML(paste0("<p class='method_data_info'><b>", used_data[j, "Dataset Name"], "</b></p>"))
                        ),
                        column(
                          width = 2,
                          HTML(paste0("<p class='method_data_info'>", used_data[j, "Type"], "</p>"))
                        ),
                        column(
                          width = 3,
                          HTML(paste0("<p class='method_data_info'>Years used: ", used_data[j, "Years Utilized"], "</p>"))
                        ),
                        column(
                          width = 4,
                          actionButton(
                            inputId = paste0("go_to_methods_data_", i, j),
                            label = "Take me here!",
                            class = "btn-secondary"
                          )
                        )
                      )
                    } else {
                      fluidRow(
                        class = "method_data_row",
                        column(
                          width = 3,
                          HTML(paste0("<p class='method_data_info'><b>", used_data[j, "Dataset Name"],"</b></p>"))
                        ),
                        column(
                          width = 2,
                          HTML(paste0("<p class='method_data_info'>", used_data[j, "Type"], "</p>"))
                        ),
                        column(
                          width = 3,
                          HTML(paste0("<p class='method_data_info'>Years used: ", used_data[j, "Years Utilized"], "</p>"))
                        ),
                        column(
                          width = 4,
                          HTML("<p class='method_data_info'><font color='red'><i>Not available</i></font></p>")
                        )
                      )
                    }
                  })
                )
              )
            ),
            
            fluidRow(
              # Toggle for showing recommended resources
              column(
                width = 6,
                align = 'left',
                class = "rec_toggle",
                materialSwitch(
                  inputId = paste0('show_more_rscs_', i),
                  label = NULL,
                  value = FALSE,
                  status = "primary",
                  inline = TRUE,
                ),
                "Show me similar resources"
              ),
              
              # Save button 
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
            
            # Recommended resource cards
            conditionalPanel(
              condition = paste0("input.show_more_rscs_", i),
              br(),
              uiOutput(outputId = paste0("rsc_recs_", i)),
              br()
            )
          )
        } else {
          bsCollapsePanel(
            title = res_name,
            value = paste0("rsc_", i),
            # Show resource info
            HTML(gen_rsc_info(page_rscs()[i,])),
            
            fluidRow(
              # Toggle for showing recommended resources
              column(
                width = 6,
                align = 'left',
                class = "rec_toggle",
                materialSwitch(
                  inputId = paste0('show_more_rscs_', i),
                  label = NULL,
                  value = FALSE,
                  status = "primary",
                  inline = TRUE,
                ),
                "Show me similar resources"
              ),
              
              # Save button 
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
            
            # Recommended resource cards
            conditionalPanel(
              condition = paste0("input.show_more_rscs_", i),
              br(),
              uiOutput(outputId = paste0("rsc_recs_", i)),
              br()
            )
          )
        }
      })
      
      # Additional bsCollapse arguments
      collapseArgs[["multiple"]] <- TRUE
      collapseArgs[["open"]] <- unlist(lapply(1:nrow(page_rscs()), function(i) {paste0("rsc_", i)}))
      
      # Make all the collapse panels
      do.call(bsCollapse, collapseArgs)
      
    } else {
      # No resources to show
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
  
  # Previous page button
  observeEvent(input$prev_page, {
    page <- max(1, current_page()-1)
    current_page(page)
  })
  
  # Next page button
  observeEvent(input$next_page, {
    page <- min(current_page()+1, total_pages())
    current_page(page)
  })
  
  # Functions for each of the resource-specific buttons
  # Need to apply function up to nrow(full_catalog) because can't access length of page_rscs() outside of render/observe
  lapply(1:nrow(full_catalog), function(i) {
    # Make resource recommendation boxes
    output[[paste0("rsc_recs_", i)]] <- renderUI({
      # Get most similar resources (i.e. all with similarity greater than pre-set threshold)
      rsc_name <- page_rscs()[i, "Name"]
      recs <- all_rsc_recs[[rsc_name]]
      
      if (is.null(recs)) {
        # Let user know if there are no sources to recommend
        HTML("<i>No recommended resources to show.</i>")
        
      } else {
        # Create a list containing all of the recommended resource boxes (divs)
        recs_list <- lapply(1:length(recs), function(j) {
          rec_name <- names(recs[j])
          # Get lists of shared tags and methodologies for display
          tags_ij <- shared_tags[rsc_name, rec_name][[1]]
          methods_ij <- shared_methods[rsc_name, rec_name][[1]]
          
          # Show 2 lines of the description -- cut off with ... if description exceeds max number of lines (in CSS file)
          desc <- full_catalog[full_catalog["Name"] == rec_name, ][["Description"]]
          disp_str <- paste0("<b>", rec_name, "</b><p class='rec_desc'>", desc, "</p>")
          
          # Display how many tags and methodologies the resource has in common with the rec
          if (length(tags_ij) > 0) {
            if (length(tags_ij) == 1) {
              disp_str <- paste0(disp_str, "<p>", length(tags_ij), " shared tag: ", paste(tags_ij, collapse=", "), "</p>")
            } else {
              disp_str <- paste0(disp_str, "<p>", length(tags_ij), " shared tags: ", paste(tags_ij, collapse=", "), "</p>")
            }
          }
          
          if (length(methods_ij) > 0) {
            if (length(methods_ij) == 1) {
              disp_str <- paste0(disp_str, "<p>", length(methods_ij), " shared methodology: ", paste(methods_ij, collapse=", "), "</p>")
            } else {
              disp_str <- paste0(disp_str, "<p>", length(methods_ij), " shared methodologies: ", paste(methods_ij, collapse=", "), "</p>")
            }
          }
          
          div(
            # Recommendation box content
            class = "rec_box",
            fluidRow(
              class = "rec_desc_div",
              column(
                width = 12,
                HTML(disp_str)
              )
            ),
            
            # "Show rec" and "Save" buttons
            fluidRow(
              class = "rec_btns",
              column(
                width = 12,
                align = "right",
                actionButton(inputId = paste0("go_to_rec_", i, j), label = "Take me here!", class = "btn-secondary"),
                actionButton(inputId = paste0("save_rec_", i, j), label = icon("heart"), class = "btn-primary")
              )
            )
          )
        })
        
        # Create a horizontal scrolling div with all recommended resources
        fluidRow(
          column(
            width = 12,
            div(
              class = "rec_scroll",
              recs_list
            )
          )
        )
      }
    })
    
    # Buttons for each of the resource recs 
    lapply(1:max_num_recs, function(j) {
      # See full resource info for rec
      observeEvent(input[[paste0("go_to_rec_", i, j)]], {
        # rec_card <- tibble::tribble(
        #   ~element, ~intro, ~tooltipClass,
        #   NA, "<p>Clicking <b>[Ready to Model]</b> reveals <b>Model Selections</b> in which you can toggle which factor categories include in your model and use the drop down menus to pick specific factors.</p><img width=\"980px\" src=\"media/tutorial1.gif\">", "tooltip-width-large",
        #   NA, "Hooray! You finished the tour! Have fun exploring and building models!", "tooltip-width",
        # )
        # 
        # introjs(
        #   session,
        #   options = list(disableInteraction = TRUE,
        #                  steps = rec_card)
        # )
        
        # Get name of rec to open
        rsc_name <- page_rscs()[i, "Name"]
        recs <- all_rsc_recs[[rsc_name]]
        rec_name <- names(recs[j])
        
        # Update reactive
        rec_open(rec_name)
        
        # Display solo card for the rec in a modal
        showModal(
          modalDialog(
            class = "rec_modal",
            
            bsCollapse(
              open = "rec_card",
              bsCollapsePanel(
                title = rec_name,
                value = "rec_card",
                # Show resource info
                HTML(gen_rsc_info(full_catalog[full_catalog["Name"] == rec_name, ])),
                
                fluidRow(
                  column(
                    width = 12,
                    align = "right",
                    # Save button
                    actionButton(
                      inputId = paste0("add_to_cart_rec"), 
                      label = "Save",
                      icon = icon("heart"),
                      class = "btn-primary"
                    ),
                    # Close button
                    actionButton(
                      inputId = paste0("close_rec"), 
                      label = "Close",
                      class = "btn-secondary"
                    )
                  )
                )
              )
            ),
            
            footer = NULL,
            easyClose = TRUE,
            size = 'l',
            fade = FALSE
          )
        )
        
      })
      
      # Save the rec
      observeEvent(input[[paste0("save_rec_", i, j)]], {
        save_clicked(TRUE)
        rsc_name <- page_rscs()[i, "Name"]
        recs <- all_rsc_recs[[rsc_name]]
        
        tmp <<- shopping_list()
        rec_name <- names(recs[j])
        tmp[[rec_name]] <<- full_catalog[full_catalog["Name"] == rec_name, ]
        shopping_list(tmp)
      })
    })
    
    # View methodology data sources
    observeEvent(input[[paste0("view_methods_data_", i)]], {
      if (input[[paste0("view_methods_data_", i)]] %% 2 == 1) {
        updateActionLink(
          session = session,
          inputId = paste0("view_methods_data_", i),
          icon = icon("caret-down")
        )
      } else {
        updateActionLink(
          session = session,
          inputId = paste0("view_methods_data_", i),
          icon = icon("caret-right")
        )
      }
    })
    
    # "Save" buttons
    observeEvent(input[[paste0("add_to_cart_rsc_", i)]], {
      save_clicked(TRUE)
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
  
  # Save button for the solo rec cards
  observeEvent(input$add_to_cart_rec, {
    save_clicked(TRUE)
    tmp <<- shopping_list()
    tmp[[rec_open()]] <<- full_catalog[full_catalog["Name"] == rec_open(), ]
    shopping_list(tmp)
  })
  
  # Close button for the solo rec cards
  observeEvent(input$close_rec, {
    removeModal()
    rec_open("")
  })
  
  # Sort the catalog according to some criterion
  observeEvent(input$sort_by, {
    tmp <- selected_rscs()
    
    # Sort the resources output according to user input
    tmp <- sort_by_criteria(tmp, input$sort_by)
    
    # Update reactive
    selected_rscs(tmp)
    
    # Reset to first page
    current_page(1)
  })
  
  # Change the number of resources to show on each page
  observeEvent(input$show_per_page, {
    if (input$show_per_page != "All") {
      # Compute how many pages there should be based on user input
      total_pages(get_num_pages(selected_rscs(), strtoi(input$show_per_page)))
    } else {
      # Show all resources on single page
      total_pages(1)
    }
    # Go back to first page if changing the number of resources per page
    current_page(1)
  })
  
  # Show all saved resources in shopping cart
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
          
          bkgd_color <- "#f0f7ff"  #light blue
          bord_color <- "#005B94"  #MITRE blue
          # bord_color <- "#bfddff"
          
          div(
            # Summary resource info (name, type, link)
            fluidRow(
              class = "saved_res",
              column(
                width = 4,
                HTML(paste0("<p class='saved_res_info'>", rsc_name, "</p>"))
              ),
              
              column(
                width = 2,
                HTML(paste0("<p class='saved_res_info'>", rsc_type, "</p>"))
              ),
              
              column(
                width = 4,
                HTML(paste0("<p class='saved_res_info'><a href=", rsc_url, ">", rsc_url, "</a></p>"))
              ),
              
              # Expand and individual trash buttons
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
                  class = "saved_res_detail",
                  HTML(gen_rsc_info(rsc_info))
                )
                
              )
            )
          )
        })
      )
    } else {
      # Display message for when there are no resources in the cart
      fluidRow(
        column(
          width = 12, 
          align = "center",
          HTML("<br><i>Your currently have no saved resources.</i>")
        )
      )
    }
    
  })
  
  # Remove all items from the cart
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
          easyClose = FALSE,
          fade = FALSE
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
      
      # Modal to confirm that CSV has been saved
      showModal(
        modalDialog(
          HTML("Successfully downloaded a CSV with all saved resources!"),
          footer = NULL,
          easyClose = TRUE,
          fade = FALSE
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
      
      # Modal to confirm that PDF has been saved
      showModal(
        modalDialog(
          HTML("Successfully downloaded a report with all saved resources!"),
          footer = NULL,
          easyClose = TRUE,
          fade = FALSE
        )
      )
    }
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
