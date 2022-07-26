# SJP Data Catalog App
# By Kirbi Joe, Data Sources and Analysis Team


# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(readxl)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(htmltools)
library(MITREShiny)
library(rmarkdown)
library(bsplus)
library(rintrojs)
library(lsa)
library(plotly)
# install.packages("remotes")
# remotes::install_github("fbreitwieser/sankeyD3")
library(sankeyD3)


# Data and Utility definitions ----

## Suporting functions ----
source("utility.R")

## Catalog data ----

# Use local data or pull from SharePoint?
local <- FALSE

# Data folder
if (local) {
  data_folder <- file.path("data")
} else {
  data_folder <- file.path(
    gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
    "Social Justice Platform WSs - Data Catalog")
}


# Read in data from each tab
path_to_catalog <- file.path(data_folder, "sjp_data_catalog.xlsx")
sheet_names <- excel_sheets(path_to_catalog)
ignore_sheets <- c("guidelines", "Copewell Indicators", "to review", "mitch_milestone_goals") #sheets to ignore
catalog <- lapply(sheet_names, function(X) {
    if (!(X %in% ignore_sheets)) {
      read_excel(path_to_catalog, sheet = X) 
    }
  })
names(catalog) <- sheet_names

# Tags info
list_of_tags <- catalog$'list of tags'
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
full_catalog <- full_catalog %>% relocate("Computed_Measure(s)", .after = "Source") #moving methodology column up so it appears higher in info cards

# Get the data sources contained in the methodologies
methods_data <- catalog$`methodology--data`
# Get max number of data sources used by a methodology for generating buttons in the server
max_methods_data <- max(table(methods_data$`Methodology Name`))

# Get min and max years across catalog
years_avail <- full_catalog[order(full_catalog$Years_Available, na.last=TRUE, decreasing=FALSE), "Years_Available"]
years_avail <- years_avail[!(is.na(years_avail)) & years_avail != "N/A" & !(grepl("Varies", years_avail))]  #get values that aren't missing or actual text input (e.g. "N/A", "Varies")
min_year <- strtoi(substr(years_avail[1], 1, 4))  #pull first 4 characters (YYYY) of first element in years_avail
end_yrs <- unlist(lapply(years_avail, function(y) {
  strtoi(substr(tail(strsplit(tail(trimws(strsplit(y, ",")[[1]]), n=1), "-")[[1]], n=1), 1, 4))
}))  #get all of the ending years of each entry
max_year <- max(end_yrs)  #take the max of all ending years to get overall max year

# Get all available geographic levels
geo_levels <- c("National", "State", "County", "City", "Zip Code", "ZCTA", "Census Tract", "Census Block")

## Computed values ----

# Generate features vectors and compute all similarity values
catalog_features <- feature_vectors(full_catalog, list_of_tags$Tags, catalog$`methodology--data`)
all_sim_values <- get_all_sims(full_catalog, catalog_features, sim_measure="cosine", total_tags=length(list_of_tags$Tags), total_methods=nrow(full_catalog[grepl("Methodology", full_catalog$Tags), ]))

# Similarity matrix between all resources
rsc_sim <- all_sim_values$sim_matrix
all_sims <- rsc_sim[lower.tri(rsc_sim)]
# sim_thresh <- quantile(all_sims)[["75%"]]
# sim_thresh <- mean(all_sims) + 1.64 * sd(all_sims)
sim_thresh <- quantile(all_sims, 0.95)

# Number of shared tags and methodologies
shared_tags <- all_sim_values$tags
shared_methods <- all_sim_values$methods

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
# Get max number of recs across all resources for generating buttons in the server
max_num_recs <- max(num_recs) 

# Get feature distributions for whole catalog -- need this for About tab
type_counts_full <- get_type_dist(full_catalog)
year_counts_full <- get_year_dist(full_catalog)
tags_counts_full <- get_tags_dist(full_catalog)



  


# UI ----
ui <- MITREnavbarPage(
  title = "Social Justice Platform Data Catalog",
  id = "navbar_tabs",
  header = includeCSS("www/style.css"),

  ## Search Catalog tab ----
  tabPanel(
    title = "Search Catalog",
    value = "search_tab",
    
    # To activate the disabling/enabling of buttons
    shinyjs::useShinyjs(),
    
    # Get name of event triggered
    tags$head(
      tags$script(
        "$(document).on('shiny:inputchanged', function(event) {
          if (event.name != 'changed') {
            Shiny.setInputValue('changed', event.name);
          }
        });"
      )
    ),
    
    # To activate the use of popovers in your page
    use_bs_popover(),
    
    # To activate the rintrojs tutorial package
    introjsUI(),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        ### Search Catalog: Search by keywords ----
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
        
        ### Search Catalog: Filter by tags ----
        h6("Filter by Tags"),
        uiOutput(outputId = "search_filter"),
        
        hr(),
        
        ### Search Catalog: Filter by year ----
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
        
        ### Search Catalog: Filter by geographic level ----
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
        
        ### Search Catalog: Filter buttons ----
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
        
        ### Search Catalog: Viewing options ----
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
        
        ### Search Catalog: Main output showing resource cards ----
        uiOutput(outputId = "sources_output"),
        
        # Forward and back page buttons -- only visible if there are resources to show
        conditionalPanel(
          condition = "!output.no_matches",
          br(),
          fluidRow(
            column(
              width = 12,
              align = "center",

              uiOutput(outputId = "page_number")
            )
          )
        )
      )
    )
  ),
  
  ## Saved Resources tab ----
  tabPanel(
    title = uiOutput("saved_res_title"),
    value = "saved_res_tab",
    
    fluidRow(
      # Left-side padding
      column(width = 2),
      
      column(
        width = 8,
        
        ### Saved Resources: Header and Buttons ----
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
                downloadLink(
                  outputId = "export_to_csv",
                  label = "To CSV"
                ),
                downloadLink(
                  outputId = "export_to_html",
                  label = "To HTML"
                )
              )
            )
          )
        ),
        
        hr(),
        
        ### Saved Resource: Cart contents ----
        uiOutput(outputId = "shopping_cart")
      ),
      
      # Right-side padding
      column(width = 2)
    )
  ),
  
  ## Insights tab ----
  tabPanel(
    title = "Insights",
    value = "insights_tab",
    
    fluidRow(
      # Left-side padding
      column(width = 2),
      
      column(
        width = 8,
        
        h1("Insights"),
        
        br(),
        
        includeHTML("www/insights.html"),
        
        br(),
        
        radioGroupButtons(
          inputId = "insights_view",
          choices = c("View Catalog", "View Saved Resources"),
          selected = "View Catalog"
        ),
        
        uiOutput(outputId = "insights_main"),
        
      ),
      
      # Right-side padding
      column(width = 2)
    )
  ),
  
  ## About tab ----
  tabPanel(
    title = "About",
    value = "about_tab",
    
    fluidRow(
      # Left-side padding
      column(width = 2),
      
      column(
        width = 8,
        
        # Had to split up About page into two HTML files because the Catalog Contents section needs to be dynamic
        # relative to the actual contents in the catalog. Needed to pull the subsection out of the HTML file so that
        # it could access the necessary dynamic values.
        
        includeHTML("www/about_overview_intro.html"),
        
        h2("Catalog Contents"),
          
        HTML(paste0("<p>The SJP Data Catalog contains a total of ", nrow(full_catalog)," resources--", type_counts_full["Dataset"], 
        " datatsets, ", type_counts_full["Data Repository"], " data repositories, ", type_counts_full["Tool"], " interactive tools, ", 
        type_counts_full["Summary Table"], " summary tables, and ", type_counts_full["Data Methodology"], " data methodologies. The 
        resources represented in the catalog are diverse across multiple characteristics with data availability spanning from ", 
        min_year," to ", max_year," and geographic levels as broad as national level and as narrow as zip codes. A total of ", 
        length(tags_counts_full)," distinct tags are used to describe the available 
        resources and cover features ranging from topic/subject to data collection method to stratifications. For a more 
        detailed summary of the catalog and its contents, please visit the <i>Insights</i> tab.</p>")),
        
        includeHTML("www/about_howto.html")
      ),
      
      # Right-side padding
      column(width = 2)
    )
    
  ),
  
  ## Contact Us tab ----
  tabPanel(
    title = "Contact Us",
    value = "contact_tab",
    
    fluidRow(
      column(width = 2),
      
      column(
        width = 8,
        
        includeHTML("www/contact_us.html")
       
      ),
      
      column(width = 2)
    )
  )
  
)




# Server ----
server <- function(input, output, session) {
  ## Define reactives ----

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
  card_open <- reactiveVal("")
  
  # For Insights tab--data and distributions for display 
  insights_full_set <- reactiveVal(full_catalog)
  insights_filtered_set <- reactiveVal(full_catalog)
  type_counts <- reactiveVal(get_type_dist(full_catalog))
  year_counts <- reactiveVal(get_year_dist(full_catalog))
  tags_counts <- reactiveVal(get_tags_dist(full_catalog))
  tags_connect_data <- reactiveVal(get_sankey_data(full_catalog, list_of_tags))
  
  # For Insights tab--data for tags used in the selected set
  used_tags_tmp <- unique(unlist(lapply(full_catalog$Tags, function(x) strsplit(x, "; ")))) #making tmp variable to access in both reactiveVals below
  used_tags <- reactiveVal(used_tags_tmp)
  list_of_used_tags <- reactiveVal(list_of_tags %>% filter(Tags %in% used_tags_tmp))
  
  
  ## Welcome Modal ----

  welcome_modal <- modalDialog(
    title = HTML("<center><b>Welcome to the Social Justice Platform Data Catalog!</center></b>"),
    
    HTML(paste0("<p>The Social Justice Platform (SJP) Data Catalog is an open source collection of data sets, resources, and tools with a specific 
    emphasis on social justice. The goal of this catalog is to provide a focused repository of social justice and equity resources where researchers 
    and analysts can go to explore relevant data for their projects. </p>
    
    <p>The dashboard allows users to interactively search through the catalog, explore recommended resources, save interesting or relevant resources, 
    and view conceptual analyses of the various catalog metadata. </p>
    
    <p>The catalog itself is a collection of ", nrow(full_catalog), " resources, spanning across multiple decades, geographic levels, and topic domains. The wordcloud 
    below provides a visualization of some of the types of features that are represented and most prominent within the catalog.</p>
    
    <p>Click the anywhere on the screen to get started!</p>
    <center><img src='wordcloud.png', height='350px'></center>")),
    
    footer = div(),
    easyClose = TRUE,
    size = "l"
  )
  
  showModal(welcome_modal)
  
  observeEvent(input$close_modal, {
    removeModal()
  })
  
  
  ## Checkbox Filters for "Search Catalog" and "Insights" ----
  
  lapply(c("search", "insights"), function(section) {
    ### Generate collapsible menus for the filter by tag checkboxes ----
    output[[paste0(section, "_filter")]] <- renderUI({
      # Need to differentiate some values for the different section checkbox groups
      if (section == "search") {
        tags_list <- list_of_tags
        start_pos <- "caret-right"
        start_val <- 1
      } else {
        tags_list <- list_of_used_tags()
        start_pos <- "caret-down"
        start_val <- 0
      }
      
      lapply(unique(tags_list$'Tag Type'), function(t) { 
        tags <- tags_list[tags_list$'Tag Type' == t, ]
        subcats <- sort(unique(tags$Subcategory), na.last = TRUE)
        
        # Display checkbox groups for each subcategory within each tag type
        div(
          # Overarching tag types
          class = 'tag_group_label',
          actionLink(
            inputId = paste0(section, "_label_", gsub(" ", "_", t)), 
            label = t, 
            icon(start_pos)
          ),
          
          # Show/hide the info within each tag type (Insights menus start open; Search start closed)
          conditionalPanel(
            condition = paste0("input.", section, "_label_", gsub(" ", "_", t)," % 2 == ", start_val),
            
            if (all(is.na(subcats))) {
              # If there are no subcategories, just show the checkbox group
              div(
                class = 'tag_chkbox',
                checkboxGroupInput(
                  inputId = paste0(section, "_", gsub(" ", "_", t)),
                  label = NULL,
                  selected = NULL,
                  choiceNames = sort(tags_list$Tags[which(tags_list$'Tag Type' == t)]),
                  choiceValues = sort(tags_list$Tags[which(tags_list$'Tag Type' == t)])
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
                        inputId = paste0(section, "_label_", gsub(" ", "_", t), "_Other"),
                        label = "Other",
                        icon("caret-right")
                      ),
                      conditionalPanel(
                        condition = paste0("input.", section,"_label_", gsub(" ", "_", t), "_Other", " % 2 == 1"),
                        div(
                          class = 'tag_chkbox',
                          checkboxGroupInput(
                            inputId = paste0(section, "_", paste(gsub(" ", "_", t), "Other", sep = "--")),
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
                        inputId = paste0(section, "_label_", gsub(" ", "_", t), "_", gsub(" ", "_", s)),
                        label = s,
                        icon("caret-right")
                      ),
                      conditionalPanel(
                        condition = paste0("input.", section, "_label_", gsub(" ", "_", t), "_", gsub(" ", "_", s), " % 2 == 1"),
                        div(
                          class = 'tag_chkbox',
                          checkboxGroupInput(
                            inputId = paste0(section, "_", paste(gsub(" ", "_", t), gsub(" ", "_", s), sep = "--")),
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
      })
    })
    
    ### Toggle the carets on the checkbox menu labels ----
    lapply(unique(list_of_tags$'Tag Type'), function(t) {
      if (section == "search") {
        start_pos <- "caret-right"
        new_pos <- "caret-down"
      } else {
        start_pos <- "caret-down"
        new_pos <- "caret-right"
      }
      
      # Labels for tag types
      observeEvent(input[[paste0(section, "_label_", gsub(" ", "_", t))]], {
        if (input[[paste0(section, "_label_", gsub(" ", "_", t))]] %% 2 == 1) {
          updateActionLink(
            session = session,
            inputId = paste0(section, "_label_", gsub(" ", "_", t)),
            icon = icon(new_pos)
          )
        } else {
          updateActionLink(
            session = session,
            inputId = paste0(section, "_label_", gsub(" ", "_", t)),
            icon = icon(start_pos)
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
          observeEvent(input[[paste0(section, "_label_", gsub(" ", "_", t), "_", gsub(" ", "_", s))]], {
            if (input[[paste0(section, "_label_", gsub(" ", "_", t), "_", gsub(" ", "_", s))]] %% 2 == 1) {
              updateActionLink(
                session = session,
                inputId = paste0(section, "_label_", gsub(" ", "_", t), "_", gsub(" ", "_", s)),
                icon = icon("caret-down")
              )
            } else {
              updateActionLink(
                session = session,
                inputId = paste0(section, "_label_", gsub(" ", "_", t), "_", gsub(" ", "_", s)),
                icon = icon("caret-right")
              )
            }
          })
        })
      } 
    })
  })
  
  ## Search Catalog: Button for filtering according to selected checkboxes ----
  observeEvent(input$filter, {
    # Update selected tags on button press
    selected_tags <-
      lapply(tag_types, function(t) {
        # Want to group tags from the subcategories together to pass to selected_tags
        subcats <- unique(list_of_tags[list_of_tags$'Tag Type' == t, ]$Subcategory)
        select_tags_type <- c()
        if (all(is.na(subcats))) {
          # If there are no subcategories, checkbox group ID is just the 
          select_tags_type <- c(select_tags_type, input[[paste0("search_", gsub(" ", "_", t))]])
        } else {
          for (s in subcats) {
            if (is.na(s)) {
              s <- "Other"
            }
            
            select_tags_type <- c(select_tags_type, input[[paste0("search_", paste(gsub(" ", "_", t), gsub(" ", "_", s), sep = "--"))]])
          }
        }
        
        return(select_tags_type)
      })
    
    
    # Update names of the list to be each of the tag types
    selected_tags <- setNames(selected_tags, tag_types)
    
    # Get sources with the selected tags
    tmp_catalog <- filter_by_tags(full_catalog, selected_tags)
    
    # Further filter by year
    if (input$filter_year[1] != min_year || input$filter_year[2] != max_year || !input$filter_year_na) {
      selected_years <- seq.int(input$filter_year[1], input$filter_year[2])
      
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
    if (!is.null(input$filter_geo_lvls) & nrow(tmp_catalog) > 0) {
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
  
  # Search Catalog: Search using keywords
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
  
  # Search Catalog: Clear sidebar inputs
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
    
    # Reset the years slider and checkbox
    updateSliderInput(
      session,
      inputId = "filter_year",
      value = c(min_year, max_year)
    )
    
    updateCheckboxInput(
      session,
      inputId = "filter_year_na",
      value = TRUE
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
  
  ## Search Catalog: Main output showing resource cards ----
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
                            inputId = paste0("go_to_methods_data_", i, "_", j),
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
                          HTML(paste0("<p class='method_data_info'><font color='red'><i>", used_data[j, "Status"], "</i></font></p>"))
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
          # Generate collapse for a non-methodology resource
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
  
  ## Search Catalog: Viewing options ----

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
  
  # Display what page user is currently viewing
  output$page_number <- renderUI({
    fluidRow(
      column(
        width = 12,
        align = "center",
        
        actionButton(
          inputId = "prev_page",
          label = icon("angle-left"),
          class = "btn-secondary"
        ),
        HTML(paste("Page", current_page(), "of", total_pages())),
        actionButton(
          inputId = "next_page",
          label = icon("angle-right"),
          class = "btn-secondary"
        )
      )
    )
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

  ## Search Catalog: Functions for each of the resource-specific buttons and features ----
  
  ### "Save" buttons ----
  observeEvent({unlist(lapply(1:nrow(full_catalog), function(i) {input[[paste0("add_to_cart_rsc_", i)]]}))}, {
    req(input$changed)
    
    if (grepl("add_to_cart_rsc", input$changed)) {
      # Get corresponding indices
      rsc_i <- strtoi(tail(strsplit(input$changed, "_")[[1]], n=1))
      
      # Add resource to shopping list if not already in list
      rsc_name <- page_rscs()[rsc_i, "Name"]
      if (!(rsc_name %in% names(shopping_list()))) {
        tmp <<- shopping_list()
        
        tmp[[rsc_name]] <<- page_rscs()[rsc_i, ]
        shopping_list(tmp)
      }
    }
    
  }, ignoreInit = TRUE)
  
  ### "Remove from Cart" buttons (in Saved Resource tab) ----
  observeEvent({unlist(lapply(1:nrow(full_catalog), function(i) {input[[paste0("rmv_cart_rsc_", i)]]}))}, {
    req(input$changed)

    if (grepl("rmv_cart_rsc", input$changed) & input[[input$changed]] > 0) {
      # Get corresponding indices
      rsc_i <- strtoi(tail(strsplit(input$changed, "_")[[1]], n=1))

      # Remove resource from shopping list
      tmp <- shopping_list()
      tmp <- tmp[-rsc_i]
      shopping_list(tmp)
    }
  }, ignoreInit = TRUE)
  
  ### View methodology data sources ----
  observeEvent({unlist(lapply(1:nrow(full_catalog), function(i) {input[[paste0("view_methods_data_", i)]]}))}, {
    req(input$changed)
    
    if (grepl("view_methods_data", input$changed)) {
      # Get corresponding index
      rsc_i <- strtoi(tail(strsplit(input$changed, "_")[[1]], n=1))
      
      # Change caret direction depending on click
      if (input[[paste0("view_methods_data_", rsc_i)]] %% 2 == 1) {
        updateActionLink(
          session = session,
          inputId = paste0("view_methods_data_", rsc_i),
          icon = icon("caret-down")
        )
      } else {
        updateActionLink(
          session = session,
          inputId = paste0("view_methods_data_", rsc_i),
          icon = icon("caret-right")
        )
      }
    }
  })
  
  ### Open solo card for a methodology data source in a modal ----
  observeEvent(unlist(lapply(1:nrow(full_catalog), function(i) {lapply(1:max_methods_data, function(j) {input[[paste0("go_to_methods_data_", i, "_", j)]]})})), {
    req(input$changed)
    
    if (grepl("go_to_methods_data", input$changed)) {
      # Get corresponding indices
      rsc_i <- strtoi(tail(strsplit(input$changed, "_")[[1]], n=2)[1])
      data_j <- strtoi(tail(strsplit(input$changed, "_")[[1]], n=2)[2])
      
      # Get name of data source to open
      rsc_name <- page_rscs()[rsc_i, "Name"]
      used_data <- methods_data[methods_data$'Methodology Name' == rsc_name, ]
      used_data <- used_data[order(used_data$'Dataset Name'), ]
      methods_data_name <- used_data[[data_j, "Dataset Name"]]
      
      # Update reactive
      card_open(methods_data_name)
      
      # Display solo card for the rec in a modal
      showModal(
        modalDialog(
          class = "card_modal",
          
          bsCollapse(
            open = "methods_data_card",
            bsCollapsePanel(
              title = methods_data_name,
              value = "methods_data_card",
              # Show resource info
              HTML(gen_rsc_info(full_catalog[full_catalog["Name"] == methods_data_name, ])),
              
              fluidRow(
                column(
                  width = 12,
                  align = "right",
                  # Save button
                  actionButton(
                    inputId = paste0("add_to_cart_solo_card"),
                    label = "Save",
                    icon = icon("heart"),
                    class = "btn-primary"
                  ),
                  # Close button
                  actionButton(
                    inputId = paste0("close_solo_card"),
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
    }
  })
  
  ### Reveal full rec description ----
  observeEvent(unlist(lapply(1:nrow(full_catalog), function(i) {lapply(1:max_num_recs, function(j) {input[[paste0("rec_desc_show_more_", i, "_", j)]]})})), {
    req(input$changed)
    
    if (grepl("rec_desc_show_more", input$changed)) {
      # Get corresponding indices
      rsc_i <- strtoi(tail(strsplit(input$changed, "_")[[1]], n=2)[1])
      rec_j <- strtoi(tail(strsplit(input$changed, "_")[[1]], n=2)[2])
      
      # Change caret direction depending on click
      if (input[[paste0("rec_desc_show_more_", rsc_i, "_", rec_j)]] %% 2 == 1) {
        updateActionLink(
          session = session,
          inputId = paste0("rec_desc_show_more_", rsc_i, "_", rec_j),
          label = "Show less",
          icon = icon("caret-up")
        )
      } else {
        updateActionLink(
          session = session,
          inputId = paste0("rec_desc_show_more_", rsc_i, "_", rec_j),
          label = "Show more",
          icon = icon("caret-down")
        )
      }
    }
  })
  
  ### Reveal full lists of shared elements ----
  observeEvent(unlist(lapply(1:nrow(full_catalog), function(i) {lapply(1:max_num_recs, function(j) {input[[paste0("rec_shared_elems_show_more_", i, "_", j)]]})})), {
    req(input$changed)
    
    if (grepl("rec_shared_elems_show_more", input$changed)) {
      # Get corresponding indices
      rsc_i <- strtoi(tail(strsplit(input$changed, "_")[[1]], n=2)[1])
      rec_j <- strtoi(tail(strsplit(input$changed, "_")[[1]], n=2)[2])
    
      # Change caret direction depending on click
      if (input[[paste0("rec_shared_elems_show_more_", rsc_i, "_", rec_j)]] %% 2 == 1) {
        updateActionLink(
          session = session,
          inputId = paste0("rec_shared_elems_show_more_", rsc_i, "_", rec_j),
          label = "Show less",
          icon = icon("caret-up")
        )
      } else {
        updateActionLink(
          session = session,
          inputId = paste0("rec_shared_elems_show_more_", rsc_i, "_", rec_j),
          label = "Show more",
          icon = icon("caret-down")
        )
      }
    }
  })
  
  
  ### See full resource info for rec ----
  observeEvent(unlist(lapply(1:nrow(full_catalog), function(i) {lapply(1:max_num_recs, function(j) {input[[paste0("go_to_rec_", i, "_", j)]]})})), {
    req(input$changed)
    
    if (grepl("go_to_rec", input$changed)) {
      # Get corresponding indices
      rsc_i <- strtoi(tail(strsplit(input$changed, "_")[[1]], n=2)[1])
      rec_j <- strtoi(tail(strsplit(input$changed, "_")[[1]], n=2)[2])
      
      # Get name of rec to open
      rsc_name <- page_rscs()[rsc_i, "Name"]
      recs <- all_rsc_recs[[rsc_name]]
      rec_name <- names(recs[rec_j])
      
      # Update reactive
      card_open(rec_name)
      
      # Display solo card for the rec in a modal
      showModal(
        modalDialog(
          class = "card_modal",
          
          bsCollapse(
            open = "rec_card",
            bsCollapsePanel(
              title = rec_name,
              value = "rec_card",
              # Show resource info
              HTML(gen_rsc_info(full_catalog[full_catalog["Name"] == rec_name, ])),
              
              # Buttons for saving and closing modal
              fluidRow(
                column(
                  width = 12,
                  align = "right",
                  # Save button
                  actionButton(
                    inputId = paste0("add_to_cart_solo_card"),
                    label = "Save",
                    icon = icon("heart"),
                    class = "btn-primary"
                  ),
                  # Close button
                  actionButton(
                    inputId = paste0("close_solo_card"),
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
    }
  })
  
  ### Save the rec ----
  observeEvent(unlist(lapply(1:nrow(full_catalog), function(i) {lapply(1:max_num_recs, function(j) {input[[paste0("save_rec_", i, "_", j)]]})})), {
    req(input$changed)
    
    if (grepl("save_rec", input$changed)) {
      # Get corresponding indices
      rsc_i <- strtoi(tail(strsplit(input$changed, "_")[[1]], n=2)[1])
      rec_j <- strtoi(tail(strsplit(input$changed, "_")[[1]], n=2)[2])
    
      # Get name of main resource and the name of the rec
      rsc_name <- page_rscs()[rsc_i, "Name"]
      recs <- all_rsc_recs[[rsc_name]]
      rec_name <- names(recs[rec_j])
      
      # Add the recommended source to the shopping list
      if (!(rec_name %in% names(shopping_list()))) {
        tmp <<- shopping_list()
        tmp[[rec_name]] <<- full_catalog[full_catalog["Name"] == rec_name, ]
        shopping_list(tmp)
      }
    }
  })

  ### Resource recommendation boxes ----
  withProgress(message = 'Loading resources...', value = 0, {
    lapply(1:nrow(full_catalog), function(i) {

      incProgress(1/nrow(full_catalog))

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
            # Get description and lists of shared tags and methodologies for display
            desc <- full_catalog[full_catalog["Name"] == rec_name, ][["Description"]]
            tags_ij <- shared_tags[rsc_name, rec_name][[1]]
            methods_ij <- shared_methods[rsc_name, rec_name][[1]]

            # Show 2 lines of the description -- cut off with ... if description exceeds max number of lines (in CSS file)
            rec_name_str <- paste0("<b>", rec_name, "</b>")
            desc_str <- paste0("<p class='rec_desc'>", desc, "</p>")

            # Display how many tags and methodologies the resource has in common with the rec
            shared_elems_str <- ""
            if (length(tags_ij) > 0) {
              if (length(tags_ij) == 1) {
                shared_elems_str <- paste0(shared_elems_str, length(tags_ij), " shared tag: ", paste(tags_ij, collapse=", "), "<br>")
              } else {
                shared_elems_str <- paste0(shared_elems_str, length(tags_ij), " shared tags: ", paste(tags_ij, collapse=", "), "<br>")
              }
            }

            if (length(methods_ij) > 0) {
              if (length(methods_ij) == 1) {
                shared_elems_str <- paste0(shared_elems_str, length(methods_ij), " shared methodology: ", paste(methods_ij, collapse=", "), "<br>")
              } else {
                shared_elems_str <- paste0(shared_elems_str, length(methods_ij), " shared methodologies: ", paste(methods_ij, collapse=", "), "<br>")
              }
            }
            # Remove the last <br> tag on the string
            shared_elems_str <- substr(shared_elems_str, 1, nchar(shared_elems_str)-4)

            div(
              # Recommendation box content
              class = "rec_box",
              
              fluidRow(
                class = "rec_desc_div",
                column(
                  width = 12,
                  HTML(rec_name_str),
                  
                  # Shortened rec description with ellipses cutoff
                  conditionalPanel(
                    condition = paste0("input.rec_desc_show_more_", i, "_", j, " % 2 == 0"),
                    HTML(paste0("<p class='rec_desc_part'>", desc, "</p>"))
                  ),
                  # Full rec description
                  conditionalPanel(
                    condition = paste0("input.rec_desc_show_more_", i, "_", j, " % 2 == 1"),
                    HTML(paste0("<p class='rec_desc_full'>", desc, "</p>"))
                  ),
                  # "Show more" option
                  actionLink(
                    inputId = paste0("rec_desc_show_more_", i, "_", j),
                    label = "Show more",
                    icon = icon("caret-down")
                  ),
                  
                  br(),
                  br(),
                  
                  # Shortened rec description with ellipses cutoff
                  conditionalPanel(
                    condition = paste0("input.rec_shared_elems_show_more_", i, "_", j, " % 2 == 0"),
                    HTML(paste0("<p class='rec_desc_part'>", shared_elems_str, "</p>"))
                  ),
                  # Full rec description
                  conditionalPanel(
                    condition = paste0("input.rec_shared_elems_show_more_", i, "_", j, " % 2 == 1"),
                    HTML(paste0("<p class='rec_desc_full'>", shared_elems_str, "</p>"))
                  ),
                  # "Show more" option
                  actionLink(
                    inputId = paste0("rec_shared_elems_show_more_", i, "_", j),
                    label = "Show more",
                    icon = icon("caret-down")
                  ),
                )
              ),
              

              # "Show rec" and "Save" buttons
              fluidRow(
                class = "rec_btns",
                column(
                  width = 12,
                  align = "right",
                  actionButton(inputId = paste0("go_to_rec_", i, "_", j), label = "Take me here!", class = "btn-secondary"),
                  actionButton(inputId = paste0("save_rec_", i, "_", j), label = icon("heart"), class = "btn-primary")
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

    })

  })

  
  ## Search Catalog: Solo pop-up modal buttons ----

  # Save button for the solo cards
  observeEvent(input$add_to_cart_solo_card, {
    card_name <- card_open()
    
    # Add the recommended source to the shopping list
    if (!(card_name %in% names(shopping_list()))) {
      tmp <<- shopping_list()
      tmp[[card_name]] <<- full_catalog[full_catalog["Name"] == card_name, ]
      shopping_list(tmp)
    }
  })
  
  # Close button for the solo cards
  observeEvent(input$close_solo_card, {
    removeModal()
    card_open("")
  })
  
  ## Saved Resources: Tab name display ----

  # Update the "Saved Resources" navbar title whenever someone adds a resource to their cart
  output$saved_res_title <- renderUI({
    if (length(shopping_list()) > 0) {
      paste0("Saved Resources (", length(shopping_list()), ")") 
    } else {
      "Saved Resources"
    }
  })

  ## Saved Resources: Show all saved resources in shopping cart ----
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
  
  ## Saved Resource: Button controls ----
  
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
  
  # Changed mind about clearing cart--do nothing
  observeEvent(input$clear_no, {
    removeModal()
  })
  
  # Button that will execute clear cart once user has confirmed this is actually what they want
  observeEvent(input$clear_yes, {
    removeModal()
    shopping_list(list())
  })
  
  # Exports a dataframe of all saved resources and their associated fields to a csv
  output$export_to_csv <- downloadHandler(
    filename = function() {
      paste0("SavedResources_SJPCatalog_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      shopping_list_df <- as.data.frame(do.call(rbind, shopping_list()))
      write.csv(shopping_list_df, file, row.names = FALSE)
    }
  )
  
  # Exports a PDF report with the list of saved resources as well as some other info
  output$export_to_html <- downloadHandler(
    filename = function() {
      paste0("SavedResourcesReport_SJPCatalog_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".html")
    },
    content = function(file) {
      rmd_report <- file.path("www/export_to_html/savedres_report.Rmd")
      
      shopping_list_df <- as.data.frame(do.call(rbind, shopping_list()))
      
      saved_type_counts <- get_type_dist(shopping_list_df)
      saved_year_counts <- get_year_dist(shopping_list_df)
      saved_tags_counts <- get_tags_dist(shopping_list_df)
      saved_tags_connect <- get_sankey_data(shopping_list_df, list_of_tags)
      
      shopping_list_df <- shopping_list_df[, c("Name", "Source", "Link")]
      
      # Set up parameters to pass to Rmd document
      params <- list(
        saved_res_df = shopping_list_df,
        type_hist = saved_type_counts,
        year_hist = saved_year_counts,
        tags_hist = saved_tags_counts,
        tags_connect = saved_tags_connect
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        rmd_report,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ## Insights: Switch view ----
  
  # User toggles between viewing the "Catalog" or their "Saved Resources"
  observeEvent(input$insights_view, {
    # Determine which set of resources to perform analytics on
    if (input$insights_view == "View Catalog") {
      insights_full_set(full_catalog)
    } else {#(input$insights_view == "View Saved Resources")
      shopping_list_df <<- as.data.frame(do.call(rbind, shopping_list()))
      insights_full_set(shopping_list_df)
    }
    
    # Update list of used tags depending on what set of resources is selected
    used_tags(unique(unlist(lapply(insights_full_set()$Tags, function(x) strsplit(x, "; ")))))
    list_of_used_tags(list_of_tags %>% filter(Tags %in% used_tags()))
    
    # Reset all of the tags checkboxes
    for (t in tag_types) {
      subcats <- unique(list_of_used_tags()[list_of_used_tags()$'Tag Type' == t, ]$Subcategory)
      
      if (all(is.na(subcats))) {
        updateCheckboxGroupInput(
          session, 
          inputId = paste0("insights_", gsub(" ", "_", t)), 
          selected = FALSE
        )
      } else {
        for (s in subcats) {
          if (is.na(s)) {
            s <- "Other"
          }
          
          updateCheckboxGroupInput(
            session, 
            inputId = paste0("insights_", paste(gsub(" ", "_", t), gsub(" ", "_", s), sep = "--")), 
            selected = FALSE
          )
        }
      }
    }

    # Recompute data for graphs
    type_counts(get_type_dist(insights_full_set()))
    year_counts(get_year_dist(insights_full_set()))
    tags_counts(get_tags_dist(insights_full_set()))
    tags_connect_data(get_sankey_data(insights_full_set(), list_of_tags))
  })
  
  observeEvent(shopping_list(), {
    if (length(shopping_list()) > 0) {
      # If there are items in the shopping cart, allow exporting from Saved Resources tab
      shinyjs::enable("export")
      
      # If there are items in the shopping cart, enable both viewing options
      updateRadioGroupButtons(
        session,
        inputId = "insights_view",
        disabledChoices = c()
      )
      
      # If the shopping list changes while in the Saved Resources view, make sure to update the graphics accordingly
      if (input$insights_view == "View Saved Resources"){
        shopping_list_df <- as.data.frame(do.call(rbind, shopping_list()))
        insights_full_set(shopping_list_df)
        
        # Recompute data for graphs
        type_counts(get_type_dist(insights_full_set()))
        year_counts(get_year_dist(insights_full_set()))
        tags_counts(get_tags_dist(insights_full_set()))
        tags_connect_data(get_sankey_data(insights_full_set(), list_of_tags))
      }
      
    } else {
      # If there are no items in the shopping cart, disable exporting from Saved Resources tab
      shinyjs::disable("export")
      
      # If there are no things in the shopping cart, disable that option from the Insights tab and switch back to Catalog view
      updateRadioGroupButtons(
        session,
        inputId = "insights_view",
        selected = "View Catalog",
        disabledChoices = c("View Saved Resources")
      )
    }
  })
  
  ## Insights: Interactive Section ----
  
  ### Filter Insights output according to selected checkboxes ----
  observeEvent(input$insights_go, {
    # Update selected tags on button press
    selected_tags <-
      lapply(unique(list_of_used_tags()$'Tag Type'), function(t) {
        # Want to group tags from the subcategories together to pass to selected_tags
        subcats <- unique(list_of_used_tags()[list_of_used_tags()$'Tag Type' == t, ]$Subcategory)
        select_tags_type <- c()
        if (all(is.na(subcats))) {
          # If there are no subcategories, checkbox group ID is just the
          select_tags_type <- c(select_tags_type, input[[paste0("insights_", gsub(" ", "_", t))]])
        } else {
          for (s in subcats) {
            if (is.na(s)) {
              s <- "Other"
            }

            select_tags_type <- c(select_tags_type, input[[paste0("insights_", paste(gsub(" ", "_", t), gsub(" ", "_", s), sep = "--"))]])
          }
        }

        return(select_tags_type)
      })

    # Update names of the list to be each of the tag types
    selected_tags <- setNames(selected_tags, tag_types)
   
    # Get sources with the selected tags
    insights_filtered_set <- filter_by_tags(insights_full_set(), selected_tags)
    
    # Analytics for Insights tab based on view
    type_counts(get_type_dist(insights_filtered_set))
    year_counts(get_year_dist(insights_filtered_set))
    tags_counts(get_tags_dist(insights_filtered_set))
    tags_connect_data(get_sankey_data(insights_filtered_set, list_of_tags))
  })
  
  ### Clear filters on Insights page ----
  observeEvent(input$insights_clear, {
    # Reset all of the tags checkboxes
    for (t in tag_types) {
      subcats <- unique(list_of_used_tags()[list_of_used_tags()$'Tag Type' == t, ]$Subcategory)
      
      if (all(is.na(subcats))) {
        updateCheckboxGroupInput(
          session, 
          inputId = paste0("insights_", gsub(" ", "_", t)), 
          selected = FALSE
        )
      } else {
        for (s in subcats) {
          if (is.na(s)) {
            s <- "Other"
          }
          
          updateCheckboxGroupInput(
            session, 
            inputId = paste0("insights_", paste(gsub(" ", "_", t), gsub(" ", "_", s), sep = "--")), 
            selected = FALSE
          )
        }
      }
    }
    
    # Recompute data for graphs
    type_counts(get_type_dist(insights_full_set()))
    year_counts(get_year_dist(insights_full_set()))
    tags_counts(get_tags_dist(insights_full_set()))
    tags_connect_data(get_sankey_data(insights_full_set(), list_of_tags))
  })
  
  ### Main Insights section with interactive plots ----
  output$insights_main <- renderUI({
    fluidRow(
      # Controls/options
      column(
        width = 2,
        class = 'insights_options',
        
        uiOutput(outputId = "insights_filter"),
        br(),
        
        fluidRow(
          column(
            width = 12,
            align = 'center',
            
            actionButton(
              inputId = "insights_go",
              label = "Go",
              class = "btn-primary"
            ),
            
            actionButton(
              inputId = "insights_clear",
              label = "Clear",
              class = "btn-secondary"
            )
            
          )
        )
      ),
      
      # Plot output area
      column(
        width = 10,
        
        tabsetPanel(
          type = "tabs",
          id = "insight_plot_tabs",
          
          # Tab for distribution output
          tabPanel(
            title = "Features",
            value = "insights_features_tab",
            
            br(),
            
            plotlyOutput(
              outputId = "insights_features_type_plot",
              height = '400px'
            ),
            
            uiOutput(
              outputId = "year_plot_space"
            )
          ),
          
          # Tab for wordcloud output
          tabPanel(
            title = "Tags",
            value = "insights_tags_tab",
            
            br(),

            plotlyOutput(
              outputId = "insights_tags_plot",
              height = "700px"
            )
          ),
          
          # Tab for sankey output
          tabPanel(
            title = "Domain Connections", 
            value = "insights_connections_tab",
            
            br(),
            
            HTML("<p><i>*Hover over the nodes or individual links to see the connections in more detail. You can also zoom by 
                 scrolling while the mouse is hovered over the figure then move the figure by clicking and dragging.</i></p>"),
            
            uiOutput(
              outputId = "sankey_legend"
            ),
            
            br(),
            
            sankeyNetworkOutput(
              outputId = "insights_connections_sankey",
              height = "800px"
            )
          )
        )
      )
    )
  })
  
  #### Type distribution ----
  output$insights_features_type_plot <- renderPlotly({
    data <- data.frame(type_counts(), stringsAsFactors = FALSE)
    
    # Organize the values by counts from greatest to least
    if (nrow(data) > 1) {
      data$all_types <- factor(data$all_types, levels = unique(data$all_types)[order(data$Freq, decreasing = TRUE)])
      x_vals <- data$all_types
      y_vals <- data$Freq
    } else {
      # No need to sort if there's only one type of count
      x_vals <- names(type_counts())
      y_vals <- as.numeric(type_counts())
    }
    
    fig <- plot_ly(
      x = x_vals,
      y = y_vals,
      type = "bar",
    )
    fig <- fig %>% layout(title = "Resource Types", yaxis = list(title = "Count"))

    return(fig)
  })
  
  #### Year distribution ----
  output$year_plot_space <- renderUI({
    if(length(year_counts()) == 0) {
      HTML("<br><center><p><i>[No available years to show]</i></p></center>")
    } else {
      plotlyOutput(
        outputId = "insights_features_year_plot",
        height = '400px'
      )
    }
  })
  
  output$insights_features_year_plot <- renderPlotly({
    fig <- plot_ly(
      x = names(year_counts()),
      y = as.numeric(year_counts()),
      type = "bar"
    )
    fig <- fig %>% layout(title = "Years Available", yaxis = list(title = "Count"))
    
    return(fig)
  })
  
  #### Tags distribution ----
  output$insights_tags_plot <- renderPlotly({
    data <- data.frame(tags_counts(), stringsAsFactors = FALSE)
    # No need to sort if there's only one type of tag
    if (nrow(data) > 1) {
      # Organize the values by counts from greatest to least
      data$Var1 <- factor(data$Var1, levels = unique(data$Var1)[order(data$Freq, decreasing = FALSE)])
    }
    
    # # initiate a line shape object - for lollipop plot
    # line <- list(
    #   type = "line",
    #   line = list(color = "gray", width=1),
    #   xref = "x",
    #   yref = "y"
    # )
    # 
    # lines <- list()
    # for (i in 1:nrow(data)) {
    #   line[["x0"]] <- 0
    #   line[["x1"]] <- data[i, "Freq"]
    #   line[c("y0", "y1")] <- data[i, "Var1"]
    #   lines <- c(lines, list(line))
    # }
    
    fig <- plot_ly(
      x = data$Freq,
      y = data$Var1,
      type = "bar",
      # mode = "markers"
    )
    fig <- fig %>% layout(
      title = "Tags Distribution",
      yaxis = list(tickfont = list(size = 10)),
      xaxis = list(title = "Count"),
      shapes = lines)
    
    return(fig)
    
  })
  
  #### Sankey diagram of tags ----
  output$sankey_legend <- renderUI({
    nodes <- tags_connect_data()$nodes
    color_map <- unique(nodes[c("group", "color", "x_pos")])
    level_map <- list("0"="Resource Type", "1"="Data Subject", "2"="Data Attribute")
    current_level <- -1
    
    legend <- ""
    
    for (i in 1:nrow(color_map)) {
      label <- color_map[i, "group"]
      color <- color_map[i, "color"]
      level <- color_map[i, "x_pos"]
      
      if (level != current_level) {
        legend <- paste0(legend, "<br><span><b>", level_map[[toString(level)]], ":&nbsp;</b></span>")
        current_level <- current_level + 1
      }
      
      legend <- paste0(legend, '<span style="background-color:', color, ';border:1px solid black;height:11px;width:11px;display:inline-block;"></span><span>&nbsp;', label, '&emsp;<span>')
    }
    
    HTML(legend)
  })
  
  output$insights_connections_sankey <- renderSankeyNetwork({
    sankeyNetwork(
      Links = tags_connect_data()$links,
      Nodes = tags_connect_data()$nodes,
      Source = "IDsource",
      Target = "IDtarget",
      Value = "value",
      NodeID = "name",
      NodeGroup = "group",
      NodeColor = "color",
      NodePosX = "x_pos",
      iterations = 0,
      zoom = TRUE,
      dragX = FALSE,
      dragY = FALSE,
      showNodeValues = FALSE,
      highlightChildLinks = TRUE,
      numberFormat = ".0f",
      linkOpacity = 0.25
    )
  })
}



# Run the application ----
shinyApp(ui = ui, server = server)
