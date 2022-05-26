# SJP Data Catalog App - Utility and Helper Functions
# By Kirbi Joe, Data Sources and Analysis Team


# Get number of total pages given the length of the results to show and the per_page input
# Inputs:
#   - df: dataframe of containing all selected resources (via filters or searching) and their attributes
#   - per_page: number of resources to show per page
# Ouptut:
#   - total_pages: total number of pages of resources
get_num_pages <- function(df, per_page) {
  if (nrow(df) %% per_page == 0) {
    total_pages <- nrow(df) %/% per_page 
  } else {
    total_pages <- nrow(df) %/% per_page + 1  
  }
  return(total_pages)
}

# Sort the catalog according to some criteria 
# Inputs:
#   - df: dataframe of containing all selected resources (via filters or searching) and their attributes
#   - criteria: method by which to sort the selected resources (e.g. "Alphabetical", "Year: Oldest to Newest", "Year: Newest to Oldest")
# Ouptut:
#   - df: dataframe of resources sorted by criteria
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


# Takes a row from the catalog df and generates a string with all the resource info to be displayed UI-side
# Input:
#   - resource: row from catalog dataframe containing all of the resource's attributes
# Ouptut:
#   - info: HTML string containing all of the resource's information
gen_rsc_info <- function(resource) {
  # Change underscores back to spaces in the column names
  names(resource) <- unlist(lapply(names(resource), function(x) {gsub("_", " ", x)})) 
  
  # Columns to ignore
  ignore_col <- c("Name", "Included Measures", "Tools", "Past Projects", "Notes")
  
  # Generate HTML string
  info <- ""
  for (info_cat in names(resource)) {
    if (!is.na(resource[[info_cat]]) & !(info_cat %in% ignore_col) & !(resource[[info_cat]] == "--")) {
      if (grepl("Link", info_cat)) {
        info <- paste0(info, "<p><b>", info_cat, ":</b> <a href=", resource["Link"], ">", resource["Link"], "</a></p>")
      } else {
        info <- paste0(info, "<p><b>", info_cat, ":</b> ", resource[info_cat], "</p>")
      }
    }
  }
  
  return(info)
}

# Generate binary features vectors for each of the resources to pass to the similarity measure--features are tags and methodologies
# Inputs:
#   - catalog: dataframe containing all resources in the catalog and their attributes
#   - tags_list: vector containing all possible tag names
#   - methods_data: dataframe representing the 'methodology--data' tab in the catalog (i.e. structure detailing what data sources are used by each methodology)
# Ouptut:
#   - rsc_feats: binary dataframe where cell value of 1 indicates that a resource (row) has a certain feature (col) and 0 represents otherwise
feature_vectors <- function(catalog, tags_list, methods_data) {
  # List of all methodology names
  method_names <- catalog[grepl("Methodology", catalog$Tags), "Name"]
  
  # Features are the complete list of tags and methodologies
  features <- c(tags_list, method_names)
  
  rsc_feats <- data.frame(matrix(nrow = nrow(catalog), ncol = length(features)))
  colnames(rsc_feats) <- features
  rownames(rsc_feats) <- catalog$Name
  
  for (i in 1:nrow(catalog)) {
    # 1 = rsc contains tag t, 0 = otherwise
    for (t in tags_list) {
      if (grepl(t, catalog[i, "Tags"])) {
        rsc_feats[i, t] = 1
      } else {
        rsc_feats[i, t] = 0
      }
    }
    
    # 1 = rsc used in methodology m, 0 = otherwise
    in_methods <- methods_data[methods_data["Dataset Name"] == catalog[i, "Name"], ][["Methodology Name"]]
    for (m in method_names) {
      if (m %in% in_methods) {
        rsc_feats[i, m] = 1
      } else {
        rsc_feats[i, m] = 0
      }
    }
  }
  
  return(rsc_feats)
}

# Compute Euclidean distance between two vectors
# Inputs:
#   - x, y: two numeric vectors of the same length
# Ouptut:
#   - Euclidean distance between vectors x and y
euclidean <- function(x, y) {
  sqrt(sum((x - y)^2))
}


# Computes all the different similarity-related values in one loop (e.g. similarity measure, number of shared tags, list of shared tags, list of shared methodologies)
# Inputs:
#   - full_catalog: dataframe containing all resources in the catalog and their attributes
#   - catalog_features: binary dataframe indicating what features each resource has
#   - sim_measure: name of similarity measure to use (e.g. "cosine", "euclidean")
#   - total_tags: total number of tags in catalog
#   - total_methods: total number of methods in catalog
#   - count_types: boolean to determine whether to include the data type tags (e.g. dataset, repository, ...) should be counted, default is TRUE
# Output:
#   - out: list containing all of the various similarity measure matrices
get_all_sims <- function(full_catalog, catalog_features, sim_measure, total_tags, total_methods, count_types=TRUE) {
  all_types <- unique(unlist(lapply(strsplit(full_catalog$Tags, '; '), function(x) {x[1]})))
  
  # Similarity measure
  sim <- data.frame(matrix(nrow = nrow(catalog_features), ncol = nrow(catalog_features)))
  # Lists of shared tags
  n_tags_shared <- data.frame(matrix(nrow = nrow(catalog_features), ncol = nrow(catalog_features)))
  # Lists of shared tags
  tags_shared <- data.frame(matrix(nrow = nrow(catalog_features), ncol = nrow(catalog_features)))
  # Lists of shared methodologies
  methods_shared <- data.frame(matrix(nrow = nrow(catalog_features), ncol = nrow(catalog_features)))

  # Set row and col names of each matrix to be resource names
  colnames(sim) <- rownames(sim) <- colnames(n_tags_shared) <- rownames(n_tags_shared) <- colnames(tags_shared) <- rownames(tags_shared) <- colnames(methods_shared) <- rownames(methods_shared) <- rownames(catalog_features)
  
  for (i in 1:(nrow(catalog_features)-1)) {
    # Get features vector for resource i
    i_feat <- as.numeric(as.vector(catalog_features[i, ]))
    for (j in (i+1):nrow(catalog_features)) {
      # Get features vector for resource j
      j_feat <- as.numeric(as.vector(catalog_features[j, ]))
      
      # Compute similarity metric
      if (sim_measure == "cosine") {
        sim_ij <- cosine(i_feat, j_feat)
      } else {#if sim_measure == "euclidean"
        sim_ij <- 1 / (1 + euclidean(i_feat, j_feat))  #sim measure based on Euclidean dist
      }
      
      sim[i, j] <- sim[j, i] <- sim_ij
      
      # Get list of shared tags and assign values to n_tags_shared and tags_shared matrices
      # Tags take up the first n_tags features in the vectors
      tags_overlap <- intersect(colnames(catalog_features)[1:total_tags][as.logical(i_feat[1:total_tags])], colnames(catalog_features)[1:total_tags][as.logical(j_feat[1:total_tags])]) 
      if (!count_types) {
        tags_overlap <- tags_overlap[!(tags_overlap %in% all_types)]
      }
      n_tags_shared[i, j] <- n_tags_shared[j, i] <- length(tags_overlap)
      tags_shared[i, j][[1]] <- tags_shared[j, i][[1]] <- list(tags_overlap)
      
      # Get list of shared methodologies and assign value to methods_shared matrix
      # Methodologies take up the last n_methods features in the vectors
      methods_overlap <- intersect(colnames(catalog_features)[(length(i_feat) - total_methods + 1):length(i_feat)][as.logical(i_feat[(length(i_feat) - total_methods + 1):length(i_feat)])], colnames(catalog_features)[(length(i_feat) - total_methods + 1):length(i_feat)][as.logical(j_feat[(length(i_feat) - total_methods + 1):length(i_feat)])])  
      methods_shared[i, j][[1]] <- methods_shared[j, i][[1]] <- list(methods_overlap)
    }
  }
  
  out <- list()
  out$sim_matrix <- sim
  out$n_tags <- n_tags_shared
  out$tags <- tags_shared
  out$methods <- methods_shared
  
  return(out)
}


# Computes all the different similarity-related values in one loop (e.g. similarity measure, number of shared tags, list of shared tags, list of shared methodologies)
# Inputs:
#   - full_catalog: dataframe containing all resources in the catalog and their attributes
#   - catalog_features: binary dataframe indicating what features each resource has
#   - total_tags: total number of tags in catalog
#   - count_types: boolean to determine whether to include the data type tags (e.g. dataset, repository, ...) should be counted, default is TRUE
# Output:
#   - out: list containing all of the various similarity measure matrices
get_shared_tags <- function(full_catalog, catalog_features, total_tags, count_types=TRUE, thresh=0) {
  all_types <- unique(unlist(lapply(strsplit(full_catalog$Tags, '; '), function(x) {x[1]})))
  
  # Lists of shared tags
  n_tags_shared <- data.frame(matrix(nrow = nrow(catalog_features), ncol = nrow(catalog_features)))
  
  # Set row and col names of each matrix to be resource names
  colnames(n_tags_shared) <- rownames(n_tags_shared) <- rownames(catalog_features)
  
  for (i in 1:(nrow(catalog_features)-1)) {
    # Get features vector for resource i
    i_feat <- as.numeric(as.vector(catalog_features[i, ]))
    for (j in (i+1):nrow(catalog_features)) {
      # Get features vector for resource j
      j_feat <- as.numeric(as.vector(catalog_features[j, ]))
      
      # Get list of shared tags and assign values to n_tags_shared and tags_shared matrices
      # Tags take up the first n_tags features in the vectors
      tags_overlap <- intersect(colnames(catalog_features)[1:total_tags][as.logical(i_feat[1:total_tags])], colnames(catalog_features)[1:total_tags][as.logical(j_feat[1:total_tags])]) 
      if (!count_types) {
        tags_overlap <- tags_overlap[!(tags_overlap %in% all_types)]
      }
      
      if (length(tags_overlap) > thresh) {
        n_tags_shared[i, j] <- n_tags_shared[j, i] <- length(tags_overlap)
      } 
    }
  }
  
  return(n_tags_shared)
}

# Get the count of each resource type found in resource_set 
# Inputs:
#   - resource_set: dataframe of a set resources
# Ouptut:
#   - type_counts: table where names are the resource types and the values are their frequencies in resource_set
get_type_dist <- function(resource_set) {
  all_types <- unlist(lapply(strsplit(resource_set$Tags, '; '), function(x) {x[1]}))
  type_counts <- sort(table(all_types), decreasing = TRUE)
  
  return(type_counts)
}

# Get the count of each year found in resource_set 
# Inputs:
#   - resource_set: dataframe of a set resources
# Ouptut:
#   - year_counts: table where names are the available years and the values are their frequencies in resource_set
get_year_dist <- function(resource_set) {
  all_years <- c()
  for (i in 1:nrow(resource_set)) {
    i_yr_val <- resource_set[i, "Years_Available"]
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
      all_years <- c(all_years, i_yrs)
    } 
  }
  year_counts <- table(all_years)
  
  return(year_counts)
}

# Get the count of each tag found in resource_set 
# Inputs:
#   - resource_set: dataframe of a set resources
# Ouptut:
#   - type_counts: table where names are the tag names and the values are their frequencies in resource_set
get_tags_dist <- function(resource_set) {
  all_types <- unlist(lapply(strsplit(resource_set$Tags, '; '), function(x) {x[1]}))
  type_counts <- sort(table(all_types), decreasing = TRUE)
  
  tag_counts_tmp <- table(unlist(strsplit(resource_set$Tags, '; ')))
  types <- names(type_counts)
  tag_counts <- tag_counts_tmp[!(names(tag_counts_tmp) %in% types)]
  
  return(tag_counts)
}


