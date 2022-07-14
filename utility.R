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

# Get the count of each resource type found in resource_set 
# Inputs:
#   - resource_set: dataframe of a set of resources
# Output:
#   - type_counts: table where names are the resource types and the values are their frequencies in resource_set
get_type_dist <- function(resource_set) {
  all_types <- unlist(lapply(strsplit(resource_set$Tags, '; '), function(x) {x[1]}))
  type_counts <- sort(table(all_types), decreasing = TRUE)
  
  return(type_counts)
}

# Get the count of each year found in resource_set 
# Inputs:
#   - resource_set: dataframe of a set of resources
# Output:
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
#   - resource_set: dataframe of a set of resources
# Output:
#   - type_counts: table where names are the tag names and the values are their frequencies in resource_set
get_tags_dist <- function(resource_set) {
  all_types <- unlist(lapply(strsplit(resource_set$Tags, '; '), function(x) {x[1]}))
  type_counts <- sort(table(all_types), decreasing = TRUE)
  
  tag_counts_tmp <- table(unlist(strsplit(resource_set$Tags, '; ')))
  types <- names(type_counts)
  tag_counts <- tag_counts_tmp[!(names(tag_counts_tmp) %in% types)]
  
  return(tag_counts)
}

# Get a similarity matrix containing the number of shared resources among the set of tags
# Inputs:
#   - resource_set: dataframe of a set of resources
#   - tags_set: list where keys are the tag types and values are the set of selected tags within that type
# Output:
#   - filtered_set: dataframe of resources filtered by tags_set
filter_by_tags <- function(resource_set, tags_set) {
  filtered_set <- resource_set
  
  # Get sources with the selected tags
  if (!is.null(unlist(tags_set))) {
    # Filter according to each type of tag
    for (tag_type in names(tags_set)) {
      if (!is.null(tags_set[[tag_type]])) {
        # Get user-selected tags for this tag type
        get_select <- tags_set[[tag_type]]
        
        # Remove any entries that don't align with the selected tags
        keep_indices <- c()
        for (i in 1:nrow(filtered_set)) {
          # Get tags for i-th entry
          i_tags <- strsplit(filtered_set[i, "Tags"], ";")[[1]]
          i_tags <- trimws(i_tags)
          # If i-th resource is not of the selected tag types, remove it from catalog
          if (length(intersect(get_select, i_tags)) > 0) {
            keep_indices <- c(keep_indices, i)
          } 
        }
        filtered_set <- filtered_set[keep_indices,]
      }
    }
  }
  
  return(filtered_set)
}

# Return necessary data objects for generating a Sankey diagram of tag connections
# Inputs:
#   - resource_set: dataframe of a set of resources
#   - list_of_tags: dataframe containing full list of available tags (pulled from Excel catalog "List of Tags" tab)
# Output:
#   - out: list containing the nodes and links dataframes to be passed to the Sankey network generation function
get_sankey_data <- function(resource_set, list_of_tags) {
  # Get set of all tags utilized in resource_set
  all_tags <- unique(unlist(strsplit(resource_set$Tags, '; ')))
  # Get the complete set of available data types
  all_types <- unique(list_of_tags$'Tags'[list_of_tags$'Tag Type' == "Resource Type"])
  # Get the set of unique resource types in resource_set
  res_types <- unique(unlist(lapply(strsplit(resource_set$Tags, '; '), function(x) {x[1]})))
  
  # Get the complete set of available "data subjects" and their associated domains
  domains <- unique(list_of_tags$Domain[list_of_tags$'Tag Type' == "Data Subject"])
  subjects_by_domain <- sapply(domains, function(d) {
    list_of_tags$Tags[(list_of_tags$'Tag Type' == "Data Subject") & (list_of_tags$Domain == d)]
  }, simplify = FALSE)
  subjects <- as.vector(unlist(subjects_by_domain))
  
  # Set of subjects present in resource_set
  res_subjects <- intersect(subjects, all_tags)
  
  # Get the complete set of available "data subjects" and their associated domains
  attr_domains <- unique(list_of_tags$Domain[list_of_tags$'Tag Type' == "Data Attribute"])
  attributes_by_domain <- sapply(attr_domains, function(d) {
    list_of_tags$Tags[(list_of_tags$'Tag Type' == "Data Attribute") & (list_of_tags$Domain == d)]
  }, simplify = FALSE)
  attributes <- as.vector(unlist(attributes_by_domain))
  
  # Set of attributes present in resource_set
  res_attributes <- intersect(attributes, all_tags)
  
  # Create universal color map that's based on all possible values so it doesn't change when resource_set changes
  color_cats <- c(all_types, domains, attr_domains)
  color_pal <- suppressWarnings(brewer.pal(length(color_cats), "Set1"))
  color_map <- lapply(1:length(color_cats), function(i) {color_pal[((i-0) %% length(color_pal)) + 1]})
  names(color_map) <- color_cats
  
  # Tag/node names
  name <- c(res_types, res_subjects, res_attributes)
  
  # Group the tag/node names according to their specified domain for coloring purposes
  subj_doms <- unlist(lapply(res_subjects, function(s) {list_of_tags$Domain[list_of_tags$'Tags' == s]}))
  attr_doms <- unlist(lapply(res_attributes, function(a) {list_of_tags$Domain[list_of_tags$'Tags' == a]}))
  group <- c(res_types, subj_doms, attr_doms)
  color <- unlist(lapply(group, function(g) {color_map[[g]]}))
  # Generate node positions (0 is left-most position then increases towards the right)
  x_pos <- c(rep(0, length(res_types)), rep(1, length(subj_doms)), rep(2, length(attr_doms)))
  
  # Nodes object
  nodes <- data.frame(name, group, color, x_pos)
  
  # Links object
  links <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(links) <- c("source", "target", "value")
  
  # Compute weight of each link (i.e. how many resources share tags between the two end nodes of the link)
  # Generate weight matrix where row/colnames are tag names and cell (i,j) is the number of resources with tags i and j
  link_weights <- data.frame(matrix(ncol=length(name), nrow=length(name)))
  link_weights[is.na(link_weights)] <- 0
  colnames(link_weights) <- rownames(link_weights) <- name
  for (i in 1:nrow(resource_set)) {
    # Resource_i information
    i_tags <- trimws(strsplit(resource_set[i, "Tags"], ";")[[1]])
    i_res_type <- i_tags[1]
    i_subj <- intersect(i_tags, subjects)
    i_attr <- intersect(i_tags, attributes)
    
    # Set of possible links--cartesian product between resource types and subjects + cartesian product between subjects and attributes
    i_links <- rbind(expand.grid(i_res_type, i_subj), expand.grid(i_subj, i_attr))
    for (j in 1:nrow(i_links)) {
      node1 <- as.character(i_links[j, "Var1"])
      node2 <- as.character(i_links[j, "Var2"])
      # If resource_i has both node1 and node2 tags, increase the link weight by 1
      if (!any(is.na(c(node1, node2)))) {
        link_weights[node1, node2] <- link_weights[node2, node1] <- link_weights[node1, node2] + 1
      }
    }
  }
  
  # Level 1 Connections: Resource Types --> Subjects
  for (r in res_types) {
    for (s in res_subjects) {
      counts <- link_weights[r, s]
      # Only add link to dataframe if it has nonzero count (otherwise the link will still be visible on the figure even with count=0)
      if (counts > 0) {
        links[nrow(links) + 1,] <- c(r, s, counts)
      }
    }
  }
  
  # Level 2 Connections: Subjects --> Attributes
  for (s in res_subjects) {
    for (a in res_attributes) {
      counts <- link_weights[s, a]
      # Only add link to dataframe if it has nonzero count (otherwise the link will still be visible on the figure even with count=0)
      if (counts > 0) {
        links[nrow(links) + 1,] <- c(s, a, counts)
      }
    }
  }
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  out <- list()
  out$nodes <- nodes
  out$links <- links
  
  return(out)
}