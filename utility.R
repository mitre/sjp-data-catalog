library(lsa)


# Get number of total pages given the length of the results to show and the per_page input
get_num_pages <- function(df, per_page) {
  if (nrow(df) %% per_page == 0) {
    total_pages <- nrow(df) %/% per_page 
  } else {
    total_pages <- nrow(df) %/% per_page + 1  
  }
  return(total_pages)
}

# Sort the catalog according to some criteria (e.g. "Alphabetical", "Year: Oldest to Newest", "Year: Newest to Oldest")
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
gen_rsc_info <- function(resource) {
  names(resource) <- unlist(lapply(names(resource), function(x) {gsub("_", " ", x)})) #change underscores back to spaces in the column names
  
  info <- ""
  
  ignore_col <- c("Name", "Included Measures", "Tools", "Past Projects", "Notes")  #columns to ignore
  
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

# Generate binary features vectors for each of the resources to pass to the similarity measure
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
euclidean <- function(x, y) {
  sqrt(sum((x - y)^2))
}

### Sim matrix is computed in get_all_sims ###
# # Generates a similarity matrix between all combinations of resources
# sim_matrix <- function(catalog_features, sim_measure="cosine") {
#   sim <- data.frame(matrix(nrow = nrow(catalog_features), ncol = nrow(catalog_features)))
#   colnames(sim) <- rownames(catalog_features)
#   rownames(sim) <- rownames(catalog_features)
#   
#   for (i in 1:(nrow(catalog_features)-1)) {
#     # Get features vector for resource i
#     i_feat <- as.numeric(as.vector(catalog_features[i, ]))
#     for (j in (i+1):nrow(catalog_features)) {
#       # Get features vector for resource j
#       j_feat <- as.numeric(as.vector(catalog_features[j, ]))
#       
#       # Compute similarity between i and j
#       if (sim_measure == "cosine") {
#         sim_ij <- cosine(i_feat, j_feat)
#       } else {#if sim_measure == "euclidean"
#         sim_ij <- 1 / (1 + euclidean(i_feat, j_feat))  #sim measure based on Euclidean dist
#       }
#       
#       # Matrix is symmetric
#       sim[i, j] <- sim_ij
#       sim[j, i] <- sim_ij
#     }
#   }
#   
#   return(sim)
# }

### Shared features and methodologies is computed in get_all_sims ###
# # Number of shared features between each pair of resources
# # This function needs to know where to parse the features vectors based on what shared elements you are counting.
# # In order to do this, you need to pass to the function the total number of tags/methodologies available.
# shared_features <- function(catalog_features, count="tags", total_tags=0, total_methods=0) {
#   num_shared <- data.frame(matrix(nrow = nrow(catalog_features), ncol = nrow(catalog_features)))
#   colnames(num_shared) <- rownames(catalog_features)
#   rownames(num_shared) <- rownames(catalog_features)
#   
#   for (i in 1:(nrow(catalog_features)-1)) {
#     i_feat <- as.numeric(as.vector(catalog_features[i, ]))
#     for (j in (i+1):nrow(catalog_features)) {
#       j_feat <- as.numeric(as.vector(catalog_features[j, ]))
#       
#       if (count == "tags") {
#         # Tags take up the first n_tags features in the vectors
#         overlap <- i_feat[1:total_tags] + j_feat[1:total_tags]
#       } else {#count == "methodologies"
#         # Methodologies take up the last n_methods features in the vectors
#         overlap <- i_feat[(length(i_feat) - total_methods + 1):length(i_feat)] + j_feat[(length(j_feat) - total_methods + 1):length(j_feat)]
#       }
#       
#       shared_ij <- sum(overlap == 2)
#       num_shared[i, j] <- shared_ij
#       num_shared[j, i] <- shared_ij
#     }
#   }
#   
#   return(num_shared)
# }

# Computes all the different similarity-related values in one loop
get_all_sims <- function(catalog_features, sim_measure, total_tags, total_methods) {
  # Similarity measure
  sim <- data.frame(matrix(nrow = nrow(catalog_features), ncol = nrow(catalog_features)))
  colnames(sim) <- rownames(catalog_features)
  rownames(sim) <- rownames(catalog_features)
  
  # Lists of shared tags
  n_tags_shared <- data.frame(matrix(nrow = nrow(catalog_features), ncol = nrow(catalog_features)))
  colnames(n_tags_shared) <- rownames(catalog_features)
  rownames(n_tags_shared) <- rownames(catalog_features)
  
  # Lists of shared tags
  tags_shared <- data.frame(matrix(nrow = nrow(catalog_features), ncol = nrow(catalog_features)))
  colnames(tags_shared) <- rownames(catalog_features)
  rownames(tags_shared) <- rownames(catalog_features)
  
  # Lists of shared methodologies
  methods_shared <- data.frame(matrix(nrow = nrow(catalog_features), ncol = nrow(catalog_features)))
  colnames(methods_shared) <- rownames(catalog_features)
  rownames(methods_shared) <- rownames(catalog_features)
  
  for (i in 1:(nrow(catalog_features)-1)) {
    # Get features vector for resource i
    i_feat <- as.numeric(as.vector(catalog_features[i, ]))
    for (j in (i+1):nrow(catalog_features)) {
      # Get features vector for resrouce j
      j_feat <- as.numeric(as.vector(catalog_features[j, ]))
      
      # Compute similarity metric
      if (sim_measure == "cosine") {
        sim_ij <- cosine(i_feat, j_feat)
      } else {#if sim_measure == "euclidean"
        sim_ij <- 1 / (1 + euclidean(i_feat, j_feat))  #sim measure based on Euclidean dist
      }
      
      sim[i, j] <- sim_ij
      sim[j, i] <- sim_ij
      
      # Get list of shared tags
      # Tags take up the first n_tags features in the vectors
      tags_overlap <- intersect(colnames(catalog_features)[1:total_tags][as.logical(i_feat[1:total_tags])], colnames(catalog_features)[1:total_tags][as.logical(j_feat[1:total_tags])]) 
      
      n_tags_shared[i, j] <- length(tags_overlap)
      n_tags_shared[j, i] <- length(tags_overlap)
      
      tags_shared[i, j][[1]] <- list(tags_overlap)
      tags_shared[j, i][[1]] <- list(tags_overlap)
      
      # Get list of shared methodologies
      # Methodologies take up the last n_methods features in the vectors
      methods_overlap <- intersect(colnames(catalog_features)[(length(i_feat) - total_methods + 1):length(i_feat)][as.logical(i_feat[(length(i_feat) - total_methods + 1):length(i_feat)])], colnames(catalog_features)[(length(i_feat) - total_methods + 1):length(i_feat)][as.logical(j_feat[(length(i_feat) - total_methods + 1):length(i_feat)])])  

      methods_shared[i, j][[1]] <- list(methods_overlap)
      methods_shared[j, i][[1]] <- list(methods_overlap)
    }
  }
  
  out <- list()
  out$sim_matrix <- sim
  out$n_tags <- n_tags_shared
  out$tags <- tags_shared
  out$methods <- methods_shared
  
  return(out)
}


