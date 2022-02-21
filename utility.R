# Takes a row from the catalog and generates a string to output in the associated UI card
gen_rsc_info <- function(resource) {
  info <- ""
  
  ignore_col <- c("Name", "Included Measures", "Tools", "Past Projects", "Notes")
  
  for (info_cat in names(resource)) {
    if (!is.na(resource[[info_cat]]) & !(info_cat %in% ignore_col)) {
      if (grepl("Link", info_cat)) {
        info <- paste0(info, "<p><b>", info_cat, ":</b> <a href=", resource["Link"], ">", resource["Link"], "</a></p>")
      } else {
        info <- paste0(info, "<p><b>", info_cat, ":</b> ", resource[info_cat], "</p>")
      }
    }
  }
  
  return(info)
}