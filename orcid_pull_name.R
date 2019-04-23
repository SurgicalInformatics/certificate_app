
orcid_pull_name <- function(list_orcid, initials = TRUE, full_name = TRUE){
  library(dplyr)
  
  # Pull orcid information / select out name
  df <- purrr::map(list_orcid, purrr::safely(function(x){xml2::as_list(xml2::read_html(paste0("https://pub.orcid.org/v2.1/",
                                                                                              x,
                                                                                              "/personal-details")))$`html`$`body`$`personal-details`$name %>%
      
      cbind.data.frame(orcid = x, fn_orcid = .$`given-names`[[1]], ln_orcid = .$`family-name`[[1]]) %>%
      .[which(colnames(.) %in% c("orcid", "fn_orcid", "ln_orcid"))]}))
  
  # If orcid invalid, then replace with NA
  df <- purrr::transpose(df)[["result"]] %>%
    purrr::map(function(x){if(is.null(x)==T){cbind.data.frame("orcid" = NA, fn_orcid = NA, ln_orcid = NA)}else{x}}) %>%
    data.table::rbindlist() %>%
    dplyr::mutate_all(as.character) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(orcid = ifelse(is.na(orcid)==T, list_orcid, orcid))
  
  if(initials==TRUE){
    df <- df %>%
      dplyr::mutate(i1 = toupper(substr(stringr::str_split_fixed(fn_orcid, " ", 3)[,1],1,1)),
                    i2 = toupper(substr(stringr::str_split_fixed(fn_orcid, " ", 3)[,2],1,1)),
                    i3 = toupper(substr(stringr::str_split_fixed(fn_orcid, " ", 3)[,3],1,1))) %>%
      dplyr::mutate(i_com = gsub(" ", "", paste0(i1, i2, i3))) %>%
      dplyr::mutate(ln_orcid = ifelse(grepl("^[[:upper:]]+$", ln_orcid)==F,
                                      ln_orcid,
                                      paste0(substr(ln_orcid, 1,1),
                                             tolower(substr(ln_orcid, 2, nchar(ln_orcid)))))) %>%
      dplyr::mutate(initial_orcid = ifelse(is.na(fn_orcid)==F&is.na(ln_orcid)==F,
                                           paste0(i_com, " ", ln_orcid),
                                           NA)) %>%
      dplyr::select(orcid, fn_orcid, ln_orcid, initial_orcid)}
  
  if(full_name==TRUE){
    df <- df %>%
      dplyr::mutate(full_name_orcid = ifelse(is.na(fn_orcid)==F&is.na(ln_orcid)==F,
                                             paste0(fn_orcid, " ", ln_orcid),
                                             NA))}
  
  return(df)}
