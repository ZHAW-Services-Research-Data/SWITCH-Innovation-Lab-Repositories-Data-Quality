## ******************************************************************************
#' rename_cols
#'
#' @param lookup_table a lookup table
#' @param pattern name of key column
#' @param replacement name of raw column
#'
#' @return lookup table with renamed columns
#' @export
#'
#' @examples
#' # some examples if you want to highlight the usage in the package
rename_cols <- function(lookup_table, pattern="key", replacement="raw") {
  names(lookup_table)[names(lookup_table)==pattern] <- "pattern"
  names(lookup_table)[names(lookup_table)==replacement] <- "replacement"
  return(lookup_table)
}

#' valueslist2rows
#'
#' @param df dataframe or tibble
#' @param cname column name containing the list of values
#' @param sep separator for list elements, default ","
#' @param cname_new name of the new column, default cname
#' @param ignore_duplicates if TRUE, ignore duplicated list elements
#' @param dont_create_empty_fields if TRUE, don't create rows with new empty string
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom rlang .data
#'
#' @return transformed data frame
#' @export
#'
#' @examples
#' # some examples if you want to highlight the usage in the packag
valueslist2rows <- function(df, cname, sep = ",",
                            cname_new = cname,
                            ignore_duplicates=TRUE,
                            dont_create_empty_fields=TRUE) {
  # create vector with elements based on separator
  values <- df %>%
    dplyr::pull(.data[[cname]]) %>%
    paste(collapse = sep)
  values <- stringr::str_split(values, stringr::fixed(sep))[[1]] %>%
    stringr::str_trim()
  
  # add helper columns with the number of columns that shall be added
  df <- df %>%
    dplyr::mutate(
      aux_no_elements = stringr::str_count(.data[[cname]], fixed(sep))+1,
      aux_original_empty = .data[[cname]] == ""
    ) %>%
    tidyr::uncount(.data$aux_no_elements)
  df[[cname_new]] <- values
  
  if (dont_create_empty_fields){
    df <- df %>% dplyr::filter(.data[[cname_new]] != "" | .data$aux_original_empty)
  }
  
  if (ignore_duplicates) {
    df <- df %>% dplyr::distinct()
  }
  
  # remove helper columns
  helper_names <- c("aux_no_elements", "aux_original_empty")
  df <- df %>% dplyr::select(-all_of(helper_names))
  
  return(df)
}


## Remove invalid records

remove_invalid_records <- function(df, first_valid_record=NULL, valid_records=NULL) {
  if (!is.null(first_valid_record)) {
    df <- df %>%
      slice(first_valid_record:n())
  } else {
    if (!is.null(valid_records)) {
      df <- df %>%
        slice(valid_records)
    }
  }
  return(df)
}


## From two columns, create a vector of the form c(pat="rep") and sort by size, descending, if specified
create_lookup_vector <- function (lookup_table, pattern = "pat", replacement = "rep", arrange_by_size=TRUE) 
{
  res <- lookup_table %>% rename_cols(replacement = replacement, pattern = pattern) 
  if (arrange_by_size){
    res <- res %>% dplyr::arrange(-stringr::str_length(.data$pattern))
  }
  
  res <- stats::setNames(res %>% dplyr::pull(.data$replacement), 
                         res %>% dplyr::pull(.data$pattern))
  return(res)
}


## Select all variables from multiple choices questions
select_two_variables <- function(df, var1, var2=NULL, additional_to_keep=NULL, additional_to_throw=NULL, remove_other=FALSE) {
  if (is.null(var2)) {
    var2 <- var1
  }
  df_var1_var2 <- df %>% select(all_of(additional_to_keep), starts_with(var1), starts_with(var2), -all_of(additional_to_throw))
  
  if (remove_other) {
    df_var1_var2 <- df_var1_var2 %>% select(-ends_with("_other"))
  }
  return(df_var1_var2)
}


## Create a dataframe with all occuring combinations of two multiple choice questions
create_product_df <- function(df, var1, var2=NULL, id_var = "record_id", sep1 = "|", sep2 = ",") {
  df_var1_var2 <- select_two_variables(df, var1, var2, additional_to_keep=id_var, remove_other=TRUE)
  
  df_cnt_var1 <- df_var1_var2 %>% select(record_id, starts_with(var1)) %>%
    gather("key", "value", -!!id_var) %>%
    filter(value != "") %>%
    count(record_id) %>%
    select(record_id, n_var1 = n) 
  
  df_var1 <- df_var1_var2 %>% 
    select(starts_with(var1))
  
  if (!is.null(var2)) {
    df_cnt_var2 <- df_var1_var2 %>% select(record_id, starts_with(var2)) %>%
      gather("key", "value", -!!id_var) %>%
      filter(!is.na(value)) %>%
      count(record_id) %>%
      select(record_id, n_var2 = n)
    
    df_var2 <- df_var1_var2 %>%
      select(starts_with(var2))
    
    df_res <- df_var1_var2 %>%
      mutate(!!var1 := unite(df_var1, !!var1, sep=sep1) %>% pull(.data[[var1]]) %>% as.factor,
             !!var2 := unite(df_var2, !!var2, sep=sep2, na.rm=TRUE) %>% pull(.data[[var2]]) %>% as.factor
      ) %>%
      select(record_id, .data[[var1]], .data[[var2]]) %>%
      left_join(df_cnt_var1, by = "record_id") %>%
      left_join(df_cnt_var2, by = "record_id") %>%
      replace_na(list(n_var1 = 0, n_var2 = 0))
    
    df_res <- valueslist2rows(df_res, var1, sep = sep1)
    df_res <- valueslist2rows(df_res, var2, sep = sep2)
  } else {
    df_res <- df_var1_var2 %>%
      mutate(!!var1 := unite(df_var1, !!var1, sep=sep1) %>% pull(.data[[var1]]) %>% as.factor
      ) %>%
      select(record_id, .data[[var1]]) %>%
      left_join(df_cnt_var1, by = "record_id") %>%
      replace_na(list(n_var1 = 0))
    
    df_res <- valueslist2rows(df_res, var1, sep = sep1)
  }
  
  return(df_res)
}


## ******************************************************************************
#' normalize_blanks_tolower
#'
#' @param x string or vector of strings
#'
#' @return normalized string or vector of normalized strings
#'
#' @import stringr
#' @export
#'
#' @examples
#' # some examples if you want to highlight the usage in the package
normalize_blanks_tolower <- function(x) {
  res <- x %>%
    stringr::str_to_lower() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\s+", " ")
  return(res)
}


## ******************************************************************************
#' normalize_text
#'
#' @param x a vector of values
#' @param lookup_table a lookup table
#' @param pattern name of raw/pattern column
#' @param replacement name of key/replacement column
#'
#' @return vector of normalized text values
#'
#' @import stringr
#' @export
#'
#' @examples
#' # some examples if you want to highlight the usage in the package
normalize_text <- function(x, lookup_table, pattern="raw", replacement="key") {
  lookup_vector <- create_lookup_vector(lookup_table, pattern=pattern, replacement=replacement)
  res <- x %>%
    normalize_blanks_tolower() %>%
    stringr::str_replace_all(lookup_vector)
  return(res)
}
