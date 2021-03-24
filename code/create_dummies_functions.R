## Create dummie variables with 0/1 values and columns contain the selected values


## different functions for the different variants due to our survey structure

create_dummies <- function(df_mrcv, var_name) {
  cnames <- colnames(df_mrcv)
  n_var <- length(cnames[grepl(var_name, cnames)])
  for (i in 1:n_var) {
    m <- df_mrcv[1:nrow(df_mrcv), i] != "" & !is.na(df_mrcv[1:nrow(df_mrcv), i])
    cnames[i] <- strsplit(cnames[i], "_", fixed = T)[[1]][[1]] %+% "." %+% df_mrcv[m, i][[1]]
  }
  names(df_mrcv)[1:n_var] <- cnames[1:n_var]
  df_mrcv <- df_mrcv %>% 
    mutate_at(.vars = cnames[1:n_var], ~ifelse(.x == "", 0, 1))
  
  #return(list(df=df_mrcv, n_var=n_var))
  return (df_mrcv)
}

create_dummies_shazam <- function(df_mrcv, var_name) {
  var_values <- df_mrcv %>% select(starts_with(var_name)) %>% gather() %>% filter(!is.na(value)) %>% filter(value != "") %>% pull(value) %>% unique
  n_var <- length(var_values)
  
  ## create new columns in new dataframe, just to be sure
  df_mrcv2 <- df_mrcv
  
  for (i in 1:n_var) {
    #print(var_values[i])
    #print(rowSums(df_mrcv %>% select(starts_with(var_name)) == var_values[i], na.rm = TRUE))
    df_mrcv2[var_name %+% "." %+% var_values[i]] <- ifelse(rowSums(df_mrcv %>% select(starts_with(var_name)) == var_values[i], na.rm = TRUE) > 0, 1, 0)
  }
  
  #return(list(df=df_mrcv2, n_var=n_var))
  return(df_mrcv2)
}

create_dummies_single <- function(df_mrcv, var_name) {
  var_values <- df_mrcv %>% pull(!!var_name)  %>% str_replace_all("[^a-zA-z0-9]", "_") %>% unique
  n_var <- length(var_values)
  
  for (i in 1:n_var) {
    df_mrcv[var_name %+% "." %+% var_values[i]] <- ifelse(str_replace_all(df_mrcv[[var_name]], "[^a-zA-z0-9]", "_") == var_values[i], 1, 0)
  }
  return(df_mrcv)
}



## source with several values in each shazam column possible
create_dummies_shazam_multiple <- function(df_mrcv, var_name, take_word=TRUE, var_values = NULL) {
  if (is.null(var_values)){
    var_values <- df_mrcv %>% select(starts_with(var_name)) %>% gather() %>% filter(!is.na(value)) %>% filter(value != "") %>% 
      pull(value) %>% str_split(",") %>% unlist %>% unique
    m <- !is.na(var_values) & var_values != ""
    var_values <- var_values[m]
  }
  
  
  orig_names <- df_mrcv %>% select(starts_with(var_name)) %>% names
  
  n_var <- length(var_values)
  
  df_mrcv2 <- df_mrcv
  for (i in 1:n_var) {
    df_mrcv2[var_name %+% "." %+% var_values[i]] <- 0
  }
  
  for (i in 1:nrow(df_mrcv2)) {
    for (j in 1:n_var) {
      values <- df_mrcv[i, orig_names] %>% gather() %>% filter(!is.na(value)) %>% filter(value != "") %>% pull(value) %>% str_split(",") %>% unlist
      df_mrcv2[i, var_name %+% "." %+% var_values[j]] <- ifelse(var_values[j] %in% values, 1, 0)
    }
  }
  
  return(df_mrcv2)
  
}