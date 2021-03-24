calculate_chi2 <- function(df_cross, var1, var2){
  records_with_singles <- df_cross %>% group_by(record_id) %>% summarise(n =n()) %>% filter(n == 1) %>% pull(record_id)
  
  #df_cross$cat <- normalize_text(df_field_kind$researchfield, df_fields_categories_lookup, pattern="key", replacement = "category")
  
  #df_category_kind <- df_field_kind %>% select(record_id, researchcategory, kind) # %>% distinct()
  
  #records_with_single_cat <- df_category_kind %>% distinct %>% group_by(record_id) %>% summarise(n =n()) %>% filter(n == 1) %>% pull(record_id)
  
  chi2_all <- df_cross %>%
    select(!!var1, !!var2) %>% 
    table %>% chisq.test(simulate.p.value = TRUE)
  
  chi2_singles <- df_cross %>%
    filter(record_id %in% records_with_singles) %>%
    select(!!var1, !!var2) %>% 
    table %>% chisq.test(simulate.p.value = TRUE)
  
  # chi2_cat_all <- df_category_kind %>%
  #   select(researchcategory, kind) %>% 
  #   table %>% chisq.test(simulate.p.value = TRUE)
  # 
  # chi2_cat_singles <- df_category_kind %>%
  #   filter(record_id %in% records_with_single_cat) %>%
  #   select(researchcategory, kind) %>% 
  #   table %>% chisq.test(simulate.p.value = TRUE)
  #   
  df_chi2 <- tribble(
    ~sample, ~n, ~Xsquared, ~p,
    "all - " %+% var1 %+% " x " %+% var2, nrow(df_cross), chi2_all$statistic, chi2_all$p.value,
    "singles " %+% var1 %+% " x " %+% var2, nrow(df_cross%>%filter(record_id %in% records_with_singles)), chi2_singles$statistic, chi2_singles$p.value,
    #"all - categories", nrow(df_category_kind), chi2_cat_all$statistic, chi2_cat_all$p.value,
    #"singles - researchcategories", nrow(df_category_kind%>%filter(record_id %in% records_with_single_cat)), chi2_cat_singles$statistic, chi2_cat_singles$p.value,
  )
  write.csv2(df_chi2, "plots/chi2_" %+% var1 %+% "-" %+% var2 %+% ".csv", append = FALSE, row.names = FALSE)
  write.csv2(df_chi2, "plots/chi2_all.csv" , append = TRUE, row.names = FALSE)
  return(df_chi2)
}


#' mrcv_test
#' transforms the dataframe to a dataframe with appropriate column names and 0/1-values
#'
#' @param df_mrcv the dataframe with the data
#' @param var1 name of variable 1
#' @param var2 name of variable 2
#' @param var1_type type of var 1: single, shazam, mrcv, shazam_multiple
#' @param var2_type 
#' @param test_type bon for bonferroni, boot for bootstrapping, the other test option did not run on my laptop
#' @param relevant_values not yet used, idea is to filter rows for these values
#' @param return_df if TRUE, return a list containing the dataframe and the test result, otherwise only the test result (default)
#'
#' @return
#' @export
#'
#' @examples
mrcv_test <- function(df_mrcv, var1, var2, var1_type, var2_type, test_type = "bon", relevant_values=NULL, return_df=FALSE) {
  df_mrcv <- df %>% 
    ## make values of experience suitable for column name
    mutate(experience = str_replace_all(experience, "[^a-zA-z0-9]", "_")) %>%
    ## ignore other fields of researchfield and criteria
    select(-researchfield_other, -criteriaother) %>%
    ## for sources, take only the first level information (for example, LIT.JOURNALS becomes LIT)
    mutate(across(starts_with("source"), ~word(.x, 1, sep="\\."))) %>%
    ## select all columns relevant for the given pair of questions
    select(starts_with(var1), starts_with(var2))
  
  ## create the dataframe structure needed for the analysis function
  print(var1 %+% " x " %+% var2)
  if (var1_type == "shazam") {
    df_mrcv2 <- create_dummies_shazam(df_mrcv, var1)
  } else if (var1_type == "mrcv"){
    df_mrcv2  <- create_dummies(df_mrcv, var1)
  } else if (var1_type == "single") {
    df_mrcv2 <- create_dummies_single(df_mrcv, var1)
  } else {
    df_mrcv2 <- create_dummies_shazam_multiple(df_mrcv, var1, relevant_values)
  }
  
  
  if (var2_type == "shazam") {
    df_mrcv2  <- create_dummies_shazam(df_mrcv2, var2)
  } else if (var2_type == "mrcv") {
    df_mrcv3 <- create_dummies(df_mrcv2 %>% select(starts_with(var2)), var2)
    df_mrcv2 <- bind_cols(df_mrcv2, df_mrcv3)
  } else if (var2_type == "single") {
    df_mrcv2 <- create_dummies_single(df_mrcv2, var2)
  } else {
    df_mrcv2 <- create_dummies_shazam_multiple(df_mrcv2, var2, relevant_values)
  }
  
  
  df_mrcv2 <- df_mrcv2 %>%
    select(starts_with(var1 %+% "."), starts_with(var2 %+% "."))
  
  n_var1 <- df_mrcv2 %>% select(starts_with(var1)) %>% names %>% length
  n_var2 <- df_mrcv2 %>% select(starts_with(var2)) %>% names %>% length
  
  
  ## run the test
  res_mrcv <- MRCV::MI.test(df_mrcv2, I = n_var1, J = n_var2, print.status = TRUE, add.constant = FALSE, type = test_type, B=1000)
  print(res_mrcv)
  
  if (return_df) {
    return(list(df=df_mrcv2, res=res_mrcv))
  } else{
    return(res_mrcv)
  }
}
