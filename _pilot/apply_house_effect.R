apply_house_effect <- function(){
  for (local_party in local_party_names){
    # local_party <- "더불어민주당" # for test
    print(sprintf("applying local_party %s ...", local_party))
    var_to_create <- paste0(local_party, "_fixed")
    print(sprintf("new varname for applied approval rate: %s ", var_to_create))
    original_var <- rlang::sym(local_party)
    effect_var <- rlang::sym(paste0("house_effect.",local_party))
    
    if(as.character(effect_var) %in% colnames(local_data)){
      local_data <- local_data %>%
        mutate(!!var_to_create := !!original_var - !!effect_var)
    }else{
      local_data <- local_data %>%
        mutate(!!var_to_create := !!original_var)
    }
  }
  return(local_data)
}