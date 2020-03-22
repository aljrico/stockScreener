## code to prepare `companies_list` dataset goes here
create_companies_list <- function(){
  rfinance::get_symbols_list('any') %>% 
    rfinance::get_company_profile() %>% 
    dplyr::select(symbol, companyName) %>% 
    dplyr::mutate(full_name = paste0(symbol, ' | ', companyName)) %>% 
    .$full_name
}

companies_list <- create_companies_list()

usethis::use_data(companies_list, overwrite = TRUE)
