set_up <- function() {
  year_type <- remDr$findElement(using = 'xpath', 
                                 value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl04_ctl03_ddValue"]/option[2]')
  year_type$clickElement()
  
  start_year <- remDr$findElement(using = 'xpath', 
                                  value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl04_ctl05_txtValue"]')
  start_year$clickElement()
  start_year$sendKeysToActiveElement(list("1/1/2001"))
  
  end_year <- remDr$findElement(using = 'xpath', 
                                value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl04_ctl09_txtValue"]')
  end_year$clickElement()
  
  end_year$sendKeysToActiveElement(list("1/1/2019"))
  
  init_selector <- remDr$findElement(using = 'xpath', 
                                     value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl04_ctl07_ddValue"]/option[2]') 
  cntry_selector$clickElement()
}

download_all <- function(x) {
  
  
  print(glue("{x} out of 266 ({round((x/266)*100, 2)}%)\n"))
  
  
  #this wont work the first time around!!
  cntry_selector <- remDr$findElement(using = 'xpath', 
                                      value = glue('//*[@id="dnn_ctr513_View_ReportViewer1_ctl04_ctl07_ddValue"]/option[{x}]'))
  cntry_selector$clickElement()
  
  
  view_report <- remDr$findElement(using = 'xpath', 
                                   value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl04_ctl00"]')
  view_report$clickElement()
  
  # Sys.sleep(20)
  
  is_visible <- T
  
  while (is_visible) {
    Sys.sleep(5)
    is_visible <- remDr$getPageSource() %>% extract2(1) %>% read_html() %>% 
      html_nodes("#dnn_ctr513_View_ReportViewer1_AsyncWait_Wait") %>% 
      html_attr("style") %>% 
      str_detect("visibility: visible") 
    
    cat("Waiting 5 seconds...\n")
  }
  
  message("Clicking now!\n")
  
  click_diskette <- remDr$findElement(using = 'xpath', 
                                      value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl05_ctl04_ctl00_ButtonImg"]')
  click_diskette$clickElement()
  
  
  download_excel <- remDr$findElement(using = 'xpath', 
                                      value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl05_ctl04_ctl00_Menu"]/div[5]/a')
  download_excel$clickElement()
  
  out <- F
  
  while (magrittr::not(out)) {
    Sys.sleep(1)
    out <- file.exists("C:/Users/fabio/Downloads/MX - Arrivals for a Demographic Profile.xls")
  }
  
  file.rename("C:/Users/fabio/Downloads/MX - Arrivals for a Demographic Profile.xls", glue("C:/Users/fabio/Downloads/{x}.xls"))
}



complete_ages <- c("Under 14" , "Age 14 to 20", "Age 21 to 30", 
                   "Age 31 to 40", "Age 41 to 50", "Age 51 to 64", 
                   "Age 65 and Over")

is_complete_age <- function(only_ages) {
  only_ages <- only_ages %>% na.omit()  
  complete_ages %>% 
    map_lgl(~str_detect(only_ages, .x) %>% any)
}


complete_years <- glue("CY {2002:2018}") %>% as.character() 

is_complete_years <- function(x) {
  data <- x %>% na.omit()  
  complete_years %>% 
    map_lgl(~str_detect(data, .x) %>% any)
}


complete_genders <- c(glue("{complete_years} F"), glue("{complete_years} M"))

is_complete_gender <- function(x) {
  data <- na.omit(x)
  complete_genders %>% 
    map_lgl(~str_detect(data, .x) %>% any)
}


get_checkers <- function(raw_dat) {
  
  age_check <- raw_dat %>% 
    magrittr::use_series(department_of_state) %>% 
    is_complete_age %>% 
    tibble(complete_ages, present = .)
  
  years_check <- raw_dat %>% 
    filter(department_of_state == "Characteristic") %>% t %>% as.character %>% na.omit() %>% 
    keep(str_detect(., "CY")) %>% 
    is_complete_years %>% 
    tibble(complete_years, present = .)
  
  gender_check <- raw_dat %>% 
    tidyr::fill(department_of_state, .direction = "down") %>% 
    filter(department_of_state == "Characteristic") %>% t %>% as_tibble() %>%
    tidyr::fill(V1, .direction = "down") %>% 
    filter(str_detect(V2, "M|F")) %>% 
    mutate(tester = paste(V1, V2)) %>% 
    use_series(tester) %>% 
    is_complete_gender() %>% 
    tibble(complete_genders, present = .) %>% 
    arrange(complete_genders)
  
  return(list(age_check = age_check, 
              years_check = years_check, 
              gender_check = gender_check))
}


check_ages_complete <- function(x) {
  age_complete <- x$age_check %>%
    magrittr::use_series(present) %>% all
  
  years_complete <- x$years_check %>%
    magrittr::use_series(present) %>% all
  
  genders_complete <- x$gender_check %>% 
    use_series(present)  %>% all
  
  return(list(age_complete = age_complete, 
              years_complete = years_complete, 
              genders_complete = genders_complete))
}


check_ages_incomplete <- function(x) {
  age_incomplete <- x$age_check %>%
    magrittr::use_series(present) %>% sum() %>% equals(0)
  
  years_incomplete <- x$years_check %>%
    magrittr::use_series(present) %>% sum() %>% equals(0)
  
  genders_incomplete <- x$gender_check %>% 
    use_series(present) %>% sum() %>% equals(0)
  
  return(list(age_incomplete = age_incomplete, 
              years_incomplete = years_incomplete, 
              genders_incomplete = genders_incomplete))
}

check_ages_partial <- function(x) {
  age_partial <- x$age_check %>%
    magrittr::use_series(present) %>% sum() %>% magrittr::is_less_than(7)
  
  years_partial <- x$years_check %>%
    magrittr::use_series(present) %>% sum() %>% magrittr::is_less_than(17)
  
  genders_partial <- x$gender_check %>% 
    use_series(present) %>% sum() %>% magrittr::is_less_than(34)
  
  return(list(age_partial = age_partial, 
              years_partial = years_partial, 
              genders_partial = genders_partial))
}



parse_age_data <- function(filename) {
  
  cat(glue("\n{parse_number(filename)} out of 266 ({round((parse_number(filename)/266)*100, 2)}%)\n"))
  
  raw_dat <- readxl::read_excel(glue("C:/Users/fabio/Downloads/{filename}.xls"), sheet = "Age Group") %>% 
    janitor::clean_names() 
  
  age_checkers <- get_checkers(raw_dat)
  
  is_ages_incomplete <- age_checkers %>% 
    check_ages_incomplete %>% as_vector  %>% all
  
  is_ages_complete <- age_checkers %>% 
    check_ages_complete %>% as_vector %>% all
  
  is_ages_partial <- age_checkers %>% 
    check_ages_partial %>% as_vector  %>% any
  
  if(is_ages_incomplete) {
    file.copy(from = glue("C:/Users/fabio/Downloads/{filename}.xls"), to = glue("C:/Users/fabio/Downloads/no_data/{filename}.xls"))
    if (file.exists(glue("C:/Users/fabio/Downloads/no_data/{filename}.xls"))) file.remove(glue("C:/Users/fabio/Downloads/{filename}.xls"))
  } else if(is_ages_complete) {
    
    cleaned_dat <- raw_dat %>% 
      tidyr::fill(department_of_state, .direction = "down") %>% 
      filter(department_of_state %in% c("Characteristic", complete_ages)) %>% 
      set_names(.[1,]) %>% 
      .[-1,] %>% 
      janitor::clean_names() %>% 
      select_if(.predicate = function(x) is.na(x) %>% all %>% not) %>% 
      gather(key, value, -characteristic) %>% 
      mutate(key = ifelse(str_detect(key, "na"), NA, key)) %>% 
      tidyr::fill(key, .direction = "down") %>% 
      mutate(gender = ifelse(characteristic == "Characteristic", value, NA)) %>% 
      tidyr::fill(gender, .direction = "down") %>%
      filter(not(gender == "Total")) %>%
      filter(not(value %in% c("F", "M"))) %>% 
      mutate(key = parse_number(key)) %>% 
      mutate(value = as.numeric(value)) %>% 
      mutate(cntry = filename)
    
    return(cleaned_dat)
    
  } else if (is_ages_partial) {
    
    simulated_ages <- expand.grid(complete_ages, 2002:2018) %>% 
      bind_rows(., .) %>% mutate(gender = c(rep("F", nrow(.)/2), rep("M", nrow(.)/2))) %>% 
      rename(characteristic = Var1, key = Var2) %>% 
      mutate(characteristic = as.character(characteristic))
    
    cleaned_dat <- raw_dat %>% 
      tidyr::fill(department_of_state, .direction = "down") %>% 
      filter(department_of_state %in% c("Characteristic", complete_ages)) %>% 
      set_names(.[1,]) %>% 
      .[-1,] %>% 
      janitor::clean_names() %>% 
      select_if(.predicate = function(x) is.na(x) %>% all %>% not)  %>% 
      gather(key, value, -characteristic) %>% 
      mutate(key = ifelse(str_detect(key, "na"), NA, key)) %>% 
      tidyr::fill(key, .direction = "down") %>% 
      mutate(gender = ifelse(characteristic == "Characteristic", value, NA)) %>% 
      tidyr::fill(gender, .direction = "down") %>%
      filter(not(gender == "Total")) %>%
      filter(not(value %in% c("F", "M"))) %>% 
      mutate(key = parse_number(key)) %>% 
      mutate(value = as.numeric(value)) %>% 
      full_join(simulated_ages) %>% 
      mutate_all(function(x) ifelse(is.na(x), 0, x)) %>% 
      mutate(cntry = filename) 
    
    return(cleaned_dat)
  }
}


parse_religion_data <- function(filename) {
  readxl::read_excel(glue("C:/Users/fabio/Downloads/{filename}.xls"), sheet = "Religion") %>% 
    janitor::clean_names()  %>% 
    tidyr::fill(department_of_state, .direction = "down") %>%
    .[-1:-14,] %>% 
    set_names(.[1,]) %>% 
    .[-1,] %>% 
    janitor::clean_names() %>% 
    select_if(.predicate = function(x) is.na(x) %>% all %>% not) %>% 
    gather(key, value, -religion) %>% 
    filter(str_detect(religion, "Total|Data prior") %>% not) %>% 
    filter(str_detect(key, "cumulative|percent") %>% not) %>% 
    mutate(key = ifelse(str_detect(key, "na"), NA, key)) %>% 
    tidyr::fill(key, .direction = "down") %>% 
    mutate(gender = ifelse(religion == "Religion", value, NA)) %>% 
    tidyr::fill(gender, .direction = "down") %>% 
    filter(not(gender == "Total")) %>%
    filter(not(value %in% c("F", "M"))) %>% 
    mutate(key = parse_number(key)) %>% 
    mutate(value = as.numeric(value)) %>% 
    mutate(cntry = filename)
}



parse_education_data <- function(filename) {
  readxl::read_excel(glue("C:/Users/fabio/Downloads/{filename}.xls"), sheet = "Education") %>% 
    janitor::clean_names()  %>% 
    tidyr::fill(department_of_state, .direction = "down") %>%
    .[-1:-14,] %>% 
    set_names(.[1,]) %>% 
    .[-1,]  %>% 
    janitor::clean_names() %>% 
    select_if(.predicate = function(x) is.na(x) %>% all %>% not) %>% 
    gather(key, value, -education) %>% 
    filter(str_detect(education, "Total|Data prior") %>% not) %>% 
    filter(str_detect(key, "cumulative|percent") %>% not) %>% 
    mutate(key = ifelse(str_detect(key, "na"), NA, key)) %>% 
    tidyr::fill(key, .direction = "down") %>% 
    mutate(gender = ifelse(education == "Education", value, NA)) %>% 
    tidyr::fill(gender, .direction = "down") %>% 
    filter(not(gender == "Total")) %>%
    filter(not(value %in% c("F", "M"))) %>% 
    mutate(key = parse_number(key)) %>% 
    mutate(value = as.numeric(value))%>% 
    mutate(cntry = filename)
}



