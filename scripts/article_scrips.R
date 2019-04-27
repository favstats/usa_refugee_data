
plot_age_viz <- function(variables) {
  age_dataset <- tidytemplate::load_it("data/age_dataset.Rdata")
  
  bind_rows(age_dataset) %>% 
    # filter(str_detect(cntry, "somalia")) %>% 
    filter(characteristic != "Characteristic") %>% 
    group_by(key, gender, characteristic) %>% 
    summarise(value = sum(value)) %>% 
    ggplot(aes(key, value, color = gender)) +
    geom_line() +
    facet_wrap(~characteristic, nrow = 2) +
    scale_color_viridis_d("Gender") +
    theme_minimal() +
    ggtitle("Refugees arriving in the United States of America by Gender and Age (2002 - 2018)") +
    labs(x = "", y = "Number of Refugees") +
    theme(legend.position = c(0.875, 0.2))
  
  # parse_age_data("204_senegal")
  
  age_viz <- age_dataset %>% 
    bind_rows %>% 
    mutate(characteristic = forcats::fct_relevel(characteristic, complete_ages)) %>% 
    filter(gender %in% c("M","F")) %>% 
    group_by(key, 
             #gender, 
             characteristic) %>% 
    summarize(value = sum(value))  %>% 
    mutate(total = sum(value)) %>% 
    mutate(perc = value/total) %>% 
    ungroup() 
  
  end_labs <- age_viz %>% 
    filter(key == "2018") %>% 
    mutate(perc2 = cumsum(perc)) %>% 
    arrange(desc(perc2))%>% 
    mutate(perc = cumsum(perc) - 0.021) %>% 
    mutate(perc = ifelse(characteristic == "Under 14", 0.82, perc))%>%
    mutate(perc = ifelse(characteristic == "Age 65 and Over", -0.025, perc))
  
  by_year_labs <- age_viz %>% 
    mutate(perc_label = perc) %>% 
    group_by(key) %>% 
    mutate(perc2 = cumsum(perc)) %>% 
    arrange(desc(perc2)) %>% 
    mutate(perc = cumsum(perc) - 0.01) %>% 
    ungroup() %>% 
    mutate(key = case_when(
      key == 2002 ~ 2002.2,
      key == 2018 ~ 2017.8,
      T ~ key
    ))
  
  age_viz %>% 
    ggplot(aes(key, perc, fill = characteristic)) +
    geom_area(position = "fill", alpha = 0.8) +
    #  facet_wrap(~gender, ncol = 2) +
    scale_fill_viridis_d("Gender", begin = 0.2) +
    scale_color_viridis_d("Gender", begin = 0.2) +
    theme_minimal() +
    ggtitle("Refugees arriving in the United States of America by Age Groups (2002 - 2018)\n") +
    labs(x = "", y = "Percentage of Refugees")  +
    scale_x_continuous(breaks = 2002:2018, labels = 2002:2018, 
                       # limits = c(2002, 2020.5),
                       minor_breaks = seq(2002, 2018, 1)) + 
    scale_y_continuous(label = scales::percent) +
    geom_text(data = end_labs, aes(y = perc,
                                   x = 2018,
                                   label = characteristic, 
                                   color = characteristic), 
              direction = "x",
              hjust = 0, nudge_x = 0.1) +
    guides(fill = F, color = F) +
    geom_hline(yintercept = 0.5, 
               linetype = "dashed", 
               alpha = 0.15) +
    geom_text(data = by_year_labs, aes(label = round(100*perc_label)), color = "black", size = 2.8, nudge_y = -0.01) +
    coord_cartesian(clip = 'off') +
    theme(plot.margin = unit(c(1,4,1,1), "lines"))
}



plot_religion_viz <- function() {
  religion_dataset <- tidytemplate::load_it("data/religion_dataset.Rdata")
  
  religion_viz <- religion_dataset %>% 
    mutate(religion_cat = case_when(
      str_detect(religion, "Moslem|Ahmadiyya") ~ "Muslim",
      str_detect(religion, "Christ|Baptist|Chald|Coptic|Greek|Jehovah|Lutheran|Mennonite|Orthodox|Pentecostalist|Protestant|Uniate|Adventist|Cath|Meth|Old Believer") ~ "Christian",
      str_detect(religion, "Atheist|No Religion") ~ "Atheist/No Religion",
      religion == "Hindu" ~ "Hindu",
      T ~ "Other/Unknown"
    ))  %>% 
    # mutate(religion_cat = forcats::fct_relevel(characteristic, complete_ages)) %>% 
    filter(gender %in% c("M","F")) %>% 
    group_by(key, 
             #gender, 
             religion_cat) %>% 
    summarize(value = sum(value))  %>% 
    mutate(total = sum(value)) %>% 
    mutate(perc = value/total) %>% 
    ungroup() 
  
  end_labs <- religion_viz %>% 
    filter(key == "2018") %>% 
    mutate(perc2 = cumsum(perc)) %>% 
    arrange(desc(perc2))%>% 
    mutate(perc = cumsum(perc) - 0.021) #%>% 
  # mutate(perc = ifelse(characteristic == "Under 14", 0.82, perc))%>%
  # mutate(perc = ifelse(characteristic == "Age 65 and Over", -0.025, perc))
  
  by_year_labs <- religion_viz %>% 
    mutate(perc_label = perc) %>% 
    group_by(key) %>% 
    mutate(perc2 = cumsum(perc)) %>% 
    arrange(desc(perc2)) %>% 
    mutate(perc = cumsum(perc) - 0.01) %>% 
    ungroup() %>% 
    mutate(key = case_when(
      key == 2002 ~ 2002.2,
      key == 2018 ~ 2017.8,
      T ~ key
    ))
  
  religion_viz %>% 
    ggplot(aes(key, perc, fill = religion_cat)) +
    geom_area(position = "fill", alpha = 0.8) +
    #  facet_wrap(~gender, ncol = 2) +
    scale_fill_viridis_d("Gender", begin = 0.2) +
    scale_color_viridis_d("Gender", begin = 0.2) +
    theme_minimal() +
    ggtitle("Refugees arriving in the United States of America by Age Groups (2002 - 2018)\n") +
    labs(x = "", y = "Percentage of Refugees")  +
    scale_x_continuous(breaks = 2002:2018, labels = 2002:2018, 
                       # limits = c(2002, 2020.5),
                       minor_breaks = seq(2002, 2018, 1)) + 
    scale_y_continuous(label = scales::percent) +
    geom_text(data = end_labs, aes(y = perc,
                                   x = 2018,
                                   label = religion_cat, 
                                   color = religion_cat), 
              direction = "x",
              hjust = 0, nudge_x = 0.1) +
    guides(fill = F, color = F) +
    geom_hline(yintercept = 0.5, 
               linetype = "dashed", 
               alpha = 0.15) +
    geom_text(data = by_year_labs, aes(label = round(100*perc_label)), color = "black", size = 2.8, nudge_y = -0.01) +
    coord_cartesian(clip = 'off') +
    theme(plot.margin = unit(c(1,4,1,1), "lines"))
}


plot_educ_viz <- function() {
  education_dataset <- tidytemplate::load_it("data/education_dataset.Rdata")
  
  education_viz <- education_dataset %>% 
    mutate(education = case_when(
      str_detect(education, "Bio Data") ~ "Unknown",
      str_detect(education, "Professional") ~ "Tech. School & Prof.",
      str_detect(education, "Tech") ~ "Tech. School & Prof.",    
      str_detect(education, "Intermediate") ~ "Secondary",
      str_detect(education, "Pre-") ~ "(Pre-)University",
      str_detect(education, "University") ~ "(Pre-)University",
      str_detect(education, "Kindergarten") ~ "None",
      str_detect(education, "NONE") ~ "None",
      str_detect(education, "Graduate School") ~ "(Pre-)University",
      T ~ education
    )) %>% 
    mutate(education = forcats::fct_relevel(education, c("Unknown", "None", "Kindergarten", "Primary", "Secondary", "Tech. School & Professional", "(Pre-)University"))) %>% 
    filter(gender %in% c("M","F")) %>% 
    group_by(key, 
             #gender, 
             education) %>% 
    summarize(value = sum(value))  %>% 
    mutate(total = sum(value)) %>% 
    mutate(perc = value/total) %>% 
    ungroup() 
  
  end_labs <- education_viz %>% 
    filter(key == "2018") %>% 
    mutate(perc2 = cumsum(perc)) %>% 
    arrange(desc(perc2))%>% 
    mutate(perc = cumsum(perc) - 0.021) %>% 
    mutate(perc = ifelse(education == "Primary", 0.55, perc))%>% 
    mutate(perc = ifelse(education == "Secondary", 0.28, perc))
  
  by_year_labs <- education_viz %>% 
    mutate(perc_label = perc) %>% 
    group_by(key) %>% 
    mutate(perc2 = cumsum(perc)) %>% 
    arrange(desc(perc2)) %>% 
    mutate(perc = cumsum(perc) - 0.01) %>% 
    ungroup() %>% 
    mutate(key = case_when(
      key == 2002 ~ 2002.2,
      key == 2018 ~ 2017.8,
      T ~ key
    ))
  
  education_viz %>% 
    ggplot(aes(key, perc, fill = education)) +
    geom_area(position = "fill", alpha = 0.8) +
    #  facet_wrap(~gender, ncol = 2) +
    scale_fill_viridis_d("Gender", begin = 0.2) +
    scale_color_viridis_d("Gender", begin = 0.2) +
    theme_minimal() +
    ggtitle("Refugees arriving in the United States of America by Education Levels (2002 - 2018)\n") +
    labs(x = "", y = "Percentage of Refugees")  +
    scale_x_continuous(breaks = 2002:2018, labels = 2002:2018, 
                       limits = c(2002, 2020.5),
                       minor_breaks = seq(2002, 2018, 1)) + 
    scale_y_continuous(label = scales::percent) +
    geom_text(data = end_labs, aes(y = perc,
                                   x = 2018,
                                   label = education, 
                                   color = education), 
              direction = "x",
              hjust = 0, nudge_x = 0.1) +
    guides(fill = F, color = F) +
    geom_hline(yintercept = 0.5, 
               linetype = "dashed", 
               alpha = 0.35) +
    geom_text(data = by_year_labs, aes(label = round(100*perc_label)), color = "black", size = 2.8, nudge_y = -0.01)
}
