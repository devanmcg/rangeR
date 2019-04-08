    # Fetch survey data
    gs_auth(new_user = TRUE)
    (my_sheets <- gs_ls()) # View available sheets
    gs_title("survey responses") %>%
               gs_read(verbose = FALSE)  %>% 
      mutate(Timestamp = as.POSIXct(Timestamp, format="%m/%d/%Y %H:%M:%S"), 
             Year = lubridate::year(Timestamp), 
		 Month = lubridate::month(Timestamp)) %>%
      filter(Year == "2019" ) %>%
      select(-Timestamp, -Year, -Month) -> survey.d

names(survey.d) 
      colnames(survey.d) <- c("degree", "program", "USundergrad", "water", 
                              "relationship", "gender", "UndergradWhere", 
					 "INTundergrad", "moist", "previous")
survey.d %>% select(-gender, -UndergradWhere, -previous) -> survey.d

write.csv(survey.d, file="./data/SurveyResponsesSP19.csv", row.names=FALSE)