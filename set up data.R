

### Create Data for simulated Pre-/Post-COVID study:

#### Create raw vectors:                                     ####
month_sequence_raw <- seq(ymd('2019-03-01'),
                          ymd('2021-02-28'),
                          by='weeks')




Calendar_Date <- rep(month_sequence_raw,100)

Period <- ifelse(Calendar_Date < ymd('2020-03-01'),
                 "Pre-COVID","Post=COVID")

table(Calendar_Date,Period_raw, useNA = 'always')

####      Form data set:                                     ####

ER_df <- cbind(as.character(Calendar_Date),Period) %>% 
  as.data.frame() %>% 
  dplyr::mutate(Calendar_Date = as.Date(Calendar_Date)) %>% 
  dplyr::mutate(Quarter_num=quarter(Calendar_Date)) %>% 
  dplyr::mutate(Month_num=as.integer(month(Calendar_Date))) %>% 
  dplyr::select(Calendar_Date, Period, Month_num, Quarter_num)

lapply(ER_df, class)



####  Set up outcome variables                               ####



set.seed(1234567890)

