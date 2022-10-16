

### Create Data for simulated Pre-\Post-COVID study:

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
  dplyr::select(Calendar_Date, Period, Month_num, Quarter_num) %>% 
  dplyr::mutate(Quarter_effect = case_when(
    Quarter_num == 1 ~ as.integer(2),
    Quarter_num == 2 ~ as.integer(1),
    Quarter_num == 3 ~ as.integer(0),
    Quarter_num == 4 ~ as.integer(0),
    TRUE ~NA_integer_  ),
    Month_effect = case_when(
      Month_num == 1 ~ as.integer(5),
      Month_num == 2 ~ as.integer(5),
      Month_num == 3 ~ as.integer(5),
      Month_num == 4 ~ as.integer(4),
      Month_num == 5 ~ as.integer(2),
      Month_num == 6 ~ as.integer(3),
      Month_num == 7 ~ as.integer(5),
      Month_num == 8 ~ as.integer(5),
      Month_num == 9 ~ as.integer(2),
      Month_num == 10 ~ as.integer(2),
      Month_num == 11 ~ as.integer(4),
      Month_num == 12 ~ as.integer(4),
      TRUE ~NA_integer_  )
  )


lapply(ER_df, class)

with(ER_df, table(Month_num,ER_df$Month_effect, 
                  useNA='always'))

hist(ER_df$Month_effect)


ggplot(ER_df) +
  aes(x=as.factor(Month_num), y=Month_effect) +
  geom_bar(stat = "identity")

ggplot(ER_df) +
  aes(x=as.factor(Month_num), y=Month_effect) +
  geom_bar(stat = "summary", fun.y = "mean")

#### Set up the COVID and Other Respiratory Rate data:     ####

set.seed(1234567890)

n_rows=nrow(ER_df)

ER_df <- ER_df %>% 
  dplyr::mutate(
    random_other = rpois(n=n_rows, lambda=10),
    random_COVID = rpois(n=n_rows, lambda=30),
    Admissions_Other = Month_effect + Quarter_effect + 
    random_other,
    Admission_COVID = ifelse(Period == "Pre-COVID",0,
      Month_effect + random_COVID)
    )

####  Plot the data                               ####









#### Save the data set.                 ####

save(ER_df, 
     file = "COVID_Mock_Data.Rdata")
