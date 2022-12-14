


# load-packages

library(emmeans)    # for emmeans
library(lme4) 
library(flextable)  # for tables with .rtf output
library(psych)      # for ICC
library(magrittr)
library(rmarkdown)  # For pandoc_version()
library(Hmisc)      # for label()
library(here)       # for here()
library(devtools)   # for session_info()
# library(TFCBTLS)    # for git_report()

library(kableExtra) # for pretty tables
library(tidyquant)  # for date conversions - zoo()

library(pastecs)    # for stat.desc
library(tidyverse)

library(readxl)     # For reading in .xlsx and .xsl files, without using Java.
# library(xlsx)       # to import from Excel (cohort dates)


library(janitor)    # for the tabyl command  (nice tables)

library(stringr)    # string work, such as st_detect()

require(gridExtra)  # for printing a grid of plots.

library(broom)      # both of these are for getting nice output
library("tidyr")    # from linear mixed models.

# library("cstatpower")
library("ggplot2")
library("effectsize")
library(hrbrthemes)  # for theme_ipsum()


library(foreign)    # for importing SPSS data sets.
library(robustbase) # for robust MH distances
library(rrcov)

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


#### Set up the COVID and Other Respiratory Rate data:     ####

set.seed(1234567890)

n_rows=nrow(ER_df)

ER_df <- ER_df %>% 
  dplyr::mutate(
    random_other = rpois(n=n_rows, lambda=1),
    random_COVID = rpois(n=n_rows, lambda=2),
    Admissions_Other = Month_effect + Quarter_effect + 
    random_other,
    Admission_COVID = ifelse(Period == "Pre-COVID",0,
      Month_effect + random_COVID)
    )

# View the data set:

View(ER_df)


# Trim the data to needed variables:

ER_df <- ER_df %>% dplyr::select(
  Calendar_Date, Period, Quarter_num, Month_num,
  Admission_COVID,Admissions_Other 
)


View(ER_df)
 
#### Pivot the data                     ####

ER_Long_df <- ER_df %>%  pivot_longer(!c(Calendar_Date, Period, Quarter_num, Month_num),
             names_to = "Cause", values_to = "Count")

View(ER_Long_df)


#### Save the data set.                 ####

save(ER_Long_df, 
     file = "ER_Long_df.Rdata")


####    Plots for the ER data:                        ####





load("ER_Long_df.Rdata")

ER_Long_df %>% 
  ggplot(.,aes(x=Calendar_Date, y=Count,  weight= Count)) +
  geom_point() 



ER_Long_df %>% 
  ggplot(.,aes(x=Calendar_Date, y=Count,  weight= Count,
               color = Period)) +
  geom_point() 





ER_Long_df %>% 
  ggplot(.,aes(x=Calendar_Date, y=Count,  weight= Count,
               color = Period)) +
  geom_point() +
  facet_wrap(~Cause, nrow =  1)



ER_Long_df %>% 
  ggplot(.,aes(x=Calendar_Date, y=Count,  weight= Count,
               color = Period)) +
  geom_point() +
  facet_wrap(~Cause, nrow = 2)



ER_Long_df %>% 
  ggplot(.,aes(x=Calendar_Date, y=Count,  weight= Count,
               color = Period)) +
  geom_point() +
  facet_wrap(~Cause, nrow = 2)+
  ggtitle("ER Visit Counts by Week, COVID/All Others, Pre-/Post-COVID") +
  ylab("Weekly Admission Rate") +
  xlab("ED Arrival Date (Week)") +
  theme(axis.text.x = element_text(angle = 90)) +
  #   theme(legend.position = c(0.2, 0.2))  +
  scale_x_date(date_breaks = "months")

 


ER_Long_df %>% 
  ggplot(.,aes(x=Calendar_Date, y=Count,  weight= Count,
               color = Period)) +
  geom_point() + 
#  geom_smooth() +
  facet_wrap(~Cause, nrow = 2)+
  ggtitle("ER Visit Counts by Week, COVID/All Others, Pre-/Post-COVID") +
  ylab("Weekly Admission Rate") +
  xlab("ED Arrival Date (Week)") +
  theme(axis.text.x = element_text(angle = 90)) +
  #   theme(legend.position = c(0.2, 0.2))  +
  scale_x_date(date_breaks = "months") 














#### Histogram                       ####

# from https://r-graph-gallery.com/220-basic-ggplot2-histogram.html
# Data 



histogram_data <- data.frame(value=rnorm(100),
  group = sample(c("Group 1", "Group 2")))

View(histogram_data)



histogram_data %>% ggplot(., aes(x=value)) +
  geom_histogram()


histogram_data %>% ggplot(., aes(x=value)) +
  geom_histogram() +
  facet_wrap(~group)

group1 <- histogram_data %>% dplyr::filter(group == "Group 1") %>% dplyr::select(value) %>% rename(group1 = value)
group2 <- histogram_data %>% dplyr::filter(group == "Group 2") %>% dplyr::select(value) %>% rename(group2 = value)

wide_histogram_data <- cbind(group1, group2) 

View(wide_histogram_data)



