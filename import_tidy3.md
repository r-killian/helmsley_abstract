Data Import and Tidy
================
Rose Killian
3/6/2023

##### Load libraries and settings

``` r
library(tidyverse)
library(viridis)
library(readxl)
library(lubridate)
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)
theme_set(theme_minimal() + theme(legend.position = "bottom"))
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

##### Version notes

Discrepancy with IDs 299 and 300 resolved. Both are aliases for ID 10.
These observations can now be assigned to their group and will no longer
be dropped.

I was also given data from REDcap that lists the dates of each patient
visit and the week that that visit fell on.

### Import

#### ASA24 Data

``` r
asa24_df = 
  read_excel(
    "data/TOTAL ASA24 DATA - Luis healthy and unhealthy foods.xlsx",
    sheet = "HelmCroh + HelmCr") %>%
  janitor::clean_names() %>% 
  select(
    user_name, recall_no, intake_start_date_time, intake_end_date_time, reporting_date, f_total, v_total, v_legumes, g_whole, pf_poult, pf_seafd_hi, pf_seafd_low, pf_nutsds, pf_legumes, d_yogurt, g_refined, pf_meat, pf_curedmeat, solid_fats, add_sugars, kcal, prot, tfat, carb, sugr, fibe, sfat, chole
  ) %>% 
mutate(
  id = str_replace(user_name, "HelmCroh", ""))

#R gets mad if I try to remove the different username prefixes in the same step. I'm sure there is a more elegant way to do this but this works so for now this is how it is. Code above imports, cleans names, and removes the first prefix used for user_name and makes new variable "id". Code below removes second prefix and converts vector to numeric.

asa24_df = 
  asa24_df %>% 
  mutate(id = str_replace(id, "HelmCr", "")) %>% 
  mutate(
    id = as.numeric(id)) %>% 
  relocate(id) %>% 
  arrange(id, reporting_date)
```

#### Visits data

``` r
visits_df = 
  read_excel(
    "data/AHolisticDietInterve_DATA_2023-03-06_1029.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(
    group = as_factor(registration_arm_1_ramdom),
    group = recode_factor(group, `1` = "Group 1", `2` = "Group 2", `3` = "Group 3P", `4` = "Group 3C")) %>% 
  rename(
    `1a` = visit_1_screening_arm_1_asa24_weekdate,
    `1b` = visit_1_screening_arm_1_dateasa24_weekday2,
    `1c` = visit_1_screening_arm_1_asa24_weekenddate,
    `2a` = week_12_ibdzoom_5_arm_1_asa24_weekdate,
    `2b` = week_12_ibdzoom_5_arm_1_dateasa24_weekday2,
    `2c` = week_12_ibdzoom_5_arm_1_asa24_weekenddate,
    `3a` = week_24_ibd_zoom_7_arm_1_asa24_weekdate,
    `3b` = week_24_ibd_zoom_7_arm_1_dateasa24_weekday2,
    `3c` = week_24_ibd_zoom_7_arm_1_asa24_weekenddate,
    `4a` = visit_3_last_ibds_arm_1_asa24_weekdate,
    `4b` = visit_3_last_ibds_arm_1_dateasa24_weekday2,
    `4c` = visit_3_last_ibds_arm_1_asa24_weekenddate,
    id = registration_arm_1_internal_pt_id
  ) %>% 
  select(
    id, group, `1a`, `1b`, `1c`, `2a`, `2b`, `2c`, `3a`, `3b`, `3c`, `4a`, `4b`, `4c`
  ) %>% 
  pivot_longer(`1a`:`4c`,
               names_to = "visit",
               values_to = "reporting_date",
               names_pattern = "(.*)[a-c]") %>% 
  mutate(
    visit = as_factor(visit),
    visit = recode_factor(visit, `1` = "Week 0", `2` = "Week 12", `3` = "Week 24", `4` = "Week 36")
  ) %>% 
  relocate(id, reporting_date) %>% 
  arrange(id, reporting_date) %>% 
  distinct()
```

#### Completion data

``` r
completion_df = 
  read_csv(
    "data/AHolisticDietInterve_DATA_2023-03-06_1242.csv") %>% 
      janitor::clean_names() %>% 
  rename(id = registration_arm_1_internal_pt_id,
         completedyesno = registration_arm_1_completedyesno)
```

    ## Rows: 115 Columns: 3
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): registration_id
    ## dbl (2): [registration_arm_1][internal_pt_id], [registration_arm_1][complete...
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
completion_df %>% 
  group_by(completedyesno) %>% 
  summarise(n_obs = n()) %>% 
  knitr::kable()
```

| completedyesno | n\_obs |
|---------------:|-------:|
|              0 |     18 |
|              1 |     93 |
|             NA |      4 |

Looking at the variable for study completion, we have 93 patients that
have completed the study, 18 that have been marked as dropped
out/incomplete, and 4 who are missing any designation (possibly still in
process of completing the study). Patients marked as dropped out will be
dropped from the data.

##### Table of Dropouts

``` r
completion_df %>% 
  filter(completedyesno == 0) %>% 
  select(id, completedyesno) %>% 
  knitr::kable()
```

|  id | completedyesno |
|----:|---------------:|
|   6 |              0 |
|   8 |              0 |
|  21 |              0 |
|  35 |              0 |
|  42 |              0 |
|  62 |              0 |
|  63 |              0 |
|  68 |              0 |
|  69 |              0 |
|  71 |              0 |
|  73 |              0 |
|  75 |              0 |
|  78 |              0 |
|  79 |              0 |
|  85 |              0 |
|  86 |              0 |
| 102 |              0 |
| 112 |              0 |

``` r
#start_completion =
#  start_completion %>% 
#  filter(is.na(completedyesno) | !completedyesno == 0)

#This code was used to make sure that only the participants marked as having dropped out were dropped from the dataset.
```

### Join Datasets

The dates from the ASA24 data and the redcap data do not always match up

``` r
#find out where dates mismatch
mismatch_df = 
full_join(asa24_df, visits_df, keep = TRUE, suffix = c('.asa', '.redcap'), by = c('id', 'reporting_date')) %>% 
  select(id.asa, id.redcap, reporting_date.asa, reporting_date.redcap, visit)

# mismatch_df %>% 
#   filter(is.na(reporting_date.asa) | is.na(reporting_date.redcap)) %>% 
#   mutate(
#     id = coalesce(id.asa, id.redcap)) %>% 
#   select(id, reporting_date.asa, reporting_date.redcap, visit) %>% 
#   arrange(id) %>% 
#   view()

#try again but after removing dropped participants

dropped = c(6 ,8, 21, 35, 42, 62, 63, 68, 69, 71, 73, 75, 78, 79, 85, 86, 102, 112)

mismatch_df = 
  mismatch_df %>%
    filter(is.na(reporting_date.asa) | is.na(reporting_date.redcap)) %>% 
    mutate(
      id = coalesce(id.asa, id.redcap)) %>% 
    select(id, reporting_date.asa, reporting_date.redcap, visit) %>%
    group_by(id) %>%
    filter(!any(id %in% dropped)) %>% 
    arrange(id)

length(mismatch_df$reporting_date.asa) - length(na.omit(mismatch_df$reporting_date.asa))
```

    ## [1] 121

``` r
length(mismatch_df$reporting_date.redcap) - length(na.omit(mismatch_df$reporting_date.redcap))
```

    ## [1] 166

121 missing from asa24 and 125 missing from redcap

``` r
mismatch_df
```

    ## # A tibble: 269 x 4
    ## # Groups:   id [63]
    ##       id reporting_date.asa  reporting_date.redcap visit  
    ##    <dbl> <dttm>              <dttm>                <fct>  
    ##  1     1 2020-01-07 00:00:00 NA                    <NA>   
    ##  2     1 2020-01-10 00:00:00 NA                    <NA>   
    ##  3     1 2020-01-12 00:00:00 NA                    <NA>   
    ##  4     1 2020-06-30 00:00:00 NA                    <NA>   
    ##  5     1 NA                  2020-01-06 00:00:00   Week 0 
    ##  6     1 NA                  2020-01-09 00:00:00   Week 0 
    ##  7     1 NA                  2020-01-11 00:00:00   Week 0 
    ##  8     1 NA                  2020-06-02 00:00:00   Week 24
    ##  9     3 2020-01-07 00:00:00 NA                    <NA>   
    ## 10     3 2020-01-10 00:00:00 NA                    <NA>   
    ## # ... with 259 more rows

``` r
#Wrote out .csv of problem dates
#write_excel_csv(mismatch_df, "data/dates_mismatch.csv")
```

Joining the data as well as dropping the dropouts noted earlier as well
as incomplete study participants

``` r
eda_df = 
  left_join(asa24_df, visits_df, by = c("id", "reporting_date"))

eda_df = 
  left_join(eda_df, completion_df, by = "id") %>% 
  filter(!completedyesno == 0) %>% 
  relocate(id, reporting_date, group, visit)
```

### Visualization

``` r
skimr::skim(eda_df)
```

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | eda\_df |
| Number of rows                                   | 1142    |
| Number of columns                                | 33      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| character                                        | 2       |
| factor                                           | 2       |
| numeric                                          | 26      |
| POSIXct                                          | 3       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

Data summary

**Variable type: character**

| skim\_variable   | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:-----------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| user\_name       |          0 |              1 |   9 |  11 |     0 |       127 |          0 |
| registration\_id |          0 |              1 |   3 |   6 |     0 |        93 |          0 |

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                            |
|:---------------|-----------:|---------------:|:--------|----------:|:---------------------------------------|
| group          |        137 |           0.88 | FALSE   |         4 | Gro: 279, Gro: 270, Gro: 265, Gro: 191 |
| visit          |        137 |           0.88 | FALSE   |         4 | Wee: 268, Wee: 258, Wee: 249, Wee: 230 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |     sd |    p0 |     p25 |     p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|--------:|-------:|------:|--------:|--------:|--------:|--------:|:------|
| id             |          0 |              1 |   54.57 |  31.81 |  1.00 |   27.00 |   52.50 |   83.00 |  110.00 | ▇▇▇▆▇ |
| recall\_no     |          0 |              1 |    5.99 |   3.56 |  1.00 |    3.00 |    6.00 |    9.00 |   16.00 | ▇▅▅▂▁ |
| f\_total       |          0 |              1 |    0.78 |   1.01 |  0.00 |    0.00 |    0.36 |    1.28 |    6.40 | ▇▂▁▁▁ |
| v\_total       |          0 |              1 |    1.47 |   1.27 |  0.00 |    0.52 |    1.20 |    2.08 |    8.90 | ▇▃▁▁▁ |
| v\_legumes     |          0 |              1 |    0.14 |   0.37 |  0.00 |    0.00 |    0.00 |    0.00 |    3.21 | ▇▁▁▁▁ |
| g\_whole       |          0 |              1 |    0.73 |   1.21 |  0.00 |    0.00 |    0.00 |    1.14 |   15.22 | ▇▁▁▁▁ |
| pf\_poult      |          0 |              1 |    2.46 |   3.25 |  0.00 |    0.00 |    1.19 |    4.19 |   19.81 | ▇▂▁▁▁ |
| pf\_seafd\_hi  |          0 |              1 |    0.96 |   2.54 |  0.00 |    0.00 |    0.00 |    0.00 |   15.69 | ▇▁▁▁▁ |
| pf\_seafd\_low |          0 |              1 |    0.66 |   1.92 |  0.00 |    0.00 |    0.00 |    0.00 |   18.88 | ▇▁▁▁▁ |
| pf\_nutsds     |          0 |              1 |    0.78 |   2.13 |  0.00 |    0.00 |    0.00 |    0.57 |   33.98 | ▇▁▁▁▁ |
| pf\_legumes    |          0 |              1 |    0.57 |   1.48 |  0.00 |    0.00 |    0.00 |    0.00 |   12.75 | ▇▁▁▁▁ |
| d\_yogurt      |          0 |              1 |    0.07 |   0.22 |  0.00 |    0.00 |    0.00 |    0.00 |    2.01 | ▇▁▁▁▁ |
| g\_refined     |          0 |              1 |    4.20 |   3.59 |  0.00 |    1.72 |    3.60 |    5.70 |   34.93 | ▇▁▁▁▁ |
| pf\_meat       |          0 |              1 |    1.84 |   3.39 |  0.00 |    0.00 |    0.00 |    2.34 |   27.70 | ▇▁▁▁▁ |
| pf\_curedmeat  |          0 |              1 |    0.83 |   1.69 |  0.00 |    0.00 |    0.00 |    0.99 |   13.37 | ▇▁▁▁▁ |
| solid\_fats    |          0 |              1 |   24.33 |  19.89 |  0.00 |    8.98 |   19.80 |   34.33 |  146.41 | ▇▃▁▁▁ |
| add\_sugars    |          0 |              1 |    6.97 |   7.54 |  0.00 |    1.42 |    4.48 |   10.15 |   59.24 | ▇▂▁▁▁ |
| kcal           |          0 |              1 | 1707.57 | 814.51 | 50.02 | 1165.38 | 1571.53 | 2118.04 | 7040.47 | ▇▇▁▁▁ |
| prot           |          0 |              1 |   91.81 |  46.83 |  0.04 |   57.84 |   84.34 |  116.13 |  371.85 | ▆▇▂▁▁ |
| tfat           |          0 |              1 |   70.15 |  39.94 |  0.00 |   40.98 |   63.32 |   88.68 |  305.87 | ▇▇▁▁▁ |
| carb           |          0 |              1 |  171.51 |  95.69 |  0.71 |  106.32 |  157.00 |  220.40 |  871.83 | ▇▅▁▁▁ |
| sugr           |          0 |              1 |   57.02 |  43.26 |  0.00 |   24.87 |   47.53 |   80.02 |  336.41 | ▇▃▁▁▁ |
| fibe           |          0 |              1 |   15.30 |   9.85 |  0.00 |    8.42 |   13.11 |   20.49 |   73.23 | ▇▆▁▁▁ |
| sfat           |          0 |              1 |   21.19 |  13.27 |  0.00 |   11.58 |   18.57 |   27.64 |  103.69 | ▇▅▁▁▁ |
| chole          |          0 |              1 |  370.90 | 257.05 |  0.00 |  171.40 |  315.30 |  516.89 | 1952.00 | ▇▅▁▁▁ |
| completedyesno |          0 |              1 |    1.00 |   0.00 |  1.00 |    1.00 |    1.00 |    1.00 |    1.00 | ▁▁▇▁▁ |

**Variable type: POSIXct**

| skim\_variable            | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:--------------------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| reporting\_date           |          0 |              1 | 2020-01-07 00:00:00 | 2022-11-23 00:00:00 | 2021-06-07 00:00:00 |       520 |
| intake\_start\_date\_time |          0 |              1 | 2020-01-06 00:00:00 | 2022-11-22 00:00:00 | 2021-06-06 00:00:00 |       520 |
| intake\_end\_date\_time   |          0 |              1 | 2020-01-06 23:59:00 | 2022-11-22 23:59:00 | 2021-06-06 23:59:00 |       522 |

148 entries missing their visit date (and group). Need to try matching
visit date with intake\_start\_date-time?

-   Absolutely not. Missing increased to 830!
