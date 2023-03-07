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

Joining the data as well as dropping the dropouts noted earlier

``` r
eda_df = 
  left_join(asa24_df, visits_df, by = c("id", "reporting_date"))

eda_df = 
  left_join(eda_df, completion_df, by = "id") %>% 
  filter(is.na(completedyesno) | !completedyesno == 0) %>% 
  relocate(id, reporting_date, group, visit)
```

### Visualization

``` r
skimr::skim(eda_df)
```

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | eda\_df |
| Number of rows                                   | 1171    |
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
| user\_name       |          0 |              1 |   9 |  11 |     0 |       131 |          0 |
| registration\_id |          0 |              1 |   3 |   6 |     0 |        97 |          0 |

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                            |
|:---------------|-----------:|---------------:|:--------|----------:|:---------------------------------------|
| group          |        148 |           0.87 | FALSE   |         4 | Gro: 285, Gro: 270, Gro: 265, Gro: 203 |
| visit          |        148 |           0.87 | FALSE   |         4 | Wee: 274, Wee: 270, Wee: 249, Wee: 230 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |     sd |    p0 |     p25 |     p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|--------:|-------:|------:|--------:|--------:|--------:|--------:|:------|
| id             |          0 |           1.00 |   56.02 |  32.71 |  1.00 |   28.00 |   54.00 |   87.00 |  115.00 | ▇▇▇▆▇ |
| recall\_no     |          0 |           1.00 |    5.96 |   3.55 |  1.00 |    3.00 |    6.00 |    9.00 |   16.00 | ▇▅▅▂▁ |
| f\_total       |          0 |           1.00 |    0.76 |   1.00 |  0.00 |    0.00 |    0.31 |    1.27 |    6.40 | ▇▂▁▁▁ |
| v\_total       |          0 |           1.00 |    1.48 |   1.27 |  0.00 |    0.52 |    1.22 |    2.09 |    8.90 | ▇▃▁▁▁ |
| v\_legumes     |          0 |           1.00 |    0.14 |   0.37 |  0.00 |    0.00 |    0.00 |    0.00 |    3.21 | ▇▁▁▁▁ |
| g\_whole       |          0 |           1.00 |    0.73 |   1.20 |  0.00 |    0.00 |    0.00 |    1.13 |   15.22 | ▇▁▁▁▁ |
| pf\_poult      |          0 |           1.00 |    2.45 |   3.23 |  0.00 |    0.00 |    1.19 |    4.19 |   19.81 | ▇▂▁▁▁ |
| pf\_seafd\_hi  |          0 |           1.00 |    0.95 |   2.52 |  0.00 |    0.00 |    0.00 |    0.00 |   15.69 | ▇▁▁▁▁ |
| pf\_seafd\_low |          0 |           1.00 |    0.67 |   1.97 |  0.00 |    0.00 |    0.00 |    0.00 |   18.88 | ▇▁▁▁▁ |
| pf\_nutsds     |          0 |           1.00 |    0.77 |   2.11 |  0.00 |    0.00 |    0.00 |    0.57 |   33.98 | ▇▁▁▁▁ |
| pf\_legumes    |          0 |           1.00 |    0.57 |   1.48 |  0.00 |    0.00 |    0.00 |    0.00 |   12.75 | ▇▁▁▁▁ |
| d\_yogurt      |          0 |           1.00 |    0.07 |   0.22 |  0.00 |    0.00 |    0.00 |    0.00 |    2.01 | ▇▁▁▁▁ |
| g\_refined     |          0 |           1.00 |    4.22 |   3.60 |  0.00 |    1.72 |    3.61 |    5.74 |   34.93 | ▇▂▁▁▁ |
| pf\_meat       |          0 |           1.00 |    1.82 |   3.36 |  0.00 |    0.00 |    0.00 |    2.33 |   27.70 | ▇▁▁▁▁ |
| pf\_curedmeat  |          0 |           1.00 |    0.83 |   1.68 |  0.00 |    0.00 |    0.00 |    0.99 |   13.37 | ▇▁▁▁▁ |
| solid\_fats    |          0 |           1.00 |   24.46 |  20.07 |  0.00 |    9.24 |   20.00 |   34.63 |  146.41 | ▇▃▁▁▁ |
| add\_sugars    |          0 |           1.00 |    6.92 |   7.49 |  0.00 |    1.35 |    4.48 |    9.82 |   59.24 | ▇▂▁▁▁ |
| kcal           |          0 |           1.00 | 1709.67 | 810.92 | 50.02 | 1166.83 | 1572.61 | 2123.94 | 7040.47 | ▇▇▁▁▁ |
| prot           |          0 |           1.00 |   91.74 |  46.59 |  0.04 |   58.15 |   84.37 |  116.17 |  371.85 | ▆▇▂▁▁ |
| tfat           |          0 |           1.00 |   70.37 |  39.86 |  0.00 |   41.29 |   63.59 |   89.68 |  305.87 | ▇▇▁▁▁ |
| carb           |          0 |           1.00 |  171.11 |  95.16 |  0.71 |  106.29 |  156.95 |  219.94 |  871.83 | ▇▅▁▁▁ |
| sugr           |          0 |           1.00 |   56.64 |  42.94 |  0.00 |   24.89 |   47.16 |   79.25 |  336.41 | ▇▃▁▁▁ |
| fibe           |          0 |           1.00 |   15.33 |   9.84 |  0.00 |    8.44 |   13.11 |   20.51 |   73.23 | ▇▆▁▁▁ |
| sfat           |          0 |           1.00 |   21.28 |  13.34 |  0.00 |   11.69 |   18.70 |   27.87 |  103.69 | ▇▅▁▁▁ |
| chole          |          0 |           1.00 |  371.54 | 258.47 |  0.00 |  170.84 |  315.29 |  521.18 | 1952.00 | ▇▅▁▁▁ |
| completedyesno |         29 |           0.98 |    1.00 |   0.00 |  1.00 |    1.00 |    1.00 |    1.00 |    1.00 | ▁▁▇▁▁ |

**Variable type: POSIXct**

| skim\_variable            | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:--------------------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| reporting\_date           |          0 |              1 | 2020-01-07 00:00:00 | 2022-11-27 00:00:00 | 2021-06-16 00:00:00 |       532 |
| intake\_start\_date\_time |          0 |              1 | 2020-01-06 00:00:00 | 2022-11-26 00:00:00 | 2021-06-15 00:00:00 |       532 |
| intake\_end\_date\_time   |          0 |              1 | 2020-01-06 23:59:00 | 2022-11-26 23:59:00 | 2021-06-15 23:59:00 |       534 |

148 entries missing their visit date (and group). Need to try matching
visit date with intake\_start\_date-time?

-   Absolutely not. Missing increased to 830!
