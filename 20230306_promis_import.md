PROMIS Import
================
Rosie Killian
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

### Promise Data

Importing promise data.

``` r
prom_df = 
  read_excel(
    "data/Promise more data.xlsx") %>%
  janitor::clean_names() %>% 
  select(!ends_with("std_error")) %>% 
  filter(!grepl('[Oo]ut', registration_id)) %>% 
  rename(
      anxiety_visit1 = visit_1_screening_arm_1_promis_sf_v10anxiety_4a_tscore,
      anxiety_visit2 = visit_2_ibdsdtzoo_arm_1_promis_sf_v10anxiety_4a_tscore,
      anxiety_visit3 = visit_3_last_ibds_arm_1_promis_sf_v10anxiety_4a_tscore,
      depression_visit1 = visit_1_screening_arm_1_promis_sf_v10depression_4a_tscore,
      depression_visit2 = visit_2_ibdsdtzoo_arm_1_promis_sf_v10depression_4a_tscore,
      depression_visit3 =  visit_3_last_ibds_arm_1_promis_sf_v10depression_4a_tscore,
      fatigue_visit1 = visit_1_screening_arm_1_promis_sf_v10fatigue_4a_tscore,
      fatigue_visit2 = visit_2_ibdsdtzoo_arm_1_promis_sf_v10fatigue_4a_tscore,
      fatigue_visit3 = visit_3_last_ibds_arm_1_promis_sf_v10fatigue_4a_tscore,
      sleepdisturbance_visit1 = visit_1_screening_arm_1_promis_sf_v10sleep_disturbance_4a_tscore,
      sleepdisturbance_visit2 = visit_2_ibdsdtzoo_arm_1_promis_sf_v10sleep_disturbance_4a_tscore,
      sleepdisturbance_visit3 = visit_3_last_ibds_arm_1_promis_sf_v10sleep_disturbance_4a_tscore,
      abilitytoparticipatesocial_visit1 = visit_1_screening_arm_1_promis_sf_v20_ability_to_participate_social_4a_tscore,
      abilitytoparticipatesocial_visit2 = visit_2_ibdsdtzoo_arm_1_promis_sf_v20_ability_to_participate_social_4a_tscore,
      abilitytoparticipatesocial_visit3 = visit_3_last_ibds_arm_1_promis_sf_v20_ability_to_participate_social_4a_tscore,
      paininterference_visit1 = visit_1_screening_arm_1_promis_sf_v10pain_interference_4a_tscore,
      paininterference_visit2 = visit_2_ibdsdtzoo_arm_1_promis_sf_v10pain_interference_4a_tscore,
      paininterference_visit3 = visit_3_last_ibds_arm_1_promis_sf_v10pain_interference_4a_tscore,
      emotionalsupport_visit1 = visit_1_screening_arm_1_promis_sf_v20_emotional_support_4a_tscore,
      emotionalsupport_visit2 = visit_2_ibdsdtzoo_arm_1_promis_sf_v20_emotional_support_4a_tscore,
      emotionalsupport_visit3 = visit_3_last_ibds_arm_1_promis_sf_v20_emotional_support_4a_tscore,
      physicalfunction_visit3 = visit_3_last_ibds_arm_1_promis_sf_v10physical_function_4a_tscore,
      hispanic = visit_1_screening_arm_1_hispanic2_1_yes_2_no_3_unknown_or_not_reported,
      individualnumber = registration_arm_1_individual_number_100_partner_101_sibling_1001_mother,
      familynumber = registration_arm_1_family_number,
      sex = registration_arm_1_sex_0_female_1_male_2_unknown,
      groups = registration_arm_1_ramdom
  ) %>% 
  mutate(
    groups = as_factor(groups),
    groups = recode_factor(groups, `1` = "Group 1", `2` = "Group 2", `3` = "Group 3P", `4` = "Group 3C"),
    hispanic = factor(hispanic),
    hispanic = recode_factor(hispanic,`1` = "Yes", `2` = "No", `3` = "Unknown"),
    sex = factor(sex),
    sex = recode_factor(sex,`0` = "Female", `1` = "Male", `2` = "Unknown"),
    individualnumber = factor(individualnumber),
    individualnumber = recode_factor(individualnumber, `100` = "Partner", `101` = "Sibling", `1001` = "Mother")
  )
```

### Pivot longer

``` r
prom_df =
  prom_df %>% 
  relocate(
    registration_id,
    groups,
    age,
    sex,
    hispanic,
    individualnumber,
    familynumber,
    anxiety_visit2,
    anxiety_visit1,
    anxiety_visit3,
    depression_visit1,
    depression_visit2,
    depression_visit3,
    fatigue_visit1,
    fatigue_visit2,
    fatigue_visit3,
    sleepdisturbance_visit1,
    sleepdisturbance_visit2,
    sleepdisturbance_visit3,
    abilitytoparticipatesocial_visit1,
    abilitytoparticipatesocial_visit2,
    abilitytoparticipatesocial_visit3,
    paininterference_visit1,
    paininterference_visit2,
    paininterference_visit3,
    emotionalsupport_visit1,
    emotionalsupport_visit2,
    emotionalsupport_visit3
  )

prom_df =
  prom_df %>% 
    pivot_longer(-c(registration_id, groups, age, sex, hispanic, individualnumber, familynumber),
                 names_to = c(".value", "visit"), 
                 names_sep = "_") %>% 
    mutate(
      visit = factor(visit),
      visit = recode_factor(visit, `visit1` = "Week 0", `visit2` = "Week 8", `visit3` = "Week 36")
      )

prom_df
```

    ## # A tibble: 291 x 16
    ##    registrati~1 groups   age sex   hispa~2 indiv~3 famil~4 visit anxiety depre~5
    ##    <chr>        <fct>  <dbl> <fct> <fct>   <fct>     <dbl> <fct>   <dbl>   <dbl>
    ##  1 1g2          Group~  44.5 Male  Yes     <NA>         NA Week~    47.9    41  
    ##  2 1g2          Group~  44.5 Male  Yes     <NA>         NA Week~    40.3    41  
    ##  3 1g2          Group~  44.5 Male  Yes     <NA>         NA Week~    40.3    41  
    ##  4 2g3p         Group~  25.4 Male  Yes     <NA>         NA Week~    56.4    51.2
    ##  5 2g3p         Group~  25.4 Male  Yes     <NA>         NA Week~    40.3    41  
    ##  6 2g3p         Group~  25.4 Male  Yes     <NA>         NA Week~    40.3    41  
    ##  7 3g3c         Group~  49.1 Fema~ No      Mother        2 Week~    40.3    49.1
    ##  8 3g3c         Group~  49.1 Fema~ No      Mother        2 Week~    49      56.1
    ##  9 3g3c         Group~  49.1 Fema~ No      Mother        2 Week~    49.2    41  
    ## 10 4g3p         Group~  39.5 Fema~ Yes     <NA>         NA Week~    51.4    41  
    ## # ... with 281 more rows, 6 more variables: fatigue <dbl>,
    ## #   sleepdisturbance <dbl>, abilitytoparticipatesocial <dbl>,
    ## #   paininterference <dbl>, emotionalsupport <dbl>, physicalfunction <dbl>, and
    ## #   abbreviated variable names 1: registration_id, 2: hispanic,
    ## #   3: individualnumber, 4: familynumber, 5: depression

``` r
skimr::skim(prom_df)
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | prom\_df |
| Number of rows                                   | 291      |
| Number of columns                                | 16       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| character                                        | 1        |
| factor                                           | 5        |
| numeric                                          | 10       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: character**

| skim\_variable   | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:-----------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| registration\_id |          0 |              1 |   3 |   6 |     0 |        97 |          0 |

**Variable type: factor**

| skim\_variable   | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                        |
|:-----------------|-----------:|---------------:|:--------|----------:|:-----------------------------------|
| groups           |          0 |           1.00 | FALSE   |         4 | Gro: 78, Gro: 72, Gro: 72, Gro: 69 |
| sex              |          0 |           1.00 | FALSE   |         2 | Fem: 153, Mal: 138                 |
| hispanic         |          0 |           1.00 | FALSE   |         3 | Yes: 159, No: 129, Unk: 3          |
| individualnumber |        219 |           0.25 | FALSE   |         3 | Par: 66, Sib: 3, Mot: 3            |
| visit            |          0 |           1.00 | FALSE   |         3 | Wee: 97, Wee: 97, Wee: 97          |

**Variable type: numeric**

| skim\_variable             | n\_missing | complete\_rate |  mean |    sd |    p0 |   p25 |  p50 |   p75 |  p100 | hist  |
|:---------------------------|-----------:|---------------:|------:|------:|------:|------:|-----:|------:|------:|:------|
| age                        |          0 |           1.00 | 37.64 | 12.40 | 20.66 | 27.41 | 35.4 | 42.60 | 68.14 | ▇▆▅▂▂ |
| familynumber               |        219 |           0.25 | 40.67 | 27.35 |  2.00 | 18.50 | 34.0 | 58.50 | 89.00 | ▇▇▅▂▆ |
| anxiety                    |          4 |           0.99 | 48.47 |  8.18 | 40.30 | 40.30 | 48.2 | 54.95 | 69.30 | ▇▃▅▂▁ |
| depression                 |          4 |           0.99 | 45.27 |  6.55 | 41.00 | 41.00 | 41.0 | 49.00 | 65.80 | ▇▁▂▁▁ |
| fatigue                    |          4 |           0.99 | 46.83 | 10.73 | 33.70 | 33.70 | 46.4 | 53.90 | 75.80 | ▇▆▆▂▁ |
| sleepdisturbance           |          4 |           0.99 | 46.92 |  8.13 | 32.00 | 41.20 | 47.2 | 52.80 | 73.30 | ▅▇▇▂▁ |
| abilitytoparticipatesocial |          4 |           0.99 | 57.79 |  7.14 | 27.50 | 51.80 | 58.5 | 64.20 | 64.20 | ▁▁▂▅▇ |
| paininterference           |          4 |           0.99 | 46.76 |  7.31 | 41.60 | 41.60 | 41.6 | 52.40 | 75.60 | ▇▂▂▁▁ |
| emotionalsupport           |         24 |           0.92 | 57.30 |  7.69 | 25.80 | 52.10 | 62.0 | 62.00 | 62.00 | ▁▁▁▂▇ |
| physicalfunction           |        198 |           0.32 | 54.18 |  5.56 | 33.40 | 56.90 | 56.9 | 56.90 | 56.90 | ▁▁▁▁▇ |
