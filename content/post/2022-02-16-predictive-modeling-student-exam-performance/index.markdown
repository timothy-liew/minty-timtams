---
title: 'Predictive Modeling: Student Exam Performance'
author: Timothy Liew
date: '2022-02-16'
slug: predictive-modeling-student-exam-performance
categories: []
tags: [statistics, machine learning, eda]
---

<script src="{{< blogdown/postref >}}index_files/core-js/shim.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/react/react.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/react/react-dom.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/reactwidget/react-tools.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index_files/reactable-binding/reactable.js"></script>

# Introduction

I’ve been musing on the application of machine learning in the context of higher education, especially when it comes to predicting student performance. At the time of this post, we’re still very much enthralled by the grip of COVID-19, and I believe it’s even more incumbent upon institutions to leverage data analytics in enhancing teaching practices and student performance.

To that end, I decided to dabble a bit in some predictive modeling of exam performance. The dataset we’ll be using is a generated (i.e., fictional) data that captures the marks obtained by students across three domains: reading, writing, and math scores.

<center>

![](https://i.pinimg.com/originals/39/79/a6/3979a63b82c9927aa015eb13fb1ff22a.png)

</center>

-   I downloaded the dataset from Kaggle, but the original source can be found on [the Royce Kimmons website](http://roycekimmons.com/tools/generated_data/exams).

Let’s read in the dataset and conduct some basic data screening.

# Data Screening

``` r
library(tidyverse)
library(caret) # For machine learning functions
library(RColorBrewer) # For some nice colour palettes
library(rpart) # For decision tree
library(rpart.plot) # For decision tree plotting
```

``` r
dat <- read.csv("StudentsPerformance.csv")

# As we can see, the dataset is made up of 1000 observations and 8 variables.
glimpse(dat)
```

    ## Rows: 1,000
    ## Columns: 8
    ## $ gender                      <chr> "female", "female", "female", "male", "mal~
    ## $ race.ethnicity              <chr> "group B", "group C", "group B", "group A"~
    ## $ parental.level.of.education <chr> "bachelor's degree", "some college", "mast~
    ## $ lunch                       <chr> "standard", "standard", "standard", "free/~
    ## $ test.preparation.course     <chr> "none", "completed", "none", "none", "none~
    ## $ math.score                  <int> 72, 69, 90, 47, 76, 71, 88, 40, 64, 38, 58~
    ## $ reading.score               <int> 72, 90, 95, 57, 78, 83, 95, 43, 64, 60, 54~
    ## $ writing.score               <int> 74, 88, 93, 44, 75, 78, 92, 39, 67, 50, 52~

``` r
# I'm going to rename some of the columns
dat <- dat %>% 
  rename(ethnicity = race.ethnicity,
         parent_education = parental.level.of.education,
         test_prep_course = test.preparation.course,
         math_score = math.score,
         read_score = reading.score,
         write_score = writing.score)

# For modeling purposes later on, I'm going to convert the character variables into factors
dat <- dat %>% 
  # This reads as "if columns return TRUE for is.character(), apply the as.factor() function)
  mutate_if(is.character, as.factor)
```

In this dataset, we have some demogprahic information on the students, such as their gender and their parents’ education, as well as their test scores in various domains (i.e., math, reading, and writing score).

Let’s take a look at the different categories for each categorical variable

``` r
# I use a simple looping to check the levels for each factor
dat %>% 
  select_if(is.factor) %>% 
  map(levels)
```

    ## $gender
    ## [1] "female" "male"  
    ## 
    ## $ethnicity
    ## [1] "group A" "group B" "group C" "group D" "group E"
    ## 
    ## $parent_education
    ## [1] "associate's degree" "bachelor's degree"  "high school"       
    ## [4] "master's degree"    "some college"       "some high school"  
    ## 
    ## $lunch
    ## [1] "free/reduced" "standard"    
    ## 
    ## $test_prep_course
    ## [1] "completed" "none"

We can check out some basic descriptive information, such as the gender distribution across the different ethnicities.

``` r
dat %>% 
  ggplot(aes(x = ethnicity)) + 
  geom_bar(aes(fill = gender), position = "fill") + 
  geom_hline(yintercept = 0.50, linetype = "dashed", alpha = 0.6) +
  theme_light() + 
  scale_fill_manual(values = c("coral1", "cornflowerblue")) +
  theme(legend.position = "bottom") + 
  labs(y = "proportion")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-1-1.png" width="672" />
We see that there’s a slightly higher number of females for some ethnicities, though this difference is pretty small.

We can also display the distribution for parent’s education.

``` r
# This code reveals that one of the two of the levels ("high school" and "some high school") are pretty much redundant, so let's collapse them into a single category
dat %>% 
  count(parent_education)
```

    ##     parent_education   n
    ## 1 associate's degree 222
    ## 2  bachelor's degree 118
    ## 3        high school 196
    ## 4    master's degree  59
    ## 5       some college 226
    ## 6   some high school 179

``` r
dat$parent_education <- dat$parent_education %>% 
  fct_collapse(`high school` = c("high school", "some high school")) %>% 
  fct_recode("college" = "some college", 
             "associate" = "associate's degree",
             "bachelor" = "bachelor's degree",
             "master" = "master's degree")

# Check if we collapsed it correctly
fct_count(dat$parent_education)
```

    ## # A tibble: 5 x 2
    ##   f               n
    ##   <fct>       <int>
    ## 1 associate     222
    ## 2 bachelor      118
    ## 3 high school   375
    ## 4 master         59
    ## 5 college       226

``` r
# Now to create a simple plot
dat %>% 
  mutate(parent_education = fct_relevel(parent_education, "high school", "college", "associate", "bachelor", "master")) %>% 
  group_by(parent_education) %>% 
  summarise(n = n()) %>% 
  # This gives us the proportion of values across each group
  mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = parent_education, y = freq, fill = parent_education)) +
  geom_col(show.legend = FALSE) + 
  theme_light() +
  scale_fill_brewer(palette = "Set3") + 
  # We change the labels to percentage and duplicate the y-axis
  scale_y_continuous(labels = scales::percent, sec.axis = dup_axis()) + 
  labs(x = "", y = "")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/parents edu 2-1.png" width="672" />
So here we can clearly see a greater percentage of parents with a lower level of education (e.g., high school) compared to the higher levels of education (e.g., master’s degree).

Let’s get a nice table up.

``` r
library(reactable)
# I'm going to set a global theme for all of the reactables we will create in this script
options(reactable.theme = reactableTheme(
  color = "#000000",
  highlightColor = "#f0f5f9",
  style = list(fontFamily = "Cambria"),
  searchInputStyle = list(width = "100%")
))

reactable(dat, 
          highlight = TRUE,
          outlined = TRUE,
          bordered = TRUE,
          wrap = FALSE,
          searchable = TRUE,
          # Set how many pages are shown 
          defaultPageSize = 8,
          # Centre-align columns as well as adjusting the width
          defaultColDef = colDef(align = "center",
                                 minWidth = 150))
```

<div id="htmlwidget-1" class="reactable html-widget" style="width:auto;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"gender":["female","female","female","male","male","female","female","male","male","female","male","male","female","male","female","female","male","female","male","female","male","female","male","female","male","male","male","female","male","female","female","female","female","male","male","male","female","female","female","male","male","female","female","male","female","male","female","female","female","male","male","male","male","male","female","female","female","male","male","female","male","male","male","female","female","male","male","female","male","female","female","male","female","male","male","male","male","male","female","female","female","male","male","male","male","female","female","female","female","female","female","male","male","male","female","male","male","female","female","female","male","male","female","male","male","female","female","male","female","female","female","male","male","female","female","male","female","female","female","female","female","male","female","male","male","female","male","male","male","female","male","male","male","female","male","male","male","male","female","male","female","female","female","male","male","female","male","male","female","male","male","male","female","male","male","female","female","male","female","male","male","female","male","male","female","female","male","female","female","female","male","male","female","female","female","female","female","female","female","female","male","female","female","female","male","male","male","male","male","female","female","male","female","male","female","male","male","male","female","female","female","female","male","female","male","male","male","male","female","female","male","male","female","male","male","male","female","female","male","male","female","male","female","male","female","female","female","male","male","female","male","male","female","male","male","male","male","female","male","male","male","female","female","male","male","male","male","female","female","male","male","female","female","male","male","female","female","male","female","female","female","male","female","female","male","male","female","female","female","female","male","male","female","female","male","male","female","female","female","male","male","male","female","female","female","male","male","female","male","male","male","male","male","female","male","male","male","male","male","male","male","male","female","male","female","male","male","male","female","female","female","male","male","female","female","male","female","male","male","female","female","female","female","female","female","female","male","male","male","female","male","male","male","male","female","female","male","male","female","female","male","female","female","male","male","female","male","female","male","male","female","male","female","female","female","female","male","female","male","female","female","male","female","female","male","male","male","male","female","female","male","female","male","female","female","male","female","female","female","male","female","male","male","female","female","female","female","female","female","male","male","female","male","male","female","male","female","female","male","male","female","male","female","female","female","female","male","female","female","male","female","male","male","male","female","male","male","male","male","male","female","female","female","female","male","female","male","male","male","male","male","female","male","female","male","male","male","male","male","male","female","female","female","female","male","female","male","male","male","male","female","female","female","male","female","male","female","male","female","male","male","male","female","female","male","female","female","male","female","male","female","female","female","female","female","female","male","male","female","male","male","female","male","male","female","male","male","female","male","male","female","female","female","female","female","male","female","female","female","male","female","female","male","female","female","female","male","male","male","female","male","male","male","female","female","female","female","female","female","female","male","female","male","male","male","male","male","female","female","female","female","female","male","female","male","female","male","female","male","male","male","male","female","female","female","male","female","male","female","male","male","male","female","male","male","female","female","male","female","male","female","female","male","female","male","male","female","female","male","male","male","male","female","female","female","male","male","female","female","female","female","female","female","female","female","female","female","female","female","female","male","male","male","female","female","female","male","male","female","female","female","female","female","male","male","male","female","female","female","female","male","female","male","female","female","female","female","male","male","male","female","male","male","male","female","male","male","male","male","female","male","male","female","female","male","male","female","female","male","female","male","female","female","female","male","female","female","female","female","female","male","female","female","female","female","female","male","male","female","male","male","male","female","female","male","female","female","female","male","male","female","male","female","female","female","female","female","female","male","male","female","male","male","female","male","female","male","male","male","male","female","female","female","female","female","female","female","female","female","male","female","female","male","female","female","male","male","male","male","female","male","female","female","male","female","female","male","female","female","male","female","male","female","male","male","male","female","male","female","male","female","male","female","male","female","male","male","female","male","male","male","female","female","female","male","male","male","male","female","male","male","male","male","female","male","female","male","male","female","male","female","female","male","female","male","female","female","male","female","male","male","male","female","female","male","female","female","female","female","male","female","female","female","female","male","female","female","female","male","female","female","female","male","male","female","female","male","female","male","female","male","male","female","female","female","male","female","female","male","male","male","male","female","male","female","male","female","male","female","female","female","female","male","female","female","male","female","female","female","male","female","female","male","female","male","female","male","female","male","female","female","male","female","male","female","male","male","male","female","male","male","female","female","male","male","female","male","female","male","male","female","female","male","female","male","male","male","male","male","male","male","female","male","male","female","male","male","male","female","female","male","female","female","male","female","female","female","male","female","male","female","female","female","male","female","female","male","female","male","female","male","female","female","female","female","male","male","female","female","male","male","female","female","female","female","female","male","female","female","male","male","female","male","female","male","male","male","female","male","female","male","male","male","male","male","male","male","female","male","male","male","female","male","male","female","female","male","female","male","female","male","female","female","male","female","male","male","female","female","male","female","female","female","female","male","female","male","male","female","female","female","male","female","female","female","female","male","male","male","female","female","male","male","female","female","male","female","male","female","female","male","female","female","female","male","female","male","female","female","female"],"ethnicity":["group B","group C","group B","group A","group C","group B","group B","group B","group D","group B","group C","group D","group B","group A","group A","group C","group C","group B","group C","group C","group D","group B","group D","group C","group D","group A","group B","group C","group C","group D","group D","group B","group E","group D","group E","group E","group D","group D","group D","group B","group C","group C","group B","group B","group E","group B","group A","group C","group D","group C","group E","group E","group C","group D","group C","group C","group E","group D","group D","group C","group E","group A","group A","group C","group D","group B","group D","group C","group B","group C","group D","group D","group A","group C","group C","group B","group E","group A","group D","group E","group B","group B","group A","group E","group D","group C","group C","group D","group A","group D","group C","group C","group C","group C","group B","group C","group B","group E","group D","group D","group B","group D","group D","group B","group C","group C","group D","group E","group B","group B","group D","group C","group A","group D","group E","group C","group B","group D","group D","group C","group C","group B","group C","group D","group E","group B","group B","group D","group D","group A","group D","group C","group E","group C","group D","group C","group B","group E","group C","group D","group D","group C","group E","group A","group D","group C","group B","group C","group D","group E","group A","group A","group B","group D","group D","group C","group E","group B","group B","group D","group B","group E","group B","group C","group E","group C","group C","group B","group B","group C","group A","group E","group D","group C","group C","group C","group B","group C","group B","group D","group C","group C","group E","group D","group C","group C","group E","group D","group B","group C","group E","group D","group B","group D","group C","group D","group C","group E","group B","group B","group C","group D","group C","group B","group C","group D","group E","group E","group B","group B","group D","group C","group C","group C","group E","group B","group E","group C","group B","group B","group D","group B","group C","group D","group B","group E","group C","group D","group A","group C","group D","group C","group B","group E","group C","group D","group D","group D","group B","group C","group D","group E","group D","group E","group D","group C","group E","group B","group B","group C","group A","group D","group B","group D","group D","group E","group C","group C","group B","group C","group C","group C","group C","group E","group D","group D","group C","group D","group D","group E","group C","group C","group D","group D","group B","group C","group C","group E","group C","group B","group D","group D","group D","group D","group B","group B","group E","group B","group B","group E","group C","group D","group C","group E","group D","group B","group A","group E","group C","group D","group A","group D","group C","group B","group C","group A","group E","group C","group B","group D","group B","group B","group D","group C","group C","group C","group D","group C","group B","group D","group C","group E","group C","group C","group C","group C","group C","group A","group C","group B","group C","group C","group E","group B","group C","group B","group D","group C","group B","group D","group C","group C","group B","group D","group D","group C","group B","group C","group D","group E","group B","group E","group C","group C","group C","group B","group A","group C","group D","group D","group B","group B","group C","group D","group C","group A","group C","group C","group A","group D","group E","group C","group D","group D","group D","group E","group D","group D","group A","group A","group B","group C","group C","group E","group A","group E","group E","group C","group D","group D","group E","group D","group E","group C","group C","group A","group B","group C","group B","group D","group C","group A","group A","group D","group C","group C","group B","group B","group D","group D","group D","group E","group D","group B","group C","group E","group C","group C","group D","group E","group C","group D","group D","group A","group B","group C","group C","group C","group A","group C","group C","group C","group C","group A","group C","group C","group D","group D","group C","group D","group C","group D","group A","group B","group A","group C","group D","group C","group B","group B","group C","group E","group C","group C","group C","group C","group D","group D","group E","group B","group C","group B","group E","group C","group A","group C","group D","group A","group A","group C","group C","group C","group C","group D","group B","group D","group E","group D","group D","group E","group B","group D","group C","group A","group B","group C","group D","group C","group B","group A","group A","group C","group C","group C","group B","group D","group C","group D","group B","group E","group D","group B","group C","group E","group D","group B","group A","group B","group C","group C","group D","group A","group D","group B","group B","group C","group D","group E","group D","group B","group D","group C","group D","group C","group C","group E","group C","group C","group D","group C","group C","group C","group E","group E","group B","group C","group C","group D","group E","group A","group C","group D","group C","group D","group D","group E","group A","group C","group C","group C","group C","group B","group B","group D","group E","group C","group C","group C","group B","group D","group D","group C","group C","group D","group B","group B","group E","group D","group B","group D","group B","group A","group C","group C","group E","group A","group A","group B","group B","group D","group D","group E","group D","group D","group D","group C","group A","group C","group C","group A","group C","group A","group E","group E","group C","group C","group B","group A","group D","group D","group D","group C","group E","group D","group D","group C","group C","group C","group E","group B","group D","group C","group C","group C","group A","group C","group E","group D","group D","group C","group C","group B","group C","group A","group E","group D","group B","group D","group D","group C","group D","group B","group B","group C","group D","group A","group B","group D","group E","group D","group D","group D","group B","group E","group B","group B","group D","group E","group B","group D","group C","group A","group D","group A","group B","group B","group C","group D","group D","group D","group C","group C","group D","group C","group D","group C","group C","group B","group C","group D","group C","group D","group C","group C","group D","group B","group E","group C","group D","group D","group D","group B","group B","group C","group B","group E","group E","group D","group A","group E","group C","group E","group C","group D","group C","group D","group C","group A","group D","group C","group E","group B","group A","group D","group B","group A","group D","group C","group D","group D","group C","group E","group D","group D","group B","group B","group C","group C","group C","group E","group C","group D","group B","group C","group B","group E","group E","group E","group D","group C","group B","group A","group C","group D","group E","group C","group C","group B","group D","group C","group D","group A","group C","group C","group B","group D","group D","group C","group C","group B","group D","group E","group C","group C","group C","group E","group D","group E","group D","group B","group C","group D","group D","group B","group D","group B","group C","group B","group D","group A","group B","group D","group B","group C","group B","group B","group B","group C","group A","group E","group D","group B","group B","group C","group C","group B","group E","group B","group C","group C","group B","group D","group D","group E","group B","group E","group D","group E","group E","group C","group C","group C","group E","group B","group C","group A","group D","group E","group C","group B","group A","group A","group C","group E","group C","group B","group A","group D","group B","group C","group A","group D","group E","group B","group C","group C","group C","group C","group D","group B","group A","group C","group A","group B","group B","group C","group E","group A","group B","group C","group D","group C","group B","group B","group D","group E","group C","group D","group C","group D","group C","group A","group E","group E","group C","group B","group B","group C","group B","group C","group C","group E","group D","group C","group C","group D","group C","group B","group E","group C","group B","group C","group B","group E","group C","group C","group D","group C","group D","group D","group C","group E","group B","group D","group E","group C","group E","group C","group D","group D","group E","group E","group A","group D","group E","group E","group B","group B","group D","group D","group D","group C","group A","group D","group D","group D","group B","group D","group C","group E","group D","group A","group C","group C","group B","group E","group E","group C","group C","group B","group D","group C","group D","group B","group D","group E","group E","group D","group E","group C","group C","group D","group D","group C","group C","group D","group A","group E","group D","group D","group C","group D","group C","group A","group B","group C","group B","group D","group B","group E","group E","group D","group E","group C","group C","group E","group C","group D","group D","group C","group A","group D","group E","group C","group D","group D","group A","group C","group E","group B","group D","group C","group A","group D","group A","group C","group B","group C","group D","group C","group B","group D","group B","group A","group C","group A","group C","group E","group A","group D","group E","group B","group D","group D","group A","group E","group C","group C","group D","group D"],"parent_education":["bachelor","college","master","associate","college","associate","college","college","high school","high school","associate","associate","high school","college","master","high school","high school","high school","master","associate","high school","college","college","high school","bachelor","master","college","bachelor","high school","master","college","college","master","college","college","associate","associate","high school","associate","associate","associate","associate","associate","college","associate","associate","associate","high school","associate","high school","college","associate","college","high school","high school","high school","associate","associate","college","high school","bachelor","high school","associate","associate","high school","high school","high school","college","associate","associate","college","college","associate","high school","high school","associate","high school","bachelor","high school","master","associate","high school","college","associate","high school","college","college","associate","college","high school","bachelor","high school","high school","associate","college","associate","high school","college","college","bachelor","college","bachelor","associate","high school","college","college","master","associate","associate","high school","associate","high school","associate","college","bachelor","high school","bachelor","bachelor","high school","college","bachelor","associate","college","high school","college","high school","high school","college","master","bachelor","master","high school","college","college","bachelor","bachelor","high school","high school","associate","college","high school","college","college","high school","college","college","high school","associate","bachelor","associate","high school","bachelor","associate","bachelor","high school","college","high school","college","associate","associate","associate","college","master","high school","master","bachelor","high school","master","high school","college","high school","high school","college","associate","bachelor","master","high school","associate","master","high school","master","college","high school","associate","high school","associate","high school","high school","high school","bachelor","associate","college","high school","college","master","associate","high school","high school","college","bachelor","associate","college","associate","associate","college","high school","bachelor","high school","college","college","high school","college","high school","associate","high school","high school","associate","high school","high school","high school","high school","associate","high school","high school","associate","master","college","high school","high school","college","college","associate","bachelor","high school","bachelor","associate","bachelor","high school","college","associate","high school","bachelor","high school","college","high school","associate","associate","high school","high school","high school","high school","college","high school","master","high school","college","associate","associate","college","master","high school","college","high school","high school","high school","high school","bachelor","high school","associate","college","bachelor","college","associate","college","college","bachelor","high school","high school","high school","bachelor","high school","high school","bachelor","college","high school","associate","associate","high school","bachelor","high school","associate","high school","high school","bachelor","high school","associate","high school","associate","high school","associate","college","high school","associate","associate","associate","college","college","high school","associate","high school","associate","bachelor","bachelor","associate","bachelor","high school","master","associate","bachelor","associate","high school","high school","college","high school","high school","college","college","college","associate","high school","high school","associate","associate","associate","bachelor","college","high school","associate","high school","high school","high school","high school","high school","associate","college","high school","college","bachelor","high school","associate","bachelor","college","college","associate","college","bachelor","associate","college","college","college","high school","high school","college","high school","college","bachelor","high school","bachelor","high school","high school","college","college","high school","college","bachelor","associate","high school","master","high school","bachelor","associate","associate","master","high school","high school","college","bachelor","associate","high school","master","high school","college","college","associate","high school","high school","high school","associate","high school","high school","high school","college","college","high school","high school","high school","associate","associate","high school","associate","master","college","associate","high school","bachelor","high school","bachelor","associate","college","high school","associate","high school","master","high school","college","college","bachelor","high school","high school","high school","associate","high school","high school","high school","high school","college","associate","associate","high school","high school","college","high school","high school","associate","high school","high school","college","high school","high school","associate","college","college","associate","college","associate","bachelor","bachelor","associate","bachelor","high school","bachelor","college","college","college","bachelor","college","associate","high school","high school","college","associate","high school","associate","high school","associate","bachelor","bachelor","associate","master","associate","high school","associate","college","high school","associate","high school","college","associate","high school","associate","associate","associate","college","bachelor","high school","high school","college","college","high school","college","master","associate","college","associate","master","high school","high school","bachelor","master","bachelor","college","high school","high school","high school","master","high school","college","college","high school","high school","college","associate","bachelor","master","high school","college","high school","high school","bachelor","associate","associate","high school","associate","associate","high school","bachelor","associate","high school","bachelor","associate","high school","associate","associate","associate","master","high school","high school","high school","high school","master","high school","bachelor","associate","college","associate","college","associate","master","associate","high school","college","college","bachelor","college","bachelor","associate","bachelor","master","high school","bachelor","college","bachelor","college","high school","high school","associate","college","high school","college","master","high school","high school","bachelor","associate","college","associate","high school","bachelor","college","high school","college","high school","bachelor","high school","bachelor","bachelor","high school","high school","high school","high school","master","high school","college","high school","master","high school","associate","master","college","associate","college","college","bachelor","associate","associate","high school","bachelor","bachelor","master","associate","high school","bachelor","high school","college","bachelor","college","associate","associate","college","high school","college","high school","bachelor","high school","high school","high school","high school","high school","college","associate","high school","associate","high school","high school","high school","bachelor","associate","high school","high school","college","high school","high school","college","associate","high school","college","associate","high school","associate","associate","college","high school","college","high school","associate","high school","college","bachelor","college","associate","high school","associate","college","associate","high school","college","college","high school","associate","college","high school","high school","high school","high school","college","master","college","associate","high school","college","associate","associate","bachelor","associate","high school","college","associate","bachelor","associate","high school","bachelor","high school","bachelor","college","high school","bachelor","high school","college","high school","associate","college","high school","college","master","high school","associate","associate","associate","high school","associate","college","high school","high school","high school","college","college","associate","high school","high school","college","associate","high school","college","high school","college","master","associate","college","associate","high school","bachelor","associate","high school","associate","college","associate","high school","college","bachelor","college","high school","college","master","high school","associate","associate","college","bachelor","college","college","high school","high school","high school","high school","college","high school","high school","high school","high school","college","high school","bachelor","high school","bachelor","college","high school","high school","college","college","associate","associate","master","high school","associate","bachelor","high school","high school","college","associate","master","high school","college","high school","high school","high school","associate","high school","associate","college","associate","high school","high school","associate","college","college","college","college","high school","high school","bachelor","high school","high school","master","high school","high school","high school","bachelor","bachelor","high school","high school","high school","bachelor","college","high school","high school","high school","associate","high school","high school","high school","college","bachelor","bachelor","high school","college","high school","high school","high school","associate","associate","high school","high school","high school","college","high school","master","master","high school","high school","associate","master","high school","college","high school","high school","bachelor","college","bachelor","high school","associate","associate","master","bachelor","college","associate","college","high school","associate","associate","associate","high school","college","associate","associate","bachelor","college","college","high school","high school","associate","bachelor","bachelor","high school","bachelor","associate","associate","associate","high school","college","high school","college","associate","master","high school","associate","high school","high school","high school","associate","high school","master","master","high school","bachelor","high school","college","high school","college","bachelor","bachelor","bachelor","college","bachelor","bachelor","associate","college","bachelor","high school","associate","college","high school","high school","high school","associate","high school","high school","associate","high school","associate","high school","college","college","associate","bachelor","associate","college","associate","high school","college","high school","master","master","high school","high school","high school","associate","high school","college","high school","high school","high school","college","high school","high school","college","associate","college","master","high school","high school","college","high school","associate","high school","college","college","high school","college","associate","bachelor","bachelor","high school","high school","college","college","college","college","associate","high school","associate","high school","high school","high school","college","high school","high school","associate","high school","high school","college","high school","high school","associate","bachelor","high school","master","high school","high school","college","college"],"lunch":["standard","standard","standard","free/reduced","standard","standard","standard","free/reduced","free/reduced","free/reduced","standard","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","free/reduced","standard","free/reduced","standard","standard","free/reduced","free/reduced","standard","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","standard","free/reduced","free/reduced","free/reduced","free/reduced","standard","standard","free/reduced","free/reduced","standard","standard","standard","free/reduced","standard","standard","free/reduced","standard","standard","free/reduced","free/reduced","standard","standard","standard","free/reduced","free/reduced","free/reduced","free/reduced","standard","standard","standard","free/reduced","standard","free/reduced","standard","free/reduced","standard","free/reduced","free/reduced","standard","free/reduced","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","free/reduced","standard","free/reduced","standard","standard","standard","standard","free/reduced","standard","free/reduced","standard","free/reduced","standard","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","standard","standard","free/reduced","standard","free/reduced","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","free/reduced","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","free/reduced","standard","free/reduced","standard","free/reduced","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","free/reduced","free/reduced","free/reduced","free/reduced","standard","standard","standard","free/reduced","free/reduced","standard","free/reduced","standard","standard","standard","standard","free/reduced","standard","free/reduced","standard","free/reduced","standard","free/reduced","free/reduced","standard","standard","free/reduced","free/reduced","standard","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","free/reduced","free/reduced","standard","free/reduced","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","free/reduced","free/reduced","free/reduced","free/reduced","standard","standard","free/reduced","free/reduced","free/reduced","standard","standard","standard","free/reduced","standard","standard","free/reduced","standard","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","standard","free/reduced","standard","standard","free/reduced","free/reduced","standard","free/reduced","standard","standard","free/reduced","standard","standard","standard","free/reduced","standard","standard","free/reduced","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","free/reduced","free/reduced","standard","standard","standard","standard","free/reduced","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","free/reduced","free/reduced","free/reduced","free/reduced","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","standard","free/reduced","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","free/reduced","free/reduced","standard","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","free/reduced","standard","standard","free/reduced","free/reduced","free/reduced","standard","standard","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","standard","standard","free/reduced","standard","free/reduced","standard","free/reduced","free/reduced","standard","standard","free/reduced","standard","standard","standard","free/reduced","standard","free/reduced","standard","standard","standard","standard","free/reduced","free/reduced","free/reduced","standard","standard","free/reduced","standard","standard","free/reduced","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","standard","free/reduced","free/reduced","standard","free/reduced","standard","free/reduced","free/reduced","standard","free/reduced","free/reduced","free/reduced","free/reduced","standard","standard","free/reduced","standard","free/reduced","standard","free/reduced","standard","standard","standard","standard","free/reduced","standard","free/reduced","standard","free/reduced","standard","standard","standard","free/reduced","standard","free/reduced","free/reduced","free/reduced","free/reduced","standard","free/reduced","standard","standard","free/reduced","free/reduced","standard","free/reduced","standard","standard","free/reduced","free/reduced","free/reduced","standard","standard","standard","standard","standard","standard","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","standard","free/reduced","free/reduced","standard","free/reduced","free/reduced","standard","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","standard","free/reduced","standard","free/reduced","standard","standard","free/reduced","standard","standard","standard","standard","free/reduced","standard","free/reduced","standard","standard","standard","standard","free/reduced","standard","standard","standard","free/reduced","standard","standard","free/reduced","free/reduced","free/reduced","standard","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","free/reduced","standard","standard","free/reduced","standard","free/reduced","free/reduced","standard","free/reduced","standard","free/reduced","standard","free/reduced","free/reduced","standard","standard","standard","standard","free/reduced","free/reduced","standard","free/reduced","free/reduced","free/reduced","free/reduced","standard","standard","standard","free/reduced","standard","free/reduced","standard","standard","free/reduced","standard","standard","standard","free/reduced","standard","standard","standard","standard","free/reduced","standard","standard","free/reduced","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","free/reduced","standard","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","free/reduced","free/reduced","standard","free/reduced","standard","free/reduced","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","standard","standard","free/reduced","standard","standard","free/reduced","standard","standard","standard","standard","standard","free/reduced","standard","free/reduced","standard","free/reduced","standard","free/reduced","standard","standard","free/reduced","free/reduced","free/reduced","standard","standard","free/reduced","free/reduced","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","standard","free/reduced","standard","standard","standard","free/reduced","free/reduced","free/reduced","standard","free/reduced","free/reduced","standard","standard","free/reduced","standard","standard","standard","free/reduced","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","standard","free/reduced","standard","standard","standard","standard","standard","free/reduced","standard","standard","standard","free/reduced","free/reduced","free/reduced","free/reduced","standard","standard","standard","standard","standard","free/reduced","standard","free/reduced","free/reduced","standard","standard","free/reduced","standard","standard","free/reduced","standard","free/reduced","standard","free/reduced","standard","standard","free/reduced","standard","standard","standard","free/reduced","standard","standard","standard","free/reduced","standard","free/reduced","standard","standard","free/reduced","free/reduced","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","standard","free/reduced","standard","standard","free/reduced","free/reduced","standard","free/reduced","standard","free/reduced","standard","standard","free/reduced","standard","free/reduced","standard","standard","standard","free/reduced","standard","free/reduced","free/reduced","standard","free/reduced","free/reduced","standard","standard","free/reduced","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","free/reduced","standard","standard","standard","free/reduced","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","free/reduced","free/reduced","free/reduced","free/reduced","standard","free/reduced","standard","free/reduced","standard","free/reduced","free/reduced","standard","standard","standard","standard","standard","standard","free/reduced","standard","free/reduced","standard","free/reduced","free/reduced","free/reduced","standard","standard","standard","standard","standard","standard","standard","standard","standard","standard","standard","free/reduced","standard","standard","free/reduced","standard","free/reduced","free/reduced","standard","standard","standard","free/reduced","standard","free/reduced","free/reduced","standard","standard","standard","free/reduced","free/reduced","free/reduced","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","standard","free/reduced","free/reduced","free/reduced","standard","standard","free/reduced","standard","standard","free/reduced","free/reduced","free/reduced","standard","standard","standard","standard","free/reduced","free/reduced","free/reduced","standard","standard","standard","free/reduced","standard","free/reduced","standard","standard","free/reduced","free/reduced","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","free/reduced","free/reduced","standard","free/reduced","free/reduced","free/reduced","free/reduced","free/reduced","free/reduced","free/reduced","free/reduced","standard","free/reduced","standard","free/reduced","standard","standard","free/reduced","standard","standard","free/reduced","standard","standard","standard","free/reduced","free/reduced","free/reduced","standard","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","standard","free/reduced","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","standard","standard","free/reduced","standard","standard","standard","free/reduced","standard","standard","standard","standard","standard","standard","standard","free/reduced","free/reduced","free/reduced","standard","free/reduced","free/reduced","standard","standard","free/reduced","free/reduced","standard","free/reduced"],"test_prep_course":["none","completed","none","none","none","none","completed","none","completed","none","none","none","none","completed","none","none","none","none","completed","none","none","completed","none","none","completed","none","none","none","none","none","none","none","none","none","none","completed","none","none","completed","none","none","none","none","completed","none","none","completed","none","completed","completed","none","completed","none","none","completed","none","completed","none","completed","none","completed","none","none","none","none","none","none","none","none","none","completed","completed","none","none","none","none","none","completed","completed","none","none","none","completed","none","none","none","none","none","none","none","none","none","none","completed","none","completed","completed","completed","none","none","none","completed","none","completed","completed","none","none","completed","none","none","completed","none","none","none","completed","none","none","none","none","none","completed","completed","completed","none","none","none","none","completed","none","none","none","completed","completed","completed","completed","none","completed","none","none","completed","none","none","completed","none","none","none","none","none","completed","completed","completed","none","completed","none","none","completed","completed","none","completed","none","completed","completed","none","none","none","completed","completed","completed","completed","completed","completed","none","none","none","completed","completed","completed","completed","completed","completed","completed","none","none","none","completed","completed","none","completed","none","none","none","completed","none","completed","completed","none","completed","none","none","none","completed","none","none","none","none","completed","none","completed","none","none","completed","none","none","completed","completed","completed","completed","none","none","completed","completed","none","none","none","completed","none","none","none","none","completed","none","none","none","none","completed","none","none","completed","none","none","completed","none","none","none","none","none","none","completed","none","none","completed","none","completed","none","none","none","none","completed","none","completed","completed","completed","none","none","none","none","none","none","completed","none","none","none","none","none","completed","none","completed","completed","none","none","none","none","none","completed","none","completed","completed","none","none","completed","none","none","completed","none","completed","none","completed","completed","completed","none","completed","none","completed","none","completed","none","completed","none","none","none","none","none","completed","completed","completed","none","completed","none","none","none","completed","none","none","none","none","none","none","none","completed","none","none","none","completed","none","none","none","completed","none","none","completed","none","none","completed","completed","none","none","none","completed","none","none","none","none","completed","none","none","none","none","completed","none","none","none","completed","none","none","none","completed","completed","none","none","none","none","completed","none","completed","none","none","none","completed","none","none","none","completed","none","none","none","none","none","none","none","none","completed","none","none","completed","completed","none","none","none","none","none","completed","none","none","completed","none","none","completed","none","completed","completed","completed","completed","none","completed","completed","none","completed","none","none","completed","completed","completed","completed","completed","none","none","none","none","none","none","none","none","none","none","none","completed","none","none","none","completed","completed","none","none","none","none","completed","none","completed","none","none","completed","none","none","none","none","none","none","none","none","completed","none","none","none","none","completed","none","none","completed","completed","none","completed","none","completed","none","completed","completed","completed","none","none","none","completed","none","none","none","none","none","none","none","completed","completed","none","none","none","none","none","completed","none","completed","none","none","none","completed","none","completed","none","none","none","none","none","none","none","none","none","completed","completed","completed","none","completed","completed","completed","none","none","none","none","completed","none","completed","none","none","none","completed","none","completed","completed","completed","completed","completed","none","completed","completed","completed","completed","none","none","none","completed","completed","completed","none","none","none","completed","none","none","none","none","completed","none","none","none","completed","none","completed","completed","none","none","completed","completed","none","none","completed","none","none","completed","none","completed","completed","none","completed","none","none","none","none","completed","none","none","none","none","none","none","none","none","none","none","completed","completed","none","none","none","none","none","none","none","none","completed","none","none","none","none","none","completed","none","completed","none","none","none","none","none","none","completed","none","none","completed","completed","none","completed","completed","none","none","completed","none","none","completed","none","none","none","completed","completed","none","none","none","completed","none","completed","none","completed","none","none","none","completed","completed","completed","completed","completed","none","none","none","none","none","none","none","none","none","none","none","completed","completed","none","none","completed","none","none","none","completed","completed","completed","completed","completed","none","none","none","none","none","completed","completed","completed","completed","none","none","none","none","none","completed","none","none","none","completed","none","completed","none","completed","none","completed","none","completed","none","none","none","none","completed","completed","completed","none","none","completed","completed","completed","completed","none","completed","none","completed","completed","none","none","completed","completed","completed","none","none","completed","none","completed","none","none","none","none","completed","none","none","none","none","none","completed","none","none","none","none","none","completed","completed","none","completed","completed","none","none","none","completed","completed","completed","none","none","completed","none","none","none","completed","completed","none","none","none","none","completed","none","none","none","none","none","completed","completed","none","none","completed","completed","completed","completed","none","none","none","none","none","none","none","completed","none","completed","none","none","none","none","completed","completed","none","none","none","none","none","none","none","none","none","none","completed","completed","none","completed","none","completed","none","none","completed","none","completed","none","none","none","completed","none","completed","none","none","completed","none","completed","none","completed","none","completed","completed","none","none","none","completed","completed","completed","none","completed","none","none","none","none","none","none","none","none","none","none","none","completed","none","none","none","completed","completed","none","completed","none","none","completed","none","none","completed","completed","none","none","none","none","none","none","none","completed","completed","none","none","none","completed","completed","none","none","none","completed","none","none","completed","none","none","none","completed","completed","completed","none","none","completed","completed","none","none","none","completed","none","completed","none","none","completed","completed","none","none","completed","none","completed","completed","none","none","none","none","none","completed","none","completed","completed","none","completed","none","completed","completed","completed","none","none","none","completed","completed","completed","none","none","completed","none","none","none","none","completed","completed","none","completed","none","completed","none","none","none","none","none","none","none","none","none","completed","none","none","completed","none","none","none","none","completed","completed","none","none","completed","completed","none","completed","none","none","none","completed","completed","none","none","none","completed","none","completed","completed","completed","none","none","none","completed","none","completed","completed","none"],"math_score":[72,69,90,47,76,71,88,40,64,38,58,40,65,78,50,69,88,18,46,54,66,65,44,69,74,73,69,67,70,62,69,63,56,40,97,81,74,50,75,57,55,58,53,59,50,65,55,66,57,82,53,77,53,88,71,33,82,52,58,0,79,39,62,69,59,67,45,60,61,39,58,63,41,61,49,44,30,80,61,62,47,49,50,72,42,73,76,71,58,73,65,27,71,43,79,78,65,63,58,65,79,68,85,60,98,58,87,66,52,70,77,62,54,51,99,84,75,78,51,55,79,91,88,63,83,87,72,65,82,51,89,53,87,75,74,58,51,70,59,71,76,59,42,57,88,22,88,73,68,100,62,77,59,54,62,70,66,60,61,66,82,75,49,52,81,96,53,58,68,67,72,94,79,63,43,81,46,71,52,97,62,46,50,65,45,65,80,62,48,77,66,76,62,77,69,61,59,55,45,78,67,65,69,57,59,74,82,81,74,58,80,35,42,60,87,84,83,34,66,61,56,87,55,86,52,45,72,57,68,88,76,46,67,92,83,80,63,64,54,84,73,80,56,59,75,85,89,58,65,68,47,71,60,80,54,62,64,78,70,65,64,79,44,99,76,59,63,69,88,71,69,58,47,65,88,83,85,59,65,73,53,45,73,70,37,81,97,67,88,77,76,86,63,65,78,67,46,71,40,90,81,56,67,80,74,69,99,51,53,49,73,66,67,68,59,71,77,83,63,56,67,75,71,43,41,82,61,28,82,41,71,47,62,90,83,61,76,49,24,35,58,61,69,67,79,72,62,77,75,87,52,66,63,46,59,61,63,42,59,80,58,85,52,27,59,49,69,61,44,73,84,45,74,82,59,46,80,85,71,66,80,87,79,38,38,67,64,57,62,73,73,77,76,57,65,48,50,85,74,60,59,53,49,88,54,63,65,82,52,87,70,84,71,63,51,84,71,74,68,57,82,57,47,59,41,62,86,69,65,68,64,61,61,47,73,50,75,75,70,89,67,78,59,73,79,67,69,86,47,81,64,100,65,65,53,37,79,53,100,72,53,54,71,77,75,84,26,72,77,91,83,63,68,59,90,71,76,80,55,76,73,52,68,59,49,70,61,60,64,79,65,64,83,81,54,68,54,59,66,76,74,94,63,95,40,82,68,55,79,86,76,64,62,54,77,76,74,66,66,67,71,91,69,54,53,68,56,36,29,62,68,47,62,79,73,66,51,51,85,97,75,79,81,82,64,78,92,72,62,79,79,87,40,77,53,32,55,61,53,73,74,63,96,63,48,48,92,61,63,68,71,91,53,50,74,40,61,81,48,53,81,77,63,73,69,65,55,44,54,48,58,71,68,74,92,56,30,53,69,65,54,29,76,60,84,75,85,40,61,58,69,58,94,65,82,60,37,88,95,65,35,62,58,100,61,100,69,61,49,44,67,79,66,75,84,71,67,80,86,76,41,74,72,74,70,65,59,64,50,69,51,68,85,65,73,62,77,69,43,90,74,73,55,65,80,50,63,77,73,81,66,52,69,65,69,50,73,70,81,63,67,60,62,29,62,94,85,77,53,93,49,73,66,77,49,79,75,59,57,66,79,57,87,63,59,62,46,66,89,42,93,80,98,81,60,76,73,96,76,91,62,55,74,50,47,81,65,68,73,53,68,55,87,55,53,67,92,53,81,61,80,37,81,59,55,72,69,69,50,87,71,68,79,77,58,84,55,70,52,69,53,48,78,62,60,74,58,76,68,58,52,75,52,62,66,49,66,35,72,94,46,77,76,52,91,32,72,19,68,52,48,60,66,89,42,57,70,70,69,52,67,76,87,82,73,75,64,41,90,59,51,45,54,87,72,94,45,61,60,77,85,78,49,71,48,62,56,65,69,68,61,74,64,77,58,60,73,75,58,66,39,64,23,74,40,90,91,64,59,80,71,61,87,82,62,97,75,65,52,87,53,81,39,71,97,82,59,61,78,49,59,70,82,90,43,80,81,57,59,64,63,71,64,55,51,62,93,54,69,44,86,85,50,88,59,32,36,63,67,65,85,73,34,93,67,88,57,79,67,70,50,69,52,47,46,68,100,44,57,91,69,35,72,54,74,74,64,65,46,48,67,62,61,70,98,70,67,57,85,77,72,78,81,61,58,54,82,49,49,57,94,75,74,58,62,72,84,92,45,75,56,48,100,65,72,62,66,63,68,75,89,78,53,49,54,64,60,62,55,91,8,81,79,78,74,57,40,81,44,67,86,65,55,62,63,88,62,59,68,77],"read_score":[72,90,95,57,78,83,95,43,64,60,54,52,81,72,53,75,89,32,42,58,69,75,54,73,71,74,54,69,70,70,74,65,72,42,87,81,81,64,90,56,61,73,58,65,56,54,65,71,74,84,55,69,44,78,84,41,85,55,59,17,74,39,61,80,58,64,37,72,58,64,63,55,51,57,49,41,26,78,74,68,49,45,47,64,39,80,83,71,70,86,72,34,79,45,86,81,66,72,67,67,67,74,91,44,86,67,100,63,76,64,89,55,53,58,100,77,85,82,63,69,92,89,93,57,80,95,68,77,82,49,84,37,74,81,79,55,54,55,66,61,72,62,55,43,73,39,84,68,75,100,67,67,70,49,67,89,74,60,86,62,78,88,53,53,92,100,51,76,83,75,73,88,86,67,51,91,54,77,70,100,68,64,50,69,52,67,76,66,52,88,65,83,64,62,84,55,69,56,53,79,84,81,77,69,41,71,62,80,81,61,79,28,62,51,91,83,86,42,77,56,68,85,65,80,66,56,72,50,72,95,64,43,86,87,82,75,66,60,52,80,68,83,52,51,74,76,76,70,64,60,49,83,70,80,52,73,73,77,75,81,79,79,50,93,73,42,75,72,92,76,63,49,53,70,85,78,92,63,86,56,52,48,79,78,46,82,82,89,75,76,70,73,60,73,77,62,41,74,46,87,78,54,84,76,75,67,87,52,71,57,76,60,61,67,64,66,82,72,71,65,79,86,81,53,46,90,61,23,75,55,60,37,56,78,93,68,70,51,38,55,61,73,76,72,73,80,61,94,74,74,65,57,78,58,71,72,61,66,62,90,62,84,58,34,60,58,58,66,64,84,77,73,74,97,70,43,90,95,83,64,86,100,81,49,43,76,73,78,64,70,67,68,67,54,74,45,67,89,63,59,54,43,65,99,59,73,65,80,57,84,71,83,66,67,72,73,74,73,59,56,93,58,58,85,39,67,83,71,59,63,66,72,56,59,66,48,68,66,56,88,81,81,73,83,82,74,66,81,46,73,85,92,77,58,61,56,89,54,100,65,58,54,70,90,58,87,31,67,88,74,85,69,86,67,90,76,62,68,64,71,71,59,68,52,52,74,47,75,53,82,85,64,83,88,64,64,48,78,69,71,79,87,61,89,59,82,70,59,78,92,71,50,49,61,97,87,89,74,78,78,49,86,58,59,52,60,61,53,41,74,67,54,61,88,69,83,60,66,66,92,69,82,77,95,63,83,100,67,67,72,76,90,48,62,45,39,72,67,70,66,75,74,90,80,51,43,100,71,48,68,75,96,62,66,81,55,51,91,56,61,97,79,73,75,77,76,73,63,64,66,57,62,68,76,100,79,24,54,77,82,60,29,78,57,89,72,84,58,64,63,60,59,90,77,93,68,45,78,81,73,61,63,51,96,58,97,70,48,57,51,64,60,74,88,84,74,80,92,76,74,52,88,81,79,65,81,70,62,53,79,56,80,86,70,79,67,67,66,60,87,77,66,71,69,63,60,73,85,74,72,76,57,78,84,77,64,78,82,75,61,72,68,55,40,66,99,75,78,58,90,53,76,74,77,63,89,82,72,78,66,81,67,84,64,63,72,34,59,87,61,84,85,100,81,70,94,78,96,76,73,72,59,90,48,43,74,75,51,92,39,77,46,89,47,58,57,79,66,71,60,73,57,84,73,55,79,75,64,60,84,69,72,77,90,55,95,58,68,59,77,72,58,81,62,63,72,75,62,71,60,48,73,67,78,65,58,72,44,79,85,56,90,85,59,81,51,79,38,65,65,62,66,74,84,52,68,70,84,60,55,73,80,94,85,76,81,74,45,75,54,31,47,64,84,80,86,59,70,72,91,90,90,52,87,58,67,68,69,86,54,60,86,60,82,50,64,64,82,57,77,52,58,44,77,65,85,85,54,72,75,67,68,85,67,64,97,68,79,49,73,62,86,42,71,93,82,53,42,74,51,58,72,84,90,62,64,82,61,72,76,64,70,73,46,51,76,100,72,65,51,85,92,67,74,62,34,29,78,54,78,84,78,48,100,84,77,48,84,75,64,42,84,61,62,61,70,100,61,77,96,70,53,66,65,70,64,56,61,43,56,74,57,71,75,87,63,57,58,81,68,66,91,66,62,68,61,82,58,50,75,73,77,74,52,69,57,87,100,63,81,58,54,100,76,57,70,68,63,76,84,100,72,50,65,63,82,62,65,41,95,24,78,85,87,75,51,59,75,45,86,81,82,76,72,63,99,55,71,78,86],"write_score":[74,88,93,44,75,78,92,39,67,50,52,43,73,70,58,78,86,28,46,61,63,70,53,73,80,72,55,75,65,75,74,61,65,38,82,79,83,59,88,57,54,68,65,66,54,57,62,76,76,82,48,68,42,75,87,43,86,49,58,10,72,34,55,71,59,61,37,74,56,57,73,63,48,56,41,38,22,81,72,68,50,45,54,63,34,82,88,74,67,82,74,36,71,50,92,82,62,70,62,62,67,74,89,47,90,72,100,64,70,72,98,49,47,54,100,74,82,79,61,65,89,92,93,56,73,86,67,74,74,51,82,40,70,84,75,48,41,56,67,69,71,64,54,47,78,33,75,66,81,93,69,68,66,47,61,88,78,60,87,64,74,85,52,49,91,100,51,78,78,70,74,78,81,70,54,87,58,77,62,100,75,66,47,70,49,65,65,68,45,87,69,79,66,62,85,52,65,51,55,76,86,77,69,68,42,78,62,76,76,66,79,27,60,56,81,75,88,39,70,56,74,73,62,75,73,54,71,54,64,94,66,42,83,78,84,77,67,74,51,80,66,83,55,43,69,71,74,68,62,53,49,83,70,72,52,70,68,77,78,81,77,78,51,90,68,41,81,77,95,70,61,42,58,71,76,73,93,75,80,57,42,46,84,78,46,82,88,82,76,77,68,70,57,75,80,60,43,68,50,75,81,52,81,64,83,69,81,44,67,52,80,57,68,69,75,65,91,78,69,63,84,79,80,53,43,94,62,19,77,51,61,35,53,81,95,66,69,43,27,60,52,63,74,67,67,75,57,95,66,76,69,52,80,57,70,70,61,69,61,89,59,78,58,32,58,60,53,61,58,85,71,70,72,96,73,41,82,100,77,62,83,95,71,45,43,75,70,67,64,75,59,77,67,56,77,41,63,95,57,54,67,43,55,100,62,68,63,77,56,85,74,78,60,67,79,69,68,67,62,54,93,64,67,80,34,62,86,65,53,54,59,70,55,50,66,53,64,73,51,82,79,80,69,76,73,77,60,80,42,72,85,97,74,49,62,47,89,48,100,68,55,45,76,91,62,91,38,65,85,76,90,74,84,61,91,83,66,72,70,67,68,56,61,46,54,71,56,74,57,82,76,70,90,90,68,66,52,76,68,72,82,92,54,92,54,80,66,54,77,87,73,43,52,62,94,85,84,73,78,79,52,84,57,50,49,59,60,43,47,70,73,53,58,94,68,83,58,62,71,86,68,80,79,89,66,80,97,64,64,69,65,88,50,64,40,33,79,66,70,62,79,74,92,80,46,45,100,78,47,67,70,92,56,64,71,53,52,89,58,68,96,80,78,80,77,76,73,62,65,65,54,50,64,73,99,72,15,48,73,81,63,30,80,51,90,62,82,54,62,65,63,66,91,74,93,72,38,83,84,68,54,56,52,86,62,99,63,46,46,55,70,65,81,85,80,64,81,88,74,73,51,90,79,80,60,81,65,68,55,81,53,76,98,74,79,67,64,61,58,85,73,63,69,67,63,60,71,87,61,77,68,50,76,84,78,66,76,76,78,60,74,60,54,44,68,100,68,73,44,83,53,78,81,73,56,86,90,70,79,59,82,72,87,67,64,65,36,52,79,58,90,85,99,84,74,87,72,99,74,80,70,59,88,42,41,71,77,57,84,37,80,43,94,44,57,59,84,73,73,55,72,56,82,72,47,74,71,68,59,86,68,65,75,85,53,92,52,72,65,77,64,54,86,63,59,72,77,60,75,57,49,74,72,79,60,55,70,43,82,82,57,84,82,62,79,44,77,32,61,61,60,70,69,77,51,73,70,81,54,57,68,73,95,87,78,74,75,40,69,51,36,49,67,76,83,87,64,76,68,88,92,93,51,82,52,58,70,76,81,53,57,89,58,89,45,74,57,79,53,73,46,51,36,76,64,84,85,50,68,69,67,63,93,61,55,96,65,81,46,72,53,87,38,80,91,88,52,41,72,51,47,76,78,82,61,66,84,54,80,74,66,70,71,44,54,80,95,59,74,48,91,85,73,75,69,38,27,79,63,82,89,74,41,100,84,77,51,91,72,70,48,82,66,66,55,66,100,52,80,91,67,46,66,65,69,60,52,71,44,51,70,62,73,74,90,58,53,57,85,69,72,96,64,61,61,58,80,60,52,73,71,83,72,54,69,62,81,100,59,71,64,53,100,75,58,72,64,60,67,80,100,69,60,61,67,77,60,58,48,94,23,78,86,91,82,54,51,76,45,83,75,78,76,74,62,95,55,65,77,86]},"columns":[{"accessor":"gender","name":"gender","type":"factor","minWidth":150,"align":"center"},{"accessor":"ethnicity","name":"ethnicity","type":"factor","minWidth":150,"align":"center"},{"accessor":"parent_education","name":"parent_education","type":"factor","minWidth":150,"align":"center"},{"accessor":"lunch","name":"lunch","type":"factor","minWidth":150,"align":"center"},{"accessor":"test_prep_course","name":"test_prep_course","type":"factor","minWidth":150,"align":"center"},{"accessor":"math_score","name":"math_score","type":"numeric","minWidth":150,"align":"center"},{"accessor":"read_score","name":"read_score","type":"numeric","minWidth":150,"align":"center"},{"accessor":"write_score","name":"write_score","type":"numeric","minWidth":150,"align":"center"}],"searchable":true,"defaultPageSize":8,"paginationType":"numbers","showPageInfo":true,"minRows":1,"highlight":true,"outlined":true,"bordered":true,"nowrap":true,"theme":{"color":"#000000","highlightColor":"#f0f5f9","style":{"fontFamily":"Cambria"},"searchInputStyle":{"width":"100%"}},"dataKey":"abe17095b511aec67ee7a6c5452e4b66","key":"abe17095b511aec67ee7a6c5452e4b66"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>

# Research Questions

After glancing at the variables, now we generate a few research questions to tackle:

1.  Are math, reading, and writing scores all correlated?
2.  How do students fare in these tests overall?
3.  Can we attempt to model the relationship between math score and other variables?

Now that we have a few preliminary questions to consider, we can move on to performing some basic exploratory data analysis.

# Exploratory Data Analysis

Right, let’s tackle the first question first: Plenty of research shows how integral reading and writing skills are to math competency; we need to be proficient in reading to be able to process and understand mathematical queries, as well as be adept in writing to be able to not only communicate our ideas, but also process and reason mathematical concepts.[^1]

To verify this three-way relationship, we can plot the data in a correlogram.

``` r
library(ggcorrplot)
```

    ## Warning: package 'ggcorrplot' was built under R version 4.1.2

``` r
# First we get some correlation values
corr <- dat %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  round(., 2)

# Now we create our correlogram
ggcorrplot(corr, 
           hc.order = TRUE,
           type = "full",
           # This will show us the correlation coefficients
           lab = TRUE,
           outline.col = "white",
           ggtheme = ggplot2::theme_minimal,
           colors = c("#6D9EC1", "white", "#E46726"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/correlogram-1.png" width="672" />

We see that reading and writing scores are highly positively related to each other (which makes sense); similarly, math scores are strongly positively correlated with both reading and writing scores, which fits our earlier assumptions about these three test scores.

Let’s take a look at the distribution of these variables.

``` r
# Let's give names to our facet labels
test_labels <- c("Math Score", "Reading Score", "Writing Score")
names(test_labels) <- c("math_score", "read_score", "write_score")

dat %>% 
  select_if(is.integer) %>% 
  pivot_longer(cols = 1:3, names_to = "test_type", values_to = "test_score") %>% 
  ggplot(aes(x = test_score)) + 
  geom_histogram(aes(fill = test_type), binwidth = 5, show.legend = FALSE) + 
  facet_wrap(~test_type, labeller = labeller(test_type = test_labels)) + 
  theme_light() +
  ggthemes::scale_fill_tableau() + 
  labs(title = "Test Score Distributions", x = "Test Score", y = "")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/test distribution-1.png" width="672" />

Generally speaking, it seems that scores for each test are negatively skewed, which means that most students score above average in math, reading, and writing tests.

Finally, let’s do some quick visualisations to see which variables might be potential predictors in our model.

``` r
# Visualise parent education and test score
bp_1 <- dat %>% 
  mutate(parent_education = fct_relevel(parent_education, "high school", "college", "associate", "bachelor", "master")) %>% 
  pivot_longer(cols = c("math_score", "read_score", "write_score"), 
               names_to = "test_type",
               values_to = "test_score") %>% 
  ggplot(aes(x = parent_education, y = test_score)) + 
  geom_boxplot(aes(fill = parent_education)) + 
  theme_minimal() + 
  ggthemes::scale_fill_tableau() + 
  facet_wrap(~test_type) + 
  theme(legend.position = "bottom", axis.text.x = element_blank())
  # Change angel to 90 degrees, than do some horizontal and vertical adjustments
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Visualise lunch plan and test score
bp_2 <- dat %>% 
  pivot_longer(cols = c("math_score", "read_score", "write_score"), 
               names_to = "test_type",
               values_to = "test_score") %>% 
  ggplot(aes(x = lunch, y = test_score)) + 
  # after_scale let's us map to fill the transparent version of color (i.e., we map fill after mapping color)
  geom_boxplot(aes(color = lunch, fill = after_scale(alpha(color, 0.5)))) + 
  geom_jitter(aes(color = lunch), width = .05, alpha = 0.05) + 
  theme_minimal() + 
  ggthemes::scale_fill_tableau() + 
  ggthemes::scale_color_tableau() +
  facet_wrap(~test_type) + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, 
                                                             vjust = 0.5, 
                                                             hjust = 1))

# Visualise test preparation and test score
vi_1 <- dat %>% 
  pivot_longer(cols = c("math_score", "read_score", "write_score"), 
               names_to = "test_type",
               values_to = "test_score") %>% 
  ggplot(aes(x = test_prep_course, 
             y = test_score, 
             color = test_prep_course, 
             fill = test_prep_course)) + 
  # The bw argument controls the smoothing of the plot
  geom_violin(aes(fill = test_prep_course), bw = .9, size = 1.2, color = NA) +
  geom_boxplot(fill = "white", width = .2, size = 1.2, outlier.shape = NA) +
  theme_minimal() + 
  ggthemes::scale_fill_tableau() + 
  ggthemes::scale_color_tableau() +
  facet_wrap(~test_type) + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, 
                                                             vjust = 0.5, 
                                                             hjust = 1))

# Let's combine all these plots together
library(ggpubr)
```

    ## Warning: package 'ggpubr' was built under R version 4.1.3

``` r
# We use nested ggarrange() to change the column/row span of the plot
ggarrange(bp_1, 
          ggarrange(bp_2, vi_1, ncol = 2, labels = c("B", "C")),
          nrow = 2, labels = "A")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/hypo-viz-1.png" width="960" />
As we can observe, on average, test scores seem to be higher when one’s parents have higher education, when one is given the standard lunch plan, and when one has completed their test prep. Let’s do some predictive modeling next to see whether these can be substantial predictors of student’s performance.

# Predictive Modeling

One question we’re interested in answering is whether we are able to predict the math score from some of the existing variables.

We’ll start off by building a linear regression model, and slowly work toward building some tree models. We will focus more on predictive metrics over inferential ones in this demonstration.

Let’s use the `caret` package to fit our models. Let’s start with linear regression

``` r
# Setting seed for reproducibility
set.seed(202)

# We'll implement a repeated 10-fold cross-validation
mod_lm <- train(form = math_score ~ gender + parent_education + lunch + test_prep_course,
                data = dat,
                method = "lm",
                trControl = trainControl(method = "repeatedcv", 
                                         number = 10, 
                                         repeats = 5))
print(mod_lm)
```

    ## Linear Regression 
    ## 
    ## 1000 samples
    ##    4 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 5 times) 
    ## Summary of sample sizes: 899, 901, 899, 900, 900, 901, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared   MAE     
    ##   13.48271  0.2114937  10.94996
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

According to the regression output, it would seem that the model isn’t all that impressive, predicting only 0.21 of the variance in math score. The RMSE value indicates that future predictions would be off by approximately 13.48.

Let’s try fitting a decision tree next.

``` r
set.seed(202)

# Let's set up a tuning grid
dt_grid <- expand.grid(cp = seq(0, 20))

mod_dt <- train(form = math_score ~ gender + parent_education + lunch + test_prep_course,
                data = dat,
                method = "rpart",
                trControl = trainControl(method = "repeatedcv", 
                                         number = 10, 
                                         repeats = 5),
                tuneGrid = dt_grid)
```

    ## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo, :
    ## There were missing values in resampled performance measures.

``` r
print(mod_dt)
```

    ## CART 
    ## 
    ## 1000 samples
    ##    4 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 5 times) 
    ## Summary of sample sizes: 899, 901, 899, 900, 900, 901, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp  RMSE      Rsquared   MAE    
    ##    0  13.69880  0.1894902  11.0482
    ##    1  15.13055        NaN  12.0201
    ##    2  15.13055        NaN  12.0201
    ##    3  15.13055        NaN  12.0201
    ##    4  15.13055        NaN  12.0201
    ##    5  15.13055        NaN  12.0201
    ##    6  15.13055        NaN  12.0201
    ##    7  15.13055        NaN  12.0201
    ##    8  15.13055        NaN  12.0201
    ##    9  15.13055        NaN  12.0201
    ##   10  15.13055        NaN  12.0201
    ##   11  15.13055        NaN  12.0201
    ##   12  15.13055        NaN  12.0201
    ##   13  15.13055        NaN  12.0201
    ##   14  15.13055        NaN  12.0201
    ##   15  15.13055        NaN  12.0201
    ##   16  15.13055        NaN  12.0201
    ##   17  15.13055        NaN  12.0201
    ##   18  15.13055        NaN  12.0201
    ##   19  15.13055        NaN  12.0201
    ##   20  15.13055        NaN  12.0201
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was cp = 0.

Seems like model performance is slightly poorer in the decision tree. We see that model’s prediction deviates by 13.7.

That said, we can plot the decision tree to get a better understanding of which are the better predictors of math score. But first, we need to refit the decision tree using the `rpart` function.

``` r
mod_dt_basic <- rpart(formula = math_score ~ gender + parent_education + lunch + test_prep_course,
                      data = dat)

rpart.plot(mod_dt_basic, type = 4)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="672" />
Looking at the tree diagram above, we can see that the best predictor is lunch plan, whereby the standard lunch plan leads to better math score. Going down the branches, we can see that higher math scores is associated with parent’s education being more than high school and having completed the test preparation course. This fits with what we interpreted from the exploratory analyses, but it’s important to note that model performance is still somewhat poor, so more data is needed to validate our claims.

# Conclusion

Let’s take a look at our research questions once again:

1.  Are math, reading, and writing scores all correlated?

From our earlier analyses, we see that these scores are definitely very related to one another, which suggests that students need to apply both reading and writing scores when dealing with mathematical problems.

2.  How do students fare in these tests overall?

A simple series of histograms shows us that most students score above average in each domain, though we would need a much larger sample size to ascertain whether this is representative of the population.

3.  Can we attempt to model the relationship between math score and other variables?

We were able to build a regression and decision tree model to predict math score from several variables (gender, test preparation course, parents’ education, and lunch plan), but unfortunately, the predictive performance was not at all impressive.

That said, when examining the decision tree diagram, we see that the pattern of results seem to align with what we identified in the exploratory analysis (e.g., higher performance seems tied to the standard lunch plan). This might spur further analysis of the effectiveness of various education policies, as well as pave the way for more robust educational interventions.

Hope you’ve enjoyed this brief demo analysis!

[^1]: Adu-Gyamfi, K., Bosse, M.J., Faulconer, J. (2010). Assessing Understanding Through Reading and Writing in Mathematics. Retrieved from https://www.cimt.org.uk/journal/adugyamfi.pdf
