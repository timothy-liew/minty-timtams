---
title: Using SQL in R
author: Timothy Liew
date: '2022-05-06'
slug: using-sql-in-r
categories: []
tags: [SQL, dplyr]
cover:
    image: "https://images.unsplash.com/photo-1544383835-bda2bc66a55d?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=836&q=80"
    # can also paste direct link from external site
    # ex. https://i.ibb.co/K0HVPBd/paper-mod-profilemode.png
    alt: "<alt text>"
    caption: "Now which drawer has my sandwich again?"
    relative: false # To use relative path for cover image, used in hugo Page-bundles

---

<script src="{{< blogdown/postref >}}index_files/core-js/shim.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/react/react.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/react/react-dom.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/reactwidget/react-tools.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index_files/reactable-binding/reactable.js"></script>

# SQL and R

I have been learning a bit of SQL over the last few months, and the whole data wrangling aspect of it really appeals to me (it’s kind of like trying to wrangle a Rubik’s cube until you get the right configuration, only I don’t end up crying halfway).

Of course, being the hardcore R fan that I am, I wanted to see whether there were ways to implement SQL queries in R Studio, and I wasn’t disappointed at all. While you’d probably not want to rely on R Studio exclusively to run SQL queries (I imagine issues will crop up when your dataset is gargantuan), it’s still nice to see these two languages meet in the middle, so to speak.

# Baby Names

In this post, we’ll take a quick look at how to create databases and run SQL queries in R Studio. We’ll then smoothly transition to doing some quick visualisations using R.

We’ll work with the baby name data (provided by the Social Security Administration) from the `babynames` package (very aptly named). The name data includes all names with at least 5 uses.

``` r
library(babynames)
library(tidyverse)
glimpse(babynames)
```

    ## Rows: 1,924,665
    ## Columns: 5
    ## $ year <dbl> 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880,…
    ## $ sex  <chr> "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", …
    ## $ name <chr> "Mary", "Anna", "Emma", "Elizabeth", "Minnie", "Margaret", "Ida",…
    ## $ n    <int> 7065, 2604, 2003, 1939, 1746, 1578, 1472, 1414, 1320, 1288, 1258,…
    ## $ prop <dbl> 0.07238359, 0.02667896, 0.02052149, 0.01986579, 0.01788843, 0.016…

# SQLite Database

Next thing we want to do is to create a database. We’ll be relying on functions from the `DBI` and `RSQLite` packages.

``` r
library(DBI)
library(RSQLite)
```

Usually, we would create a new database with something like the following, which would read in an existing file.

``` r
mydb <- dbConnect(drv = RSQLite::SQLite(), "my-db.sqlite")
dbDisconnect(mydb)
```

In this example, however, we’ll be creating a new database by writing the datasets from `babynames` into a temporary database.

``` r
# This will create a temporary database 
mydb <- dbConnect(RSQLite::SQLite(), "")
dbWriteTable(conn = mydb, name = "babynames", value = babynames::babynames)
dbWriteTable(mydb, "applicants", babynames::applicants)
dbWriteTable(mydb, "births", babynames::births)

# dbWriteTable throws an error for lifetables because of a duplicate column, so we'll fix that first before writing it into the database
lifetables <- lifetables %>% 
  rename("age-span" = Lx) 

dbWriteTable(mydb, "lifetables", lifetables)

dbListTables(mydb)
```

    ## [1] "applicants" "babynames"  "births"     "lifetables"

# SQL Queries

All right, now for the fun part. Let’s try issuing some SQL queries with `dbGetQuery`. We can find out the top ten male names in 2017. Remember to put quotation marks around your SQL statements!

``` r
dbGetQuery(mydb,
           "SELECT * 
           FROM babynames
           WHERE year = 2017 AND sex = 'M'
           ORDER BY n desc
           LIMIT 10")
```

    ##    year sex     name     n       prop
    ## 1  2017   M     Liam 18728 0.00953909
    ## 2  2017   M     Noah 18326 0.00933433
    ## 3  2017   M  William 14904 0.00759134
    ## 4  2017   M    James 14232 0.00724906
    ## 5  2017   M    Logan 13974 0.00711764
    ## 6  2017   M Benjamin 13733 0.00699489
    ## 7  2017   M    Mason 13502 0.00687723
    ## 8  2017   M   Elijah 13268 0.00675804
    ## 9  2017   M   Oliver 13141 0.00669336
    ## 10 2017   M    Jacob 13106 0.00667553

What about the top ten male names across all years? (This time we’ll only display the names and their frequencies)

``` r
dbGetQuery(mydb,
           "SELECT name, SUM(n) as frequency 
           FROM babynames
           WHERE sex = 'M'
           GROUP BY name
           ORDER BY frequency desc
           LIMIT 10")
```

    ##       name frequency
    ## 1    James   5150472
    ## 2     John   5115466
    ## 3   Robert   4814815
    ## 4  Michael   4350824
    ## 5  William   4102604
    ## 6    David   3611329
    ## 7   Joseph   2603445
    ## 8  Richard   2563082
    ## 9  Charles   2386048
    ## 10  Thomas   2304948

Not too surprising that James and John are the most common male name.

## Subqueries/Joins

Okay time for something just a little more complex. Let’s say we want to combine see the total number of SSN applications and total births for each year. We will need to use an INNER JOIN to combine two tables.

``` r
dbGetQuery(mydb,
           "SELECT a.year, SUM(a.n_all) as applicants, b.births
           FROM applicants a INNER JOIN births b
            ON a.year = b.year
           GROUP BY a.year
           LIMIT 10")
```

    ##    year applicants  births
    ## 1  1909     544963 2718000
    ## 2  1910     628047 2777000
    ## 3  1911     683213 2809000
    ## 4  1912    1038169 2840000
    ## 5  1913    1191160 2869000
    ## 6  1914    1479944 2966000
    ## 7  1915    1904816 2965000
    ## 8  1916    2008978 2964000
    ## 9  1917    2083023 2944000
    ## 10 1918    2251039 2948000

Note that we had to sum up the number of male and female applicants prior to joining the tables.

Okay, so what if for some reason I wanted to indicate when the number of applicants exceeded the number of births each year? We can use a combination of subqueries and CASE WHEN statements to accomplish this goal.

``` r
# We'll insert the results into a reactable table
options(reactable.theme = reactable::reactableTheme(
  color = "#000000",
  highlightColor = "#f0f5f9",
  style = list(fontFamily = "Cambria"),
  searchInputStyle = list(width = "100%")
))

dbGetQuery(mydb,
           "SELECT *, 
          CASE 
            WHEN applicants > births THEN 'YES' ELSE 'NO'
            END AS `A>B` 
          FROM (SELECT a.year, SUM(a.n_all) as applicants, b.births
          FROM applicants a INNER JOIN births b
            ON a.year = b.year
            GROUP BY a.year)") %>% 
  # Let's input the data into a reactable
  reactable::reactable(filterable = TRUE,
                       highlight = TRUE,
                       outlined = TRUE,
                       bordered = TRUE)
```

<div id="htmlwidget-1" class="reactable html-widget" style="width:auto;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"year":[1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017],"applicants":[544963,628047,683213,1038169,1191160,1479944,1904816,2008978,2083023,2251039,2189985,2344856,2417651,2372817,2384785,2464772,2414520,2375658,2398252,2336634,2265059,2295845,2173193,2180511,2065944,2143963,2156093,2141661,2195245,2277661,2267205,2367271,2500511,2798464,2889585,2755423,2717349,3263065,3675205,3525140,3557327,3577685,3759105,3876475,3930355,4059252,4094288,4203842,4285006,4218059,4244767,4245537,4232356,4129034,4053127,3984578,3722589,3573492,3496473,3485618,3592701,3737511,3570746,3287139,3168246,3196878,3183767,3205089,3354785,3352888,3514958,3635385,3650650,3701111,3652685,3679115,3769725,3766004,3823302,3923909,4087345,4205122,4152308,4102917,4036135,3986970,3932213,3920254,3906145,3965144,3984550,4082272,4047462,4039488,4105458,4128775,4154208,4279886,4328220,4259382,4141930,4010406,3963636,3962129,3940587,3996030,3983648,3946228,3838189],"births":[2718000,2777000,2809000,2840000,2869000,2966000,2965000,2964000,2944000,2948000,2740000,2950000,3055000,2882000,2910000,2979000,2909000,2839000,2802000,2674000,2582000,2618000,2506000,2440000,2307000,2396000,2377000,2355000,2413000,2496000,2466000,2559000,2703000,2989000,3104000,2939000,2858000,3411000,3817000,3637000,3649000,3632000,3823000,3913000,3965000,4078000,4097000,4218000,4300000,4255000,4244796,4257850,4268326,4167362,4098020,4027490,3760358,3606274,3520959,3501564,3600206,3731386,3555970,3258411,3136965,3159958,3144198,3167788,3326632,3333279,3494398,3612258,3629238,3680537,3638933,3669141,3760561,3756547,3809394,3909510,4040958,4158212,4110907,4065014,4000240,3952767,3899589,3891494,3880894,3941553,3959417,4059000,4026000,4021726,4089950,4112052,4138349,4265555,4316233,4247694,4130665,3999386,3953590,3952841,3932181,3988076,3978497,3945875,3855500],"A>B":["NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","NO"]},"columns":[{"accessor":"year","name":"year","type":"numeric"},{"accessor":"applicants","name":"applicants","type":"numeric"},{"accessor":"births","name":"births","type":"numeric"},{"accessor":"A>B","name":"A>B","type":"character"}],"filterable":true,"defaultPageSize":10,"paginationType":"numbers","showPageInfo":true,"minRows":1,"highlight":true,"outlined":true,"bordered":true,"theme":{"color":"#000000","highlightColor":"#f0f5f9","style":{"fontFamily":"Cambria"},"searchInputStyle":{"width":"100%"}},"dataKey":"0b6152932adbec59157080f9e95326a9","key":"0b6152932adbec59157080f9e95326a9"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>

# Data Visualisation

Now that we’ve warmed up from going through some basic SQL queries, let’s try to put it into practice by wrangling our dataset and then creating a basic visualisation.

One thing we can do is to visualise the top male and female names across the years. First, we need to wrangle our data to get that information. Here we will see some subquery action.

``` r
# Note: We need a subquery here because window functions can't be used when you have WHERE, GROUP BY, or HAVING; we use subqueries to get around this limitation

# Inner query: We use a window function to sort the names in descending order (grouped by sex)
# Outer query: We select everything from the subqueries, choosing only the top names (automatically grouped by  and year thanks to the inner query)
# We get the total frequency for each name for both sex
top_names <- dbGetQuery(mydb,
                         "
                         SELECT sex, name, year, n
                         FROM 
                         (SELECT *,
                         RANK() OVER (PARTITION BY sex, year ORDER by n DESC) AS rank
                         FROM babynames)
                         WHERE rank = 1
                         ")
```

## Visualising most popular names

Let’s see how we can visualise the trend of the top names across time. We can show the top names every 5 years instead of every single year to avoid overplotting.

``` r
decade_vector <- tibble(decade = seq(1880, 2017, 10))

# Define names for facet labels
gender_labels <- c(
  "F" = "Female Names",
  "M" = "Male Names"
)

library(viridis)
top_names %>% 
  filter(year %in% decade_vector$decade) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = year, y = n, color = year), size = 1.5) +
  geom_point() + 
  geom_segment(aes(x = year, xend = year, y = 0, yend = n), size = 1) + 
  geom_text(aes(label = name), size = 3.5, nudge_y = 8000) + 
  ggthemes::theme_clean() + 
  # The begin and end arguments help define the range of colors we want (i.e., I don't want yellow which is at the end of the viridis color range)
  scale_color_viridis(discrete = TRUE, option = "B", begin = 0, end = 0.8) + 
  theme(legend.position = "None", 
        strip.text.x = element_text(size = 12, face = "bold")) + 
  labs(title = "Top Male and Female Names",
       y = NULL,
       x = "Year") + 
  facet_wrap(~sex, nrow = 2, labeller = as_labeller(gender_labels))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="960" />
We can see that the names Mary and John were the most popular female and male names for several decades, after which various other names took the top spot for most popular baby names.

# SQL to R and back

One other cool thing I discovered is the existence of packages that facilitate translating SQL code to R code and vice versa. For example, `queryparser` provides a function that parses an SQL query into a list with the corresponding R expressions.

-   You can even set `tidyverse = TRUE` to see the operations using tidyverse syntax.

``` r
library(queryparser)
parse_query("SELECT * 
            FROM babynames
            WHERE year = 2017 AND sex = 'M'
            ORDER BY n desc
            LIMIT 10", tidyverse = TRUE)
```

    ## $select
    ## $select[[1]]
    ## dplyr::everything()
    ## 
    ## 
    ## $from
    ## $from[[1]]
    ## babynames
    ## 
    ## 
    ## $where
    ## $where[[1]]
    ## year == 2017 & sex == "M"
    ## 
    ## 
    ## $order_by
    ## $order_by[[1]]
    ## dplyr::desc(n)
    ## 
    ## 
    ## $limit
    ## $limit[[1]]
    ## [1] 10

Another interesting packages is `dbplyr`, which translates `dplyr` syntax into SQL queries.

``` r
library(dbplyr)
tbl(mydb, "babynames") %>% 
  filter(year == 2017 & sex == "M") %>% 
  slice_max(n = 10, order_by = n) %>% 
  show_query()
```

    ## <SQL>
    ## SELECT `year`, `sex`, `name`, `n`, `prop`
    ## FROM (SELECT `year`, `sex`, `name`, `n`, `prop`, RANK() OVER (ORDER BY `n` DESC) AS `q01`
    ## FROM `babynames`
    ## WHERE (`year` = 2017.0 AND `sex` = 'M'))
    ## WHERE (`q01` <= 10)

Ordinarily I wouldn’t think this terribly useful, but it might prove helpful when trying to teach SQL to someone who has prior experience coding in R. Being more familiar with `dplyr` myself, I also imagine it might be somewhat useful in helping to figure out SQL queries that you know how to do in `dplyr`.
