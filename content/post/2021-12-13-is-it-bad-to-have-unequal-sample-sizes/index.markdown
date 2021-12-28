---
title: Is it bad to have unequal sample sizes?
author: Timothy Liew
date: '2021-12-13'
slug: is-it-bad-to-have-unequal-sample-sizes
categories: []
tags: [statistics]
---

<script src="{{< blogdown/postref >}}index_files/core-js/shim.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/react/react.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/react/react-dom.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/reactwidget/react-tools.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index_files/reactable-binding/reactable.js"></script>
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>

<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />

# Introduction

In psychological research, we love doing comparisons in our experiments. For instance, a researcher might subject different participants to various experimental conditions (e.g., one group of participants receives an antidepressant drug, while another group receives a sugar pill), and then measure and compare depression scores between these groups. They might do this to investigate the effectiveness of the administered antidepressant.

![“Placebo Pills”](https://media.istockphoto.com/photos/small-white-pearls-and-a-bottle-on-a-blue-background-picture-id172174891?b=1&k=20&m=172174891&s=170667a&w=0&h=3LU03swJyrhmoRqQFTaMqtKYMm2QMQ-ERqiMBis9M8g=)

This isn’t limited to experiments either; another research might be interested in measuring and comparing agreeableness between males and females, or in contrasting between younger and older adults in terms of their temperament.

# The t-test

Another thing we love doing in psychology research is, of course, statistical modeling. In the case of making comparisons, we might want to run a t-test to verify the difference in say, depression scores between a treatment group and a control group. The utility of running statistical tests such as the t-test is that it allows us to make statements about whether the difference is *genuine* or simply due to *chance*.

-   There’s a lot of underlying statistical concepts that I’m not going to go into here, suffice it to say, we run a t-test to examine whether one group is *statistically different* from another group.

# Statistical Assumptions

Now here is where things get dicey: Like all parametric tests, the t-test is a bit fussy when it comes to the data you give it. You see, the t-test has certain *assumptions* about the data; for example, it assumes that the data is normally distributed across each group, and that the variance in each group is roughly equal.

Here are some plots to illustrate these assumptions:

<img src="{{< blogdown/postref >}}index_files/figure-html/assumptions 1-1.png" width="672" style="display: block; margin: auto;" />

The graph on the left indicates that our groups are normally distributed (i.e., the data in each group follows a bell-shaped curve); the graph on the right shows that the spread of the data in each group is roughly of the same size - indicating homogeneity/equality of variances.

Now here is what happens when both of these assumptions are violated:

<img src="{{< blogdown/postref >}}index_files/figure-html/assumptions 2-1.png" width="672" style="display: block; margin: auto;" />

On the graph to the left, we can see that neither groups follow a nice bell-shaped curve; to the right, we see that there is *heterogeneity* of variances, whereby the spread of the variance in group 1 is markedly wider than that of group 2.

# The case of unequal sample sizes

So you might be wondering why we’ve been talking so much about assumptions when this post is supposed to be about sample sizes; recall earlier that the t-test holds assumptions about the data - when these assumptions are violated (e.g., heterogeneity of variances), the t-test becomes the statistical equivalent of a petulant child and refuses to cooperate. We end up being less confident in whatever results the t-test outputs for us.

-   For example, when assumptions are violated, we are more prone to encounter *false alarms* (or in more statistical parlance, *Type I Errors*), whereby our t-test claims there is a significant difference between our groups, when in fact there isn’t.

Fortunately, when we have equal sample sizes, the t-test is said to be *robust* to assumption violations, which means that it can still give us reliable outputs despite these violations. Think of equal sample sizes as being a stern babysitter, who ensures our t-test does not misbehave.

Unfortunately, that also means that when we don’t have equal sample sizes, the t-test is vulnerable to assumption violations, which means that we can’t be rule out the possibility that the output is untrustworthy (e.g., there is a higher likelihood that we would commit a Type I error).

Let’s now take a look at just how much “protection” does equal sample sizes confer to our t-test (and many other statistical tests for that matter). To do so, we’re gonna dive deep into some simulations.

# Simulating equal/unequal sample sizes

For our simulation, we are going to leverage the wonderful [`SimDesign`](https://cran.r-project.org/web/packages/SimDesign/index.html) package, which provides a four-step framework in all sorts of simulations.

``` r
library(SimDesign)
```

The first step of our simulation is the Design stage, and this is where we define various different conditions that we want to examine. For example, we’ll be varying parameters such as sample size, group ratios, and variance ratios as well. This will allow us to examine how reliable our t-test is under differing conditions.

``` r
# First we want to define our design conditions
Design <- createDesign(mean_diff = c(0),
                       grp_ratio = c(1, 1.5, 2),
                       sample_size = c(30, 300, 3000),
                       var_ratio = c(1, 2))

library(reactable)
reactable(Design, 
          highlight = TRUE,
          outlined = TRUE,
          bordered = TRUE,
          # Centre-align columns
          defaultColDef = colDef(align = "center"),
          # Apply some themes (i.e., text color, highlight color, typeface)
          theme = reactableTheme(color = "#000000",
                                 highlightColor = "#f0f5f9",
                                 style = list(fontFamily = "Cambria")))
```

<div id="htmlwidget-1" class="reactable html-widget" style="width:auto;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"mean_diff":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"grp_ratio":[1,1.5,2,1,1.5,2,1,1.5,2,1,1.5,2,1,1.5,2,1,1.5,2],"sample_size":[30,30,30,300,300,300,3000,3000,3000,30,30,30,300,300,300,3000,3000,3000],"var_ratio":[1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2]},"columns":[{"accessor":"mean_diff","name":"mean_diff","type":"numeric","align":"center"},{"accessor":"grp_ratio","name":"grp_ratio","type":"numeric","align":"center"},{"accessor":"sample_size","name":"sample_size","type":"numeric","align":"center"},{"accessor":"var_ratio","name":"var_ratio","type":"numeric","align":"center"}],"defaultPageSize":10,"paginationType":"numbers","showPageInfo":true,"minRows":1,"highlight":true,"outlined":true,"bordered":true,"theme":{"color":"#000000","highlightColor":"#f0f5f9","style":{"fontFamily":"Cambria"}},"dataKey":"84a8ce826ed45f3dcee91800c488ade0","key":"84a8ce826ed45f3dcee91800c488ade0"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>

From the table above, you can see that we have 18 different conditions to simulate over. For example, the first row corresponds to a condition where there is zero mean difference, the group ratio is 1:1 (i.e., equal sample sizes), total sample size is 30, and the variance ratio is 1:1 (i.e., equal variances across the two groups).

The next step, which is the Generate stage, is to write a function that essentially creates the necessary data under the differing conditions that we obtained from the Design stage.[^1].

``` r
Generate <- function(condition, fixed_objects = NULL) {
  # Define a control variance
  control_var <- 1
  
  # Let's create our group data
  Attach(condition) # This makes referencing easier
  
  # We define the group sizes based on the following logic:
  # Group 1 = Sample Size / (Allocation Ratio, e.g., 1.5) + 1
  # Group 2 = Sample Size - Group 1
  group1 <- sample_size/(grp_ratio + 1)
  group2 <- sample_size - group1
  
  sample1 <- rnorm(n = group1, mean = 0, sd = sqrt(control_var*var_ratio))
  sample2 <- rnorm(n = group2, mean = mean_diff, sd = sqrt(control_var))
  
  # Store our data in a data frame
  dat <- data.frame (group = c (rep("group1" , group1), rep("group2" , group2)),
                     score = c(sample1, sample2))
  dat
}
```

Let’s see whether this work for one of the conditions

``` r
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
Generate(Design[1,]) %>% 
  mutate(score = round(score, 2)) %>% 
  group_by(group) %>% 
  sample_n(5) %>% 
  kable() %>% 
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
group
</th>
<th style="text-align:right;">
score
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
group1
</td>
<td style="text-align:right;">
-0.11
</td>
</tr>
<tr>
<td style="text-align:left;">
group1
</td>
<td style="text-align:right;">
-1.15
</td>
</tr>
<tr>
<td style="text-align:left;">
group1
</td>
<td style="text-align:right;">
2.81
</td>
</tr>
<tr>
<td style="text-align:left;">
group1
</td>
<td style="text-align:right;">
-0.92
</td>
</tr>
<tr>
<td style="text-align:left;">
group1
</td>
<td style="text-align:right;">
1.03
</td>
</tr>
<tr>
<td style="text-align:left;">
group2
</td>
<td style="text-align:right;">
0.42
</td>
</tr>
<tr>
<td style="text-align:left;">
group2
</td>
<td style="text-align:right;">
-1.66
</td>
</tr>
<tr>
<td style="text-align:left;">
group2
</td>
<td style="text-align:right;">
0.63
</td>
</tr>
<tr>
<td style="text-align:left;">
group2
</td>
<td style="text-align:right;">
-0.74
</td>
</tr>
<tr>
<td style="text-align:left;">
group2
</td>
<td style="text-align:right;">
0.51
</td>
</tr>
</tbody>
</table>

Now that we have the data for our different conditions, it’s time to run the t-test. Essentially, we write a function that runs a Student’s t-test, and then extract the relevant information such as p-values and mean differences.

``` r
# We'll create a function that runs a t-test and extract the relevant information
Analyse <- function(condition, dat, fixed_objects = NULL) {
  # First we set an object to indicate to the t-test whether we want Welch or Student t-test
  equal_var <- TRUE
  
  # Run our statistical test
  test_results <- t.test(formula = score ~ group, data = dat, var.equal = equal_var)
  
  # Extract the target information and renames the columns
  ret <- broom::tidy(test_results)
  colnames(ret)[c(1,2,3,6)] <- c("est_mean_diff", "mean1", "mean2", "df")
  
  ret
}
```

It’s important to remember that we’re not interested in the results of a single analysis (e.g., the p-value of a single t-test). In simulations, we want to be able to say something about the long-term behaviour of a particular statistic.

For example, we are going to calculate the average number of p-values which are below our designated alpha level (e.g., 0.05), which would tell us the overall Type I Error/False Alarm rate. Luckily, the `simDesign` package makes things easy; we write a Summarise function that easily gives us the Type I error rate of the different conditions we specified earlier.

``` r
# Next we want some kind of summary statistics
Summarise <- function(condition, results, fixed_objects = NULL) {
  # Explicitly set the alpha level
  alpha_level <- 0.05
  
  # Extract the necessary information, including EDR.
  # Note that this will be replicated in runSim, so you need to make sure that whatever information is "averaged" in some way to give you a single statistic
  ret <- data.frame(edr = EDR(p = results$p.value, alpha = alpha_level),
                    mdiff = mean(results$est_mean_diff))
  ret
}
```

The next part essentially combines all of the different steps we’ve described thus far and runs them for a number of replication (e.g., 1000 replications). That’s the beauty of simulations, in that it’s akin to you running the same experiment 1000 times!

``` r
# This will run our simulation for a number of replications
set.seed(201)
res <- runSimulation(design = Design, 
                     replications = 1000, 
                     generate = Generate, 
                     analyse = Analyse, 
                     summarise = Summarise,
                     save_results = TRUE)

# Note: To save time, I've saved the results as a file to be loaded into R whenever I want
saveRDS(res,file = "mcs_res.rds")
```

# Visualisaing our results

Admittedly, everything I described previously might seem a bit abstract/esoteric; luckily, we can leverage the power of visualisations to make things a bit more understandable.

Recall that our goal was to examine how the robustness of the t-test changes when we have unequal sample sizes, and whether this affects the reliability of our results. Let’s first take a look at the case where there is homogeneity of variances.

``` r
# I'll define some new labels for the facets here
grp_ratio.labs <- c("1:1 Ratio", "1:1.5 Ratio", "1:2 Ratio")
names(grp_ratio.labs) <- c("1", "1.5", "2")

# Let's plot the Type I error rate for the data with equal variances
res %>% 
  dplyr::filter(var_ratio == 1) %>% 
  mutate(grp_ratio = as.factor(grp_ratio)) %>% 
  ggplot(aes(x = as.factor(sample_size), y = edr)) + 
  geom_col(aes(fill = grp_ratio), show.legend = FALSE) + 
  geom_hline(yintercept = 0.05, linetype = "dashed", alpha = 0.3) +
  facet_wrap(~grp_ratio, nrow = 3, labeller = labeller(grp_ratio = grp_ratio.labs)) + 
  theme_light() + 
  # Labels the Type I error for each bar graph at the centre of the bar
  geom_text(aes(label = edr), position = position_stack(vjust = 0.5), size = 3.5) +
  labs(title = "Type I Error Rates",
       subtitle = "Homogeneity of variance is maintained across all simulations",
       x = "Sample Size", 
       y = "Type I Error Rate",
       caption = "Bar chart representing Type I error rates for different combinations of sample sizes and group ratios.
       \nThe horizontal dashed line indicates the chosen alpha level.") +
  # Add a secondary label axis for the facets
  scale_y_continuous(sec.axis = sec_axis(~ . , 
                                         name = "Group Ratios", 
                                         labels = NULL, 
                                         breaks = NULL)) +
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.caption = element_text(hjust = 0, face = "italic"),
        plot.caption.position = "plot",
        strip.text.x = element_text(size = 10, color = "black"),
        panel.grid = element_blank())
```

<img src="{{< blogdown/postref >}}index_files/figure-html/plot 1-1.png" width="672" />
As we can see, when homogeneity of variance is assumed, the sample size ratio doesn’t matter quite as much; while the type I error rate is very tightly controlled when you have 1:1 group sizes, the simulations indicate that the error rate is relatively close to the alpha level even when you have unequal sample sizes. This property also seems to be “strengthened” when you increase the sample size.

In other words, unequal sample size does not pose any major statistical issues *provided that your assumptions are met.*

So what happens when your assumptions *are* violated? Time to find out!

``` r
# I'll define some new labels for the facets here
grp_ratio.labs <- c("1:1 Ratio", "1:1.5 Ratio", "1:2 Ratio")
names(grp_ratio.labs) <- c("1", "1.5", "2")

# Let's plot the Type I error rate for the data with unequal variances
res %>% 
  dplyr::filter(var_ratio == 2) %>% 
  mutate(grp_ratio = as.factor(grp_ratio)) %>% 
  ggplot(aes(x = as.factor(sample_size), y = edr)) + 
  geom_col(aes(fill = grp_ratio), show.legend = FALSE) + 
  geom_hline(yintercept = 0.05, linetype = "dashed", alpha = 0.3) +
  facet_wrap(~grp_ratio, nrow = 3, labeller = labeller(grp_ratio = grp_ratio.labs)) + 
  theme_light() + 
  # Labels the Type I error for each bar graph at the centre of the bar
  geom_text(aes(label = edr), position = position_stack(vjust = 0.5), size = 3.5) +
  labs(title = "Type I Error Rates",
       subtitle = "All simulations are run under heterogenous variances",
       x = "Sample Size", 
       y = "Type I Error Rate",
       caption = "Bar chart representing Type I error rates for different combinations of sample sizes and group ratios.
       \nThe horizontal dashed line indicates the chosen alpha level.") +
  # Add a secondary label axis for the facets
  scale_y_continuous(sec.axis = sec_axis(~ . , 
                                         name = "Group Ratios", 
                                         labels = NULL, 
                                         breaks = NULL)) +
  scale_fill_brewer(palette = "Set2") + 
  theme(plot.caption = element_text(hjust = 0, face = "italic"),
        plot.caption.position = "plot",
        strip.text.x = element_text(size = 10, color = "black"),
        panel.grid = element_blank())
```

<img src="{{< blogdown/postref >}}index_files/figure-html/plot 2-1.png" width="672" />
Here is when all hell breaks loose; when you have heterogeneity of variances, the group ratios matter a lot. For example, with equal sample sizes, we can see that the Type I error rate is maintained comfortably at our alpha level of 0.05 at all sample sizes.

However, as the sample sizes becomes more unequal, the Type I error rate increases, and not even having 3000 observations helps ameliorate this situation.

In a nutshell, when you data does not meet the parametric assumptions, there is a greater need to strive to achieve equal group sizes, as that is what helps prevent running into a Type I Error/False Alarm.

# Conclusion

We’re frequently told that equal group sizes are important, which they are! You wouldn’t want to claim that males have lower conscientiousness than females when you have 5 males versus 95 females in your sample. But hopefully, this post has given you a better understanding of the role of equal group sizes in statistical modeling.

We see that when the assumptions of our statistical tests are met, unequal sample sizes does not affect the reliability of our findings as much (i.e., Type I error rates are still within acceptable ranges). In contrast, when assumptions are violated (which unfortunately is more common in the real world), equal sample sizes becomes a “shield” of sorts that ensures our findings are still reliable.

There’s a lot more to explore with regard to how we navigate assumption violations in statistical modelling, but let’s save that for another post ;)

[^1]: The effect of heterogeneous variances will differ depending on which of the two groups have the larger variance. In this simulation, I’m only applying the larger variance to the group with the smaller sample size, but you’re welcome to try seeing what happens when you apply the larger variance to the group with the bigger sample size
