Emerging trends in the reporting of the coronavirus
================

Context
-------

As of today, we are currently in the midst of one of the most disruptive pandemics in history.

A lot of attention has rightly been placed on the epidemiological effects of the virus. However, citizens rely on the media to deliver this crucial information. To be clear, the media has always played a pivotal role in shaping how society thinks - this is especially true in recent decades where *social* media has exploded in popularity.

In these chaotic times it is tempting to think that newspapers and digital media platforms wield even more power than they normally do. But, just like us, they are subject to the emotional ups and downs of their surrounding environment.

This document explores trends applicable to the coronavirus, as reported by the New York Times, using article metadata kindly provided by the [New York Times API](https://developer.nytimes.com/apis). My sincerest of gratitudes goes to them for making this data available to developers and the research community.

Load required packages
----------------------

For this script, we will leverage the following libraries:

``` r
library(tidyverse)
library(scales) # for percentage format on axis labels
library(tidytext)
library(lubridate) # for working with dates and times
library(zoo) # for the rollmean() function
library(topicmodels) # for LDA implementation
library(broom) # for tidying model outputs

theme_set(theme_light())
```

Import article metadata
-----------------------

Note that this data was originally acquired via the [New York Times API](https://developer.nytimes.com/apis). Once you sign up, you will be issued with an API key which you can use to access metadata on thousands of different article types.

In this case I have used the 'Archive API' which allows me to acquire all NYT article metadata (e.g. headline, article description and more) for a given month - I have created a simple script to extract this data via the R package `jsonlite` for multiple months (January to April as of today). I won't include my API key here (for obvious reasons) but you'll need to create a function like this to access the data:

``` r
## Archive Search API call from R ##
nyt_archive_search <- function(api_key = "<INSERT YOUR KEY HERE>", year, month) {
  
  if (!stringr::str_detect(year, regex("^[0-9]{4}$")) | !stringr::str_detect(month, regex("^[0-9]{1}$"))) {
    stop("Year and month inputs must be numeric e.g. '2020' and '1' (for January) respectively", call. = FALSE)
  }
  
  jsonlite::fromJSON(paste0("http://api.nytimes.com/svc/archive/v1/", year, "/", month, ".json?api-key=", api_key),
           flatten = TRUE) %>%
    data.frame() %>%
    dplyr::transmute(article_id = row_number(),
                     article_headline = response.docs.headline.main,
                     article_description = response.docs.abstract, 
                     source = response.docs.source,
                     publication_date = as.Date(str_sub(response.docs.pub_date, start = 1L, end = 10L)),
                     section = response.docs.section_name,
                     word_count = response.docs.word_count) %>%
    tibble::as_tibble()
}
```

I am in the process of compiling a small package to allow other users to critique and leverage the code like this so that you can do the same.

I saved off my extraction to a CSV file for use in what follows:

``` r
# Clearly, the following path will not work on your local system (amend accordingly)
archive_2020 <- "/Users/Johnny/Desktop/Data Science/R/Project NY Times/Extractor script/article-extractions/2020_1-to-4-extract.csv"
archive_raw <- read_csv(file = archive_2020)

glimpse(archive_raw)
```

    ## Rows: 28,535
    ## Columns: 7
    ## $ article_id          <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1…
    ## $ article_headline    <chr> "‘A Different Era’: Anti-Semitic Crimes, and…
    ## $ article_description <chr> "Attacks have been traditionally underreport…
    ## $ source              <chr> "The New York Times", "The New York Times", …
    ## $ publication_date    <date> 2020-01-03, 2020-01-01, 2020-01-02, 2020-01…
    ## $ section             <chr> "U.S.", "Arts", "Movies", "Opinion", "Fashio…
    ## $ word_count          <dbl> 1015, 1015, 2076, 1145, 1958, 1422, 1220, 13…

As you can see, we have a total of 28,535 observations split across 7 different variables, spanning the period from 1 January 2020 to 30 April 2020.

Data pre-processing
-------------------

The data has already undergone a certain level of pre-processing as the API returns data in a JSON format, which had to be engineered back into the `tibble` format.

However, there is still work to do: what we would like, ideally, is a dataset where each word occupies one row (this is often called an 'unnested' dataframe) - this will make it easier to count words, apply models and perform correlation analyses later on. We'll also want to split the `publication_date` variable into its constituents (year, month, day) as this will make it easier to analyse trends by each respective constituent. We will also calculate a `cum_time_elapsed` variable to indicate when each article was released relative to the new year.

We will make the slightly bold decision to unite the headline and description into one field. This is based on a few assumptions: (i) there is no significant difference in the nature of a headline as compared with an article description *in terms of its content*; and (ii) more words is better than less (why analyse *only* the description field when we could combine it with the headline field?). By doing this we are essentially creating a 'bag of words' column.

We will retain a copy of the data without the unnesting procedure applied just incase we need it.

``` r
new_year <- date("2020-01-01")

archive_clean <- archive_raw %>%
  select(-article_id) %>%
  distinct() %>% # there are duplicates in the data that we need to get rid of
  mutate(article_id = row_number(),
         publication_day = day(publication_date),
         publication_month = month(publication_date),
         publication_year = year(publication_date),
         cum_time_elapsed = publication_date - new_year) %>%
  unite(article_headline, article_description, col = "article_content", sep = " ---- ", remove = F) %>%
  mutate(covid_flag = str_detect(article_content, regex("(coronavirus|covid)", ignore_case = T)))

archive_unnested <- archive_clean %>%
  unnest_tokens(output = "article_term", input = "article_content") %>%
  anti_join(tidytext::stop_words, by = c("article_term" = "word")) %>%
  filter(!str_detect(article_term, "[0-9]"))
```

Exploratory analysis
--------------------

Let's first examine how words grow in usage over time. More specifically, let us direct our attention to the 'coronavirus' and track its trajectory from the new year:

``` r
coronavirus_theme <- theme(plot.title = element_text(face = "bold"),
                           axis.title = element_text(size = 10),
                           panel.grid.minor = element_blank(),
                           panel.grid.major.x = element_blank(),
                           panel.grid.major.y = element_line(colour = "grey50"),
                           axis.ticks = element_blank(),
                           plot.subtitle = element_text(size = 10),
                           plot.caption = element_text(colour = "grey70"),
                           plot.background = element_rect(fill = "grey30"),
                           panel.background = element_rect(fill = "grey30"),
                           panel.border = element_blank(),
                           text = element_text(colour = "grey90"),
                           axis.text = element_text(colour = "grey90"))

arrows <- tibble(x_start = c(date("2020-01-15"), date("2020-04-01")),
                 y_start = c(30, 90),
                 x_end = c(date("2020-01-09"), date("2020-03-18")),
                 y_end = c(5, 90))

archive_clean %>% 
  filter(covid_flag) %>% 
  count(publication_date) %>%
  arrange(publication_date) %>%
  mutate(n_ma = rollmean(x = n, k = 15, fill = list(NA, NULL, NA))) %>%
  ggplot() +
  geom_point(mapping = aes(x = publication_date, y = n), alpha = 0.25, show.legend = F, colour = "grey90") +
  geom_line(mapping = aes(x = publication_date, y = n_ma), colour = "#00E6FF") +
  geom_vline(mapping = aes(xintercept = date("2020-03-17")), lty = "dotted", colour = "grey90") +
  geom_curve(data = arrows, mapping = aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
             arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
             colour = "grey90", curvature = 0.4, alpha = 0.25) +
  annotate("text", x = date("2020-01-15"), y = 50, size = 2.8, colour = "grey90", label = "Headline on 9 January 2020:", fontface = 2, hjust = 0) +
  annotate("text", x = date("2020-01-15"), y = 40, size = 2.8, colour = "grey90", label = "'The new coronavirus doesn't appear to be \n readily spread by humans, but researchers\n caution that more study is needed'", hjust = 0, fontface = 1) +
  annotate("text", x = date("2020-04-04"), y = 90, size = 2.8, colour = "grey90", label = "Multiple US states go into\nlockdown following weeks of\ngovernment inaction", hjust = 0, fontface = 1) +
  labs(x = NULL,
       y = "Articles related to COVID-19",
       caption = "Source: New York Times API",
       title = "Coronavirus reporting timeline",
       subtitle = "How the number of articles on the coronavirus has changed over time") +
  coronavirus_theme
```

![](emerging-trends-analysis_files/figure-markdown_github/unnamed-chunk-5-1.png)

Each of these dots corresponds to the incremental number of articles released each day from the new year up until the present day (almost, at least).

Things get a little more interesting if we present numbers on a *cumulative* basis:

``` r
archive_clean %>% 
  filter(covid_flag) %>%
  count(publication_date, name = "article_count") %>%
  arrange(publication_date) %>%
  mutate(cum_count = cumsum(article_count),
         prop = cum_count / max(cum_count)) %>%
  ggplot(aes(publication_date, prop)) +
  geom_line(size = 1, colour = "grey90") +
  geom_smooth(alpha = 0.08, colour = "#89A2FF", method = 'glm', method.args = list(family = "binomial"), lty = 2) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = NULL,
       y = "Cumulative proportion (%) of COVID-19 articles",
       title = "Growth in cumulative number of coronavirus articles",
       subtitle = "The 'viral' nature of words can be captured by a 'logistic growth' model (dashed line)",
       caption = "Source: New York Times API") +
  coronavirus_theme
```

    ## `geom_smooth()` using formula 'y ~ x'

![](emerging-trends-analysis_files/figure-markdown_github/unnamed-chunk-6-1.png)

This pattern is ubiquitous in nature and it has a name: 'logistic growth'. Back in 1838 Pierre-Francois Verhulst applied a logistic growth model to population dynamics on the basis that the the rate of reproduction within a given society is proportional to both the existing population and the amount of available resources. That first point means that populations grow exponentially at first. However, a bottleneck is incurred at a certain point in time whereby members of the population compete with each other for critical resources, such as food or living space. Ultimately, this causes a plateauing effect to occur, which can be observed at approximately April 1 in the graph presented above.

Growing and shrinking themes
----------------------------

There are two types of trends that we want to focus on:

1.  **Growth**: which particular topics are becoming increasingly 'important' as time goes on?
2.  **Shrinkage**: which topics have fallen off the radar?

One possible way of modelling growth (or equivalently, shrinkage) in reporting trends is through the use of a logistic growth (logistic regression) model. Logistic regression models are typically applied in the context of classification problems in machine learning. But, it might surprise some practitioners to know that this statistical model was originally employed in the biological domain.

In this context, it is used to model variables which exhibit an initial period of exponential growth followed by a period of plateau from a certain point in time. The latter period of 'tailing off' is driven by some kind of constraint on the variable of interest. In epidemiology, for example, one can examine the initial phase of viral spread and note that it is indeed exponential but only up until a certain point - eventually, everyone who is infected either dies or people recover and gain immunity leading to a 'tailing off' in the initial spread.

Certain topics as reported in the media can be thought of in a similar way: as the coronavirus emerges some topics will become more newsworthy and increase exponentially up until a certain point: reporters cannot, of course, continue to discuss such topics at an exponential rate since there is a limited number of reporters at any one time and other topics will inevitably emerge to fill their place. Indeed, we have just observed this feature in a particular choice of topic, namely: 'coronavirus'.

Our plan is to regress an output variable, representing the relative frequency of a term in a given day, against time elapsed since the new year. With regards to the former, we can think of the number of times a given term appears in a given day as the number of 'successes' and the number of times that *any* term appears in a given day (i.e. the number of words we have for that day) as the number of 'trials'. This ratio is a basic indicator of term coverage.

In order to conduct our analysis, we need to first calculate the number of terms we have for each day since the new year - you can think of this as the number of trials we run each day:

``` r
cum_time_counts <- archive_unnested %>%
  count(cum_time_elapsed, name = "trials")
```

Then, we need to prepare the number of successes by counting how many times each word appears in each day.

One important point to make here is that we don't want to include *all* words in our analysis - some words (such as 'Bolton' in regards to Trump's impeachment trial) appear a high number of times on one or two days then vanish thereafter. This type of term, from our point of view, is only going to create unnecessary noise in our algorithm fitting process so we want to exclude them. One way of doing this is to require that all words included in our analysis appear a certain number of times overall - I've gone with 50 as this seemed to provide the most fair results:

``` r
logit_model_inputs <- archive_unnested %>%
  count(article_term, cum_time_elapsed, name = "successes") %>%
  add_count(article_term, name = "term_count") %>%
  filter(term_count > 50) %>% # only include words which appear more than a certain number of times overall over the past 4 months - we want to identify the signal, not the noise
  arrange(cum_time_elapsed) %>%
  left_join(cum_time_counts, by = "cum_time_elapsed") %>%
  mutate(p_est = successes / trials,
         cum_time_elapsed = as.integer(cum_time_elapsed)) %>%
  select(article_term, cum_time_elapsed, successes, trials, p_est, term_count) # purely for presentational purposes

logit_model_inputs
```

    ## # A tibble: 33,481 x 6
    ##    article_term cum_time_elapsed successes trials   p_est term_count
    ##    <chr>                   <int>     <int>  <int>   <dbl>      <int>
    ##  1 accused                     0         1    960 0.00104         61
    ##  2 act                         0         1    960 0.00104         56
    ##  3 agency                      0         1    960 0.00104         54
    ##  4 ago                         0         1    960 0.00104         59
    ##  5 ahead                       0         1    960 0.00104         72
    ##  6 america                     0         3    960 0.00312         94
    ##  7 american                    0         1    960 0.00104        109
    ##  8 amid                        0         1    960 0.00104         87
    ##  9 angeles                     0         1    960 0.00104         61
    ## 10 anti                        0         2    960 0.00208         53
    ## # … with 33,471 more rows

Now we are ready to fit a logistic regression to each individual term. Let *U*<sub>*t*</sub><sup>(*w*)</sup> denote the 'relative usage' of word *w* at time *t* - by 'relative usage' we mean how frequently word *w* is used relative to all other words at the same time *t*. Then *U*<sub>*t*</sub><sup>(*w*)</sup> is connected to the linear combination −(*β*<sub>0</sub><sup>(*w*)</sup> + *β*<sub>1</sub><sup>(*w*)</sup>*t*) by way of a [logistic transformation](https://en.wikipedia.org/wiki/Logistic_regression#Logistic_model).

The coefficient *β*<sub>1</sub><sup>(*w*)</sup> can be interpreted as the *rate* of growth (or shrinkage, if negative) associated with word *w*. Indeed, this formula is on a per-word basis; we will fit a logistic growth model to each respective term in the input data via `stats::glm()`.

We also make use of the `purrr::map()` function in order to apply the `glm()` function to each respective word:

``` r
logit_model_results <- logit_model_inputs %>%
  group_by(article_term) %>%
  nest() %>%
  mutate(logit_model = map(data, function(xx) glm(cbind(successes, trials - successes) ~ cum_time_elapsed, family = "binomial", data = xx)),
         logit_model_tidy = map(logit_model, broom::tidy)) %>%
  select(article_term, logit_model_tidy) %>%
  unnest(cols = c(logit_model_tidy)) %>%
  ungroup()

trending_terms <- logit_model_results %>%
  filter(term == "cum_time_elapsed") %>%
  arrange(desc(estimate)) %>%
  filter(!str_detect(article_term, regex("(april|virus|primary)"))) %>% # see commentary below on this
  head(10) 

shrinking_terms <- logit_model_results %>%
  filter(term == "cum_time_elapsed") %>%
  arrange(estimate) %>%
  head(10) 
```

So now, if we inspect each respective set of terms we can identify which particular topics are growing exponentially in popularity and which topics have fallen off the radar. Note that I have filtered out some obvious trending terms (those which don't require such an analysis to know about): virus-related terms and the month of April (of course that will appear to be trending).

Let's start with trending terms. Note that 'estimate' in this context refers to the aforementioned *β*<sub>1</sub><sup>(*w*)</sup> that we discussed above. Thus, a higher estimate indicates a higher rate of growth (and vice versa):

``` r
growth_theme <- theme(plot.background = element_rect(fill = "grey35"),
                      text = element_text(colour = "grey95"),
                      line = element_line(colour = "grey30"),
                      axis.line = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text = element_text(colour = "grey95"),
                      axis.title = element_text(size = 10),
                      plot.title = element_text(face = "bold"),
                      plot.subtitle = element_text(size = 10),
                      panel.grid = element_blank(),
                      panel.grid.major.y = element_line(colour = "grey50"),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      strip.background = element_rect(fill = "black"),
                      plot.caption = element_text(colour = "gray70"))

logit_model_inputs %>%
  filter(article_term %in% trending_terms$article_term) %>%
  ggplot(mapping = aes(cum_time_elapsed, p_est, colour = article_term)) +
  geom_line(alpha = 0.80, show.legend = F) +
  facet_wrap(~article_term) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Time (days since new year)",
       y = "Relative frequency",
       colour = "Term",
       title = "Trending terms (excluding obvious terms e.g. 'coronavirus')",
       subtitle = "Healthcare, lockdown and kids are becoming emerging concerns",
       caption = "Source: New York Times API") +
  scale_colour_brewer(type = 'div', palette = 'Set3') +
  growth_theme
```

![](emerging-trends-analysis_files/figure-markdown_github/unnamed-chunk-10-1.png)

No surprises here, per se. We removed it but the word 'coronavirus' is indeed the highest growing term if it is accounted for. However, even in this set of 20 terms we start to notice other observable reporting trends: how the coronavirus may affect 'kids', the plight of 'health workers', the efficacy of 'masks'.

What is interesting though is to note *when* these terms start to gain traction. It happens at a different point in time to when the coronavirus was the most important topic because of 'competition' (though it is less so competition and moreso a shift in what is important) between topics for coverage:

``` r
focus_terms <- c("masks", "kids", "workers", "coronavirus")

term_coef <- logit_model_results %>%
  filter(article_term %in% focus_terms) %>%
  select(article_term, term, estimate) %>%
  spread(key = term, value = estimate) %>%
  rename(b_0 = `(Intercept)`, b_1 = cum_time_elapsed) %>%
  mutate(max_growth_point = abs(b_0 / b_1))

term_coef_error <- logit_model_results %>% 
  filter(article_term %in% focus_terms) %>% 
  select(article_term, term, std.error) %>% 
  spread(key = term, value = std.error) %>% 
  select(article_term, b_1_error = cum_time_elapsed)

term_coef_complete <- inner_join(term_coef, term_coef_error, by = "article_term") %>%
  transmute(article_term, 
            b_0, 
            b_1_lower = b_1 - b_1_error, 
            b_1, 
            b_1_upper = b_1 + b_1_error, 
            max_growth_point,
            max_growth_point_upper = abs(b_0 / b_1_lower),
            max_growth_point_lower = abs(b_0 / b_1_upper))

competing_theme <- theme(plot.title = element_text(face = "bold"),
                         axis.title = element_text(size = 10),
                         panel.grid.minor = element_blank(),
                         panel.grid.major.x = element_blank(),
                         panel.grid.major.y = element_line(colour = "grey50"),
                         axis.ticks = element_blank(),
                         plot.subtitle = element_text(size = 10),
                         plot.caption = element_text(colour = "grey70"),
                         plot.background = element_rect(fill = "grey30"),
                         panel.background = element_rect(fill = "grey30"),
                         panel.border = element_blank(),
                         text = element_text(colour = "grey90"),
                         axis.text = element_text(colour = "grey90"),
                         legend.background = element_rect(fill = "grey30"),
                         legend.key = element_rect(fill = "grey30"))

competing_logit_growth <- crossing(x = seq(0.01, 1000, 0.01), article_term = focus_terms) %>%
  left_join(term_coef_complete, by = "article_term") %>%
  mutate(logit_output_central = 1 / (1 + exp(-b_0 -b_1*x))) 

competing_arrow <- tibble(x_start = 850,
                          y_start = 0.40,
                          x_end = 700,
                          y_end = 0.50)

ggplot(data = competing_logit_growth, aes(x, logit_output_central, colour = article_term)) +
  geom_line(alpha = 0.70) +
  geom_point(data = term_coef, mapping = aes(x = max_growth_point, y = 0.50)) +
  geom_segment(data = term_coef, mapping = aes(x = max_growth_point, xend = max_growth_point,
                                               y = 0, yend = 0.50),
               lty = "dashed") +
  geom_hline(yintercept = 1, lty = 'dashed', colour = "grey90", alpha = 0.70) +
  labs(y = "Term usage",
       x = "Time",
       title = "Trajectory of viral growth split by topic",
       subtitle = "As the 'coronavirus' plateaus, other topics emerge (e.g. 'masks')",
       caption = "Source: New York Times API",
       colour = "Topic") +
  theme(axis.text.x = element_blank()) +
  scale_colour_manual(values = c("#00E6FF", "#8F9FFF", "#CF77F0", "#F546B4")) +
  scale_y_continuous(labels = percent_format()) +
  annotate("text", x = 750, y = 0.35, size = 2.8, colour = "grey90", label = "These dots reflect\nthe point of\nmaximum growth", hjust = 0, alpha = 0.90) +
  geom_curve(data = competing_arrow, mapping = aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
             arrow = arrow(length = unit(0.05, "inch")), size = 0.5,
             colour = "grey90", curvature = 0.4, alpha = 0.90) +
  competing_theme
```

![](emerging-trends-analysis_files/figure-markdown_github/unnamed-chunk-11-1.png)

We can observe this phenomena more precisely by looking at how much longer it takes other topics to emerge, relative to the 'coronavirus' (it takes 'masks' around two times as long):

``` r
coronavirus_growth_point <- term_coef_complete %>%
  filter(article_term == "coronavirus") %>%
  pull(max_growth_point)

term_coef_complete %>%
  transmute(article_term,
            central_rel = max_growth_point / coronavirus_growth_point,
            lower_rel = ifelse(article_term != "coronavirus", max_growth_point_lower / coronavirus_growth_point, NA),
            higher_rel = ifelse(article_term != "coronavirus", max_growth_point_upper / coronavirus_growth_point, NA)) %>%
  mutate(article_term = fct_reorder(article_term, 1 / central_rel)) %>%
  ggplot(aes(colour = article_term)) +
  geom_point(aes(x = central_rel, y = article_term), show.legend = F) +
  geom_errorbarh(aes(x = central_rel, xmin = lower_rel, xmax = higher_rel, y = article_term), height = 0.1, show.legend = F) +
  geom_vline(xintercept = 1, lty = 'dashed', colour = "grey90", alpha = 0.50) +
  scale_colour_manual(values = rev(c("#00E6FF", "#8F9FFF", "#CF77F0", "#F546B4"))) +
  labs(x = "Time to maximum growth (relative to 'coronavirus')",
       y = NULL,
       title = "Ratio of time to maximum growth relative to 'coronavirus'",
       subtitle = "For example, on average, 'masks' begin to gain maximum growth ~ 2 times after the 'coronavirus'") +
  competing_theme
```

    ## Warning: Ignoring unknown aesthetics: x

    ## Warning: Removed 1 rows containing missing values (geom_errorbarh).

![](emerging-trends-analysis_files/figure-markdown_github/unnamed-chunk-12-1.png)

As for shrinking terms, we note the following:

``` r
logit_model_inputs %>%
  filter(article_term %in% shrinking_terms$article_term) %>%
  ggplot(aes(cum_time_elapsed, p_est, colour = article_term)) +
  geom_line(alpha = 0.80, show.legend = F) +
  facet_wrap(~article_term) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Time (days since new year)",
     y = "Relative frequency",
     colour = "Term",
     title = "Shrinking terms",
     subtitle = "Iran (the 'killing' of Soleimani), defense and Trump's impeachment trial have all fallen off the radar",
     caption = "Source: New York Times API") +
  growth_theme +
  scale_colour_brewer(type = 'div', palette = 'Set3')
```

![](emerging-trends-analysis_files/figure-markdown_github/unnamed-chunk-13-1.png)

There's a story - in fact, a history even - borne out of these terms. Whatever happened to the situation in Iran? Trump's impeachment? Though it isn't unsurpirising, these topics have clearly taken a back seat to the coronavirus.

What is Latent Dirichlet Allocation (LDA)?
------------------------------------------

Analysing emerging trends in certain words forces us to contemplate an important question: which topics are emerging in the media as a result of the coronavirus?

The way in which NLP practitioners achieve this is via the use of a Bayesian technique known as Latent Dirichlet Allocation (also known as 'LDA'). The basic assumption behind this technique is that 'documents' (or, in our case, newspaper articles) are composed of different mixtures of topics. For example, you might read an article about how nutrition impacts the performance of professional footballers. Such an article is not *just* 'sports' but neither is it just 'science' - in fact, it's a potentially complex mixture of topics.

A further string to the bow of LDA is that each topic is assumed to consist of a given distribution of words. For example, if we take the topic 'sports' once again, one can imagine that this topic draws (i) very frequently from words such as 'football', 'touchdown' or 'goal'; (ii) moderately frequently from words such as 'brain' (in relation to injuries perhaps); and (iii) doesn't really touch words like 'sewing' or 'chicken' at all.

With these two assumptions in mind, LDA imagines that 'documents' (newspaper articles) are 'generated' (written) by first deciding on which specific topics a given document will be about. After this has been done, the second step is to randomly sample words from a random selection of these topics in order to build newspaper articles. Of course, writers don't follow these steps in practice but this process does arguably reflect a subconscious decision-making process occurring in the writer's head. If they are writing about the latest results on election polls, for example, then clearly there *is* a certain limitation on the words they choose to write down and their frequency (words like 'election' will come up more frequently).

Applying LDA
------------

Now, of course we aren't generating newspaper articles: we already have those. What we're actually trying to do is reverse-engineer the aforementioned process. Think about it: if we know which ('Dirichlet') parameters led to the creation of the articles that we are able to observe then we will be able to identify the approximate topic distribution within each article as well as the approximate distribution of words within each topic. Therefore, the goal of LDA is just that: to discover which specific parameters led to the realisation of the articles that we are able to observe. At the beginning of this process, these parameters are unknown and thus termed 'latent'.

To successfully apply LDA, we employ `topicmodels::LDA()`. First, we need to 'cast' our data into a document-term matrix format:

``` r
archive_dtm <- archive_unnested %>%
  count(article_id, article_term) %>%
  cast_dtm(article_id, article_term, n)

archive_dtm
```

    ## <<DocumentTermMatrix (documents: 15998, terms: 27682)>>
    ## Non-/sparse entries: 222941/442633695
    ## Sparsity           : 100%
    ## Maximal term length: 32
    ## Weighting          : term frequency (tf)

Now we can apply LDA. Let's make the hypothesis that there are, say, `k = 2` different topics:

``` r
archive_lda <- LDA(x = archive_dtm, k = 2, control = list(seed = 1234))

archive_lda
```

    ## A LDA_VEM topic model with 2 topics.

``` r
archive_topics <- tidy(archive_lda, matrix = "beta")

archive_topics_terms <- archive_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

archive_topics_terms %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F, aes(alpha = beta)) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("#00FFFF", "#AE6CD3")) +
  labs(x = NULL,
       y = "Beta",
       title = "Application of Latent Dirichlet Allocation (LDA) with two topics",
       subtitle = "Here's the latest news on: the coronavirus or Trump's election campaign") +
  growth_theme
```

![](emerging-trends-analysis_files/figure-markdown_github/unnamed-chunk-16-1.png)

Notice how the same words appear in each topic: this is an *advantage* of LDA over 'hard' clustering methods which naively separate words into different clusters. The algorithm recognises that words such as 'president' and 'coronavirus' can and do appear in the same articles despite being used in different contexts (e.g. suppose each article is actually about 'impeachment' and 'healthcare concerns', respectively).

Sentiment analysis
------------------

One question we might ask is whether the media (in this case the New York Times) has, knowlingly or not, caused a shift in sentiment over time.

We can have a stab at answering this by examining how the proportion of 'negative' words in each passing day has changed over time:

``` r
prop_neg <- archive_unnested %>%
  inner_join(get_sentiments("bing"), by = c("article_term" = "word")) %>%
  group_by(publication_date) %>%
  summarise(total_negative = sum(sentiment == "negative"),
            total = n(),
            prop_neg = total_negative / total) 

prop_neg %>%
  mutate(prop_neg_ma = rollmean(x = prop_neg, k = 10, fill = list(NA, NULL, NA))) %>%
  ggplot() +
  geom_point(aes(x = publication_date, y = prop_neg), alpha = 0.25, colour = "grey90") +
  geom_line(aes(x = publication_date, y = prop_neg_ma), colour = "#FCB448", alpha = 0.80) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = NULL,
       y = "Proportion of 'negative' terms relative to positive terms",
       title = "How negative sentiment is changing over time",
       subtitle = "Negative sentiment is fairly stationary however there is a peak during March to April",
       caption = "Source: New York Times API") +
  coronavirus_theme
```

![](emerging-trends-analysis_files/figure-markdown_github/unnamed-chunk-17-1.png)

It looks as if there is some kind of increasing trend in negativity from the period February to May but it's not clear whether the increase is significant or not. How can we make this more rigorous? Statisticians typically apply 'hypothesis testing' to this type of scenario. The hypothesis test can establish whether the increase in negative sentiment is beyond the realms of normality or not.

We can split the data at the midpoint (the start of March) and perform a hypothesis test that the average proportion of negative words following this midpoint is greater than the average proportion of negative words preceding it:

``` r
reference_date <- date("2020-03-01")

before_props <- prop_neg %>%
  mutate(test_boundary = ifelse(publication_date < reference_date, "Before", "After")) %>%
  filter(test_boundary == "Before") %>%
  pull(prop_neg)

after_props <- prop_neg %>% 
  mutate(test_boundary = ifelse(publication_date < reference_date, "Before", "After")) %>%
  filter(test_boundary == "After") %>%
  pull(prop_neg)

t_test_results <- t.test(after_props, before_props, alternative = "greater") %>%
  tidy()

prop_neg %>%
  mutate(prop_neg_ma = rollmean(x = prop_neg, k = 10, fill = list(NA, NULL, NA)),
         reference = ifelse(publication_date < reference_date, TRUE, FALSE)) %>%
  ggplot() +
  geom_point(aes(x = publication_date, y = prop_neg), alpha = 0.25, colour = "grey90") +
  geom_line(aes(x = publication_date, y = prop_neg_ma, colour = reference), alpha = 0.80, show.legend = F) +
  geom_vline(xintercept = reference_date, lty = "dotted", colour = "#D593E8", alpha = 0.50) +
  scale_y_continuous(labels = percent_format()) +
  scale_colour_manual(values = c("#D593E8", "#FCB448")) +
  labs(x = NULL,
       y = "Proportion of 'negative' terms relative to positive terms",
       title = "How negative sentiment is changing over time",
       subtitle = "Negative sentiment is fairly stationary however there is a peak during March to April",
       caption = "Source: New York Times API") +
  annotate("text", x = date("2020-03-03"), y = 0.45, size = 2.8, label = glue::glue("Statistically significant difference (mean = ", round(pull(t_test_results, estimate), 5), ", p < 0.05)\nin relative frequency of negative sentiment after\nmidpoint, 1 March 2020"), colour = "grey90", hjust = 0, alpha = 0.80, fontface = 1) +
  coronavirus_theme
```

![](emerging-trends-analysis_files/figure-markdown_github/unnamed-chunk-18-1.png)

So, whilst the difference itself is small (around 0.04), it is nonetheless statistically significant.

Conclusions
-----------

Our exploration of the New York Times metadata, though brief, has unlocked a few key insights into the impact of the coronavirus on reporting. Namely,

-   Words really are 'viral'. They grow exponentially and then proceed to tail off once the topic has become exhausted and subtopics take its place
-   Words can also be 'retroviral'. Certain topics fall off the radar as a result of dominant topics like the coronavirus. The question of Iran is one such topic
-   The vast majority of articles written can be grouped into a 'political' and 'health-related' category - the two are *not* mutually exclusive either as demonstrated by the outcome of the LDA algorithm
-   Negative sentiment, though fairly stationary, has incurred somewhat of a hit during the coronavirus as one would have expected. This hit *is* statistically significant
