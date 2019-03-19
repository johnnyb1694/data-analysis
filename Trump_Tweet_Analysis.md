Analysis of Trump Tweets
================
Johnny Breen
05/03/2019

### Introduction

The data that I have chosen to analyse in this document is a repository of Trump tweets which span roughly two years of data all the way up until Trump's election as US president in November 2016. The original CSV file I have used in the following analysis can be found [here](https://www.kaggle.com/kingburrito666/better-donald-trump-tweets) on Kaggle.

### Credits

My analysis of Trump's tweets draws a substantial amount of insight from David Robinson's recent screencast where he analysed a repository of medium articles - that specific screencast can be found [here](https://www.youtube.com/watch?v=C69QyycHsgE) and I highly encourage anyone interested in text mining to watch it at their next available moment.

### Plan

I have outlined below the main tasks that need to be performed in order to create a simple text mining model which can be fit to the data:

-   Read in the data
-   Preprocess the data
-   Explore the data (attempt to perform sentiment & word correlation analyses)
-   Apply a machine learning model to predict the number of retweets, based on the contents of a given tweet

We would like to create a model that can take a given input tweet and assign to that tweet the predicted number of retweets based on its contents. This type of task may (with large caveats of course!) be useful for a business looking to improve their social media presence.

Preliminaries
-------------

The first step is to load any necessary packages and read in the data:

``` r
load_req_packages <- function (req_libraries = c("tidyverse", "magrittr", "broom", "widyr", "ggraph", "igraph", "tidytext")) {
  for (req_lib in req_libraries) {
    if (!(req_lib %in% installed.packages())) {
      install.packages(req_lib)
    }
  }
  sapply(req_libraries, require, character.only = TRUE)
}

load_req_packages() # runs the function defined above
```

    ## tidyverse  magrittr     broom     widyr    ggraph    igraph  tidytext 
    ##      TRUE      TRUE      TRUE      TRUE      TRUE      TRUE      TRUE

``` r
theme_set(theme_light()) # initialise the theme for ggplot2
```

To read in CSV data, I will make use of the `readr::read_csv()` function:

``` r
proj_root <- "~/Desktop/Data Science/R/Project Trump/" 
trump_tweets_raw <- read_csv(file = paste0(proj_root, "Data/Trump_Tweets.csv"))
```

Pre-processing
--------------

Before we pre-process the data, we can inspect the first 10 rows of the data using the `head()` function:

``` r
 trump_tweets_raw %>%
    head()
```

    ## # A tibble: 6 x 12
    ##   Date  Time  Tweet_Text Type  Media_Type Hashtags Tweet_Id Tweet_Url
    ##   <chr> <tim> <chr>      <chr> <chr>      <chr>       <dbl> <chr>    
    ## 1 16-1… 15:26 Today we … text  photo      ThankAV…  7.97e17 https://…
    ## 2 16-1… 13:33 Busy day … text  <NA>       <NA>      7.97e17 https://…
    ## 3 16-1… 11:14 Love the … text  <NA>       <NA>      7.97e17 https://…
    ## 4 16-1… 02:19 Just had … text  <NA>       <NA>      7.97e17 https://…
    ## 5 16-1… 02:10 A fantast… text  <NA>       <NA>      7.97e17 https://…
    ## 6 16-1… 19:31 Happy 241… text  photo      <NA>      7.97e17 https://…
    ## # ... with 4 more variables:
    ## #   twt_favourites_IS_THIS_LIKE_QUESTION_MARK <int>, Retweets <int>,
    ## #   X11 <int>, X12 <chr>

Questions to Ponder
===================

Straight off the bat we have a few preliminary data-based questions:

1.  Are the missing values in the `Media_Type` and `Hashtags` MCAR ("Missing Completely At Random") or MNAR ("Missing Not At Random")? In other words, do they actually
2.  Is the `Tweet_Id` useful at all? Why does it appear to be populated with a static value of `7.97e17`?
3.  What are `X11` and `X12`? Are they completely missing? They seem to stem from a parsing error when using `read_csv()` but we should verify this by glancing at the documentation for the source data

We can solve question 3 by checking the documentation - this reveals that `X11` and `X12` are likely to be due to some parsing error. We can therefore remove them. Question 2 is not as clear-cut but it does not appear to be informative so we will drop it as well.

As for question 1, I will make the assumption that `NA` indicates that either no photo was attached (in the case of `Media_Type`) or that no hashtag exists (in the case of `Hashtags`).

Pre-processing Tasks
====================

We now have a series of clear pre-processing tasks to follow:

1.  Remove redundant columns
2.  Rename columns
3.  Impute missing values on 'Media\_Type' and 'Hashtags' variables
4.  Convert the relevant variables to factors
5.  Separate dates and times into different columns
6.  Remove link quotations in the 'Tweet\_Text' field

``` r
# define which columns we would like to remove, rename and convert into factors
cols_to_remove <- c("X11", "X12", "Tweet_Id", "Tweet_Url")
cols_to_rename <- c(Tweet = "Tweet_Text", Likes = "twt_favourites_IS_THIS_LIKE_QUESTION_MARK")
cols_to_fct <- c("Type", "Media_Type", "Hashtags")

# define a simple function which can take, as input, the columns to turn into factors and output the corrected dataframe
to_factor <- function (df, cols_to_fct) {
  df[cols_to_fct] <- map_dfr(df[cols_to_fct], factor) %>%
    as_tibble()
  return(df)
}

trump_tweets_clean <- trump_tweets_raw %>%
  select(-cols_to_remove) %>% 
  rename(!!cols_to_rename) %>% # since dplyr quotes its inputs, by default, we need to communicate that we have already done this (i.e. we don't want 'rename' to take 'cols_to_rename' too literally...) by using '!!'
  replace_na(list(Media_Type = "(None)", Hashtags = "(None)")) %>%
  to_factor(cols_to_fct) %>% 
  separate(col = Date, into = c("Year", "Month", "Day"), sep = "-", convert = TRUE) %>% 
  separate(col = Time, into = c("Hour", "Minute", "Second"), sep = ":", convert = TRUE) %>% 
  mutate(Hour_Min = Hour + Minute / 60) %>% 
  select(-Minute, -Second) %>% 
  mutate(Tweet = str_replace_all(Tweet, "(www|http:|https:)+[^\\s]+", "")) %>% # remove links
  mutate(Tweet = str_replace_all(Tweet, "\\d+\\:\\d+", "")) 

trump_tweets_clean %>%
    head()
```

    ## # A tibble: 6 x 11
    ##    Year Month   Day  Hour Tweet Type  Media_Type Hashtags  Likes Retweets
    ##   <int> <int> <int> <int> <chr> <fct> <fct>      <fct>     <int>    <int>
    ## 1    16    11    11    15 "Tod… text  photo      ThankAV… 127213    41112
    ## 2    16    11    11    13 Busy… text  (None)     (None)   141527    28654
    ## 3    16    11    11    11 Love… text  (None)     (None)   183729    50039
    ## 4    16    11    11     2 Just… text  (None)     (None)   214001    67010
    ## 5    16    11    11     2 A fa… text  (None)     (None)   178499    36688
    ## 6    16    11    10    19 "Hap… text  photo      (None)   159176    44655
    ## # ... with 1 more variable: Hour_Min <dbl>

Data Exploration
----------------

We can see from the data the period of time over which this data spans:

``` r
trump_tweets_clean %>%
    select(Year, Month) %>%
    distinct()
```

    ## # A tibble: 17 x 2
    ##     Year Month
    ##    <int> <int>
    ##  1    16    11
    ##  2    16    10
    ##  3    16     9
    ##  4    16     8
    ##  5    16     7
    ##  6    16     6
    ##  7    16     5
    ##  8    16     4
    ##  9    16     3
    ## 10    16     2
    ## 11    16     1
    ## 12    15    12
    ## 13    15    11
    ## 14    15    10
    ## 15    15     9
    ## 16    15     8
    ## 17    15     7

Donald Trump was officially elected in November 2016 so it looks as though these tweets capture most of his electoral journey. It would be interesting to observe his daily activity on Twitter split by year and month:

``` r
trump_tweets_clean %>%
  ggplot(aes(Hour, fill = as.factor(Month))) +
  geom_density(show.legend = FALSE) +
  facet_grid(Year~Month) +
  theme(axis.text.x = element_text(size = 7.5)) +
  labs(y = "Density", 
       title = "Trump's Twitter Activity", 
       subtitle = "Split by year and month - Trump generally becomes active during the evenings")
```

![](Trump_Tweet_Analysis_files/figure-markdown_github/unnamed-chunk-6-1.png)

It seems there isn't much variation in his tweeting patterns over the course of the electoral journey.

Let's turn our attention to text mining by looking at the most frequently occurring words in this dataset. To do this, we leverage the tidytext function 'unnest\_tokens' which splits each tweet into a one word per row format. To maintain an understanding of which word belongs to which tweet, we mutate the initial dataframe - before unnesting - so that each tweet has a pre-defined 'ID'. We also remove any stop-words (e.g. 'a', 'of', 'the'), digits and words like 'Trump / trump':

``` r
trump_tweets_unnested <- trump_tweets_clean %>%
  mutate(ID = row_number()) %>% 
  unnest_tokens(output = "Word", input = "Tweet") %>% # split each tweet into a series of one-word rows
  anti_join(stop_words, by = c("Word" = "word")) %>% # remove stop-words like 'a', 'the' etc.
  filter(!(Word %in% c("trump", "Trump")), str_detect(Word, "[a-z]")) # filter out any numbers

trump_tweets_unnested # check on representation of data
```

    ## # A tibble: 57,401 x 12
    ##     Year Month   Day  Hour Type  Media_Type Hashtags  Likes Retweets
    ##    <int> <int> <int> <int> <fct> <fct>      <fct>     <int>    <int>
    ##  1    16    11    11    15 text  photo      ThankAV… 127213    41112
    ##  2    16    11    11    15 text  photo      ThankAV… 127213    41112
    ##  3    16    11    11    15 text  photo      ThankAV… 127213    41112
    ##  4    16    11    11    15 text  photo      ThankAV… 127213    41112
    ##  5    16    11    11    15 text  photo      ThankAV… 127213    41112
    ##  6    16    11    11    15 text  photo      ThankAV… 127213    41112
    ##  7    16    11    11    15 text  photo      ThankAV… 127213    41112
    ##  8    16    11    11    13 text  (None)     (None)   141527    28654
    ##  9    16    11    11    13 text  (None)     (None)   141527    28654
    ## 10    16    11    11    13 text  (None)     (None)   141527    28654
    ## # ... with 57,391 more rows, and 3 more variables: Hour_Min <dbl>,
    ## #   ID <int>, Word <chr>

Now that we have split the data into a 'tidy' format, we can investigate the top 20 (say) most common used words:

``` r
trump_tweets_unnested %>%
  count(Word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(Word = reorder(Word, n)) %>%
  ggplot(aes(Word, n, fill = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradient(low = "blue", high = "red") +  
  theme_classic() +
  labs(x = "Term",
       y = "Frequency",
       title = "Top 20 Most Frequently Used Words",
       subtitle = "An insight into Trump's most prevalent tweet contents")
```

![](Trump_Tweet_Analysis_files/figure-markdown_github/unnamed-chunk-8-1.png)
