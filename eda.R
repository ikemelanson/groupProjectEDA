library(tidyverse)
df <- read_csv("https://www.dropbox.com/scl/fi/2bbujng7y0dxpj8blb4ej/19_train.csv?rlkey=k8pxvbva2jwr0qp7oj1cc9ul5&dl=1")

# 3. (12 points) Work as a group to perform an exploratory data analysis (EDA), in which you 
  # generate a general understanding of the columns in the data. This can be in whatever format 
  # you find useful, but you should integrate narrative text combined with applicable visualizations 
  # and/or tables. Your narrative text should summarize what analyses you are performing, and note anything 
  # interesting or concerning. This analysis writeup should include the following sections:
# -------------------------------------------------------------------------------------------------------


# A. (3 points) Examine the characteristics of the dependent variable that you’ll be training a
  # model to predict. Look at the distribution and anything else that seems relevant, focusing 
  # especially on anything that you’ll likely need to account for as you build your model(s). 
  # Include one or more visualizations to illustrate the most important takeaway(s).




# -------------------------------------------------------------------------------------------------------
# B. (4 points) Summarize the characteristics of the remaining variables in the dataset at a high level. 
  # This should include any relevant groupings of like variables, a description of the number of 
  # categorical vs. continuous variables, any relevant patterns of missingness or odd distributions, etc.
  # This should not be a long, comprehensive section of your report. What we’re looking for here is a high-level 
  # sketch of the dataset so that, in a page or two, the reader can get an idea of the data that you’ll be using
  # and anything in particular that you’ll need to watch out for as you progress to building your model. (Note that,
  # even though I’m asking for just a page or two of column summary, your team will likely need to look fairly carefully
  # at each of the columns in order to identify any lurking issues and know which columns to highlight, etc.)







# -------------------------------------------------------------------------------------------------------
# C. (5 points) Perform an initial analysis in which you systematically examine each independent 
  # variable as it relates to your dependent variable. This will provide your group with an initial
  # idea of which (if any) variable jump out as especially promising features.


  # - For numeric (continuous) variables, this will likely involve looking at correlations or something
  # similar in order to identify variables (if any) that are likely related to your dependent variable.
  # Any particularly interesting continuous variables may deserve a scatterplot or some other visual 
  # demonstration of the relationship.

numeric_cols <- c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms')
dependent_variable <- 'track_popularity'

# lets start by taking a look at the mean, median, min, max, and std for each of our numeric columns
df %>% 
  select(all_of(numeric_cols)) %>%
  summary()
# Wow, looks like instrumentalness is potentially extremly skewed, we'll take a deeper look at that later.
# It also looks like we are dealing with different scales for a lot of these variables.
# A lot of them are 0-1 but loudness, tempo, and duration appear to be on a different scale.
# we will potentially need to scale those varialbes to use the same scale as the other variables.

# lets take a look at some density plots for our numeric features. This will give us a better idea
# as to which are skewed, and other potential problems

df %>% 
  select(all_of(c(numeric_cols))) %>%
  pivot_longer(
    cols = everything()
  ) %>% 
  ggplot(aes(x=value, fill=name)) +
  geom_density() +
  facet_wrap(~name, nrow = 6, scales = 'free') +
  theme_bw()

# just like we thought, instrumentalness is so extremely skewed, that its hard to tell what is even happening.
# we can take a deeper look at that in a bit. duration, valence, and temp (kind of) have nice and normal distributions.
# looks like acousticness, liveness and speechiness are positivly skewed, and dancebility, energy, and loudness 
# are negativly skewed. We can look more into some potential transformations on these columns to make them more normal.

# lets look at a correlation matrix for numeric columns with our dependent variable
df %>% select(all_of(c(dependent_variable, numeric_cols))) %>% cor()
# looks like our most correlated features are 'instrumentalness' -0.147904235, 'duration_ms' -0.143246007, and 'energy' -0.106449655
# neither of these have crazy strong correlations, but definitely still worth taking a look at.
# it is also interesting that these three variables are negatively correlated.
# that means that popular songs have a lot of vocals (low instrumentalness score means that there is a lot of singing),
  # have a short duration, and are lower energy ("energetic tracks feel fast, loud, and noisy. For example, death metal has high energy").
# to me, this pretty much describes a lot of pop music today. Shorter songs, with less guitar and drum solos, that are more "chill".

# another thing to take note of is that 'loudness' and 'energy' scored a 0.67774883, which is pretty high.
# we might need to drop one of these columns because they are so strongly correlated. we can look into that later tho
# thinking about 'loudness' and 'energy', it kinda makes sense that they would be so strongly correlated tho

# lets take a deeper dive for our most important variables
important_variables <- c('instrumentalness', 'duration_ms', 'energy')

df %>% 
  select(all_of(c(dependent_variable, important_variables))) %>% 
  pivot_longer(important_variables) %>% 
  ggplot(aes(x = track_popularity, y = value, color = name)) +
  geom_point(alpha=.1) +
  facet_wrap(~name, nrow = 3, scales = 'free_y') +
  theme_bw()
  
# Obviosly the correlation scores were not very high, but these plots show just how low the correlation is.
# just as we expected, the duration_ms is the neatest looking, followed by energy, and instrumentalness
# looks like garbage

# lets take a look at the very highly correlated 'loudness' and 'energy' columns just for fun
df %>% 
  select(loudness, energy) %>% 
  ggplot(aes(x=energy, y=loudness)) +
  geom_point(color='blue', alpha=.1)
# very nicely correlated indeed, later in the model building process, we can take a look at potentially
# removing one of these columns. This shows that in general, high energy songs are also loud, which makes
# a lot of sense!

# now lets go back a little bit and just investigate our instrumentalness column a little bit.
df %>% 
  select(instrumentalness) %>%
  ggplot(aes(x = instrumentalness))+
  geom_boxplot()
# useless

# lets try a bar chart
df %>% 
  select(instrumentalness) %>% 
  ggplot(aes(x=instrumentalness)) +
  geom_histogram(bins= 50)


  # - For categorical variables, this will involve looking at the pattern of the dependent
  # variable at various levels of the category. This will look different depending on your dependent 
  # variable, but what you’re looking to do is highlight any categorical variables where the dependent 
  # variable appears “different” (in terms of rate, frequency, average, etc.) at different levels of that 
  # category. Summarize anything you find particularly interesting or relevant using an appropriate visualization.

# playlist_genre, playlist_subgenre, key, mode, 

categoricals <- c('playlist_genre', 'playlist_subgenre', 'key', 'mode')

# select(all_of(c(dependent_variable, important_variables))) %>% 
#   pivot_longer(important_variables) %>% 

df %>%
  select(all_of(c(dependent_variable, categoricals))) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(categoricals) %>% 
  group_by(name) %>% 
  ggplot(aes(x=name)) +
  geom_bar() +
  facet_wrap(~name, nrow = 3, scales = 'free_y')

# average track popularity by subgenre
df %>% 
  select(playlist_subgenre, track_popularity) %>% 
  group_by(playlist_subgenre) %>% 
  summarise(avg_popularity = mean(track_popularity)) %>% 
  ggplot(aes(x = playlist_subgenre, y=avg_popularity)) +
  geom_col() +
  theme_bw()

df %>% 
  select(playlist_genre, track_popularity) %>% 
  group_by(playlist_genre) %>% 
  summarise(avg_popularity = mean(track_popularity)) %>% 
  ggplot(aes(x = playlist_genre, y=avg_popularity)) +
  geom_col() +
  theme_bw()

df %>% 
  select(key, track_popularity) %>% 
  group_by(key) %>% 
  summarise(avg_popularity = mean(track_popularity)) %>% 
  ggplot(aes(x = key, y=avg_popularity)) +
  geom_col() +
  theme_bw()
  
df %>% 
  select(mode, track_popularity) %>% 
  group_by(mode) %>% 
  summarise(avg_popularity = mean(track_popularity)) %>% 
  ggplot(aes(x = mode, y=avg_popularity)) +
  geom_col() +
  theme_bw()
  


  




