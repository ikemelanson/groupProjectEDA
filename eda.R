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

# Grab summary stats
stats <- summary(spotify$track_popularity)

# Grab standard deviation
standard_deviation <- spotify %>% 
  summarize(
    standard_deviation = sd(track_popularity)
  )

# Create histogram to show distribution
histogram <- spotify %>%
  ggplot(aes(x = track_popularity)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Track Popularity",
       x = "Track Popularity",
       y = "Frequency")

# Create density to show distribution
density <- spotify %>%
  ggplot(aes(x = track_popularity)) +
  geom_density(fill = "orange", color = "black") +
  labs(title = "Density Plot of Track Popularity",
       x = "Track Popularity",
       y = "Density")

# Grab any duplicate track ids to try and explain the spike near 0-10 on track popularity
duplicates <- spotify %>% 
  group_by(track_id) %>% 
  summarize(count = n()) %>% 
  filter(count > 1) %>% 
  arrange(desc(count))

# To check for the track popularity of the duplicates, which was not overwhelmingly values of 0-10, so this does not explain the peculiar spike
joined <- duplicates %>% left_join(spotify, by = 'track_id') %>% select(track_id, count, track_popularity)

# -------------------------------------------------------------------------------------------------------
# B. (4 points) Summarize the characteristics of the remaining variables in the dataset at a high level. 
  # This should include any relevant groupings of like variables, a description of the number of 
  # categorical vs. continuous variables, any relevant patterns of missingness or odd distributions, etc.
  # This should not be a long, comprehensive section of your report. What we’re looking for here is a high-level 
  # sketch of the dataset so that, in a page or two, the reader can get an idea of the data that you’ll be using
  # and anything in particular that you’ll need to watch out for as you progress to building your model. (Note that,
  # even though I’m asking for just a page or two of column summary, your team will likely need to look fairly carefully
  # at each of the columns in order to identify any lurking issues and know which columns to highlight, etc.)

# get count of numeric variables
num_numeric <- print(sapply(df, is.numeric))


# determine the missingness of the dataset
colSums(is.na(df))

df %>% 
  filter(is.na(track_name))


# plot the distributions for each continuous variable
danceability_plot <- df %>%
  ggplot(aes(x = danceability)) +
  geom_histogram(fill = "skyblue", color = "black")

energy_plot <- df %>%
  ggplot(aes(x = energy)) +
  geom_histogram(fill = "skyblue", color = "black")

key_plot <- df %>%
  ggplot(aes(x = key)) +
  geom_histogram(fill = "skyblue", color = "black")

loudness_plot <- df %>%
  ggplot(aes(x = loudness)) +
  geom_histogram(fill = "skyblue", color = "black")

speechiness_plot <- df %>%
  ggplot(aes(x = speechiness)) +
  geom_histogram(fill = "skyblue", color = "black")

acousticness_plot <- df %>%
  ggplot(aes(x = acousticness)) +
  geom_histogram(fill = "skyblue", color = "black")

instrumentalness_plot <- df %>%
  ggplot(aes(x = instrumentalness)) +
  geom_histogram(fill = "skyblue", color = "black")

liveness_plot <- df %>%
  ggplot(aes(x = liveness)) +
  geom_histogram(fill = "skyblue", color = "black")

valence_plot <- df %>%
  ggplot(aes(x = valence)) +
  geom_histogram(fill = "skyblue", color = "black")

tempo_plot <- df %>%
  ggplot(aes(x = tempo)) +
  geom_histogram(fill = "skyblue", color = "black")

duration_plot <- df %>%
  ggplot(aes(x = duration_ms)) +
  geom_histogram(fill = "skyblue", color = "black")


# find the number of unique playlist_genres, subgenres, names, and artists
df %>% 
  group_by(playlist_genre) %>% 
  count(playlist_genre)

df %>% 
  group_by(playlist_subgenre) %>% 
  count(playlist_subgenre)

df %>% 
  group_by(playlist_name) %>% 
  count(playlist_name)

df %>% 
  group_by(track_artist) %>% 
  count(track_artist)




# -------------------------------------------------------------------------------------------------------
# C. (5 points) Perform an initial analysis in which you systematically examine each independent 
  # variable as it relates to your dependent variable. This will provide your group with an initial
  # idea of which (if any) variable jump out as especially promising features.


  # - For numeric (continuous) variables, this will likely involve looking at correlations or something
  # similar in order to identify variables (if any) that are likely related to your dependent variable.
  # Any particularly interesting continuous variables may deserve a scatterplot or some other visual 
  # demonstration of the relationship.



  # - For categorical variables, this will involve looking at the pattern of the dependent 
  # variable at various levels of the category. This will look different depending on your dependent 
  # variable, but what you’re looking to do is highlight any categorical variables where the dependent 
  # variable appears “different” (in terms of rate, frequency, average, etc.) at different levels of that 
  # category. Summarize anything you find particularly interesting or relevant using an appropriate visualization.






