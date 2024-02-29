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
  ggtitle("Track Popularity vs Duration, Energy, and Instrumentalness") +
  theme_bw()
  
# Obviosly the correlation scores were not very high, but these plots show just how low the correlation is.
# just as we expected, the duration_ms is the neatest looking, followed by energy, and instrumentalness
# looks like garbage

# lets take a look at the very highly correlated 'loudness' and 'energy' columns just for fun
df %>% 
  select(loudness, energy) %>% 
  ggplot(aes(x=energy, y=loudness)) +
  geom_point(color='blue', alpha=.1) +
  ggtitle("Energy and Loudness Correlation") +
  theme_bw()
# very nicely correlated indeed, later in the model building process, we can take a look at potentially
# removing one of these columns. This shows that in general, high energy songs are also loud, which makes
# a lot of sense!


  # - For categorical variables, this will involve looking at the pattern of the dependent
  # variable at various levels of the category. This will look different depending on your dependent 
  # variable, but what you’re looking to do is highlight any categorical variables where the dependent 
  # variable appears “different” (in terms of rate, frequency, average, etc.) at different levels of that 
  # category. Summarize anything you find particularly interesting or relevant using an appropriate visualization.


categoricals <- c('playlist_genre', 'playlist_subgenre', 'key', 'mode')

# Let's take a look at our categorical variables and how they relate to track_popularity. We will do this by
# comparing the average track_popularity for each category.

df %>% 
  select(playlist_genre, track_popularity) %>% 
  group_by(playlist_genre) %>% 
  summarise(avg_popularity = mean(track_popularity)) %>% arrange(desc(avg_popularity))
  ggplot(aes(x = playlist_genre, y=avg_popularity)) +
  geom_col(fill='skyblue', color='black') +
  ggtitle("Average Popularity Score for each Playlist Genre") +
  theme_bw()
  
  # Looks like there's not really a strong correlation here. The Pop category scored the highest, which is no
  # surprise. The latin was right behind, which was a little bit surprising. These seem to be very broad categories
  # however, so they each represent a lot of different subgenres. Let's dig into the possible subgenres.
  
df %>% 
  select(playlist_subgenre, track_popularity) %>% 
  group_by(playlist_subgenre) %>% 
  summarise(avg_popularity = mean(track_popularity)) %>% arrange(desc(avg_popularity))
  ggplot(aes(x = playlist_subgenre, y=avg_popularity)) +
  geom_col(fill='skyblue', color='black') +
  ggtitle("Average Popularity Score for each Playlist Subgenre") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

  # Looks like post-teen pop took the cake on this one, followed by permanent wave (whatever that is) and hip hop. 
  # Post-teen pop and hip hop make a lot of sense, those are very popular genres. Permanent wave includes artists 
  # like Post Malone, Coldplay, and The Red Hot Chili Peppers, all of whom make very popular music.
  
  # We might need to simplify down some of these subgenres. Maybe rate them on popularity and have a 
  # subgere_popularity column? This many categories in a column can be problematic when we try to turn them into dummy variables. 
  
  # Let's see if the music key of the song makes a difference. 

df %>% 
  select(key, track_popularity) %>% 
  group_by(key) %>% 
  summarise(avg_popularity = mean(track_popularity)) %>% 
  ggplot(aes(x = key, y=avg_popularity)) +
  geom_col(fill='skyblue', color='black') +
  ggtitle("Average Popularity Score for each Key") +
  theme_bw()
  
# Looks like 8 (key of G#/A♭) is the winner, but not in any significant way.
# Depending on the model we use, we can certainly drop this column
              
# Let's look at the Mode (a major key is 1, minor key is 0).

df %>% 
  select(mode, track_popularity) %>% 
  group_by(mode) %>% 
  summarise(avg_popularity = mean(track_popularity)) %>% 
  ggplot(aes(x = mode, y=avg_popularity)) +
  geom_col(fill='skyblue', color='black') +
  ggtitle("Average Popularity Score by Mode") +
  theme_bw()

# Pretty much no difference, songs in a major key are the tiniest bit more popular. Probably drop this column in a lot of our models. 


