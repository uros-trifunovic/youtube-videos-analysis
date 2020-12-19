# Load required libraries
library(dplyr); library(tidyverse); library(ggplot2); library(tm);
library(rjson); library(lubridate); library(gridExtra); library(modelr);
library(randomForest); library(kableExtra); library(wordcloud);

# Import data
yt_vids_us <- read_csv('PATH-TO-THE-FILE/USvideos.csv')
yt_vids_us_json <- fromJSON(file = 'PATH-TO-THE-FILE/US_category_id.json')
json_data_frame <- as.data.frame(yt_vids_us_json)

# Map categories' names from the JSON file to the categories' IDs 
yt_vids_categories <- data.frame()
for (item in 1:length(yt_vids_us_json[['items']])){
  id <- print(yt_vids_us_json[['items']][[item]]['id'])
  category <- print(yt_vids_us_json[['items']][[item]]['snippet'][[1]]['title'])
  
  yt_vids_categories <- yt_vids_categories %>% rbind(c(id, category))
}
colnames(yt_vids_categories) <- c('category_id', 'category_title')
yt_vids_categories$category_id <- as.double(yt_vids_categories$category_id)

# Explore the dataset
dim(yt_vids_us)
str(yt_vids_us)
colSums(is.na(yt_vids_us))

# Reformat the trending date clumn
yt_vids_us$trending_date <- format(
  as.Date(yt_vids_us$trending_date, "%y.%d.%m"), "%m/%d/%y"
)

# Add the categories' names to the dataset
yt_vids_us <- yt_vids_us %>% left_join(yt_vids_categories) 
yt_vids_us %>% select(category_id, category_title)

# Rename the "Howto & Style" column
yt_vids_us$category_title[yt_vids_us$category_title == 'Howto & Style'] <- 'How To & Style'

# Remove unnecessary columns
yt_vids_us <- yt_vids_us %>% select(-c('category_id', 'thumbnail_link', 'description'))
colSums(is.na(yt_vids_us))

# Explore the formatted dataset
yt_vids_us %>% head() 
yt_vids_us %>% names()
summary(yt_vids_us$views)

# Look at data distribution
Views <- quantile(yt_vids_us$views, c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
Dislikes <- quantile(yt_vids_us$dislikes, c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
Likes <- quantile(yt_vids_us$likes, c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
`Comment Count` <- quantile(yt_vids_us$comment_count, c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
quantiles_df <- as.data.frame(rbind(Views, Dislikes, Likes, `Comment Count`))
quantiles_df

# Plot the distributions
views_dislikes_freq <- yt_vids_us %>% 
  ggplot(aes(dislikes/1000)) +
  geom_histogram(aes(fill = ..count..), bins = 40, show.legend = F) +
  scale_x_continuous(
    breaks = seq(0, 10, 2),
    limits=c(0, 10)
  ) +
  scale_fill_gradient(high =  "#E31A1C", low = "#FB9A99") +
  labs(x = 'Dislikes (in thousands)') +
  labs(y = 'Frequency') +
  labs(title = 'Dislikes Frequency') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

views_likes_freq <- yt_vids_us %>% 
  ggplot(aes(likes/1000)) +
  geom_histogram(aes(fill = ..count..), bins = 40, show.legend = F) +
  scale_x_continuous(
    breaks = seq(0, 500, 100),
    limits=c(0, 500)
  ) +
  scale_fill_gradient(high =  "#33A02C", low = "#B2DF8A") +
  labs(x = 'Likes (in thousands)') +
  labs(y = 'Frequency') +
  labs(title = 'Likes Frequency') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

views_comments_freq <- yt_vids_us %>% 
  filter(views <= 30000000 & comment_count <= 101000) %>%
  ggplot(aes(comment_count/1000)) +
  geom_histogram(aes(fill = ..count..), bins = 40, show.legend = F) +
  scale_x_continuous(
    breaks = seq(0, 50, 10),
    limits=c(0, 50)
  ) +
  scale_fill_gradient(high =  "#1F78B4", low = "#A6CEE3") +
  labs(x = 'Comments (in thousands)') +
  labs(y = 'Frequency') +
  labs(title = 'Comments Frequency') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

grid.arrange(
  views_dislikes_freq, views_likes_freq, views_comments_freq, 
  nrow = 1, ncol = 3
)

# Plot views vs vars
views_dislikes <- yt_vids_us %>% 
  ggplot(aes(dislikes/1000, views/1000000)) +
  geom_hex(bins = 25, color = "white", show.legend = F)+
  scale_fill_gradient(high = "#E31A1C", low = "#FB9A99" ) +
  geom_smooth(color = "#E31A1C", se = F, show.legend = F) +
  labs(x = 'Dislikes (in thousands)') +
  labs(y = 'Views (in millions)') +
  labs(title = 'Views vs. Dislikes') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

views_likes <- yt_vids_us %>% 
  ggplot(aes(likes/1000, views/1000000)) +
  geom_hex(bins = 25, color = "white", show.legend = F)+
  scale_fill_gradient(high = "#33A02C", low = "#B2DF8A") +
  geom_smooth(color = "#33A02C", se = F, show.legend = F) +
  labs(x = 'Likes (in thousands)') +
  labs(y = 'Views (in millions)') +
  labs(title = 'Views vs. Likes') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

views_comments <- yt_vids_us %>% 
  ggplot(aes(comment_count/1000, views/1000000)) +
  geom_hex(bins = 25, color = "white", show.legend = F)+
  scale_fill_gradient(high = "#1F78B4", low = "#A6CEE3") +
  geom_smooth(color = "#1F78B4", se = F, show.legend = F) +
  labs(x = 'Commments (in thousands)') +
  labs(y = 'Views (in millions)') +
  labs(title = 'Views vs. Comments') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

grid.arrange(
  views_dislikes, views_likes, views_comments, 
  nrow = 1, ncol = 3
)

# Split the data
yt_videos_train_indices <- sample(1:nrow(yt_vids_us), 0.8 * nrow(yt_vids_us))
yt_videos_train <- yt_vids_us %>% slice(yt_videos_train_indices)
yt_videos_test <- yt_vids_us %>% slice(-yt_videos_train_indices)

# Fit the model
yt_vids_lm <- lm(views ~ likes + dislikes + comment_count, data = yt_videos_train)

# Model output
r_sq <- summary(yt_vids_lm)$r.squared
adj_r_sq <- summary(yt_vids_lm)$adj.r.squared
coef_dislikes <- coef(summary(yt_vids_lm))["dislikes","Estimate"]
coef_likes <- coef(summary(yt_vids_lm))["likes","Estimate"]
coef_comments <- coef(summary(yt_vids_lm))["comment_count","Estimate"]
rmse_lm <- rmse(yt_vids_lm, yt_videos_test)
lm_model_summary <- as.data.frame(xtable::xtable(summary(yt_vids_lm)))
rownames(lm_model_summary) <- c('(Intercept)', 'Likes', 'Dislikes', 'Comment Count')
lm_model_summary

# Get residuals
yt_videos_test <- yt_videos_test %>%
  add_residuals(yt_vids_lm)

# Plot residuals
resids_dislikes <- yt_videos_test %>% 
  ggplot(aes(dislikes/1000, resid/1000000)) +
  geom_hex(bins = 25, color = "white", show.legend = F)+
  scale_fill_gradient(high = "#E31A1C", low = "#FB9A99" ) +
  labs(x = 'Dislikes (in thousands)') +
  labs(y = 'Residuals (in millions)') +
  labs(title = 'Residuals vs. Dislikes') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

resids_likes <- yt_videos_test %>% 
  ggplot(aes(likes/1000, resid/1000000)) +
  geom_hex(bins = 25, color = "white", show.legend = F)+
  scale_fill_gradient(high = "#33A02C", low = "#B2DF8A") +
  labs(x = 'Likes (in thousands)') +
  labs(y = 'Residuals (in millions)') +
  labs(title = 'Residuals vs. Likes') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

resids_comments <- yt_videos_test %>% 
  ggplot(aes(comment_count/1000, resid/1000000)) +
  geom_hex(bins = 25, color = "white", show.legend = F) +
  scale_fill_gradient(high = "#1F78B4", low = "#A6CEE3") +
  labs(x = 'Commments (in thousands)') +
  labs(y = 'Residuals (in millions)') +
  labs(title = 'Residuals vs. Comments') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

grid.arrange(
  resids_dislikes, resids_likes, resids_comments, 
  nrow = 1, ncol = 3
)

# Log-transform the data
yt_vids_us_log <- yt_vids_us %>%
  mutate(
    lviews = log2(views + 1),
    llikes = log2(likes + 1),
    ldislikes = log2(dislikes + 1),
    lcomments = log2(comment_count + 1)
  )

# Plot the log-transformed data
views_dislikes_log <- yt_vids_us_log %>%
  ggplot(aes(ldislikes, lviews)) +
  geom_hex(bins = 25, color = "white", show.legend = F)+
  scale_fill_gradient(high = "#E31A1C", low = "#FB9A99" ) +
  geom_smooth(color = "#E31A1C", se = F, show.legend = F) +
  labs(x = 'log(Dislikes)') +
  labs(y = 'log(Views)') +
  labs(title = 'log(Views) vs. log(Dislikes)') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

views_likes_log  <- yt_vids_us_log %>%
  ggplot(aes(llikes, lviews)) +
  geom_hex(bins = 25, color = "white", show.legend = F)+
  scale_fill_gradient(high = "#33A02C", low = "#B2DF8A") +
  geom_smooth(color = "#33A02C", se = F, show.legend = F) +
  labs(x = 'log(Likes)') +
  labs(y = 'log(Views)') +
  labs(title = 'log(Views) vs. log(Likes)') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

views_comments_log  <- yt_vids_us_log %>%
  ggplot(aes(lcomments, lviews)) +
  geom_hex(bins = 25, color = "white", show.legend = F)+
  scale_fill_gradient(high = "#1F78B4", low = "#A6CEE3") +
  geom_smooth(color = "#1F78B4", se = F, show.legend = F) +
  labs(x = 'log(Commments)') +
  labs(y = 'log(Views)') +
  labs(title = 'log(Views) vs. log(Commments)') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

grid.arrange(
  views_dislikes_log ,views_likes_log, views_comments_log,
  nrow = 1, ncol = 3
)

# Split the data
yt_videos_train_log <- yt_vids_us_log %>% slice(yt_videos_train_indices)
yt_videos_test_log <- yt_vids_us_log %>% slice(-yt_videos_train_indices)

# Fit the log-transformed model
yt_vids_lm_log <- lm(lviews ~ llikes + ldislikes + lcomments, data = yt_videos_train_log)

# Log-transformed model output
r_sq_log <- summary(yt_vids_lm_log)$r.squared
adj_r_sq_log <- summary(yt_vids_lm_log)$adj.r.squared
coef_dislikes_log <- coef(summary(yt_vids_lm_log))["ldislikes","Estimate"]
coef_likes_log <- coef(summary(yt_vids_lm_log))["llikes","Estimate"]
coef_comments_log <- coef(summary(yt_vids_lm_log))["lcomments","Estimate"]
p_comments_log <- coef(summary(yt_vids_lm_log))["lcomments","Pr(>|t|)"]

# Back transform the data and get residuals and predictions
yt_videos_test_log <- yt_videos_test_log %>%
  add_residuals(yt_vids_lm_log, "lresid") %>%
  add_predictions(yt_vids_lm_log, "lviews") %>%
  mutate(
    resid = (2 ^ lresid) - 1,
    pred_views = (2 ^ lviews) - 1
  )

# Calculate RMSE
rmse_lm_log <- sqrt(mean((yt_videos_test_log$pred_views - yt_videos_test_log$views)^2))

# Model output summary
lm_model_summary_log <- as.data.frame(xtable::xtable(summary(yt_vids_lm_log)))
rownames(lm_model_summary_log) <- c('(Intercept)', 'Likes', 'Dislikes', 'Comment Count')
lm_model_summary_log

# Plot the residuals
resids_dislikes_log <- yt_videos_test_log %>% 
  ggplot(aes(dislikes/1000, resid/1000000)) +
  geom_hex(bins = 25, color = "white", show.legend = F)+
  scale_fill_gradient(high = "#E31A1C", low = "#FB9A99" ) +
  labs(x = 'Dislikes (in thousands)') +
  labs(y = 'Residuals (in millions)') +
  labs(title = 'Residuals vs. Dislikes') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

resids_likes_log <- yt_videos_test_log %>% 
  ggplot(aes(likes/1000, resid/1000000)) +
  geom_hex(bins = 25, color = "white", show.legend = F)+
  scale_fill_gradient(high = "#33A02C", low = "#B2DF8A") +
  labs(x = 'Likes (in thousands)') +
  labs(y = 'Residuals (in millions)') +
  labs(title = 'Residuals vs. Likes') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

resids_comments_log <- yt_videos_test_log %>% 
  ggplot(aes(comment_count/1000, resid/1000000)) +
  geom_hex(bins = 25, color = "white", show.legend = F) +
  scale_fill_gradient(high = "#1F78B4", low = "#A6CEE3") +
  labs(x = 'Commments (in thousands)') +
  labs(y = 'Residuals (in millions)') +
  labs(title = 'Residuals vs. Comments') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

grid.arrange(
  resids_dislikes_log, resids_likes_log, resids_comments_log,
  nrow = 1, ncol = 3
)

# Format the tags data
all_tags <- tolower(yt_vids_us$tags)
all_tags <- gsub("^[ ]+", "", gsub("[^a-z|]+", " ", all_tags))
all_tags <- paste(all_tags, collapse = "|") 
all_tags <- str_trim(unlist(strsplit(all_tags, split = '\\|'))) 

keep_words <- c('2017', '2018', 'how', 'to')
stop_words <- setdiff(stopwords('english'), keep_words)

all_tags <- removeWords(all_tags, stop_words)
all_tags <- all_tags[!(is.na(all_tags) | all_tags == "" | all_tags == " " | all_tags == "none")]
all_tags <- str_trim(all_tags)
all_tags_tab <- as_tibble(sort(xtabs(~all_tags), decreasing = TRUE))
all_tags_tab <- all_tags_tab %>% filter(!(is.na(all_tags) | all_tags == "" | all_tags == " "))

top_500tags <- all_tags_tab[1:500, ]

# Generate the wordcloud
wordcloud(
  words = top_500tags$all_tags, freq = top_500tags$n, min.freq = 1,
  max.words=500, random.order=F, rot.per=0.35, colors=brewer.pal(8, "Paired")
)

# Add tag logical variables to the dataset
for (tag in top_500tags %>% pull(all_tags)) {yt_vids_us <- yt_vids_us %>% mutate(!!tag := grepl(tag, tags))}

# Split the data
yt_videos_train <- yt_vids_us %>% slice(yt_videos_train_indices)
yt_videos_test <- yt_vids_us %>% slice(-yt_videos_train_indices)

rf_data_train <- yt_videos_train %>% select(views, channel_title, category_title, last_col(0:499))
colnames(rf_data_train) <- str_replace_all(colnames(rf_data_train), ' ', '_')

rf_data_test <- yt_videos_test %>% select(views, channel_title, category_title, last_col(0:499))
colnames(rf_data_test) <- str_replace_all(colnames(rf_data_test), ' ', '_')

# Fit the Random Forest model
yt_vids_tags_rf <- randomForest(views ~ ., data = rf_data_train, importance = TRUE, do.trace = 10, ntree = 500)

# Model output
num_trees <- yt_vids_tags_rf$ntree
num_pred_at_node <- yt_vids_tags_rf$mtry
rf_mse <- yt_vids_tags_rf$mse[num_trees]
rf_rmse_test <- rmse(yt_vids_tags_rf, rf_data_test)
rf_rsq <- yt_vids_tags_rf$rsq[num_trees]

# Most important variables
imp_rf_20 <- importance(yt_vids_tags_rf) %>% as_tibble(rownames = "var") 

imp_rf_20$var <- str_replace(
  imp_rf_20$var, str_sub(imp_rf_20$var, start = 1, end = 1),
  toupper(str_sub(imp_rf_20$var, start = 1, end = 1))
)
imp_rf_20$var <- str_replace_all(imp_rf_20$var, "_", " ")

# Add columns for time of day when videos were published
breaks_tod <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
labels_tod <- c("Night", "Morning", "Afternoon", "Evening")

yt_vids_us <- yt_vids_us %>%
  mutate(
  publish_hour = hour(yt_vids_us$publish_time),
  publish_tod = cut(publish_hour, breaks = breaks_tod, labels = labels_tod, include.lowest = T)
  ) %>%
  group_by(channel_title) %>%
  mutate(tot_views = sum(views)) %>%
  ungroup()

# Add columns for channel size
quantiles <- quantile(yt_vids_us$tot_views, c(0.25, 0.5, 0.75, 1))
breaks_size <- c(0, quantiles[1] , quantiles[2], quantiles[3], quantiles[4])
labels_size <- c("Small", "Medium", "Big", "Mega")
 
yt_vids_us <- yt_vids_us %>% mutate(
  channel_size = cut(yt_vids_us$tot_views, breaks = breaks_size, labels = labels_size, right = T)
  )

# Group data by time of day when videos were published
tod_df <- yt_vids_us %>% group_by(publish_tod) %>% summarise(
    count = n(),
    pct = round(count / nrow(yt_vids_us) * 100, 2)
  ) %>% arrange(desc(count))

# Group data by the hour when videos were published
time_df <- yt_vids_us %>% group_by(publish_hour) %>% summarise(
    count = n(),
    pct = round(count / nrow(yt_vids_us) * 100,2)
  ) %>% arrange(desc(count))

# Plot the data by the hour when videos were published
time_df %>% ggplot(aes(publish_hour, count, fill = count)) + geom_col(show.legend = F) + 
  scale_x_discrete(limits = 0:23) +
  scale_fill_gradient2() +
  labs(y = 'Videos') +
  labs(x = 'Hour') +
  labs(title = 'Videos published by hour') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12))

# Plot the data by the category and hour when videos were published
cat_plot <- yt_vids_us %>% group_by(category_title, publish_hour) %>% 
  summarise(n = n()) %>%
  ggplot(aes(publish_hour, n, color = str_wrap(category_title, 15))) + 
  geom_path() +
  geom_point() +
  scale_x_discrete(limits = 0:23) +
  scale_fill_gradient2() +
  labs(y = 'Videos') +
  labs(x = 'Hour') +
  labs(title = 'Videos published by hour and category') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12), legend.position = "top", legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 2, byrow = T))
cat_plot

# Plot the data by the channel size and hour when videos were publishe
size_plot <- yt_vids_us %>% group_by(channel_size, publish_hour) %>% 
  summarise(n = n()) %>%
  ggplot(aes(publish_hour, n, color = channel_size)) + 
  geom_path() +
  geom_point() + 
  scale_x_discrete(limits = 0:23) +
  labs(y = 'Videos') +
  labs(x = 'Hour') +
  labs(title = 'Videos published by hour and channel size') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12), legend.position = 'top', legend.title = element_blank())
size_plot

# Add publish time of day logical variables to the dataset
for (tod in yt_vids_us$publish_tod) {yt_vids_us <- yt_vids_us %>% mutate(!!tod := grepl(tod, publish_tod))}

# Extract top 200 most important variables identified by the Random Forest model
least_imp_300 <- imp_rf_20 %>% arrange(`%IncMSE`) %>% head(300)
copy_vids <- yt_vids_us
names(copy_vids) <- str_replace_all(names(copy_vids), ' ', '_')
least_imp_300$var <- tolower(str_replace_all(least_imp_300$var, ' ', '_'))
copy_vids <- copy_vids %>% select(-c(least_imp_300$var, video_id, trending_date, title, publish_time, tags, comments_disabled, ratings_disabled, video_error_or_removed, publish_hour, publish_tod, tot_views))

# Split the data
rf_data_train_final <- copy_vids %>% slice(yt_videos_train_indices)
rf_data_test_final <- copy_vids %>% slice(-yt_videos_train_indices)

rf_data_train_final$channel_size <- as.character(rf_data_train_final$channel_size)
rf_data_test_final$channel_size <- as.character(rf_data_test_final$channel_size)

# Fit the Random Forest model
yt_vids_rf_final <- randomForest(views ~ ., data = rf_data_train_final, importance = TRUE, do.trace = 10, ntree = 500)

# Model output
num_trees_final <- yt_vids_rf_final$ntree
num_pred_at_node_final <- yt_vids_rf_final$mtry
rf_mse_final <- yt_vids_rf_final$mse[num_trees]
rf_rmse_test_final <- rmse(yt_vids_rf_final, rf_data_test_final)
rf_rsq_final <- yt_vids_rf_final$rsq[num_trees]

# Most important variables
imp_rf_20_final <- importance(yt_vids_rf_final) %>% 
  as_tibble(rownames = "var") 

imp_rf_20_final$var <- str_replace(
  imp_rf_20_final$var, str_sub(imp_rf_20_final$var, start = 1, end = 1),
  toupper(str_sub(imp_rf_20_final$var, start = 1, end = 1))
)
imp_rf_20_final$var <- str_replace_all(imp_rf_20_final$var, "_", " ")

# Plot the RF importance
plot_imp_mse_final <- imp_rf_20_final %>%
  arrange(desc(`%IncMSE`)) %>%
  head(20) %>%
  ggplot(aes(reorder(var, `%IncMSE`), `%IncMSE`, fill = `%IncMSE`)) +
  geom_col(show.legend = F) +
  scale_fill_gradient2() +
  labs(y = '% Increase in Mean Squared Error') +
  labs(x = 'Category / Channel') +
  labs(title = 'Top 20 most important variables') +
  theme_minimal(base_family = 'Times') +
  theme(plot.title = element_text(size = 12)) +
  coord_flip()
plot_imp_mse_final

# Compare the models
model_comp <- data.frame('Model' = c('Linear (cont)', 'Linear (log)', 'Random Forest (cat)', 'Random Forest (agg)'))
model_comp <- model_comp %>% cbind('RMSE' = c(rmse_lm, rmse_lm_log, rf_rmse_test, rf_rmse_test_final),
                                   'R-squared' = c(r_sq, r_sq_log, rf_rsq, rf_rsq_final))
model_comp


