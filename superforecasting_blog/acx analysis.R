library(tidyverse)

df = read.csv("data/acx 2022 predictions.csv")

# convert to tidy format
# first, take columns with guesses (note: excluding Scott from this)
columnwise_pred = as.matrix(df[1:70,13:ncol(df)])
# rotate columns and append to dataframe with IDs + other info
preds = cbind(data.frame(id = seq(1, dim(columnwise_pred)[2])),
              pred_mkt_exp = as.character(df[72,13:ncol(df)]),
              superforecaster = as.character(df[73,13:ncol(df)]),
              time = as.character(df[74,13:ncol(df)]),
              lw_id = as.character(df[75,13:ncol(df)]),
              ea_id = as.character(df[76,13:ncol(df)]),
              profession = as.character(df[77,13:ncol(df)]),
              degree = as.character(df[78,13:ncol(df)]),
              t(columnwise_pred))
# change column names of questions for ease of use and remove row names
rownames(preds) = NULL
names(preds)[9:ncol(preds)] = paste0("Q", seq(1,70))
# replace blanks with NA
preds[preds == ''] = NA

# create a data frame with question info
q_info = data.frame(q_id = paste0("Q", seq(1,70)),
                    resolution = as.numeric(df[1:70,2]),
                    question = as.character(df[1:70,1]),
                    scott = as.numeric(df[1:70,3]/100))

##### Plot questions answered by superforecaster status

plot0 = preds %>%
  mutate(Superforecaster = case_when(superforecaster == "Yes" ~ "Superforecaster",
                                     TRUE ~ "Non-Superforecaster")) %>%
  pivot_longer(cols = starts_with("Q"), names_to = "question", values_to = "probability") %>%
  filter(!is.na(probability)) %>%
  group_by(id) %>%
  tally()

plot0 = merge(plot0, preds[,c("id","superforecaster")] %>%
                mutate(Superforecaster = case_when(superforecaster == "Yes" ~ "Superforecaster",
                                                   TRUE ~ "Non-Superforecaster")) , all.x = T) %>%
  select(!superforecaster)

ggplot(plot0, aes(x = n, color = Superforecaster)) +
  geom_density(fill = NA) +
  scale_x_continuous(name = "Number of Questions Answered", expand = c(0,0)) +
  scale_y_continuous(name = "Density", expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = "bottom")

##### Plot answers to each question by superforecaster status
# count how many answered each question
plot1 = preds %>%
  mutate(Superforecaster = case_when(superforecaster == "Yes" ~ "Superforecaster",
                                     TRUE ~ "Non-Superforecaster")) %>%
  pivot_longer(cols = starts_with("Q"), names_to = "question", values_to = "probability") %>%
  filter(!is.na(probability)) %>%
  select(Superforecaster, question) %>%
  group_by(Superforecaster, question) %>%
  count(Superforecaster, name = "answered")

# replace NA with 0 and convert to proportions
plot1[is.na(plot1$answered),"answered"] = 0
plot1$answered = as.double(plot1$answered)
plot1[plot1$Superforecaster == "Superforecaster","answered"] = 
  as.numeric(unlist(plot1[plot1$Superforecaster == "Superforecaster", "answered"])) / 12
plot1[plot1$Superforecaster == "Non-Superforecaster", "answered"]= 
  as.numeric(unlist(plot1[plot1$Superforecaster == "Non-Superforecaster", "answered"]))  / 496
plot1$question = with(plot1, reorder(question, answered, FUN = min))

# in all but how many q's do superforecasters answer more than non-superforecasters? 61 / 70
plot1 %>%
  pivot_wider(names_from = Superforecaster, values_from = answered) %>%
  filter(Superforecaster > `Non-Superforecaster`) %>%
  nrow()

# plot
ggplot(plot1, aes(x = answered, y = question, color = Superforecaster)) +
  geom_point() +
  scale_x_continuous(name = "Proportion Answered", expand = c(0.001,0.001)) +
  scale_y_discrete(name = "Question", expand = c(0.001,0.001)) +
  theme_bw() +
  theme(legend.position = "bottom") 

##### Calibration 
plot2 = preds %>%
  mutate(Superforecaster = case_when(superforecaster == "Yes" ~ "Superforecaster",
                                     TRUE ~ "Non-Superforecaster")) %>%
  pivot_longer(cols = starts_with("Q"), names_to = "question", values_to = "probability") %>%
  filter(!is.na(probability))
plot2 = merge(plot2, q_info[,c("q_id","resolution")] %>% filter(!is.na(resolution)), 
              by.x = "question", by.y = "q_id", 
              all.x = TRUE) %>%
  filter(!is.na(resolution)) %>%
  mutate(probability = as.numeric(probability)/100,
         bin = cut_width(probability, 0.05, labels = FALSE),
         bin_avg = (bin-1)*0.05) %>%
  group_by(Superforecaster, bin_avg) %>%
  summarise(mean_resolution = mean(resolution))

# plot calibration - hmm...
ggplot(plot2, aes(x = bin_avg,
                  y = mean_resolution,
                  color = Superforecaster)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se=FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  scale_x_continuous(limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = c(0.01,0.01), name = "Probability") +
  scale_y_continuous(limits = c(0,1), breaks = c(0,1), expand = c(0.01,0.01), name = "Resolution") +
  theme_bw() +
  theme(legend.position = "bottom")
  
##### Permutation test for group scores
# replace NA values with Scott's prediction
plot3 = preds %>%
  pivot_longer(cols = starts_with("Q"), names_to = "question", values_to = "probability") %>%
  mutate(probability = as.numeric(probability)/100,
         Superforecaster = case_when(superforecaster == "Yes" ~ "Superforecaster",
                                     TRUE ~ "Non-Superforecaster"))
plot3 = merge(plot3, q_info[,c("q_id","scott")], by.x = "question", by.y = "q_id", all.x = TRUE)
plot3[is.na(plot3$probability),"probability"] = plot3[is.na(plot3$probability),"scott"]  
  
# function to calculate logloss score
logloss = function(y, p) {
  return()
}

# Question level permutations
# make empty dataframe to hold results
ql_perm = data.frame(perm = numeric(),
                     question = character(),
                     avg_prob = numeric())
# get ID numbers of non-superforecasters
non_sf = plot3[plot3$Superforecaster == "Non-Superforecaster", "id"]
# get N permutations
set.seed(108)
for(i in 1:10000){
  ql_perm = rbind(ql_perm,
                  plot3 %>%
                    filter(id %in% sample(non_sf, 12)) %>%
                    group_by(question) %>%
                    summarize(avg_prob = mean(probability)) %>%
                    mutate(perm = i))
}
# merge in resolutions
ql_perm = merge(ql_perm, q_info[,c("q_id","resolution")], by.x = "question", by.y = "q_id", all.x = TRUE)
# add log-loss scores
ql_perm$score = mapply(FUN = function(x,y) -(y*log(x) + (1-y)*log(1-x)),
                       x = ql_perm$avg_prob,
                       y = ql_perm$resolution)
ql_perm = ql_perm %>% filter(!is.na(score))

# get scores for superforecasters
sf_scores = read.csv("data/acx 2022 scores.csv")[1:70,7]
ql_sf = data.frame(question = paste0("Q", seq(1,70)),
                   sf_score = sf_scores) %>%
  filter(!is.na(sf_score))

# reorder
ql_perm = merge(ql_perm, ql_sf, by = "question")
ql_perm$question = with(ql_perm, reorder(question, sf_score))
ql_sf$question = with(ql_sf, reorder(question, sf_score))

# plot permutation results
ggplot() +
  geom_point(data = ql_perm,
             aes(x = score, y = question),
             color = "gray",
             shape = 1,
             fill = NA) +
  geom_point(data = ql_sf,
             aes(x = sf_score, y = question),
             shape = 3,
             color = "red") +
  scale_x_continuous(expand = c(0,0), name = "Log-Loss Score") +
  scale_y_discrete(name = "")

# get probability that randomly selected 12 non-superforecasters are better
# than superforecaster aggregate
ql_perm_sum = ql_perm %>%
  mutate(non_sf_better = score < sf_score) %>%
  group_by(question) %>%
  summarize(pr_better = sum(non_sf_better)/10000)

# probability that non-sf groups are better (39%)
nrow(ql_perm_sum[ql_perm_sum$pr_better > 0.5,]) / nrow(ql_perm_sum)

# plot the above
ggplot(ql_perm_sum, aes(x = pr_better)) +
  geom_density() +
  scale_x_continuous(name = "Probability that non-superforecaster groups are more accurate for a given question",
                     breaks = c(0,0.25,0.5,0.75,1),
                     limits = c(0,1),
                     expand = c(0,0)) +
  scale_y_continuous(name = "Density", expand = c(0,0)) +
  theme_bw()

##### Tournament level predictions
#get superforecaster sum
sf_sum = sum(ql_sf$sf_score)
ql_sum = ql_perm %>%
  group_by(perm) %>%
  summarize(sum_score = sum(score))

#plot
ggplot() +
  geom_density(data = ql_sum, 
               aes(x = sum_score),
               color = "gray") +
  geom_vline(aes(xintercept = sf_sum),
             color = "red") +
  scale_x_continuous(expand = c(0,0),
                     name = "Non-superforecaster group scores") +
  scale_y_continuous(name = "Density",
                     expand = c(0,0)) +
  theme_bw()

# proportion of non-superforecaster groups with higher score
nrow(ql_sum[ql_sum$sum_score < sf_sum,]) / nrow(ql_sum)
