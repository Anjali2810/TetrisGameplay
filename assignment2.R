# Installing tidyverse package to get download all libraries
install.packages(c("tidyverse"))

# Loading libraries
library(dplyr)
library(ggplot2)

# Loading data from csv provided
tetris = read.csv ("tetris.csv")

# As usual we'll check out the head and tail. Note that this data frame has 64 rows, so once again you might not want to look at the whole thing in the console.
head(tetris)
tail(tetris)

#Check for duplicates
tetris = tetris %>% distinct()

# Check null values
sum(is.na(tetris))

#Verify condition values are valid and consistent
unique(tetris$condition)

#Verify the score range
range(tetris$score)

# Separating for auditory and visual scores.
auditory_scores = tetris$score[tetris$condition == "auditory"]
visual_scores = tetris$score[tetris$condition == "visual"]

# Now let's generate some descriptive statistics for the scores.
mean (auditory_scores)
sd(auditory_scores)
max (auditory_scores)
min (auditory_scores)
median (auditory_scores)
IQR (auditory_scores)

mean (visual_scores)
sd(visual_scores)
max (visual_scores)
min (visual_scores)
median (visual_scores)
IQR (visual_scores)

#Import libraries for plots
library(ggplot2)

#Plotting box plot 
boxplot <- ggplot(tetris, aes(x = condition, y = score, fill = condition)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(
       x = "Condition",
       y = "Score") +
  theme_minimal() +
  scale_fill_manual(values = c("auditory" = "skyblue", "visual" = "lightgreen"))

print(boxplot)

# Histogram
histogram <- ggplot(tetris, aes(x = score, fill = condition)) +
  geom_histogram(alpha = 0.6, bins = 10, position = "identity") +
  geom_density(alpha = 0.3) +
  labs(title = "Histogram of Scores by Condition",
       x = "Score",
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("auditory" = "skyblue", "visual" = "lightgreen"))

print(histogram)

# Before performing t-test, we shall check the normality using shapiro-wilk normality test
shapiro.test (auditory_scores)
shapiro.test (visual_scores)

## Installing car package to run Levene's test.
install.packages ('car')
library ('car')
leveneTest(tetris$score, tetris$condition)

# Now lets run the welch two sample t-test
t.test (auditory_scores, visual_scores, paired=FALSE)

# Now let's generate bar plot with error bars
# Derive mean and standard variance for the data
mean = c(mean(visual_scores), mean(auditory_scores))
sd = c(sd(visual_scores), sd(auditory_scores))
names(mean) = c("Visual", "Auditory")

# Now generate barplot
bar = barplot(mean,
               main = "Mean Scores by Condition",
               xlab = "Condition",
               ylab = "Mean Score",
               ylim = c(0, 100),
               col = c("skyblue", "lightgreen"))

# Adding error bars
se = sd / sqrt(c(length(visual_scores), length(auditory_scores)))  
arrows(bar, mean + se, bar, mean - se, angle = 90, code = 3, length = 0.1)
