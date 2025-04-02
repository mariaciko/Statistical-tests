# 1) Header-----------------------------------------
# MariaCiko_Unit1Part1_BIOL672
# MacOS Sonoma 14.6.1
# Libraries/Packages: ggplot2, grid, tidyverse
# Data files: starships.csv

library("ggplot2")
library("grid")
library(tidyverse)

# 2) Dataset ---------------------------------------
# Generate 5000 random numbers from a normal distribution
set.seed(123)  
random_numbers <- rnorm(n = 5000, mean = 0, sd = 1) # rnorm() generates normally distributed random numbers
data = data.frame(random_numbers)

# Calculate the sample mean and standard deviation
sample_mean <- mean(random_numbers)
sample_sd <- sd(random_numbers)

# Print the results
print(paste("Sample Mean:", sample_mean))
print(paste("Sample Standard Deviation:", sample_sd))

# Plot a histogram with a density line
myPlot1<-ggplot(data, aes(x = random_numbers)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Histogram with Density Line", x = "Random Numbers", y = "Density")
print(myPlot1)

# Overlay a normal distribution curve
myPlot2<-ggplot(data = data, aes(x = random_numbers)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  geom_density(color = "red", linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = sample_mean, sd = sample_sd), color = "blue", size = 1) +
  labs(title = "Histogram with Density Line and Normal Curve", x = "Random Numbers", y = "Density")
print(myPlot2)

cat("Sample Mean:", sample_mean, "\n")
cat("Sample Standard Deviation:", sample_sd, "\n")

# 3) Automate the printing of stat test output ---------
# Redirect output to a file
sink("desc.txt")
cat("Sample Mean:", sample_mean, "\n")
cat("Sample Standard Deviation:", sample_sd, "\n")
sink()  # Stop redirecting output

# Save plot as Rplots.pdf by default
pdf("Rplots.pdf")
myPlot3<-ggplot(data = data, aes(x = random_numbers)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "lightblue") +
  geom_density(color = "red", linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = sample_mean, sd = sample_sd), color = "blue", size = 1) +
  labs(title = "Histogram with Density Line and Normal Curve", x = "Random Numbers", y = "Density")
print(myPlot3)
dev.off()  # Close the PDF device

# Rename the saved Rplots.pdf to histo.pdf
file.rename("Rplots.pdf", "histo.pdf")

# put plots on single PDF page
library('grid')
pushViewport(viewport(layout = grid.layout(1, 2)))
print(myPlot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(myPlot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))


# 4) One-way ANOVA -------------------------------------------------------------
# Research Question: Are there significant differences in hyperdrive ratings 
# between starships from different manufacturers?

# Load dataset for one-way ANOVA
# df <- read.table("data/archive/csv/starships.csv", sep = ",", header = TRUE)
df <- read.table("starships.csv", sep = ",", header = TRUE) |>
  select(manufacturer, hyperdrive_rating)
glimpse(df)

# Convert manufacturer to a factor (categorical variable)
df$manufacturer <- as.factor(df$manufacturer)

# Keep only manufacturers with more than 1 observations
df_filtered <- df |>
  group_by(manufacturer) |>
  filter(n() > 1) |>
  ungroup()

# Perform one-way ANOVA assuming equal variances
anova_result <- oneway.test(hyperdrive_rating ~ manufacturer, data = df_filtered, var.equal = TRUE)
print(anova_result)

# Create the error bar plot in descending order of ratings for x-axis readability
myErrorBarPlot <- ggplot(df_filtered, aes(x = reorder(manufacturer, -hyperdrive_rating, FUN = mean), 
                                                      y = hyperdrive_rating, fill = manufacturer)) + scale_fill_brewer(palette = "OrRd") +
  stat_summary(fun = mean, geom = "bar", color = "black", show.legend = FALSE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Error Bar Plot of Hyperdrive Rating by Manufacturer", x = "Manufacturer", y = "Hyperdrive Rating") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, vjust = 1)) # tilt x axis labels for readability
ggsave("error_bar_chart.pdf") # Save the plot as a PDF file

# Perform pairwise t-tests with multiple test correction
pairwise_ttests <- pairwise.t.test(df_filtered$hyperdrive_rating, df_filtered$manufacturer, 
                                   p.adjust.method = "none")

# Perform pairwise t-tests with Bonferroni correction
pairwise_bonferroni <- pairwise.t.test(df_filtered$hyperdrive_rating, df_filtered$manufacturer, 
                                       p.adjust.method = "bonferroni")

# Perform pairwise t-tests with Benjamini-Hochberg correction
pairwise_bh <- pairwise.t.test(df_filtered$hyperdrive_rating, df_filtered$manufacturer, 
                               p.adjust.method = "BH")

# Export pairwise t-test results to text files
capture.output(print(pairwise_ttests), file = "pairwise_ttests.txt")
capture.output(print(pairwise_bonferroni), file = "pairwise_ttests_bonferroni.txt")
capture.output(print(pairwise_bh), file = "pairwise_ttests_bh.txt")


# 5) Kruskal-Wallis test, Pearson and Spearman Correlation Tests------
kruskal_result <- kruskal.test(hyperdrive_rating ~ manufacturer, data = df_filtered)
print(kruskal_result)

# Save ANOVA and Kruskal-Wallis test results to a text file
capture.output({
  print(anova_result)
  print(kruskal_result)}, 
  file = "anova_kruskal_results.txt")

# Verbal interpretation of the results
interpretation <- "Interpretation:
1. ANOVA Results: The ANOVA test indicated no significant difference in hyperdrive ratings across different manufacturers (p-value > 0.05).
2. Pairwise T-tests: Pairwise t-tests further examine differences between each pair of manufacturers. Adjustments with Bonferroni and Benjamini-Hochberg methods control for false positives due to multiple comparisons.
3. Kruskal-Wallis Test: Similar to the ANOVA, the Kruskal-Wallis test revealed no significant differences in hyperdrive ratings among manufacturers, confirming the results from the ANOVA.
"

# Print to console and export interpretation to a text file
cat(interpretation)
capture.output(cat(interpretation), file = "interpretation.txt")



# Pearson and Spearman Correlation Tests
# Select two numeric columns for correlation (e.g., hyperdrive_rating and max_atmosphering_speed)
df_corr <- read.table("starships.csv", sep = ",", header = TRUE) |>
  select(hyperdrive_rating, max_atmosphering_speed)

# Pearson correlation (assumes normality)
pearson_test <- cor.test(df_corr$hyperdrive_rating, df_corr$max_atmosphering_speed, method = "pearson")
print(pearson_test)
capture.output(print(pearson_test), file = "pearson_correlation.txt")

# Spearman rank correlation (does not assume normality)
spearman_test <- cor.test(df_corr$hyperdrive_rating, df_corr$max_atmosphering_speed, method = "spearman")
print(spearman_test)
capture.output(print(spearman_test), file = "spearman_correlation.txt")

# Scatterplot for Pearson correlation
ggplot(df_corr, aes(x = max_atmosphering_speed, y = hyperdrive_rating)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = "blue") +
  labs(title = "Hyperdrive Rating vs max_atmosphering_speed (Pearson)", 
       x = "max_atmosphering_speed", y = "Hyperdrive Rating")
ggsave("scatterplot_pearson.pdf")

# Scatterplot for Spearman correlation
ggplot(df_corr, aes(x = max_atmosphering_speed, y = hyperdrive_rating)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = "red") +
  labs(title = "Hyperdrive Rating vs max_atmosphering_speed (Spearman)", 
       x = "max_atmosphering_speed", y = "Hyperdrive Rating")
ggsave("scatterplot_spearman.pdf")


# Perform one-sample KS test on hyperdrive_rating to compare with normal distribution
ks_test <- ks.test(df_filtered$hyperdrive_rating, "pnorm",
                   mean = mean(df_filtered$hyperdrive_rating, na.rm = TRUE),
                   sd = sd(df_filtered$hyperdrive_rating, na.rm = TRUE))
print(ks_test)
capture.output(print(ks_test), file = "ks_test_results.txt")

# Verbal interpretation of the KS test results
interpretation2 <- "Correlation and KS Interpretation:
4. Correlation: Both Pearson and Spearman correlations between hyperdrive rating and max atmosphering speed showed a moderate negative correlation (with the Spearman showing a higher negative correlation), suggesting that as the max atmosphering speed increases, the hyperdrive rating tends to decrease.
5. KS Test for normality: p-value (0.05322) slightly bigger than 0.05, indicating that the hyperdrive ratings do not strictly follow a normal distribution."

full_interpretation <- paste(interpretation, interpretation2, sep="\n")
cat(full_interpretation)
capture.output(cat(full_interpretation), file = "full_interpretation.txt")



# 6) Linear Regression--------------------------------
# Run linear regression
linear_model <- lm(hyperdrive_rating ~ max_atmosphering_speed, data = df_corr)
summary(linear_model)

# Save the linear regression results
capture.output(summary(linear_model), file = "linear_regression_results.txt")

# Plot the regression line with confidence intervals
reg_line <- ggplot(df_corr, aes(x = max_atmosphering_speed, y = hyperdrive_rating)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE, color = "blue") +
  labs(title = "Linear Regression: Hyperdrive Rating vs max_atmosphering_speed",
       x = "max_atmosphering_speed", y = "Hyperdrive Rating") +
  theme_minimal()
ggsave("linear_regression_plot.pdf")

interpretation3 <- "\n Linear regression Interpretation:
6. There is still a moderately indirect relationship between max atmosphering speed and hyperdrive rating, confirming our previous findings with the Pearson and Spearman correlations.
However, the p-value < 0.05, meaning there is significance in max atmosphering speed determining hyperdrive rating.
Regression is more appropriate when you want to make predictions of the dependent variable based on the independent variable, and see how changing the latter affects the first."

full_interpretation <- paste(interpretation, interpretation2, interpretation3, sep="\n")
cat(full_interpretation)
capture.output(cat(full_interpretation), file = "full_interpretation.txt")

