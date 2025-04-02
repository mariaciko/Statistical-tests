# Statistical-tests
## Question: Are there significant differences in hyperdrive ratings between starships from different manufacturers?

Using the [starships.csv file](https://github.com/mariaciko/Statistical-tests/blob/main/starships.csv) from the [Star Wars Dataverse](https://www.kaggle.com/datasets/jsphyg/star-wars), we aim to answer the above question by implementing statistical tests including one-way ANOVA, Kruskal-Wallis, Pearson and Spearman correlation tests between variables hyperdrive_rating and max_atmosphering_speed, and running linear regression in R.

We find that:
  <ins>ANOVA Results:</ins> The ANOVA test indicated no significant difference in hyperdrive ratings across different manufacturers (p-value > 0.05).
  <ins>Pairwise T-tests:</ins> Pairwise t-tests further examine differences between each pair of manufacturers. Adjustments with Bonferroni and Benjamini-Hochberg methods control for false positives due to multiple comparisons.
  <ins>Kruskal-Wallis Test:</ins> Similar to the ANOVA, the Kruskal-Wallis test revealed no significant differences in hyperdrive ratings among manufacturers, confirming the results from the ANOVA.

The correlation tests reveal:
  a moderate negative correlation (with the Spearman showing a higher negative correlation), suggesting that as the max atmosphering speed increases, the hyperdrive rating tends to decrease.

KS Test for normality:
  p-value (0.05322) slightly bigger than 0.05, indicating that the hyperdrive ratings do not strictly follow a normal distribution.

Linear regression interpretation:
  There is still a moderately indirect relationship between max_atmosphering_speed and hyperdrive_rating, confirming our previous findings with the Pearson and Spearman correlations.
  However, the p-value < 0.05 implies the significance of max atmosphering speed determining hyperdrive rating.
  Regression is more appropriate when you want to make predictions of the dependent variable based on the independent variable, and see how changing the latter affects the first.
