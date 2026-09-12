# ============================================================
# LAB PROBLEM STATEMENT 6
# STATISTICAL ANALYSIS OF PHYSICAL CHARACTERISTICS
# OF PALMER PENGUINS
# ============================================================


# ============================================================
# 1. INSTALL AND LOAD REQUIRED PACKAGES
# ============================================================

# Run install.packages() only ONCE if packages are not installed.

install.packages("palmerpenguins")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("moments")
install.packages("car")
install.packages("effectsize")

# Load packages

library(palmerpenguins)
library(dplyr)
library(ggplot2)
library(moments)
library(car)
library(effectsize)


# ============================================================
# 2. LOAD PALMER PENGUINS DATASET
# ============================================================

data(penguins)

# Display first few observations

head(penguins)

# Structure of dataset

str(penguins)

# Summary of dataset

summary(penguins)

# Number of observations

nrow(penguins)

# Number of variables

ncol(penguins)


# ============================================================
# 3. CHECK MISSING VALUES
# ============================================================

# Missing values in each column

colSums(is.na(penguins))

# Number of missing values in body mass

sum(is.na(penguins$body_mass_g))

# Number of missing values in sex

sum(is.na(penguins$sex))

# Number of missing values in flipper length

sum(is.na(penguins$flipper_length_mm))


# ============================================================
# 4. CREATE CLEAN DATASETS
# ============================================================

# Dataset for body mass analysis

body_mass_data <- penguins %>%
  filter(!is.na(body_mass_g),
         !is.na(species))

# Dataset for male vs female analysis

sex_data <- penguins %>%
  filter(!is.na(body_mass_g),
         !is.na(sex))

# Dataset for two-way ANOVA

two_way_data <- penguins %>%
  filter(!is.na(body_mass_g),
         !is.na(species),
         !is.na(sex))

# Dataset for flipper length

flipper_data <- penguins %>%
  filter(!is.na(flipper_length_mm),
         !is.na(species))


# ============================================================
# 5. BASIC INFORMATION ABOUT SPECIES
# ============================================================

# Number of penguins belonging to each species

table(body_mass_data$species)

# Number of penguins by sex

table(sex_data$sex)

# Species and sex combination

table(two_way_data$species, two_way_data$sex)


# ============================================================
# 6. DESCRIPTIVE STATISTICS - OVERALL BODY MASS
# ============================================================

# Extract body mass

body_mass <- body_mass_data$body_mass_g

# Mean

mean_body_mass <- mean(body_mass)

# Median

median_body_mass <- median(body_mass)

# Minimum

min_body_mass <- min(body_mass)

# Maximum

max_body_mass <- max(body_mass)

# Variance

variance_body_mass <- var(body_mass)

# Standard deviation

sd_body_mass <- sd(body_mass)

# First quartile

Q1_body_mass <- quantile(body_mass, 0.25)

# Third quartile

Q3_body_mass <- quantile(body_mass, 0.75)

# Interquartile range

IQR_body_mass <- IQR(body_mass)

# Skewness

skewness_body_mass <- skewness(body_mass)

# Kurtosis

kurtosis_body_mass <- kurtosis(body_mass)

# Display results

cat("Mean:", mean_body_mass, "\n")
cat("Median:", median_body_mass, "\n")
cat("Minimum:", min_body_mass, "\n")
cat("Maximum:", max_body_mass, "\n")
cat("Variance:", variance_body_mass, "\n")
cat("Standard Deviation:", sd_body_mass, "\n")
cat("First Quartile:", Q1_body_mass, "\n")
cat("Third Quartile:", Q3_body_mass, "\n")
cat("IQR:", IQR_body_mass, "\n")
cat("Skewness:", skewness_body_mass, "\n")
cat("Kurtosis:", kurtosis_body_mass, "\n")


# ============================================================
# 7. DESCRIPTIVE STATISTICS BY SPECIES
# ============================================================

species_statistics <- body_mass_data %>%
  group_by(species) %>%
  summarise(
    Count = n(),
    Mean = mean(body_mass_g),
    Median = median(body_mass_g),
    Minimum = min(body_mass_g),
    Maximum = max(body_mass_g),
    Variance = var(body_mass_g),
    Standard_Deviation = sd(body_mass_g),
    Q1 = quantile(body_mass_g, 0.25),
    Q3 = quantile(body_mass_g, 0.75),
    IQR = IQR(body_mass_g),
    Skewness = skewness(body_mass_g),
    Kurtosis = kurtosis(body_mass_g)
  )

print(species_statistics)


# ============================================================
# 8. HISTOGRAM OF BODY MASS
# ============================================================

ggplot(body_mass_data, aes(x = body_mass_g)) +
  geom_histogram(
    bins = 30,
    color = "black"
  ) +
  labs(
    title = "Histogram of Penguin Body Mass",
    x = "Body Mass (g)",
    y = "Frequency"
  ) +
  theme_minimal()


# ============================================================
# 9. SPECIES-WISE BOXPLOT OF BODY MASS
# ============================================================

ggplot(body_mass_data,
       aes(x = species, y = body_mass_g)) +
  geom_boxplot() +
  labs(
    title = "Body Mass by Penguin Species",
    x = "Species",
    y = "Body Mass (g)"
  ) +
  theme_minimal()


# ============================================================
# 10. DENSITY PLOT OF BODY MASS
# ============================================================

ggplot(body_mass_data,
       aes(x = body_mass_g)) +
  geom_density() +
  labs(
    title = "Density Plot of Penguin Body Mass",
    x = "Body Mass (g)",
    y = "Density"
  ) +
  theme_minimal()


# ============================================================
# 11. SPECIES-WISE DENSITY PLOT
# ============================================================

ggplot(body_mass_data,
       aes(x = body_mass_g, fill = species)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Body Mass Distribution by Penguin Species",
    x = "Body Mass (g)",
    y = "Density",
    fill = "Species"
  ) +
  theme_minimal()


# ============================================================
# 12. SEX-WISE BODY MASS BOXPLOT
# ============================================================

ggplot(sex_data,
       aes(x = sex, y = body_mass_g)) +
  geom_boxplot() +
  labs(
    title = "Body Mass by Sex",
    x = "Sex",
    y = "Body Mass (g)"
  ) +
  theme_minimal()


# ============================================================
# 13. HYPOTHESIS TESTING: MALE VS FEMALE BODY MASS
# ============================================================

# Null hypothesis:
# H0: Mean body mass of male penguins = mean body mass
#     of female penguins.

# Alternative hypothesis:
# H1: Mean body mass of male penguins != mean body mass
#     of female penguins.


# ============================================================
# 14. MEAN BODY MASS BY SEX
# ============================================================

sex_summary <- sex_data %>%
  group_by(sex) %>%
  summarise(
    Count = n(),
    Mean = mean(body_mass_g),
    Median = median(body_mass_g),
    SD = sd(body_mass_g)
  )

print(sex_summary)


# ============================================================
# 15. SHAPIRO-WILK NORMALITY TEST
# ============================================================

# Male penguins

male_mass <- sex_data %>%
  filter(sex == "male") %>%
  pull(body_mass_g)

shapiro_male <- shapiro.test(male_mass)

print(shapiro_male)


# Female penguins

female_mass <- sex_data %>%
  filter(sex == "female") %>%
  pull(body_mass_g)

shapiro_female <- shapiro.test(female_mass)

print(shapiro_female)


# ============================================================
# 16. QQ-PLOTS FOR MALE AND FEMALE BODY MASS
# ============================================================

# Male QQ plot

qqnorm(
  male_mass,
  main = "QQ Plot - Male Penguin Body Mass"
)

qqline(male_mass)


# Female QQ plot

qqnorm(
  female_mass,
  main = "QQ Plot - Female Penguin Body Mass"
)

qqline(female_mass)


# ============================================================
# 17. INDEPENDENT TWO-SAMPLE T-TEST
# ============================================================

t_test_result <- t.test(
  body_mass_g ~ sex,
  data = sex_data,
  var.equal = FALSE
)

print(t_test_result)


# ============================================================
# 18. 95% CONFIDENCE INTERVAL
# ============================================================

cat(
  "95% Confidence Interval for Difference in Means:\n"
)

print(t_test_result$conf.int)


# ============================================================
# 19. COHEN'S d EFFECT SIZE
# ============================================================

cohen_d_result <- cohens_d(
  body_mass_g ~ sex,
  data = sex_data
)

print(cohen_d_result)


# ============================================================
# 20. ONE-WAY ANOVA
# ============================================================

# Model:
# body_mass_g ~ species

one_way_model <- aov(
  body_mass_g ~ species,
  data = body_mass_data
)

# ANOVA table

summary(one_way_model)


# ============================================================
# 21. ANOVA MODEL DETAILS
# ============================================================

anova_table <- summary(one_way_model)

print(anova_table)


# ============================================================
# 22. CHECK NORMALITY BY SPECIES
# ============================================================

# Adélie

adelie_mass <- body_mass_data %>%
  filter(species == "Adelie") %>%
  pull(body_mass_g)

shapiro_adelie <- shapiro.test(adelie_mass)

print(shapiro_adelie)


# Chinstrap

chinstrap_mass <- body_mass_data %>%
  filter(species == "Chinstrap") %>%
  pull(body_mass_g)

shapiro_chinstrap <- shapiro.test(chinstrap_mass)

print(shapiro_chinstrap)


# Gentoo

gentoo_mass <- body_mass_data %>%
  filter(species == "Gentoo") %>%
  pull(body_mass_g)

shapiro_gentoo <- shapiro.test(gentoo_mass)

print(shapiro_gentoo)


# ============================================================
# 23. QQ-PLOT BY SPECIES
# ============================================================

# QQ plots using the ANOVA model

par(mfrow = c(1, 3))

qqnorm(
  adelie_mass,
  main = "Adelie QQ Plot"
)
qqline(adelie_mass)

qqnorm(
  chinstrap_mass,
  main = "Chinstrap QQ Plot"
)
qqline(chinstrap_mass)

qqnorm(
  gentoo_mass,
  main = "Gentoo QQ Plot"
)
qqline(gentoo_mass)

par(mfrow = c(1, 1))


# ============================================================
# 24. HOMOGENEITY OF VARIANCE - LEVENE'S TEST
# ============================================================

levene_result <- leveneTest(
  body_mass_g ~ species,
  data = body_mass_data
)

print(levene_result)


# ============================================================
# 25. TUKEY HSD POST-HOC TEST
# ============================================================

tukey_result <- TukeyHSD(one_way_model)

print(tukey_result)


# ============================================================
# 26. TUKEY HSD PLOT
# ============================================================

plot(
  tukey_result,
  las = 1
)


# ============================================================
# 27. KRUSKAL-WALLIS TEST
# ============================================================

kruskal_result <- kruskal.test(
  body_mass_g ~ species,
  data = body_mass_data
)

print(kruskal_result)


# ============================================================
# 28. COMPARE ANOVA AND KRUSKAL-WALLIS RESULTS
# ============================================================

cat("One-Way ANOVA p-value:\n")

anova_p_value <- summary(one_way_model)[[1]][["Pr(>F)"]][1]

print(anova_p_value)

cat("\nKruskal-Wallis p-value:\n")

print(kruskal_result$p.value)


# ============================================================
# 29. TWO-WAY ANOVA
# ============================================================

# Model:
# body_mass_g ~ species * sex

two_way_model <- aov(
  body_mass_g ~ species * sex,
  data = two_way_data
)

summary(two_way_model)


# ============================================================
# 30. TWO-WAY ANOVA TABLE
# ============================================================

two_way_anova_table <- summary(two_way_model)

print(two_way_anova_table)


# ============================================================
# 31. SPECIES AND SEX INTERACTION VISUALIZATION
# ============================================================

ggplot(
  two_way_data,
  aes(
    x = species,
    y = body_mass_g,
    fill = sex
  )
) +
  geom_boxplot() +
  labs(
    title = "Body Mass by Species and Sex",
    x = "Species",
    y = "Body Mass (g)",
    fill = "Sex"
  ) +
  theme_minimal()


# ============================================================
# 32. MEAN BODY MASS BY SPECIES AND SEX
# ============================================================

species_sex_summary <- two_way_data %>%
  group_by(species, sex) %>%
  summarise(
    Count = n(),
    Mean = mean(body_mass_g),
    SD = sd(body_mass_g),
    .groups = "drop"
  )

print(species_sex_summary)


# ============================================================
# 33. FLIPPER LENGTH - DESCRIPTIVE STATISTICS
# ============================================================

flipper_summary <- flipper_data %>%
  group_by(species) %>%
  summarise(
    Count = n(),
    Mean = mean(flipper_length_mm),
    Median = median(flipper_length_mm),
    Minimum = min(flipper_length_mm),
    Maximum = max(flipper_length_mm),
    Variance = var(flipper_length_mm),
    SD = sd(flipper_length_mm),
    Q1 = quantile(flipper_length_mm, 0.25),
    Q3 = quantile(flipper_length_mm, 0.75),
    IQR = IQR(flipper_length_mm)
  )

print(flipper_summary)


# ============================================================
# 34. SPECIES-WISE FLIPPER LENGTH BOXPLOT
# ============================================================

ggplot(
  flipper_data,
  aes(
    x = species,
    y = flipper_length_mm
  )
) +
  geom_boxplot() +
  labs(
    title = "Flipper Length by Penguin Species",
    x = "Species",
    y = "Flipper Length (mm)"
  ) +
  theme_minimal()


# ============================================================
# 35. FLIPPER LENGTH ONE-WAY ANOVA
# ============================================================

flipper_anova <- aov(
  flipper_length_mm ~ species,
  data = flipper_data
)

summary(flipper_anova)


# ============================================================
# 36. FLIPPER LENGTH NORMALITY CHECK
# ============================================================

flipper_adelie <- flipper_data %>%
  filter(species == "Adelie") %>%
  pull(flipper_length_mm)

flipper_chinstrap <- flipper_data %>%
  filter(species == "Chinstrap") %>%
  pull(flipper_length_mm)

flipper_gentoo <- flipper_data %>%
  filter(species == "Gentoo") %>%
  pull(flipper_length_mm)


shapiro.test(flipper_adelie)

shapiro.test(flipper_chinstrap)

shapiro.test(flipper_gentoo)


# ============================================================
# 37. FLIPPER LENGTH LEVENE'S TEST
# ============================================================

flipper_levene <- leveneTest(
  flipper_length_mm ~ species,
  data = flipper_data
)

print(flipper_levene)


# ============================================================
# 38. FLIPPER LENGTH TUKEY HSD
# ============================================================

flipper_tukey <- TukeyHSD(
  flipper_anova
)

print(flipper_tukey)


# ============================================================
# 39. FLIPPER LENGTH KRUSKAL-WALLIS TEST
# ============================================================

flipper_kruskal <- kruskal.test(
  flipper_length_mm ~ species,
  data = flipper_data
)

print(flipper_kruskal)


# ============================================================
# 40. FLIPPER LENGTH MEAN COMPARISON
# ============================================================

flipper_means <- flipper_data %>%
  group_by(species) %>%
  summarise(
    Mean = mean(flipper_length_mm),
    SD = sd(flipper_length_mm),
    Count = n(),
    SE = SD / sqrt(Count)
  )

print(flipper_means)


# ============================================================
# 41. FLIPPER LENGTH MEAN + ERROR BAR PLOT
# ============================================================

ggplot(
  flipper_means,
  aes(
    x = species,
    y = Mean
  )
) +
  geom_col() +
  geom_errorbar(
    aes(
      ymin = Mean - SE,
      ymax = Mean + SE
    ),
    width = 0.2
  ) +
  labs(
    title = "Mean Flipper Length by Penguin Species",
    x = "Species",
    y = "Mean Flipper Length (mm)"
  ) +
  theme_minimal()


# ============================================================
# 42. BODY MASS MEAN COMPARISON
# ============================================================

body_mass_means <- body_mass_data %>%
  group_by(species) %>%
  summarise(
    Mean = mean(body_mass_g),
    SD = sd(body_mass_g),
    Count = n(),
    SE = SD / sqrt(Count)
  )

print(body_mass_means)


# ============================================================
# 43. BODY MASS MEAN + ERROR BAR PLOT
# ============================================================

ggplot(
  body_mass_means,
  aes(
    x = species,
    y = Mean
  )
) +
  geom_col() +
  geom_errorbar(
    aes(
      ymin = Mean - SE,
      ymax = Mean + SE
    ),
    width = 0.2
  ) +
  labs(
    title = "Mean Body Mass by Penguin Species",
    x = "Species",
    y = "Mean Body Mass (g)"
  ) +
  theme_minimal()


# ============================================================
# 44. FINAL COMPARISON - BODY MASS VS FLIPPER LENGTH
# ============================================================

# Standardized comparison of species-wise means

comparison_table <- body_mass_data %>%
  group_by(species) %>%
  summarise(
    Mean_Body_Mass = mean(body_mass_g)
  ) %>%
  left_join(
    flipper_data %>%
      group_by(species) %>%
      summarise(
        Mean_Flipper_Length = mean(flipper_length_mm)
      ),
    by = "species"
  )

print(comparison_table)


# ============================================================
# 45. FINAL STATISTICAL RESULTS
# ============================================================

cat("\n============================================\n")
cat("FINAL STATISTICAL RESULTS\n")
cat("============================================\n")


cat("\n--- Male vs Female t-test ---\n")
print(t_test_result)


cat("\n--- Cohen's d ---\n")
print(cohen_d_result)


cat("\n--- One-way ANOVA: Body Mass ---\n")
print(summary(one_way_model))


cat("\n--- Tukey HSD: Body Mass ---\n")
print(tukey_result)


cat("\n--- Kruskal-Wallis: Body Mass ---\n")
print(kruskal_result)


cat("\n--- Two-way ANOVA ---\n")
print(summary(two_way_model))


cat("\n--- Flipper Length ANOVA ---\n")
print(summary(flipper_anova))


cat("\n--- Flipper Length Tukey HSD ---\n")
print(flipper_tukey)


cat("\n--- Flipper Length Kruskal-Wallis ---\n")
print(flipper_kruskal)


# ============================================================
# 46. FINAL CONCLUSION
# ============================================================

cat("\n============================================\n")
cat("CONCLUSION\n")
cat("============================================\n")

cat("
The Palmer Penguins dataset was statistically analyzed
using descriptive statistics, hypothesis testing, ANOVA,
post-hoc analysis, and non-parametric testing.

Body mass was summarized using measures such as mean,
median, variance, standard deviation, quartiles, IQR,
skewness, and kurtosis.

The difference in body mass between male and female
penguins was examined using an independent two-sample
t-test. Normality was assessed using Shapiro-Wilk tests
and QQ-plots. A 95% confidence interval and Cohen's d
effect size were also calculated.

A one-way ANOVA was performed to determine whether mean
body mass differed among Adelie, Chinstrap, and Gentoo
penguins. Normality and homogeneity of variance were
examined before interpreting the ANOVA. Tukey's HSD was
used for pairwise comparisons when appropriate.

The Kruskal-Wallis test was also performed to provide a
non-parametric comparison of body mass among species.

A two-way ANOVA was conducted to examine the effects of
species and sex on body mass, including their interaction.

Finally, flipper length was analyzed across species using
descriptive statistics, ANOVA, Tukey HSD, and Kruskal-Wallis
testing.

The statistical results and visualizations provide evidence
about differences in physical characteristics among penguin
species and between sexes.
")

# ============================================================
# END OF LAB 6
# ============================================================