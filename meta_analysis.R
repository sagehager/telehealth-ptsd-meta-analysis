# Load the library
library(meta)

# create dataset
# Ne = Number Experimental (Telehealth)
# Me = Mean Experimental
# Se = Standard Deviation Experimental
# Nc = Number Control (In-Person)
# Mc = Mean Control
# Sc = Standard Deviation Control

study_data <- data.frame(
  Study = c("Acierno 2016", "Acierno 2017", "Liu 2020", "Morland 2014",
            "Morland 2015", "Acierno 2021", "Morland 2022", "Peterson 2022"),
  Year = c(2016, 2017, 2020, 2014, 2015, 2021, 2022, 2022),

  # Telehealth Group Data (Post-Treatment) *baseline SD used where Post SD missing
  Ne = c(63, 63, 80, 48, 43, 46, 25, 25),
  Me = c(47.5, 51.0, 51.3, 55.6, 50.5, 34.02, 27.1, 23.1),
  Se = c(12.7, 11.3, 16.3, 18.8, 15.3, 22.14, 9.35, 14.5),

  # In-Person Group Data (Post-Treatment) *baseline SD used where Post SD missing
  Nc = c(73, 69, 74, 52, 49, 49, 27, 26),
  Mc = c(48.5, 45.0, 49.3, 58.7, 53.6, 28.80, 24.1, 30.9),
  Sc = c(14.0, 11.1, 16.7, 21.0, 16.8, 22.01, 9.95, 12.8)
)

# print data
print(study_data)

# run meta-analysis
meta_analysis <- metacont(
  n.e = Ne, mean.e = Me, sd.e = Se, # Experimental (Telehealth)
  n.c = Nc, mean.c = Mc, sd.c = Sc, # Control (In-Person)
  studlab = Study,
  data = study_data,
  sm = "SMD",
  method.smd = "Hedges", # Hedges' g correction for small sample sizes
  common = TRUE,          # Run Fixed (Common) Effects
  random = TRUE          # Run Random Effects
)

# Print Results
summary(meta_analysis)

# Generate Forest Plot
forest(meta_analysis,
       sortvar = Year,             # Sort studies by Year
       prediction = TRUE,          # Add prediction interval
       print.tau2 = TRUE,          # Show heterogeneity variance
       leftcols = c("studlab", "n.e", "n.c"),
       leftlabs = c("Study", "N (Tele)", "N (IP)"),
       xlab = "Favors Telehealth <---   ---> Favors In-Person",
       main = "Telehealth vs In-Person PTSD Treatment Efficacy")

# Check Heterogeneity Statistics
# Q-statistic: Is there significant heterogeneity?
# I-squared: What % of variance is due to true heterogeneity rather than chance?

print(paste("I-squared:", round(meta_analysis$I2 * 100, 2), "%"))
print(paste("Tau-squared:", round(meta_analysis$tau2, 4)))
print(paste("P-value for Heterogeneity:", meta_analysis$pval.Q))

# Meta-Regression
meta_reg <- metareg(meta_analysis, ~ Year)

# Print the regression output
print(meta_reg)

# Visualize the Regression (Bubble Plot)
# This plots the Effect Size on Y axis and Year on X axis
bubble(meta_reg,
       xlab = "Publication Year",
       ylab = "Standardized Mean Difference (SMD)",
       main = "Meta-Regression: Effect Size by Year")

# Funnel plot to check for publication bias
funnel(meta_analysis,
       main = "Funnel Plot of Standardized Mean Differences")

# Egger's test (exploratory, since there are fewer than 10 studies)
metabias(meta_analysis, k.min = 8)

png("forest_plot.png", width = 12, height = 6, units = "in", res = 300)
forest(meta_analysis,
       sortvar = Year,             # Sort studies by Year
       prediction = TRUE,          # Add prediction interval
       print.tau2 = TRUE,          # Show heterogeneity variance
       leftcols = c("studlab", "n.e", "n.c"),
       leftlabs = c("Study", "N (Tele)", "N (IP)"),
       xlab = "Favors Telehealth <---   ---> Favors In-Person",
       main = "Telehealth vs In-Person PTSD Treatment Efficacy")
dev.off()
