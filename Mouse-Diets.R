library(tidyverse)  # ggplot(), etc.
library(lmPerm)     # permutation tests: aovp(), lmp(), etc.
library(Sleuth3)

# Data

data("case0501")
names(case0501)


# Histogram of the life expectantcy based on diet
ggplot(data = case0501, aes(x = Diet, y = Lifetime, fill = Diet)) +
    geom_boxplot(outlier.color = "red", outlier.size = 2) +
    labs(title = "Life Expectancy of Mice Based on Diet") +
    theme_classic()


# Standard One-Way ANOVA
summary(
    ANOVA.model <- aov(Lifetime ~ Diet, data = case0501)
)


# Compare means
TukeyHSD(ANOVA.model)


# Checks model for normaly-distributed residuals
GraphNormality <- function(model) {
    residual.data <- data.frame(e = model$residuals)
    H <- shapiro.test(residual.data$e)
    ggplot(residual.data, aes(sample = e)) + 
        stat_qq() +
        geom_abline(color = "blue", intercept = mean(residual.data$e), slope = sd(residual.data$e)) +
        labs(title = "Normally Distributed Residuals?", 
             subtitle = paste("Shapiro-Wilks test p-value =", signif(H$p.value,5)) ) +
        theme_classic()
}


# Checks model for homogeneity of variances
GraphHomogeneity <- function(response, predictor, dataset = NULL) {
    H <- bartlett.test(response~predictor)    
    ggplot(data = dataset, aes(x = predictor, y = response, fill = predictor)) +
        geom_boxplot(outlier.color = "red", outlier.size = 3) +
        geom_jitter(width = 0.2) +
        labs(title = "Homogenous Variances?",
              subtitle = paste("Bartlett's test p-value =", signif(H$p.value,5)) ) +    
        theme_classic()
}


# Plots model checks
GraphNormality(ANOVA.model)
GraphHomogeneity(case0501$Lifetime, case0501$Diet, dataset = case0501)


# Both assumptions are violated, so use two non-parametric tests to verify


# Permutation test
summary(
    perm.model <- aovp(Lifetime ~ Diet, data = case0501)
)
class(perm.model) 


# Compare means
plot(TukeyHSD(perm.model))


# Rank-Sum test
(kw.test <- kruskal.test(Lifetime ~ Diet, data = case0501))
class(kw.test)


# Nonparametric test


# Compare all 15 to find effective vs ineffective diets
attach(case0501)
wilcox.test(Lifetime[Diet == "N/R40"], Lifetime[Diet == "N/N85"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "N/R50"], Lifetime[Diet == "N/N85"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "NP"], Lifetime[Diet == "N/N85"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "R/R50"], Lifetime[Diet == "N/N85"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "lopro"], Lifetime[Diet == "N/N85"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "N/R50"], Lifetime[Diet == "N/R40"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "NP"], Lifetime[Diet == "N/R40"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "R/R50"], Lifetime[Diet == "N/R40"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "lopro"], Lifetime[Diet == "N/R40"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "NP"], Lifetime[Diet == "N/R50"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "R/R50"], Lifetime[Diet == "N/R50"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "lopro"], Lifetime[Diet == "N/R50"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "R/R50"], Lifetime[Diet == "NP"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "lopro"], Lifetime[Diet == "NP"], alternative="two.sided", conf.level=0.9967)
wilcox.test(Lifetime[Diet == "lopro"], Lifetime[Diet == "R/R50"], alternative="two.sided", conf.level=0.9967)
detach(case0501)