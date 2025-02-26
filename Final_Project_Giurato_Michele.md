---
title: "Final Project Notebook Giurato Michele"
output: html_document
editor_options: 
  markdown: 
    wrap: sentence
---

```{r}
neonati<-read.csv("neonati.csv",stringsAsFactors = T,sep=",")
summary(neonati)
```

The dataset contains the following variables:

-   Anni.madre (Numeric, continuous): Mother's age in years.
    This is a continuous variable with values ranging from 0 to 46 years (there are some anomalous data as 0 years and may be an error in the data that should be removed).

-   N.gravidanze (Numeric, discrete): Number of pregnancies carried by the mother.
    This is a discrete variable, as it only takes integer values (from 0 to 12).

-   Fumatrici (Binary, categorical): Indicates whether the mother is a smoker (0 = No, 1 = Yes).
    It is a binary categorical variable indicating a specific behavior.

-   Gestazione (Numeric, continuous): Number of weeks of gestation for the newborn.
    This is a continuous variable ranging from 25 to 43 weeks.

-   Peso (Numeric, continuous): Weight of the newborn in grams.
    This is the main response variable, and it ranges from 830 g to 4930 g.

-   Lunghezza (Numeric, continuous): Length of the newborn in millimeters.
    This is a continuous variable ranging from 310 mm to 565 mm.

-   Cranio (Numeric, continuous): Diameter of the newborn’s head in millimeters.
    This is a continuous variable ranging from 235 mm to 390 mm.

-   Tipo.parto (Categorical, nominal): Indicates the type of delivery (Nat = Natural, Ces = Cesarean).
    This is a nominal categorical variable that describes the mode of birth.

-   Ospedale (Categorical, nominal): Indicates the hospital where the birth occurred (osp1, osp2, osp3).
    This is a nominal categorical variable.

-   Sesso (Categorical, nominal): Indicates the sex of the newborn (M = Male, F = Female).
    This is a nominal categorical variable.

Let's visualize now this variables

```{r}
library(DescTools)
```

```{r}
library(ggplot2)

ggplot(neonati, aes(x = as.factor(Anni.madre))) + 
  geom_bar() +
  labs(title = "Distribution of years of mother", 
       x = "Year mother", 
       y = "Count") +
  theme_minimal()

```

The lowest values 0 and 1 years old are clearly wrong data and it is wise to remove them from the dataset.
Looking qualitatively at the histogram, this variable seems to have a gaussian-shape and the most frequent values equal to 27 and 30 years old

```{r}
ggplot(neonati, aes(x = as.factor(N.gravidanze))) + 
  geom_bar() +
  labs(title = "Distribution of Number of Pregnancies", 
       x = "Number of Pregnancies", 
       y = "Count") +
  theme_minimal()
```

From the histogram plot it is clear that the largest sub group is represented by women approaching to the first delivery.
Then, generally the number of women decreases as the number of pregnancies increases.

```{r}
ggplot(neonati, aes(x = as.factor(Gestazione))) + 
  geom_bar() +
  labs(title = "Distribution of Pregnancy period", 
       x = "Number of Pregnancie Period (in weeks)", 
       y = "Count") +
  theme_minimal()
```

We can see that 40 is the most frequent number of weeks for the last of the pregnancy and the largest part of women delivers close to this number of weeks

```{r}
ggplot(neonati, aes(x = Peso)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth = 90, fill = "lightblue", color = "black") +
  geom_density(alpha = 0.2, fill = "red") +
  labs(title = "Distribution of Newborn Weight", 
       x = "Newborn Weight (grams)", 
       y = "Count") +
  theme_minimal()
```

```{r}
ggplot(neonati, aes(x = Lunghezza)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth = 10, fill = "lightblue", color = "black") +
  geom_density(alpha = 0.2, fill = "red") +
  labs(title = "Distribution of Newborn Length", 
       x = "Newborn Length (in millimiters)", 
       y = "Count") +
  theme_minimal()
```

```{r}
ggplot(neonati, aes(x = Cranio)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth = 5, fill = "lightblue", color = "black") +
  geom_density(alpha = 0.2, fill = "red") +
  labs(title = "Distribution of Head Length", 
       x = "Head Length (in millimiters)", 
       y = "Count") +
  theme_minimal()
```

Peso, Lunghezza and Cranio show a gaussian-like distribution but with a longer left tail (left-skewed).
So for Peso in particular we need to take care about the hypotesis of normality.

```{r}
Desc(neonati$Tipo.parto, plotit=TRUE)
```

Tipo.parto shows that most women undergo a natural delivery.

```{r}
Desc(neonati$Ospedale, plotit=TRUE)
```

Ospedale tell us that the women pregnancies were managed more or less equally from the three hospitals, with the largest number in Osp1.

```{r}
Desc(neonati$Sesso, plotit=TRUE)
```

Sex variable shows that female births are slightly larger than male births.

Let's remove the wrong data corresponding to 0 and 1 years old.

```{r}
neonati <- subset(neonati, Anni.madre >= 12)

ggplot(neonati, aes(x = as.factor(Anni.madre))) + 
  geom_bar() +
  labs(title = "Distribution of years of mother", 
       x = "Year mother", 
       y = "Count") +
  theme_minimal()
```

To verify if the mean of Peso and Lunghezza for this sample is significantly equal to the mean of the correspondent population mean, t-test is applied.
Considering a Peso population mean of 3300 grams and a Length population mean of 500 millimiters:

```{r}
peso_population_mean <- 3300
t_test_peso <- t.test(neonati$Peso, mu = peso_population_mean,
       conf.level = 0.95, 
       alternative = "two")
print(t_test_peso)
```

The p-value (0.1324) is greater than the typical significance level of 0.05. Therefore, we fail to reject the null hypothesis. This means there is insufficient evidence to conclude that the mean weight of the neonates is different from the population mean of 3300 grams.
The 95% confidence interval for the mean weight of the sample is (3263.577, 3304.791). Since this interval includes the population mean of 3300, it suggests that the true mean could plausibly be 3300 grams.

```{r}
lunghezza_population_mean <- 500
t_test_lunghezza <- t.test(neonati$Lunghezza, mu = lunghezza_population_mean,
       conf.level = 0.95, 
       alternative = "two")
print(t_test_lunghezza)

```

Since the p-value is far less than 0.05, we reject the null hypothesis. This means there is strong evidence that the true mean length of the neonates is different from the population mean of 500.
The 95% confidence interval for the sample mean length is (493.6628, 495.7287), which does not include 500.
This further supports the conclusion that the true mean length is statistically different from 500.

Let's verify now if these and other variables show significative differences by sex.

```{r}
t_test_sesso_peso <- t.test(Peso ~ Sesso, data = neonati)
print(t_test_sesso_peso)
```

Since the p-value is much smaller than 0.05, we reject the null hypothesis.
This means there is strong evidence that the mean weight of neonates differs significantly between females (F) and males (M).
The mean weight in group F (female) is 3161.061 grams, while the mean weight in group M (male) is 3408.496 grams.
This difference is substantial, with males weighing more on average.

The visualization of the box plot makes more easy to qualitatively detect this significant difference:

```{r}
Desc(Peso ~ Sesso, neonati, digits=1, plotit=TRUE)
```

```{r}
t_test_sesso_lunghezza <- t.test(Lunghezza ~ Sesso, data = neonati)
print(t_test_sesso_lunghezza)
```

Since the p-value is much smaller than 0.05, we reject the null hypothesis.
So also in this case there is strong evidence that there is a significant difference in mean length between female and male neonates.
The mean length in group F (female) is 489.7641 mm, while the mean length in group M (male) is 499.6750 mm.
This suggests that male neonates are, on average, longer than female neonates.

This is confirmed by the boxplot:

```{r}
Desc(Lunghezza ~ Sesso, neonati, digits=1, plotit=TRUE)
```

```{r}
t_test_fumo_peso <- t.test(Peso ~ Fumatrici, data = neonati)
print(t_test_fumo_peso)
```

There is no statistically significant difference in the weights of neonates based on maternal smoking status.
The mean weight of neonates born to non-smokers is slightly higher than that of neonates born to smokers, but the evidence does not support a strong conclusion about this difference

```{r}
Desc(Peso ~ Fumatrici, neonati, digits=1, plotit=TRUE)
```

```{r}
(z <- Desc(Peso ~ Fumatrici, neonati, test=t.test, digitd=1, plotit=FALSE))
plot(z, type="dens")
```

To check if in some hospitals caesarean births occur significantly more often than natural births let's perform a Chi-squared test of independence: it allows to assess whether there is a significant association between the two categorical variables: type of delivery (Tipo.parto) and the hospital (Ospedale) in which the delivery occurred

```{r}
contingenza <- table(neonati$Tipo.parto, neonati$Ospedale)
chi_test <- chisq.test(contingenza)
print(chi_test)
```

X-squared of 1.083 is the value of the Chi-squared statistic.
It measures the discrepancy between the observed frequencies (actual counts in the contingency table) and the expected frequencies (counts we would expect if there were no association between the variables).
Since the p-value (0.5819) is much greater than the common significance level of 0.05, we fail to reject the null hypothesis.
This suggests that there is no statistically significant association between the type of delivery and the hospital where the delivery occurred.

```{r}
Desc(Tipo.parto ~ Ospedale, neonati, digits=1,plotit=TRUE)
```

Moreover also newborn weight seems not depending on the kind of hospital:

```{r}
boxplot(neonati$Peso ~ neonati$Ospedale, 
                    xlab = "Ospedale", 
                    ylab = "Peso (g)", 
                    main = "Peso del Neonato per Ospedale",
                    col = terrain.colors(3))

mylevels <- levels(neonati$Ospedale)
levelProportions <- summary(neonati$Ospedale) / nrow(neonati[, c('Ospedale','Peso')])

for (i in 1:length(mylevels)) {

  thisvalues <- neonati[neonati$Ospedale == mylevels[i], "Peso"]
  myjitter <- jitter(rep(i, length(thisvalues)), amount = levelProportions[i] / 2)
  points(myjitter, thisvalues, pch = 20, cex= .5, col = rgb(0, 0, 0, .9)) 
}

```

```{r}
t_test_tipo_parto_peso <- t.test(Peso ~ Tipo.parto, data = neonati)
print(t_test_tipo_parto_peso)
```

Also in this case, there is no statistically significant difference in the weights of neonates based on the typo of birth.

```{r}
boxplot(neonati$Peso ~ neonati$Tipo.parto, 
                    xlab = "Tipo di Parto", 
                    ylab = "Peso (g)", 
                    main = "Peso del Neonato per Tipo di Parto",
                    col = terrain.colors(3))

mylevels <- levels(neonati$Tipo.parto)
levelProportions <- summary(neonati$Tipo.parto) / nrow(neonati[, c('Tipo.parto','Peso')])

for (i in 1:length(mylevels)) {

  thisvalues <- neonati[neonati$Tipo.parto == mylevels[i], "Peso"]
  myjitter <- jitter(rep(i, length(thisvalues)), amount = levelProportions[i] / 2)
  points(myjitter, thisvalues, pch = 20, cex= .5, col = rgb(0, 0, 0, .9)) 
}

```
Let's now look at the correlations inside the dataset.

```{r}
library(DescTools)
par(mfrow=c(1,1))
m <- cor(neonati[,which(sapply(neonati, is.numeric))], use="pairwise.complete.obs")
col_palette <- colorRampPalette(c("red", "white", "blue"))(100)
PlotCorr(m, col=col_palette, border="grey",
 args.colorlegend=list(labels=Format(seq(-1,1,.25), 2), frame="grey"))
```

```{r}
PlotWeb(m, col=c(hred, hblue))
```

Lunghezza and Cranio are highly correlated with Peso and will be essential in the predictive model.
They provide significant information on the baby's overall growth, making them strong predictors of weight.
Gestazione is another key predictor: longer gestation times lead to heavier babies, so including gestation as a variable will improve the model's accuracy.
Anni.madre, N.gravidanze, and Fumatrici show weak correlations with Peso, implying that they may not add much predictive power.

```{r}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor *1.3)
}

pairs(neonati[, c("Peso", "Cranio", "Gestazione", "Lunghezza")],lower.panel=panel.cor, upper.panel=panel.smooth, pch = 19, cex.labels = 1.3)
```

```{r}
shapiro.test(neonati$Peso)
moments::skewness(neonati$Peso)
moments::kurtosis(neonati$Peso)-3
```

The Shapiro-Wilk normality test strongly suggests that the Peso variable does not follow a normal distribution.
The very low p-value indicates a statistically significant deviation from normality. Such deviations from normality of the response variable most of the time also affect the residuals after calculating the model.
Hence, the most critical thing to check next is whether the residuals of our model are normally distributed.

A skewness of -0.647 is moderately negative but not extreme, suggesting some asymmetry in the distribution.
Excess kurtosis of -0.9712 indicates the distribution has light tails and is slightly platykurtic.
In other words, the distribution is flatter or less peaked than a normal distribution, with fewer outliers in the tails.

Therefore Shapiro-Wilk test, combined with the skewness and kurtosis values, confirms that the Peso variable is not normally distributed.
This needs to be taken into consideration for the further multidimensional analysis and searching of the 'best' predicting model for newborn weight.

```{r}
attach(neonati)
mod<-lm(Peso~Gestazione+Lunghezza+Cranio+Fumatrici+N.gravidanze+Anni.madre+Tipo.parto+Ospedale+Sesso)
summary(mod)
```

Significant Predictors:

- Gestazione: Highly significant (p-value less than 2e-16) with a positive effect on birth weight. Each additional week of gestation increases the birth weight by about 32.58 grams.
- Lunghezza: Also highly significant (p-value less than 2e-16) with a strong positive relationship.
- Cranio: Another very significant predictor (p-value less than 2e-16).
- N.gravidanze: Slightly significant (p-value = 0.0148), with a positive effect. For each additional pregnancy, birth weight increases by 11.38 grams. This suggests that having more pregnancies may lead to higher birth weight.
- Tipo.partoNat: Slightly significant (p-value = 0.0143), with a positive impact of 29.63 grams on birth weight for natural births compared to C-section.
- Ospedaleosp3 (Hospital 3): Slightly significant (p-value = 0.0366) with a 28.25 gram increase in birth weight for births occurring in Hospital 3 compared to the reference hospital (Hospital 1).
- SessoM: Very significant (p-value = 5.18e-12). Male infants are predicted to weigh 77.57 grams more than female infants.

Non-Significant Predictors:

- Fumatrici: Not significant (p-value = 0.2719). The impact of the mother smoking on birth weight appears to be negative (-30.27 grams), but the lack of significance suggests that it does not have a strong or consistent effect in this dataset.
- Anni.madre: Not significant (p-value = 0.4845), with a small estimated effect (0.80 grams increase per year). This suggests that mother’s age does not strongly influence birth weight in this model.
- Ospedaleosp2: Not significant (p-value = 0.4096). There’s no meaningful difference in birth weight between Hospital 2 and the reference hospital.

Multiple R-squared (0.7289) and Adjusted R-squared (0.7278) indicate that about 72.89% of the variability in birth weight is explained by the model.
This is quite a strong result, meaning the predictors collectively do a good job of explaining the variance in birth weight.
The very low p-value indicates that the model as a whole is statistically significant.
The significant predictors, such as gestational age, length, head circumference, number of pregnancies, type of birth, hospital, and sex, may be initially kept in the model as they have clear and statistically significant relationships with birth weight.
The non-significant predictors (mother’s age, smoking status, and some hospital variables) could potentially be removed to simplify the model.
However we can also consider checking for interactions or nonlinear relationships.

```{r}
mod2 <- update(mod,~.-Fumatrici-Anni.madre-Ospedale)
summary(mod2)
```

By removing Fumatrici, Anni.madre, and Ospedale, we’ve simplified the model without substantially sacrificing predictive power or fit.
The R-squared has only dropped slightly, and the remaining predictors continue to have similar significance and effect sizes.
Since Fumatrici and Anni.madre were non-significant in the original model, and only one of the hospital variables (Ospedaleosp3) was marginally significant, their removal makes the model more parsimonious without much loss in explanatory power.

```{r}
mod3<-update(mod2,~.-N.gravidanze-Tipo.parto)
summary(mod3)
```

Impact of Removing N.gravidanze and Tipo.parto: While both variables were significant in mod2, their removal has had minimal effect on the performance metrics of the model.
The coefficients for the remaining predictors have remained relatively stable, and the R-squared has only dropped slightly.
This suggests that the bulk of the predictive power is captured by the four remaining variables (gestation, length, head circumference, and sex).
By removing these two variables, we have simplified the model further, reducing the number of predictors, while sacrificing only a small amount of predictive accuracy.
The slightly increased residual standard error suggests a small increase in prediction error, but the simplicity gained by reducing the model complexity may outweigh this trade-off.

```{r}
mod4<-update(mod2,~.-Tipo.parto)
summary(mod4)
```

Keeping also N.gravidanze slightly increases residual standard error suggesting a small increase in prediction error.
So the choice between mod3 and mod4 is just about how much we want to prefer model simplicity/complexity given an almost identical overall performance.

From the above scatterplot it can appear a non linear relationship between Peso and Lunghezza.
Let's try to add this non linear contribute using a quadratic term:

```{r}
mod5<-update(mod3,~.+I(Lunghezza^2))
summary(mod5)
```

The increment in accuracy from adding the quadratic term is statistically significant but the change in predictive accuracy (a 1% increase in R-squared and a 4.8-gram reduction in residual error) is relatively modest.
If we prioritize simplicity and ease of interpretation, then sticking with the simpler model (mod3) without the quadratic term is a reasonable choice.
The simpler model still explains around 72.6% of the variance in birth weight and provides interpretable coefficients with minimal complexity.

Let's try to add for example the effect of interaction between Lunghezza and Cranio

```{r}
mod6<-lm(Peso~Lunghezza*Cranio)
summary(mod6)
```

It seems that the interaction term between Lunghezza and Cranio does not provide a substantial improvement to the model's performance, and may even slightly harm it.
Therefore, mod3 (the simpler model) is likely preferable in this case.
Verifying also other possible interactions is seems not beneficial: for this case interactions just complicate the model without offering significant gains in predictive performance.

```{r}
anova(mod3,mod6)
```

Anova confirms that the interaction is statistically significant, but it does not improve the model's fit sufficiently to justify its inclusion.
mod3, the simpler model without the interaction term, is preferred based on the overall model performance and its ability to explain the variability in Peso

```{r}
n <- nrow(neonati)
stepwise.mod <- MASS::stepAIC(mod,direction="both",k=log(n))
summary(stepwise.mod)
```

MASS stepAIC procedure chooses what we called mod4.

MASS library offers also the possibility to use the Box-Cox transformation, a method that can help to identify a possible optimal transformation of the response variable (in this case, Peso) that minimizes residual sum of squares (RSS) and improves model fit.

```{r}
library(MASS)
boxcox(mod3) 
```

The plot shows a quite broad peak corresponding to lambda spanning from 0 to 1.
So we can try a log-transformation of Peso (corresponding to lambda = 0) or a square root transformation of Peso (corresponding to lambda = 0.5).

```{r}
mod7 <- lm(log(Peso) ~ Gestazione + Lunghezza + Cranio + Sesso)
summary(mod7)
```

Compared to mod3, applying log transformation on Peso increases by almost 5% the capacity of the model to explain the variability in birth weight (from 72.89% to 77.62%)

```{r}
mod8 <- lm(sqrt(Peso) ~ Gestazione + Lunghezza + Cranio + Sesso)
summary(mod8)
```

Compared to mod3, applying square root transformation on Peso increases by about 3% the capacity of the model to explain the variability in birth weight (from 72.89% to 75.61%)

```{r}
AIC(mod,mod2,mod3,mod4,mod5,mod7,mod8)
BIC(mod,mod2,mod3,mod4,mod5,mod7,mod8)
```

mod (with the full set of predictors) has the highest AIC and BIC, indicating overfitting, and suggesting that some predictors may not contribute significantly to improving model performance.
mod3 and mod4 (simpler models) both perform similarly in terms of AIC and BIC, but they perform slightly worse than mod5.
mod5, the one in which the quadratic term for Lunghezza is added, is the best performing model among those without log transformations, but its AIC and BIC are much higher than mod7 and mod8, implying it fits the data less well.
Again, the choice between mod3/mod4 and mod5 is about preference between model simplicity/slightly worse model performance and model complexity/slightly better model performance.
mod7 (log transformations of variables) has the lowest AIC and BIC values, both substantially negative, which indicates it offers the best trade-off between model fit and complexity.

```{r}
library(car)
car::vif(mod7)
```

The Variance Inflation Factor (VIF) values provided from mod7 (but also for mod3 for example) suggest that multicollinearity is not a major concern for the predictors because all the VIF values are below 5.9
So we can proceed with interpreting the results from the regression model without needing to address collinearity issues.

```{r}
par(mfrow=c(2,4))
plot(mod3)
mtext("mod 3", side = 3, line = -2, outer = TRUE, cex = 1.5, adj = 0.5)
plot(mod7)
mtext("mod 7", side = 3, line = -19, outer = TRUE, cex = 1.5, adj = 0.5)
```
Both mod3 and mod7 shows some issues with non-linearity, heteroscedasticity and normality of residuals.
This is evident from the curved pattern in the residual plots and the deviation from normality in the Q-Q plot.
Some refinements may be needed to fully meet the assumptions of linear regression and maybe make me choose the final model.

```{r}
par(mfrow=c(1,2))
cook3<-cooks.distance(mod3)
plot(cook3,ylim = c(0,2))
max(cook3)
cook7<-cooks.distance(mod7)
plot(cook7,ylim = c(0,2))
max(cook7)
```

Both models (mod3 and mod7) have a single influential point with a Cook's distance around 1, suggesting that this point may have a significant impact on the regression results.
We should examine this observation (1549) to understand its influence and consider whether it affects the overall model performance.

```{r}
neonati[1549, ]
```

Comparing this observation with the Peso and Lunghezza distribution, we can notice that for this newborn we have a Lunghezza value of 315mm (one of the lowest values recorded in this dataset, left tail of the distribution) corresponding to a relatively large Peso value of 4370 grams (right tail of Peso distribution).
This may explain the 'bad' influence of this point on the regression results.
Let's try to remove this observation and check if this action provides an improvement.

```{r}
neonati_try <- neonati[-c(1549), ]
mod_try<-lm(Peso~Gestazione+Lunghezza+Cranio+Sesso, data=neonati_try)
summary(mod_try)
mod_log_try<-lm(log(Peso)~Gestazione+Lunghezza+Cranio+Sesso, data=neonati_try)
summary(mod_log_try)
```

Comparing to mod3 and mod7 Adjusted R-squared, the removal of observation 1549 increases the overall model performance of both models.

```{r}
par(mfrow=c(2,4))
plot(mod_try)
plot(mod_log_try)
```

```{r}
par(mfrow=c(1,2))
cook_try<-cooks.distance(mod_try)
plot(cook_try,ylim = c(0,1))
max(cook_try)
cook_log_try<-cooks.distance(mod_log_try)
plot(cook_log_try,ylim = c(0,1))
max(cook_log_try)
```

We still see slight curved patterns in the residual plots and the deviation from normality in the Q-Q plot, but no influential points looking at the Cook's distance

Let's now carry out the tests on the residuals and check possible improvements by the removal of that observation

```{r}
library(lmtest)
bptest(mod3)
dwtest(mod3)
bptest(mod7)
dwtest(mod7)
```

```{r}
bptest(mod_try)
dwtest(mod_try)
bptest(mod_log_try)
dwtest(mod_log_try)
```
None of the models show strong evidence of autocorrelation, as the Durbin-Watson statistics are close to 2 and the p-values are not significant. Therefore, there seems to be no serious problem with autocorrelation in the residuals of these models
Insted, while mod3, mod7, and mod_log_try show strong evidence of heteroscedasticity, mod_try is the only one that shows a mild indication of it (p-value = 0.05338). So mod_try is the model less close to a variance variability of the residuals (which violates one of the assumptions of linear regression).


```{r}
shapiro.test(mod3$residuals)
plot(density(residuals(mod3)))
shapiro.test(mod3$residuals)
plot(density(residuals(mod7)))
```

```{r}
par(mfrow=c(1,2))
shapiro.test(mod_try$residuals)
plot(density(residuals(mod_try)))
shapiro.test(mod_log_try$residuals)
plot(density(residuals(mod_log_try)))
```
For all models, the residuals are not normally distributed based on the Shapiro-Wilk test. The p-values are all well below 0.05, which suggests that the assumption of normality is violated in each case.
mod_log_try and mod_try show residuals that are closer to normal (even though both show significant departures from normality), with respect to mod3 and mod7 that have the most pronounced departures from normality, with a very low W statistic and a p-value extremely close to zero.

The further removal of other two possible observations that from the QQ plots appear 'far' from the line corresponding normality (1306 and 155) does not change the overall model perfomances of mod_try and mod_log_try, even though improves the Shapiro-Wilk test (not enough to pass the residuals normality assumption)

```{r}
neonati_try_2 <- neonati_try[-c(1306,155), ]
mod_try_2<-lm(Peso~Gestazione+Lunghezza+Cranio+Sesso, data=neonati_try_2)
summary(mod_try_2)
mod_log_try_2<-lm(log(Peso)~Gestazione+Lunghezza+Cranio+Sesso, data=neonati_try_2)
summary(mod_log_try_2)
```

```{r}
par(mfrow=c(2,4))
plot(mod_try_2)
plot(mod_log_try_2)
```

```{r}
bptest(mod_try_2)
dwtest(mod_try_2)
bptest(mod_log_try_2)
dwtest(mod_log_try_2)
```
```{r}
par(mfrow=c(1,2))
shapiro.test(mod_try_2$residuals)
plot(density(residuals(mod_try_2)))
shapiro.test(mod_log_try_2$residuals)
plot(density(residuals(mod_log_try_2)))
```
After all these checks and the general statistical analysis, I would choose the model "mod_try_2" has the best predictive model for the neonati dataset. This model has the 73.85% of capability to explain the variability in birth weight and it was crucial for my choice the removal of the 1594th observation that makes this model meet (although weakly) homoscedasticity. The further removal of 1306th and 155th observations was just a refinement to better improve (even tough not enough) the residuals normality assumption.

Let's now use this predictive model to provide a value for the birth weight of a newborn, given N.Gravidanze equal to 3 (but this value will not be used in our model since that variable was discarded as not impactful), Gestazione equal to 39 and female Sesso. No measures are provided by the ecography, so I will use Lunghezza and Cranio mean values from neonati_try_2 dataset.

```{r}
mean_lunghezza <- mean(neonati_try_2$Lunghezza, na.rm = TRUE)
mean_cranio <- mean(neonati_try_2$Cranio, na.rm = TRUE)

nuova_neonata <- data.frame(Gestazione = 39, Lunghezza = mean_lunghezza, Cranio = mean_cranio, Sesso = "F")
predizione_peso <- predict(mod_try_2, newdata = nuova_neonata)

predizione_peso
```
The newborn weight is predicted to be 3245.524 grams.

Let's now try to create some spatial visualizations of our predictive model.
```{r}
library(plotly)

neonati_try_2$predicted_peso <- predict(mod_try_2, neonati_try_2)

plot_ly(neonati_try_2, x = ~Lunghezza, y = ~Cranio, z = ~predicted_peso, 
        type = "scatter3d", mode = "markers", color = ~predicted_peso) %>%
  layout(scene = list(xaxis = list(title = 'Lunghezza'),
                      yaxis = list(title = 'Cranio'),
                      zaxis = list(title = 'Peso Predetto')))
```

```{r}
plot_ly(neonati_try_2, x = ~Lunghezza, y = ~Gestazione, z = ~predicted_peso, 
        type = "scatter3d", mode = "markers", color = ~predicted_peso) %>%
  layout(scene = list(xaxis = list(title = 'Lunghezza'),
                      yaxis = list(title = 'Gestazione'),
                      zaxis = list(title = 'Peso Predetto')))
```

```{r}
plot_ly(neonati_try_2, x = ~Cranio, y = ~Gestazione, z = ~predicted_peso, 
        type = "scatter3d", mode = "markers", color = ~predicted_peso) %>%
  layout(scene = list(xaxis = list(title = 'Cranio'),
                      yaxis = list(title = 'Gestazione'),
                      zaxis = list(title = 'Peso Predetto')))
```

```{r}

plot_ly(neonati_try_2, x = ~Lunghezza, y = ~Cranio, z = ~predicted_peso, 
        type = "scatter3d", mode = "markers", 
        color = ~factor(Sesso),
        colors = c('pink', 'blue'), 
        marker = list(symbol = ~ifelse(Sesso == 0, 'circle', 'diamond'))) %>% 
  layout(scene = list(xaxis = list(title = 'Lunghezza'),
                      yaxis = list(title = 'Cranio'),
                      zaxis = list(title = 'Peso Predetto')))

```
