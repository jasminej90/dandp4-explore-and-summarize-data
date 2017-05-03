Red Wine Quality by Yasmin Aljedawi
========================================================

```{r echo=FALSE, message=FALSE, warning=FALSE, packages}
# Load all of the packages that you end up using in your analysis in this code
# chunk.

# Notice that the parameter "echo" was set to FALSE for this code chunk. This
# prevents the code from displaying in the knitted HTML output. You should set
# echo=FALSE for all code chunks in your file, unless it makes sense for your
# report to show the code that generated a particular plot.

# The other parameters for "message" and "warning" should also be set to FALSE
# for other code chunks once you have verified that each plot comes out as you
# want it to. This will clean up the flow of your report.

library(ggplot2)
library(gridExtra)
library(PerformanceAnalytics)
```

```{r echo=FALSE, Load_the_Data}
# Load the Data
rw <- read.csv("wineQualityReds.csv")

```

This report explores a dataset describing the quality of 1,599 red wines based on their chemical properties.

# Univariate Plots Section

```{r echo=FALSE, Univariate_dim}
dim(rw)
```
```{r echo=FALSE, Univariate_struc}
str(rw)
```
```{r echo=FALSE, Univariate_summary}
summary(rw)
```


The red wine dataset contains 1,599 observations with 13 variables.


```{r echo=FALSE, Univariate_plot1}

ggplot(aes(x = quality), data = rw) + 
  geom_histogram(color = "black", fill = "#993366", binwidth = 1) +
  ggtitle("Distribution of Wine Quality") +
  scale_x_continuous(limits = c(1,10))

summary(rw$quality)

```

It seems that overall the wine dataset is normally distributed with an average of approximately 6, this is an indication that it's a collection of fairly good-quality wines, where 0 (very bad) and 10 (very excellent).


```{r echo=FALSE, Univariate_plot_all}

p1 <- ggplot(aes(x = fixed.acidity), data = rw) + 
  geom_histogram(color = "black", fill = "#993366", binwidth = 0.5)

p2 <- ggplot(aes(x = volatile.acidity), data = rw) +
  geom_histogram(color = "black", fill = "#993366", binwidth = 0.05)

p3 <- ggplot(aes(x = citric.acid), data = rw) +
   geom_histogram(color ="black", fill = "#993366", binwidth = 0.03)

p4 <- ggplot(aes(x = residual.sugar), data = rw) +
  geom_histogram( color = "black", fill = "#993366", binwidth = 0.5)

p5 <- ggplot(aes(x = chlorides), data = rw) +
  geom_histogram(color = "black", fill = "#993366", binwidth = 0.025)

p6 <- ggplot(aes(x = free.sulfur.dioxide), data = rw) +
   geom_histogram(color = "black", fill = "#993366", binwidth = 5)

p7 <- ggplot(aes(x = total.sulfur.dioxide), data = rw) +
   geom_histogram(color = "black", fill = "#993366", binwidth = 10)

p8 <- ggplot(aes(x = density), data = rw) +
   geom_histogram(color = "black", fill = "#993366", binwidth = 0.001)

p9 <- ggplot(aes(x = pH), data = rw) +
   geom_histogram(color = "black", fill = "#993366", binwidth = 0.05)

p10 <- ggplot(aes(x = sulphates), data = rw) +
   geom_histogram(color = "black", fill = "#993366", binwidth = 0.1)

p11 <- ggplot(aes(x = alcohol), data = rw) +
   geom_histogram(color = "black", fill = "#993366", binwidth = 0.25)

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, ncol = 3)

```

I plotted all chemical variables that might potentially have an impact on wine quality, I also wonder if they impact one another. At a glance, we can tell that data transformation can be applied on several variables as their histograms are positively skewed (residual sugar, chlorides, free and total sulfur dioxide, and sulphates)


```{r echo=FALSE, Univariate_plot_facid}

ggplot(data = rw, aes(x = fixed.acidity)) +
  geom_histogram(binwidth = 0.5, color = 'black', fill = "#993366")

summary(rw$fixed.acidity)

```

Most red wines are of a fixed acidity (tartaric acid) between [6 - 10] g/dm^3: mean 8.32 g/dm^3 and median 7.90 g/dm^3.


```{r echo=FALSE, Univariate_plot_vacid}

ggplot(data = rw, aes(x = volatile.acidity)) +
  geom_histogram(binwidth = 0.05, color = 'black', fill = "#993366")

summary(rw$volatile.acidity)

```

Most red wines are of a volatile acidity (acetic acid) between [0.3 - 0.7] g/dm^3: both mean and median are about 0.5 g/dm^3.


```{r echo=FALSE, Univariate_plot_cacid}

ggplot(data = rw, aes(x = citric.acid)) +
  geom_histogram(binwidth = 0.03, color = 'black', fill = "#993366")

summary(rw$citric.acid)

```

Most red wines are of a citric acid, which adds 'freshness' and flavor to wines, between [0.1 - 0.5] g/dm^3: mean is about 0.27 g/dm^3 and median is about 0.26 g/dm^3, which is reasonable as citric acid is usually found in small quantities.


```{r echo=FALSE, Univariate_plot_rsugar}
p1 <- ggplot(aes(x = residual.sugar), data = rw) +
  geom_histogram(binwidth = 0.25, color = 'black', fill = "#993366")

p2 <- p1 + scale_x_log10()

p3 <- p1 + scale_x_sqrt()

grid.arrange(p1, p2, p3, ncol = 1)

summary(rw$residual.sugar)

```

The histogram of residual sugar is positively skewed, so I applied log and sqrt transformation on the data to better understand the distribution of residual sugar. The log transformed residual sugar distribution appears normal with a peak at 2. Last histogram represents sqrt transformation of the data, which is still long tailed. Most red wines are of a residual sugar between [1 - 3] g/dm^3, mean is about 2.5 g/dm^3 and median is about 2.2 g/dm^3. It's rare to find wines with less than 1 gram/liter and wines with greater than 45 grams/liter are considered sweet


```{r echo=FALSE, Univariate_plot_chloride}
p1 <- ggplot(aes(x = chlorides), data = rw) +
  geom_histogram(binwidth = 0.025, color = 'black', fill = "#993366")

p2 <- p1 + scale_x_log10()

grid.arrange(p1, p2, ncol = 1)

summary(rw$chlorides)

```

The histogram of chloride is positively skewed, so the log transformation of the data would help us to better understand its distribution, which appears to be normal with a peak at 0.08.


```{r echo=FALSE, Univariate_plot_fsd}

p1 <- ggplot(data = rw, aes(x = free.sulfur.dioxide)) +
  geom_histogram(binwidth = 5, color = 'black', fill = "#993366")

p2 <- ggplot(data = rw, aes(x = free.sulfur.dioxide)) +
  geom_histogram(binwidth = 0.1, color = 'black', fill = "#993366") + 
  scale_x_log10()

p3 <- ggplot(data = rw, aes(x = total.sulfur.dioxide)) +
  geom_histogram(binwidth = 10, color = 'black', fill = "#993366")

p4 <- ggplot(data = rw, aes(x = total.sulfur.dioxide)) +
  geom_histogram(binwidth = 0.05, color = 'black', fill = "#993366") + 
  scale_x_log10()

grid.arrange(p1, p2, p3, p4)

summary(rw$total.sulfur.dioxide)

```

The histograms of free and total sulfur dioxide are both long tailed. With the log transformation of the data, we can tell that both variables have normal distributions. Free sulfur dioxide peaks at 9, 11 and 14, where total sulfur dioxide peaks at 40 mg / dm^3.


```{r echo=FALSE, Univariate_plot_den}

ggplot(data = rw, aes(x = density)) +
  geom_histogram(binwidth = 0.001, color = 'black', fill = "#993366")

summary(rw$density)

```

Most red wines are of density between [0.995 - 1.0]: mean and median are about 0.996 g / cm^3


```{r echo=FALSE, Univariate_plot_ph}

ggplot(data = rw, aes(x = pH)) +
  geom_histogram(binwidth = 0.05, color = 'black', fill = "#993366")

summary(rw$pH)

```

Most red wines are of pH between [3.1 - 3.5]: mean and median are about 3.3


```{r echo=FALSE, Univariate_plot_sul}

p1 <- ggplot(data = rw, aes(x = sulphates)) +
  geom_histogram(binwidth = 0.1, color = 'black', fill = "#993366")

p2 <- p1 + scale_x_log10()

grid.arrange(p1, p2)

summary(rw$sulphates)

```

Most red wines are of sulphates between [0.5 - 0.9]: mean and median are about 0.6 g/dm3


```{r echo=FALSE, Univariate_plot_alcoh}

ggplot(data = rw, aes(x = alcohol)) +
  geom_histogram(binwidth = 0.25, color = 'black', fill = "#993366")

summary(rw$alcohol)

```

Most red wines are of an alcohol percentage between [9 - 11.5]: mean and median are about 10%.

```{r echo=FALSE, Modify_the_Dataframe}

# Turn quality discrete numbers into a numeric categorical factor
rw$quality.factor <- factor(rw$quality)

# Summarise quality numbers to 3 categorical factors, OK=3-4, Good=5-6, V.Good=7-8
rw$grade <- cut(rw$quality, breaks = c(2.5,4.5,6.5,8.5), labels = c('OK','Good','V.Good'))

# Turn alcohol percentages into a string categorical factor
rw$alcohol.intensity <- cut(rw$alcohol, breaks = c(7.9, 10.4, 12.9, 15.4), labels = c('Low.Alcohol','Medium.Alcohol','High.Alcohol'))

```


# Univariate Analysis

### What is the structure of your dataset?
A data frame containing 1,599 red wines with 11 attributes (fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, pH, sulphates, alcohol) + output (quality rating) between 0 = very bad and 10 = very excellent where at least 3 wine experts rated the quality.

### What is/are the main feature(s) of interest in your dataset?
The main features of my dataset are quality and alcohol. I am interested in deciding what features can be used to predict wine quality. I noticed a possible correlation between wine quality and alcohol, which can be used as a predictive model for wine quality.

### What other features in the dataset do you think will help support your investigation into your feature(s) of interest?
Citric and volatile acidity, sulphates, and maybe density.

### Did you create any new variables from existing variables in the dataset?
Yes, I noticed that the wine dataset did not have any factors. I created 3 new variables, the first is **quality.factor** which is a numeric categorical factor based on the wine quality number. The second is **grade**, I noticed that wine quality numbers can be lumped into three groups, for simplicity and less-busy plots, representing the grade of the wine. Grade can be either OK, Good or Very Good. The third variable is **alcohol.intensity**, which is a string description representing the intensity of the alcohol (Low, Medium, or High). However, it is important to note here that the alcohol intensity for my current sample only vary from 8.4% up to 14.9%, which is not a wide-enough range to label the upper limit as 'High' as some wine rating sites consider only wine with alcohol percentage above 15% as 'High'.

### Of the features you investigated, were there any unusual distributions? Did you perform any operations on the data to tidy, adjust, or change the form of the data? If so, why did you do this?
Yes, I applied both log and sqrt transformation on a subset of the data where their graphs were positively skewed in order to understand their distribution better.


# Bivariate Plots Section

```{r echo=FALSE, message=FALSE, warning=FALSE, Correlation}

df <- subset(rw, select = c(2:13))
cor(df)

```

Quality correlates highly with alcohol and volatile acidity (correlation coefficient > 0.3), but also there seems to be interesting correlations with some of the supporting variables. Free sulfur dioxide correlates highly with total sulfur dixoide, fixed acidity with both pH and density, density with both alcohol and residual sugar, sulphates and chlorides. Let me generate a correlation matrix to have a better insight.

```{r echo=FALSE, message=FALSE, warning=FALSE, Correlation_Matrix}

chart.Correlation(subset(rw, select = c("volatile.acidity", "citric.acid", "alcohol", "density", "pH", "quality")),
                  histogram=TRUE, pch=20)

```

I chose to show mainly the chemical features that perhaps has a meaningful correlation with wine quality. from the above correlation matrix, quality correlates positivly with alcohol, with a correlation coefficient of about 0.48. On the other hand, it correlates negatively with volatile acid, with a -0.39 coefficient. Citric and volatile acids tend to correlate negatively.

```{r echo=FALSE, Bivariate_Plots_Alcho_and_Qual}
ggplot(aes(x = quality,y = alcohol), data = rw) +
      geom_point()

ggplot(aes(x = quality,y = alcohol), data = rw) +
  geom_point(alpha = 1/5, position = position_jitter(h = 0), color = '#993366') +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = 'blue')

```

The original plot of wine quality and alcohol looks overplotted, so I decided to plot another one with transparency and jitter effects. I also graphed the average, 1st, 2nd, and 3rd quantile. As alcohol increases, wine quality increases too. Most of good-quality (rating of 5.5 and higher) wine is of alcohol concentration between 9.5 - 13%.

```{r echo=FALSE, Bivariate_Plots_Vol_and_Qual}
ggplot(aes(x = quality, y = volatile.acidity), data = rw) +
  geom_point()

ggplot(aes(x = quality, y = volatile.acidity), data = rw) +
  geom_point(alpha = 1/5, position = position_jitter(h = 0), color = '#993366') + 
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = 'blue')

# 
# ggplot(aes(x = alcohol.intensity, y = quality), data = rw) +
#   geom_boxplot()

```

By plotting the relationship between quality and volatile acidity, it is also clear from the first plot that volatile acidity is a set of integers just like alcohol percentages. The plot suffered from overplotting, so I modified it using transperancy and jitter, where it is shown that there's a negative correlation between quality and volatile acidity.

```{r echo=FALSE, message=FALSE, Bivariate_Plots_Sulph_and_Citric}

ggplot(aes(x = citric.acid,y = sulphates), data = rw) +
  geom_point(alpha = 1/5, position = position_jitter(h = 0), color = '#993366') +
  geom_smooth()

```

As citric acid level increases, sulphates level tend to increase as well.

```{r echo=FALSE, message=FALSE, Bivariate_Plots_Cit_and_Vol}

ggplot(aes(x = volatile.acidity, y = citric.acid), data = rw) +
  geom_point(alpha=1/5, color = '#993366') +
  geom_smooth(se = FALSE)
  
```

There's an interesting negative correlation between citric and volatile acid that can be clearly shown using geom_smooth function.

```{r echo=FALSE, message=FALSE, Bivariate_Plots_Sug_and_Den}

ggplot(aes(x = density, y = residual.sugar), data = rw) +
  geom_point(alpha = 1/5, color = '#993366') +
  geom_smooth()

```

As density increases, residual sugar amount increases as well. Geom_smooth helped in showing the positive correlation.


```{r echo=FALSE, message=FALSE, Bivariate_Plots_Boxp1}

ggplot(aes(x = quality.factor, y = sulphates), data = rw) +
  geom_boxplot()


```

Best quality wines have the highest sulphates levels. It is also interesting to see lots of sulphates outliers specifically with wine quality number 5. I wonder what the cause of this.

```{r echo=FALSE, message=FALSE, Bivariate_Plots_Boxp2}

ggplot(aes(x = grade, y = volatile.acidity), data = rw) +
  geom_boxplot()

```

Wines with highest quality have the lowest median volatile acidity. Which is as I expected since it was shown from the correlation matrix that quality correlates negatively with volatile acidity.

```{r echo=FALSE, message=FALSE, Bivariate_Plots_Boxp3}

ggplot(aes(x = alcohol.intensity, y = density), data = rw) +
  geom_boxplot()

ggplot(aes(x = grade, y = density), data = rw) +
  geom_boxplot()

```

Here, I plot one of the created factors representing alcohol intensity against density. High alcohol intensity wines have the lowest meadian density. I wonder if that also dictates that high quality wines have lower density since I noticed from above plots that alcohol correlates highly with wine quality. After plotting, yes it does.


# Bivariate Analysis

### Talk about some of the relationships you observed in this part of the investigation. How did the feature(s) of interest vary with other features in the dataset?
I found a positive correlation btween wine quality and alcohol. A negative correlation between quality and volatile acidity.

### Did you observe any interesting relationships between the other features (not the main feature(s) of interest)?
Yes, I found an interesting relationship between some supporting variables, volatile acidity and citric acid (negative), sulphates and citric acid (positive). Free sulfur dioxide correlates highly with total sulfur dixoide, fixed acidity with both pH and density, density with both alcohol and residual sugar, and sulphates correlates with chlorides.

### What was the strongest relationship you found?
The quality of the wine is positivley and highly correlated with alcohol. Moreover, alcohol correlates very highly with the pH levels of the wine. On the other hand, the citric acid levels of the wine correlates highly and negatively with volatile acidity levels which in return correlates with wine quality as well.

# Multivariate Plots Section

```{r echo=FALSE, Multivariate_Plots1}

ggplot(aes(x = alcohol, y = citric.acid), data = rw) +
  geom_point(aes(color = grade)) +
  scale_colour_brewer(palette = "RdPu")  + theme_dark()

ggplot(aes(x = alcohol, y = volatile.acidity), data = rw) +
  geom_point(aes(color = grade), position = position_jitter(h = 0)) +
  scale_colour_brewer(palette = "RdPu")  + theme_dark()

ggplot(aes(x = alcohol, y = density), data = rw) +
  geom_point(aes(color = grade), position = position_jitter(h = 0)) +
  scale_colour_brewer(palette = "RdPu")  + theme_dark()
# 
# 
# ggplot(aes(x = quality.factor, y = density), data = rw) +
#   geom_point(aes(color = alcohol), position = position_jitter(h = 0)) +
#   scale_color_gradient(low = "white", high = "brown")
# 
# 
# qplot(data=rw, x=alcohol) + facet_wrap(~quality.factor )

```

Here I'm exploring the correlation between wine grade and alcohol combined against a 3rd chemical feature. Since the rating.number factor still gives a busy plot, I grouped two quality numbers together where Q(3,4) is considered OK, Q(5,6) = Good, and Q(7,8) is Very Good. It looks to me that most of the high quality wines have medium alcohol intensity and higher citirc acid, but lower volatile acidity, and lower density.


```{r echo=FALSE, Multivariate_Plots2}

ggplot(aes(x = chlorides, y = free.sulfur.dioxide), data = rw) +
  geom_point(aes(color = alcohol.intensity), position = position_jitter(h = 0), alpha = 0.6) +
  scale_colour_brewer(palette = "YlOrRd")  + theme_dark()

ggplot(aes(x = density, y = volatile.acidity), data = rw) +
  geom_point(aes(color = alcohol.intensity), position = position_jitter(h = 0), alpha = 0.6) +
  scale_colour_brewer(palette = "YlOrRd")  + theme_dark()

ggplot(aes(x = citric.acid, y = density), data = rw) +
  geom_point(aes(color = alcohol.intensity), position = position_jitter(h = 0), alpha = 0.6) +
  geom_line(stat = 'summary', fun.y = mean, color = 'white') +
  scale_colour_brewer(palette = "YlOrRd")  + theme_dark()

ggplot(aes(x = sulphates, y = residual.sugar), data = rw) +
  geom_point(aes(color = alcohol.intensity), position = position_jitter(h = 0), alpha = 0.6) +
  scale_colour_brewer(palette = "YlOrRd")  + theme_dark()

ggplot(aes(x = sulphates, y = residual.sugar), data = rw) +
  geom_point(aes(color = alcohol.intensity), position = position_jitter(h = 0)) +
  scale_colour_brewer(palette = "YlOrRd")  + theme_dark() +
  coord_cartesian(xlim = c(0.3, 1), ylim = c(1, 8)) +
  geom_line(stat = 'summary', fun.y = mean)

```

I created those plots to explore how alcohol interacts with other chemical variables. High intensity alcohol have low chlorides but increasing free sulfur dioxide. Most of high alchohol intensity have both low density and volatile acidity. As for sulphates and residual sugar against alcohol intensity, the plot was very busy, so I changed the x and y limits and added the mean to the graph. I am not very sure what can I infer from this plot. I think I reached a deadend with it. 

```{r echo=FALSE, Multivariate_Plots3}

ggplot(aes(x = pH, y = residual.sugar), data = rw) +
  geom_line(aes(color = grade), stat = 'summary', fun.y = median) +
  geom_line(stat = 'summary', fun.y = mean, linetype = 2) +
  coord_cartesian(xlim = c(2.9, 3.75)) +
  scale_colour_brewer(palette = "RdPu")  + theme_dark() 

ggplot(aes(x = sulphates, y = volatile.acidity), data = rw) +
  geom_line(aes(color = grade), stat = 'summary', fun.y = median) +
  geom_line(stat = 'summary', fun.y = mean, linetype = 2) +
  scale_colour_brewer(palette = "RdPu")  + theme_dark() 

ggplot(aes(x = citric.acid, y = chlorides), data = rw) +
  geom_point(aes(color = grade), position = position_jitter(h = 0), alpha = 0.6) +
  geom_line(stat = 'summary', fun.y = mean, color = 'white') +
  scale_colour_brewer(palette = "RdPu")  + theme_dark()


```

Wines with higher quality tend to have higher medians under low pH but high residual sugar levels, while the interesting catch is that lower quality wines tend to have higher medians as pH level increases. Another interesting patterns are found in the volatile acidity vs. sulphates plot. Wines of higher quality tend to have lower medians in sulphates levels. As for the last plot, it shows how wine quality correlates positively with citric acid but negatively with chlorides levels.


```{r echo=FALSE, Multivariate_Plots4}

ggplot(aes(x = density, y = volatile.acidity), data = rw) +
  geom_point(aes(color =  density),  size = 1) +
  scale_color_gradient(low = "white", high = "navy") +
  facet_grid(alcohol.intensity ~ grade)  

```

Wines of higher quality and alcohol have higher density.

```{r echo=FALSE, Multivariate_Plots5}

ggplot(aes(y = pH, x = density), data = rw) +
  geom_point(aes(color = rw$fixed.acidity), alpha = 0.5) 

```

Since I noticed from the correlation matrix that there's a correlation between fixed acidity and both density and pH, I plotted the 3 variables. As fixed acidity increases, density increases as well, but pH decreases.

# Multivariate Analysis

### Talk about some of the relationships you observed in this part of the investigation. Were there features that strengthened each other in terms of looking at your feature(s) of interest?
High quality wines have highest medians of alcohol but lower volatile acidity. Quality also seems to correlates positively with citric acid. Also from the line graphs, quality correlates positively with residual sugar but negativeily with pH.

### Were there any interesting or surprising interactions between features?
The wine quality median pattern changes dramatically with the increase of pH levels. Higher quality wines have higher medians when pH levels are low. On the other hand, higher quality wines have lower medians when plotted against sulphates and volatile acidity.

### OPTIONAL: Did you create any models with your dataset? Discuss the strengths and limitations of your model.
No.

------

# Final Plots and Summary

### Plot One
```{r echo=FALSE, Plot_One}

ggplot(aes(x = quality), data = rw) + 
  geom_histogram(color = "black", fill = "#993366", binwidth = 1) +
  xlab("Quality (10-Point Scale)") + ylab("Number of Wine Bottles") +
  ggtitle("Distribution of Wine Quality") +
  scale_x_continuous(limits = c(1,10))


```

### Description One
The distribution of wine quality appears to be normal with a median around 6. 

### Plot Two
```{r echo=FALSE, Plot_Two}

ggplot(aes(x = quality, y = alcohol), data = rw) +
  geom_point(alpha = 1/5, position = position_jitter(h = 0), color = '#993366') +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = 'blue') +
  xlab("Quality (10-Point Scale)") + ylab("Alcohol (percentage by volume)") +
  ggtitle("Alcohol vs. Wine Quality")

```

### Description Two
Wine quality correlates positively with alcohol concentration. 75% of my wine dataset has an alcohol concentration less than 13%.


### Plot Three
```{r echo=FALSE, Plot_Three}

ggplot(aes(x = alcohol, y = volatile.acidity), data = rw) +
  geom_point(aes(color = grade), position = position_jitter(h = 0)) +
  scale_colour_brewer(palette = "RdPu")  + theme_dark() +
  xlab("Alcohol (percentage by volume)") + ylab("Volatile Acidity (acetic acid - g / dm^3)") +
  ggtitle("Volatile Acidity vs. Alcohol")

ggplot(aes(x = citric.acid, y = chlorides), data = rw) +
  geom_point(aes(color = grade), position = position_jitter(h = 0), alpha = 0.6) +
  geom_line(stat = 'summary', fun.y = mean, color = 'white') +
  scale_colour_brewer(palette = "RdPu")  + theme_dark() +
  xlab("Citric Acid (g / dm^3)") + ylab("Chlorides (sodium chloride - g / dm^3)") +
  ggtitle("Chlorides vs. Citric Acid")

```

### Description Three

High quality wines appear to have higher alcohol percentage but lower volatile acidity. Higher citric acid and but lower chlorides

------

# Reflection

The redwine dataset contains 1,599 observations with 13 variables. I started by exploring each variable individually by looking at the distribution of each. There are 12 chemical variables for each observation in the wine dataset which outputs a quality number. I converted the quality number into a categorical factor, and also created 2 other factors to represent alcohol intensity and a more simplified alcohol grade.

Through the exploratory data analysis, I managed to observe that wine quality was highly and positively correlated with **alcohol** content, which was surprising as I expected alcohol to come little bit after in terms of quality effect. Secondly, wine quality depends on **volatile acidity** but negatively, which is not surprising as too high of levels of volatile acidity can lead to an unpleasant, vinegar taste. Thirdly, unlike volatile acidity, wine quality correlates positively with **citric acid**, probably because citric acid can add 'freshness' and flavor to wines. Lastly, wine quality correlates negatively with **chlorides** which is not surprising as chlorides reflects the amount of salt in the wine.

Some limitation to the dataset is lack of fermentation information. After some research, fermentation in terms of the time and the process the wine took to be fermented can also affect wine quality. The predictive model for wine quality that to be developed would have more accuracy with a time-series data regarding each wine. Another limitation is the alcohol percentage. In my sample the alcohol percentage was not very high compared to the percentages wine rating websites abide by, where they consider high alcohol intensity to be of a concentration more than 15%.


# References

- [1] http://winefolly.com/tutorial/the-lightest-to-the-strongest-wine/
- [2] http://winefolly.com/review/different-types-of-wine/
- [3] http://www.sawislibrary.co.za/dbtextimages/17136.pdf


