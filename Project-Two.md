Individual’s Attributes and Their Effect on Annual Health Insurance
Charges
================
Dylan Kruse
4/18/2021

# Introduction

The dataset in this project was obtained from [Kaggle](www.kaggle.com)
but is originally published in Brett Lantz’s *Machine Learning with R*
and originally contained 1338 observations of six different variables.
Since I found this dataset separate from *Machine Learning with R*, I
was unable to find any literature regarding the collection methods of
this data, but it is reasonable to assume that the data comes from a
sample of clients from a singular insurance agency.

The variables that are present in the original *insurance* dataset were
age, sex, BMI, number of children, smoking status, region of inhabitant,
and annual medical insurance charges of different individuals. The four
numeric variables in this dataset were age, body mass index (BMI),
number of children, and insurance charges which had respective units of
years, kilogram per meter squared, number, and dollars. The categorical
variables in this dataset were sex, smoking status and region of
inhabitant. Sex and smoking status were both binary variables with
response of either male or female and yes or no. Region of inhabitant
included four regions, the northeast, southeast, southwest, and
northwest. After removing the region variable, there was still 1338
observations.

Luckily, the data was in tidy form upon obtaining it, but region
variable was dropped as I felt it would convolute the models based upon
the other variables.

The main motivation I had in analyzing this dataset was based on
actuarial science. The reason an insurance company would collect this
data about their clients is simply based on profit. An insurance agency
does not profit unless they receive more money in patient charges than
they spend in medical costs, so they have an incentive to precisely
determine how much they should be charging their patients based on the
patient’s traits.

Based on what I know about how factors such as age, BMI, and smoking
affect an individuals health, I would expect individuals who are older,
smoke, and have a larger BMI to require more medical attention and thus
have larger medical costs. Larger medical costs would correspond to
higher insurance premiums. While I did anticipate this effect, I didn’t
believe sex would play too large of a role on medical insurance when age
is held constant.

# Explanatory Data Analysis

### Calling the necessary libraries

Every library that was to be used throughout this assignment was called
in order to save time.

### Importing the dataset

``` r
# use `read.csv` to import the dataset 
insurance <- read.csv("insurance.csv") %>% select(-region)
head(insurance)
```

    ##   age    sex    bmi children smoker   charges
    ## 1  19 female 27.900        0    yes 16884.924
    ## 2  18   male 33.770        1     no  1725.552
    ## 3  28   male 33.000        3     no  4449.462
    ## 4  33   male 22.705        0     no 21984.471
    ## 5  32   male 28.880        0     no  3866.855
    ## 6  31 female 25.740        0     no  3756.622

### Summary Statisitcs

``` r
insurance_num <- insurance %>%
  select_if(is.numeric)
summary(insurance_num) %>%
   kbl(caption = 
         "Summary Statistics for Age, BMI, Number of Children, and Insurance Charges") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>
Summary Statistics for Age, BMI, Number of Children, and Insurance
Charges
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
age
</th>
<th style="text-align:left;">
bmi
</th>
<th style="text-align:left;">
children
</th>
<th style="text-align:left;">
charges
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Min. :18.00
</td>
<td style="text-align:left;">
Min. :15.96
</td>
<td style="text-align:left;">
Min. :0.000
</td>
<td style="text-align:left;">
Min. : 1122
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
1st Qu.:27.00
</td>
<td style="text-align:left;">
1st Qu.:26.30
</td>
<td style="text-align:left;">
1st Qu.:0.000
</td>
<td style="text-align:left;">
1st Qu.: 4740
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Median :39.00
</td>
<td style="text-align:left;">
Median :30.40
</td>
<td style="text-align:left;">
Median :1.000
</td>
<td style="text-align:left;">
Median : 9382
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Mean :39.21
</td>
<td style="text-align:left;">
Mean :30.66
</td>
<td style="text-align:left;">
Mean :1.095
</td>
<td style="text-align:left;">
Mean :13270
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
3rd Qu.:51.00
</td>
<td style="text-align:left;">
3rd Qu.:34.69
</td>
<td style="text-align:left;">
3rd Qu.:2.000
</td>
<td style="text-align:left;">
3rd Qu.:16640
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Max. :64.00
</td>
<td style="text-align:left;">
Max. :53.13
</td>
<td style="text-align:left;">
Max. :5.000
</td>
<td style="text-align:left;">
Max. :63770
</td>
</tr>
</tbody>
</table>

``` r
insurance$quartile <- ntile(insurance$charges, 4)
head(insurance)
```

    ##   age    sex    bmi children smoker   charges quartile
    ## 1  19 female 27.900        0    yes 16884.924        4
    ## 2  18   male 33.770        1     no  1725.552        1
    ## 3  28   male 33.000        3     no  4449.462        1
    ## 4  33   male 22.705        0     no 21984.471        4
    ## 5  32   male 28.880        0     no  3866.855        1
    ## 6  31 female 25.740        0     no  3756.622        1

In order to create another categorical variable, a variable was created
which showed the quartile of insurance charges. Having this variable
present allows for better analysis of predeictor variables across
varying levels of the response variable. Plus, the quartile variable
makes the MANOVA test possible.

Let’s create some graphs to look at the variables.

### Graphs

``` r
# graphs for cost quartiles age and body mass index
insurance %>%
  select(quartile,age,bmi) %>%
  pivot_longer(-1,names_to='DV', values_to='measure') %>%
  ggplot(aes(quartile,measure,fill=quartile)) +
  geom_bar(stat="summary", fun = "mean") +
  geom_errorbar(stat="summary", fun.data = "mean_se", width=.5) +
  facet_wrap(~DV, nrow=2) +
  coord_flip() + 
  ylab("") + xlab("Annual Insurance Cost Quartile") +
  ggtitle("Mean Age and BMI for Each Cost Quartile") 
```

![](Project-Two_files/figure-gfm/BMI%20and%20Age%20Graph-1.png)<!-- -->

From these graphs, we can see that the mean BMI is steadily increasing
across insurance cost quartile, but it doesn’t appear to be too drastic.
The mean age is highest for the third cost quartile. Let’s investigate
the age variable a bit more in depth.

``` r
# graph for age distribution across charge quartiles
ggplot(insurance, aes(age)) +
  geom_density(fill="cornflower blue", color="black", alpha=.6) +
  facet_wrap(~quartile, scales = "free_x") +
  xlab("Age") + ylab("Density") +
  ggtitle("Age Distribution Across Insurance Cost Quartiles")
```

![](Project-Two_files/figure-gfm/Age%20Graph-1.png)<!-- -->

``` r
insurance %>%
  group_by(quartile) %>%
  summarize("mean age" = mean(age)) %>%
  kbl(caption = "Mean Age for Each Costs Quartile") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), position = "center", full_width = F)
```

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Mean Age for Each Costs Quartile
</caption>
<thead>
<tr>
<th style="text-align:right;">
quartile
</th>
<th style="text-align:right;">
mean age
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
23.81194
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
40.25970
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
52.57784
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
40.22156
</td>
</tr>
</tbody>
</table>

As can be seen in the first, second, and third cost quartile, the
distribution of individual’s age is increasing. This is understandable,
as one would expect older individuals to require more medical attention
and thus pay more money for medical insurance. Interestingly, the
distribution of ages in the fourth cost quartile appears to be uniformly
distributed across all ages. In fact, the mean age of individual in the
fourth cost quartile is 40.22 while the mean age of individual in the
third cost quartile is 52.58. This indicates that age alone is not
responsible for the increase of insurance costs.

After having seen these numeric variable’s distribution across cost
quartiles it would be beneficial to visualize a correlation matrix in
the form of a heat map. This will help determine which variables have a
correlation with our response variable – insurance charge

``` r
# Create Correlation Heatmap
cor(insurance_num, use = "pairwise.complete.obs") %>%
  as.data.frame %>%
  rownames_to_column %>%
  pivot_longer(-1, names_to = "other_var", values_to = "correlation") %>%
  ggplot(aes(rowname, other_var, fill=correlation)) +
  geom_tile() +
  scale_fill_gradient2(low="red",mid="white",high="cornflowerblue") +
  geom_text(aes(label = round(correlation,2)), color = "black", size = 4) +
  labs(title = "Correlation Matrix for Insurance Dataset", x = "Variable 1", y = "Variable 2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Project-Two_files/figure-gfm/Correlation%20Heat%20Map-1.png)<!-- -->

Understandably, there appears to be relatively weak correlation
coefficients across the board. This makes sense seeing as there is only
one response variable being looked at. The two largest correlation
coefficients observed are the ones between age and insurance charges and
between BMI and insurance charges with coefficients of 0.3 and 0.2
respectively.

While there is a lack of a strong correlation coefficient, it is very
likely that there exists interactions with other, categorical,
variables.

Since we have yet to look at any categorical variables, lets visualize
what role smoking has on individual’s insurance cost quartile.

``` r
# find number of smokers/non smokers for each cost quartile
insurance %>%
  group_by(quartile, smoker) %>%
  summarize(n = n()) 
```

    ## `summarise()` has grouped output by 'quartile'. You can override using the `.groups` argument.

    ## # A tibble: 6 x 3
    ## # Groups:   quartile [4]
    ##   quartile smoker     n
    ##      <int> <chr>  <int>
    ## 1        1 no       335
    ## 2        2 no       335
    ## 3        3 no       314
    ## 4        3 yes       20
    ## 5        4 no        80
    ## 6        4 yes      254

``` r
# make a smoker dataset
smoker <- data.frame(quartile = c(1,1,2,2,3,3,4,4),
                     smoke = c("yes", "no","yes", "no","yes", "no","yes", "no"),
                     n = c(0,335,0,335,20,314,254,80))
#create plot
ggplot(data=smoker, aes(x=quartile, y=n, fill = smoke)) +
  geom_col(position = "dodge", color = "black", alpha = .8) +
  scale_fill_manual(values = c("cornflower blue", "firebrick")) +
  geom_text(aes(label = n), colour = "white", vjust = 1.5, position = position_dodge(.9)) +
  xlab("Insurance Cost Quartile") + ylab("Individual Count") +
  ggtitle("Number of Smokers in Each Insurance Cost Quartile") 
```

![](Project-Two_files/figure-gfm/Smoker%20Table-1.png)<!-- -->

As you can see, the largest number of smokers exist in the fourth
quartile for annual insurance cost, while all the smokers present in the
dataset exist in either the third or fourth cost quartile. This
indicates that there is likely to be a positive relationship between
smoking and annual cost of insurance.

Having seen how the variables age, BMI, and smoking affect and
individual’s annual insurance charge, it is now time to model a method
to predict an individuals insurance charge.

Let’s begin with a MANOVA.

# MANOVA

### Initial MANOVA Test

``` r
# create MANOVA for the three relevant numerical predictor variables
manova_insurance <- manova(cbind(age, bmi, children) ~ quartile, data = insurance)
summary(manova_insurance)
```

    ##             Df  Pillai approx F num Df den Df    Pr(>F)    
    ## quartile     1 0.25013   148.32      3   1334 < 2.2e-16 ***
    ## Residuals 1336                                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Since the MANOVA test yielded significant results (F = 148.32, df = 1,
p-val &lt; 0.05), there is evidence to suggest that the mean of the
insurance charge is different at the different levels of age, BMI, and
children

Since the MANOVA was significant, a one-way ANOVA is performed for each
variable.

### Univariate ANOVA Analysis

``` r
# since significant, perform one-way ANOVA for each variable
summary.aov(manova_insurance)
```

    ##  Response age :
    ##               Df Sum Sq Mean Sq F value    Pr(>F)    
    ## quartile       1  63443   63443  422.78 < 2.2e-16 ***
    ## Residuals   1336 200482     150                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response bmi :
    ##               Df Sum Sq Mean Sq F value    Pr(>F)    
    ## quartile       1    421  421.06  11.411 0.0007512 ***
    ## Residuals   1336  49299   36.90                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response children :
    ##               Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## quartile       1   24.32 24.3222  16.936 4.102e-05 ***
    ## Residuals   1336 1918.62  1.4361                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Each variable, age, BMI, and number of children yielded a significant
ANOVA test, so Post-Hoc analysis for each predictor variable was
conducted.

``` r
# post-hoc analysis for age
pairwise.t.test(insurance$age, insurance$quartile, p.adj = "none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  insurance$age and insurance$quartile 
    ## 
    ##   1      2      3     
    ## 2 <2e-16 -      -     
    ## 3 <2e-16 <2e-16 -     
    ## 4 <2e-16 0.96   <2e-16
    ## 
    ## P value adjustment method: none

``` r
# post-hoc analysis for bmi
pairwise.t.test(insurance$bmi, insurance$quartile, p.adj = "none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  insurance$bmi and insurance$quartile 
    ## 
    ##   1      2      3     
    ## 2 0.3591 -      -     
    ## 3 0.0135 0.1197 -     
    ## 4 0.0024 0.0340 0.5721
    ## 
    ## P value adjustment method: none

``` r
# post-hoc analysis for children
pairwise.t.test(insurance$children, insurance$quartile, p.adj = "none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  insurance$children and insurance$quartile 
    ## 
    ##   1       2       3      
    ## 2 < 2e-16 -       -      
    ## 3 1.7e-06 3.1e-07 -      
    ## 4 8.6e-10 0.00017 0.17292
    ## 
    ## P value adjustment method: none

The post-hoc analysis tests run above show the significance level of
age, BMI, and number of children across the levels of insurance costs
quartile. The significance of these results cannot yet be discussed
since we need to correct the alpha value through which we look at the
results.

``` r
# probability of at least one type I error
1 - (.95)^22
```

    ## [1] 0.6764665

``` r
# Bonferroni correction
0.05/22
```

    ## [1] 0.002272727

Since in each post-hoc analysis test, six individual t-tests were run,
there is no longer a 0.05 probability of receiving a type I error.
Instead, there is now a 0.676 probability of receiving a type I error.
To correct for this, it is imperative that we alter our alpha value
using a Bonferroni correction. The new alpha value we should assess our
post-hoc analyses with is 0.00227. Making this the new alpha value
preserves the 0.05 probability of receiving a type I error.

Now that our alpha value has been adjusted, we can discuss the results
of the post-hoc analyses.

When analyzing the BMI variable across the varying levels of insurance
costs quartiles, there were no insurance cost quartiles which indicated
that there was a signifiicant difference in the BMI of the included
individuals.

When looking at the age of individual, each test run, with the exception
of one, returned a significant result. With the exception of the test
run between the ages of individuals in cost quartiles two and four, each
pairing of cost quartiles showed a significant difference in the age of
individuals.

Finally, when looking at the number of children an individual has, only
one non-significant result was found. There showed to be no statistical
difference in the number of children an individual claimed between the
third and fourth quartiles of insurance charges. Each other pairing of
cost quartiles showed significant difference in the number of children
and individual claims.

### MANOVA Assumptions

There are five different assumptions for the MANOVA test: random sample
and independent observations, multivariate normality, homogeneity of
grouped covariance matrices, linear relationship of response variables
(no multicollinearity), and no extreme outliers.

These five assumptions make the MANOVA test a rather restrictive one. It
is unlikely that all five assumptions are met in the MANOVA performed
above. The homogeneity of grouped covariance matrices is a very
difficult assumption to meet seeing as it requires the variance across
all groups, in this case cost quartiles, to be equal for each response
variable as well as the covariance to be equal between any of the
eighteen possible groupings of predictor variable and quartile pairings.

# Randomization Test

For the randomization test, let’s determine if the the difference in
annual insurance charges between men and women is significantly
different. To do this, we will re-sample from our sample multiple times
and calculate the difference in annual costs between men and women.

### Assumptions

The **null hypothesis** for this randomization test will be that the
true mean difference in annual insurance charges between men and women
is zero.

The corresponding **alternative hypothesis** for this test will be that
the true mean difference in annual insurance charges between men and
women is not equal to zero.

### Test

``` r
# find true difference in insurance costs between male and females
true_diff <- insurance %>%
  group_by(sex) %>%
  summarize(means = mean(charges)) %>%
  summarize(mean_diffs = diff(means)) %>%
  pull
# re sample 5000 times
mean_diff <- vector()

for(i in 1:5000){
  temp <- data.frame(sex = insurance$sex, charges = sample(insurance$charges))
  
  mean_diff[i] <- temp %>%
    group_by(sex) %>%
    summarize(means = mean(charges)) %>%
    summarize(mean_diff = diff(means)) %>%
    pull
}
```

### Null Distribution

``` r
# show null distribution 
{hist(mean_diff, main = "Distribution of Mean Annual Charges Differences Between Sexes"); abline(v = true_diff, col = "red")}
```

![](Project-Two_files/figure-gfm/Null%20Distribution-1.png)<!-- -->

The above histogram shows the distribution of annual insurance charge
mean differences between men and women when the data charges data was
re-sampled 5000 times. The distribution appears normally distributed
with a slight negative skew. The red line indicates the observed
difference in mean insurance charges between men and women.

### Randomization Test P-Value and Interpretation

``` r
# calculate a two sided p value
mean(mean_diff > true_diff | mean_diff < -true_diff)
```

    ## [1] 0.0402

With the randomization test constructed, it was determined that there
was a significant difference between the mean annual insurance costs for
men and women (p-value = 0.0398). This p-value indicates that there is a
0.04 or 4 % probability of observing a mean difference as extreme as the
one observed in the original *insurance* dataset.

# Linear Regression Model

### Orignial Regression Model

Now that both a MANOVA test and randomization test have been performed,
it is time to fit a linear regression model to the *insurance* dataset.
For this model, the explanatory variable will be the charge that is
billed to each individual while the predictor variables will be BMI,
age, and smoking status. Since the interaction terms were calculated,
the BMI and age variables were mean centered prior to fitting the model.

``` r
# consider character variables as factors
insurance <- insurance %>%
  mutate_if(is.character, as.factor)
# mean center BMI and age variable
insurance$bmi_c <- insurance$bmi - mean(insurance$bmi)
insurance$age_c <- insurance$age - mean(insurance$age)
# fit model
mlr_fit <- lm(charges ~ bmi_c * age_c * smoker, data = insurance)
summary(mlr_fit)
```

    ## 
    ## Call:
    ## lm(formula = charges ~ bmi_c * age_c * smoker, data = insurance)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -12153.0  -2013.3  -1338.7   -303.7  29393.3 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            8379.6617   151.4748  55.321   <2e-16 ***
    ## bmi_c                     7.3685    25.0946   0.294   0.7691    
    ## age_c                   266.7579    10.7674  24.774   <2e-16 ***
    ## smokeryes             23749.3215   333.7142  71.167   <2e-16 ***
    ## bmi_c:age_c               0.6835     1.7497   0.391   0.6961    
    ## bmi_c:smokeryes        1443.9253    53.7205  26.878   <2e-16 ***
    ## age_c:smokeryes          -3.7233    23.9717  -0.155   0.8766    
    ## bmi_c:age_c:smokeryes     6.6244     3.9561   1.674   0.0943 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4905 on 1330 degrees of freedom
    ## Multiple R-squared:  0.8368, Adjusted R-squared:  0.836 
    ## F-statistic: 974.3 on 7 and 1330 DF,  p-value: < 2.2e-16

``` r
summary(mlr_fit)$r.sq
```

    ## [1] 0.836816

The interpretation for the calculated coefficients are as follows:

While holding smoking status and age constant, it is expected that the
average insurance costs increase by $7.37 for every 1 unit increase in
BMI.

While holding smoking status and BMI constant, it is expected that the
average insurance costs increase by $266.76 for every 1 year increase in
age. This did indicate that age is a significant indicator of insurance
charges.

While holding both BMI and age constant, it is expected that the average
insurance costs are, on average, $23,749.32 more expensive for a smoker
versus a non-smoker.

There was no significant interaction between BMI and age, indicating
that the effect of BMI on insurance charges is the same for varying ages
and vice versa. Insurance costs only increase by $0.68 based on BMI at
varying levels of age.

There was a significant interaction between BMI and smoking status,
indicating that the effect of BMI on insurance costs is different
depending on whether or not the individual was a smoker. If the
individual is a smoker, it is expected that their average insurance
charges increase by $1443.93 for every 1 unit increase in BMI.

While not a significant interaction, the model suggests that, on
average, an individuals insurance costs are $3.72 less per 1 year
increase in age if they’re a smoker.

The final interaction coefficient indicates that the effect BMI has on
insurance costs at varying age levels does not significantly vary if the
individual is a smoker, indicating that, on average, it only increases
by $6.62.

Based on the calculated R Squared value, it can be determined that this
model suggests that 83.6% of the variation in the insurance costs can be
determined by the variation in individual’s BMI, age, and smoking
status.

### Interaction Visualization

Below is a graph which illustrates the interaction between BMI and
smoking status.

``` r
ggplot(insurance, aes(x = bmi, y = charges, color = smoker)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  xlab("Body Mass Index (BMI)") + ylab("Insurance Charges ($)") +
  ggtitle("Insurance Charges by BMI Colored by Smoking Status") 
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Project-Two_files/figure-gfm/Interaction%20Visualization-1.png)<!-- -->

It is easily observed that the slopes between BMI and insurance charges
vary drastically depending on whether or nor the individual is a smoker.
BMI affects insurance costs much more if the individual is a smoker
opposed to if they were not, which can be seen by the slope difference
of the blue and red lines.

### Assumptions

``` r
# check for linearity and equal variance
plot(mlr_fit, which = 1)
```

![](Project-Two_files/figure-gfm/Linear%20Regression%20Assumptions-1.png)<!-- -->

``` r
bptest(mlr_fit)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  mlr_fit
    ## BP = 5.3852, df = 7, p-value = 0.6131

``` r
# check for normality
plot(mlr_fit, which = 2)
```

![](Project-Two_files/figure-gfm/Linear%20Regression%20Assumptions-2.png)<!-- -->

``` r
shapiro.test(mlr_fit$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  mlr_fit$residuals
    ## W = 0.6764, p-value < 2.2e-16

Based on both the graph of residuals by fitted values and the
Breusch-Pagan test, it is likely that, in this model, the linearity and
homoscedasticity assumptions are met. Based on the QQ-Plot and the
Shapiro-Wilk test, however, the normality assumption appears to not be
met. The independence assumption is likely to be met, but cannot be
stated for certain based on ambiguous data collection methods.

``` r
# log transformation
trans_mlr_fit <- lm(log(charges) ~ bmi_c * age_c * smoker, data = insurance)
bptest(trans_mlr_fit)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  trans_mlr_fit
    ## BP = 100.06, df = 7, p-value < 2.2e-16

``` r
shapiro.test(trans_mlr_fit$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  trans_mlr_fit$residuals
    ## W = 0.75666, p-value < 2.2e-16

``` r
# square root transformation
trans_mlr_fit <- lm(sqrt(charges) ~ bmi_c * age_c * smoker, data = insurance)
bptest(trans_mlr_fit)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  trans_mlr_fit
    ## BP = 21.855, df = 7, p-value = 0.002691

``` r
shapiro.test(trans_mlr_fit$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  trans_mlr_fit$residuals
    ## W = 0.66682, p-value < 2.2e-16

``` r
# reciprocal transformation
trans_mlr_fit <- lm(1/(charges) ~ bmi_c * age_c * smoker, data = insurance)
bptest(trans_mlr_fit)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  trans_mlr_fit
    ## BP = 272.03, df = 7, p-value < 2.2e-16

``` r
shapiro.test(trans_mlr_fit$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  trans_mlr_fit$residuals
    ## W = 0.85354, p-value < 2.2e-16

Since the initial model indicated that the normality assumption was not
met, a response transformation seemed to be the right decision. After
making each transformation, the p-values for the Breusch-Pagan and
Shapiro tests were worse than they were for the original model.

### Bootstrapped/Robust Standard Errors

``` r
# original coefficients
summary(mlr_fit)$coef
```

    ##                            Estimate Std. Error    t value      Pr(>|t|)
    ## (Intercept)            8379.6617203 151.474751 55.3205181  0.000000e+00
    ## bmi_c                     7.3684599  25.094631  0.2936269  7.690888e-01
    ## age_c                   266.7578977  10.767441 24.7744946 1.002806e-111
    ## smokeryes             23749.3214923 333.714238 71.1666413  0.000000e+00
    ## bmi_c:age_c               0.6834826   1.749712  0.3906258  6.961364e-01
    ## bmi_c:smokeryes        1443.9252922  53.720500 26.8784783 1.844050e-127
    ## age_c:smokeryes          -3.7232515  23.971651 -0.1553189  8.765934e-01
    ## bmi_c:age_c:smokeryes     6.6243688   3.956135  1.6744545  9.427641e-02

``` r
# robust coefficients
coeftest(mlr_fit, vcov = vcovHC(mlr_fit))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                          Estimate  Std. Error t value Pr(>|t|)    
    ## (Intercept)            8379.66172   143.89730 58.2336  < 2e-16 ***
    ## bmi_c                     7.36846    20.95911  0.3516  0.72522    
    ## age_c                   266.75790    10.67754 24.9831  < 2e-16 ***
    ## smokeryes             23749.32149   377.48176 62.9151  < 2e-16 ***
    ## bmi_c:age_c               0.68348     1.40720  0.4857  0.62726    
    ## bmi_c:smokeryes        1443.92529    59.65406 24.2050  < 2e-16 ***
    ## age_c:smokeryes          -3.72325    24.97363 -0.1491  0.88151    
    ## bmi_c:age_c:smokeryes     6.62437     3.98705  1.6615  0.09685 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

After determining the robust standard errors, there appears to be no
differences in the significance of coefficients. Age and smoker status
continued to show significance in predicting average insurance costs as
well as the interaction between BMI and smoker status.

``` r
##  Bootstrap from observations
# Repeat bootstrapping 5000 times, saving the coefficients each time
samp_SEs <- replicate(5000, {
  # Bootstrap your data (resample observations)
  boot_data <- sample_frac(insurance, replace = TRUE)
  # Fit regression model
  fitboot <- lm(charges ~ bmi_c * age_c * smoker, data = boot_data)
  # Save the coefficients
  coef(fitboot)
})

# Estimated SEs
samp_SEs %>%
  # Transpose the obtained matrices
  t %>%
  # Consider the matrix as a data frame
  as.data.frame %>%
  # Compute the standard error (standard deviation of the sampling distribution)
  summarize_all(sd)
```

    ##   (Intercept)    bmi_c    age_c smokeryes bmi_c:age_c bmi_c:smokeryes
    ## 1    143.0141 20.97807 10.56126   377.226     1.41551        57.74791
    ##   age_c:smokeryes bmi_c:age_c:smokeryes
    ## 1        25.52504              3.806833

The bootstrapped standard errors don’t seem to follow a specific pattern
in comparison to the standard errors of the original model, as some are
larger while others are smaller. The standard errors of the coefficients
which were significant in the original regression model seemed to
increase in size while the standard errors of the non-significant
coefficients appeared to become smaller. While I am unaware how to
determine the p-values from the bootstrapped standard errors, I would
assume that the previously significant effects would have an even
smaller p-value with the bootstrapped SE’s and thus be much more robust.

# Logistic Regression Model

### Creating Binary Variable

While the *insurance* dataset does have two binary variables, sex and
smoking status, the ideal response variable to be analyzed in this
situation is the insurance charge variable. In this project, a binary
variable will be created based on whether or not an individual’s
insurance charges are in the fourth quartile. This will indicate high
annual insurance costs.

``` r
# find insurance charge associated with 
quantile(insurance$charges)
```

    ##        0%       25%       50%       75%      100% 
    ##  1121.874  4740.287  9382.033 16639.913 63770.428

``` r
insurance <- insurance %>%
  mutate(y = ifelse(charges >= 16639.913, 1, 0))
```

Once the binary variable representing high insurance charges has been
created it is now time to fit the logistic regression model.

### Fitting the Logistic Regression Model

``` r
# create logistic regression model
logi_fit <- glm(y ~ age + bmi + smoker, data = insurance)
summary(logi_fit)
```

    ## 
    ## Call:
    ## glm(formula = y ~ age + bmi + smoker, data = insurance)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -0.88915  -0.09139  -0.05976   0.00284   0.99671  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.104903   0.039990  -2.623 0.008809 ** 
    ## age          0.001685   0.000509   3.310 0.000956 ***
    ## bmi          0.003710   0.001172   3.165 0.001589 ** 
    ## smokeryes    0.856726   0.017610  48.650  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.0675243)
    ## 
    ##     Null deviance: 251.125  on 1337  degrees of freedom
    ## Residual deviance:  90.077  on 1334  degrees of freedom
    ## AIC: 196.81
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
# find exponential of coefficients
exp(coef(logi_fit))
```

    ## (Intercept)         age         bmi   smokeryes 
    ##   0.9004118   1.0016865   1.0037170   2.3554365

``` r
# percent changes
```

In a logistic regression model, the calculated coefficients represent
the log of the odds. With this in mind, when interpreting the
coefficients, the exponential of the coefficients is calculated in order
to interpret only odds.

The odds of a patient’s insurance costs being in the fourth quartile
increase by a factor of 1.002 for every additional year increase in
their age.

The odds of a patient’s insurance costs being in the fourth quartile
increase by a factor of 1.004 for every additional unit increase in
their BMI.

The odds of a patient’s insurance costs being in the fourth quartile
increase by a factor of 2.36 if they are a smoker opposed to a
non-smoker.

Let’s look into how accurate the logistic regression model we created is
in predicting whether or not an individual experiences high annual
insurance charges based on their BMI, age, and smoking status.

### Creating Confusion Matrix

``` r
# create named binary value
insurance$outcome <- ifelse(insurance$y == 1, "high annual charges", "not high annual charges")
# create variables for probability of being a fourth quartile charge individual
insurance$prob <- predict(logi_fit, type = "response")
# create predictions
insurance$predicted <- ifelse(insurance$prob > .5, "high annual charges", "not high annual charges")
# create confusion matrix
table(truth = insurance$outcome, prediction = insurance$predicted) %>% addmargins() 
```

    ##                          prediction
    ## truth                     high annual charges not high annual charges  Sum
    ##   high annual charges                     255                      80  335
    ##   not high annual charges                  19                     984 1003
    ##   Sum                                     274                    1064 1338

Above is the confusion matrix created for our predicted model. It
illustrates the counts of correctly and incorrectly high and not high
annual insurance costs.

``` r
# compute accuracy 
(255 + 984) / 1338
```

    ## [1] 0.926009

``` r
# compute sensitivity (True Positive Rate)
255 / (255 + 80)
```

    ## [1] 0.761194

``` r
# compute specificity (True Negative Rate)
984 / (984 + 19)
```

    ## [1] 0.9810568

``` r
# compute precision (Positive Predicted Value)
255 / (255 + 19)
```

    ## [1] 0.9306569

The above calculations are statistics derived from the confusion matrix.
The first calculated number reports the accuracy of the model, which is
0.926 or 92.6 %. This indicates that the created model is 92.6 %
accurate.

The next number computed represents the sensitivity or true positive
rate (TPR) of the model. The designed logistic model indicates that
there is a 0.761 TPR or 76.1 % of the observations were correctly
predicted as having high annual insurance costs.

Specificity is the next calculated number which shows the true negative
rate (TNR). The model’s confusion matrix indicates that there is a 0.981
or 98.1% true negative rate which indicates that 98.1 % of observations
were correctly predicted to have not high annual insurance costs.

Finally, the recall, or positive predicted value, was calculated. The
recall was found to be 0.93 or 93 %. This indicates that of the
observations that were predicted to have high annual insurance charges,
93 % were correctly predicted to be true.

``` r
# create variable for log odds
insurance$logit <- predict(logi_fit, type = "link")
# create plot of density of log-odds by binary outcome variable
insurance %>%
  ggplot() + 
  geom_density(aes(logit, color = outcome, fill = outcome), alpha = .4) +
    geom_rug(aes(logit, color = outcome)) +
  geom_text(x = .9, y = 4, label = "TN = 984") +
  geom_text(x = .8, y = .1, label = "FN = 80") +
  geom_text(x = .09, y = 1, label = "FP = 13") +
  geom_text(x = .1, y = 4, label = "TP = 255") +
  xlab("logit (log-odds)") +
  ggtitle("Density of Log-Odds by High or Not High Annual Insurance Charges")
```

![](Project-Two_files/figure-gfm/Logit%20Odds%20by%20Binary-1.png)<!-- -->

While I am not entirely sure, I believe the fact that there is such
little overlap between the log-odds for high and not high annual
insurance charges, the model is accurate.

``` r
# create ROC plot
ROCplot <- ggplot(insurance) +
  geom_roc(aes(d = y, m = prob), n.cuts = 0, color = "cornflowerblue") +
  ggtitle("Age, BMI, and Smoking Status Interaction ROC Plot") +
  xlab("False Positive Fraction") +
  ylab("True Positive Fraction")
ROCplot
```

![](Project-Two_files/figure-gfm/ROC%20Plot%20and%20AUC-1.png)<!-- -->

``` r
# calculate area under the curve
calc_auc(ROCplot)
```

    ##   PANEL group       AUC
    ## 1     1    -1 0.8921802

The calculated cross-validation area under the curve (AUC) is 0.892
which indicates that the constructed model correctly classifies
observations 89.2 % of the time. The constructed model is a good model.
