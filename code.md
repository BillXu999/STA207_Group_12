---
title: "STA 207 Project 1, Analysis of Class Size Effect on Test Scores"
date: "2020/1/17"
output: 
  html_document:
    df_print: paged
    fig_caption: yes
    number_sections: yes
  pdf_document: default
---

<style type="text/css">

body{ /* Normal  */
      font-size: 16px;
  }
math {
  font-size: tiny;
}  
</style>

```{r setup, include=FALSE}
knitr::opts_chunk$set(message=FALSE,warning=FALSE)
```

***
Team ID: 12

Name (responsibilities): Joseph Gonzalez (discussion, summary, proof)

Name (responsibilities): Yanhao Jin(model fitting, model diagnostics and sensitivity analysis)

Name (responsibilities): Ruichen Xu(descriptive analysis and sensitivity analysis (plots) )

Name (responsibilities): Bohao Zoum(odel fitting, model diagnostics)


***

# Introduction
In elementary school, students and teachers are faced with growing class sizes. While having a large class size may be great for social interaction or budget stability, it often takes away from individual student teacher interactions. As a result, teachers may not adequately identify student needs and, thus, present the material in a non-effective way. This leads to the question “does class size matter?” This project attempts to investigate this issue by examining class size’s relationship with test scores.

## Background

In this project, we analyze data from a 4-year longitudinal experimental study of reduced class size called Project Star. Project Star examined the effect of a small(13 to 17 students per teacher), regular(22 to 25 students per teacher), and regular-with-aide (22 to 25 students with a full-time teacher's aide) class size had on the students’ test scores. In this experiment, schools with a sufficient number of students were randomly selected, the students in these schools were randomly assigned to the three class types and the teachers were randomly assigned to classes.

This analysis is important because it could show how relevant class size is in determining the foundation for a child’s success in school. If small class size is positively associated with math scores, the analysis results can help educators understand the significance of smaller class sizes in promoting student achievement and learning. This analysis could also persuade legislators to avoid mandating larger class sizes in elementary schools.


## Statistical questions of interest

The overall scientific question of interest is whether the class size for first-grade students is associated with their math test scores. We intend to estimate the mean math scaled scores for each class size and investigate the effects class types might have on the scaled math scores. In addition, we will examine whether there is a difference in the math scaled score in 1st grade across students in different class types. 

# Analysis Plan

1.  Install the `AER` package and load the `STAR` dataset.  
2.  Explore this dataset and generate summary statistics (in forms of tables or plots) that you find informative, and explain them. Draw the histogram of Math score for the students of 1st grade, and draw the boxplot for the class types.
3.  Use a one-way ANOVA model to study the effects of class types on the math scaled scores. Explain why your model is appropriate for this task on this data set. 
4.  Fit the one-way ANOVA model and get the ANOVA Table. 
5.  Conduct model diagnostic. 
6.  Test whether there is a difference in the math scaled score in 1st grade  across students in different class types. Derive the confidence intervals for all factor level means and all pairwise differences.
7.  Discuss whether you are able to make any causal statements based on your analysis. 

## Population and study design

The study randomly assigned students to small classes, regular classes, and regular classes with a teacher’s aide. In order to randomize properly, schools were enrolled only if they had enough studybody to have at least one class of each type. Once the schools were enrolled, students were randomly assigned to the three types of classes, and one teacher was randomly assigned to one class.

The dataset contains scaled scores for math and reading from kindergarten to 3rd grade. We first only focus on the math scores in 1st grade.

## Statistical Analysis

### Descriptive Analysis

In order to get insights of the dataset, we propose following descriptive analysis to the `STAR` data set:

***

* First, we plan to explore this dataset and generate summary statistics.
* If there are any missing values, we will drop them from the data set.
* After dropping the missing values, we will obtain the summary statistics for the 1st-grade students’ math scores and the class sizes.
* We will also generate the histogram of the first-grade math scores and the side-by-side box plots of the response variable for each factor level.

***

### Main Analysis

In the dataset, the three treatment groups for the 1st grade students are small classes with 13 to 17 students, regular classes with 22 to 25 students and regular classes of 22 to 25 students with a teaching aide. 

To determine whether class size has an effect on the math score, The following causal effects can be defined for each group:
$$\small \mu_{j}-\mu_{i},\quad 1\leq i\leq j\leq 3$$
If the difference between two groups $\mu_{j}-\mu_{i}$ is not equal to zero, we can say that the class size has causal effect on math score for the first grade students.

Out of this purpose, we consider the following one-way ANOVA model

$$\small Y_{ij}=\mu_{i}+\epsilon_{ij}\quad\quad i=1,\dots,r;\quad j=1,\dots,n_i$$
where in the dataset $\small r=3$ is the number of treatments

* $\small Y_{ij}$ is the math test score of $\small j$-th individual in group $i$
* $\small \mu_i$ is the mean math score for the 1st grade of each treatment group:
  + $\small \mu_{1}$ is the mean math score for the 1st grade of regular class;
  + $\small \mu_{2}$ is the mean math score for the 1st grade of small class
  + $\small \mu_{3}$ is the mean math score for the 1st grade of regular class with a teacher aide.
* $\small \epsilon_{ij}$'s are independent normal random variables with mean zero and variance $\small \sigma^{2}$.
* $\small n_i$ is the number of each treatment group and $\small n_T=\sum_{i=1}^{r}n_{i}$ is the total number of students in the experiment.

For the fitted model, we will have two binary variables that are indicators for the respective treatment groups. This allows for the differences estimator to capture the treatment effect for each treatment group separately. This yields the following regression model:

$$\small Y_{i}=\beta_{0}+\beta_{1} \text { SmallClass}_{i}+\beta_{2} \text { RegAide}_{i}+\varepsilon_{i}$$
where $\small Y_i$ is the math test score of first grade students, regular class size is the reference class, $\small \text{SmallClass}_i$ is the small class size indicator variable and $\small \text{RegAide}_i$ is the regular class with with a teacher aide indicator variable. The error term, $\small \varepsilon_i$, is normally distributed with a mean of zero and variance $\small \sigma^2$.


To test whether or not the mean math score of the first grade students differs in three groups, we propose the following hypothesis test:

* Test hypothesis: 
  $$\small H_0: \mu_1=\mu_2=\mu_3\quad v.s.\quad H_{a}:\text{Not all factor level means are the same}$$
*The F-ratio is 
	$$\small F^{*}=\frac{M S T R}{M S E}=\frac{SSTR/2}{SSE/n_T-3}$$
* Under the null hypothesis: $\small F^{*} \stackrel{\text { iid }}{\sim} F_{(2,n_T-3)}$. 
* We can reject the null hypothesis if $$\small F^{*}>F\left(1-\alpha, 2, n_{T}-3\right)$$ or if $$\small p=P\left(F_{(2,n_T-3)}>F(1-\alpha,2,n_T-3)\right)<\alpha$$

After applying the hypothesis test on whether or not the mean math score of the first grade students differs in three groups, we will construct the confidence intervals for all factor level means. 

If we are only interested in one certain factor level mean $\small \mu_i, i=1,\cdots,r$, the confidence interval of $\mu_i$ is given by

$$\small \operatorname{C.I.}(\mu_i)=\widehat{\mu}_{i} \pm se(\widehat{\mu}_i) \times T$$

where $\small T=t\left(1-\frac{\alpha}{2} ; n_{T}-r\right)$.

On the contrary, if we are interested in all factor level means, we will use Bonferroni’s procedure ($\small g=3$ is the number of treatment groups). The form for the Bonferroni confidence intervals are:

$$\small \operatorname{C.I.}(\mu_i)=\widehat{\mu}_{i} \pm se(\widehat{\mu}_i) \times B$$
where $\small B=t\left(1-\frac{\alpha}{2 g} ; n_{T}-r\right)$.

Furthermore, we will conduct three pairwise comparison tests between the mean math scores for each class size. For the comparison, we define $$\small D_{ij}=\mu_{i}-\mu_{j}$$ for some $\small i \neq j$. For the sample, the differences between the mean math scores for each class size is represented by 

$$\small \widehat{D}_{ij}=\overline{Y}_{i\cdot}-\overline{Y}_{j\cdot}$$ 
$\small \widehat{D}_{ij}$ is an unbiased estimator of $\small D$ and its variance is:

$$\small \operatorname{Var}(\widehat{D}_{ij})=\operatorname{Var}\left(\overline{Y}_{i\cdot}\right)+\operatorname{Var}\left(\overline{Y}_{j\cdot}\right)=\sigma^{2}\left\{\frac{1}{n_{i}}+\frac{1}{n_{j}}\right\}$$

For the formula above, we must substitute $\small MSE$ for $\small \sigma^{2}$ because  $\small \sigma^{2}$ is unknown. With this substitution, the formula for the standard deviation of  $\small \widehat{D}_{ij}$ becomes:

$$\small se(\widehat{D}_{ij})=\sqrt{M S E\left(1 / n_{i}+1 / n_{j}\right)}$$

Ultimately, we will use $\small se(\widehat{D}_{ij})$, $\small \widehat{D}_{ij}$, and $\small D_{ij}$ to conduct hypothesis tests for the differences between two mean math scores for each class size. The general hypothesis test is represented by $\small H_{0}: D=0$ vs. $\small H_{a}: D \neq 0$, where the null hypothesis states that there is no difference between the mean math scores for the two respective class sizes and we note that under the null hypothesis,  
$$\small \frac{\widehat{D}_{ij}-D_{ij}}{se(\widehat{D}_{ij})} \sim t_{\left(n_{T}-r\right)}$$

The equation above produces the t-test statistic for the difference in math score means for the two different class sizes. In general, this value represents how far the sample difference in means is from the true population difference in means. Furthermore, we can construct a confidence interval for $\small D_{ij}$, which is given by 

$$\small \widehat{D}_{ij} \pm se(\widehat{D}_{ij}) t\left(1-\frac{\alpha}{2} ; n_{T}-r\right)$$
The above confidence interval provides a range that the true difference in math score means for different class sizes may reside in. This interval determines if we will reject or fail to reject the null hypothesis that there is no difference between the mean math scores for the two different class sizes. If the interval contains 0, then we fail to reject the null. Mathematically, this is represented by: 

$$\small 0 \in \widehat{D}_{ij} \pm se(\widehat{D}_{ij}) t\left(1-\frac{\alpha}{2} ; n_{T}-r\right)$$

If zero is not in the confidence interval,  then we reject $\small H_{0}$ at level $\small \alpha$ and we can conclude that the math score means for the two different class sizes are not the same(or different in value). 

We are also interested in the family-wise confidence intervals for the comparison of the factor level means, i.e. find 
$$\small P\left(D_{i j} \in \widetilde{C}_{i j}(\alpha), \text { for all } 1 \leq i<j \leq l\right)=1-\alpha$$
i.e., the probability that these Confidence Intervals simultaneously cover their respective parameter.

Type one errors are more likely to occur for simultaneous family-wise confidence intervals. To avoid this, we will control the family-wise type 1 error rate using multipliers that alter the width of the confidence intervals. One option is to use Tukey's procedure. In Tukey's procedure for families of pairwise comparisons, the confidence interval is given by
$$\small \widetilde{C}_{i j}^{T}(\alpha):=\widehat{D}_{i j} \pm s\left(\widehat{D}_{i j}\right) \times T$$
with the multiplier
$$\small T:=\frac{1}{\sqrt{2}} q\left(1-\alpha ; r, n_{T}-r\right)$$
where $\small q\left(r, n_{T}-r\right)$ is the studentized range distribution with parameters $\small r$ and $\small n_{T}-r$

At a family-wise significance level of $0.05$, we used the function `TukeyHSD()` to obtain Tukey's $95 \%$ confidence intervals for all pairwise comparisons.

Another type of confidence interval we can use for correcting the type 1 error rate is the Bonferroni confidence interval. We use the Bonferroni procedure for pre-specified pairwise comparisons. These confidence intervals have the form:

$$\small C^{B}(\alpha)=\widehat{D}_{ij} \pm s(\widehat{D}_{ij}) \times B$$
where $\small B=t\left(1-\frac{\alpha}{2 g} ; n_{T}-r\right)$ and $\small g$ is the number of comparison.

The proposed one-way anova is appropriate because we only interested in the effect of one factor (class size) on the math score. If the model assumptions are all satisfied, then 


### Model Diagnostics

For the single factor ANOVA model:
$$\small Y_{i j}=\mu_{i}+\epsilon_{i j}, \quad i=1, \cdots, r, j=1, \cdots, n_{i}$$
we rely on following assumptions

***

* Normality: $\small \epsilon_{i j}$'s are normal random variables (with mean zero).
* Equal Variance: $\small \epsilon_{i j}$'s have the same variance.
* Independence: $\small \epsilon_{i j}$'s are independent random variables.

***

To check the equal variance assumption, we use the residual vs. fitted value plot. If the residuals have a similar dispersion (around zero) across different treatment groups, then there is a constant error variance. To check the normality assumption, we use the normal Q-Q plot. If the Q-Q plots are (nearly) linear, then the normality assumption is satisfied.

To check the homogeneity of variance, we use Levene's test. Levene's test is an inferential statistic, which is used to assess the equality of variances for a variable that is calculated for two or more groups. The null hypothesis, $\small H_0$, is all $\small r$ population variances are equal and the alternative hypothesis is at least two are different. 

$$\small H_0:\sigma_{1}^{2}=\cdots=\sigma_{r}^{2}\quad v.s.\quad H_{a}:\text{Not all variances are the same}$$
The test statistic is defined as
$$\small W=\frac{(n_T-r)}{(r-1)} \cdot \frac{\sum_{i=1}^{r} n_{i}\left(Z_{i}-Z_{. .}\right)^{2}}{\sum_{i=1}^{r} \sum_{j=1}^{n_{i}}\left(Z_{i j}-Z_{i .}\right)^{2}}$$
where

***

* $\small Z_{ij}=\left|Y_{i j}-\bar{Y}_{i \cdot}\right|$, here $\small \bar{Y}_{i .}$ is the mean of the $\small i$-th group,
* $\small Z_{i .}=\frac{1}{n_{i}} \sum_{j=1}^{n_{i}} Z_{i j}$ is the mean of the $\small Z_{i j}$ for group $\small i$,
* $\small Z_{..}=\frac{1}{n_T} \sum_{i=1}^{r} \sum_{j=1}^{n_{i}} Z_{i j}$ is the mean of all $\small Z_{i j}$

***

Under null hypothesis, the test statistic $\small W$ is approximately F-distributed with $\small r-1$ and $\small n_T-r$ degrees of freedom. We reject the null hypothesis if  $$\small W\geq F(\alpha, r-1, n_T-r)$$

# Results

## Descriptive Analysis
The `STAR` data set is part of the package `AER`. 
```{r echo=FALSE}
library(MASS)
library(AER)
data("STAR")
```

After loading the packages `AER` and `MASS`, and the dataset `STAR`, `names(STAR)` shows that there are a variety of factor variables that describe student and teacher characteristics as well as various school indicators, which are separately recorded for the four different grades. The data is in wide format, which means each variable has its own column and, for each student, the rows contain observations for the variables. Using `dim(STAR)` we find that there are a total of 11598 observations on 47 variables.

***

Here is the summary of the STAR data set.
Get a overview. 

```{r echo=FALSE}
#install.packages("AER")
#get the data set
library(AER) #install.packages("AER")
data("STAR")
Data<- STAR

#select the coulomns gender, ethnicity, birth, star1 ,math1 as a new data.frame star
star<- data.frame(Data['gender'], Data['ethnicity'], Data['birth'], Data['star1'], Data['math1']) 
star[,3]<-as.numeric( substr(star[,3], 1,4) ) # translate the entries of birth from character to number

# create the summary table
Sum<-summary(star)
Sum1<- summary(star[star[,4]!='NA',])
Varbirth<- var(star[3], na.rm = TRUE)
Varmath<- var(star[5], na.rm = TRUE)
Summary<- data.frame(gender = c("male = 3541","female = 3297","","","",""), ethnicity = c("caucasion = 4528", "africa american = 2221", "asian = 22", "hispanic =9", "american indian = 9", "other = 1"), birth = c("1977 = 10", "1978 = 230", "1979 = 2105", "1980 = 4240", "1981 = 11","" ), class = c("small class 1925", "regular class = 2584", "regular class with teacher's aid = 2320", " "," "," " ) ,mathresult= c("mean = 530","variance = 1857.9", "minimum = 404", "maximum = 676", " "," ") )
 mathresult= c("mean = 530","variance = 1", "minimum = 404", "maximum = 676", " "," ")
colnames(Summary)<- c("gender", "ethnicity", "birth", "kind of class", "math score")
colnames(star)<- c("gender", "ethnicity", "birth", "class", "score")
library(knitr)
knitr::kable(Summary[,-2])
```

In this dataset, the number of male is approximately the same as the number of female. The birth of individual is concentred on 1979 and 4240. The experiment assign these people to three different size of class, and the number are all most the same. The range of math score is between 530 and 676,  and the mean is 530, the variance is 1857. 

The values above show the total number of students for each class size. We see that the regular class has 2507 students, the small class size has 1868 students, and the regular class size with a teacher aide has 2225 students. Not all treatments have the same number of students, which means the experiment is unbalanced. 

Then, we introduce the density distribution histogram of math score. 
```{r echo=FALSE}
library(plyr)
star[,4]<- revalue(star[,4], c("regular"="regular class","small"="small class","regular+aide"=" regular class with teacher's aid"))
library(ggplot2)
star<- na.omit(star)
library(ggplot2)
ggplot(star, aes(x=score)) + 
  geom_histogram(aes(y=..density..),
              binwidth=10,
               colour="black", fill="white") +
geom_density(alpha=.2, fill="#FF6666")+labs(title = "The Density Distribution Histogram of Math Score")
```

The shape of the distribution of the math score is bell-shaped. Most math score are between 480 and 580. 

And, we introduce the boxplot of the math score with respect to different genders. 
```{r echo=FALSE}
ggplot(data=star, aes(x=gender,y=score))+geom_boxplot(aes(fill=gender))+labs(title = "The Boxplot of Math Score with respect to different Genders")+geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .05)
```

This plot shows that, for different gender, the distribution of math score is almost the same. What's more, the means of the two are almost the same. Following from this we can draw a conclusion that gender does not affect the distribution of math scores. 

Then we introduce the boxplot of the math score with respect to different birth.
```{r echo=FALSE}
star[,3]<- as.factor(star[,3])
ggplot(data=star, aes(x=birth,y=score))+geom_boxplot(aes(fill=birth))+labs(title = "The Boxplot of Math Score with respect to different Birth")
```

This plot shows that the math score distributions of individual whose birth are 1979 and 1980 are almost the same; on the other hand, the distribution of math scores among individuals born in 1977 was lower than it was in 1978, and the distribution of math scores among individuals born in 1978 was lower than it was in 1979, and the distribution of math scores among individuals born in 1981 was lower than it was in 1980.  

And, we introduce another another plot, the boxplot of math score with respect to different class size. 

```{r echo=FALSE}
ggplot(data=star, aes(x=class,y=score))+geom_boxplot(aes(fill=score), fill = "red")+labs(title = "box plots of math score with respect to different kinds of class") +geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .05)
```

This plot shows that the distribution of math scores among individuals in samll class was larger than it was in regular class with teacher's aid; the distribution of math scores among individuals in regular class with teacher's aid  was larger than it was in regular class.


```{r echo=FALSE}
data <- STAR[!is.na(STAR[,5]),]
data <- STAR[!is.na(STAR[,13]),]
```

```{r echo=FALSE, message=FALSE, results="asis"}
library(magrittr)
library(qwraps2)
library(ggplot2)
library(survival)
library(ggfortify)
set.seed(42)
options(qwraps2_markup = "markdown")

our_summary1 <-
  list("Math Scores" =
       list("min" = ~ min(data$math1),
            "max" = ~ max(data$math1),
            "mean (sd)" = ~ qwraps2::mean_sd(data$math1)),
       "Class Size" =
       list("Regular" = ~ qwraps2::n_perc0(data$star1 == "regular"),
            "Small"  = ~ qwraps2::n_perc0(data$star1 == "small"),
            "Regular with a teacher aside"= ~ qwraps2::n_perc0(data$star1 == "regular+aide"))
       )
#summary_table(data, our_summary1)
```


```{r echo=FALSE}
n1<-2507
n2<-1868
n3<-2225
nt<-n1+n2+n3
```


```{r echo=FALSE}
#hist(STAR$math1,main="Histogram of Math Score of the 1st Grade",xlab = "Math Score")
#boxplot(math1~star1, data=data, xlab = "School Size", main= "Box plots of the Math Score of the 1st Grade for the factor levels")
#stripchart(math1~star1, vertical = TRUE, data = data, 
 #   method = "jitter", add = TRUE, pch = 20, col = 'blue')
```


## Inferential Analysis

We use a one-way ANOVA model to fit the data set. The summary for the regression model is given by

```{r echo=FALSE}
fit1 <- lm(math1 ~ star1, data = data)
#summary(fit1)
fit_show<- data.frame(Name = c("Intercept","small class","regular class with teacher's aid"), Estimated_Coefficient = c("525.2744","13.4033","4.3507") , Standard_Error = c("0.8541","1.3071","1.2456"))
knitr::kable(fit_show)
```

The summary represents the fit of our model involving class size as an explanatory categorical variable and math scaled scores as a numerical response variable. The regression model looks like: 

$$Y_{i}=525.2744+ 13.4033 \text { SmallClass}_{i}+4.3507\text { RegAide}_{i}+\varepsilon_{i}$$

From this model, we can use the estimated coefficients to calculate the mean for each treatment. Therefore, we can see that 

$$\hat{\mu}_{1}=525.2744\quad \hat{\mu}_{2}=525.2744+13.4033=538.6777\quad\hat{\mu}_{3}=525.2744+4.3507=529.6251$$

```{r include=FALSE}
mu1 <- 525.2744
mu2 <- 538.6777
mu3 <- 529.6251
```

For the above result, we see that the average math scores for the first-grade classes are 525.2744 points for the regular class size, 538.6777 points for the small class size, and 529.6251 points for the regular class size with a teacher aide.

Next, we get the anova table for the above regression model.

```{r include=FALSE}
anova(fit1)
```

From the analysis of variance table, we obtain a summary of the sum of squares. The sum of squares provide details about the variability of the model. The sum of squares are provided in the diagram below.

$$\begin{aligned}
&SSTR=195075.5\quad \text{df}(SSTR)=2\quad MSTR= 97537.740\\
&SSE=12065522.6\quad \text{df}(SSE)=6597\quad MSE= 1828.941
\end{aligned}$$

```{r include=FALSE}
mse <- 1828.941
```

SSTR or treatment sum of squares is a measure of the variability between the small, regular and regular with a teacher aide class-size math score means and the mean of the entire first-grade math score data set.  SSE or error sum of squares is a variability measure of the observed math scores around their respective class size math score means. We use SSTR, SSE and their degrees of freedom(df(SSTR) and df(SSE)) to calculate MSTR and MSE, which are the mean sum of squares. These values are needed to calculate the F-statistic, which we can use to determine if there is a difference between the class sizes’ mean math scores. Therefore, we calculate the F-ratio as

$$F^{*}=\frac{97537.740}{1828.941}=53.330$$

In addition, we calculate the critical value at a $95\%$ confidence level to determine where the F-ratio is located in the F-distribution. The critical value is

```{r include=FALSE}
qf(0.95,2,6597)
```

The F-ratio(53.330) is much larger than the critical value(2.997). Therefore, we reject the null hypothesis that the class size math score means are equal and there is sufficient evidence to suggest that there is a difference between the 1st-grade math scaled score means across students in different class sizes.

The standard error of $\hat{\mu_1}$, $\hat{\mu}_2$ and $\hat{\mu}_3$ are

```{r include=FALSE}
c(42.77/sqrt(2507),42.77/sqrt(1868),42.77/sqrt(2225))
```

The mathematical equations to calculate the standard errors of the means are included below. We will use these values to calculate the confidence intervals for each class size math score mean.

$$\begin{aligned}
se(\hat{\mu}_1) & =\sqrt{\frac{M S E}{n_{1}}}=\frac{42.77}{\sqrt{2507}}=0.8542049\\
se(\hat{\mu}_2) & =\sqrt{\frac{M S E}{n_{2}}}=\frac{42.77}{\sqrt{1868}}=0.9895798\\
se(\hat{\mu}_3) & =\sqrt{\frac{M S E}{n_{3}}}=\frac{42.77}{\sqrt{2225}}=0.9067222\\
\end{aligned}$$

If we are only interested in the individual(not simultaneous) class size math score mean, then the confidence intervals for each factor level mean $\mu_i$ are calculated by

```{r include=FALSE}
se1 <- 0.8542049
se2 <- 0.9895798
se3 <- 0.9067222
c(mu1-se1*qt(1-0.05/2,nt-3),mu1+se1*qt(1-0.05/2,nt-3))
c(mu2-se2*qt(1-0.05/2,nt-3),mu2+se2*qt(1-0.05/2,nt-3))
c(mu3-se3*qt(1-0.05/2,nt-3),mu3+se3*qt(1-0.05/2,nt-3))
```

Mathematically, the confidence intervals for each $\small \mu_i$ are represented by

$$\begin{aligned}
\operatorname{C.I.}(\mu_1) &= [523.5999 526.9489]\\
\operatorname{C.I.}(\mu_2) &= [536.7378 540.6176]\\
\operatorname{C.I.}(\mu_3) &= [527.8476 531.4026]
\end{aligned}$$

Now we construct the confidence intervals for all simultaneous class size math score means. We will use the Bonferroni procedure ($\small g=3$) because the number of treatment groups is small and the pre-specified inferences on the class size means. The corresponding Bonferroni multiplier is:

```{r include=FALSE}
qt(1-0.05/6,6597)
```

The Bonferroni confidence intervals for all three class size math score means are calculated by

```{r include=FALSE}
c(525.2744-42.77*qt(1-0.05/6,6597)/sqrt(2507),525.2744+42.77*qt(1-0.05/6,6597)/sqrt(2507))
c(538.6777-42.77*qt(1-0.05/6,6597)/sqrt(1868),538.6777+42.77*qt(1-0.05/6,6597)/sqrt(1868))
c(529.6251-42.77*qt(1-0.05/6,6597)/sqrt(2225),529.6251+42.77*qt(1-0.05/6,6597)/sqrt(2225))
```

Additionally, the confidence intervals for all factor level means are represented by

$$\begin{aligned}
\operatorname{C.I.}^{B}(\mu_1) &= [523.2289,527.3199]\\
\operatorname{C.I.}^{B}(\mu_2) &= [536.3081,541.0473]\\
\operatorname{C.I.}^{B}(\mu_3) &= [527.4539,531.7963]
\end{aligned}$$

Next, we determine the simultaneous confidence intervals for all pairwise differences of the class size math score means. There are a total of $3$ pairwise simultaneous comparisons and, in this case, we first will use Tukey’s procedure. Using the function `TukeyHSD()`, we obtained Tukey's $95 \%$ confidence intervals for all pairwise comparisons. The R output is shown below:

```{r include=FALSE}
TukeyHSD(aov(fit1))
```

Therefore, the confidence interval for all pairwise differences are 
$$\begin{aligned}
\mu_2-\mu_1:&\quad [10.339053,16.467545]\\
\mu_3-\mu_1:&\quad [1.430754,7.270720]\\
\mu_3-\mu_2:&\quad [-12.198626,-5.906497]
\end{aligned}$$

Since all three confidence intervals do not contain zero, we suggest that all three class size math score means have different values.

Next, we will use the Bonferroni procedure to calculate the simultaneous confidence intervals for the differences in class size math score means. This step is necessary to determine the confidence intervals that have smaller widths and, therefore, are more precise. The limits of Bonferroni’s confidence intervals for all three pre-specified comparisons are generated by

```{r include=FALSE}
d21 <- mu2-mu1
d31 <- mu3-mu1
d32 <- mu3-mu2
se21 <- sqrt(mse*(1/n2+1/n1))
se31 <- sqrt(mse*(1/n3+1/n1))
se32 <- sqrt(mse*(1/n3+1/n2))
c(d21 - se21*qt(1-0.05/6,nt-3),d21 + se21*qt(1-0.05/6,nt-3))
c(d31 - se31*qt(1-0.05/6,nt-3),d31 + se31*qt(1-0.05/6,nt-3))
c(d32 - se32*qt(1-0.05/6,nt-3),d32 + se32*qt(1-0.05/6,nt-3))
```

Therefore, the Bonferroni confidence interval for all three pre-specified pairwise differences are mathematically represented as

$$\begin{aligned}
\mu_2-\mu_1:&\quad [10.27323,16.53337]\\
\mu_3-\mu_1:&\quad [1.367988,7.333412]\\
\mu_3-\mu_2:&\quad [-12.26625,-5.83895]
\end{aligned}$$

## Model Diagnostics and Sensitivity Analysis
```{r echo=FALSE}
par(mfrow=c(2,2))
plot(fit1)
```

The residuals v.s. fitted values plot shows no sign of unequal variance and the normal Q-Q plot shows that the distribution of the residuals is slightly light in both the right and left tail. In other words, this means that there is less probability in tails of this distribution of residuals compared to the normal distribution. However, we will conclude that the model’s assumptions hold. In addition, we provided the histogram of residuals below.

```{r echo=FALSE}
residuals=fit1$residuals;
hist(residuals)
```

For the next step in diagnostics, we will perform formal tests for evaluating equal variances.

```{r echo=FALSE}
leveneTest(math1~star1, data=data, center=mean)
```
Based on the Levene's Test, under significance level $\alpha=0.05$, the $p$-value for Levene's Test is 
$$p=0.05167969 >0.05$$
Hence, we can conclude that all groups have equal variance and the model assumption for homogeneity of variance holds.

For sensitivity analysis, it is necessary to add other factors in the data set to check whether the class size still has the effect on the mathscore. Out of this purpose, we introduce new factors and draw the boxplot of the math scores for all factor levels. We first consider the class size and gender.

```{r echo=FALSE}
Data<- STAR

#select the coulomns gender, ethnicity, birth, star1 ,math1 as a new data.frame star
star<- data.frame(Data['gender'], Data['ethnicity'], Data['birth'], Data['star1'], Data['math1']) 
star[,3]<-as.numeric( substr(star[,3], 1,4) ) # translate the entries of birth from character to number

# create the summary table
Sum<-summary(star)
Varbirth<- var(star[3], na.rm = TRUE)
Varmath<- var(star[5], na.rm = TRUE)
Summary<- data.frame(gender = c("male 6122","female 5456","","","",""), ethnicity = c("caucasion = 7193", "africa american = 4173", "asian = 32", "hispanic =21", "american indian = 14", "other = 20"), birth = c("median = 1980", "mean = 1980", "variance = 0.4", "minimum = 1977", "maximum = 1982","" ), class = c("small class 1925", "regular class = 2584", "regular class with teacher's aid = 2320", " "," "," " ) ,mathresult= c("mean = 530","variance = 1857.9", "minimum = 404", "maximum = 676", " "," ") )
 mathresult= c("mean = 530","variance = 1", "minimum = 404", "maximum = 676", " "," ")
colnames(Summary)<- c("gender", "ethnicity", "birth", "kind of class", "math socre")
colnames(star)<- c("gender", "ethnicity", "birth", "class", "score")
##Summary
##summary(Summary)
library(plyr)
star[,4]<- revalue(star[,4], c("regular"="regular class","small"="small class","regular+aide"=" regular class with teacher's aid"))
library(ggplot2)
star<- na.omit(star)
#ggplot(star, aes(x = score)) + geom_histogram(binwidth = 5)

#(star, aes(x=math1, fill=gender)) +
 # geom_histogram(aes(y=..density..), binwidth=25, alpha=.5, position="dodge")
#ggplot(star, aes(x=score, fill=ethnicity)) +
 # geom_histogram(aes(y=..density..), binwidth=25, alpha=.5, position="dodge")
#ggplot(star, aes(x=score, fill=class)) +
 # geom_histogram(aes(y=..density..), binwidth=25, alpha=.5, position="dodge")
#ggplot(star, aes(x=score)) + 
#  geom_histogram(aes(y=..density..),
 #                binwidth=25,
 #                colour="black", fill="white") +
#  geom_density(alpha=.2, fill="#FF6666")


#ggplot(data=star, aes(x=class,y=score))+geom_boxplot(aes(fill=score), fill = "white")+labs(title = "box plots of math score with respect to different kinds of class") +geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .05)


ggplot(data=star, aes(x=class,y=score))+geom_boxplot(aes(fill=gender))
```

Based on the boxplot, we can see that the mean value of the mathscore of the male students in three different class size are still different. And the mean value of the mathscore of the female students in three different class size are still different. Then we can say that the class size still has an effect on the math score of the 1st grade students.

Then we add birth as the third factor into consideration and get the boxplot of math score for all treatment.

```{r echo=FALSE}
star[,3]<- as.factor(star[,3])
ggplot(data=star, aes(x=class,y=score))+geom_boxplot(aes(fill=birth))
```

Based on the boxplot, we can see that in each birth year (from 1977 to 1981), the mean value of the mathscore of the male students in three different class size are still different. We can still say that the class size still has an effect on the math score of the 1st grade students.

# Discussion

In this project, we attempts to explore whether the class size for first-grade
students is associated with their math test scores.We drop the data which it
contains NA value. 

First, we extract the summary statistics for those two variables to get an overview of the dataset. Then we draw the histogram of Math Score variable and the boxplot of math score and class type. Those plots indicate that the distribution of the Math Score is nearly normally and average of math scores between different classes are not the same. The variance of math scores between those classes are nearly same. 

Next, by using one-way ANOVA model we can get that the
estimation of mean math score of regular class $\small \hat{\mu}_1$, small class $\small \hat{\mu}_2$ and regular class with a teacher aide $\small \hat{\mu}_3$. By using F test we can get the conclusion that under significant level of 0.05, there is a difference between the math score means across students in different class types. Because those math score mean of different classes are estimators, so we give its a 95% Confidence interval (C.I) to those estimators respectively. The description of each mean is more precisely. 

We also want to know which mean is different. We construct the 95%
confidence interval for all pairwise difference simultaneously. We use two
different ways to construct the C.I., Tukey method and Bonferroni correction. The result shows that under 95%
confidence, the two different methods indicate that all three pairs $\mu_{2}-\mu_{1}$, $\mu_{3}-\mu_{1}$ and $\mu_{3}-\mu_{2}$ are not equal to zero. 

The model diagnostics suggests that the assumptions of the one-way anova model are satisfied. And sensitivity analysis shows that there are still differences of the math score in different class size when we take more fatcors into considerations. 

Based on the analysis, we can see that class size can cause the different in math score of the 1st grade year students in each class size. The 1st grade students in small class has the highest math score, and the math scores of the students in regular class with teacher's aide are higher than that in regular class. These results make sense because students in small class have higher effeciency and teachers also can offer more guidance on math. 

# Session Information

```{r echo=FALSE}
print(sessionInfo(), local = FALSE)
```
The github link is :https://github.com/BillXu999/STA207_Group_12

```{r eval=FALSE, include=FALSE}
## load the required packages
library(MASS)
library(AER)

## load the data set and overview
data("STAR")
dim(STAR)
library(rmarkdown)
paged_table(STAR)
library(rmarkdown)
quan<- data.frame()
summ<- summary(STAR)
quan<-data.frame(readk = c(315,627,437), 
           read1 = c(404,651,521),
           read2 = c(468,732,584),
           read3 = c(499,775,615))
rownames(quan)<- c("Min", "Max", "Mean")
paged_table(quan)
quan<-data.frame(experiencek = c(0,27,9), 
           experience1 = c(0,42,12),
           experience2 = c(0,40,13),
           experience3 = c(0,38,14))
rownames(quan)<- c("Min", "Max", "Mean")
paged_table(quan)
quan<-data.frame(mathk = c(288,626,485), 
           math1 = c(404,676,531),
           math2 = c(441,721,581),
           math3 = c(487,774,618))
rownames(quan)<- c("Min", "Max", "Mean")
paged_table(quan)
summ<- summary(STAR)
quan<- data.frame(gender = summ[,1], ethnicity = summ[,2], birth = summ[,3] ,
                  stark = summ[,4],star1 = summ[,5],star2 = summ[,6],star3 = summ[,7] )
paged_table(quan)
summ<- summary(STAR)
quan<- data.frame(lunchk = summ[,16],lunch1 = summ[,17],lunch2 = summ[,18],lunch3 = summ[,19],
                  schoolk = summ[,20],school1 = summ[,21],school2 = summ[,22],school3 = summ[,23] )
paged_table(quan)
summ<- summary(STAR)
quan<- data.frame(degreek = summ[,24],degree1 = summ[,25],degree2 = summ[,26],degree3 = summ[,27],
                  ladderk = summ[,28],ladder1 = summ[,29],ladder2 = summ[,30],ladder3 = summ[,31] )
paged_table(quan)

summ<- summary(STAR)
quan<- data.frame(tethnicityk = summ[,32],tethnicity1 = summ[,33],tethnicity2 = summ[,34],tethnicity3 = summ[,35],
                  systemk = summ[,36],system1 = summ[,37],system2 = summ[,38],system3 = summ[,39],
                  schoolidk = summ[,40],schoolid1 = summ[,41],schoolid2 = summ[,42],schoolid3 = summ[,43] )
paged_table(quan)

## Drop the missing value
data <- STAR[!is.na(STAR[,5]),]
data <- STAR[!is.na(STAR[,13]),]
library(magrittr)
library(qwraps2)
library(ggplot2)
library(survival)
library(ggfortify)
set.seed(42)
options(qwraps2_markup = "markdown")

our_summary1 <-
  list("Math Scores" =
       list("min" = ~ min(data$math1),
            "max" = ~ max(data$math1),
            "mean (sd)" = ~ qwraps2::mean_sd(data$math1)),
       "Class Size" =
       list("Regular" = ~ qwraps2::n_perc0(data$star1 == "regular"),
            "Small"  = ~ qwraps2::n_perc0(data$star1 == "small"),
            "Regular with a teacher aside"= ~ qwraps2::n_perc0(data$star1 == "regular+aide"))
       )
summary_table(data, our_summary1)
n1<-2507
n2<-1868
n3<-2225
nt<-n1+n2+n3

## Discriptive analysis
hist(STAR$math1,main="Histogram of Math Score of the 1st Grade",xlab = "Math Score")
boxplot(math1~star1, data=data, xlab = "School Size", main= "Side-by-side box plots of the Math Score of the 1st Grade for the factor levels")
stripchart(math1~star1, vertical = TRUE, data = data, 
    method = "jitter", add = TRUE, pch = 20, col = 'blue')

## Fit the model
fit1 <- lm(math1 ~ star1, data = data)
summary(fit1)
anova(fit1)
mu1 <- 525.2744
mu2 <- 538.6777
mu3 <- 529.6251
mse <- 1828.941

## Hypothesis test and Interested Confidence Intervals
qf(0.95,2,6597)
## Standard Errors of the estimated mean values
c(42.77/sqrt(2507),42.77/sqrt(1868),42.77/sqrt(2225))
se1 <- 0.8542049
se2 <- 0.9895798
se3 <- 0.9067222
## Confidence Intervals for each factor level mean
c(mu1-se1*qt(1-0.05/2,nt-3),mu1+se1*qt(1-0.05/2,nt-3))
c(mu2-se2*qt(1-0.05/2,nt-3),mu2+se2*qt(1-0.05/2,nt-3))
c(mu3-se3*qt(1-0.05/2,nt-3),mu3+se3*qt(1-0.05/2,nt-3))
## Confidence Intervals for all factor level means
qt(1-0.05/6,6597)
c(525.2744-42.77*qt(1-0.05/6,6597)/sqrt(2507),525.2744+42.77*qt(1-0.05/6,6597)/sqrt(2507))
c(538.6777-42.77*qt(1-0.05/6,6597)/sqrt(1868),538.6777+42.77*qt(1-0.05/6,6597)/sqrt(1868))
c(529.6251-42.77*qt(1-0.05/6,6597)/sqrt(2225),529.6251+42.77*qt(1-0.05/6,6597)/sqrt(2225))
TukeyHSD(aov(fit1))
d21 <- mu2-mu1
d31 <- mu3-mu1
d32 <- mu3-mu2
se21 <- sqrt(mse*(1/n2+1/n1))
se31 <- sqrt(mse*(1/n3+1/n1))
se32 <- sqrt(mse*(1/n3+1/n2))
c(d21 - se21*qt(1-0.05/6,nt-3),d21 + se21*qt(1-0.05/6,nt-3))
c(d31 - se31*qt(1-0.05/6,nt-3),d31 + se31*qt(1-0.05/6,nt-3))
c(d32 - se32*qt(1-0.05/6,nt-3),d32 + se32*qt(1-0.05/6,nt-3))

## Model Diagnostics
par(mfrow=c(2,2))
plot(fit1)
residuals=fit1$residuals;
hist(residuals)
leveneTest(math1~star1, data=data, center=mean)

Data<- STAR

#select the coulomns gender, ethnicity, birth, star1 ,math1 as a new data.frame star
star<- data.frame(Data['gender'], Data['ethnicity'], Data['birth'], Data['star1'], Data['math1']) 
star[,3]<-as.numeric( substr(star[,3], 1,4) ) # translate the entries of birth from character to number

# create the summary table
Sum<-summary(star)
Varbirth<- var(star[3], na.rm = TRUE)
Varmath<- var(star[5], na.rm = TRUE)
Summary<- data.frame(gender = c("male 6122","female 5456","","","",""), ethnicity = c("caucasion = 7193", "africa american = 4173", "asian = 32", "hispanic =21", "american indian = 14", "other = 20"), birth = c("median = 1980", "mean = 1980", "variance = 0.4", "minimum = 1977", "maximum = 1982","" ), class = c("small class 1925", "regular class = 2584", "regular class with teacher's aid = 2320", " "," "," " ) ,mathresult= c("mean = 530","variance = 1857.9", "minimum = 404", "maximum = 676", " "," ") )
 mathresult= c("mean = 530","variance = 1", "minimum = 404", "maximum = 676", " "," ")
colnames(Summary)<- c("gender", "ethnicity", "birth", "kind of class", "math socre")
colnames(star)<- c("gender", "ethnicity", "birth", "class", "score")
##Summary
##summary(Summary)
library(plyr)
star[,4]<- revalue(star[,4], c("regular"="regular class","small"="small class","regular+aide"=" regular class with teacher's aid"))
library(ggplot2)
star<- na.omit(star)
#ggplot(star, aes(x = score)) + geom_histogram(binwidth = 5)

#(star, aes(x=math1, fill=gender)) +
 # geom_histogram(aes(y=..density..), binwidth=25, alpha=.5, position="dodge")
#ggplot(star, aes(x=score, fill=ethnicity)) +
 # geom_histogram(aes(y=..density..), binwidth=25, alpha=.5, position="dodge")
#ggplot(star, aes(x=score, fill=class)) +
 # geom_histogram(aes(y=..density..), binwidth=25, alpha=.5, position="dodge")
#ggplot(star, aes(x=score)) + 
#  geom_histogram(aes(y=..density..),
 #                binwidth=25,
 #                colour="black", fill="white") +
#  geom_density(alpha=.2, fill="#FF6666")


#ggplot(data=star, aes(x=class,y=score))+geom_boxplot(aes(fill=score), fill = "white")+labs(title = "box plots of math score with respect to different kinds of class") +geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .05)


ggplot(data=star, aes(x=class,y=score))+geom_boxplot(aes(fill=gender))
star[,3]<- as.factor(star[,3])
ggplot(data=star, aes(x=class,y=score))+geom_boxplot(aes(fill=birth))

```
