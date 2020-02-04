

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
SASSI Example

```{r}
gender_sample = c(1,0)
data_source_sample = c(0,1,2,3,4)
diag_sample = c(1,0)
test_sample = c(1,0)

develop_dat = data.frame(gender = sample(gender_sample,50, replace = TRUE), data_source = sample(data_source_sample, 50, replace = TRUE), diag = sample(diag_sample, 50, replace = TRUE), test = sample(test_sample, 50, replace = TRUE)) 

cross_val_dat = data.frame(gender = sample(gender_sample,50, replace = TRUE), data_source = sample(data_source_sample, 50, replace = TRUE), diag = sample(diag_sample, 50, replace = TRUE), test = sample(test_sample, 50, replace = TRUE)) 

norm_dat = data.frame(gender = sample(gender_sample,50, replace = TRUE), data_source = sample(data_source_sample, 50, replace = TRUE), diag = sample(diag_sample, 50, replace = TRUE), test = sample(test_sample, 50, replace = TRUE)) 

clincial_dat = data.frame(gender = sample(gender_sample,50, replace = TRUE), data_source = sample(data_source_sample, 50, replace = TRUE), diag = sample(diag_sample, 50, replace = TRUE), test = sample(test_sample, 50, replace = TRUE)) 

stab_dat = data.frame(gender = sample(gender_sample,50, replace = TRUE), data_source = sample(data_source_sample, 50, replace = TRUE), diag = sample(diag_sample, 50, replace = TRUE), test = sample(test_sample, 50, replace = TRUE)) 
```
Table 1

Analyses of the frequency of cases in the development versus the cross-validation sample as a function of type of treatment setting, diagnostic categories, or demographic variables 

The number of cases did not differ between the samples on the treatment setting diagnostic categories or demographic variables
```{r}
table(develop_dat$gender, cross_val_dat$gender)
test = chisq.test(develop_dat$gender, cross_val_dat$gender)
test$p.value
test = fisher.test(develop_dat$data_source, cross_val_dat$data_source)


develop_dat_demos = develop_dat[,1:2]
cross_val_dat_demos = cross_val_dat[,1:2]

p_value_develop_cross_val_results = list()

for(i in 1:length(develop_dat_demos)){
  p_value_develop_cross_val_results[[i]] = fisher.test(develop_dat_demos[[i]], cross_val_dat_demos[[i]])
  p_value_develop_cross_val_results[[i]] = p_value_develop_cross_val_results[[i]]$p.value
}
p_value_develop_cross_val_results = data.frame(p_value_develop_cross_val_results)
names(p_value_develop_cross_val_results) = names(develop_dat_demos)
p_value_develop_cross_val_results
```
Table 2
Get omegas and stability (test re-test) with two data sets combined 
```{r}

test_sample = c(0,1,2,3,4,5)
set.seed(1234)
sassi_overall = data.frame(a = sample(x = test_sample, 50, replace = TRUE, prob = c(.5, .3, rep(.05, 4))), b = sample(x = test_sample, 50, replace = TRUE, prob = c(.5, .3, rep(.05, 4)))
, c = sample(x = test_sample, 50, replace = TRUE, prob = c(.5, .3, rep(.05, 4)))
)
sassi_overall

face_valid = sassi_overall

asses_list = list(sassi_overall, face_valid)
library(MBESS)
ci.reliability(face_valid)
omega_results = list()
for(i in 1:length(asses_list)){
  omega_results[[i]] = ci.reliability(asses_list[[i]])
  omega_results[[i]] = omega_results[[i]][c(1,3,4)]
}
omega_results = t(data.frame(omega_results))
omega_results = data.frame(omega_results)
omega_results$vars = rep(c("sassi_overall", "face_valid"), each = 3)
omega_results
```
Tables 3 through 5
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2636062/#!po=19.2308
Sens = ability to predictve a true positive (percentage of people with disease that test caught)
Specify = abiliyt to predict a true negative (percentage of people not with disease that test caught)
Positive = (number of positive test that are true positives)
Negative = number of negative tests are the true negatives

Levels for refernece are opposite, because true value is 1 not 0.
https://www.statmethods.net/advstats/discriminant.html

```{r}
library(caret)

gender_sample = c(1,0)
data_source_sample = c(0,1,2,3,4)
diag_sample = c(1,0)
test_sample = c(1,0)


develop_dat = data.frame(gender = sample(gender_sample,50, replace = TRUE), data_source = sample(data_source_sample, 50, replace = TRUE), diag = sample(diag_sample, 50, replace = TRUE), test = sample(test_sample, 50, replace = TRUE)) 

cross_val_dat = data.frame(gender = sample(gender_sample,50, replace = TRUE), data_source = sample(data_source_sample, 50, replace = TRUE), diag = sample(diag_sample, 50, replace = TRUE), test = sample(test_sample, 50, replace = TRUE)) 

develop_dat$diag = as.factor(develop_dat$diag)
develop_dat$test = as.factor(develop_dat$test)
library(prettyR)
test = table(develop_dat$test, develop_dat$diag)
test
col_margins =  rbind(sum(test[1,]), sum(test[2,]))
col_margins
row_margins = c(sum(test[,1]), sum(test[,2]))
row_margins
phi(test)
cor(as.numeric(develop_dat$test), as.numeric(develop_dat$diag))

sensitivity(develop_dat$test, develop_dat$diag, positive = levels(develop_dat$diag)[2])
table(develop_dat$test, develop_dat$diag)
10/(25)

confusionMatrix(develop_dat$test, develop_dat$diag, positive = "1")
library(MASS)

dis_dat = data.frame(sassi_overall, diag_true =develop_dat$diag) 
fit <- lda(diag_true ~ a + b + c, data=dis_dat, CV=TRUE)
fit$class
correct = table(develop_dat$diag, fit$class)
correct
sum(diag(prop.table(correct)))



```
Table 8 
Table 8 presents overall accuracy rates of SASSI-4 decision-rule classifications as a function of scores on the DEF scale
Just get confusionMatrix and accuracy for the different DEF scores


Table 10
Assuming decision rule accuracy means test positive divided by total positive


Paper extra stuff
Test of missingness
```{r}
cor.test(as.numeric(develop_dat$test), as.numeric(develop_dat$diag))
confusionMatrix(develop_dat$test, develop_dat$diag, positive = "1")
library(cvAUC)
data(ROCR.simple)
ROCR.simple
library(pROC)
test_roc =  roc(develop_dat$test, develop_dat$diag)
test_auc = ci.auc(test_roc)
test_auc
```
Just for fun how to find the optimal cut point 
https://cran.r-project.org/web/packages/cutpointr/vignettes/cutpointr.html
```{r}
library(cutpointr)
data(suicide)
head(suicide)
library(prettyR)
describe.factor(suicide$dsi)
cp <- cutpointr(suicide, dsi, suicide, method = maximize_metric, metric = sum_sens_spec)
summary(cp)
plot(cp)
```
Is the cut point different by gender
```{r}
opt_cut <- cutpointr(suicide, dsi, suicide, gender, method = minimize_metric, metric = misclassification_cost, cost_fp = 1, cost_fn = 10)
summary(opt_cut)
```








