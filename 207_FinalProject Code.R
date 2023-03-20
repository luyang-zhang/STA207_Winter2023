pytsession=list()
session = list()
for(i in 1:5){
  session[[i]]=readRDS(paste('/Users/luyang/rstudio/STA 207/project/session',i,'.rds',sep=''))
  
}
library(dplyr)
library(ggplot2)
library(ROCR)
library(pander)
library(tidyverse)
library(gridExtra)
library(gplots)
library(DescTools)
library(car)
library(astsa)
library(MASS)


ID=1
t=0.4 # from Background 

n.trials=length(session[[ID]]$spks)
n.neurons=dim(session[[ID]]$spks[[1]])[1]

firingrate1=numeric(n.trials)

for(i in 1:n.trials){
  firingrate1[i]=sum(session[[ID]]$spks[[i]])/n.neurons/t
}

ID=2
t=0.4 # from Background 

n.trials=length(session[[ID]]$spks)
n.neurons=dim(session[[ID]]$spks[[1]])[1]

firingrate2=numeric(n.trials)

for(i in 1:n.trials){
  firingrate2[i]=sum(session[[ID]]$spks[[i]])/n.neurons/t
}

ID=3
t=0.4 # from Background 

n.trials=length(session[[ID]]$spks)
n.neurons=dim(session[[ID]]$spks[[1]])[1]

firingrate3=numeric(n.trials)

for(i in 1:n.trials){
  firingrate3[i]=sum(session[[ID]]$spks[[i]])/n.neurons/t
}

ID=4
t=0.4 # from Background 

n.trials=length(session[[ID]]$spks)
n.neurons=dim(session[[ID]]$spks[[1]])[1]

firingrate4=numeric(n.trials)

for(i in 1:n.trials){
  firingrate4[i]=sum(session[[ID]]$spks[[i]])/n.neurons/t
}

ID=5
t=0.4 # from Background 

n.trials=length(session[[ID]]$spks)
n.neurons=dim(session[[ID]]$spks[[1]])[1]

firingrate5=numeric(n.trials)

for(i in 1:n.trials){
  firingrate5[i]=sum(session[[ID]]$spks[[i]])/n.neurons/t
}

ssn1 <- data.frame(contrast_left = session[[1]]$contrast_left,
                   contrast_right = session[[1]]$contrast_right,
                   firing_rate = firingrate1,
                   feedback_type = session[[1]]$feedback_type,
                   session = 1)

ssn2 <- data.frame(contrast_left = session[[2]]$contrast_left,
                   contrast_right = session[[2]]$contrast_right,
                   firing_rate = firingrate2,
                   feedback_type = session[[2]]$feedback_type,
                   session = 2)

ssn3 <- data.frame(contrast_left = session[[3]]$contrast_left,
                   contrast_right = session[[3]]$contrast_right,
                   firing_rate = firingrate3,
                   feedback_type = session[[3]]$feedback_type,
                   session = 3)

ssn4 <- data.frame(contrast_left = session[[4]]$contrast_left,
                   contrast_right = session[[4]]$contrast_right,
                   firing_rate = firingrate4,
                   feedback_type = session[[4]]$feedback_type,
                   session = 4)

ssn5 <- data.frame(contrast_left = session[[5]]$contrast_left,
                   contrast_right = session[[5]]$contrast_right,
                   firing_rate = firingrate5,
                   feedback_type = session[[5]]$feedback_type,
                   session = 5)

df <- rbind(ssn1, ssn2, ssn3, ssn4, ssn5)
df$session <- as.factor(df$session)
df$feedback_type <- as.factor(df$feedback_type)
df$contrast_left <- as.factor(df$contrast_left)
df$contrast_right <- as.factor(df$contrast_right)

head(df) %>% pander(caption = '**Table 1**')

library(sqldf)
library(RSQLite)
sqldf('SELECT SESSION, COUNT(SESSION) AS "Number of Trials" FROM df GROUP BY SESSION') %>% pander(title = "**Table 2**")

library(ggplot2)
grid.arrange(ggplot(df) + geom_density(aes(x = firing_rate, fill = session), alpha = 0.6) + xlab('firing rate') + theme_bw() +xlab('Mean Firing Rate') + ylab('Density'),
             ggplot(df) + geom_density(aes(x = firing_rate, fill = feedback_type), alpha = 0.5)+ xlab('Mean Firing Rate') +ylab('Density') + theme_bw(),
             ncol = 2)

query1 <- sqldf('SELECT contrast_left, contrast_right, firing_rate
      FROM df GROUP BY contrast_left, contrast_right
      ORDER BY firing_rate DESC
      LIMIT 5')

query2 <- sqldf('SELECT contrast_left, contrast_right, firing_rate
      FROM df GROUP BY contrast_left, contrast_right
      ORDER BY firing_rate ASC
      LIMIT 5')

#pander(multicolumn = 2, multirow = 2, list(query1, query2), width = 'fit')

pander(query1, caption = "**Table 3(a)**")
pander(query2, caption = "**Table 3(b)**")


ggplot(df) + geom_boxplot(aes(x = firing_rate, y = session, fill= session)) + theme_bw() + xlab('Firing Rate')

df %>% group_by(contrast_right) %>% count() %>% pander(caption = "**Table 4(a)**")
df %>% group_by(contrast_left) %>% count() %>% pander(caption = "**Table 4(b)**")


library(lme4)
library(lmerTest)

model_full <- lmerTest::lmer(firing_rate ~ contrast_right*contrast_left + (1|session), data = df)

model_fixed_full <- lm(firing_rate ~ contrast_right*contrast_left, data = df)

model_add <- lmerTest::lmer(firing_rate ~ contrast_right + contrast_left + (1|session), data = df)

model_fixed <- lm(firing_rate ~ contrast_right+contrast_left, data = df)

coefs <- summary(model_full)$coef[,1]
coef_mat <- matrix(coefs, ncol = 4, byrow = T)
rownames(coef_mat) <- c('left 0', 'left 0.25', 'left 0.5', 'left 1')
colnames(coef_mat) <- c('right 0', 'right 0.25', 'right 0.5', 'right 1')


anova(model_full, model_add) %>% pander(caption = "**Table 6**") 
anova(model_full, model_fixed_full) %>% pander(caption = "**Table 7**")

par(mfrow = c(2,2))
#plot(model_add, which = 1)
plot(summary(model_full)$residual, ylab = 'residual', type = 'p', cex = 0.5);abline(h=0, col = 'red')
qqnorm(summary(model_full)$residual);qqline(summary(model_add)$residual)
hist(summary(model_full)$residual, xlab = 'residual', main = 'Histogram of residuals')

shapiro.test(resid(model_full)) %>% pander(caption = "**Table 8**")
leveneTest(firing_rate~contrast_right*contrast_left, data = df) %>% pander(caption = "**Table 9(a)**")
leveneTest(resid(model_full)~contrast_right*contrast_left, data = df)%>% pander(caption = "**Table 9(b)**")
boxplot(resid(model_full), horizontal = T)

df_train <- df[101:nrow(df), ]
df_test <- df[1:100, ]

library(MASS)
logit_model <- glm(feedback_type ~ ., family = binomial(), data=df_train)

summary(logit_model)$coef %>% pander(digits = 4, caption = "**Table 10**")

h0_logit_model <- glm(feedback_type ~ .-session, family = binomial(), data = df_train)
anova(h0_logit_model, logit_model, test = 'Chi') %>%pander(caption = "**Table 11**")

logit_pred <- ifelse(predict(logit_model, df_test, type = "response") <.5, "-1", "1")

logit_cm=table(Feedback=df_test$feedback_type,Prediction=logit_pred)
logit_cm
sum(diag(logit_cm))/sum(logit_cm)

logit_cm[2,2]/(logit_cm[2,2]+logit_cm[2,1]) # TPR
logit_cm[1,2]/(logit_cm[1,2]+logit_cm[1,1]) # FPR

res.P = residuals(logit_model, type = "pearson")
res.D = residuals(logit_model, type = "deviance")
boxplot(cbind(res.P, res.D), names = c("Pearson", "Deviance"))

par(mfrow=c(1,2))
plot(logit_model$fitted.values, res.P, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values')
lines(smooth.spline(logit_model$fitted.values, res.P, spar=0.9), col=2)
abline(h=0, lty=2, col='grey')
plot(logit_model$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', xlab='Fitted Values')
lines(smooth.spline(logit_model$fitted.values, res.D, spar=0.9), col=2)
abline(h=0, lty=2, col='grey')

leverage = hatvalues(logit_model)
plot(names(leverage), leverage, xlab="Index", type="h")
points(names(leverage), leverage, pch=16, cex=0.5)
p = length(coef(logit_model))
n = nrow(df_train)
infPts = which(leverage>2*p/n)
abline(h=2*p/n,col=2,lwd=2,lty=2)

cooks = cooks.distance(logit_model)
plot(cooks, ylab="Cook's Distance", pch=16, cex=0.2)
points(infPts, cooks[infPts], pch=17, cex=0.8, col='red') # influential points
susPts = as.numeric(names(sort(cooks[infPts], decreasing=TRUE))[1:3]) - 100
text(susPts, cooks[susPts], susPts, adj=c(-0.1,-0.1), cex=0.7, col='blue')

lda_fit <- lda(feedback_type~contrast_left + contrast_right + session + firing_rate, data = df_train)
lda_pred <- predict(lda_fit, dplyr::select(df_test, -feedback_type))


lda_cm=table(Feedback=df_test$feedback_type, Prediction=lda_pred$class)

sum(diag(lda_cm))/sum(lda_cm)
lda_cm

lda_cm[2,2]/(lda_cm[2,2]+lda_cm[2,1]) # TPR
lda_cm[1,2]/(lda_cm[1,2]+lda_cm[1,1]) # FPR

library(ROCR)

# Logistic 
logit_probs <- predict(logit_model, df_test, type = "response")
prediction_logit <- prediction(predictions = logit_probs, labels = df_test$feedback_type)

perf_logit <- performance(prediction.obj = prediction_logit, 'tpr', 'fpr')

# LDA 
prediction_lda <- prediction(predictions = lda_pred$posterior[, 2],
                             labels = df_test$feedback_type)

perf_lda <- performance(prediction.obj = prediction_lda, 'tpr', 'fpr') 
perf_lda 

plot(perf_lda, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
plot(perf_logit, col = 'red', add = T)