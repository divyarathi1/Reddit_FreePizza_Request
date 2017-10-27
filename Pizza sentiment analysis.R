# Posivity and Sentiment 
install.packages("syuzhet")
library(syuzhet)

# create sentiment table 
pizza$requester_received_pizza <- ifelse( "TRUE", 1, 0)
pizzaSuccsess <- pizza$requester_received_pizza
sentiment <- get_nrc_sentiment (pizza$request_text)
anger <- sentiment[,"anger"]
anticipation <- sentiment[,"anticipation"]
disgust <- sentiment[,"disgust"]
fear <- sentiment[,"fear"]
joy <- sentiment[,"joy"]
sadness <- sentiment[,"sadness"]
surprise <- sentiment[,"surprise"]
trust <- sentiment[,"trust"]
negative <- sentiment[,"negative"]
positive <- sentiment[,"positive"]
sentimentData <- data.frame(pizzaSuccsess, anger, anticipation, disgust,
	fear, joy, sadness, surprise, trust, negative, positive)
head(sentimentData)

#sentiment logistic regresssion 
mod.sentiment <- lm(pizzaSuccsess~anger+anticipation+disgust+fear+
	joy+sadness+surprise+trust+negative+positive, data=sentimentData)
summary(mod.sentiment)

Call:
lm(formula = pizzaSuccsess ~ anger + anticipation + disgust + 
    fear + joy + sadness + surprise + trust + negative + positive, 
    data = sentimentData)

Residuals:
       Min         1Q     Median         3Q        Max 
-4.780e-15 -3.400e-16 -1.300e-16  9.000e-17  9.013e-13 

Coefficients:
               Estimate Std. Error    t value Pr(>|t|)    
(Intercept)   1.000e+00  2.462e-16  4.062e+15   <2e-16 ***
anger         1.769e-16  2.298e-16  7.700e-01    0.441    
anticipation -1.904e-17  1.465e-16 -1.300e-01    0.897    
disgust       1.993e-16  2.780e-16  7.170e-01    0.473    
fear          2.035e-16  2.162e-16  9.410e-01    0.347    
joy          -1.133e-16  1.905e-16 -5.950e-01    0.552    
sadness      -3.567e-16  2.226e-16 -1.603e+00    0.109    
surprise      1.782e-16  2.104e-16  8.470e-01    0.397    
trust         8.011e-18  1.391e-16  5.800e-02    0.954    
negative      1.464e-16  1.629e-16  8.990e-01    0.369    
positive     -4.547e-17  1.113e-16 -4.080e-01    0.683    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.199e-14 on 5660 degrees of freedom
Multiple R-squared:    0.5,     Adjusted R-squared:  0.4991 
F-statistic: 566.1 on 10 and 5660 DF,  p-value: < 2.2e-16
