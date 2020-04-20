
### Functions ###
# P-value of linear model object
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Binary factorization of the labels
factorize_df <- function(df_, binary_columns=c()){
  for (col_name in binary_columns){
    df_[col_name] = factor(df_[[col_name]], levels=c(0, 1), labels=c('No', 'Yes'))
  }
  return (df_)
}
  
# Single variable linear model
linear_model_unadjusted <- function (cohort, response, variable, plot_variable=FALSE) {
  
  # Linear model
  out.lm = lm(as.formula(paste(response, paste(variable),  sep = " ~ ")), data=cohort)
  confint(out.lm)
  
  # Confidence interval
  newx = seq(min(cohort[variable]),max(cohort[variable]),by = 0.05)
  newdata = data.frame(eval(parse(text=paste(paste(variable, 'newx', sep='=')))))
  conf_interval <- predict(out.lm, newdata=newdata, interval="confidence", level=0.95)
  
  # Plotting
  if (plot_variable){
    jpeg(paste('C:\\Data\\Vaccines\\Plots\\LinearRegression\\', paste(response, '_', variable, '.jpg', sep=''), sep=''),
         width = 1024, height = 1024, quality=300, pointsize=25)
    
    plot(cohort[[variable]], cohort[[response]], col='blue', pch = 1, xlab=variable, ylab=response)
    
    lines(newx, conf_interval[,1], col='black', lty=1)
    matlines(newx, conf_interval[,2:3], col = "black", lty=2)
    
    legend("bottomright", legend=c('Regression with 95%CI'),
           col=c('black'), pch=c(150), cex=1.2)
    
    grid()
    title('Linear regression with 95% CI, full data set')
    dev.off()
  }
  
  return(out.lm)
}
### End Functions ###

### Script ###
# Read dataset
df = read.csv('C:\\Data\\Vaccines\\Data\\train_set.csv', header=TRUE)

# Clean data
# TODO
#
#

# Ordinal variables
df$h1n1_concern = factor(df$h1n1_concern, ordered=TRUE, labels=c('NotAtAll', 'NotVery', 'Somewhat', 'Very'))
df$h1n1_knowledge = factor(df$h1n1_knowledge, ordered=TRUE, labels=c('None', 'Little', 'Lots'))
df$opinion_h1n1_vacc_effective = factor(df$opinion_h1n1_vacc_effective, ordered=TRUE, 
                                        labels=c('NotAtAll', 'NotVery', 'DontKnow', 'Somewhat', 'Very'))
df$opinion_seas_vacc_effective = factor(df$opinion_seas_vacc_effective, ordered=TRUE, 
                                        labels=c('NotAtAll', 'NotVery', 'DontKnow', 'Somewhat', 'Very'))
df$opinion_h1n1_sick_from_vacc = factor(df$opinion_h1n1_sick_from_vacc, ordered=TRUE, 
                                        labels=c('NotAtAll', 'NotVery', 'DontKnow', 'Somewhat', 'Very'))
df$opinion_seas_sick_from_vacc = factor(df$opinion_seas_sick_from_vacc, ordered=TRUE, 
                                        labels=c('NotAtAll', 'NotVery', 'DontKnow', 'Somewhat', 'Very'))
df$opinion_h1n1_risk = factor(df$opinion_h1n1_risk, ordered=TRUE, 
                                        labels=c('VeryLow', 'Low', 'DontKnow', 'High', 'VeryHigh'))
df$opinion_seas_risk = factor(df$opinion_seas_risk, ordered=TRUE, 
                                        labels=c('VeryLow', 'Low', 'DontKnow', 'High', 'VeryHigh'))


# Binary variables
binary_variables = c('behavioral_antiviral_meds', 'behavioral_avoidance', 'behavioral_face_mask', 'behavioral_wash_hands',
                     'behavioral_large_gatherings', 'behavioral_outside_home', 'behavioral_touch_face', 'doctor_recc_h1n1',
                     'doctor_recc_seasonal', 'chronic_med_condition', 'child_under_6_months', 'health_worker',
                     'health_insurance')

df = factorize_df(df, binary_variables)

### End Script ###


# TODO: Encode the environmental and biometric data (sex, education, employment)
# One-hot encoding?
# Single column?
# 
# TODO: Clean the data
#
# TODO: plot distributions (boxplots)
