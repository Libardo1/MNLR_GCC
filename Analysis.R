# load the data prepared in Data_Preparation.R:

load('~/MNLR_GCC/Data/Filtered_Data.RData')

# load the packages we will use:

library(dplyr)

# read about `dplyr` here: <http://r4ds.had.co.nz/transform.html>

library(glmnet)

# read about `glmnet` here: <https://cran.r-project.org/web/packages/glmnet/vignettes/glmnet_beta.html>

library(caret)

# read about `caret` here: <http://topepo.github.io/caret/index.html>

# we'll use `caret` with parallel processing provided by `doMC`

library(doMC)

registerDoMC(cores = 6)

# create cross validation folds that have proportions of the different crops as balanced as possible
# (recall the rarest crop classes we have included have > 1e3 observations each)

# set the number of cross validation folds
n.folds <- 10

cv.k.f <- createFolds(y = Data$crop.c, k = n.folds, returnTrain = TRUE)

class(cv.k.f)

# e.g.

Data %>%
  slice(cv.k.f[[1]]) %>%
    group_by(crop.c) %>%
      summarise(count = n())

Data %>%
  slice(cv.k.f[[10]]) %>%
    group_by(crop.c) %>%
      summarise(count = n())

# at least all crop classes have > 1000 observations in the training set

# do test run of glmnet with alpha = 1 and set lambda sequence length to get the glmnet recommended lambda sequence

system.time(m.test <- glmnet(x = as.matrix(select(.data = Data, -crop.c)), y = Data$crop.c, family = 'multinomial', alpha = 1, nlambda = 1e3)) # 15 mins

summary(m.test$lambda) # min = 3.399e-05, max = 2.775e-01 

# set up for using `caret` to tune to glmnet model using the cross validation folds create above

trCtrl <- trainControl(method = 'cv',
                       index = cv.k.f)

# create the grid of tunnign parameters to trial:

seq.leng <- 10

tunning.grid <- expand.grid(alpha = seq(from = 0, to = 1, length.out = seq.leng),
                            lambda = seq(from = min(m.test$lambda), to = max(m.test$lambda), length.out = seq.leng))

# conduct the cross validation with `caret`

system.time(
  train.obj <- train(x = as.matrix(select(.data = Data, -crop.c)),
                  y = Data$crop.c,
                  trControl = trCtrl,
                  method = 'glmnet',
                  tuneGrid = tunning.grid)
) # 3 hrs 43 mins

# save.image(file = '~/MNLR_GCC/Data/Fit_10FCV.RData')

load('~/MNLR_GCC/Data/Fit_10FCV.RData')

train.obj

# 'The final values used for the model were alpha = 1 and lambda = 3.39855e-05. '

# use the tunned model to predict the full response vector:

y.hat <- predict.train(object = train.obj, newdata = as.matrix(select(.data = Data, -crop.c)))

summary(y.hat)

# calculate overall predictive accuracy:

# number of correct predictions

summary(y.hat == Data$crop.c)

# % of correct predictions

100*length(y.hat[y.hat == Data$crop.c])/length(Data$crop.c) # 75.7% of pixels correctly classified

# distribution of correct predictions:

Pred.Objs <- data.frame(Prediction = y.hat, Observation = Data$crop.c)

class(Pred.Objs$Prediction)

class(Pred.Objs$Observation)

length(levels(Pred.Objs$Prediction)) == length(levels(Pred.Objs$Observation))

summary(levels(Pred.Objs$Prediction) %in% levels(Pred.Objs$Observation))

# overall % of pixels predicted correctly classified:

100*sum(Pred.Objs$Observation == Pred.Objs$Prediction)/nrow(Pred.Objs)

Pred.Objs %>%
  mutate(Correct = (Prediction == Observation)) %>%
    summarise(100*sum(Correct)/n())

# % of pixels in each crop category predicted correctly:

Pred.Objs %>%
  mutate(Correct = (Prediction == Observation)) %>%
    group_by(Observation) %>%
      summarise(Percentage.Correctly.Predicted = 100*sum(Correct)/n()) %>%
        arrange(desc(Percentage.Correctly.Predicted)) -> Pc.Cor.by.Crop

Pc.Cor.by.Crop

colnames(Pc.Cor.by.Crop) <- c('crop.c', 'Percentage.Correctly.Predicted')

# compare to:

Data %>%
  group_by(crop.c) %>%
    summarise(n = n()) %>%
      arrange(desc(n)) -> N.by.Crop

N.by.Crop

left_join(x = N.by.Crop, y = Pc.Cor.by.Crop, by = 'crop.c') %>%
  mutate( Percentage.of.Observations = 100*n/nrow(Data)) -> Accuracy.by.Category

View(Accuracy.by.Category)

Accuracy.by.Category.Print <- Accuracy.by.Category

colnames(Accuracy.by.Category.Print) <- c('Ground.Cover', 'N.Pixels', 'Pct.Correct', 'Pct.Pixels')

Accuracy.by.Category.Print

library(knitr)

select(.data = Accuracy.by.Category.Print, Ground.Cover, N.Pixels, Pct.Pixels, Pct.Correct) %>%
  kable(x = ., format = 'markdown', caption = 'Predictions from Elastic Net Regularized Multinomial Logistic Regression tuned by 10 fold cross validation with a 10 by 10 tuning parameter grid. Tuning parameter estimated: alpha = 1 and lambda = 3.39855e-05.', digits = 2)

# but see also

postResample(pred = y.hat, obs = Data$crop.c)


#
length(levels(Data$crop.c))


png(filename = '~/Pictures/Crop_EN_MN_Coef_10FCV.png', width = 800, height = 1200)
par(mfcol = c(4,3))
plot(train.obj$finalModel, label = TRUE, cex.lab = 1.5)
dev.off()
# tiny numbers at the end of lines of column numbers of the associated covariate in the design matrix 

train.obj

coef.s <- coef(object = train.obj$finalModel, s = 3.39855e-05)

names(coef.s)

Coef.df <- data.frame(Term = row.names(as.matrix(coef.s[[1]])))

data.frame(as.matrix(coef.s[[1]]))

for(i in 1:length(coef.s)){
  Out <- data.frame(Term = row.names(as.matrix(coef.s[[i]])), as.matrix(coef.s[[i]]))
  colnames(Out) <- c('Term', paste(names(coef.s)[i]))
  Coef.df <- left_join(x = Coef.df, y = Out, by = 'Term')
}



kable(x = Coef.df, digits = 2)

library(tidyr)

Coef.df.G <- gather(data = Coef.df, -Term, key = 'ID', value = 'coefficient')

coef.p <- ggplot(aes(x = Term, y = coefficient), data = Coef.df.G) + theme_bw()

coef.p <- coef.p+ geom_point() + facet_wrap(facets = 'ID', nrow = 4) + theme_update(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

coef.p

ggsave(filename = '~/Pictures/Crop_EN_MN_Coef_Ests_10FCV.png', plot = coef.p, units = 'in', width = 6, height = 10, dpi = 300)


# the next step would be to examine the performance on some data held out from the cross validation scheme entirely...
