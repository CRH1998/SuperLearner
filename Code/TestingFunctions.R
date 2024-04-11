
data(BostonHousing)

X_boston <- data.matrix(BostonHousing[,colnames(BostonHousing) != 'medv'])
Y_boston <- data.matrix(BostonHousing[,colnames(BostonHousing) == 'medv'])


rr_boston <- ridge_regression(X_boston, Y_boston)
ols_boston <- ols_regression(X_boston, Y_boston)
adap_lasso_boston <- adaptive_lasso(X_boston, Y_boston, regression_method = 'adaptive_weights', 
                                    lambda_seq = 10^seq(5, -5, by = -.1), gamma_seq = seq(0.01,5,0.25))


rr_boston_pred <- predict(rr_boston, newx = X_boston)
ols_boston_pred <- predict(ols_boston, newdata = data.frame(X_boston))
adap_lasso_boston_pred <- predict(adap_lasso_boston$adaptive_lasso, newx = X_boston)

View(cbind(rr_boston_pred, ols_boston_pred, adap_lasso_boston_pred, Y_boston))


mean((rr_boston_pred - Y_boston)^2)
mean((ols_boston_pred - Y_boston)^2)
mean((adap_lasso_boston_pred - Y_boston)^2)


SuperLearner(Y = Y_boston, X = data.frame(X_boston), newX = X_boston, SL.library = c('SL.ridge', 'SL.adaptive.lasso', 'SL.xgboost', 'SL.lm'))





data(iris)

X_iris <- data.matrix(iris[,colnames(iris) != 'Species'])
Y_iris <- as.numeric((iris[,colnames(iris) == 'Species'] == 'versicolor'))

SuperLearner(Y = Y_iris, X = data.frame(X_iris), newX = X_iris, family = 'binomial', SL.library = c('SL.ridge', 'SL.adaptive.lasso', 'SL.xgboost', 'SL.lm'))


rr_iris <- ridge_regression(X_iris, Y_iris, family = 'binomial')
ols_iris <- ols_regression(X_iris, Y_iris, family = 'binomial')
adap_lasso_iris <- adaptive_lasso(X_iris, Y_iris, family = 'binomial', regression_method = 'adaptive_weights', 
                                  nfolds = 10, lambda_seq = 10^seq(5, -5, by = -.1), gamma_seq = seq(0.01,5,0.05))


rr_iris_pred <- predict(rr_iris, newx = X_iris, type = 'response') > 0.5
ols_iris_pred <- predict(ols_iris, newdata = data.frame(X_iris), type = 'response') > 0.5
adap_lasso_iris_pred <- predict(adap_lasso_iris$adaptive_lasso, newx = X_iris, type = 'response') > 0.5


View(cbind(rr_iris_pred, ols_iris_pred, adap_lasso_iris_pred, Y_iris))


sum(rr_iris_pred == Y_iris)/length(rr_iris_pred)
sum(ols_iris_pred == Y_iris)/length(ols_iris_pred)
sum(adap_lasso_iris_pred == Y_iris)/length(adap_lasso_iris_pred)




























X <- data.matrix(BostonHousing[, -which(names(BostonHousing) == 'medv')])
Y <- data.matrix(BostonHousing[, which(names(BostonHousing) == 'medv')])

ridge_regression(X, Y)

adap_lasso_output <- adaptive_lasso(X, Y, regression_method = 'ridge')

adap_lasso <- adap_lasso_output$adaptive_lasso

pred <- predict(adap_lasso, newx = cbind(1, X), type = "response")


function (Y, X, newX, family, obsWeights, model = TRUE, ...) 
{
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }
  fit <- stats::lm(Y ~ ., data = X, weights = obsWeights, model = model)
  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }
  pred <- predict(fit, newdata = newX, type = "response")
  if (family$family == "binomial") {
    pred = pmin(pmax(pred, 0), 1)
  }
  fit <- list(object = fit, family = family)
  class(fit) <- "SL.lm"
  out <- list(pred = pred, fit = fit)
  return(out)
}






best_adaptive_weights <- adap_lasso$best_adaptive_weights
own_lasso_beta <- adap_lasso$adaptive_lasso$beta

initial_regression <- ridge_regression(BostonHousing, 'medv', family = gaussian())
initial_regression_coefs <- initial_regression$beta

X <- data.matrix(cbind(1, BostonHousing[, -which(names(BostonHousing) == 'medv')])) #cbind(1,...) to account for intercept
y <- data.matrix(BostonHousing[, which(names(BostonHousing) == 'medv')])


test_lasso <- gcdnet(X, y,lambda2 = 0, lambda = adap_lasso$best_lambda, method = 'ls', pf = best_adaptive_weights)
test_lasso2 <- glmnet(X, y, alpha = 1, lambda = adap_lasso$best_lambda, family = gaussian(), penalty.factor = best_adaptive_weights)


