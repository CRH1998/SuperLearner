
data(BostonHousing)

X_boston <- data.matrix(BostonHousing[,colnames(BostonHousing) != 'medv'])
Y_boston <- data.matrix(BostonHousing[,colnames(BostonHousing) == 'medv'])

rr_boston <- ridge_regression(X_boston, Y_boston)
ols_boston <- ols_regression(X_boston, Y_boston)
adap_lasso_boston <- adaptive_lasso(X_boston, Y_boston, regression_method = 'ridge')


rr_boston_pred <- predict(rr_boston, newx = X_boston)
ols_boston_pred <- predict(ols_boston, newdata = data.frame(X_boston))
adap_lasso_boston_pred <- predict(adap_lasso_boston$adaptive_lasso, newx = X_boston)

View(cbind(rr_boston_pred, ols_boston_pred, adap_lasso_boston_pred, Y_boston))


mean((rr_boston_pred - Y_boston)^2)
mean((ols_boston_pred - Y_boston)^2)
mean((adap_lasso_boston_pred - Y_boston)^2)




data("Titanic")
Titanic <- data.frame(Titanic)

X_titanic <- data.matrix(Titanic[,colnames(Titanic) != 'Survived'])
Y_titanic <- Titanic[,colnames(Titanic) == 'Survived']






rr_titanic <- ridge_regression(X_titanic, Y_titanic, family = 'binomial')
ols_titanic <- ols_regression(X_titanic, Y_titanic, family = 'binomial')
adap_lasso_titanic <- adaptive_lasso(X_titanic, Y_titanic, family = 'binomial', regression_method = 'ridge', nfolds = 10, gamma_seq = c(0.5), lambda_seq = c(0.00001, 0.000011))

adap_lasso_titanic$adaptive_lasso$beta

rr_titanic_pred <- predict(rr_titanic, newx = X_titanic, type = 'response') > 0.5
ols_titanic_pred <- predict(ols_titanic, newdata = data.frame(X_titanic), type = 'response') > 0.5
adap_lasso_titanic_pred <- predict(adap_lasso_titanic$adaptive_lasso, newx = X_titanic, type = 'response') > 0.5

gl_ols <- glm(Y_titanic ~ X_titanic - 1, family = binomial())


































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


