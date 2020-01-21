output_coefficients <- function(fit_obj, dat, response_idx){
  stopifnot(any(c("glm", "cv.glmnet") %in% class(fit_obj)))
  
  if("glm" %in% class(fit_obj)){
    coef_vec <- coef(fit_obj)
  } else {
    coef_vec <- as.numeric(stats::coef(fit_obj, s = "lambda.1se"))[-1]
    names(coef_vec) <- colnames(dat)[-response_idx]
  }
  
  coef_vec
}

output_predictions <- function(fit_obj, dat, response_idx){
  stopifnot(any(c("glm", "cv.glmnet") %in% class(fit_obj)))
  stopifnot(is.data.frame(dat))
  stopifnot(response_idx %% 1 == 0, response_idx > 0, response_idx <= ncol(dat))
  
  if("glm" %in% class(fit_obj)){
    
    pred_vec <-  stats::predict(fit_obj, newx = dat, type = "response")
    pred_vec <- as.numeric(pred_vec >= 0.5)
      
  } else {
     
    pred_vec <- as.numeric(glmnet:::predict.cv.glmnet(fit_obj, newx = as.matrix(dat[,-response_idx]), 
                                           s = "lambda.1se", type = "class"))
  }
  
  pred_vec
}


