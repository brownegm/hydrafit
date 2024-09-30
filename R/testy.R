objective_function <- function(pars, model, psi, y_obs) {
  A <- pars[[1]]
  B <- pars[[2]]
  #C <- pars[[3]]

  # Model prediction
  y_pred <- model(A=A, B=B, psi)

  # Return sum of squared errors
  return(sum((y_obs - y_pred)^2))
}

objective_function(define_pars(input_df = input_df, "exp")[[3]], model=Exponential, psi=test_df_exp$psi,y_obs=test_df_exp$kl)
# Assuming you have psi and y_obs defined
y_obs <- test_df_exp$kl  # Your observed values here
psi <- rnorm(100, 3, 1)  # or your generated psi values

# Initial parameter guesses
initial_params <- c(A = define_pars(input_df = input_df, "exp")[[1]]["A"] , B = define_pars(input_df = input_df, "exp")[[1]]["B"])  # or any reasonable starting values

# Run optim
optim_result <- optim(
  par = initial_params,
  fn = objective_function,
  model = Exponential,
  psi = test_df_exp$psi,
  y_obs = y_obs,
  method = "L-BFGS-B",  # You can use other methods as well
  lower = c(0, 0.1),     # Set lower bounds for A and B
  upper = c(max(y_obs), 20)  # Set upper bounds for A and B
)

# Extract estimated parameters
estimated_params <- optim_result$par
print(estimated_params)
