obj_fun_part2 <- function(x, target_np, target_ap) {
  np <- normalised_power(x)
  ap <- average_power(x)
  (np - target_np)^2 + (ap - target_ap)^2
}

obj_fun_part1 <- function(x, target_ap) {
  ap <- average_power(x)
  (ap - target_ap)^2
}

#' simulator
#' 
#' @param power_init dbl Initial power values
#' @param lossfunc dbl Loss function
#' @param n_seconds int Number of seconds to simulate
#' @param max_power dbl Maximum power
#' @param ... Other arguments to pass to optim, e.g. target_np, target_ap
#' 
#' @return optim output
simulator <- function(power_init, lossfunc, n_seconds, max_power, ...) {
  sim_result <- optim(
    par = power_init,
    fn = lossfunc,
    method = "L-BFGS-B",
    lower = rep(1, n_seconds),
    upper = rep(max_power, n_seconds),
    ...
  )
  
  # check that the optimizer converged
  if (sim_result$convergence != 0) {
    stop("Optimizer did not converge. Is the target NP achievable with the specified max power and NP?")
  }

  return(sim_result)
}

#' simulate_power
#'
#' @description Simulate power data for a given NP and AP
#'
#' @param n_seconds int Number of seconds to simulate
#' @param target_np dbl Normalised power
#' @param target_ap dbl Average power
#'
#' @return Vector of power values
#' @export
simulate_power <- function(n_seconds, target_ap, target_np, max_power = 1000) {
  # Create some initial values using a beta distribution
  power_init <- rbeta(n_seconds, 3, 15) * max_power

  # Run the initial value optimizer for AP
  run1_power <- simulator(
    power_init = power_init,
    lossfunc = obj_fun_part1,
    n_seconds = n_seconds,
    max_power = max_power,
    target_ap = target_ap
  )
  # Run the initial value optimizer for AP
  run2_power <- simulator(
    power_init = run1_power$par,
    lossfunc = obj_fun_part2,
    n_seconds = n_seconds,
    max_power = max_power,
    target_ap = target_ap,
    target_np = target_np
  )

  # Extract the optimized power values
  run2_power$par
}
