#' Title
#' This function applies a Kalman filter to estimate the state of a linear system over time.
#' @param observations A numerical vector representing observations over time.
#' @param A A State transition coefficient
#' @param C  A Coefficient of observation
#' @param Q The variance of the process noise, which represents the uncertainty of the state prediction.
#' @param R Variance of the observational noise , which represents the uncertainty in the observed measurements.
#' @param x0 Initial condition estimate
#' @param P0 Initial Covariance of State Estimation
#'
#' @return A list
#' @export
#'
#' @examples
#' observations <- rnorm(50, mean = 100, sd = 10)
#' A <- 1
#' C <- 1
#' Q <- 1
#' R <- 1
#' x0 <- 100
#' P0 <- 10
#'
#' # Application du filtre de Kalman
#' result <- kalman_filter(observations, A, C, Q, R, x0, P0)
#'
#' # Affichage des résultats
#' print(result$x_hat)       # Estimations de l'état
#' print(result$residuals)   # Résidus
#' print(result$P)           # Covariance

kalman_filter <- function(observations, A, C, Q, R, x0, P0) {
  n <- length(observations)  # Nombre de pas de temps
  x_hat <- numeric(n)        # Estimation de l'état
  P <- numeric(n)            # Estimation de la covariance de l'état
  residuals <- numeric(n)    # Résidus (écarts entre observation et estimation)

  # Initialisation
  x_hat[1] <- x0
  P[1] <- P0

  for (t in 2:n) {
    # 1. Prédiction de l'état
    x_pred <- A * x_hat[t-1]  # État prédit à l'instant t
    P_pred <- A * P[t-1] * A + Q  # Covariance prédite

    # 2. Calcul du gain de Kalman
    K <- P_pred * C / (C * P_pred * C + R)  # Gain de Kalman

    # 3. Mise à jour de l'estimation de l'état
    x_hat[t] <- x_pred + K * (observations[t] - C * x_pred)

    # 4. Mise à jour de la covariance
    P[t] <- (1 - K * C) * P_pred

    # Calcul des résidus
    residuals[t] <- observations[t] - C * x_hat[t]
  }

  return(list(x_hat = x_hat, residuals = residuals, P = P))
}
