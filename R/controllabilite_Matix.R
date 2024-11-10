#' Calculate the Controllability Matrix
#'
#' This function calculates the controllability matrix for the given system.
#'
#' @param A The state matrix.
#' @param B The Control Matrix
#' @return A controllability matrix.
#' @export
#' @examples
#' \dontrun{
#' A <- matrix(c(0, 1, -2, -3), nrow = 2, byrow = TRUE)
#' B <- matrix(c(0, 1), nrow = 2, byrow = TRUE)
#' calculer_matrice_controllabilite(A, B)
#' }
calculate_matrix_controllability <- function(A, B) {
  n <- nrow(A)
  Control <- B
  puiss <- diag(n) # Initialisation de la matrice identité
  for (i in 1:(n-1)) {
    puiss <- puiss %*% A
    Control <- cbind(C, puiss %*% B)
  }

   Control
}
