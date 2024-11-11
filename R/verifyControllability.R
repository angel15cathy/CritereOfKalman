#' Verify system controllability
#'
#' This function checks whether the system is controllable.
#'
#' @param A The state matrix.
#' @param matrice The controllability matrix.
#' @return TRUE if the system is controllable, otherwise FALSE.
#' @export
#' @examples
#' \dontrun{
#' A <- matrix(c(0, 1, -2, -3), nrow = 2, byrow = TRUE)
#' C <- matrix(c(0, 1, 1, -3), nrow = 2, byrow = TRUE)
#' verifier_controllabilite(A, C)
#' }
verify_controllability <- function(A, matrice) {
  rang_A <- qr(A)$rank
  rang_C <- qr(matrice)$rank
  if (rang_C == rang_A) {
    cat("Le système est contrôlable.\n")
    return(TRUE)
  } else {
    cat("Le système n'est pas contrôlable.\n")
    return(FALSE)
  }
}
