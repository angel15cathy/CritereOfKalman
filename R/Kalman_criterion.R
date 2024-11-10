#' Calculate and verify controllability
#'
#' This function reads the state and control matrices, calculates the controllability matrix, and checks whether the system is controllable.
#'
#' @return A message indicating whether the system is controllable.
#' @export
#' @examples
#' \dontrun{
#' kalman_criterion()
#' }
kalman_criterion <- function() {

  A <- read_state_matrix()

  B <- read_control_matrix(nrow(A))

  Control <- calculate_matrix_controllability(A, B)

  # Check controllability
  is_controllable <- verify_controllability(A,Control)

  if (is_controllable) {
    cat("le systeme est optimal.\n")
  } else {
    cat("le systeme n'est pas optimal.\n")
  }
}
