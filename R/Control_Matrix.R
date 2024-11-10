#' Control Matrix
#'
#' @param nb_states
#'
#' @return a matrix representing the control matrix.
#' @export
#'
#' @examples
#' \dontrun{
#' lire_matrice_controle(3)
#' }


read_control_matrix <- function(nb_states) {
  repeat {
    lignes <- as.integer(readline(prompt = "Entrez le nombre de lignes de la matrice de contrôle B: "))
    colonnes <- as.integer(readline(prompt = "Entrez le nombre de colonnes de la matrice de contrôle B: "))

    if (lignes == nb_states) {
      break
    } else {
      cat("Le nombre de lignes de la matrice de contrôle doit être égal au nombre de lignes de la matrice d'état. Veuillez réessayer.\n")
    }
  }

  cat("Entrez les éléments de la matrice de contrôle B ligne par ligne, séparés par des espaces:\n")
  matrice <- matrix(NA, lignes, colonnes)

  for (i in 1:lignes) {
    ligne <- readline(prompt = paste("Ligne", i, ": "))
    matrice[i, ] <- as.numeric(unlist(strsplit(ligne, " ")))
  }

  matrice
}
