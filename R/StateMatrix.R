#' State Matrix
#'
#' @return A matrix representing the states of the system.
#' @export
#'
#' @examples
#' \dontrun{
#' lire_matrice_etat()
#' }


read_state_matrix <- function() {
  # Ask for the number of states
  nb_states <- as.integer(readline(prompt = "Entrez le nombre d'états dans le système: "))

  # Read the names of the states
  name_states <- character(nb_etats)
  for (i in 1:nb_states) {
    name_states <- readline(prompt = paste("Entrez le nom de l'état", i, ": "))
  }

  # Read the dimension of the states
  repeat {
    dimensions <- integer(nb_states)
    for (i in 1:nb_etats) {
      dimensions[i] <- as.integer(readline(prompt = paste("Entrez la dimension de l'état", noms_etats[i], ": ")))
    }

    # Check if all dimensions are equal
    if (length(unique(dimensions)) == 1) {
      dimension_state <- dimensions[1]
      break
    } else {
      cat("Les dimensions de tous les états doivent être les mêmes. Veuillez réessayer.\n")
    }
  }

  # Read the values for each state
  state_matrix <- matrix(NA, nb_states, dimension_state)
  for (i in 1:nb_state) {
    cat(paste("Entrez les valeurs de l'état", name_states[i], "séparées par des espaces:\n"))
    values <- as.numeric(unlist(strsplit(readline(), " ")))
    while (length(values) != dimension_state) {
      cat("Le nombre de valeurs doit correspondre à la dimension de l'état. Veuillez réessayer.\n")
      values <- as.numeric(unlist(strsplit(readline(), " ")))
    }
   state_matrix[i, ] <- values
  }

  state_matrix
}
