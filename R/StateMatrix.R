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
  # Demande le nombre d'états
  nb_states <- as.integer(readline(prompt = "Entrez le nombre d'états dans le système: "))

  # Lit les noms des états
  name_states <- character(nb_states)
  for (i in 1:nb_states) {
    name_states[i] <- readline(prompt = paste("Entrez le nom de l'état", i, ": "))
  }

  # Lit la dimension des états
  repeat {
    dimensions <- integer(nb_states)
    for (i in 1:nb_states) {
      dimensions[i] <- as.integer(readline(prompt = paste("Entrez la dimension de l'état", name_states[i], ": ")))
    }

    # Vérifie si toutes les dimensions sont égales
    if (length(unique(dimensions)) == 1) {
      dimension_state <- dimensions[1]
      break
    } else {
      cat("Les dimensions de tous les états doivent être les mêmes. Veuillez réessayer.\n")
    }
  }

  # Lit les valeurs pour chaque état
  state_matrix <- matrix(NA, nb_states, dimension_state)
  for (i in 1:nb_states) {
    cat(paste("Entrez les valeurs de l'état", name_states[i], "séparées par des espaces:\n"))
    input_values <- readline()  # Saisit les valeurs en tant que texte
    values <- as.numeric(unlist(strsplit(input_values, " ")))  # Convertit en valeurs numériques

    # Vérifie la correspondance avec la dimension spécifiée
    while (length(values) != dimension_state) {
      cat("Le nombre de valeurs doit correspondre à la dimension de l'état. Veuillez réessayer.\n")
      input_values <- readline()
      values <- as.numeric(unlist(strsplit(input_values, " ")))
    }
    state_matrix[i, ] <- values
  }

  state_matrix
}
