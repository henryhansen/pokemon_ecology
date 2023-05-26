# allows a user to generate pokemon observations from
# a standard pokedex
# https://www.kaggle.com/datasets/mariotormo/complete-pokemon-dataset-updated-090420?resource=download

#' Pokedex Simulator -Samples random pokemon
#'
#' @param n number of pokemon to return
#' @param replace controls whether to sample with replacement or not
#' @param gen controls which generation of pokemon to sample from, default is 1st gen
#' @param prob an optional parameter that controls probability of sampling see sample()
#'
#' @return a dataframe of random selected pokemon
#' @export
#'
#' @examples pokedex.sim(10)
pokedex.sim <- function(n, replace = T, gen = 1, prob) {
  pokedex <- pokedex[pokedex$generation == gen,]
  if(missing(prob)) {
    index <- sample(1:nrow(pokedex), n, replace = replace)
  } else {
    index <- sample(1:nrow(pokedex), n, replace = replace, prob = prob)
  }
  return(pokedex[index,])
}


# allows a user to simulate a fake dataset about a single pokemon species
# a user can specify the species, target traits with standard variation, and number of individuals
#' Pokemon Population Sampler - Samples specified pokemon and traits
#'
#' @param pokemon name of pokemon in title case
#' @param n number of pokemon to return
#' @param traits column name from pokedex describing pokemon traits
#' @param vary controls whether to vary numerical trait values
#' @param sd optional parameter to influence how much variation to add
#'
#' @return a dataframe of pokemon with selected traits
#' @export
#'
#' @examples pokemon.pop.sim("Bulbasaur", 10)
pokemon.pop.sim <- function(pokemon, n, traits, vary, sd = 1) {
  pokemon <- pokedex[pokedex$name == pokemon,]
  temp <- do.call("rbind", replicate(n, pokemon, simplify = FALSE))

  if(missing(traits)) {
    message("No traits selected. No variation applied")
    return(temp)
  } else {
    temp <- temp[,c("name",traits)]
    message("The following traits were selected ", list(traits))
  }

  if(missing(vary)) {
    return(temp)
  } else {
    v.temp <- temp[,c("name",traits)]
    for(i in 1:ncol(v.temp)) {
      trait.value <- v.temp[1,i]
      if (is.integer(trait.value) | is.double(trait.value)) {
        v.temp[ , i] <- rnorm(n, mean = trait.value, sd = sd)
      } else {
        message(c("Only ", list(traits)," were varied."))
      }
    }
    return(v.temp)
  }
}
