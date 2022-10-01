events_func <- function(current_guess, countries_rem){
  active <- FALSE
  event_text <- ""
  if(tolower(current_guess) %in% c("reykjavic", "rejkyavik", "reikjavik", "reykavik",
                                   "reyjakvik", "reikavik", "reikjavik")){
    event_text <- "Here's a wild fact just in case you were wondering: the name for the northern-most capital in the world
    is derived from the Old Norse for smokey (Reykja) bay (vík). Crazy aye?"
    active <- TRUE
  }
  if(tolower(current_guess) == "cape town" | tolower(current_guess) == "bloemfontein"){
    event_text <- "Cape Town and Bloemfontein are capitals of South Africa.
    However, South Africa officially has three capitals,
    and the World Map that I downloaded uses the other one - good luck!"
    active <- TRUE
  }
  # if(rv$countries_rem == 1){
  #   event_text <- "Nearly there... How's your Icelandic spelling..?"
  #   active <- TRUE
  # }
  if(tolower(current_guess) == "tel aviv" | tolower(current_guess) == "jerusalem"){
    event_text <- "This one is controversial...
    I just downloaded a world map and am not smart enough to change it.."
    active <- TRUE
  }
  if(tolower(current_guess) == "amsterdam"){
    event_text <- "While The Hague is the administrative capital of the country and the home of the court and government, Amsterdam is the official capital."
    active <- TRUE
  }
  if(tolower(current_guess) == "colombo" | tolower(current_guess) == "sri jayawardenepura kotte"){
    event_text <- "Sri Lanka officially has two capitals: Colombo and Sri Jayawardenepura Kotte.
    However, they are very close to each other, and therefore I'll accept either one."
    active <- TRUE
  }
  if(tolower(current_guess) == "bern"){
    event_text <- "Switzerland has no official capital, as it wanted to keep every territory of equal importance.
    Bern is the unofficial capital, as it is the seat of Switzerlands federal government."
    active <- TRUE
  }
  
  return(c(event_text, active))
}