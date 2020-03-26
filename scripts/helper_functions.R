library(stringr)


clean_names <- function(x){
  x <- tolower(x)
  x <- str_replace_all(x,c(
    " "   = "_"
    ,"\\(" = ""
    ,"\\)" = ""
  )
  )
}
