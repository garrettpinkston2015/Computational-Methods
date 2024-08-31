is_palandrome <- function(number){
  # is palandromic
  # if number == number[::-1]
  
  # need to get number into list
  #number = as.numeric(strsplit(number,""))
  print(number)
  if (number == rev(number)){
    return(T)
  }
  else{
    return(F)
  }
}

is_palandrome("ac")
