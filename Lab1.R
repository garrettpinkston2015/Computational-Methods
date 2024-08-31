is_palandrome <- function(number){
  # is palandromic
  # if number == number[::-1]
  
  # need to get number into list
  #number = as.numeric(strsplit(number,""))
  
  number <- as.character(number)
  number <- strsplit(number, "")[[1]]
  n2 <- rev(number)
  print((n2))
  print((number))
  
  
  identical(number,n2)
}

is_palandrome(12321)
