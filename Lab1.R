is_palandrome <- function(num){
  # is palandromic
  # if number == number[::-1]
  
  # remove trailing 0's (since they will appear before number anyways)
  
  # convert number to string
  # get number into list using strsplit
  # reverse list
  # check if lists are equal
  
  num <- as.character(num)
  num <- strsplit(num, "")[[1]]
  n2 <- rev(num)
  status <- identical(num,n2)
  return(list(isPalindromic = status, reversed = n2))
}

is_palandrome(728827)
