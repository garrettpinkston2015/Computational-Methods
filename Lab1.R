



# Palindromes

isPalindromic <- function(num){
  # is palandromic
  # if number == number[::-1]
  
  # remove trailing 0's (since they will appear before number anyways)
  #while(num %% 10 == 0 && num != 0){
  #  num = num/10
  #}
  
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
isPalindromic(100)

nextPalindrome <- function(num){
  status <- isPalindromic(num)
  while(status$isPalindromic == FALSE){
    num = num + 1
    status <- isPalindromic(num)
  }
  return(num)
}

nextPalindrome(19272719)


