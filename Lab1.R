isPalindromic <- function(num){
  # is palandromic
  # if number == number[::-1]
  
  # remove trailing 0's (since they will appear before number anyways)
  while(num %% 10 == 0 && num != 0){
    num = num/10
  }
  
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

nextPalindrome <- function(number){
  
  # same as before, put num to array
  
  num <- as.character(number)
  num <- strsplit(num, "")[[1]]
  
  # take first half of array, rounded down
  # take middle of array (only if odd)
  # reverse first half of array. combine all 3 segments
  
  midpoint = floor(length(num)/2)
  first = num[1:midpoint]
  
  middle = ""
  if(length(num)%%2 != 0){
    middle = num[midpoint+1]
  }
  
  last = rev(first)
  
  final = c(first,middle,last)
  final = as.numeric(paste(final,collapse = ""))
  
  print(final)
}

nextPalindrome <- function(num){
  status <- isPalindromic(num)
  print(status$isPalindromic)
  while(status$isPalindromic == FALSE){
    num = num + 1
    status <- isPalindromic(num)
  }
  return(num)
}

nextPalindrome(30113)
