# AskAManager

setwd('/Users/garrettpinkston/Desktop/Michigan/STAT506/Data/')
df <- read.csv('AskAManager.csv')

names(df)
df$X = NULL
descriptors <- c("Timestamp","Age","Industry","Title","Additional Title Info",
                 "Salary","Bonus Pay","Currency","Other Currency","Additional Income Info",
                 "Country", "State", "City", "Total Experience", "Industry Experience", 
                 "Education", "Gender", "Race")

colnames(df) <- descriptors
df <- df[df$Currency == 'USD',]

df$MinimumExperience = as.numeric(substr(df$`Total Experience`,1,1))
df$MinimumAge = (substr(df$Age,1,1))

df$MinimumExperience <- gsub( "-.*", "", df$`Total Experience`)



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


