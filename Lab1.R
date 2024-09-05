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

df$MinimumExperience <- as.numeric(gsub("([0-9]+).*$", "\\1", df$`Total Experience`)) # found code on stackoverflow with RegEx to help
df$MinimumAge <- as.numeric(gsub("([0-9]+).*$", "\\1", df$Age)) # found code on stackoverflow with RegEx to help

df$MinimumAge <- as.numeric(gsub(".*/|-.*", "", df$Age)) #extract first number in Age








df$Invalid <- df$MinimumAge - df$MinimumExperience


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


