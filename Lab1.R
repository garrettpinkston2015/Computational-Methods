# Wines
setwd('/Users/garrettpinkston/Desktop/Michigan/STAT506/Data/')

wine <- read.table("wine.data",sep=",")

wineNames <- c("Class", "Alcohol","Malic acid","Ash","Alcalinity of ash", "Magnesium", "Total phenols", 
  "Flavanoids","Nonflavanoid phenols", "Proanthocyanins", "Color intensity", "Hue", 
  "OD280/OD315 of diluted wines", "Proline")

colnames(wine) <- wineNames

counts <- c(59,71,48)
bool <- TRUE
for (i in 1:3){
  if(nrow(wine[wine$Class==i,]) != counts[i]){
    bool <- FALSE
  }
}

bool

alcColorCor <- cor(wine$Alcohol,wine$`Color intensity`)
alcColorCor

corrs = c()
for (i in 1:3){
  wineCopy <- wine[wine$Class == i,]
  corrs[i] <- cor(wineCopy$Alcohol, wineCopy$`Color intensity`)
}

paste("Class", which.min(corrs), "achieved the smallest correlation with a coefficient of", round(min(corrs),4))
paste("Class", which.max(corrs), "achieved the smallest correlation with a coefficient of", round(max(corrs),4))


paste("The alcohol content of the wine with the highest color intensity is", wine$Alcohol[which.max(wine$`Color intensity`)])

higherPro <- nrow(wine[wine$Proanthocyanins > wine$Ash,])
paste0("Approximately ", round((higherPro/nrow(wine)*100),2), "% of the wines contain higher Proanthocyanins than Ash")

# AskAManager

setwd('/Users/garrettpinkston/Desktop/Michigan/STAT506/Data/')
df <- read.csv('AskAManager.csv')

names(df)
df$X = NULL #remove first column (junk) dataframe

descriptors <- c("Timestamp","Age","Industry","Title","Additional Title Info",
                 "Salary","Bonus Pay","Currency","Other Currency","Additional Income Info",
                 "Country", "State", "City", "Total Experience", "Industry Experience", 
                 "Education", "Gender", "Race")

colnames(df) <- descriptors #replace column names with accurate descriptions
df <- df[df$Currency == 'USD',] # Keep all entries of just USD

df$MinimumExperience <- as.numeric(gsub(".*?([0-9]+).*", "\\1", df$`Total Experience`)) # found code on stackoverflow with RegEx to help
df$MinimumAge <- as.numeric(gsub(".*?([0-9]+).*", "\\1", df$Age)) # found code on stackoverflow with RegEx to help
df <- df[(df$MinimumAge - df$MinimumExperience) > 18,] #filter all rows where people started work before 18

# Will use IQR to remove outliers
quartiles <- quantile(df$Salary, probs = c(0.25,0.75)) #only returns Q1 and Q3 as a list
salaryRange <- IQR(df$Salary) #returns scalar

lowerLimit <- quartiles[1] - (1.5 * salaryRange) # way below first quartile 
upperLimit <- quartiles[2] + (1.5 * salaryRange) # way above third quartile

df <- df[df$Salary > lowerLimit,] # keep all rows where salary is above lower limit
df <- df[df$Salary < upperLimit,] # keep all rows where salary is below upper limit

# I used IQR to remove outliers because it is a robust method that takes

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


