# The script classify each hours to rain/non-rain hours
# Start of non-rain hour = -1. Consecutive non-rain hours = -2, -3 , etc.
# Similarly for rain hours

# Import data. Data should be arranged in 2 columns, 1st: continuous datetime, 2nd: Rainfall (mm)
Rainfall_classiciation <- read_csv("Rainfall_classiciation.csv")

# Make sure rainfall data is numeric type
Rainfall_classiciation$`Rainfall (mm)` = as.numeric(Rainfall_classiciation$`Rainfall (mm)`)

# Initialize all Classification to 0
Rainfall_classiciation$Classification = 0

# Define first classication
if (Rainfall_classiciation[1,2] <= 0.2) Rainfall_classiciation[1,3] = -1 else Rainfall_classiciation[1,3] = 1

# identify rainfall classes for all other hours
rows = c(2:nrow(Rainfall_classiciation))
for (i in rows) {
  # Hours with no rainfall data
  if (is.na(Rainfall_classiciation[i,2])) {
    Rainfall_classiciation[i,3] = 0
  }
  # Hours with no rainfall followed by dry
  else if (is.na(Rainfall_classiciation[i-1,2]) && Rainfall_classiciation[i,2] <= 0.2) {
    Rainfall_classiciation[i,3] = -1
  }
  # Hours with no rainfall followed by wet
  else if (is.na(Rainfall_classiciation[i-1,2]) && Rainfall_classiciation[i,2] > 0.2) {
    Rainfall_classiciation[i,3] = 1
  }
  # Consecutive dry hours
  else if (Rainfall_classiciation[i,2] <= 0.2 && Rainfall_classiciation[i-1,2] <= 0.2) {
    Rainfall_classiciation[i,3] = Rainfall_classiciation[i-1,3] - 1
  }
  # Consecutive wet hours
  else if (Rainfall_classiciation[i,2] > 0.2 && Rainfall_classiciation[i-1,2] > 0.2) {
    Rainfall_classiciation[i,3] = Rainfall_classiciation[i-1,3] + 1
  }
  # Change from dry to wet
  else if (Rainfall_classiciation[i,2] > 0.2 && Rainfall_classiciation[i-1,2] <= 0.2) {
    Rainfall_classiciation[i,3] = 1
  }
  # Change from wet to dry
  else if (Rainfall_classiciation[i,2] <= 0.2 && Rainfall_classiciation[i-1,2] > 0.2){
    Rainfall_classiciation[i,3] = -1
  }
  print (i)
}
