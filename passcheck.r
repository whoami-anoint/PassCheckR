# PassCheckR - Password Analysis tool to check password strength using R programming language
# @whoami-anoint
# anoint.02

# Define the minimum requirements for a strong password
min_length <- 8
uppercase <- "[A-Z]"
lowercase <- "[a-z]"
numbers <- "[0-9]"
special_characters <- " !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"

# Define function to check if password is strong
check_password_strength <- function(password) {
  # Check password length
  if (nchar(password) < min_length) {
    return("Weak")
  }
  
  # Check password complexity and assign score
  score <- 0
  
  # checks the password strength using the `grepl()` function
  if (grepl(uppercase, password)) {
    score <- score + 1
  }
  
  if (grepl(lowercase, password)) {
    score <- score + 1
  }
  
  if (grepl(numbers, password)) {
    score <- score + 1
  }
  
  if (grepl(special_chars, password)) {
    score <- score + 1
  }
  
  # Return password strength message based on score
  if (score == 0) {
    return("Weak")
  } else if (score <= 2) {
    return("Moderate")
  } else {
    return("Strong")
  }
}

# Password Taken from users
password <- readline("Enter your password: ")
strength <- check_password_strength(password)
cat("Your password strength is", strength, "\n")
