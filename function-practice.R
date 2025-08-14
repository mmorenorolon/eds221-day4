#add the number of dogs and birds

#defined function
birddog_sum <- function(bird, dog) {
  #write the internal part first
  pets <- bird + dog
  return(pets)
}

#use it!
total_pets <- birddog_sum(bird = 2, dog = 5) #You can ommit writing the variable

#Double values with a function
double_it <- function(x) {
  print(2 * x) #test the internal part before creating the function
}

double_it(139642)

#write a function with conditionals
#converting animals ages
 
animal_age<- function(animal, age) {
if (animal == "dog") {
   print(age * 7)
 } else if (animal == "goat") {
   print(age*4.7)
 }
} 

animal_age(animal = "dog", age = 7)
# 49

#updated version of the function with error and warning messages

animal_age_stop <- function(animal, age) {
  #"if animal is NOT dog or goat, output message
  if (!animal %in% c("dog", "goat")){
    stop("Oops! Animal must be a dog or goat.")
  }
  #"if age is not a number, output message
  if (is.numeric(age) == FALSE) {
    stop("The age must be a number.")
  }
  
  if (age <= 0 | age > 50) {
    warning("Are you sure about your animal's age?")
  }
  
  #body of the executable function
  if (animal == "dog") {
    print(age * 7)
  } else if (animal == "goat") {
    print(age*4.7)
  }
}

animal_age_stop("elephant", 10)

#Functions meet for loops

#all the dataframes in the function are called df --> argument df

df_means <- function(df) { #step 6 paste everything inside the function bracket
  
  #step 5 (paste) everything inside the 'for' brackets
  for (i in 1:ncol(df)) { #step 4
    if (is.numeric(df[i])) {
    column_name <- colnames(df[i]) #step 3
    col_mean <- mean(df[[i]]) #step 2
    print(paste("The mean value of", column_name, "is", col_mean, 
                round(col_mean, 2))) } #step 1 
}

}

#Logistic growth example

logistic_growth <- function(K, N0, r, t){
  Nt <- K / (1 + ((K - N0)/N0) * exp(-r * t))
  print(Nt)
}

logistic_growth(K = 6000, N0 = 100, r = 0.27, t = 40)
# [1] 5992.787

#working on example just with time

time_vec <- seq(from = 0, to = 35, by = 0.1)

# apply logistic growth function to that vector
pop_35 <- logistic_growth(K = 6000, N0 = 100, r = 0.27, t = time_vec)

#converting time steps and population size into a data frame
pop_time_35 <- data.frame(time_vec, pop_35)

#plot it
library(ggplot2)

ggplot(data = pop_time_35, aes(x = time_vec, y = pop_35)) + 
  geom_line(size = 0.5)

#alternatively 
pop_35_vec <- vector(mode = "numeric", length = length(time_vec))

for (i in seq_along(time_vec)) {
  population <- logistic_growth(6000, 100, 0.27, length(time_vec[i]))
  pop_35_vec[i] <- population
}

#series of growth rates
r_seq <- seq(from = 0.2, to = 0.4, by = 0.01)

#empty matrix to store output
out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq))

for (i in seq_along(time_vec)) {
  population <- logistic_growth(6000, 100, r = r_seq[j], length(time_vec[i]))
  out_matrix[i, j] <- population
}

for (j in seq_along(r_seq)) {
  for (i in seq_along(time_vec)) {
    population <- logistic_growth(6000, 100, r = r_seq[j], length(time_vec[i]))
    out_matrix[i, j] <- population
  }
}

# Let's wrangling it a little bit 
out_df <- data.frame(out_matrix, time = time_vec) # Make it a data frame and add time

# Update the column names of out_df, keeping time column name the same
colnames(out_df) <- c(paste0("gr_", r_seq), "time")

# pivot_longer to make it tidy (you'll learn more about this next week)
out_df_long <- out_df %>% 
  pivot_longer(cols = -time, names_to = "growth_rate", values_to = "population")

# Then plot it: 
ggplot(data = out_df_long, aes(x = time, y = population)) +
  geom_line(aes(color = growth_rate)) +
  theme_minimal()
