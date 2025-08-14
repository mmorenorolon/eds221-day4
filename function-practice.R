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