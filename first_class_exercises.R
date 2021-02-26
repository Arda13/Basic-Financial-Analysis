
#### EXERCISES 2 ####

# 1. Create a vector named "desk.items" with elements named 
#pens, sheets, keyboards, screens, cups, fingers, change and values 2, 125, 1, 2, 1, 10, 12.34

desk.items <- c("pens" = 2, "sheets"= 125, "keyboards" = 1, "screens" = 2, "cups" = 1, "fingers"=10, "change"=12.34)

# 2. Create a vector named "locker.items" with elements named
# pens, sheets, cups, change and values 10, 500, 1, 0.02

locker.items <- c("pens"=10,"sheets"=500,"cups"=1,"change"=0.02)

# 3. How many pens are there in total?

desk.items["pens"] + locker.items["pens"]

# 4. How much money is there in total?

desk.items["change"] + locker.items["change"]

# 5. . Create a vector weight containing patient weight measurements: 60,72,57,90,95 
# and a vector height containing patient height measurements 1.72, 1.80, 1.65, 1.90, 1.74.

weight <- c(60,72,57,90,95)
height <- c(1.72,1.80,1.65,1.90,1.74)

# 6. Calculate the BMI of patients. BMI = weight / (height)2

BMI <- weight / (height)*2

# 7. Sum up the elements of vector weight and divide calculated sum by length of vector weight. 
# 	Assign the calculated value to a vector average_weight.

average_weight <- sum(weight) / length(weight)

# 8. Calculate the average value of vector weight using the function mean().

mean(weight)

# 9. Create a character vector FILMS containing three movie titles (any).

FILMS <- c("FILM1","FILM2","FILM3")

# 10. Select from vector FILMS elements 1 and 3.

FILMS[-2]

# 11. Add to the vector FILMS the fourth title of a film (any).

FILMS <- append(FILMS,"FILM4")
