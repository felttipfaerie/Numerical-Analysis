gender <- c("male","female",
            
            "male","female","male","female")



solution <- c("stick","stick","tree","tree","failed","failed")

count <- c(32,15,43,65,25,20)

Adult <- data.frame(gender, solution, count)

print(Adult)


tab <- xtabs(count~gender+solution,data = Adult)

tab

sol.chisq <- chisq.test(tab.)

sol.chisq
