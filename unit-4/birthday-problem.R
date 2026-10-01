# Coding-assistant note: OpenAI Codex assisted with coding. The project owner made the key decisions on how to segment and manage the data.
y <- 365 # years
n <- 20 # number of people
p <- 0 # probability increasing
q <- 1 # probability decreasing


#probability that 2 people share a birthday
for (i in 1:(n - 1)){
  p <- p + (i / (y - i)) 
  print((paste(i, "P:", p)))
}


# probability that no one shares a birthday

for (i in 1:(n - 1)){
  q <- q - (i / y)
  print((paste(i, "q:", q)))
}
