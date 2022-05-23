library(markovchain)

#Create a vector of the states
DriverZone <- c("North","South","West")
DriverZone

#Create transition matrix of zone movement probabilities

ZoneTransition <- matrix(c(0.3,0.3,0.4,0.4,0.4,0.2,0.5,0.3,0.2),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimname = list(DriverZone,DriverZone))
  
ZoneTransition

#Creating markov chain with state space equal to predefined vector, using transition matrix in step 2.

MCZone <- new("markovchain",states = DriverZone,
              byrow = TRUE,
              transitionMatrix = ZoneTransition,
              name = "DriverMovement")

MCZone
class(MCZone)

#Solving probability problems

MCZone^10

#Determine Steady States
steadyStates(MCZone)

