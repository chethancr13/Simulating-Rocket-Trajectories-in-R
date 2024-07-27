library(deSolve)

# Function to calculate the rocket's state derivatives
rocket_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Unpack state variables
    x <- state[1]
    y <- state[2]
    vx <- state[3]
    vy <- state[4]
    
    # Parameters
    g <- 9.81  # Acceleration due to gravity
    
    # Equations of motion
    dxdt <- vx
    dydt <- vy
    dvxdt <- 0  # Assuming no horizontal acceleration
    dvydt <- -g
    
    list(c(dxdt, dydt, dvxdt, dvydt))
  })
}

# Initial conditions
state <- c(x = 0, y = 0, vx = 100, vy = 50)
parameters <- list()  # No additional parameters for this simple model

# Time points for simulation
times <- seq(0, 10, by = 0.1)

# Solve the differential equations
output <- ode(y = state, times = times, func = rocket_model, parms = parameters)

# Extract results
x <- output[, 2]
y <- output[, 3]

# Plot the trajectory
plot(x, y, type = "l", xlab = "Distance (m)", ylab = "Height (m)", main = "Rocket Trajectory")

