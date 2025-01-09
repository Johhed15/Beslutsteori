# Beslutsteori
Labbar från en beslutsteorikurs!

## labb 1
Posterior probability and odds

## labb 2 

Decision making under ignorance and with data

## labb 3

Problem reasoning around decisions 

## labb 4

Bayesian decision-making and value of information sampling.

# Exempeluppgift

What is the ENGS for a sample of 10 chips using a single-stage sampling plan.

Expected net gain of sampling as a function of the sample size n:

$$ENGS(n) = EVSI(n) - CS(n)$$

Cost of 10 draws is 2.5$

The only samples that will make us change from the prior actions are the ones that have more red chips than blue, so we only have to consider the probabilities from drawing 6, 7, 8, 9 and 10 red chips out och 10 chips. 

But for the table too look closer to the one in the lecture I will do the calculations from collecting 0 red chips out of 10 even though the EVSI for y lower then 6 will be 0. 


´´´{R}
# Constants
n <- 10 # Total draws
prior_red <- 0.4 # Prior for state 1 (70% red)
prior_blue <- 0.6 # Prior for state 2 (70% blue)


# Initialize a table that looks like that in the lecture
results <- data.frame(
  y = 0:n,                                # Number of red chips drawn
  theta = "State 1 (70% red)",            # State description
  P_theta = prior_red,                    # etc
  P_y_given_theta = rep(NA, 11),       
  P_theta_given_y = rep(NA, 11),       
  E_R_Guess_Red = rep(NA, 11),         
  E_R_Guess_Blue = rep(NA, 11),        
  Optimal_Action = rep(NA, 11),        
  E_a_prior = rep(NA, 11),             
  VSI = rep(NA, 11),                   
  P_y = rep(NA, 11),                 
  EVSI = rep(NA, 11)                   
)

# Fill the table
for (y in 0:n) {
  # Likelihoods for each state
  likelihood_red <- choose(n, y) * (0.7^y) * (0.3^(n - y)) # State 1 (70% red)
  likelihood_blue <- choose(n, y) * (0.3^y) * (0.7^(n - y)) # State 2 (70% blue)
  
  # Marginal probability P(y)
  p_y <- likelihood_red * prior_red + likelihood_blue * prior_blue
  
  # Posterior probabilities
  post_red <- (likelihood_red * prior_red) / p_y
  post_blue <- (likelihood_blue * prior_blue) / p_y

  # VSI and EVSI (only doing the calculations when changing from the prior)
  vsi <- (5 * post_red - 3 * post_blue) - (-3 * post_red + 5 * post_blue)
  vsi <- ifelse(vsi>0,vsi,0) # 
  evsi <- vsi * p_y
  
  # Fill row
  results$P_y_given_theta[y +1] <- likelihood_red
  results$P_theta_given_y[y+1] <- post_red
  results$E_R_Guess_Red[y +1] <- (5 * post_red - 3 * post_blue)
  results$E_R_Guess_Blue[y +1] <- (-3 * post_red + 5 * post_blue)
  results$Optimal_Action[y+1] <- ifelse(vsi>0,
                  "Guess 70% Red", "Guess 70% Blue")
  results$E_a_prior[y +1] <-  "Guess 70% Blue"
  results$VSI[y +1] <- vsi
  results$P_y[y +1] <- p_y
  results$EVSI[y +1] <- evsi
}

# Print the table
print(results)



paste('EVSI for a draw of 10 chips is: ',round(sum(results$EVSI),2))
paste('ENGS for a draw of 10 chips is: ',round(sum(results$EVSI) -2.5,2))

```


```

<br>
<br>
<br>
<div align="center">
  <img src="https://media0.giphy.com/media/v1.Y2lkPTc5MGI3NjExMm5qeW9zbWRrOHExcThpamMyOTBsZXJuOHRoYzdjMTlqMTVoMXFjZCZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/cKvFuyGg5E27S5ydBY/giphy.webp" width="600" height="600"/>
</div>

<div align="center">
  <img src="https://media1.giphy.com/media/v1.Y2lkPTc5MGI3NjExYjIwYjlqYXd3MHE0ZzF2ZmliYmdvZDRvcXRxNGV1Y3dnbXJ5MXRveiZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/L3c8aAACw7Qpx2ojGB/giphy.webp" width="600" height="600"/>
</div>


