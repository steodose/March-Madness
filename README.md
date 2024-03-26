# March-Madness

![Simulations](https://github.com/steodose/March-Madness/blob/main/2024%20March%20Madness%20Predictions.png?raw=true)

This project simulates the NCAA Men's Basketball Tournament outcomes using a Monte Carlo method based on team ratings. The simulation predicts the probability of each team advancing through the various rounds of the tournament, culminating in the championship.

## Overview

The simulation takes into account the ratings for each team, which represent the expected point differential against an average team on a neutral court. These ratings are used to calculate the probability of winning for each game in the tournament through a logistic regression model. The tournament is simulated multiple times to estimate the probabilities of each team reaching each round, including the Sweet Sixteen, Elite Eight, Final Four, and the Championship Game.

## How it Works

- **Input Data**: A CSV file containing team ratings, seeds, and regions for the tournament teams.
- **Simulation Process**:
  - Calculates win probabilities based on team ratings.
  - Simulates each game of the tournament.
  - Repeats the tournament simulation for the specified number of iterations.
  - Calculates the probabilities of each team advancing through each round based on the outcomes of the simulations.
- **Output**: A dataframe (`sims`) with each team's probabilities of reaching each tournament stage.

## Customization

You can customize the simulation by adjusting the following:

- **Number of Simulations**: Increase or decrease the `num_simulations` variable to change the simulation's accuracy and runtime.
- **Team Ratings**: Update the input CSV file with current or alternative ratings to simulate different tournament outcomes.

## Contributing

Contributions to improve the simulation or extend its capabilities are welcome. Please feel free to fork the repository and submit pull requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


