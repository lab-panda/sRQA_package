## Files

| File | Description |
|------|-------------|
| `single_example.R` | Generates stochastic, periodic, and chaotic time series, applies sRQA, and produces symbolization plots, recurrence plots, Table 1, and windowed regime shift detection |
| `simulation.R` | Runs 100 iterations of the same three systems, produces boxplots (Figure 2a, 2c), and performs statistical comparisons |

## Reproducing the Results

1. Run `single_example.R` to generate the single-run figures and Table 1 values
2. Run `simulation.R` to generate the simulation boxplots and statistical results

The scripts can be run independently in any order. If running both in the same R session, variable name prefixes (`s1_` and `sim_`) prevent conflicts.

All file paths are hardcoded and will need to be updated to match your local directory structure.
