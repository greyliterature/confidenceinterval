# Distribution free confidence intervals for percentiles
These codes implement functions in R and (G)LUA to:
1. Return confidence given left and right order statistics (DistributionFreeConfidenceInterval(leftorderstatistic, rightorderstatistic, n, p))
2. Return left and right order statistics given alpha (DistributionFreeConfidenceInterval(NULL, NULL, n, p, alpha)) (requires p = 0.5 for now)
3. Return left and right order statistics to satisfy alpha using Large Sample Approximation method (LargeSampleApproximation(n, p, alpha))
