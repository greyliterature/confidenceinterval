# Distribution free confidence intervals for percentiles
These codes implements functions in R and (G)LUA to:
1. Return confidence given left and right order statistics (DistributionFreeConfidenceInterval(leftorderstatistic, rightorderstatistic, n, p))
2. Return left and right order statistics given alpha (DistributionFreeConfidenceInterval(NULL, NULL, n, p, alpha))
3. Return left and right order statistics to satisfy alpha using Large Sample Approximation method (LargeSampleApproximation(n, p, alpha))
