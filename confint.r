############################
    ##Non math helpers
############################

PadString = function(str, padlength, padstring){ #for binomial table formatting
    str = toString(str)
    stringtable = strsplit(str, "")[[1]]
    for (i in 1:max(padlength, nchar(str))){
        if (i > padlength){ 
            stringtable[i] = ""
        }
        else if (i <= padlength && i > nchar(str)){
            stringtable[i] = padstring
        }
    }
    return (paste(stringtable, sep = "", collapse = ""))
}
##print(PadString("hello", 7, "d"))
## output: "hellodd"

############################
    ##Binomial math
############################
NChooseK = function(n, k){
    return (factorial(n) / (factorial(k) * factorial(n - k)))
    ## n! / (k!(n - k)!)
}

##print(NChooseK(3, 2))
## output: 3
##
## PMF of binomial distribution: https://en.wikipedia.org/wiki/Binomial_distribution#Probability_mass_function
## k = trials
## n = sample amount
## p = probability per trial
bin = function(k, n, p){
    FormulaChooseX = NChooseK(n, k)
    return (FormulaChooseX * p ^ k * (1 - p) ^ (n - k))
}

##print(bin(4, 6, 0.3))
## output: 0.059535
## which matches https://en.wikipedia.org/wiki/Binomial_distribution#Example

binCDF = function(trials, n, p){
    sum = 0
    for (k in 0:trials){
        sum = sum + bin(k, n, p)
    }
    return (sum)
}

#print(binCDF(0, 1, 0.05))
#output: 0.95
#which matches https://baek.math.umbc.edu/stat355/binomial.pdf

FindHighestKUnderAlpha = function(alpha, n, p){
    highest_p_value = 0
    highest_k = 0
    for (k in 0:n){ ## This for loop is unoptimized. I cannot think of a better way of doing this right now unfortunately.
        p_value = binCDF(k, n, p)
        if (p_value <= alpha){
            highest_p_value = p_value
            highest_k = k
        }
    }
    return (c(highest_k, highest_p_value))
}
#print(FindHighestKUnderAlpha(0.05, 8, 0.5))
#output: 1.00000000 0.03515625
#which matches n = 8, k = 1, p = 0.5 https://baek.math.umbc.edu/stat355/binomial.pdf

############################
    ##normal distribution math
############################
inverseerrorfunction = function(x){ # Based on Mike Giles' CUDA code (table 5) in https://people.maths.ox.ac.uk/gilesm/files/gems_erfinv.pdf
    w = -log((1 - x) * (1 + x))
    p = NULL
    if (w < 5){
        w = w - 2.5
        p = 2.81022636 * 10 ^ -08
        p = 3.43273939 * 10 ^ -07 + p * w
        p = -3.5233877 * 10 ^ -06 + p * w
        p = -4.39150654 * 10 ^ -06 + p * w
        p = 0.00021858087 + p * w
        p = -0.00125372503 + p * w
        p = -0.00417768164 + p * w
        p = 0.246640727 + p * w
        p = 1.50140941 + p * w
    }
    else{
        w = math.sqrt(w) - 3
        p = -0.000200214257
        p = 0.000100950558 + p * w
        p = 0.00134934322 + p * w
        p = -0.00367342844 + p * w
        p = 0.00573950773 + p * w
        p = -0.0076224613 + p * w
        p = 0.00943887047 + p * w
        p = 1.00167406 + p * w
        p = 2.83297682 + p * w
    }
    return (p * x)
}

#print(inverseerrorfunction(0.5))
#output: 0.4769363

errorfunction = function(x){ # Necessary for normalCDF formula
    # Uses Abromowitz & Stegun error function approximation https://personal.math.ubc.ca/%7Ecbm/aands/page_299.htm
    # Thank you https://math.stackexchange.com/a/321582 for linking the paper.
    signflip = 1
    if (x < 0){
        signflip = -1
    }

    x = abs(x)
    t = function(){
        p = 0.47047
        return (1 / (1 + p * x))
    }

    e = exp(1)
    a_1 = 0.34802
    a_2 = -0.09587
    a_3 = 0.74785
    return (signflip * (1 - (a_1 * t() + a_2 * t() ^ 2 + a_3 * t() ^ 3) * e ^ -x ^ 2))
}

#print(errorfunction(1))
#output: 0.8427169

normaldistributionCDF = function(z, mew, stdev){
    return (1 / 2 * (1 + errorfunction((z - mew) / (stdev * sqrt(2)))))
}

#print(normaldistributionCDF(0.1, 0, 1))
#output: 0.5398386
# which matches https://math.arizona.edu/~rsims/ma464/standardnormaltable.pdf
#
# https://en.wikipedia.org/wiki/Normal_distribution#Quantile_function
# returns left side z value given probability
inversenormaldistributionCDF = function(p, mew, stdev){
    #Φ^-1(p) = sqrt(2) * erf^-1(2p - 1)
    return (mew + stdev * sqrt(2) * inverseerrorfunction(2 * p - 1))
}

#print(inversenormaldistributionCDF(0.97500, 0, 1))
#output: 1.959964
# which nearly matches (off by 0.001) https://math.arizona.edu/~rsims/ma464/standardnormaltable.pdf

############################
    ##Distribution-free confidence interval functions
############################
DistributionFreeConfidenceInterval = function(leftorderstatistic, rightorderstatistic, n, p, alpha){
    if (is.list(n)){
        n = length(n)
    }
    if (!is.null(leftorderstatistic) && !is.null(rightorderstatistic)){ # Give confidence based on indices
        i = leftorderstatistic - 1
        j = rightorderstatistic - 1
        confidence = binCDF(j, n, p) - binCDF(i, n, p)
        return (confidence)
    }
    else { # Give indices based on confidence
        a = alpha / 2 # alpha constraint (2 tail so divide by 2) 
        i = FindHighestKUnderAlpha(a, n, p)[1]
        i = i + 1 # left tail
        j = n - i + 1 # right tail
        return (c(i, j))
    }
}

# Example 1
#print(DistributionFreeConfidenceInterval(4, 11, 14, 0.5))
# output: 0.942627
#Example 2
#print(DistributionFreeConfidenceInterval(NULL, NULL, 20, 0.5, 0.05))
#output: 6 15

LargeSampleApproximation = function(n, p, alpha){
    zCrit = inversenormaldistributionCDF(alpha / 2, 0, 1)
    zCrit = abs(zCrit) # make it positive so that i, j are in the correct order
    i = n * p - zCrit * sqrt(n * p * (1 - p))
    j = n * p + zCrit * sqrt(n * p * (1 - p))
    i = floor(i)
    j = ceiling(j)
    return (c(i, j))
}

# Example 3
#print(LargeSampleApproximation(150, 0.5, 0.05))
#output: 62 88
