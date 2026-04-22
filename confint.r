############################
    ##Non math helpers
############################

PadString = function(str, padlength, padstring){ #for binomial table formatting
    ##str = tostring(str)
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
        if (p_value >= highest_p_value && p_value <= alpha){
            highest_p_value = p_value
            highest_k = k
        }
    }
    return (c(highest_k, highest_p_value))
}
#print(FindHighestKUnderAlpha(0.05, 8, 0.5))
#output: 1.00000000 0.03515625
#which matches n = 8, k = 1, p = 0.5 https://baek.math.umbc.edu/stat355/binomial.pdf
