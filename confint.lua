-- it's simpler to write in lua and then port to R
--[[------------------------
    Non math helpers
--------------------------]]
local function PadString(str, padlength, padstring) -- for binomial table formatting
    str = tostring(str)
    local stringtable = string.Split(str, "")
    for i = 1, math.max(padlength, #str) do
        if i > padlength then -- 
            stringtable[i] = nil
        elseif i <= padlength and i > #str then
            stringtable[i] = padstring
        end
    end
    return table.concat(stringtable)
end

--print(PadString("hello", 7, "d"))
-- output: "hellod"
--[[------------------------
    Binomial math
--------------------------]]
local function NChooseK(n, k)
    return math.Factorial(n) / (math.Factorial(k) * math.Factorial(n - k))
    -- n! / (k!(n - k)!)
end

-- PMF of binomial distribution: https://en.wikipedia.org/wiki/Binomial_distribution#Probability_mass_function
-- k = trials
-- n = sample amount
-- p = probability per trial
local function bin(k, n, p)
    local FormulaChooseX = NChooseK(n, k)
    return FormulaChooseX * p ^ k * (1 - p) ^ (n - k)
end

--print(bin(4, 6, 0.3))
-- output: 0.059535
-- which matches https://en.wikipedia.org/wiki/Binomial_distribution#Example
local function binCDF(trials, n, p)
    local sum = 0
    for k = 0, trials do
        sum = sum + bin(k, n, p)
    end
    return sum
end

--[[------------------------
    Binomial table 
--------------------------]]
local BinomialTable = {} -- so that we don't have to search in it manually
local BinomialTableProbabilities = {0.01, 0.05, 0.10, 0.15, 0.20, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95}
local function MakeBinomialTable()
    for i = 1, #BinomialTableProbabilities do -- prevent truncating trailing zeroes
        BinomialTableProbabilities[i] = PadString(BinomialTableProbabilities[i], 4, "0")
    end

    for n = 2, 7 do
        BinomialTable[n] = {}
        for i = 0, n do
            BinomialTable[n][i] = {}
            for binomindex = 1, #BinomialTableProbabilities do
                local p = BinomialTableProbabilities[binomindex]
                BinomialTable[n][i][p] = bin(i, n, p)
            end
        end
    end
end

MakeBinomialTable()
local function PrintBinomialTable()
    local leftpadcount = ""
    for i = 1, (#BinomialTableProbabilities * 4 + #BinomialTableProbabilities * 4) / 2 do -- this would be a backwards padding, but there's no point to make the padstring function account for left sides just for this one time
        leftpadcount = leftpadcount .. " "
    end

    print(leftpadcount .. "p")
    print("n    " .. table.concat(BinomialTableProbabilities, "   "))
    for n = 2, table.Count(BinomialTable) + 1 do
        for i = 0, n do
            local probabilities = {}
            for binomindex = 1, #BinomialTableProbabilities do
                local p = BinomialTableProbabilities[binomindex]
                probabilities[#probabilities + 1] = PadString(BinomialTable[n][i][p], 6, "0")
            end

            print(((i == 0 and n .. "  " .. i) or "   " .. i) .. " " .. table.concat(probabilities, " "))
        end
    end
end

PrintBinomialTable()
-- output: 
--[[
n    0.01   0.05   0.10   0.15   0.20   0.30   0.35   0.40   0.45   0.50   0.55   0.60   0.65   0.70   0.75   0.80   0.85   0.90   0.95
2  0 0.9801 0.9025 0.8100 0.7225 0.6400 0.4900 0.4225 0.3600 0.3025 0.2500 0.2025 0.1600 0.1225 0.0900 0.0625 0.0400 0.0225 0.0100 0.0025
   1 0.0198 0.0950 0.1800 0.2550 0.3200 0.4200 0.4550 0.4800 0.4950 0.5000 0.4950 0.4800 0.4550 0.4200 0.3750 0.3200 0.2550 0.1800 0.0950
   2 0.0001 0.0025 0.0100 0.0225 0.0400 0.0900 0.1225 0.1600 0.2025 0.2500 0.3025 0.3600 0.4225 0.4900 0.5625 0.6400 0.7225 0.8100 0.9025
3  0 0.9702 0.8573 0.7290 0.6141 0.5120 0.3430 0.2746 0.2160 0.1663 0.1250 0.0911 0.0640 0.0428 0.0270 0.0156 0.0080 0.0033 0.0010 0.0001
   1 0.0294 0.1353 0.2430 0.3251 0.3840 0.4410 0.4436 0.4320 0.4083 0.3750 0.3341 0.2880 0.2388 0.1890 0.1406 0.0960 0.0573 0.0270 0.0071
   2 0.0002 0.0071 0.0270 0.0573 0.0960 0.1890 0.2388 0.2880 0.3341 0.3750 0.4083 0.4320 0.4436 0.4410 0.4218 0.3840 0.3251 0.2430 0.1353
   3 1e-060 0.0001 0.0010 0.0033 0.0080 0.0270 0.0428 0.0640 0.0911 0.1250 0.1663 0.2160 0.2746 0.3430 0.4218 0.5120 0.6141 0.7290 0.8573
4  0 0.9605 0.8145 0.6561 0.5220 0.4096 0.2401 0.1785 0.1296 0.0915 0.0625 0.0410 0.0256 0.0150 0.0081 0.0039 0.0016 0.0005 0.0001 6.25e-
   1 0.0388 0.1714 0.2916 0.3684 0.4096 0.4116 0.3844 0.3456 0.2994 0.2500 0.2004 0.1536 0.1114 0.0756 0.0468 0.0256 0.0114 0.0036 0.0004
   2 0.0005 0.0135 0.0486 0.0975 0.1536 0.2646 0.3105 0.3456 0.3675 0.3750 0.3675 0.3456 0.3105 0.2646 0.2109 0.1536 0.0975 0.0486 0.0135
   3 3.96e- 0.0004 0.0036 0.0114 0.0256 0.0756 0.1114 0.1536 0.2004 0.2500 0.2994 0.3456 0.3844 0.4116 0.4218 0.4096 0.3684 0.2916 0.1714
   4 1e-080 6.25e- 0.0001 0.0005 0.0016 0.0081 0.0150 0.0256 0.0410 0.0625 0.0915 0.1296 0.1785 0.2401 0.3164 0.4096 0.5220 0.6561 0.8145
5  0 0.9509 0.7737 0.5904 0.4437 0.3276 0.1680 0.1160 0.0777 0.0503 0.0312 0.0184 0.0102 0.0052 0.0024 0.0009 0.0003 7.5937 1e-050 3.125e
   1 0.0480 0.2036 0.3280 0.3915 0.4096 0.3601 0.3123 0.2592 0.2058 0.1562 0.1127 0.0768 0.0487 0.0283 0.0146 0.0064 0.0021 0.0004 2.9687
   2 0.0009 0.0214 0.0729 0.1381 0.2048 0.3087 0.3364 0.3456 0.3369 0.3125 0.2756 0.2304 0.1811 0.1323 0.0878 0.0512 0.0243 0.0081 0.0011
   3 9.801e 0.0011 0.0081 0.0243 0.0512 0.1323 0.1811 0.2304 0.2756 0.3125 0.3369 0.3456 0.3364 0.3087 0.2636 0.2048 0.1381 0.0729 0.0214
   4 4.95e- 2.9687 0.0004 0.0021 0.0064 0.0283 0.0487 0.0768 0.1127 0.1562 0.2058 0.2592 0.3123 0.3601 0.3955 0.4096 0.3915 0.3280 0.2036
   5 1e-100 3.125e 1e-050 7.5937 0.0003 0.0024 0.0052 0.0102 0.0184 0.0312 0.0503 0.0777 0.1160 0.1680 0.2373 0.3276 0.4437 0.5904 0.7737
6  0 0.9414 0.7350 0.5314 0.3771 0.2621 0.1176 0.0754 0.0466 0.0276 0.0156 0.0083 0.0040 0.0018 0.0007 0.0002 6.4e-0 1.1390 1e-060 1.5625
   1 0.0570 0.2321 0.3542 0.3993 0.3932 0.3025 0.2436 0.1866 0.1358 0.0937 0.0608 0.0368 0.0204 0.0102 0.0043 0.0015 0.0003 5.4e-0 1.7812
   2 0.0014 0.0305 0.0984 0.1761 0.2457 0.3241 0.3280 0.3110 0.2779 0.2343 0.1860 0.1382 0.0951 0.0595 0.0329 0.0153 0.0054 0.0012 8.4609
   3 1.9405 0.0021 0.0145 0.0414 0.0819 0.1852 0.2354 0.2764 0.3032 0.3125 0.3032 0.2764 0.2354 0.1852 0.1318 0.0819 0.0414 0.0145 0.0021
   4 1.4701 8.4609 0.0012 0.0054 0.0153 0.0595 0.0951 0.1382 0.1860 0.2343 0.2779 0.3110 0.3280 0.3241 0.2966 0.2457 0.1761 0.0984 0.0305
   5 5.94e- 1.7812 5.4e-0 0.0003 0.0015 0.0102 0.0204 0.0368 0.0608 0.0937 0.1358 0.1866 0.2436 0.3025 0.3559 0.3932 0.3993 0.3542 0.2321
   6 1e-120 1.5625 1e-060 1.1390 6.4e-0 0.0007 0.0018 0.0040 0.0083 0.0156 0.0276 0.0466 0.0754 0.1176 0.1779 0.2621 0.3771 0.5314 0.7350
7  0 0.9320 0.6983 0.4782 0.3205 0.2097 0.0823 0.0490 0.0279 0.0152 0.0078 0.0037 0.0016 0.0006 0.0002 6.1035 1.28e- 1.7085 1e-070 7.8125
   1 0.0659 0.2572 0.3720 0.3960 0.3670 0.2470 0.1847 0.1306 0.0871 0.0546 0.0319 0.0172 0.0083 0.0035 0.0012 0.0003 6.7774 6.3e-0 1.0390
   2 0.0019 0.0406 0.1240 0.2096 0.2752 0.3176 0.2984 0.2612 0.2140 0.1640 0.1172 0.0774 0.0466 0.0250 0.0115 0.0043 0.0011 0.0001 5.9226
   3 3.3620 0.0035 0.0229 0.0616 0.1146 0.2268 0.2678 0.2903 0.2918 0.2734 0.2387 0.1935 0.1442 0.0972 0.0576 0.0286 0.0108 0.0025 0.0001
   4 3.3960 0.0001 0.0025 0.0108 0.0286 0.0972 0.1442 0.1935 0.2387 0.2734 0.2918 0.2903 0.2678 0.2268 0.1730 0.1146 0.0616 0.0229 0.0035
   5 2.0582 5.9226 0.0001 0.0011 0.0043 0.0250 0.0466 0.0774 0.1172 0.1640 0.2140 0.2612 0.2984 0.3176 0.3114 0.2752 0.2096 0.1240 0.0406
   6 6.93e- 1.0390 6.3e-0 6.7774 0.0003 0.0035 0.0083 0.0172 0.0319 0.0546 0.0871 0.1306 0.1847 0.2470 0.3114 0.3670 0.3960 0.3720 0.2572
   7 1e-140 7.8125 1e-070 1.7085 1.28e- 0.0002 0.0006 0.0016 0.0037 0.0078 0.0152 0.0279 0.0490 0.0823 0.1334 0.2097 0.3205 0.4782 0.6983
--]]
-- Which matches https://uwf.edu/media/university-of-west-florida/colleges/cse/departments/mathematics-and-statistics/documents/student-resources/binomial-tables.pdf
--
--[[--------------------------------------------------
    Distribution-free confidence interval function
----------------------------------------------------]]
local function DistributionFreeConfidenceInterval(leftorderstatistic, rightorderstatistic, n, p)
    local confidence = binCDF(rightorderstatistic, n, p) - binCDF(leftorderstatistic, n, p)
    return confidence
end
--print(DistributionFreeConfidenceInterval(3, 10, 14, 0.5))
-- output: 0.942626953125
