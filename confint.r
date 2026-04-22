############################
    #Non math helpers
############################

PadString = function(str, padlength, padstring){ #for binomial table formatting
    #str = tostring(str)
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
#print(PadString("hello", 7, "d"))
# output: "hellodd"
