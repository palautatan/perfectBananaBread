
#FIX PREP TIME(ONLY MINUTES)
def fixPrep(entry):
    try:
        return(int(entry.replace("'","").replace("[","").replace("]","")))
    except:
        return(np.NaN)

#FIX COOKING TIME
def cook_time(x):
    try:
        if len(x)>6:
            to_conv = [int(m) for m in x.replace("'","").replace("[","").replace("]","").split(", ")]
            return to_conv[0]*60 + to_conv[1]
        else:
            return fixPrep(x)
    except:
        return np.NaN


#TEST
fixed_cook_time = [cook_time(x) for x in bb_df["cook"]]