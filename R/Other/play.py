
import numpy as np
import pandas as pd

def idx_greater_than(x, l):
    """Find the first index of list l that has a corresponding value greater than x"""
    return next(i for i,v in enumerate(l) if v > x)
                                       
def run():
    df = pd.DataFrame({
        "colour": ["red", "green", "red", "blue", "blue", None, "blue", None],
        "coolest": ["braun", "kinz", "braun", None, None, "skittles", "skittles", "kinz"]
    })

    for feature in df:
        # Get index of nans
        nan_idxs = np.where(df[feature].isnull())[0]
        # If no nans, don't worry
        if nan_idxs.size == 0:
            continue
        srs_notnull = df.loc[df[feature].notnull(), feature]
        # Get unique labels and counts for the non-nan features
        labels, counts = np.unique(srs_notnull, return_counts=True)
        cum_counts = np.cumsum(counts)
        # Generate random numbers of size len(nan_idxs)
        rand_vals = np.random.randint(0, len(srs_notnull), size=len(nan_idxs))
        # Find out the largest number in cum_counts that each rand_val is less than
        label_idxs = [idx_greater_than(x, cum_counts) for x in rand_vals]
        # Get values corresponding to above index (could merge this with the above line to save a
        # pass through the array, but kept separate here for clarity)
        new_vals = [labels[idx] for idx in label_idxs]
        # Update the df with the new vals
        df.loc[nan_idxs, feature] = new_vals

if __name__ == "__main__":
    run()