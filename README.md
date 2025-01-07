# Extract Markov chains from client and therapist categorical labels
This tool comes from a project exploring client-therapist interaction based on 
categorical labels from moment to moment. The tool extracts transition
probability matrix based on discrete time Markov chain from the categorical labels 
of both client and therapist. 

## Copy the repository
```bash
git clone https://github.com/XinY-Z/markov-chain.git
```

## Install the required packages
```R
install.packages("data.table")
install.packages("stringr")
```

## Data
The data should be in the format of a dataframe (preferrably data.table) 
with at least the following variables:  

- Session ID
- Speaker labels (e.g., T or P)
- Categorical labels (e.g., therapist's interventions, client's themes)  

The data should be sorted in chronological order.

## Usage
First, customize the options in `utils.R` to your own data. For example
```R
LABEL <- 'code'           # column name for categorical codes
SESSION_ID <- 'session'   # column name for session ID
SPEAKER <- 'speaker'      # column name for speaker
THERAPIST <- 'T'          # therapist label
CLIENT <- 'P'             # client label
CONCAT <- F               # consider codes that co-occur or not
# By setting CONCAT to TRUE, co-occurred codes are considered as a single code, 
# e.g., 'reflection,socratic' is considered one code instead of two separate codes.
```

Second, customize the options in `getMarkov.R` to your own data. For example
```R
INPUT_PATH <- <your data file path>
OUTPUT_PATH <- <your output file path>
SESSION_ID <- 'session'   # column name for session ID
TOPIC <- 'REC'            # focus on code REC
CONCAT <- F               # consider therapist codes that co-occur or not
STARTWITH <- 'P'          # consider only transition from client to therapist
# STARTWITH determines the direction of condition, e.g., STARTWITH=P means looking 
# at P -> T sequences.
```

Finally, run the script `getMarkov.R` to get the transition probability matrix.
```R
source('getMarkov.R')
# This will export the processed file to the output file path you specified.
```

## Example result
The first column is the session ID, the following columns are code sequences and their
transition probabilities, e.g., a probability of transitioning from MOOD code to ADW code
in session 8510 is 0.00311.
```txt
session     MOOD2ADP     MOOD2ADW     MOOD2AF     MOOD2CO      ...
   8430      0.00000      0.00000     0.02272     0.00000      ...
   8433      0.00000      0.00000     0.00000     0.00000      ...
   8472      0.00000      0.00000     0.00000     0.00000      ...
   8510      0.00000      0.00311     0.02180     0.00000      ...
    ...          ...          ...         ...         ...      ...
```

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
