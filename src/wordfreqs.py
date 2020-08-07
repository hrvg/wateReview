import multiprocessing
from joblib import Parallel, delayed

ofile = open("./data/freqs.txt", 'w')

## read input files
vfile = open("./data/vocab.txt", 'r')
cfile = open("./data/corpus.dat", 'r')

#not concerned with memory usage here...
vocab = [word for word in vfile.readlines()]
corpus = [doc for doc in cfile.readlines()] 

#split the corpus into chunks to be processed independently
ncores = 40
chunk_size = len(corpus) // ncores
chunks = [corpus[x:x+chunk_size] for x in range (0, len(corpus), chunk_size)]

#handle individual chunk
def getwf(chunk):
    bow = {}
    collection = " ".join(chunk)
    words = collection.split(" ")
    for w in words:
        if w not in bow:
            bow[w] = 1
        else:
            bow[w] += 1
    return bow

results = (Parallel(n_jobs=ncores)(delayed(getwf)(chunk) for chunk in chunks))

for v in vocab:
    v = v.strip()
    count = 0
    for r in results:
        if v in r:
            count = count + r[v]
    print (count, file=ofile)

