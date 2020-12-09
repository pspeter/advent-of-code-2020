from functools import reduce

# %%
def read(i): 
    return open(f"/mnt/c/users/peter/Downloads/lvl{i}.txt").read()

lvl6 = read(6)

groups = lvl6.split("\n\n")
# %%

sum(
    len(reduce(
            lambda a, b: a.intersection(b), 
            (set(a) for a in g.split("\n"))
    ))
     for g in groups
)