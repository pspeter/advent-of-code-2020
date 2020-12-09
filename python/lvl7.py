# %%
import re
from itertools import groupby


# %%
def read(i): 
    return open(f"/mnt/c/users/peter/Downloads/lvl{i}.txt").read()

lvl7 = read(7)

# %%
rules = dict(l.split(" bags contain ") for l in lvl7.splitlines())
parsed_rules = {a: re.findall(r"(\w[\w ]*) bag", b) for a, b in rules.items()}

# %% Part 1
rev_rules = [
    (inner_bag if inner_bag == "no other" else inner_bag[2:], outer_bag)
     for outer_bag, inner_bags in parsed_rules.items()
      for inner_bag in inner_bags
]
inner_to_outer = {a: list(outer for inner, outer in b) for a, b in groupby(sorted(rev_rules), key=lambda x: x[0])}

# %%
def num_bags_containing(color):
    to_check = set(inner_to_outer[color])
    checked = set()

    while to_check:
        inner = to_check.pop()
        can_be_in = set(inner_to_outer[inner]) if inner in inner_to_outer else set()
        new_outers = can_be_in - checked
        to_check |= new_outers
        checked.add(inner)
        print(f"checked {inner}, can be in {can_be_in}, new are {new_outers}")

    return len(checked) 

num_bags_containing("shiny gold")


# %% Part 2

def parse_inner(inner):
    amount, *bag = inner.split(" ")
    amount = int(amount)
    bag = " ".join(bag)
    return amount, bag

outer_to_inner = {outer: [parse_inner(inner) for inner in inners if inner != "no other"] for outer, inners in parsed_rules.items()}


# %%

def num_contained(bag):
    contained_bags = outer_to_inner[bag]
    total = 0
    for n, inner_bag in contained_bags:
        total += n  # the bags themselves
        total += n * num_contained(inner_bag)  # their contents

    return total

num_contained("shiny gold")

# %%
