import datasets

d = datasets.load_dataset("bigcode/the-stack-dedup", data_dir="data/scheme", split="train")
r = d[1]
