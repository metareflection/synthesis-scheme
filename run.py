import torch
from transformers import pipeline

device = torch.device("cuda") if torch.cuda.is_available() else torch.device("cpu")
pipe = pipeline(
    "text-generation", model="codeparrot-ds", device=device
)

txt = """
(define proper-list?
      (lambda (x lag)

"""
print(pipe(txt, num_return_sequences=1)[0]["generated_text"])
