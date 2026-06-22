"""Experimental Python preset using Ty, Pyrefly, ruff and codebook"""


def servers():
    return [
        ["ty", "server"],
        ["pyrefly", "lsp"],
        ["ruff", "server"],
        ["codebook-lsp", "serve"],
    ]
