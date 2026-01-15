"""Python preset using basedpyright and codebook"""


def servers():
    return [
        ["basedpyright-langserver", "--stdio"],
        ["codebook-lsp", "serve"],
    ]
