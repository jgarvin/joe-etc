#!/usr/bin/env python

import argparse
import tiktoken

def estimate_claude_tokens(text):
    """
    This is an approximation using the GPT-2 tokenizer.
    Actual token count may vary slightly.
    """
    encoder = tiktoken.get_encoding("gpt2")
    tokens = encoder.encode(text)
    return len(tokens)

def read_file(filename):
    """Read content from a file."""
    with open(filename, 'r') as file:
        return file.read()

def main():
    parser = argparse.ArgumentParser(description="Estimate token count for Claude API")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("-s", "--string", help="Input string to tokenize")
    group.add_argument("-f", "--file", help="Input file to tokenize")

    args = parser.parse_args()

    if args.string:
        content = args.string
        print("Processing input string")
    else:
        content = read_file(args.file)
        print(f"Reading from file: {args.file}")

    estimated_tokens = estimate_claude_tokens(content)
    print(f"Estimated token count: {estimated_tokens}")

if __name__ == "__main__":
    main()
