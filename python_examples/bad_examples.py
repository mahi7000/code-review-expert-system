# This file has many violations for testing

import os  # unused import
import sys
from collections import defaultdict

myBadVariable = 42  # bad naming - should be caught

def calculate(a,b,c):  # no type hints, no docstring
    result = a * b + c * 999  # magic number 999
    if result > 500:  # magic number 500
        return "big"
    else:
        return "small"

class bad_class_name:  # bad naming
    def __init__(self):
        self.data = []

def long_function_name_with_many_characters_that_exceeds_the_line_limit_of_79_chars(x):
    return x * 99  # magic number 99

# TODO: Fix this later
# FIXME: This needs work