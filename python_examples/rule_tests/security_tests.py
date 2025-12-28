# Security violations test file

import os
import pickle
import subprocess

# Rule 15: eval() usage
x = 10
result = eval("x + 5")  # BAD!

# Rule 16: pickle.load()
with open('data.pkl', 'rb') as f:
    data = pickle.load(f)  # DANGEROUS!

# Rule 17: Shell injection
user_input = input("Enter filename: ")
os.system("rm " + user_input)  # SHELL INJECTION!
subprocess.call("ls " + user_input, shell=True)  # ALSO BAD!

# Rule 18: Hardcoded credentials
password = "supersecret123"  # BAD!
api_key = "abcdef123456"
token = "xyz789"

# Rule 19: Missing if __name__ == "__main__"
print("This runs when imported!")  # Should be guarded

# Rule 20: Bare except
try:
    risky_operation()
except:  # BARE EXCEPT!
    print("Caught everything")

# This is OK - uses environment variables
db_password = os.getenv("DB_PASS")