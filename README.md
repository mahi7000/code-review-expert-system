# File Structure
```
python_rule_analyzer/
│
├── README.md
├── requirements.txt
│
├── prolog/                      # ALL PROLOG CODE
│   ├── main.pl                  # Entry point - Person 2
│   ├── rule_engine.pl           # Rule runner - Person 2
│   ├── explanations.pl          # All rule docs - Person 2
│   ├── reporter.pl              # Reports - Person 2
│   │
│   ├── rules/                   # RULES SPLIT BY PERSON
│   │   ├── style/               # PERSON 1
│   │   │   ├── rule1_line_length.pl
│   │   │   ├── rule2_docstring.pl
│   │   │   ├── rule3_naming.pl
│   │   │   ├── rule4_type_hints.pl
│   │   │   ├── rule5_unused_imports.pl
│   │   │   ├── rule6_todo.pl
│   │   │   └── rule7_magic_numbers.pl
│   │   │
│   │   ├── performance/         # PERSON 2
│   │   │   ├── rule8_range_len.pl
│   │   │   ├── rule9_string_concat.pl
│   │   │   ├── rule10_type_vs_isinstance.pl
│   │   │   ├── rule11_mutable_default.pl
│   │   │   ├── rule12_list_comprehension.pl
│   │   │   ├── rule13_open_without_with.pl
│   │   │   └── rule14_empty_except.pl
│   │   │
│   │   └── security/            # PERSON 1
│   │       ├── rule15_eval.pl
│   │       ├── rule16_pickle.pl
│   │       ├── rule17_shell_injection.pl
│   │       ├── rule18_creds.pl
│   │       ├── rule19_name_main.pl
│   │       └── rule20_exception.pl
│   │
│   └── utils/                   # SHARED UTILITIES
│       ├── file_loader.pl
│       └── formatter.pl
│
├── python_examples/             # TEST FILES
│   ├── bad_examples.py          # All violations - Both
│   ├── good_examples.py         # Fixed versions - Both
│   └── rule_tests/              # Per-rule tests
│       ├── style_tests.py       # Person 1
│       ├── performance_tests.py # Person 2
│       └── security_tests.py    # Person 1
│
├── tests/                       # TESTS
│   ├── test_all_rules.pl        # Integration - Both
│   ├── test_style.pl            # Person 1
│   ├── test_performance.pl      # Person 2
│   └── test_security.pl         # Person 1
│
└── demo/                        # DEMO FILES
    ├── demo.pl                  # Person 2
    ├── demo_code.py             # Demo Python file
    └── README_DEMO.md           # How to run demo
```