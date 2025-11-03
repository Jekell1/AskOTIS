"""
Complete analysis of all CALL statements in APIPAY.CBL
"""

print("=" * 80)
print("ALL CALL STATEMENTS IN APIPAY.CBL")
print("=" * 80)
print()

calls = [
    (347, "FORM-PROGX", "CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME."),
    (1437, "FORM-PROGX", "CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME"),
    (1481, "FORM-PROGX", "CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME"),
    (1646, "FORM-PROGX", "CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME"),
    (1663, "FORM-PROGX", "CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME"),
    (1709, "FORM-PROGX", "CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME PROG-BUF"),
    (1726, "FORM-PROGX", "CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME"),
    (1880, "FORM-PROGX", "CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME"),
    (1945, "FORM-PROGX", "CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME"),
    (1964, "FORM-PROGX", "CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME"),
    (2024, "FORM-PROGX", "CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME"),
    (2294, "FORM-PROGX", "CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME"),
    (2563, "C$MAKEDIR", 'CALL "C$MAKEDIR" USING DIR-ACCESS-BUF GIVING STAT-99'),
]

# Summary
unique_programs = {}
for line, program, code in calls:
    if program not in unique_programs:
        unique_programs[program] = []
    unique_programs[program].append(line)

print(f"Total CALL statements: {len(calls)}")
print(f"Unique programs called: {len(unique_programs)}")
print()

print("=" * 80)
print("SUMMARY BY PROGRAM")
print("=" * 80)
print()

for program, lines in sorted(unique_programs.items()):
    print(f"{program}")
    print(f"  Called {len(lines)} time(s)")
    print(f"  Lines: {', '.join(map(str, lines))}")
    print()

print("=" * 80)
print("DETAILED LIST")
print("=" * 80)
print()

for i, (line, program, code) in enumerate(calls, 1):
    print(f"{i:2}. Line {line:4}: {code}")

print()
print("=" * 80)
print("NOTES")
print("=" * 80)
print()
print("✅ All CALL statements found in source code")
print()
print("Programs called:")
print("  1. FORM-PROGX    - Form processing (called 12 times)")
print("  2. C$MAKEDIR     - System call to create directories (called 1 time)")
print()
print("Total: 2 unique programs, 13 total CALL statements")
print()
print("Note: The 'calls' index had 17 entries including false positives from")
print("      comments (lines 627-628 with 'CALL TO' in comments, and line 1081")
print("      with 'GP-PAYOFF-NONCASH' in a comment).")
