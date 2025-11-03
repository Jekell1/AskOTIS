"""
Transaction Codes in APIPAY - Answer based on source code analysis
"""

print("=" * 80)
print("TRANSACTION CODES IN APIPAY")
print("=" * 80)
print()
print("Based on APIPAY.CBL header comments (lines 25-30):")
print()

transaction_codes = [
    ("RP", "Rebate fix (for 'IN' rebate)", "LONPFB"),
    ("PL", "Payment posting (requires BT-PL-REASON)", "LONPF9"),
    ("P2", "Payment type 2 (requires BT-PL-REASON)", "LONPF9"),
    ("P3", "Payment type 3 (requires BT-PL-REASON)", "LONPF9"),
    ("RE", "Repo (repossession)", "LONPF7"),
    ("O2", "Other transaction type 2", "LONPF7"),
    ("RV", "Reversal", "LONPF2"),
    ("BK", "Bankruptcy", "LONPF2"),
    ("BD", "Bad debt", "LONPF2"),
    ("PY", "Payment (standard)", "LONPFC"),
    ("PA", "Payment adjustment", "LONPFC"),
    ("OT", "Other transaction", "LONPF7"),
]

print("┌────────┬──────────────────────────────────────────┬──────────────┐")
print("│  Code  │ Description                              │ Handler      │")
print("├────────┼──────────────────────────────────────────┼──────────────┤")

for code, desc, handler in transaction_codes:
    print(f"│  {code:4}  │ {desc:40} │ {handler:12} │")

print("└────────┴──────────────────────────────────────────┴──────────────┘")
print()

print("NOTES:")
print("-" * 80)
print()
print("1. These transaction codes determine which loan processing logic")
print("   (LONPF* program) APIPAY calls")
print()
print("2. PL, P2, P3 require additional BT-PL-REASON field in input string")
print()
print("3. The codes are passed in the input data string when running APIPAY:")
print("   Format: ./APIPAY.SH (BT-REC data) (REPAY TRANS ID)")
print("   Example: 00100786740000001000CUACH090123OT 1234567890")
print("                                       ^^")
print("                                       Transaction code position")
print()
print("4. Source: APIPAY.CBL lines 25-30 (header comments)")
print()

print("=" * 80)
print("LOAN PROCESSING HANDLERS")
print("=" * 80)
print()

handlers = {
    "LONPFB": ["RP"],
    "LONPF9": ["PL", "P2", "P3"],
    "LONPF7": ["RE", "O2", "OT"],
    "LONPF2": ["RV", "BK", "BD"],
    "LONPFC": ["PY", "PA"],
}

for handler, codes in sorted(handlers.items()):
    print(f"{handler}: {', '.join(codes)}")

print()
print("=" * 80)
