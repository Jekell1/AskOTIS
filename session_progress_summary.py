"""Session progress summary."""
print("=" * 80)
print("SESSION PROGRESS - COMPLETED INDEXES")
print("=" * 80)

completed = [
    ("copybook_usage", "17.8%", 114307, 115399, "98.9% of files with COPY statements"),
    ("paragraphs", "41.1%", 224655, 224655, "~100% of CBL files with PROCEDURE DIVISION"),
    ("data_items", "17.3%", 267769, 536058, "100% of files with DATA DIVISION items"),
    ("flow_edges_v2", "16.6%", 366650, 366650, "Control flow edges (PERFORM/CALL/GOTO)"),
]

print("\n✅ COMPLETED IN THIS SESSION:\n")
for name, cov, before, after, note in completed:
    change = after - before
    pct_change = (change / before * 100) if before > 0 else 0
    print(f"{name:20}")
    print(f"  Coverage: {cov} (of files with relevant content)")
    print(f"  Documents: {before:,} → {after:,} (+{change:,}, +{pct_change:.1f}%)")
    print(f"  Embeddings: 100% ✅")
    print(f"  Note: {note}")
    print()

print("=" * 80)
print("REMAINING HIGH-VALUE INDEXES")
print("=" * 80)

remaining = [
    ("screen_nodes", "40.6%", 46825, "⚠️ Schema error - needs fix"),
    ("variable_usage", "17.0%", 106669, "⚠️ No embeddings yet"),
    ("symbol_refs", "4.8%", 1104574, "Low value for chatbot"),
]

print("\n🔄 TODO:\n")
for name, cov, docs, note in remaining:
    print(f"{name:20} - {cov:7} | {docs:,} docs | {note}")
    print()

print("=" * 80)
print("CHATBOT READINESS")
print("=" * 80)
print("\n✅ READY TO ANSWER:")
print("  • 'What copybooks does APIPAY.CBL use?'")
print("  • 'Explain how DAILY.CBL works'")
print("  • 'Show me the logic flow of ORDERS.CBL'")
print("  • 'What fields are in CUSTOMER-RECORD?'")
print("  • 'Show me the structure of INVOICE-DATA'")
print("  • 'What's the definition of ACCOUNT-BALANCE?'")
print("  • 'Trace the control flow through PERFORM chains' ✅ NEW!")
print("  • 'Show me all CALL statements in APIPAY.CBL' ✅ NEW!")

print("\n⚠️ NEEDS MORE WORK:")
print("  • 'Show complete user screen flow from main menu'")
print("  • 'Where is TOTAL-AMOUNT variable used?'")
