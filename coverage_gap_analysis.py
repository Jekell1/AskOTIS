"""Analyze program coverage gaps across key indexes."""

print("🔍 PROGRAM COVERAGE GAP ANALYSIS")
print("=" * 50)

print("\n📊 EXPECTED vs ACTUAL COVERAGE:")
print(f"  • Total programs: 9,678 (from program_meta)")
print(f"  • Total copybooks: 7,817 (from copybook_meta)")
print(f"  • Expected total files: 9,951 CBL/CPY files")

print("\n❌ SIGNIFICANT COVERAGE GAPS IDENTIFIED:")

print("\n1️⃣ PROGRAM DEPENDENCIES:")
print(f"   • Current: 1,571 programs")
print(f"   • Expected: 9,678 programs") 
print(f"   • Gap: {9678 - 1571:,} programs missing ({(9678-1571)/9678*100:.1f}%)")

print("\n2️⃣ PROGRAM FLOWS:")
print(f"   • Current: 1,571 programs")
print(f"   • Expected: 9,678 programs")
print(f"   • Gap: {9678 - 1571:,} programs missing ({(9678-1571)/9678*100:.1f}%)")

print("\n3️⃣ COPYBOOK COVERAGE:")
print(f"   • Current: 7,817 copybooks")
print(f"   • Expected: Check if all CPY files represented")

print("\n4️⃣ FACTS COVERAGE:")
print(f"   • Current: 31 facts")
print(f"   • Expected: Likely needs significant expansion")

print("\n🎯 REQUIRED ACTIONS:")
print("1. Build missing program dependencies (8,107 programs)")
print("2. Build missing program flows (8,107 programs)")  
print("3. Verify copybook coverage completeness")
print("4. Expand facts coverage if needed")

print("\n⚠️  CURRENT STATUS: PARTIAL COVERAGE")
print("   Major indexes missing 83.8% of programs!")