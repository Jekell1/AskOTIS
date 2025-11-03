# Help Fields Index - Integration Analysis

## Overview
The `help_fields` index contains **26,212 field-level help entries** from **1,589 screen help files**. This provides context-sensitive documentation that enriches your existing screen and UI indexes.

---

## 🔗 Relationships to Existing Indexes

### 1. **`new_cobol_screen_nodes`** (PRIMARY LINK)
**Connection:** `screen_id` ↔ `program_id`/`screen_name`

**How they relate:**
- **help_fields.screen_id** (e.g., "LAPG01", "LAWI05", "1ONPI") represents the help file name
- **screen_nodes.screen_id** (e.g., "LONPF2_SCR1") is generated from COBOL SCREEN SECTION parsing
- **screen_nodes.program_id** (e.g., "LONPF2") often matches or relates to help file screen IDs

**Example Linkage:**
```
Help File: LAWI05
├─ Help Entry: LAWI05-010 → "Enter the AUTO TYPE (N) - New (U) - Used"
├─ Help Entry: LAWI05-020 → "Enter the DESCRIPTION of the vehicle"
└─ Help Entry: LAWI05-030 → "Enter the MAKE of the vehicle"

Screen Node: LAWI05_SCR1  
├─ program_id: "LAWI05"
├─ fields_json: ["AUTO-TYPE", "VEHICLE-DESC", "VEHICLE-MAKE"]
└─ summary_text: "Vehicle information entry screen"
```

**Value Add:**
- Help fields provide **user-facing descriptions** for fields
- Screen nodes provide **technical structure** (field names, positions)
- Together: Complete field documentation (technical name + business purpose)

---

### 2. **`new_cobol_ui_paths`** (INDIRECT LINK)
**Connection:** Via `screen_id` through navigation paths

**How they relate:**
- **ui_paths** shows navigation: Screen A → Screen B → Screen C
- **help_fields** documents what each screen does and what fields mean
- Together: Explain "how to get there" + "what to do when you arrive"

**Example:**
```
UI Path: START → LAPG01 → LAPG02 → LAPG03
         (Menu) → (Borrower 1) → (Borrower 2) → (Vehicle Info)

Help Fields:
├─ LAPG01: "Hit F1-F4 to toggle between four borrowers"
├─ LAPG02: Field 010 = "Enter SOCIAL SECURITY NUMBER"
└─ LAPG03: Field 020 = "Enter VEHICLE MAKE"
```

**Value Add:**
- Answer: "How do I enter a borrower's SSN?" → Path to LAPG02 + Field 010 help
- Answer: "What fields are on the borrower screen?" → Enumerate all LAPG02 help entries

---

### 3. **`new_cobol_menu_trees`** (INDIRECT LINK)
**Connection:** Menu options lead to screens with help documentation

**How they relate:**
- **menu_trees** shows hierarchical menu structure
- **help_fields** documents the screens those menus navigate to

**Example:**
```
Menu Tree:
└─ Loan Processing (LP)
   └─ Applications (AM)
      └─ Add Application
         → Launches LAPG01

Help Fields for LAPG01:
├─ GENERALHELP: "Application entry screen for loan applications"
├─ Field 010: "Enter SOCIAL SECURITY NUMBER"
└─ SEL: "Hit F7-KEY to go to options menu"
```

---

### 4. **`new_cobol_program_meta`** (PROGRAM LINK)
**Connection:** `screen_id` often matches `program_id`

**How they relate:**
- Many help file screen IDs correspond to program names
- Help fields document the **UI aspect** of programs
- Program meta documents the **code structure**

**Example:**
```
Program Meta: LAWI05
├─ program_type: "ONLINE-FORM"
├─ has_screen_section: true
├─ copybooks_used: ["LAWI05_SCN.CPY"]
└─ business_purpose: "Vehicle information maintenance"

Help Fields: LAWI05 (multiple entries)
├─ LAWI05-010: "Enter the AUTO TYPE"
├─ LAWI05-020: "Enter the DESCRIPTION"
└─ [100+ more field definitions]
```

**Value Add:**
- Answer: "What does program LAWI05 do?" → Combine program meta + help field summaries
- Answer: "What fields does LAWI05 have?" → List all LAWI05-* help entries

---

### 5. **`new_cobol_data_items`** (FIELD LINK)
**Connection:** Field names in help text may match data item names

**Potential linkage (requires matching):**
```
Help Field: LAPG02, Field 010
└─ "Enter the SOCIAL SECURITY NUMBER"

Data Item: APPL-SOC-SEC-NUM
└─ PIC X(9), Found in LAPG02 copybook

Linkage: "SOCIAL SECURITY NUMBER" text → APPL-SOC-SEC-NUM variable
```

**Challenge:** Help fields use numeric positions (010, 020), not variable names
**Solution:** Need to map field positions to data item names (future enhancement)

---

## 📊 Statistics & Coverage

### Module Distribution (by help entries):
| Module | Entries | Description |
|--------|---------|-------------|
| **LP** | 6,347 | Loan Processing (largest) |
| **SP** | 5,927 | Servicing/Payments |
| **GB** | 4,651 | General/Banking |
| **MN** | 4,119 | Menu/Navigation |
| **WI** | 1,685 | Workflow/Interface |
| **FX** | 1,004 | Functions |
| **GL** | 659 | General Ledger |
| **AP** | 493 | Applications |
| **NP** | 564 | Note Processing |
| **LS** | 281 | Loan Servicing |
| **AM** | 264 | Application Management |
| **UP** | 170 | Updates |
| **BC** | 32 | Batch/Collections |
| **CV** | 16 | Conversions |

### Field Type Distribution:
| Type | Count | Example |
|------|-------|---------|
| **other** | 16,819 | Complex identifiers (LAWI05-TILL, SEL(MSEL ~ 'HARD')) |
| **numeric_position** | 7,629 | Simple positions (010, 020, 030) |
| **system** | 1,630 | Special fields (SEL, GENERALHELP, TILL) |
| **program_field** | 100 | Program-specific (LAWI05-010) |
| **keyed_field** | 20 | Keyed fields (K010) |
| **selection** | 14 | Selection fields (SEL variants) |

### Validation Metadata:
- **1,895 required fields** (`MUST` or `REQUIRED` in help text)
- **430 optional fields** (can skip)
- **10 fields with scan windows** (F6-KEY lookup)
- **18 fields with validation codes** (e.g., (J)-Joint, (E)-Endorser)

---

## 🎯 Use Cases & Query Examples

### Use Case 1: Field-Level Help
**Query:** "What does field 010 mean in LAPG02?"

**Retrieval Strategy:**
1. Search `help_fields` with filter: `screen_id eq 'LAPG02' and field_id eq '010'`
2. Return: "Enter the SOCIAL SECURITY NUMBER for the given borrower. Hit F6-KEY to pull up a scan window..."

---

### Use Case 2: Screen Documentation
**Query:** "What fields are on the LAWI05 vehicle screen?"

**Retrieval Strategy:**
1. Search `help_fields` with filter: `screen_id eq 'LAWI05'`
2. Return all field entries (100+ fields)
3. Optionally join with `screen_nodes` for structure

**Combined Response:**
```
LAWI05 Vehicle Information Screen:
├─ Field 010: AUTO TYPE (N=New, U=Used)
├─ Field 020: VEHICLE DESCRIPTION
├─ Field 030: VEHICLE MAKE
├─ Field 040: VEHICLE MODEL
└─ [96 more fields...]

Screen Structure (from screen_nodes):
├─ Program: LAWI05
├─ Total Fields: 98
└─ Actions: Accept, Update, Delete
```

---

### Use Case 3: Navigation + Field Help
**Query:** "How do I enter a borrower's phone number?"

**Retrieval Strategy:**
1. Semantic search `help_fields` for "phone number"
2. Find: LAPG02 field 050 (HOME PHONE) and 051 (WORK PHONE)
3. Search `ui_paths` for paths leading to LAPG02
4. Return: Navigation path + field instructions

**Response:**
```
To enter borrower phone numbers:

1. Navigate: Main Menu → Loan Processing → Applications → Add Application
2. This opens screen LAPG02 (Borrower Information)
3. Fields:
   - Field 050: Enter HOME PHONE NUMBER (format: xxx-xxx-xxxx)
   - Field 051: Enter WORK PHONE NUMBER
   - Field 052: Enter EXTENSION for work phone
```

---

### Use Case 4: Function Key Documentation
**Query:** "What does F6 do?"

**Retrieval Strategy:**
1. Search `help_fields` where `function_keys` contains "F6"
2. Return all contexts where F6 is mentioned

**Response:**
```
F6-KEY usage across screens:
├─ LAPG02 Field 010: "Pull up scan window for SOCIAL SECURITY NUMBERs"
├─ LAPG02 Field 070: "Pull up scan window for valid LOAN CLASSES"
├─ LAPG02 Field 080: "Pull up scan window for valid SOURCE CODES"
└─ [127 more F6 usages...]

Summary: F6-KEY typically opens a lookup/scan window for valid values
```

---

### Use Case 5: Validation Rules
**Query:** "What are the valid loan application status codes?"

**Retrieval Strategy:**
1. Search `help_fields` for "status" + "valid" + semantic match
2. Find: LAPG02 Field 140 with validation codes

**Response:**
```
APPLICATION STATUS CODES (Field 140):
├─ (P) - Pending
├─ (C) - Conditional  
├─ (A) - Approved
├─ (M) - Made
├─ (T) - Turn down
├─ (U) - Turned us down
└─ (V) - Void
```

---

## 🔧 Integration Recommendations

### 1. **Add to Routing Profiles**
Update `otis_rag/router.py` to include `help_fields` in relevant profiles:

```python
ROUTING_PROFILES = {
    "menu": {
        "indexes": [
            "screen_nodes",      # Screen structure
            "help_fields",       # ← ADD: Field help text
            "ui_paths",
            "menu_trees",
            "programs"
        ],
        "index_weights": {
            "screen_nodes": 3.0,
            "help_fields": 2.5,  # ← ADD: Boost field help
            "ui_paths": 2.0
        }
    },
    "ui": {
        "indexes": [
            "screen_nodes",
            "help_fields",       # ← ADD
            "ui_paths"
        ],
        "index_weights": {
            "help_fields": 3.0   # ← ADD: Prioritize for UI questions
        }
    }
}
```

### 2. **Enhance Screen Queries**
When querying screens, join with help fields:

```python
def get_screen_documentation(screen_id: str):
    """Get complete screen documentation."""
    # Get screen structure
    screen = search_client.search(
        index="new_cobol_screen_nodes",
        filter=f"screen_id eq '{screen_id}'"
    )
    
    # Get field help
    help_entries = search_client.search(
        index="help_fields",
        filter=f"screen_id contains '{screen_id}'"
    )
    
    return {
        "structure": screen,
        "field_help": help_entries
    }
```

### 3. **Create Field Mapping**
Build a mapping between field positions and data item names:

```python
# Future: Parse SCREEN SECTION copybooks to map:
# Field 010 (line 5, col 10) → DATA-ITEM-NAME
# Then link: help_fields.field_id → data_items.name
```

### 4. **Add Specialized Queries**
Create convenience methods for common patterns:

```python
def get_field_help(screen_id: str, field_id: str):
    """Get help for a specific field."""
    return search_help_fields(f"screen_id eq '{screen_id}' and field_id eq '{field_id}'")

def get_screen_fields(screen_id: str):
    """Get all fields for a screen."""
    return search_help_fields(f"screen_id eq '{screen_id}'")

def find_fields_by_purpose(purpose: str):
    """Semantic search for fields by purpose."""
    return semantic_search_help_fields(purpose)
```

---

## 📈 Impact on RAG Queries

### Before `help_fields`:
**Q:** "What is field 010 in LAPG02?"
**A:** "I don't have specific information about field numbers."

### After `help_fields`:
**Q:** "What is field 010 in LAPG02?"
**A:** "Field 010 in LAPG02 is the SOCIAL SECURITY NUMBER field for the borrower. You can enter the SSN directly, or press F6 to open a scan window showing available social security numbers."

---

### Before:
**Q:** "How do I enter a borrower's phone number?"
**A:** [Searches code, finds variables like BORR-PHONE, but no context]

### After:
**A:** "To enter a borrower's phone number in the application screen (LAPG02):
- Field 050: HOME PHONE NUMBER
- Field 051: WORK PHONE NUMBER  
- Field 052: EXTENSION (for work phone)
All phone fields are optional and can be skipped by pressing RETURN."

---

## 🚀 Next Steps

1. ✅ **Index created:** `help_fields` (26,212 docs)
2. ✅ **Data uploaded:** All help entries indexed
3. ⏳ **Embeddings:** Backfill vectors for semantic search
4. ⏳ **Router integration:** Add to routing profiles
5. ⏳ **Testing:** Validate field-level queries
6. 🔮 **Future:** Map field positions to COBOL data item names

---

## Summary

The `help_fields` index provides the **missing link** between:
- **Technical structure** (screen_nodes, data_items) 
- **User documentation** (what fields mean and how to use them)

This enables your RAG system to answer **practical user questions** about forms, fields, and navigation that were previously difficult to address. It's particularly valuable for:
- Onboarding new users
- Training documentation
- Field-level help in modernized UIs
- Understanding business rules embedded in form validation
