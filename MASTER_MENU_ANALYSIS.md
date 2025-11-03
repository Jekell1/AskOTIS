# Master Menu Screen Analysis

## Summary

Searched Azure Cognitive Search indexes for "MASTER MENU" related screens.

---

## Screen_Nodes Index

### Total Screens with "MASTER MENU": 15 screens

**Key Finding**: Only **1 screen is actually the LPMENU main menu**. The other 14 screens are submenu screens that have "F7 - MASTER MENU" as a **navigation option** to return to the main menu.

---

## The Actual LPMENU Main Menu

**Screen ID**: `EA66581C142E6BA08A83D2DC773BC990DB403C3C_2`

**Content**:
```
M A S T E R     M E N U
Posting for:

1. DAILY PROCESSING         6. END OF DAY
2. REPORTS                   7. END OF MONTH
```

**Summary Text**:
> "Screen EA66581C142E6BA08A83D2DC773BC990DB403C3C_SCREEN_2 represents a COBOL user interface screen. Users see the following menu options and interface elements on this screen: 1. DAILY PROCESSING, 2. REPORTS, 6. END OF DAY, 7. END OF MONTH, M A S T E R     M E N U, Posting for: These are the text prompts, menu selections, and interface elements displayed to users when this screen appears."

✅ **Has Vector**: Yes (1536-dim embedding)

---

## Other Screens (Not the Main Menu)

These 14 screens are **submenus** with "F7 - MASTER MENU" as a navigation link:

### Examples:

1. **Screen C5894A1FD0BC83AFABD36B83D40EFD436118CCA7_2**
   - Shows: "F6 - VIEW REPORTS", "F7 - MASTER MENU"
   - This is a navigation screen, not the actual master menu

2. **Screen BF7CB9C3422AD6417A40FDC2DF52ECB97B8EAF47_1**
   - Shows: "4. REFUND/CANCELLATION TABLE FILE", "5. DEALER STATISTICS FILE", etc.
   - Also has "F7 - MASTER MENU" navigation option
   - This is the **File Maintenance Menu** (a submenu)

3. **Screen FA010D40BF68A8DA99E477366B636D8B05DA387F_1**
   - Shows: "5. IRS REPORTING", "9. USER SECURITY MENU", etc.
   - Also has "F7 - MASTER MENU" navigation
   - This is another submenu (Reports/Admin menu)

---

## The Problem

When users ask "What are the main menu options?" and the RAG system searches for "MASTER MENU":

- **15 screens match** the search term "MASTER MENU"
- **14 of them are wrong** - they're submenus that reference the master menu as a navigation option
- **Only 1 is correct** - the actual LPMENU main menu screen

The RAG retrieval returns multiple screens, and the LLM gets confused because:
1. Most screens only show "F7 - MASTER MENU" (a button label)
2. The actual main menu content is diluted among 15 competing results
3. The summary text for LPMENU doesn't use strong enough keywords like "main menu" or "primary menu"

---

## Solution Needed

The router needs to:
1. **Distinguish** between "the MASTER MENU screen" (LPMENU) vs "screens that link to MASTER MENU"
2. **Boost** screens that have multiple numbered menu options (1, 2, 6, 7) as likely menu screens
3. **Filter out** screens that only have "F7 - MASTER MENU" as a navigation link
4. **Add query expansion** - when user asks "main menu options", also search for screen names with multiple numbered options

OR

Update screen node summaries to better distinguish:
- LPMENU summary should say: "This is the **main application menu** showing..."
- Other screens should say: "This is the [specific submenu name] with navigation back to MASTER MENU"

---

*Analysis Date: October 29, 2025*
