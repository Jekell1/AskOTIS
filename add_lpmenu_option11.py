"""
Add option 11 (ALTERNATE BRANCH LOGIN) to LPMENU in scn_files_analysis.json.
This option is added dynamically at runtime and isn't in the screen definition.
"""
import json

# Load the analysis file
with open('scn_files_analysis.json', 'r', encoding='utf-8') as f:
    data = json.load(f)

# Find LPMENU
for screen in data['scn_files']:
    if 'LPMENU' in screen['filename']:
        print(f"Found: {screen['filename']}")
        
        # Update menu_screen_info
        current_menu = screen['menu_screen_info']
        
        # Add option 11 after option 10
        updated_menu = current_menu.replace(
            '10. OPTIONAL MODULES',
            '10. OPTIONAL MODULES\n11. ALTERNATE BRANCH LOGIN'
        )
        
        screen['menu_screen_info'] = updated_menu
        
        # Update description to note dynamic option
        if 'Note:' not in screen['description']:
            screen['description'] = screen['description'].rstrip() + '\n\nNote: Option 11 (ALTERNATE BRANCH LOGIN) is added dynamically at runtime by the LPMENU program and is not visible in the static screen definition file.'
        
        print("\nUpdated menu info:")
        print(screen['menu_screen_info'][:600])
        break

# Save updated file
with open('scn_files_analysis.json', 'w', encoding='utf-8') as f:
    json.dump(data, f, indent=2, ensure_ascii=False)

print("\n✅ Updated scn_files_analysis.json")
print("Next step: Run 'python upload_unified_screens.py' to update Azure Search")
