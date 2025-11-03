"""Add menu filtering method to retriever.py"""

new_method = '''    
    def _filter_menu_screens(self, results: List[Dict[str, Any]], query: str) -> List[Dict[str, Any]]:
        """Filter menu screen results to prioritize actual menu content over navigation screens.
        
        When searching for menus, many results contain "F7 - MASTER MENU" which is just
        a navigation link back to the menu from submenus. This filters to prioritize
        screens that have actual menu options (numbered items like "1. DAILY PROCESSING").
        
        Args:
            results: List of screen_nodes results
            query: Original query
            
        Returns:
            Filtered and reordered results with actual menus first
        """
        import re
        
        menu_screens = []  # Screens with numbered menu options
        navigation_screens = []  # Screens with only F-key navigation
        
        for result in results:
            summary = result.get('summary_text', '')
            
            # Count numbered menu options (1. through 99.)
            numbered_options = len(re.findall(r'\\b(\\d{1,2})\\.\\s+[A-Z]', summary))
            
            # Check for F-key navigation patterns
            has_f_key_nav = bool(re.search(r'F\\d+\\s*-\\s*(MASTER\\s+)?MENU', summary, re.IGNORECASE))
            
            # Classify the screen
            if numbered_options >= 3:
                # Has at least 3 numbered options - likely an actual menu screen
                menu_screens.append(result)
                logger.debug(f"  ✓ Menu screen: {result.get('screen_id', '')[:20]}... ({numbered_options} options)")
            elif numbered_options >= 1 and has_f_key_nav:
                # Has some options AND navigation - could be either, keep it but lower priority
                menu_screens.append(result)
                logger.debug(f"  ~ Mixed screen: {result.get('screen_id', '')[:20]}... ({numbered_options} options + F-key)")
            elif has_f_key_nav and numbered_options == 0:
                # Only has F-key navigation - likely just a submenu screen linking back
                navigation_screens.append(result)
                logger.debug(f"  ✗ Navigation screen: {result.get('screen_id', '')[:20]}... (F-key only)")
            else:
                # Unknown pattern, keep it with menu screens to be safe
                menu_screens.append(result)
        
        logger.info(f"📋 Menu filtering: {len(menu_screens)} menu screens, {len(navigation_screens)} navigation screens")
        
        # Return actual menu screens first, then navigation screens
        # This ensures the LLM sees real menu options before navigation links
        return menu_screens + navigation_screens
'''

# Read file
with open('otis_rag/retriever.py', 'r', encoding='utf-8') as f:
    content = f.read()

# Find the location after "return final_results" in the retrieve method
# Insert before _expand_submenus
marker = "    def _expand_submenus(self, results: List[Dict[str, Any]], query: str) -> List[Dict[str, Any]]:"
if marker in content:
    content = content.replace(marker, new_method + "\n" + marker)
    print("✓ Method inserted successfully")
else:
    print("✗ Marker not found")
    exit(1)

# Write back
with open('otis_rag/retriever.py', 'w', encoding='utf-8') as f:
    f.write(content)

print("✓ File updated")
