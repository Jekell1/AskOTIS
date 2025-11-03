      * do not change case
       01  help-legend-line.
           03  using help-legend1
               col 1
               line 1.
       01  generalhelp-legend-line.
           03  using help-legend2
               col 1
               line 1
               reverse.
       01  help-screen-1.
           03  entry-field, line 2, col 1, value multiple help-table-1,
               lines 20, size 70 vscroll-bar, read-only,
               no-box, no-autosel, reverse.
       01  general-help-screen-1.
           03  entry-field, line 2, col 1, value multiple help-table-1,
               lines 20, size 70 vscroll-bar, read-only,
               no-box, no-autosel.
