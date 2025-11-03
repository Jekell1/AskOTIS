      *-----------------------------------------------------------------
       OPEN-LP1-FILE.
           PERFORM LOAD-LP1-FILE.
           PERFORM OPEN-IT.
           MOVE LP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
      * ***NOTE*** THIS 'START' IS NOT FOR DISPLAYING MULTIPLE PAGES.
      * ONLY SHOULD BE USED ONCE PER PROGRAM/BRANCH/ACCOUNT LIKE IN
      * A STANDARD REPORT.  USE START-LP1-FILE-SCAN FOR DISPLAYING
      * MULTIPLE PAGES.
      *-----------------------------------------------------------------
