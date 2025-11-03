      * COPYMEMBER: LIBLP/LPCDBCL
      ******************************************************************
      * CLEAR CDB FILE
      *
      * REV:
      *  BAH 01/31/2019 NEW, FOR COMBINED LAYOUT
      *   CS 190513 CLEAR FUTURE FIELDS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      ******************************************************************
       CLEAR-CDB-FILE SECTION.
           MOVE SPACES TO CDB-REC.

           MOVE 0 TO CDB-LTOUCH-DATE  
                     CDB-CN-PHONE CDB-CN-PHONE-EXT

                     CDB-GC-GLFEENO       CDB-GC-FEE-PRODNO           
                     CDB-GC-FEE-PRICE     CDB-GC-NEW-PRODNO           
                     CDB-GC-FEE-MAXAMT    CDB-GC-ALT-GLFEENO          
                     CDB-GC-COMM-GLFEENO  CDB-GC-COMM-PERCENT         
                     CDB-GC-BRANCH-COMM-AMOUNT.
