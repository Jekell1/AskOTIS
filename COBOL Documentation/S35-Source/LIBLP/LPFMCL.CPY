      * COPYMEMBER: LIBLP/LPFMCL
      ******************************************************************
      *
      *    LPFMCL   FORM REQUEST FILE CLEAR ROUTINE
      *
      * REV:
      *  2019.0613 NEW
      *  2020.0610 ADDED FM-LTOUCH-DATE
      *  BAH 20210427 REMOVED FUTURE FIELDS
      ******************************************************************
       CLEAR-FM-FILE SECTION.
           MOVE SPACES TO FM-REC.

           MOVE 0 TO FM-BRNO FM-FMNO
                     FM-STMT-COUNTER FM-TRDATE FM-LTOUCH-DATE.
