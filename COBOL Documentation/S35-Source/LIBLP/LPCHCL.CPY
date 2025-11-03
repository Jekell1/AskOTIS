      * COPYMEMBER: LIBLP/LPCHCL
      ******************************************************************
      *
      *    LIBLP/LPCHCL:  CLEAR CHANGES RECORD
      *
      *-----------------------------------------------------------------
      * REV:
      *  BAH 161101 NEW
      ******************************************************************
       CLEAR-CH-FILE SECTION.
           MOVE SPACES TO CH-REC.
           MOVE 0 TO CH-BRNO CH-DATE CH-TIME CH-SEQ CH-LTOUCH-DATE
                     CH-ACCTNO CH-SSNO CH-TYPE.
