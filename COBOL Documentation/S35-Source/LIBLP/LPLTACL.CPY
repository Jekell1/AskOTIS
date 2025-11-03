      * COPYMEMBER: LIBLP/LPLTACL
      **************************************************************************
      *    LPLTACL    AUTO INSURANCE TRAILER FILE CLEAR ROUTINE
      *
      * REV:
      *  07/01/2015 BAH NEW
      *  190506 CLEAR FUTURE FIELDS
      *  BAH 20210426 REMOVED FUTURE FIELDS
      **************************************************************************
       CLEAR-LTA-FILE SECTION.
           MOVE SPACES TO LTA-REC.

           MOVE 0 TO LTA-LTOUCH-DATE LTA-PHONE.

           MOVE 0 TO LTA-EFF-DATE    LTA-EXP-DATE    LTA-CAN-DATE    
                     LTA-CONF-DATE   LTA-LAST-FORM   LTA-BIL-COV     
                     LTA-PDL-COV     LTA-PIP-COV     LTA-UMB-COV     
                     LTA-COL-COV     LTA-COM-COV.

