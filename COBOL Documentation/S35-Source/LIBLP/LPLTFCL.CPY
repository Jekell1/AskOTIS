      * COPYMEMBER: LIBLP/LPLTFCL
      **************************************************************************
      *    LPLTFCL    FEE CODE PAYABLE TRAILER FILE CLEAR ROUTINE
      *
      * REV:
      *  07/02/2015 BAH NEW
      * 190603 CLEAR FUTURE FIELDS
      * 20190719 FUTURE FIELDS ONLY OCCUR 10, THIS HAD LTFSUB > 20, AND DONT
      *          MOVE 0 TO FEECODE2, ITS ALPHA!
      * AMM 20210429 REMOVED FUTURE FIELDS
      **************************************************************************
       CLEAR-LTF-FILE SECTION.
           MOVE SPACES TO LTF-REC.

           MOVE 0 TO LTF-LTOUCH-DATE.

           PERFORM VARYING LTFSUB FROM 1 BY 1 UNTIL LTFSUB > 10
              MOVE 0 TO LTF-FEECODE-PAYABLE(LTFSUB)
           END-PERFORM.
