      * COPYMEMBER: LIBLP/LPLTBCL
      **************************************************************************
      *    LPLTBCL    BANKRUPTCY TRAILER FILE CLEAR ROUTINE
      *
      * REV:
      *  07/01/2015 BAH NEW
      *  05/02/2016 BAH CLEAR NEW REAFF/DISCHARGE FIELDS, WORLD 1150
      *  09/25/2019 BAH ADDED LTBSUB INSTEAD OF USING SUB.
      *  20210421 BAH REMOVED FUTURE FIELDS
      **************************************************************************
       CLEAR-LTB-FILE SECTION.
           MOVE SPACES TO LTB-REC.

           MOVE 0 TO LTB-LTOUCH-DATE.

           MOVE 0 TO LTB-FILEDATE      LTB-CHAPTER       LTB-BDDATE 
                     LTB-CLAIM-AMT     LTB-INT-LC-DUE    LTB-OTH-DUE 
                     LTB-TDUEAMT.

          
           PERFORM VARYING LTBSUB FROM 1 BY 1 UNTIL LTBSUB > 3
              MOVE 0 TO LTB-PHONE(LTBSUB)
           END-PERFORM.

           MOVE 0 TO LTB-REAFF-DATE LTB-REAFF-SYSDATE
                     LTB-REAFF-BALANCE LTB-REAFF-PAYAMT
                     LTB-REAFF-NO-PAYS LTB-INTRATE
                     LTB-RESCIND-DATE LTB-DISCHARGE-DATE
                     LTB-DISCHARGE-SYSDATE LTB-GROSS-BAL.
