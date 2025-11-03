      * COPYMEMBER: LIBLP/LPLXCL
      **************************************************************************
      *    LPLXCL    LOAN PAYMENT TRAILER CLEAR
      *
      * REV:
      *  01/15/2019 BAH NEW
      *  CS 190502 CLEAR FUTURE FIELDS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      **************************************************************************
       CLEAR-LX-FILE SECTION.
           MOVE SPACES TO LX-REC.

           MOVE 0 TO LX-LTOUCH-DATE.

           PERFORM VARYING LX-SUB FROM 1 BY 1 UNTIL LX-SUB > 20
              MOVE 0 TO LXT-EARN(LX-SUB)
                        LXP-EARN(LX-SUB)
           END-PERFORM.
           PERFORM VARYING LX-SUB FROM 1 BY 1 UNTIL LX-SUB > 9
              MOVE 0 TO LXA-START-PMT(LX-SUB)
                        LXA-END-PMT(LX-SUB)
                        LXA-NET-CHANGE(LX-SUB)
           END-PERFORM.

           MOVE 0 TO LXA-LN-INSCOMP LXA-LN-INSEFF-DATE
                     LXA-LN-INSEXP-DATE LXA-LN-INSPREM
                     LXA-LN-INSCOVR LXA-LN-INSCOMM.

           MOVE 0 TO LXZ-POST-REGPY LXZ-POST-EXTRA-PRIN LXZ-POST-OTH        
                     LXZ-POST-LCHG  LXZ-PDUE-APINT LXZ-PDUE-APPRIN     
                     LXZ-CUR-APINT  LXZ-CUR-APPRIN.

           MOVE 0 TO LXR-REB-AMT.

           MOVE 0 TO LXM-MIP-CLPAID LXM-MIP-CLPDTH-DATE
                     LXM-MIP-AHPAID LXM-MIP-AHPDTH-DATE.
