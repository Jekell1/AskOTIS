      * COPYMEMBER: LIBLP/LPLTLCL
      **************************************************************************
      *    LPLTLCL    LIENHOLDER TRAILER FILE CLEAR ROUTINE
      *
      * REV:
      *  06/19/2015 BAH NEW
      * AMM 20210429 REMOVED FUTURE FIELDS
      **************************************************************************
       CLEAR-LTL-FILE SECTION.
           MOVE SPACES TO LTL-REC.

           MOVE 0 TO LTL-LTOUCH-DATE.

           PERFORM VARYING LTLSUB FROM 1 BY 1 UNTIL LTLSUB > 3
              MOVE 0 TO LTL-LIEN-DATE(LTLSUB)
                        LTL-LIEN-TYPE(LTLSUB)
           END-PERFORM.

           MOVE 0 TO LTL-TITLE-SEND-FMNO
                     LTL-TITLE-DATE
                     LTL-TITLE-LIEN-DATE LTL-TITLE-RCV-DATE.

