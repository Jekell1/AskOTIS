      * COPYMEMBER: LIBLP/LPTRKCL

      **************************************************************************
      *    LPTRKCL    POSTING TRANSACTION FILE CLEAR ROUTINE
      *                   CONVERSION
      *
      * REV:
      *  MJD 112395 ADDED CLEAR FOR TK-REFBR.
      *  JTG 080129 CHANGED FOR JUDY/CS/JOHN TO ALLOW BULK-CONVERSION-FG
      *             TO TAKE UPFRONT EARNINGS                   FINAUTO     #3141
      *  BAH 160926 FIXED FOR NEW A30 LAYOUT
      * CS 161117 ADD TRK-SUBCLASS
      * BAH 20210426 REMOVED FUTURE FIELDS
      **************************************************************************
       CLEAR-TRK-FILE SECTION.
           MOVE SPACES TO TRK-REC.

           MOVE 0 TO TK-DATE TK-BRNO TK-CLASS TK-ACCTNO
                     TK-LAWCODE TK-LINE TK-LTOUCH-DATE TK-LOANDATE
                     TK-PROCEEDS TK-LNAMT TK-FINCHG TK-INTCHG
                     TK-SERCHG TK-EXTCHG TK-MAINTFEE TK-ORGTERM
                     TK-DLNO TK-DLRV TK-DLSPRV TK-DLDISC
                     TK-DLPARV TK-DLPARTRCAMT TK-PURBAL
                     TK-PRLNNO TK-DLPARVPAID TK-DLPARVHELD
                     TK-POSTDATE TK-POSTTIME TK-REFBR.
           MOVE 0 TO TK-SUBCLASS.
      *-----------------------------------------------------------------

           PERFORM CLEAR-TK-TABLES VARYING TRKSUB FROM 1 BY 1
                                    UNTIL TRKSUB > 10.
 
           GO TO CLEAR-TRK-FILE-EXIT.

       CLEAR-TK-TABLES.
           IF TRKSUB < 4
              MOVE 0 TO TK-DLRVPRIN(TRKSUB) TK-DLRVCHGS(TRKSUB).
           IF TRKSUB < 5
              MOVE 0 TO TK-SSNO(TRKSUB).

           MOVE 0 TO TK-UNEARN(TRKSUB)
                     TK-EARNED(TRKSUB)
                     TK-INSEARNED(TRKSUB)
                     TK-INSUNEARN(TRKSUB).

       CLEAR-TRK-FILE-EXIT.
           EXIT.
