      * COPYMEMBER: LIBLP/LPTRBCL

      ***********************************************************************
      *    LPTRBCL    POSTING TRANSACTION FILE CLEAR ROUTINE
      *
      * REV:
      *  MJD 112395 ADDED CLEAR FOR TB-REFBR.
      *  MJD 091097 FIXED BUG; CLEAR-TB-TABLES IS GETTING PERFORMED TO MANY
      *             TIMES. AFTER PERFORM, IT IS FALLING THROUGH A SEVENTH TIME.
      *   CS 161003 REMOVED TB-SEQ, CHANGED TP-NUMBER TO TP-ACCTNO
      *             REMOVED CLEAR OF FEE TABLE, INSURANCE TABLE
      *   CS 161117 ADD TB-SUBCLASS
      * BAH 20210423 REMOVED FUTURE FIELDS
      ***********************************************************************
       CLEAR-TRB-FILE SECTION.
           MOVE SPACES TO TRB-REC.

           MOVE 0 TO TB-DATE TB-BRNO TB-CLASS TB-ACCTNO 
                     TB-LAWCODE TB-LINE TB-LOANDATE TB-LTOUCH-DATE
                     TB-PROCEEDS TB-LNAMT TB-FINCHG TB-INTCHG
                     TB-SERCHG TB-EXTCHG TB-MAINTFEE TB-ORGTERM
                     TB-DLNO TB-DLRV TB-DLSPRV TB-DLDISC
                     TB-DLPARV TB-DLPARTRCAMT TB-PURBAL
                     TB-REF-ACCTNO TB-DLPARVPAID TB-DLPARVHELD
                     TB-POSTDATE TB-POSTTIME TB-REFBR.

           MOVE 0 TO TB-SUBCLASS.
           PERFORM CLEAR-TB-TABLES VARYING TRBSUB FROM 1 BY 1
                                    UNTIL TRBSUB > 10.

           GO TO EXIT-CLEAR-TB-FILE.

       CLEAR-TB-TABLES.
           IF TRBSUB < 4
              MOVE 0 TO TB-DLRVPRIN(TRBSUB) TB-DLRVCHGS(TRBSUB).

           IF TRBSUB < 5
              MOVE 0 TO TB-SSNO(TRBSUB).

           MOVE 0 TO TB-UNEARN(TRBSUB)
                     TB-INSUNEARN(TRBSUB).

       EXIT-CLEAR-TB-FILE.
           EXIT.
