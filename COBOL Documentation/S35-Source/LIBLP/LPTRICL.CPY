      * COPYMEMBER: LIBLP/LPTRICL
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPTRICL
      *
      * DESCRIPTION   :  TRANSACTION FILE CLEAR ROUTINE - TYPE 'I'
      *                    INSURANCE CLAIMS
      *
      *=================================================================
      * REV:
      * KEC 2005.0824 {PR#00419} WORLD <A15> [JIM ROSENAUER]
      *               ***NEW***
      * BAH 20210426 REMOVED FUTURE FIELDS
      ******************************************************************
       CLEAR-TRI-FILE SECTION.
           MOVE SPACES     TO TRI-REC.

           MOVE 0          TO
                TI-BRNO TI-KEY-DATE TI-SEQ
                TI-TYPE                     TI-AMOUNT
                TI-DATE                     TI-DRAFT
                TI-PRODUCER                 TI-POSTTIME
                TI-POSTDATE                 TI-INSCOMP.
           MOVE 0       TO TI-LTOUCH-DATE.


       CLEAR-TRI-FILE-EXIT.
           EXIT.
