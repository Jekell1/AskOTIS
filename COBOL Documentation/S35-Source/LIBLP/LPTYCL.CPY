      * COPYMEMBER: LIBLP/LPTYCL
      ******************************************************************
      *    LPTYCL    TODAYS TOTALS FILE CLEAR ROUTINE
      *
      * REV:
      *  CHANGED TY-DEL BUCKETS TO OCCURS 10 (WAS 18).
      *  MJD 011696 ADDED 02, INT, SOLD, TRANS, PURCH, PL, AND DEALER
      *             RESERVE TBL CLEARS.
      *  MJD 012396 ADDED PL RECOVERY PRIN/INT/LATE COLLECTED
      *  MJD 012696 CHANGED DLR RESERVE TO A GAIN/LOSS-VOLUME
      *              TABLE. ADDED TY-PL-O2COL.
      *  KEC 030697 REVISED FOR LOAN APPLICATIONS.
      *  KEC 030697 ADDED CLEAR OF FIELDS: TY-INS-TBL & TY-MISC-INS-TBL.
      *  JTG 032097 ADDED CLEAR OF TY-NO-TURNUSDOWN
      *  KEC 051297 ADDED CLEAR OF TY-NO-MADE
      *  MJD 070297 ADDED CLEAR OF TY-DLDISC
      *  MJD 051898 ADDED CLEAR OF TY-UNITS BNKRPT AND TY-OUTBAL-BNKRPT.
      *             CHANGED EXREB TO BE PART-CHGOFF.
      *  JTG 000323 MODIFIED TO CLEAR UNEARNED STATISTICS, MERCURY #223
      *  JTG 000330 ADDED CLEAR OF TY-PROCEEDS, RE: MJD MISSED (MERC PR#223)
      *  MJD 000929 ADDED TY-LOCKBOX (MFC PR#237)
      *
      * KEC 190208 A32 F2SL - REMOVED THE TY-APP-ACTIVITY-TBL FIELDS
      *            SINCE LOAN APPS WAS REMOVED FROM THE BYFILE.
      *  CS 190429 CLEAR FUTURE FIELDS
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      **************************************************************************
       CLEAR-TY-FILE SECTION.
           MOVE SPACES TO TY-REC.

           MOVE 0 TO TY-BRNO TY-DATE TY-CLASS TY-LTOUCH-DATE.

      * TY-CKWRITTEN:
           MOVE 0 TO TY-CKWRITTEN(1)  TY-CKWRITTEN(2)
                     TY-CKWRITTEN(3)  TY-CKWRITTEN(4)
                     TY-CKWRITTEN(5)  TY-CKWRITTEN(6)
                     TY-CKWRITTEN(7)  TY-CKWRITTEN(8)
                     TY-CKWRITTEN(9)  TY-CKWRITTEN(10)
                     TY-CKWRITTEN(11) TY-CKWRITTEN(12)
                     TY-CKWRITTEN(13) TY-CKWRITTEN(14)
                     TY-CKWRITTEN(15) TY-CKWRITTEN(16).

      * TY TODAYS NO'S:

           PERFORM CLEAR-TY-PL-RECOVERY
                   VARYING TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 5.

           MOVE 0 TO TY-NO-SOLD  TY-AMT-SOLD
                     TY-NO-TRANS TY-AMT-TRANS
                     TY-NO-PURCH TY-AMT-PURCH
                     TY-OTHCOL   TY-OT2COL
                     TY-INTCOL   TY-LATECOL  TY-PRINCOL
                     TY-DEFCOL   TY-INTREB   TY-CLREB1
                     TY-CLREB2   TY-CLREB3   TY-CLREB4
                     TY-AHREB    TY-PPREB    TY-SCREB
                     TY-O1REB    TY-O2REB
                     TY-O3REB    TY-O4REB    TY-O5REB
                     TY-O6REB    TY-O7REB    TY-RNPRIN
                     TY-MFREB    TY-DFREB    TY-PART-CHGOFF
                     TY-UN7-REB  TY-UN8-REB TY-UN9-REB
                     TY-UN10-REB
                     TY-ALLOTMENTS
                     TY-LOCKBOX
                     TY-PROCEEDS.

           MOVE 0 TO TY-INS-NO-ELIG(1)
                     TY-INS-NO-WROTE(1)
                     TY-INS-NO-NOTWROTE(1)
                     TY-INS-AMT-WROTE(1).

           MOVE TY-INS-TYPE(1) TO TY-INS-TYPE(2)
                                  TY-INS-TYPE(3)
                                  TY-INS-TYPE(4)
                                  TY-INS-TYPE(5)
                                  TY-INS-TYPE(6)
                                  TY-INS-TYPE(7)
                                  TY-INS-TYPE(8)
                                  TY-INS-TYPE(9)
                                  TY-INS-TYPE(10).

           PERFORM VARYING TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 3
                 MOVE 0 TO TY-FEE-NO-ELIG(TYSUB1)
                 MOVE 0 TO TY-FEE-NO-WROTE(TYSUB1)
                 MOVE 0 TO TY-FEE-NO-NOTWROTE(TYSUB1)
                 MOVE 0 TO TY-FEE-AMT-WROTE(TYSUB1)
           END-PERFORM.

           MOVE 0 TO TY-OTH-NO-COL
                     TY-OT2-NO-COL
                     TY-INT-NO-COL
                     TY-LATE-NO-COL
                     TY-OTH-NO-WAIVED
                     TY-OT2-NO-WAIVED
                     TY-INT-NO-WAIVED
                     TY-LATE-NO-WAIVED
                     TY-OTH-AMT-WAIVED
                     TY-OT2-AMT-WAIVED
                     TY-INT-AMT-WAIVED
                     TY-LATE-AMT-WAIVED.

      * TY-ACTIVITY-TBL:
           PERFORM CLEAR-TY-ACTIVITY-TBL VARYING
              TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 11
                AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

      * TY-DEL-BUCKETS:
           PERFORM CLEAR-TY-DEL-BUCKETS VARYING
              TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 10
                AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

      * TY-REPO-ACTIVITY-TBL:
           PERFORM CLEAR-TY-REPO-ACTIVITY-TBL VARYING
               TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 5
                  AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

      * TY-PL-ACTIVITY-TBL:
           PERFORM CLEAR-TY-PL-ACTIVITY-TBL VARYING
               TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 10
                  AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

      * TY-DLR-ACTIVITY-TBL:
           PERFORM CLEAR-TY-DLR-ACTIVITY-TBL VARYING
               TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 3
                  AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

      * TY-APP-PERIOD-TBL:
      *    PERFORM CLEAR-TY-APP-PERIOD-TBL VARYING
      *        TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 11
      *           AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

      * TY-UNEARNED-ACTIVITY:
           PERFORM VARYING TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 10
                     AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2
               MOVE 0 TO TY-UNEARNED-AMT(TYSUB1 TYSUB2)
           END-PERFORM.

      * TY SUMWGT:
           MOVE 0 TO TY-SUMWGTTM
                     TY-SUMWGTAV
                     TY-SUMWGTTA.

           GO TO CLEAR-TY-FILE-EXIT.
      * ----------------------------------------------------

       CLEAR-TY-PL-RECOVERY.
              MOVE 0 TO TY-PL-PRINCOL(TYSUB1)
                        TY-PL-INTCOL(TYSUB1)
                        TY-PL-LATECOL(TYSUB1)
                        TY-PL-O2COL(TYSUB1).

       CLEAR-TY-ACTIVITY-TBL.
           MOVE 0 TO TY-NO(TYSUB1 TYSUB2)
                     TY-AMT(TYSUB1 TYSUB2)
                     TY-O2(TYSUB1 TYSUB2)
                     TY-INT(TYSUB1 TYSUB2).

       CLEAR-TY-DEL-BUCKETS.
           MOVE 0 TO TY-UNITS(TYSUB1 TYSUB2)
                     TY-OUTBAL(TYSUB1 TYSUB2)
                     TY-UNITS-BNKRPT(TYSUB1 TYSUB2)
                     TY-OUTBAL-BNKRPT(TYSUB1 TYSUB2).

       CLEAR-TY-REPO-ACTIVITY-TBL.
           MOVE 0 TO TY-NO-REPO(TYSUB1 TYSUB2)
                     TY-AMT-REPO(TYSUB1 TYSUB2)
                     TY-O2-REPO(TYSUB1 TYSUB2)
                     TY-INT-REPO(TYSUB1 TYSUB2).

       CLEAR-TY-PL-ACTIVITY-TBL.
           MOVE 0 TO TY-NO-PL(TYSUB1 TYSUB2)
                     TY-AMT-PL(TYSUB1 TYSUB2)
                     TY-O2-PL(TYSUB1 TYSUB2)
                     TY-INT-PL(TYSUB1 TYSUB2).

       CLEAR-TY-DLR-ACTIVITY-TBL.
           MOVE 0 TO TY-DLRVPRIN(TYSUB1 TYSUB2)
                     TY-DLRVCHGS(TYSUB1 TYSUB2)
                     TY-DLPARVHELD(TYSUB1)
                     TY-DLDISC.


      *CLEAR-TY-APP-PERIOD-TBL.
      *    MOVE 0 TO TY-NO-PENDING(TYSUB1 TYSUB2 )
      *              TY-NO-COND-APP(TYSUB1 TYSUB2 )
      *              TY-NO-APPROVED(TYSUB1 TYSUB2 )
      *              TY-NO-MADE(TYSUB1 TYSUB2 )
      *              TY-NO-TURNDOWN(TYSUB1 TYSUB2 )
      *              TY-NO-TURNUSDOWN(TYSUB1 TYSUB2 ).

       CLEAR-TY-FILE-EXIT.
           EXIT.
