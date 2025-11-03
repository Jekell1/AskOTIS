      * COPYMEMBER: LIBLP/LPSTAT
      ***********************************************************
      *   NAME:  LPSTAT 05/23/88
      *   DESC:  SETUP OF LOAN NUMBER FOR DISPLAY OR PRINT
      *          INSERT LPS-STATUSFLAGS INTO POSITIONS
      *          10 - 16 OF DISPLAY AREA.
      *   OUT :  LPS-DISPLAY-LNNO.
      *
      * * NOTE: NEEDS BW-REC FOR PRIMARY MAKER (LN-SSNO(1))
      *
      *      STATUS CODES
      *        A - ACCELERATE
      *        B - BANKRUPT
      *        C - CEASE & DESIST
      *        D - DEBTAID
      *        E - EXPIRED
      *        J - JUDGEMENT
      *        L - LEGAL
      *        N - NON-ACCRUAL STATUS
      *        P - P&L, P2, P3, P4, P5
      *        R - INTEREST REDUCTION
      *        S - REPO SOLD
      *        T - SMALL T TRANSFER(FROM BRANCH), CAPITAL T TRANSFER(TO BR)
      *        V - REPO REDEEM
      *        X - REPO
      *
      *  REVS:
      *   SLC 031390 ADDED OPTION TO DISPLAY SEQUENCE NUMBER INSTEAD
      *              OF LAWCODE.
      *   MJD 110295 ADDED OPTION TO DISPLAY SUBCLASS INSTEAD OF
      *              LAWCODE OR SEQUENCE.
      *   MJD 120996 ADDED OPTION TO DISPLAY BW-SOLICIT IN FIRST
      *              POSITION.
      *   BAH 050219 ADDED MDTE-DC-FLAG AND SP-EXPD-FRMLA SO DUE DATE
      *              CHANGES DONT AFFECT MATURITY DATE, LENDMARK PL# 463
      * KEC 2015.0605 REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *               FOR USE WITH MICROFOCUS.
      * BLM 2016.0615 DON'T ALLOW VALUE OF "S" FOR BR-LOAN-NUM-FLAG SINCE
      *               WE CON'T HAVE LN-SEQNO ANYMORE
      * BAH 2022.0128 WHEN REMOVED INDEXED BY, THEY SUBTRACTED 1 FROM LPS-INDX
      *               WHEN IT SHOULD HAVE BEEN ADD. IT WAS DOING SET INDX UP
      *               IN A15, SO THAT WOULD BE "ADD" NOT "SUBTRACT"
      ********************************************************************
       LPSTAT-DISPLAY SECTION.

           MOVE SPACES       TO LPS-DISPLAY-LNNO.
           MOVE LN-CLASS     TO LPS-CLASS.
           MOVE "-"          TO LPS-DASH.
           MOVE LN-ACCTNO    TO LPS-NUMBER.
           MOVE LN-STATFLAGS TO LPS-STATFLAGS.

      * KEC 2015.0604 REMOVED 'INDEXED BY' FOR MICROFOCUS
      *               NOTE THAT WE NEEDED TO SUBTRACT 01 FROM LPS-INDX
      *               IF CONDITION TRUE WITHIN PERFORM VARYING SO
      *               KEEP LPS-INDX AT THAT LEVEL.
      
      *    SET INDX TO 1.
      *    SEARCH LPS-STATUSFLAGS
      *           AT END GO TO LPSTAT-M
      *           WHEN LPS-STATFG (INDX) = "E"
      *           GO TO LPSTAT-CONT.
      
           MOVE STAT---BAD                 TO STAT.

           PERFORM VARYING LPS-INDX FROM 1 BY 1
                     UNTIL LPS-INDX > 10
                        OR STAT-GOOD
                   IF ( LPS-STATFG (LPS-INDX) = "E" )
                      SUBTRACT 01        FROM LPS-INDX
                      MOVE STAT---GOOD     TO STAT
                   END-IF
           END-PERFORM.

           IF ( STAT-GOOD )
              GO TO LPSTAT-CONT.

      *-----------------------------------------------------------------
       LPSTAT-M.
           MOVE " " TO MDTE-FLAG MDTE-DC-FLAG.
           IF SP-EXPD-FRMLA = "A" OR "B"
              MOVE "Y" TO MDTE-FLAG.
           IF SP-EXPD-FRMLA = "B"
              MOVE "Y" TO MDTE-DC-FLAG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           IF DATE-YYYYMMDD < MDTE-DATE
              GO TO LPSTAT-CONT.

      * KEC 2015.0604 REMOVED 'INDEXED BY' FOR MICROFOCUS
      *               NOTE THAT WE NEEDED TO SUBTRACT 01 FROM LPS-INDX
      *               IF CONDITION TRUE WITHIN PERFORM VARYING SO
      *               KEEP LPS-INDX AT THAT LEVEL.
      
      *    SET INDX TO 1.
      *    SEARCH LPS-STATUSFLAGS
      *           AT END GO TO LPSTAT-SHIFT
      *           WHEN LPS-STATFG (INDX) = " "
      *           MOVE "M" TO LPS-STATFG (INDX)
      *           GO TO LPSTAT-CONT.
      
           MOVE STAT---BAD                 TO STAT.

           PERFORM VARYING LPS-INDX FROM 1 BY 1
                     UNTIL LPS-INDX > 10
                        OR STAT-GOOD
                   IF ( LPS-STATFG (LPS-INDX) = " " )
                      MOVE "M"             TO LPS-STATFG (LPS-INDX)
                      SUBTRACT 01        FROM LPS-INDX
                      MOVE STAT---GOOD     TO STAT
                   END-IF
           END-PERFORM.

           IF ( STAT-GOOD )
              GO TO LPSTAT-CONT.

      *-----------------------------------------------------------------
       LPSTAT-SHIFT.
           MOVE LPS-STATFG (2) TO LPS-STATFG (1).
           MOVE LPS-STATFG (3) TO LPS-STATFG (2).
           MOVE LPS-STATFG (4) TO LPS-STATFG (3).
           MOVE LPS-STATFG (5) TO LPS-STATFG (4).
           MOVE LPS-STATFG (6) TO LPS-STATFG (5).
           MOVE LPS-STATFG (7) TO LPS-STATFG (6).
           MOVE LPS-STATFG (8) TO LPS-STATFG (7).
           MOVE LPS-STATFG (9) TO LPS-STATFG (8).
           MOVE LPS-STATFG (10) TO LPS-STATFG (9).
           MOVE "M" TO LPS-STATFG (10).
       LPSTAT-CONT.
           IF BR-LOAN-NUM-FLAG  = "L" OR "C"
              GO TO LPSTAT-LAW-CODE.
           IF BR-LOAN-NUM-FLAG  = "O"
              GO TO LPSTAT-SOLICIT-CODE.
           IF LPS-STATFG (1) = " "
              GO TO END-LPSTAT.
           IF LPS-STATFG (7) = " "
              MOVE "-"            TO LPS-STAT (1)
              MOVE LPS-STATFG (1) TO LPS-STAT (2)
              MOVE LPS-STATFG (2) TO LPS-STAT (3)
              MOVE LPS-STATFG (3) TO LPS-STAT (4)
              MOVE LPS-STATFG (4) TO LPS-STAT (5)
              MOVE LPS-STATFG (5) TO LPS-STAT (6)
              MOVE LPS-STATFG (6) TO LPS-STAT (7)
              GO TO END-LPSTAT.

      * KEC 2015.0604 REMOVED 'INDEXED BY' FOR MICROFOCUS
      *               NOTE THAT WE NEEDED TO SUBTRACT 01 FROM LPS-INDX
      *               IF CONDITION TRUE WITHIN PERFORM VARYING SO
      *               KEEP LPS-INDX AT THAT LEVEL.
    
      *    SET INDX TO 8.
      *    SEARCH LPS-STATUSFLAGS
      *       AT END GO TO LPSTAT-MOVES-A
      *       WHEN LPS-STATFG (INDX) = " "
      *            NEXT SENTENCE.
     
           MOVE STAT---BAD                 TO STAT.

           PERFORM VARYING LPS-INDX FROM 8 BY 1
                     UNTIL LPS-INDX > 10
                        OR STAT-GOOD
                   IF ( LPS-STATFG (LPS-INDX) = " " )
                      SUBTRACT 01        FROM LPS-INDX
                      MOVE STAT---GOOD     TO STAT
                   END-IF
           END-PERFORM.

       LPSTAT-MOVES-A.

      * KEC 2015.0604 REMOVED 'INDEXED BY' FOR MICROFOCUS
      
      *    SET INDX DOWN BY 7.
      *    MOVE LPS-STATFG (INDX) TO LPS-STAT (1)
      *    SET INDX UP BY 1.
      *    MOVE LPS-STATFG (INDX) TO LPS-STAT (2)
      *    SET INDX UP BY 1.
      *    MOVE LPS-STATFG (INDX) TO LPS-STAT (3)
      *    SET INDX UP BY 1.
      *    MOVE LPS-STATFG (INDX) TO LPS-STAT (4)
      *    SET INDX UP BY 1.
      *    MOVE LPS-STATFG (INDX) TO LPS-STAT (5)
      *    SET INDX UP BY 1.
      *    MOVE LPS-STATFG (INDX) TO LPS-STAT (6)
      *    SET INDX UP BY 1.
      *    MOVE LPS-STATFG (INDX) TO LPS-STAT (7)
      *    GO TO END-LPSTAT.
      
           SUBTRACT 07 FROM LPS-INDX.
           MOVE LPS-STATFG (LPS-INDX) TO LPS-STAT (1)
           ADD 01 TO LPS-INDX.
           MOVE LPS-STATFG (LPS-INDX) TO LPS-STAT (2)
           ADD 01 TO LPS-INDX.
           MOVE LPS-STATFG (LPS-INDX) TO LPS-STAT (3)
           ADD 01 TO LPS-INDX.
           MOVE LPS-STATFG (LPS-INDX) TO LPS-STAT (4)
           ADD 01 TO LPS-INDX.
           MOVE LPS-STATFG (LPS-INDX) TO LPS-STAT (5)
           ADD 01 TO LPS-INDX.
           MOVE LPS-STATFG (LPS-INDX) TO LPS-STAT (6)
           ADD 01 TO LPS-INDX.
           MOVE LPS-STATFG (LPS-INDX) TO LPS-STAT (7)
           GO TO END-LPSTAT.

      *-----------------------------------------------------------------
       LPSTAT-LAW-CODE.
           IF BR-LOAN-NUM-FLAG  = "L"
              MOVE LN-LAWCODE TO LPS-LAWCODE-TEMP
           ELSE
           IF BR-LOAN-NUM-FLAG  = "C"
              MOVE LN-SUBCLASS TO LPS-LAWCODE-TEMP.

           MOVE "-"            TO LPS-STAT (1).
           MOVE LPS-LAWCODE-TA TO LPS-STAT (2).
           MOVE LPS-LAWCODE-TB TO LPS-STAT (3).

           IF LPS-STATFG (5) = " "
              MOVE 05 TO LPS-INDX 
              GO TO LPSTAT-MOVES-B.

      * KEC 2015.0604 REMOVED 'INDEXED BY' FOR MICROFOCUS
      *               NOTE THAT WE NEEDED TO SUBTRACT 01 FROM LPS-INDX
      *               IF CONDITION TRUE WITHIN PERFORM VARYING SO
      *               KEEP LPS-INDX AT THAT LEVEL.
      
      *    SET INDX TO 1.
      *    SEARCH LPS-STATUSFLAGS
      *       AT END GO TO LPSTAT-MOVES-B
      *       WHEN LPS-STATFG (INDX) = " "
      *            NEXT SENTENCE.
      
           MOVE STAT---BAD                 TO STAT.

           PERFORM VARYING LPS-INDX FROM 8 BY 1
                     UNTIL LPS-INDX > 10
                        OR STAT-GOOD
                   IF ( LPS-STATFG (LPS-INDX) = " " )
                      SUBTRACT 01        FROM LPS-INDX
                      MOVE STAT---GOOD     TO STAT
                   END-IF
           END-PERFORM.

           IF ( STAT-BAD )
              GO TO LPSTAT-MOVES-B.

      *-----------------------------------------------------------------
       LPSTAT-SOLICIT-CODE.
           MOVE "-"        TO LPS-STAT (1).
           MOVE BW-SOLICIT TO LPS-STAT (2).
           IF LPS-STATFG (6) = " "
              MOVE 06 TO LPS-INDX 
              GO TO LPSTAT-MOVES-B.

      * KEC 2015.0604 REMOVED 'INDEXED BY' FOR MICROFOCUS
      *               NOTE THAT WE NEEDED TO SUBTRACT 01 FROM LPS-INDX
      *               IF CONDITION TRUE WITHIN PERFORM VARYING SO
      *               KEEP LPS-INDX AT THAT LEVEL.
      
      *    SET INDX TO 1.
      *    SEARCH LPS-STATUSFLAGS
      *       AT END GO TO LPSTAT-MOVES-B
      *       WHEN LPS-STATFG (INDX) = " "
      *            NEXT SENTENCE.
      
           MOVE STAT---BAD                 TO STAT.

           PERFORM VARYING LPS-INDX FROM 1 BY 1
                     UNTIL LPS-INDX > 10
                        OR STAT-GOOD
                   IF ( LPS-STATFG (LPS-INDX) = " " )
                      SUBTRACT 01        FROM LPS-INDX
                      MOVE STAT---GOOD     TO STAT
                   END-IF
           END-PERFORM.

           IF ( STAT-BAD )
              GO TO LPSTAT-MOVES-B.

      *-----------------------------------------------------------------
       LPSTAT-MOVES-B.

      * KEC 2015.0604 REMOVED 'INDEXED BY' FOR MICROFOCUS
      
      *    IF BR-LOAN-NUM-FLAG = "O"
      *       SET INDX DOWN BY 5
      *       MOVE LPS-STATFG (INDX) TO LPS-STAT (3)
      *       SET INDX UP BY 1
      *    ELSE
      *       SET INDX DOWN BY 4.
      *    MOVE LPS-STATFG (INDX) TO LPS-STAT (4).
      *    SET INDX UP BY 1.
      *    MOVE LPS-STATFG (INDX) TO LPS-STAT (5).
      *    SET INDX UP BY 1.
      *    MOVE LPS-STATFG (INDX) TO LPS-STAT (6).
      *    SET INDX UP BY 1.
      *    MOVE LPS-STATFG (INDX) TO LPS-STAT (7).
      *    SET INDX UP BY 1.
      *    GO TO END-LPSTAT.
      
           IF ( BR-LOAN-NUM-FLAG = "O" )
              SUBTRACT 05 FROM LPS-INDX
              MOVE LPS-STATFG (LPS-INDX) TO LPS-STAT (3)
              ADD      01 TO   LPS-INDX
           ELSE
              SUBTRACT 04 FROM LPS-INDX.

           MOVE LPS-STATFG (LPS-INDX) TO LPS-STAT (4).
           ADD  01       TO LPS-INDX.
           MOVE LPS-STATFG (LPS-INDX) TO LPS-STAT (5).
           ADD  01       TO LPS-INDX.
           MOVE LPS-STATFG (LPS-INDX) TO LPS-STAT (6).
           ADD  01       TO LPS-INDX.
           MOVE LPS-STATFG (LPS-INDX) TO LPS-STAT (7).
           ADD  01       TO LPS-INDX.
           GO TO END-LPSTAT.

       END-LPSTAT.
