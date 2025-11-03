      * COPYMEMBER: LIBFP/PST.FORM
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/PST.FORM
      *
      * DESCRIPTION   :  LASER (PST) FORM PROGRAM ROUTINES
      *
      *=================================================================
      * REV:
      * KEC 2007-1101 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               **     ** PCL
      *
      * KEC 2009-0130 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               DUE TO A REQUEST BY JUDY (PCL524), WE NEEDED TO
      *               BE ABLE TO CROSS DATA PATHS IF RUNNING THIS FROM
      *               BATCH PRINT USING ALL BRANCHES.
      *               ADDED 'CROSS-DATA-PATHS' TO CHANGE BRANCH PATHS.
      *
      * KEC 2009-0826 {PR#03423} REGENCY <A15> [LORI MCBEE]
      *               ADDED LOGIC TO USE THE PD FILE (SAVED LOAN) AND
      *               THE PC FILE (SAVED COLLATERAL).
      *
      * KEC 2011-0729 {PR#00000} PARADATA <A15> [JIM ROSENAUER]
      *               ADDED 'SAVE-LM-MEMO-WORKERS' DUE TO AN ISSUE WITH
      *               A FORM FOR MID-ATLANTIC.
      *
      * KEC 2011-0930 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               **     ** POSTSCRIPT
      *
      * AMM 2014-0530 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ADDED LOAD B2 FILE SECTION
      * BAH 2015-1021 REMOVED B2FILE, PD0003
      *
      * AMM 2018-0920 {PR#01212} WORLD [KEVIN MULLEN]
      *               ADDED GET 1STPYDA EXTENSION PARAGRAPH TO USE ON
      *               LASER ENVELOPE; ORIGINAL PR# WAS DONE IN A15 ON 
      *               01-20-17 THEN ADDED TO A30 LATER
      * BAH 2018-1112 GLOBAL FMFILE
      * BAH 2018-1127 GLOBAL LMFILE
      * BAH 2018-1203 GLOBAL LNFILE
      * BAH 2019-0312 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-KEY 
      * JKC 2021-0608 ADDED ACUSQL ITEMS (SQL-CONNECT)
      * JKC 2022-0503 ADDED TRC-PATH-OVERRIDE
      * BAH 2024.1031 ONLY DISPLAY "PRINT IN PROGRESS" IF NOT USING
      *               F3-ALL (PST-WS-OUTPUT-SWITCH-ALL) #1683 S35Q-142
      *
      *****************************************************************
      *
      *    P S T - F O R M - M A I N - M O D U L E
      *
      ******************************************************************
       PST-FORM-MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.
           PERFORM PST-FORM-INITIALIZATION.

      * CHECK TO SEE IF CROSSING DATA PATHS IS NEEDED

           IF ( BRANCH = ZEROES )
              MOVE CROSS-DATA-PATHS---ON   TO CROSS-DATA-PATHS
           ELSE
              MOVE CROSS-DATA-PATHS---OFF  TO CROSS-DATA-PATHS.

           MOVE ZEROES                     TO CROSS-DATA-PATHS-BRNO.

      * VERIFY BATCH PRINT 

           IF ( LOAN-KEY NOT = ZEROES )
              GO TO PST-FORM-READ-PRIMARYS
           ELSE
           IF ( BORROWER-KEY NOT = ZEROES )
              GO TO PST-FORM-READ-PRIMARYS.

           MOVE "Y"                        TO BATCH-PR.

           MOVE FM-PATH-OWNBR      TO FM-BRNO
                                      QFM2-WBEG-BRNO
                                      QFM2-WEND-BRNO.
           MOVE IP-FMNO            TO FM-FMNO
                                      QFM2-WBEG-FMNO
                                      QFM2-WEND-FMNO.
           MOVE SPACES             TO FM-ZIP
                                      QFM2-WBEG-ZIP.
           MOVE ALL "Z"            TO QFM2-WEND-ZIP.

           PERFORM START-FM2-FILE.

       PST-FORM-FM2-READ-NEXT.

           MOVE ZEROES                     TO LOAN-KEY.
           PERFORM READ-FM2-FILE-NEXT.
           IF ( IO-BAD ) OR ( FM-BRNO NOT = FM-PATH-OWNBR )
              GO TO PST-FORM-MAIN-MOD-EXIT.

           IF ( FM-FMNO NOT = IP-FMNO )
              GO TO PST-FORM-MAIN-MOD-EXIT.

           IF ( BRANCH  NOT = ZEROES ) AND
              ( FM-BRNO NOT = BRANCH )
              GO TO PST-FORM-FM2-READ-NEXT.

           IF ( PRINT-REPRINT = "P" ) AND
              ( FM-PRINT      = "Y" )
              GO TO PST-FORM-FM2-READ-NEXT.

           IF ( PRINT-REPRINT = "R" ) AND
              ( FM-PRINT  NOT = "Y" )
              GO TO PST-FORM-FM2-READ-NEXT.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO LN-ACCTNO LOAN-KEY.

       PST-FORM-READ-PRIMARYS.

           PERFORM PST-FORM-CROSS-DATA-PATHS.

           IF ( BATCH-PR NOT = "Y" )
              MOVE FM-PATH-OWNBR          TO FM-BRNO
              MOVE SPACES                 TO LPWSFM-EDIT-FM-INITKEY
              MOVE LOAN-KEY               TO LPWSFM-EDIT-FM-ACCTNO
              MOVE IP-FMNO                TO LPWSFM-EDIT-FM-FORMNO
                                             FM-FMNO
              MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY
              PERFORM READ-FM1-FILE.
      *       IF ( IO-GOOD )
      *          UNLOCK FM-FILE.

       PST-FORM-READ-PRIMARY-LN.

           IF NOT ( PST-PRIMARY-FILE-LN )
              GO TO PST-FORM-READ-PRIMARY-PD.

           IF ( LOAN-KEY = ZEROES )
              GO TO PST-FORM-MAIN-MOD-EXIT.

           MOVE LOAN-KEY                   TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

       PST-FORM-READ-PRIMARY-PD.

           IF NOT ( PST-PRIMARY-FILE-PD )
              GO TO PST-FORM-PROCESS-RECS.

           IF ( BORROWER-KEY = ZEROES )
              GO TO PST-FORM-MAIN-MOD-EXIT.

           MOVE "L"                        TO PD-ID.
           MOVE BRANCH                     TO PD-BRANCH.
           MOVE BORROWER-KEY               TO PD-SOC-SEC.
           PERFORM OPEN-PD1-FILE.
           PERFORM READ-PD1-FILE.
           PERFORM CLOSE-PD1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE PD-RECORD                  TO LN-REC.
           MOVE PD-SOC-SEC                 TO LN-SSNO(1).

       PST-FORM-PROCESS-RECS.

           MOVE LN-ACCTNO                  TO WS-LN-ACCTNO.

           MOVE LN-OWNBR                   TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE IP-FMNO                    TO FD-FMNO.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           PERFORM OPEN-GB-FILE.
           MOVE LN-OWNBR TO GB-BRNO.
           MOVE 1        TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE LN-ORGST                   TO SP-ORGST.
           MOVE LN-SPRCLASS                TO SP-SPRCLASS.
           MOVE LN-SUBCLASS                TO SP-SUBCLASS.
           MOVE LN-LAWCODE                 TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           PERFORM PRINT-ROUTINE.

           IF ( FD-LNMEMO = "Y"       ) AND
              ( PST-LM-MEMO-SWITCH-ON )
              PERFORM PST-UPDATE-LOAN-MEMO.

           IF ( BATCH-PR = "Y" )
              MOVE FM-BRNO TO HOLD-FM2-BRNO
              MOVE FM-FMNO TO HOLD-FM2-FMNO
              MOVE FM-ZIP  TO HOLD-FM2-ZIP
              MOVE FM1-KEY TO HOLD-FM1-KEY
              PERFORM READ-FM1-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR
              ELSE
                 MOVE "Y"                  TO FM-PRINT
                 PERFORM REWRITE-FM1-FILE
                 GO TO PST-FORM-FM2-READ-NEXT.

           IF ( FM-PRINT = "Y" )
              GO TO PST-FORM-MAIN-MOD-EXIT.

           PERFORM READ-FM1-FILE.
           IF ( IO-GOOD )
              MOVE "Y"                     TO FM-PRINT
              PERFORM REWRITE-FM1-FILE.

           IF ( ERRCD NOT = "L" )
              MOVE SPACES                  TO ERRCD.

       PST-FORM-MAIN-MOD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F O R M - I N I T I A L I Z A T I O N
      *
      ******************************************************************
       PST-FORM-INITIALIZATION SECTION.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE FORM-NAM                   TO CURRENT-FORM-NAM.

           MOVE 0            TO Q-POS.
           MOVE 0           TO C-POS.
           MOVE LONPM-PRU-POS              TO P-POS.
           MOVE "N"                        TO Q-BUF.
           MOVE 00                         TO P-BUF.

           MOVE SPACES                     TO IP-PROG.
           PERFORM GET-GPENV.

           PERFORM OPEN-FM1-FILE.

           MOVE "PST_OUTPUT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-BAD
              MOVE "N" TO PST-WS-OUTPUT-SWITCH
           ELSE
              MOVE GETENV-BUF TO PST-WS-OUTPUT-SWITCH.
           IF NOT (PST-WS-OUTPUT-SWITCH-VALID)
              MOVE "N" TO PST-WS-OUTPUT-SWITCH.
           IF NOT PST-WS-OUTPUT-SWITCH-ALL
              MOVE "PRINT IN PROGRESS................." TO WR-BUF
              MOVE MSEL-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       PST-FORM-INITIAL-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F O R M - C R O S S - D A T A - P A T H S
      *
      *=================================================================
      *    IN  :  CROSS-DATA-PATHS (ON/OFF)
      *           CROSS-DATA-PATHS-BRNO
      *    OUT :  CROSS-DATA-PATHS-BRNO
      *    DESC:  ROUTINE TO SET LOCAL FILE PATHS FOR CROSSING
      *           DATA PATHS IF NEEDED.
      ******************************************************************
       PST-FORM-CROSS-DATA-PATHS SECTION.

           IF ( CROSS-DATA-PATHS-OFF            ) OR
              ( FM-BRNO = CROSS-DATA-PATHS-BRNO )
              GO TO PST-FORM-C-D-PATHS-EXIT.

           MOVE FM-BRNO               TO BR-NO   CROSS-DATA-PATHS-BRNO.
           PERFORM OPEN-BR-FILE
           PERFORM READ-BR-FILE
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              CK-PATH-OVERRIDE
                                              TRC-PATH-OVERRIDE
                                              CX-PATH-OVERRIDE
                                              GB-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE
                                              PC-PATH-OVERRIDE
                                              PD-PATH-OVERRIDE.

       PST-FORM-C-D-PATHS-EXIT.
           EXIT.


      ******************************************************************
      *
      *    P S T - U P D A T E - L O A N - M E M O
      *
      ******************************************************************
       PST-UPDATE-LOAN-MEMO SECTION.

           PERFORM OPEN-LM1-FILE.

           MOVE SPACES                     TO LM-REC.
           MOVE LN-OWNBR                   TO SAVE-LM-BRNO.
           MOVE LN-ACCTNO                  TO SAVE-LM-ACCTNO.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           COMPUTE SAVE-LM-DATE = 99999999 - DATE-YYYYMMDD.
           MOVE 01                         TO SAVE-LM-SEQ.

       PST-UPDATE-LOAN-MEMO-LOOP.

           MOVE SAVE-LM1-KEY TO LM1-KEY.
           PERFORM READ-LM1-FILE.
           IF ( IO-FG = 0 )
              IF ( LM-SEQ < 999 )
                 ADD 01                    TO SAVE-LM-SEQ
                 GO TO PST-UPDATE-LOAN-MEMO-LOOP
              ELSE
                 GO TO PST-UPDATE-LOAN-MEMO-END.

           MOVE SPACES                     TO LM-REC.
           MOVE SAVE-LM1-KEY               TO LM1-KEY.

           MOVE FD-FMNO                    TO SAVE-LM-MEMO-FMNO.
           MOVE FD-DESC                    TO SAVE-LM-MEMO-DESC.
           MOVE SAVE-LM-MEMO               TO LM-MEMO.
           PERFORM WRITE-LM1-FILE.

       PST-UPDATE-LOAN-MEMO-END.

           PERFORM CLOSE-LM1-FILE.

       PST-UPDATE-LOAN-MEMO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P S T - F O R M - M I S C - P A R A G R A P H
      *
      ******************************************************************
       PST-FORM-MISC-PARAGRAPH SECTION.
      *=================================================================
      *    IN  : PST-PHONE-UNEDIT
      *    OUT : PST-PHONE-EDIT
      *    DESC: CONVERT UNEDITED PHONE NUMBER TO AN EDITED VERSION.
      *=================================================================
       PST-CONVERT-PHONE-TO-EDIT.
           MOVE PST-PHONE-UNEDIT-AREA      TO PST-PHONE-EDIT-AREA.
           MOVE PST-PHONE-UNEDIT-PREFIX    TO PST-PHONE-EDIT-PREFIX.
           MOVE PST-PHONE-UNEDIT-SUFFIX    TO PST-PHONE-EDIT-SUFFIX.

      *=================================================================
      *    IN  : WS-MONTH-MM
      *    OUT : WS-MONTH-TEXT
      *    DESC: FINDS THE TEXT VERSION OF THE MONTH.
      *=================================================================
       LOAD-WS-MONTH-TEXT.

           MOVE "*UNKNOWN* " TO WS-MONTH-TEXT.

           IF ( WS-MONTH-MM = 01 ) MOVE "JANUARY   " TO WS-MONTH-TEXT.
           IF ( WS-MONTH-MM = 02 ) MOVE "FEBRUARY  " TO WS-MONTH-TEXT.
           IF ( WS-MONTH-MM = 03 ) MOVE "MARCH     " TO WS-MONTH-TEXT.
           IF ( WS-MONTH-MM = 04 ) MOVE "APRIL     " TO WS-MONTH-TEXT.
           IF ( WS-MONTH-MM = 05 ) MOVE "MAY       " TO WS-MONTH-TEXT.
           IF ( WS-MONTH-MM = 06 ) MOVE "JUNE      " TO WS-MONTH-TEXT.
           IF ( WS-MONTH-MM = 07 ) MOVE "JULY      " TO WS-MONTH-TEXT.
           IF ( WS-MONTH-MM = 08 ) MOVE "AUGUST    " TO WS-MONTH-TEXT.
           IF ( WS-MONTH-MM = 09 ) MOVE "SEPTEMBER " TO WS-MONTH-TEXT.
           IF ( WS-MONTH-MM = 10 ) MOVE "OCTOBER   " TO WS-MONTH-TEXT.
           IF ( WS-MONTH-MM = 11 ) MOVE "NOVEMBER  " TO WS-MONTH-TEXT.
           IF ( WS-MONTH-MM = 12 ) MOVE "DECEMBER  " TO WS-MONTH-TEXT.

      *=================================================================
      *    IN  : WS-1STPY-DD
      *    OUT : WS-1STPY-EXT 
      *    DESC: FINDS THE EXTENSION OF THE 1STPYDA.
      *=================================================================
       LOAD-WS-1STPY-EXT.

           MOVE "TH" TO WS-1STPY-EXT.

           IF ( WS-1STPY-DD = 01 OR 21 OR 31 )
              MOVE "ST"       TO WS-1STPY-EXT.
           IF ( WS-1STPY-DD = 02 OR 22 )
              MOVE "ND"       TO WS-1STPY-EXT.
           IF ( WS-1STPY-DD = 03 OR 23 )
              MOVE "RD"       TO WS-1STPY-EXT.

