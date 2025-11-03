       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RCMAIN.
      ******************************************************************
      *
      *                 (MN)
      *  DESCRIPTION:   DAILY RECAP FILE MAINTENANCE
      * REV:
      *  MJD 062295 ADDED FIELDS FOR RC-CHECK-(DATE & TIME).
      *  MJD 091295 ADDED FIELDS FOR RC-EARNINGS-DATE,,
      *             RC-EARNINGS-TIME, AND RC-LAST-ACCRUE.
      *             REMOVED RC-EOM1-BATCH-(DATE/TIME).
      *  BLL 092895 MOVE BRANCH MACHINE TO RC PATH FOR RISC2 RC
      *  JTG 121996 CHANGED 'G1' STATUS TO ACCRUAL DONE
      *  MJD 112497 EXPANDED RC-REC TO 160
      *  MG  010726 {DR#0004} ADDED F6-KEY SEARCH OPTION
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *  BLM 160920 ADDED RC-CHECK-DEPOSIT, WORLD PR# 1188
      *   CS 161202 ADDED A FORM-RESET AT ENTER-RC-KEY-BEGIN, WHEN SCANNING
      *             AND HITTING THE END OF FILE, THE LAST RECORD IS STILL     
      *             DISPLAYED ON SCREEN.
      * 170313 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2023.1006 CHANGED SCREEN FIELD USERID TO MUST Y TO ALLOW
      *               LOWERCASE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/RCMAIN.CBL S34 06/06/21-16:36:58 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".


       01  ERRCD           PIC X VALUE " ".
       01  CHAN            PIC X(4).
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  HOLD-RC1-KEY.
           03  HOLD-RC-BRNO       PIC 9(4).
           03  HOLD-RC-TRANS-DATE PIC 9(8).

       01  HOLD-BRNO       PIC 9(04)   VALUE ZEROES.

       01  FIRST-SCAN      PIC X(01).
       01  SEARCH-STAT     PIC X(02).

       01  WS-DATE         PIC 9(4).
       01  WS-DATE-R REDEFINES WS-DATE.
      *M     03  FILLER      PIC XX.
           03  WS-MM       PIC 99.
           03  WS-DD       PIC 99.
       01  DATE-SW         PIC XXX    VALUE "   ".
           88  BAD-DATE               VALUE "BAD".

       01  ORIGINAL-RC-REC             PIC X(RC-SIZE).
       01  CHANGED-RC-REC              PIC X(RC-SIZE).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC XX.
           03  F011        PIC X(26).
           03  F02         PIC X.
           03  F03         PIC X.
           03  F04         PIC X.
           03  F05         PIC X.
           03  F06         PIC X.
           03  F07         PIC X.
           03  F08         PIC X.
           03  F09         PIC 9999.
           03  F10         PIC ZZZZ,ZZ9.99-.
           03  F11         PIC ZZZZ,ZZ9.99-.
           03  F12         PIC ZZZZ,ZZ9.99-.
           03  F13         PIC 99/99/99.
           03  F14         PIC 9(6).
           03  F15         PIC X(12).
           03  F16         PIC 99/99/99.
           03  F17         PIC 9(6).
           03  F18         PIC X(12).
           03  F19         PIC 99/99/99.
           03  F20         PIC 9(6).
           03  F21         PIC 99/99/99.
           03  F22         PIC 9(6).
           03  F23         PIC 99/99/99.
           03  F24         PIC 9(6).
           03  F25         PIC 99/99/99.
           03  F26         PIC 9(6).
           03  F27         PIC 99/99/99.
           03  F28         PIC 9(6).
           03  F29         PIC 99/99/99.
           03  F30         PIC 9(6).
           03  F31         PIC 99/99/99.
           03  F32         PIC 9(6).
           03  F33         PIC ZZZ,ZZZ,ZZZ.99-.
           03  F34         PIC ZZZZ,ZZ9.99-.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(44) VALUE
           "010 011 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
           "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(40) VALUE
           "210 220 230 240 250 260 270 280 290 300 ".
           03  FILLER      PIC X(16) VALUE
           "310 320 330 340.".
      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       78  MAX                      VALUE 34.
       01  SPEC-TABLE.
      * 1. RC-STATUS
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 2. RC-RECAP-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 3. RC-GL-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 4. RC-EOD-BATCH-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 5. RC-NIGHTLY-FUNDS-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 6. RC-EOM1-BATCH-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 7. RC-EOM-UPDATE-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 8. RC-EOM2-BATCH-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 9. LAST ACCRUE
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      *10. RC-DEPOSIT
           03  FILLER      PIC X(8) VALUE "EN S072 ".
      *11. RC-OVER-SHORT
           03  FILLER      PIC X(8) VALUE "EN S072 ".
      *12. RC-CHECKS
           03  FILLER      PIC X(8) VALUE "EN S072 ".
      *13. RC-OPENED-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *14. RC-OPENED-TIME
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *15. RC-OPENED-USERID
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *16. RC-CLOSED-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *17. RC-CLOSED-TIME
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *18. RC-CLOSED-USERID
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *19. RC-UPDATED-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *20. RC-UPDATED-TIME
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *21. RC-EOD-BATCH-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *22. RC-EOD-BATCH-TIME
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *23. RC-FUNDS-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *24. RC-FUNDS-TIME
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *25. RC-CHECK-DATE.
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *26. RC-CHECK-TIME.
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *27. RC-EOM1-BATCH-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *28. RC-EOM1-BATCH-TIME
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *29. RC-EOM-UPDATED-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *30. RC-EOM-UPDATED-TIME
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *31. RC-EOM2-BATCH-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *32. RC-EOM2-BATCH-TIME
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *33. RC-BANK-BAL
           03  FILLER      PIC X(8) VALUE "E  S092 ".
      *34. RC-CHECK-DEPOSIT
           03  FILLER      PIC X(8) VALUE "EN S072 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      *
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  RCMAIN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/RCMAIN_DEF.CPY".
       COPY "LIBMN/RCMAIN_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/RCMAIN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************************
       MAIN-PROGRAM SECTION.
      ***********************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE "RCMAIN" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS RCMAIN-WINDOW-HANDLE.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM ENTER-RC-KEY.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.


      ***************************
       ENTER-RC-KEY SECTION.
      ***************************
       ENTER-RC-KEY-BEGIN.

           PERFORM FORM-RESET.

       ENTER-BRANCH.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY )
              GO TO ENTER-RC-KEY-BEGIN.

           IF (        F6-KEY )
              PERFORM SEARCH-RC-FILE
              IF ( STAT-GOOD )
                 GO TO READ-RCFILE
              ELSE
                 GO TO ENTER-RC-KEY-BEGIN.

           IF (        F7-KEY ) MOVE "END" TO IO-TYPE
                                GO TO END-ENTER-RC-KEY.

           IF ( NOT ENTER-KEY ) GO TO ENTER-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-BRANCH.

           MOVE VDUNUM0 TO RC-BRNO BR-NO HOLD-RC-BRNO HOLD-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ VERIFY BRANCH  }
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              PERFORM NOT-FOUND
              GO TO ENTER-BRANCH.

           MOVE BR-MACHINE  TO RC-ALL-MACHINE.
           MOVE RC-ALL-PATH TO RC-PATH.

      *-----------------------------------------------------------------
       ENTER-RC-DATE.

           MOVE SPACES TO IO-TYPE.

           MOVE "ERNM060DATE." TO VDU.
           PERFORM SCREENIO.

           IF UP-KEY GO TO ENTER-BRANCH.
           IF F1-KEY GO TO ENTER-RC-KEY-BEGIN.
           IF F6-KEY
              PERFORM SEARCH-RC-FILE
              IF ( STAT-GOOD )
                 GO TO READ-RCFILE
              ELSE
                 GO TO ENTER-RC-KEY-BEGIN.
           IF F7-KEY 
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RC-KEY.
           IF NOT ENTER-KEY  GO TO ENTER-RC-DATE.

           IF VDUNUM0 = 0
              GO TO ENTER-RC-DATE.

           MOVE VDU-DATE-YYYYMMDD TO HOLD-RC-TRANS-DATE.

       READ-RCFILE.
           MOVE HOLD-RC1-KEY TO RC1-KEY.
           PERFORM OPEN-RC1-FILE.
           PERFORM READ-RC1-FILE.
           PERFORM CLOSE-RC1-FILE.
           IF IO-FG = 0
              MOVE RC-REC TO ORIGINAL-RC-REC
              PERFORM DISPLAY-FIELDS
              GO TO END-ENTER-RC-KEY
            ELSE
              PERFORM NEW-RECORD-SETUP.

       END-ENTER-RC-KEY.
           EXIT.

      ***************************************
       SEARCH-RC-FILE SECTION.
      ***************************************

           MOVE "Y"    TO FIRST-SCAN.
           MOVE "00"   TO SEARCH-STAT.
           MOVE 0      TO HOLD-BRNO.

           PERFORM OPEN-RC1-FILE.

           MOVE 0        TO RC1-KEY.
           MOVE 19000101 TO RC-TRANS-DATE.

           PERFORM START-RC1-FILE.
           PERFORM READ-RC1-FILE-NEXT.

           MOVE RC-BRNO TO HOLD-BRNO.
      *-----------------------------------------------------------------
       SEARCH-RC-FILE-ENTER-KEY.

           MOVE "EN N040SCAN-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) MOVE "XX" TO SEARCH-STAT
                                GO TO SEARCH-RC-FILE-END.
           IF (        F6-KEY ) IF ( FIRST-SCAN = "Y" )
                                   GO TO SEARCH-RC-FILE-START-NOT-G
                                ELSE
                                   GO TO SEARCH-RC-FILE-READ-PREV.
           IF ( NOT ENTER-KEY ) GO TO SEARCH-RC-FILE-ENTER-KEY.

           IF ( VDUNUM0 = 0 )
              IF ( FIRST-SCAN = "N" )
                 MOVE RC1-KEY TO HOLD-RC1-KEY
                 GO TO SEARCH-RC-FILE-END
              ELSE
                 GO TO SEARCH-RC-FILE-ENTER-KEY.

           MOVE VDUNUM0 TO HOLD-BRNO.
           GO TO SEARCH-RC-FILE-START-NOT-G.

       SEARCH-RC-FILE-START.

           MOVE HOLD-BRNO TO RC-BRNO.
           MOVE 99991231  TO RC-TRANS-DATE.
           PERFORM START-RC1-FILE.
           PERFORM READ-RC1-FILE-NEXT.
           IF ( IO-BAD )
              MOVE "XX" TO SEARCH-STAT
              GO TO SEARCH-RC-FILE-END.

           MOVE RC-BRNO TO HOLD-BRNO.

      *-----------------------------------------------------------------
       SEARCH-RC-FILE-START-NOT-G.

           MOVE HOLD-BRNO TO RC-BRNO.

           MOVE 99991231  TO RC-TRANS-DATE.

           PERFORM START-RC1-FILE-NOT-GREATER.

      *-----------------------------------------------------------------
       SEARCH-RC-FILE-READ-PREV.

           PERFORM READ-RC1-FILE-PREVIOUS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IO-BAD ON A READ-PREVIOUS MEANS HIT THE BEGINNING OF FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( IO-BAD ) OR
              ( HOLD-BRNO NOT = RC-BRNO )
              GO TO SEARCH-RC-FILE-START.

      *    UNLOCK RC-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ DISPLAY KEY  }
           MOVE "SN N040BRNO." TO VDU.
           MOVE RC-BRNO        TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A080DATE."     TO VDU.
           MOVE RC-TRANS-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N" TO FIRST-SCAN.
           GO TO SEARCH-RC-FILE-ENTER-KEY.

      *-----------------------------------------------------------------
       SEARCH-RC-FILE-END.

           PERFORM CLOSE-RC1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SEARCH-STAT TO STAT.

       SEARCH-RC-FILE-EXIT.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
             MOVE "DEL" TO IO-TYPE
             GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08
                 F-09 F-10 F-11 F-12 F-13 F-14 F-15 F-16
                 F-17 F-18 F-19 F-20 F-21 F-22 F-23 F-24
                 F-25 F-26 F-27 F-28 F-29 F-30 F-31 F-32
                 F-33 F-34
                 DEPENDING ON ELE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO RC-STATUS.
           IF NOT RC-STATUS-VALID
              MOVE "ENTER A1,B1,C1,D1,G1,H1,I1,J1,K1,K2 OR M1"
                                                        TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM SET-RC-STATUS-DESC.
           MOVE "S  A260011." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO RC-RECAP-FG.
           IF NOT (RC-RECAP-FG = "Y" OR " ")
              MOVE "ENTER 'Y' OR ' '" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO RC-GL-FG.
           IF NOT (RC-GL-FG = "Y" OR " ")
              MOVE "ENTER 'Y' OR ' '" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO RC-EOD-BATCH-FG.
           IF NOT (RC-EOD-BATCH-FG = "Y" OR " ")
              MOVE "ENTER 'Y' OR ' '" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO RC-NIGHTLY-FUNDS-FG.
           IF NOT (RC-NIGHTLY-FUNDS-FG = "Y" OR " ")
              MOVE "ENTER 'Y' OR ' '" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO RC-EOM1-BATCH-FG.
           IF NOT (RC-EOM1-BATCH-FG = "Y" OR " ")
              MOVE "ENTER 'Y' OR ' '" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO RC-EOM-UPDATE-FG.
           IF NOT (RC-EOM-UPDATE-FG = "Y" OR " ")
              MOVE "ENTER 'Y' OR ' '" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO RC-EOM2-BATCH-FG.
           IF NOT (RC-EOM2-BATCH-FG = "Y" OR " ")
              MOVE "ENTER 'Y' OR ' '" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-09.
           PERFORM DATE-CHECK-2.
           IF BAD-DATE
              GO TO  ENTER-ELE.
           MOVE VDUNUM0 TO RC-LAST-ACCRUE.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUNUM2 TO RC-DEPOSIT.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUNUM2 TO RC-OVER-SHORT.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM2 TO RC-CHECKS.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDU-DATE-YYYYMMDD TO RC-OPENED-DATE.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUNUM0 TO RC-OPENED-TIME.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUBUF TO RC-OP-USERID.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDU-DATE-YYYYMMDD TO RC-CLOSED-DATE.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUNUM0 TO RC-CLOSED-TIME.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUBUF TO RC-CL-USERID.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDU-DATE-YYYYMMDD TO RC-UPDATED-DATE.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUNUM0 TO RC-UPDATED-TIME.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDU-DATE-YYYYMMDD TO RC-EOD-BATCH-DATE.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUNUM0 TO RC-EOD-BATCH-TIME.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDU-DATE-YYYYMMDD TO RC-FUNDS-DATE.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUNUM0 TO RC-FUNDS-TIME.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDU-DATE-YYYYMMDD TO RC-CHECK-DATE.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE VDUNUM0 TO RC-CHECK-TIME.
           GO TO CREATE-ROUTINE.
       F-27.
           MOVE VDU-DATE-YYYYMMDD TO RC-EARNINGS-DATE.
           GO TO CREATE-ROUTINE.
       F-28.
           MOVE VDUNUM0 TO RC-EARNINGS-TIME.
           GO TO CREATE-ROUTINE.
       F-29.
           MOVE VDU-DATE-YYYYMMDD TO RC-EOM-UPDATED-DATE.
           GO TO CREATE-ROUTINE.
       F-30.
           MOVE VDUNUM0 TO RC-EOM-UPDATED-TIME.
           GO TO CREATE-ROUTINE.
       F-31.
           MOVE VDU-DATE-YYYYMMDD TO RC-EOM2-BATCH-DATE.
           GO TO CREATE-ROUTINE.
       F-32.
           MOVE VDUNUM0 TO RC-EOM2-BATCH-TIME.
           GO TO CREATE-ROUTINE.
       F-33.
           MOVE VDUNUM2 TO RC-BANK-BAL.
           GO TO CREATE-ROUTINE.
       F-34.
           MOVE VDUNUM2 TO RC-CHECK-DEPOSIT.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       DATE-CHECK-2 SECTION.
      ******************************************************************
           MOVE "   " TO DATE-SW.
           MOVE VDU-NUM TO WS-DATE.

           IF WS-MM = 0 AND WS-DD = 0
              GO TO DATE-CHECK-2-EXIT.

           IF WS-MM < 1 OR WS-MM > 12
              MOVE "BAD MONTH" TO MESS
              PERFORM SEND-MESS
              MOVE "BAD" TO DATE-SW.

       DATE-CHECK-2-EXIT.
           EXIT.

      *******************************************
       WRITE-MODULE SECTION.
      *******************************************
           PERFORM OPEN-RC1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE RC-REC TO CHANGED-RC-REC.
           IF ORIGINAL-RC-REC = CHANGED-RC-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-RC1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF RC-REC NOT = ORIGINAL-RC-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-RC-REC TO RC-REC.
           PERFORM REWRITE-RC1-FILE.

           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-RC1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-RC1-FILE.

      *******************************************
       DELETE-MODULE SECTION.
      *******************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-RC1-FILE.
           PERFORM READ-RC1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF RC-REC NOT = ORIGINAL-RC-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-RC1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-RC1-FILE.

      *******************************************
       NEW-RECORD-SETUP SECTION.
      *******************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-RC-FILE.
           MOVE HOLD-RC1-KEY TO RC1-KEY.
           MOVE 0 TO ELE.

      *******************************************
       DISPLAY-FIELDS SECTION.
      *******************************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE RC-STATUS           TO F01.
           PERFORM SET-RC-STATUS-DESC.
           MOVE MESS                TO F011.
           MOVE RC-RECAP-FG         TO F02.
           MOVE RC-GL-FG            TO F03.
           MOVE RC-EOD-BATCH-FG     TO F04.
           MOVE RC-NIGHTLY-FUNDS-FG TO F05.
           MOVE RC-EOM1-BATCH-FG    TO F06.
           MOVE RC-EOM-UPDATE-FG    TO F07.
           MOVE RC-EOM2-BATCH-FG    TO F08.
           MOVE RC-LAST-ACCRUE      TO F09.
           MOVE RC-DEPOSIT          TO F10.
           MOVE RC-OVER-SHORT       TO F11.
           MOVE RC-CHECKS           TO F12.
           MOVE RC-OPENED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F13.
           MOVE RC-OPENED-TIME      TO F14.
           MOVE RC-OP-USERID        TO F15.
           MOVE RC-CLOSED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F16.
           MOVE RC-CLOSED-TIME      TO F17.
           MOVE RC-CL-USERID        TO F18.
           MOVE RC-UPDATED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F19.
           MOVE RC-UPDATED-TIME     TO F20.
           MOVE RC-EOD-BATCH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F21.
           MOVE RC-EOD-BATCH-TIME   TO F22.
           MOVE RC-FUNDS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F23.
           MOVE RC-FUNDS-TIME       TO F24.
           MOVE RC-CHECK-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F25.
           MOVE RC-CHECK-TIME       TO F26.
           MOVE RC-EARNINGS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F27.
           MOVE RC-EARNINGS-TIME    TO F28.
           MOVE RC-EOM-UPDATED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F29.
           MOVE RC-EOM-UPDATED-TIME TO F30.
           MOVE RC-EOM2-BATCH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F31.
           MOVE RC-EOM2-BATCH-TIME  TO F32.
           MOVE RC-BANK-BAL         TO F33.
           MOVE RC-CHECK-DEPOSIT    TO F34.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GBRCCL.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO RCMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RCMAIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY RCMAIN-SCREEN UPON RCMAIN-WINDOW-HANDLE.
           MOVE RCMAIN-FORM-FIELDS TO WR-BUF.
           MOVE RCMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.


      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/RCMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       SETUP-LINK-INFO SECTION.
      ****************************************
           MOVE RCMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE RCMAIN-HELP--DIR TO HELP-LINK-DIR.

      **************************
       MISC-PARAGRAPHS SECTION.
      **************************
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       SET-RC-STATUS-DESC.
           IF RC-STATUS = "A1"
              MOVE "DAY OPENED" TO MESS
           ELSE
           IF RC-STATUS = "B1"
              MOVE "DAILY UPDATE INPROCESS" TO MESS
           ELSE
           IF RC-STATUS = "C1"
              MOVE "DAY UPDATED" TO MESS
           ELSE
           IF RC-STATUS = "D1"
              MOVE "EOD BATCH DONE" TO MESS
           ELSE
           IF RC-STATUS = "G1"
              MOVE "EARNINGS ACCRUAL DONE " TO MESS
           ELSE
           IF RC-STATUS = "H1"
              MOVE "EOM1 BATCH DONE" TO MESS
           ELSE
           IF RC-STATUS = "I1"
              MOVE "EOM UPDATED" TO MESS
           ELSE
           IF RC-STATUS = "J1"
              MOVE "EOM2 BATCH DONE" TO MESS
           ELSE
           IF RC-STATUS = "K1"
              MOVE "BANK DEPOSIT MADE" TO MESS
           ELSE
           IF RC-STATUS = "K2"
              MOVE "BANK TRANSMITTED FINISHED" TO MESS
           ELSE
           IF RC-STATUS = "M1"
              MOVE "CHECK EXTRACTION COMPLETED" TO MESS.

       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.
       NOT-FOUND.
           MOVE " NOT FOUND" TO MESS.
           PERFORM SEND-MESS.


      *******************************************
       IO-ROUTINES SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBRC1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *******************************************
       END-ROUTINE SECTION.
      *******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "SP/UTMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY RCMAIN-SCREEN.
           DESTROY RCMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
