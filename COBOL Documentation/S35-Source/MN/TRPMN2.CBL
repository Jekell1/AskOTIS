       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRPMN2.
       DATE-WRITTEN. 12-12-94.
      ******************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   TRANSACTION FILE MAINTENANCE
      *
      * REV:
      *   MJD 050995 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *   MJD 062995 ADDED TP-POSTTIME. SET NO RESET ON ALL FIELDS
      *   MJD 112195 FIELDS 20 THROUGH 25 WERE OUT OF SYNC.
      *   BAH 062598 CHANGED TP-TILL PIC 9 TO TP-TILLNO PIC X
      *    CS 120807 ADDED VALUES FOR TP-MAX-DEFER-FG REGACC PR#318
      * MJD 131203 A30 CONVERSION COMPLETED
      *  CS 160921 HOLD-TRP-KEY TO HOLD-TRP-KEY,SAVE KEY FROM X(14) TO X(27)(TRP      *            MIN LINK)REMOVE ENTER-SEQNO AND REFERENCES TO TRP-SEQ 
      *            REMOVED TP-MORELN
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *         ADDED LIBLP/LPFRTR; REMOVED LIBLP/LPTRTMPW.
      *         ADDED LPFRTR-UPDATE-CHGLOG; REMOVED WRITE-TR-CHANGE-LOG.
      * BLM 161102 CHANGED TP-TILL PIC X TO TP-TILLNO PIC 99
      * BAH 2019.0508 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * BLM 211011 CHANGED SAVEKEY TO X(26)
      * BLM 211022 FIX GL ACCT ENTRY SO YOU CAN ENTER OR NOT ENTER
      *            DASH AND LEADING ZEROES
      * BLM 2021.1115 SAVE-MMYY ALWAYS SPACES, CAUSES CAST ERRORS,
      *               POPULATE WITH VALUES IN SAVEKEY
      * BAH 2025.0124 VALIDATE MONTH ENTERED S35Q-159
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

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/TRPMN2.CBL S34 11/15/21-11:40:50 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".

       01  ACTIVE-FILE-SW        PIC X(3)    VALUE "OFF".
       01  LEAVE-BRANCH-MMYY-SW  PIC X(3)    VALUE "OFF".

       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  OVER-20-ELE     PIC 9(2).
       01  OVER-40-ELE     PIC 9(2).

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
           03  CHNO-X      REDEFINES CHNO
                           PIC X(2).

       01  FORM-NUM1       REDEFINES ACTION.
           03  FORM-KEY1   PIC X(4).
           03  FORM-P1     PIC X.
           03  FORM-SEL1   PIC 9.

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-TIME      PIC 9       VALUE 0.

       01  SAVE-BRNO       PIC 9(4).
       01  SAVE-MMYY       PIC 9(4).
       01  SAVE-MMYYX REDEFINES SAVE-MMYY.
           03  SAVE-MM     PIC 99.
           03  SAVE-YY     PIC 99.

       01  PAGE-NUMBER     PIC X(2).
           88  VALID-PAGE  VALUE "1" "3" "4".

       01 TRPMIN-LINK VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(26).
           03  FILLER REDEFINES SAVEKEY.
               05  FILLER     PIC X(6).
               05  SAVEKEY-YY PIC 9(2).
               05  SAVEKEY-MM PIC 9(2).
               05  FILLER     PIC X(16).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  FILLER REDEFINES FORMSEL.
               05  FILLER    PIC 9.
               05  FORMNUM   PIC 9.
           03  SAVEBRNO      PIC 9(4).
           03  TRPMIN-WINDOW-HANDLE PIC X(10).

       01  ORIGINAL-TRP-REC    PIC X(TRP-SIZE).
       01  CHANGED-TRP-REC     PIC X(TRP-SIZE).


      ******************************************************************
      *    *     SCREEN DISPLAY BUFFER      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(12).
           03  F02         PIC ZZ,ZZZ.99-     BLANK WHEN ZERO.
           03  F03         PIC X(12).
           03  F04         PIC ZZ,ZZZ.99-     BLANK WHEN ZERO.
           03  F05         PIC X(12).
           03  F06         PIC ZZ,ZZZ.99-     BLANK WHEN ZERO.
           03  F07         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F08         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F09         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F10         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F11         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F12         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F13         PIC ZZ,ZZZ.99-     BLANK WHEN ZERO.
           03  F14         PIC ZZ,ZZZ.99-     BLANK WHEN ZERO.
           03  F15         PIC ZZ,ZZZ.99-     BLANK WHEN ZERO.
           03  F16         PIC ZZ,ZZZ.99-     BLANK WHEN ZERO.
           03  F17         PIC ZZ,ZZZ.99-     BLANK WHEN ZERO.
           03  F18         PIC ZZ,ZZZ.99-     BLANK WHEN ZERO.
           03  F19         PIC 9(6)           BLANK ZERO.
           03  F20         PIC X.
           03  F21         PIC X.
           03  F22         PIC X.
           03  F23         PIC X.
           03  F24         PIC X.
           03  F25         PIC X.
           03  F26         PIC X.
           03  F27         PIC X.
           03  F28         PIC X.
           03  F29         PIC X.
           03  F30         PIC 99             BLANK WHEN ZERO.
           03  F31         PIC XX.
           03  F32         PIC X.
           03  F33         PIC X.
           03  F34         PIC X.
           03  F35         PIC X.
           03  F36         PIC 99.
           03  F37         PIC X.
           03  F38         PIC X.
           03  F39         PIC X.
           03  F40         PIC X.
           03  F41         PIC X.
           03  F42         PIC X.
           03  F43         PIC X.
           03  F44         PIC X.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
               "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(40) VALUE
               "210 220 230 240 250 260 270 280 290 300 ".
           03  FILLER      PIC X(40) VALUE
               "310 320 330 340 350 360 370 380 390 400 ".
           03  FILLER      PIC X(16) VALUE
               "410 420 430 440.".

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS
      ******************************************************************
       01  MAX             PIC 99   VALUE 44.
       01  SPEC-TABLE.
      * 1. GL ACCOUNT NUMBER 1
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      * 2. GL AMOUNT 1
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      * 3. GL ACCOUNT NUMBER 2
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      * 4. GL AMOUNT 2
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      * 5. GL ACCOUNT NUMBER 3
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      * 6. GL AMOUNT 3
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      * 7. WORK AMOUNT 1
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 8. WORK AMOUNT 2
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 9. WORK AMOUNT 3
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *10. WORK AMOUNT 4
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *11. WORK AMOUNT 5
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *12. WORK AMOUNT 6
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *13. EARNINGS 1
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *14. EARNINGS 2
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *15. EARNINGS 3
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *16. EARNINGS 4
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *17. EARNINGS 5
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *18. EARNINGS 6
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *19. POST TIME
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *20. SERVICE ACCOUNT
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *21. SALE FIN
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *22. INSURED
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *23. INSOURS
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *24. LOANTYPE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *25. OTH BRANCH FLAG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *26. REV
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *27. STATUS FLAG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *28. INSTYPES
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *29. MAX DEFERMENTS FLAG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *30. LAW CODE
           03  FILLER      PIC X(8) VALUE "E NN020 ".
      *31. POCD
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *32. PL CD
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *33. MORE LN
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *34. MAKER
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *35. IBPC
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *36. TILLNO
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *37. DL-RVCHGS-CODE(1)
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *38. DL-RVCHGS-CODE(2)
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *39. DL-RVCHGS-CODE(3)
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *40. DL-PARVPAID-CODE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *41. DL-PARVHELD-CODE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *42. MANUAL-RB
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *43. KEEP AP INT
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *44. JD CD
           03  FILLER      PIC X(8) VALUE "E NA010 ".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 44 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      *
       01  KEY-BUF.
           03  D-BRANCH    PIC 9(4).
           03  D-MMYY.
               05  D-MM    PIC 99.
               05  D-YY    PIC 99.
           03  D-DAY       PIC 99.
           03  D-BRNO      PIC 9(4).
           03  D-CODE      PIC X.
           03  D-CLASS     PIC 99.
           03  D-NUMBER    PIC 999999.
           03  D-LINE      PIC 99.
       01  KEY-LIST.
           03  FILLER      PIC X(44) VALUE
               "BRANCH MMYY DAY BRNO CODE CLASS NUMBER LINE.".

       01  SCREEN-GL.
           03  SC-BRANCH   PIC 9(4).
           03  SC-DASH     PIC X.
           03  SC-NUMBER   PIC 9(7).
       01  SCREEN-GL-NUM REDEFINES SCREEN-GL PIC 9(12).
       01  WORK-GL         PIC 9(11).
       01  WORK-NUMBER REDEFINES WORK-GL.
           03  WORK-BR     PIC 9(4).
           03  WORK-NO     PIC 9(7).
       01  DISP-GL.
           03  DISP-BR     PIC 9(4).
           03  FILLER      PIC X VALUE "-".
           03  DISP-NO     PIC 9(7).
      ******************************************************************
       COPY "LIBWI/TRPSCNW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TRPMN2_DEF.CPY".
       COPY "LIBMN/TRPMN2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/TRPMN2_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N    P R O G R A M    M O D U L E
      *
      ******************************************************************
        MAIN-PROGRAM SECTION.

      D    STOP MESS.

        COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TRPMN2"      TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO TRPMIN-LINK.
           MOVE SAVEKEY-MM TO SAVE-MM.
           MOVE SAVEKEY-YY TO SAVE-YY.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           IF SAVEKEY NOT = " "
              MOVE "ON" TO ACTIVE-FILE-SW.

           MOVE "OFF" TO LEAVE-BRANCH-MMYY-SW.

      *-----------------------------------------------------------------
        MAIN-MODULE.

            PERFORM FORM-RESET.

            IF LEAVE-BRANCH-MMYY-SW = "OFF"
               PERFORM ENTER-BRANCH-MMYY-KEYS-MODULE.

            IF ACTIVE-FILE-SW = "OFF"
               PERFORM ENTER-KEYS-MODULE.

            IF IO-TYPE = "END"
               MOVE "X" TO ERRCD
               GO TO END-ROUTINE.

            IF IO-TYPE = "CLR"
               MOVE "OFF" TO LEAVE-BRANCH-MMYY-SW
               MOVE " " TO NEW-REC
               GO TO MAIN-MODULE.

            PERFORM RESTART-MAINT.

            IF IO-TYPE = "RWR"
               PERFORM WRITE-MODULE.

            IF IO-TYPE = "DEL"
               MOVE " " TO NEW-REC
               PERFORM DELETE-MODULE.

            IF IO-TYPE = "END"
               MOVE "X" TO ERRCD
               GO TO END-ROUTINE.

            IF IO-TYPE = "CLR"
               MOVE "OFF" TO LEAVE-BRANCH-MMYY-SW
               MOVE " " TO NEW-REC
               GO TO MAIN-MODULE.

            IF NEW-REC = "Y"
               MOVE "P" TO FORM-P1
               MOVE 3 TO FORM-SEL1.

            IF FORM-SEL1 NOT = 0
               IF FORM-P1 = "P"
                  MOVE BR-NO           TO SAVEBRNO
                  MOVE LPWSTRP-TRP1-KEY TO SAVEKEY
                  MOVE FORM-SEL1        TO FORMSEL
                  GO TO END-ROUTINE.

            GO TO MAIN-MODULE.

       EXIT-MAIN-PROGRAM.
           EXIT.

      ***************************************
       RESTART-MAINT SECTION.
      ***************************************
       RESTART-HERE.
           IF ACTIVE-FILE-SW = "OFF"
              PERFORM ENTRY-MODULE
           ELSE
              PERFORM READ-TRANS-FILE
              MOVE "OFF" TO ACTIVE-FILE-SW.

           IF IO-TYPE NOT = "RWR"
              IF IO-TYPE NOT = "DEL"
                 IF IO-TYPE NOT = "CLR"
                    IF IO-TYPE NOT = "END"
                       GO TO RESTART-HERE.

      ******************************************************************
       ENTER-BRANCH-MMYY-KEYS-MODULE SECTION.
      ******************************************************************

           MOVE "ON" TO LEAVE-BRANCH-MMYY-SW.
           MOVE 0    TO NEW-FLAG.

           IF ( ACTIVE-FILE-SW NOT = "ON" )
              GO TO ENTER-BRANCH.

      * SET KEY THEN DISPLAY IF KEY FOR TRFILE EXISTS OTHERWISE
      * FORCE USER TO ENTER KEY.

           MOVE SAVEBRNO TO BR-NO.

           MOVE SAVEKEY  TO LPWSTRP-TRP1-KEY TRP1-KEY.
           MOVE SPACES   TO SAVEBRNO SAVEKEY.

           PERFORM LOAD-BRANCH-MODULE.
           IF ( BR-PATH = SPACES )
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

           PERFORM DISPLAY-SCAN-KEY.

           GO TO EXIT-BRANCH-MMYY-MODULE.

      *-----------------------------------------------------------------
       ENTER-BRANCH.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRANCH." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO SAVE-BRNO.
           PERFORM SCREENIO.
      * FORCE BRANCH YOU ARE IN

      *    MOVE "ENNN040BRANCH." TO VDU.
      *    PERFORM SCREENIO.

      *    IF VDUSTATUS = "1>"
      *       MOVE "X" TO ERRCD
      *       GO TO END-ROUTINE.
      *    IF VDUSTATUS NOT = "00"
      *       GO TO ENTER-BRANCH.

      *    MOVE VDUNUM0 TO BR-NO SAVE-BRNO.

           PERFORM LOAD-BRANCH-MODULE.
           IF ( BR-PATH = SPACES )
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
      *    ENTER MONTH
      *-----------------------------------------------------------------
       ENTER-MONTH.
           MOVE "ENNN040MMYY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MONTH.

           MOVE VDUNUM0 TO SAVE-MMYY.
           IF SAVE-MM < 1 OR > 12
              MOVE "INVALID MMYY, PLEASE RE-ENTER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-MONTH.

           PERFORM OPEN-TRP1-FILE.

       EXIT-BRANCH-MMYY-MODULE.
           EXIT.

      ******************************************************************
       ENTER-KEYS-MODULE SECTION.
      ******************************************************************
       ENTER-DAY.
           MOVE "F6 - SCAN"    TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPACES         TO IO-TYPE.

           MOVE 0 TO LPWSTRP-TP-DATE LPWSTRP-TP-BRNO
                     LPWSTRP-TP-CLASS LPWSTRP-TP-ACCTNO
                     LPWSTRP-TP-LINE.

           MOVE 0              TO NEW-FLAG.

           MOVE "ENNN020DAY."  TO VDU.
           PERFORM SCREENIO.

           MOVE SPACES         TO MESS.
           PERFORM CALL-TMMSG.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1U"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1<"
              MOVE SAVE-BRNO  TO TRPSCN-BRNO
              MOVE SAVE-MMYY  TO TRPSCNW-MMYY
              MOVE "Y"        TO TRPSCN-RETURN
              MOVE "WI/TRPSCN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH TRPSCN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF TRPSCN-STATUS NOT = "00"
                 GO TO ENTER-DAY
              ELSE
                 MOVE TRPSCN-TRP1-KEY TO LPWSTRP-TRP1-KEY
                 PERFORM DISPLAY-KEY
                 GO TO READ-TRANS-FILE.

           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DAY.

           MOVE VDUNUM0 TO DATE-MMDDYY-DD.
           MOVE SAVE-MM TO DATE-MMDDYY-MM.
           MOVE SAVE-YY TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.

           MOVE DATE-YYYYMMDD TO LPWSTRP-TP-DATE.

       ENTER-BRNO.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRNO." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO LPWSTRP-TP-BRNO
                                      TP-BRNO.
           PERFORM SCREENIO.
      * FORCE BRANCH YOU ARE IN

      *    MOVE "ENNN040BRNO." TO VDU.
      *    PERFORM SCREENIO.

      *    IF VDUSTATUS = "10"
      *       MOVE "CLR" TO IO-TYPE
      *       GO TO END-ENTER-KEYS-MODULE.
      *    IF VDUSTATUS = "1U"
      *       GO TO ENTER-DAY.
      *    IF VDUSTATUS = "1>"
      *       MOVE "X" TO ERRCD
      *       MOVE "END" TO IO-TYPE
      *       GO TO END-ENTER-KEYS-MODULE.
      *    IF VDUSTATUS NOT = "00"
      *       GO TO ENTER-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DO *NOT* SET FILPATH-WORKERS FROM THIS READ-BR-FILE.
      *    THIS BRNO MAYBE DIFFERENT FROM THE ONE USED FOR THE
      *    PREVIOUS READ-BR-FILE USED TO GET TRANS-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE VDUNUM0 TO BR-NO LPWSTRP-TP-BRNO TP-BRNO.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           MOVE "S NA200BRDESC." TO VDU.
           MOVE BR-NAME          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S NA010CODE." TO VDU.
           MOVE "P"            TO VDUBUF.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       ENTER-CLASS.
           MOVE "ENNN020CLASS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-DAY.

           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CLASS.

           MOVE VDUNUM0 TO LPWSTRP-TP-CLASS.

      *-----------------------------------------------------------------
       ENTER-NUMBER.
           MOVE "ENNN060NUMBER." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-CLASS.

           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-NUMBER.

           MOVE VDUNUM0 TO LPWSTRP-TP-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-LINE.
           MOVE "ENNN020LINE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-NUMBER.

           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-LINE.

           MOVE VDUNUM0 TO LPWSTRP-TP-LINE.

      *-----------------------------------------------------------------
       READ-TRANS-FILE.

           PERFORM OPEN-TRP1-FILE.
           MOVE LPWSTRP-TRP1-KEY TO TRP1-KEY.
           PERFORM READ-TRP1-FILE.
           IF IO-FG NOT = 0
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           MOVE TRP-REC TO ORIGINAL-TRP-REC.

           IF NEW-REC = "Y"
              MOVE "S  A000NEW."        TO VDU
              MOVE "*** NEW RECORD ***" TO VDUBUF
              PERFORM SCREENIO
              MOVE 1 TO NEW-FLAG
           ELSE
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG.

       END-ENTER-KEYS-MODULE.
           EXIT.

      ******************************************************************
      *    E N T R Y   M O D U L E
      *
      *        NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                   1 - INITIAL NEW RECORD ENTRY
      *                   2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.

       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.

           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

      *--->IF VDUSTATUS = "1#"
      *       PERFORM WRITE-MODULE
      *       MOVE "CON" TO IO-TYPE
      *------>GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR = 2
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE FORM-SEL1 TO PAGE-NUMBER
                 IF NOT VALID-PAGE
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE "RWR" TO IO-TYPE
                    GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC (ELE)  TO VDU.
           MOVE ELE         TO VDUROW.
           MOVE 0           TO VDUCOL.
           MOVE "."         TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
      *TEST-F12.
      *    IF VDUSTATUS = "1#"
      *       PERFORM WRITE-MODULE
      *       MOVE "CON" TO IO-TYPE
      *       GO TO END-ENTRY-MODULE.
      *
      *-----------------------------------------------------------------

       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 F-16 F-17 F-18 F-19 F-20
                 DEPENDING ON ELE.
           SUBTRACT 20 FROM ELE GIVING OVER-20-ELE.
           GO TO F-21 F-22 F-23 F-24 F-25
                 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34 F-35
                 F-36 F-37 F-38 F-39 F-40
                 DEPENDING ON OVER-20-ELE.
           SUBTRACT 20 FROM OVER-20-ELE GIVING OVER-40-ELE.
           GO TO F-41 F-42 F-43 F-44
                 DEPENDING ON OVER-40-ELE.

           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *    *      SET RECORD ELEMENTS
      *****************************************************************
       F-01.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE WORK-GL TO TP-GLNO(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUNUM2 TO TP-GLAMT(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE WORK-GL TO TP-GLNO(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUNUM2 TO TP-GLAMT(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE WORK-GL TO TP-GLNO(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUNUM2 TO TP-GLAMT(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUNUM2 TO TP-WORKAMT(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUNUM2 TO TP-WORKAMT(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUNUM2 TO TP-WORKAMT(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUNUM2 TO TP-WORKAMT(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUNUM2 TO TP-WORKAMT(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDUNUM2 TO TP-WORKAMT(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           MOVE VDUNUM2 TO TP-EARNINGS(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUNUM2 TO TP-EARNINGS(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUNUM2 TO TP-EARNINGS(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUNUM2 TO TP-EARNINGS(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUNUM2 TO TP-EARNINGS(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           MOVE VDUNUM2 TO TP-EARNINGS(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
            MOVE VDUNUM0 TO TP-POSTTIME.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           MOVE VDUBUF TO TP-SALEFIN.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           MOVE VDUBUF TO TP-INSURED.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           MOVE VDUBUF TO TP-INSOURS.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           MOVE VDUBUF TO TP-LOANTYPE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           MOVE VDUBUF TO TP-OTHBR-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           MOVE VDUBUF TO TP-SERVACCT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           MOVE VDUBUF TO TP-REV.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-27.
           MOVE VDUBUF TO TP-STATUSFG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-28.
           MOVE VDUBUF TO TP-INSTYPES.

           IF NOT TP-INSTYPES-VALID
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-29.
           MOVE VDUBUF TO TP-MAX-DEFER-FG.
      *    IF TP-MAX-DEFER-FG NOT = "Y"
      *       TP-MAX-DEFER-FG NOT = "N"
      *          GO TO ENTER-ELE.
           IF NOT (TP-MAX-DEFER-FG = "Y" OR "1" OR "2" OR "D" OR "N")
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-30.
           MOVE VDUNUM0 TO TP-LAWCODE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-31.
           MOVE VDUBUF  TO TP-POCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-32.
           MOVE VDUBUF TO TP-PLCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-33.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-34.
           MOVE VDUBUF TO TP-MAKER.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-35.
           MOVE VDUBUF TO TP-IBPC.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-36.
           MOVE VDUNUM0 TO TP-TILLNO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-37.
           MOVE VDUBUF  TO TP-DL-RVCHGS-CODE(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-38.
           MOVE VDUBUF  TO TP-DL-RVCHGS-CODE(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-39.
           MOVE VDUBUF  TO TP-DL-RVCHGS-CODE(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-40.
           MOVE VDUBUF TO TP-DL-PARVPAID-CODE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-41.
           MOVE VDUBUF TO TP-DL-PARVHELD-CODE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-42.
           MOVE VDUBUF TO TP-MANUAL-RB.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-43.
           MOVE VDUBUF TO TP-KEEPAPINT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-44.
           MOVE VDUBUF TO TP-JDCD.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TRP1-FILE.
           MOVE TRP-REC TO CHANGED-TRP-REC.

           IF ORIGINAL-TRP-REC = CHANGED-TRP-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-TRP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF TRP-REC NOT = ORIGINAL-TRP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              PERFORM DISPLAY-FIELDS
              MOVE ORIGINAL-TRP-REC TO TRP-REC
              PERFORM DISPLAY-FIELDS
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-TRP-REC TO TRP-REC.
           PERFORM REWRITE-TRP1-FILE.

           MOVE "C"                        TO ERRLOGW-ACTION.
           MOVE TRP1-KEY                   TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME              TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-WRITE-MODULE.
           PERFORM CLOSE-TRP1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-TRP1-FILE.
           PERFORM READ-TRP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF TRP-REC NOT = ORIGINAL-TRP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-TRP1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-TRP1-FILE.

      ********************************************
       CHECK-GLNO SECTION.
      ********************************************
           MOVE VDUBUF TO SCREEN-GL.
           CALL "C$JUSTIFY" USING SCREEN-GL, "R".
           INSPECT SCREEN-GL REPLACING LEADING " " BY "0".
           MOVE 0 TO WORK-GL.
           IF SCREEN-GL-NUM IS NUMERIC
              MOVE SCREEN-GL-NUM TO WORK-GL
              GO TO DISPLAY-GL. 
           IF SC-DASH = "-"
              IF (SC-BRANCH NUMERIC AND SC-NUMBER NUMERIC)
                 MOVE SC-BRANCH TO WORK-BR
                 MOVE SC-NUMBER TO WORK-NO.
           IF WORK-GL NOT NUMERIC
              MOVE 9 TO IO-FG
              GO TO EXIT-CHECK-GLNO.
       DISPLAY-GL.
           MOVE "S  A120" TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0   TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO VDUBUF.
           PERFORM SCREENIO.
           IF WORK-GL = 0
              MOVE 0 TO IO-FG.
       EXIT-CHECK-GLNO.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

            IF TP-GLAMT(1) NOT = 0
               MOVE TP-GLNO(1) TO WORK-GL
               MOVE WORK-BR TO DISP-BR
               MOVE WORK-NO TO DISP-NO
               MOVE DISP-GL TO F01.

            MOVE TP-GLAMT(1)           TO F02.

            IF TP-GLAMT(2) NOT = 0
               MOVE TP-GLNO(2) TO WORK-GL
               MOVE WORK-BR TO DISP-BR
               MOVE WORK-NO TO DISP-NO
               MOVE DISP-GL TO F03.

            MOVE TP-GLAMT(2)           TO F04.

            IF TP-GLAMT(3) NOT = 0
               MOVE TP-GLNO(3) TO WORK-GL
               MOVE WORK-BR TO DISP-BR
               MOVE WORK-NO TO DISP-NO
               MOVE DISP-GL TO F05.

            MOVE TP-GLAMT(3)           TO F06.
            MOVE TP-WORKAMT(1)         TO F07.
            MOVE TP-WORKAMT(2)         TO F08.
            MOVE TP-WORKAMT(3)         TO F09.
            MOVE TP-WORKAMT(4)         TO F10.
            MOVE TP-WORKAMT(5)         TO F11.
            MOVE TP-WORKAMT(6)         TO F12.
            MOVE TP-EARNINGS(1)        TO F13.
            MOVE TP-EARNINGS(2)        TO F14.
            MOVE TP-EARNINGS(3)        TO F15.
            MOVE TP-EARNINGS(4)        TO F16.
            MOVE TP-EARNINGS(5)        TO F17.
            MOVE TP-EARNINGS(6)        TO F18.
           MOVE TP-POSTTIME           TO F19.
           MOVE TP-SALEFIN            TO F20.
           MOVE TP-INSURED            TO F21.
           MOVE TP-INSOURS            TO F22.
           MOVE TP-LOANTYPE           TO F23.
           MOVE TP-OTHBR-FG           TO F24.
           MOVE TP-SERVACCT           TO F25.
           MOVE TP-REV                TO F26.
           MOVE TP-STATUSFG           TO F27.
           MOVE TP-INSTYPES           TO F28.
           MOVE TP-MAX-DEFER-FG       TO F29.
           MOVE TP-LAWCODE            TO F30.
           MOVE TP-POCD               TO F31.
           MOVE TP-PLCD               TO F32.
           MOVE SPACES                TO F33.
           MOVE TP-MAKER              TO F34.
           MOVE TP-IBPC               TO F35.
           MOVE TP-TILLNO             TO F36.
           MOVE TP-DL-RVCHGS-CODE(1)  TO F37.
           MOVE TP-DL-RVCHGS-CODE(2)  TO F38.
           MOVE TP-DL-RVCHGS-CODE(3)  TO F39.
           MOVE TP-DL-PARVPAID-CODE   TO F40.
           MOVE TP-DL-PARVHELD-CODE   TO F41.
           MOVE TP-MANUAL-RB          TO F42.
           MOVE TP-KEEPAPINT          TO F43.
           MOVE TP-JDCD               TO F44.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************

           MOVE SAVE-BRNO        TO D-BRANCH.

           MOVE LPWSTRP-TP-DATE  TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO D-MM.
           MOVE DATE-YYYYMMDD-YY TO D-YY.

           MOVE DATE-YYYYMMDD-DD       TO D-DAY.
           MOVE LPWSTRP-TP-BRNO        TO D-BRNO.
           MOVE " "                    TO D-CODE.
           MOVE LPWSTRP-TP-CLASS       TO D-CLASS.
           MOVE LPWSTRP-TP-ACCTNO      TO D-NUMBER.
           MOVE LPWSTRP-TP-LINE        TO D-LINE.

           MOVE KEY-BUF TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE EXT-CONAME-CONAME TO TRPMN2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRPMN2-SYSDATE.
           DISPLAY SPACES UPON TRPMIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRPMN2-SCREEN UPON TRPMIN-WINDOW-HANDLE.
           MOVE TRPMN2-FORM-FIELDS TO WR-BUF.
           MOVE TRPMN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE TRPMN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRPMN2-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRPMN2_EVA.CPY".
           END-EVALUATE.

      ******************************************************************
       DISPLAY-SCAN-KEY SECTION.
      ******************************************************************

           MOVE "SNNN040BRANCH." TO VDU
           MOVE BR-NO TO VDUNUM0   SAVE-BRNO.
           PERFORM SCREENIO.

           MOVE LPWSTRP-TP-DATE  TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO DATE-MMYY-MM.
           MOVE DATE-YYYYMMDD-YY TO DATE-MMYY-YY.
           MOVE "SNNN040MMYY." TO VDU.
           MOVE DATE-MMYY      TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020DAY." TO VDU.
           MOVE DATE-YYYYMMDD-DD TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BRNO." TO VDU.
           MOVE LPWSTRP-TP-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

      *    MOVE "S NA010CODE."  TO VDU.
      *    MOVE LPWSTRP-TP-CODE TO VDUBUF.
      *    PERFORM SCREENIO.

           MOVE "SNNN020CLASS." TO VDU.
           MOVE LPWSTRP-TP-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060NUMBER." TO VDU.
           MOVE LPWSTRP-TP-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LINE." TO VDU.
           MOVE LPWSTRP-TP-LINE TO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       DISPLAY-SCAN-KEY-EXIT.
           EXIT.
     
      ******************************************************************
       LOAD-BRANCH-MODULE SECTION.
      ******************************************************************
     
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              MOVE SPACES                  TO BR-PATH
              GO TO LOAD-BRANCH-MODULE-EXIT.
     
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.
     
           MOVE "B"                        TO TRP-PATH-OVERRIDE.
     
       LOAD-BRANCH-MODULE-EXIT.
           EXIT.
     

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPTRP1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           MOVE BR-NO TO SAVEBRNO.
           PERFORM CLOSE-FILES.

      *-----------------------------------------------------------------
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TRPMN2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE TRPMIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
