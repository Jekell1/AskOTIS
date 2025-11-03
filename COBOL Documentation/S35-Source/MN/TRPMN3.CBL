       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRPMN3.
       DATE-WRITTEN. 12-12-94.
      ************************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   TRANSACTION FILE MAINTENANCE
      *
      *REV:
      * MJD 050995 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      * KEC 111098 ADDED TP-REVOLVING
      * JTG 120498 ADDED TP-RINSPREM(4) TP-RINSCOMM(4) REVOLVING REGENCY
      *            ADDED TP-APOTH-AMT(1-8) REVOLVING REGENCY
      * MJD 001016 ADDED TP-COLLECTOR (BAY PR#1317)
      * MJD 001113 ADDED TP-PENALTY-INT (NEWS PR#1264)
      * BAH 011109 ADDED TP-INSCOMP , REGACC PR# 1617
      * BAH 011210 ADDED TP-PROMDATE, TP-PROMAMT, TP-PROMID TO THE TRANS
      *            THAT CAUSES THE LOAN FIELDS TO CLEAR, REGACC PR# 1662
      * JTG 020322 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *            ADDED TP-INS-TAX-RB                               WORLD #288
      *  BAH 020502 ADDED TP-PROMDAYS-PASTDUE FOR "DAYS PAST DUE AT TIME
      *             PROMISE TAKEN", REGACC PR# 1783
      *  CS  021126 ADDED TP-INS-TAX-COUNTY-CD, TP-INS-TAX-CITY-CD [WORLD PR#288
      * MJD 131203 A30 CONVERSION COMPLETED
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *         ADDED LIBLP/LPFRTR; REMOVED LIBLP/LPTRTMPW.
      *         ADDED LPFRTR-UPDATE-CHGLOG; REMOVED WRITE-TR-CHANGE-LOG.
      *  CS 170509 ACTIVATE 8 DIGIT DATES
      * BAH 2019.0508 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      * BAH 20190627 REMOVED TP-REVOLVING, PT0153
      * BAH 20190627 REMOVED TP-RINS* TP-APOTH* REVOLVING PT0153
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * BLM 211011 CHANGED SAVEKEY TO X(26)
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/TRPMN3.CBL S34 11/15/21-11:54:23 >".

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
           88  INVALID-ELE VALUE 12 13.
       01  OVER-20-ELE     PIC 9(2).
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

       01  SAVE-BRNO       PIC 9(4).
       01  SAVE-MMYY       PIC 9(4).
       01  SAVE-MMYYX REDEFINES SAVE-MMYY.
           03  SAVE-MM     PIC 99.
           03  SAVE-YY     PIC 99.

       01  PAGE-NUMBER     PIC X(2).
           88  VALID-PAGE  VALUE "1" "2" "4".

       01  TRPMIN-LINK VALUE SPACES.
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
           03  F01         PIC X.
           03  F02         PIC 99             BLANK ZERO.
           03  F03         PIC X.
           03  F04         PIC X.
           03  F05         PIC X(03).
           03  F06         PIC 99/99/99       BLANK ZERO.
           03  F07         PIC ZZZZ9.99       BLANK ZERO.
           03  F08         PIC XXX.
           03  F09         PIC ZZZZ9.99-      BLANK ZERO.
           03  F10         PIC ZZZ.
           03  F11         PIC X(3).
           03  F111        PIC X(3).
           03  F12         PIC X.
           03  F13         PIC X.
           03  F14         PIC ZZZ,ZZ9.99-    BLANK ZERO.
           03  F15         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F16         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F17         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F18         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F19         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F20         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F21         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F22         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F23         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F24         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F25         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F26         PIC 9999.99        BLANK ZERO.
           03  F27         PIC X.
           03  F28         PIC 9(3)           BLANK ZERO.
           03  F29         PIC 99             BLANK ZERO.
           03  F30         PIC ZZZZZZ9.99-    BLANK ZERO.
           03  F31         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F32         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F33         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F34         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F35         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F36         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F37         PIC ZZ,ZZ9.99-     BLANK ZERO.
           03  F38         PIC ZZ,ZZ9.99-     BLANK ZERO.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(44) VALUE
               "110 111 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(40) VALUE
               "210 220 230 240 250 260 270 280 290 300 ".
           03  FILLER      PIC X(32) VALUE
               "310 320 330 340 350 360 370 380.".

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS
      ******************************************************************
       01  MAX             PIC 99   VALUE 38.
       01  SPEC-TABLE.
      * 1. TP REPO FLAG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 2. TP SPRCLASS
           03  FILLER      PIC X(8) VALUE "E NN020 ".
      * 3. TP PLTYPE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 4. 
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 5. TP COLLECTOR
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 6. TP-PROMDATE
           03  FILLER      PIC X(8) VALUE "E NM060 ".
      * 7. TP-PROMAMT
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      * 8. TP-PROMID
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 9. TP-INS-TAX-RB
           03  FILLER      PIC X(8) VALUE "ENNS052 ".
      *10. TP-PROMDAYS-PASTDUE
           03  FILLER      PIC X(8) VALUE "EN N030 ".
      *11. TP-INS-TAX-COUNTY-CD
           03  FILLER      PIC X(8) VALUE "E NA030 ".
      *12. *************NOT IN USE*****************
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *13. *************NOT IN USE*****************
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *14. TP PL OTH AR
           03  FILLER      PIC X(8) VALUE "E NS062 ".
      *15. TP DLRVCHGS ADJ 1
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *16. TP DLRVCHGS ADJ 2
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *17. TP DLRVCHGS ADJ 3
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *18. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *19. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *20. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *21. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *22. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *23. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *24. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *25. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *26. TP LP DLNO
           03  FILLER      PIC X(8) VALUE "EN N042 ".
      *27. TP-UNITPER-CD
           03  FILLER      PIC X(8) VALUE "EN A010 ".
      *28. TP-UNITPER-FREQ
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      *29. TP-INSCOMP
           03  FILLER      PIC X(8) VALUE "EN N020 ".
      *30. TP-PENALTY-INT
           03  FILLER      PIC X(8) VALUE "ENNS072 ".
      *31. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *32. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *33. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *34. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *35. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *36. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *37. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *38. N/A
           03  FILLER      PIC X(8) VALUE "E NS052N".

       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 38 TIMES.
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

      ******************************************************************
       COPY "LIBWI/TRPSCNW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/UPERTSTW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TRPMN3_DEF.CPY".
       COPY "LIBMN/TRPMN3_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/TRPMN3_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N    P R O G R A M    M O D U L E
      *
      ******************************************************************
        MAIN-PROGRAM SECTION.
        COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TRPMN3" TO VDU-FORM-NAME.

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
               MOVE 4 TO FORM-SEL1.

            IF FORM-SEL1 NOT = 0
               IF FORM-P1 = "P"
                  MOVE BR-NO           TO SAVEBRNO
                  MOVE LPWSTRP-TRP1-KEY TO SAVEKEY
                  MOVE FORM-SEL1        TO FORMSEL
                  GO TO END-ROUTINE.

            GO TO MAIN-MODULE.

      *-----------------------------------------------------------------
       RESTART-MAINT.
           IF ACTIVE-FILE-SW = "OFF"
              PERFORM ENTRY-MODULE
           ELSE
              PERFORM READ-TRANS-FILE
              MOVE "OFF" TO ACTIVE-FILE-SW.

           IF IO-TYPE NOT = "RWR"
              IF IO-TYPE NOT = "DEL"
                 IF IO-TYPE NOT = "CLR"
                    IF IO-TYPE NOT = "END"
                          GO TO RESTART-MAINT.

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

      *-----------------------------------------------------------------
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

           IF ( INVALID-ELE )
              GO TO CREATE-ROUTINE.

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
             IF FORM-SEL1 < 0 OR = 3
                 GO TO MODIFY-ROUTINE
             ELSE
      *----------IF FORM-SEL1 NOT = 1
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

           IF ( INVALID-ELE )
              GO TO MODIFY-ROUTINE.

      *-----------------------------------------------------------------
       ENTER-ELE.
           IF ELE = 11
              GO TO CREATE-ROUTINE.

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
                 F-36 F-37 F-38
                 DEPENDING ON OVER-20-ELE.

           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *    *      SET RECORD ELEMENTS
      *****************************************************************
       F-01.
           MOVE VDUBUF   TO TP-REPOCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUNUM0  TO TP-SPRCLASS.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF   TO TP-PLTYPE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO TP-COLLECTOR.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDU-DATE-YYYYMMDD TO TP-PROMDATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUNUM2 TO TP-PROMAMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO TP-PROMID.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUNUM2 TO TP-INS-TAX-RB.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUNUM0 TO TP-PROMDAYS-PASTDUE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUNUM2 TO TP-PL-OTH-AR.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUNUM2 TO TP-DLRVCHGS-ADJ(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUNUM2 TO TP-DLRVCHGS-ADJ(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUNUM2 TO TP-DLRVCHGS-ADJ(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           MOVE VDUNUM2 TO TP-LP-DLNO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-27.
           MOVE VDUBUF TO TP-UNITPER-CD
                          UPERTST-UNITPER-CD.
           PERFORM UPERTST-UNITPER-CODE.
           IF UPERTST-ERRCD NOT = " "
              GO TO ENTER-ELE.
           IF NEW-FLAG NOT = 1
              MOVE 28 TO ELE
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-28.
           MOVE VDUNUM0 TO UPERTST-UNITPER-FREQ TP-UNITPER-FREQ.
           MOVE TP-UNITPER-CD TO UPERTST-UNITPER-CD.
           PERFORM UPERTST-UNITPER-FREQUENCY.
           IF UPERTST-ERRCD NOT = " "
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-29.
           MOVE VDUNUM0 TO TP-INSCOMP.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-30.
           MOVE VDUNUM2 TO TP-PENALTY-INT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-31.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-32.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-33.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-34.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-35.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-36.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-37.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-38.
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

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE TP-REPOCD             TO F01.
           MOVE TP-SPRCLASS           TO F02.
           MOVE TP-PLTYPE             TO F03.
           MOVE SPACES                TO F04.
           MOVE TP-COLLECTOR          TO F05.
           MOVE TP-PROMDATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO F06.
           MOVE TP-PROMAMT            TO F07.
           MOVE TP-PROMID             TO F08.
           MOVE TP-INS-TAX-RB         TO F09.
           MOVE TP-PROMDAYS-PASTDUE   TO F10.
           MOVE TP-INS-TAX-COUNTY-CD  TO F11.
           MOVE TP-INS-TAX-CITY-CD    TO F111.
      *------------------------------>TO F12.
      *------------------------------>TO F13.
           MOVE TP-PL-OTH-AR          TO F14.
           MOVE TP-DLRVCHGS-ADJ(1)    TO F15.
           MOVE TP-DLRVCHGS-ADJ(2)    TO F16.
           MOVE TP-DLRVCHGS-ADJ(3)    TO F17.
           MOVE 0                     TO F18 F19 F20 F21 F22 F23
                                         F24 F25.
           MOVE TP-LP-DLNO            TO F26.
           MOVE TP-UNITPER-CD         TO F27.
           MOVE TP-UNITPER-FREQ       TO F28.
           MOVE TP-INSCOMP            TO F29.
           MOVE TP-PENALTY-INT        TO F30.
           MOVE 0                     TO F31 F32 F33 F34 F35 F36
                                         F37 F38.

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

      ******************************************************
       FORM-RESET SECTION.
      ******************************************************
           MOVE EXT-CONAME-CONAME TO TRPMN3-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRPMN3-SYSDATE.
           DISPLAY SPACES UPON TRPMIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRPMN3-SCREEN UPON TRPMIN-WINDOW-HANDLE.
           MOVE TRPMN3-FORM-FIELDS TO WR-BUF.
           MOVE TRPMN3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************
           MOVE TRPMN3-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRPMN3-HELP--DIR TO HELP-LINK-DIR.

      ******************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRPMN3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       DISPLAY-SCAN-KEY SECTION.
      ******************************************************************

           MOVE "SNNN040BRANCH." TO VDU
           MOVE BR-NO TO VDUNUM0   SAVE-BRNO.
           PERFORM SCREENIO.

           MOVE LPWSTRP-TP-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO DATE-MMYY-MM.
           MOVE DATE-YYYYMMDD-YY TO DATE-MMYY-YY.
           MOVE "SNNN040MMYY." TO VDU.
           MOVE DATE-MMYY      TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020DAY."    TO VDU.
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
       COPY "LIBLP/UPERTST.CPY".

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
           MOVE BR-NO          TO SAVEBRNO.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TRPMN3-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE TRPMIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
