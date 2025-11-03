       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRPMN1.
       DATE-WRITTEN. 12-09-94.
      **************************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   TRANSACTION FILE MAINTENANCE (PAYMENTS)
      *                   PAGE 1
      *
      * REV:
      *  MJD 050995 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *  MJD 062995 ADDED TP-USERID AND TP-POSTDATE. SET NO RESET ON ALL
      *             DISPLAY FIELDS.
      *  MJD 071296 CHANGED DISPLAY/ENTRY OD PDTH DATES TO ZZ/ZZ/ZZ
      *  JTG 093098 ALLOWED LN-CURBAL & LN-PURBAL TO GO NEGATIVE; REGENCY REVOL
      *  JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99    LENDMARK PR#J021
      * MJD 131203 A30 CONVERSION COMPLETED
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *         ADDED LIBLP/LPFRTR; REMOVED LIBLP/LPTRTMPW.
      *         ADDED LPFRTR-UPDATE-CHGLOG; REMOVED WRITE-TR-CHANGE-LOG.
      *  CS 170508 ACTIVATE 8 DIGIT DATES
      * BAH 2019.0508 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      * BLM 20210811 CHANGED SAVEKEY TO X(26)
      * BLM 20210817 CHANGED FIELD 23 TO TP-POOLID, MOVE TP-PRIOR-POOLID
      *              TO FIELD 231
      * BLM 2021.1116 SAVE-MMYY ALWAYS SPACES, CAUSES CAST ERRORS,
      *               POPULATE WITH VALUES IN SAVEKEY
      * BAH 2023.1006 CHANGED SCREEN FIELD USERID TO MUST Y TO ALLOW
      *               LOWERCASE
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

      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/TRPMN1 S35 09/23/22-11:43:07 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".

       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  OVER-20-ELE     PIC 9(2).

       01  ACTIVE-FILE-SW        PIC X(3)    VALUE "OFF".
       01  LEAVE-BRANCH-MMYY-SW  PIC X(3)    VALUE "OFF".

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
           88  VALID-PAGE  VALUE "2" "3" "4".
       01  SOCSEC-KEY      PIC 9(9).
       01  SS-KEY REDEFINES SOCSEC-KEY.
           03  SS-1        PIC 999.
           03  SS-2        PIC 99.
           03  SS-3        PIC 9999.

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

      *----------------------------------------------------------------

       01  ORIGINAL-TRP-REC    PIC X(TRP-SIZE).
       01  CHANGED-TRP-REC     PIC X(TRP-SIZE).


      *****************************************************************
      *    *     SCREEN DISPLAY BUFFER      *
      *****************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC XX.
           03  F02         PIC 99/99/99       BLANK WHEN ZERO.
           03  F03         PIC 99/99/99       BLANK WHEN ZERO.
           03  F04         PIC X(5).
           03  F05         PIC ZZZZZZ9.99-    BLANK WHEN ZERO.
           03  F06         PIC ZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F07         PIC ZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F08         PIC ZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F09         PIC ZZZ,ZZ9.99-    BLANK WHEN ZERO.
           03  F10         PIC ZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F11         PIC ZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F12         PIC ZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F13         PIC ZZZ,ZZ9.99-    BLANK WHEN ZERO.
           03  F14         PIC 999/99/9999    BLANK WHEN ZERO.
           03  F15         PIC 9(3)           BLANK WHEN ZERO.
           03  F16         PIC ZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F17         PIC ZZ/ZZ/ZZ       BLANK WHEN ZERO.
           03  F18         PIC 9999.99        BLANK WHEN ZERO.
           03  F19         PIC ZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F20         PIC ZZ/ZZ/ZZ       BLANK WHEN ZERO.
           03  F21         PIC ZZZ9           BLANK WHEN ZERO.
           03  F22         PIC ZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F23         PIC 9(5)           BLANK WHEN ZERO.
           03  F231        PIC 9(5)           BLANK WHEN ZERO.
           03  F24         PIC ZZ9.999        BLANK WHEN ZERO.
           03  F25         PIC XX.
           03  F26         PIC Z(3)           BLANK WHEN ZERO.
           03  F27         PIC 99/99/99       BLANK WHEN ZERO.
           03  F28         PIC ZZ9-           BLANK WHEN ZERO.
           03  F29         PIC ZZZ,ZZ9.99-    BLANK WHEN ZERO.
           03  F30         PIC 99/99/99       BLANK WHEN ZERO.
           03  F31         PIC 99/99/99       BLANK WHEN ZERO.
           03  F32         PIC 99/99/99       BLANK WHEN ZERO.
           03  F33         PIC ZZZ,ZZ9.99-    BLANK WHEN ZERO.
           03  F34         PIC ZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F35         PIC ZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F36         PIC ZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F37         PIC ZZZZ9.99-      BLANK ZERO.
           03  F38         PIC X(10).
           03  F39         PIC 99/99/99       BLANK WHEN ZERO.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
               "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(44) VALUE
               "210 220 230 231 240 250 260 270 280 290 300 ".
           03  FILLER      PIC X(37) VALUE
               "310 320 330 340 350 360 370 380 390.".

      ****************************************************************
      *    *   ELEMENT SPECIFICATIONS
      ****************************************************************
       01  MAX             PIC 99   VALUE 39.
       01  SPEC-TABLE.
      * 1. TRCD
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      * 2. PAY-DATE
           03  FILLER      PIC X(8) VALUE "E NM060 ".
      * 3. DUE-DATE
           03  FILLER      PIC X(8) VALUE "E NM060 ".
      * 4. REFNO
           03  FILLER      PIC X(8) VALUE "E NA050 ".
      * 5. TRP-AMOUNT
           03  FILLER      PIC X(8) VALUE "ENNS072 ".
      * 6. APOTH
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      * 7. APINT
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      * 8. APLC
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      * 9. APCUR
           03  FILLER      PIC X(8) VALUE "E NS062 ".
      *10. OTHBAL
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *11. INTBAL
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *12. LC BAL
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *13. CURBAL
           03  FILLER      PIC X(8) VALUE "E NS062 ".
      *14. SOC-SEC-NUMBER
           03  FILLER      PIC X(8) VALUE "ERNX090 ".
      *15. ORG TERM
           03  FILLER      PIC X(8) VALUE "E NN030 ".
      *16. INT DUE
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *17. PD-THRU
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *18. DL-NUMB
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      *19. DL-PROC
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *20. LCPD
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *21. POSTING-BR
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      *22. INTPAID
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *23. POOLID
           03  FILLER      PIC X(8) VALUE "ENNN050 ".
      *24. NWSTRATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      *25. REVTRCD
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *25. REVSEQNO
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      *27. PO-DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *28. TOT NO DEF
           03  FILLER      PIC X(8) VALUE "E NS030 ".
      *29. LN AMT
           03  FILLER      PIC X(8) VALUE "E NS062 ".
      *30. OR 1ST PAY
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *31. OLD 1ST PAY DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *32. 1ST PAY DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *33. INSCOMM
           03  FILLER      PIC X(8) VALUE "E NS062 ".
      *34. ADDON-BAL
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *35. DLPARVPAID-ADJ
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *36. DLPARVHELD-ADJ
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *37. TP-APOT2
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *38. USER-ID
           03  FILLER      PIC X(8) VALUE "E NA100 ".
      *39. POSTING DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 39 TIMES.
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
           03  D-ACCTNO    PIC 999999.
           03  D-LINE      PIC 99.
       01  KEY-LIST.
           03  FILLER      PIC X(39) VALUE
               "BRANCH MMYY DAY BRNO CODE CLASS NUMBER ".
           03  FILLER      PIC X(5) VALUE
               "LINE.".

      ****************************************************************
       COPY "LIBWI/TRPSCNW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TRPMN1_DEF.CPY".
       COPY "LIBMN/TRPMN1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/TRPMN1_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ****************************************************************
      *
      *    M A I N   M O D U L E
      *
      ****************************************************************
        MAIN-PROGRAM SECTION.
      D     STOP MESS.

        COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TRPMN1"      TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO TRPMIN-LINK.
           MOVE SAVEKEY-MM TO SAVE-MM.
           MOVE SAVEKEY-YY TO SAVE-YY.


           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           IF SAVEKEY NOT = " "
              MOVE "ON" TO ACTIVE-FILE-SW.

           MOVE "OFF" TO LEAVE-BRANCH-MMYY-SW.

      *---------------------------------------------------------------
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
              GO TO MAIN-MODULE.

           PERFORM RESTART-MAINT.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
              MOVE "OFF" TO LEAVE-BRANCH-MMYY-SW
              GO TO MAIN-MODULE.

           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P1
              MOVE 2 TO FORM-SEL1.

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

      ****************************************************************
       ENTER-BRANCH-MMYY-KEYS-MODULE SECTION.
      ****************************************************************
       ENTER-SAVE-BRANCH.
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

       ENTER-BRANCH.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRANCH." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO SAVE-BRNO.
           PERFORM SCREENIO.
      * FORCE BRANCH YOU ARE IN

           PERFORM LOAD-BRANCH-MODULE.
           IF ( BR-PATH = SPACES )
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

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

      ****************************************************************
       ENTER-KEYS-MODULE SECTION.
      ****************************************************************
       ENTER-DAY.
           MOVE "F6 - SCAN"   TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPACES        TO IO-TYPE.

           MOVE 0 TO LPWSTRP-TP-DATE LPWSTRP-TP-BRNO 
                     LPWSTRP-TP-CLASS LPWSTRP-TP-ACCTNO
                     LPWSTRP-TP-LINE.

           MOVE 0             TO NEW-FLAG.

           MOVE "ENNN020DAY." TO VDU.
           PERFORM SCREENIO.

           MOVE SPACES        TO MESS.
           PERFORM CALL-TMMSG.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1U"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1<"
              MOVE SAVE-BRNO   TO TRPSCN-BRNO
              MOVE SAVE-MMYY   TO TRPSCNW-MMYY
              MOVE "Y"         TO TRPSCN-RETURN
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

      *---------------------------------------------------------------
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
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S NA010CODE." TO VDU.
           MOVE "P"            TO VDUBUF.
           PERFORM SCREENIO.

      *---------------------------------------------------------------
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

      *--->IF VDUSTATUS = "1#"
      *       MOVE "CON" TO IO-TYPE
      *------>GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CLASS.

           MOVE VDUNUM0 TO LPWSTRP-TP-CLASS.

      *---------------------------------------------------------------
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

      *---------------------------------------------------------------
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

      *--->IF VDUSTATUS = "1#"
      *       MOVE "CON" TO IO-TYPE
      *------>GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-LINE.

           MOVE VDUNUM0 TO LPWSTRP-TP-LINE.

      *---------------------------------------------------------------
       READ-TRANS-FILE.
           PERFORM OPEN-TRP1-FILE.
           MOVE LPWSTRP-TRP1-KEY TO TRP1-KEY.
           PERFORM READ-TRP1-FILE.
           IF IO-FG = 0
              MOVE TRP-REC TO ORIGINAL-TRP-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-ENTER-KEYS-MODULE.
           EXIT.

      ****************************************************************
      *        E N T R Y    M O D U L E
      *
      *        NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                   1 - INITIAL NEW RECORD ENTRY
      *                   2 - MODIFY NEW RECORD
      ****************************************************************
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

      *---------------------------------------------------------------
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

      *---------------------------------------------------------------
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

           MOVE SPEC (ELE) TO VDU.
           MOVE ELE        TO VDUROW.
           MOVE 0          TO VDUCOL.
           MOVE "."        TO VDUDECIMAL.
           PERFORM SCREENIO.

      *---------------------------------------------------------------
       TEST-F4.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *---------------------------------------------------------------
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.

      *---------------------------------------------------------------
      *TEST-F12.
      *    IF VDUSTATUS = "1#"
      *       PERFORM WRITE-MODULE
      *       MOVE "CON" TO IO-TYPE
      *       GO TO END-ENTRY-MODULE.
      *
      *---------------------------------------------------------------
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
                 F-36 F-37 F-38 F-39
                 DEPENDING ON OVER-20-ELE.
           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *    *      SET RECORD ELEMENTS
      *****************************************************************
       F-01.
           MOVE VDUBUF TO TP-TRCD.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-02.
           MOVE VDU-DATE-YYYYMMDD  TO TP-PAYDATE.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-03.
           MOVE VDU-DATE-YYYYMMDD  TO TP-DUEDATE.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO TP-REFNO.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-05.
           MOVE VDUNUM2 TO TP-TRAMT.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-06.
           MOVE VDUNUM2 TO TP-APOTH.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-07.
           MOVE VDUNUM2 TO TP-APINT.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-08.
           MOVE VDUNUM2 TO TP-APLC.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-09.
           MOVE VDUNUM2 TO TP-APCUR.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-10.
           MOVE VDUNUM2 TO TP-OTHBAL.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-11.
           MOVE VDUNUM2 TO TP-INTBAL.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-12.
           MOVE VDUNUM2 TO TP-LCBAL.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-13.
           MOVE VDUNUM2 TO TP-CURBAL.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-14.
           MOVE VDUNUM0 TO SOCSEC-KEY.
           IF SS-KEY IS NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE SOCSEC-KEY TO TP-SSNO.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-15.
           MOVE VDUNUM0 TO TP-ORGTERM.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-16.
           MOVE VDUNUM2 TO TP-INTDUE.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-17.
           MOVE VDU-DATE-YYYYMMDD TO TP-PDTH-DATE.

           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-18.
           MOVE VDUNUM2 TO TP-DLNO.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-19.
           MOVE VDUNUM2 TO TP-DLPROC.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-20.
           MOVE VDU-DATE-YYYYMMDD  TO TP-LCPDTH-DATE.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-21.
           MOVE VDUNUM0 TO TP-POSTING-BR.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-22.
           MOVE VDUNUM2 TO TP-INTPAID.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-23.
           MOVE VDUNUM0 TO TP-POOLID.
       F-231.
           MOVE "ENNN050231." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT ENTER-KEY GO TO F-231.
           MOVE VDUNUM0 TO TP-PRIOR-POOLID.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-24.
           MOVE VDUNUM3 TO TP-NEWSTAT-RATE.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-25.
           MOVE VDUBUF  TO TP-REVTRCD.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-26.
           MOVE VDUNUM0 TO TP-REVSEQNO.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-27.
           MOVE VDU-DATE-YYYYMMDD  TO TP-PODATE.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-28.
           MOVE VDUNUM0 TO TP-TOTNODEF.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-29.
           MOVE VDUNUM2 TO TP-LNAMT.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-30.
           MOVE VDU-DATE-YYYYMMDD  TO TP-ORIG-1STPYDATE.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-31.
           MOVE VDU-DATE-YYYYMMDD  TO TP-OLD1ST-PYDATE.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-32.
           MOVE VDU-DATE-YYYYMMDD  TO TP-1STPYDATE.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-33.
           MOVE VDUNUM2 TO TP-INSCOMM.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-34.
           MOVE VDUNUM2 TO TP-OT2BAL.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-35.
           MOVE VDUNUM2 TO TP-DLPARVPAID-ADJ.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-36.
           MOVE VDUNUM2 TO TP-DLPARVHELD-ADJ.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-37.
           MOVE VDUNUM2 TO TP-APOT2.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-38.
           MOVE VDUBUF  TO TP-USERID.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       F-39.
           MOVE VDU-DATE-YYYYMMDD  TO TP-POSTDATE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ****************************************************************
       WRITE-MODULE SECTION.
      ****************************************************************
           PERFORM OPEN-TRP1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

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
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-TRP-REC TO TRP-REC.
           PERFORM REWRITE-TRP1-FILE.

           MOVE "C"                        TO ERRLOGW-ACTION.
           MOVE TRP1-KEY                   TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME              TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-TRP1-FILE.

           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.

       END-WRITE-MODULE.
           PERFORM CLOSE-TRP1-FILE.

      ****************************************************************
       DELETE-MODULE SECTION.
      ****************************************************************
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

      ****************************************************************
       NEW-RECORD-SETUP SECTION.
      ****************************************************************
           MOVE 1                    TO NEW-FLAG.
           MOVE "S  A000NEW."        TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-TRP-FILE.
           MOVE LPWSTRP-TRP1-KEY TO TRP1-KEY.
           MOVE 0 TO ELE.

      ****************************************************************
       DISPLAY-FIELDS SECTION.
      ****************************************************************
           MOVE SPACES      TO DISPLAY-BUF.
           MOVE TP-TRCD     TO F01.
           MOVE TP-PAYDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F02.

           MOVE TP-DUEDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F03.

           MOVE TP-REFNO    TO F04.
           MOVE TP-TRAMT    TO F05.
           MOVE TP-APOTH    TO F06.
           MOVE TP-APINT    TO F07.
           MOVE TP-APLC     TO F08.
           MOVE TP-APCUR   TO F09.
           MOVE TP-OTHBAL   TO F10.
           MOVE TP-INTBAL   TO F11.
           MOVE TP-LCBAL    TO F12.
           MOVE TP-CURBAL   TO F13.

           IF TP-SSNO > ZERO
              MOVE TP-SSNO TO F14
              INSPECT F14 REPLACING ALL "/" BY "-".

           MOVE TP-ORGTERM        TO F15.
           MOVE TP-INTDUE         TO F16.
           MOVE TP-PDTH-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO F17.

           MOVE TP-DLNO           TO F18.
           MOVE TP-DLPROC         TO F19.
           MOVE TP-LCPDTH-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F20.
 
           MOVE TP-POSTING-BR     TO F21.
           MOVE TP-INTPAID        TO F22.
           MOVE TP-POOLID         TO F23.
           MOVE TP-PRIOR-POOLID   TO F231.
           MOVE TP-NEWSTAT-RATE   TO F24.
           MOVE TP-REVTRCD        TO F25.
           MOVE TP-REVSEQNO       TO F26.
           MOVE TP-PODATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F27.

           MOVE TP-TOTNODEF       TO F28.
           MOVE TP-LNAMT          TO F29.
           MOVE TP-ORIG-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F30.

           MOVE TP-OLD1ST-PYDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F31.
        
           MOVE TP-1STPYDATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F32.

           MOVE TP-INSCOMM        TO F33.
           MOVE TP-OT2BAL         TO F34.
           MOVE TP-DLPARVPAID-ADJ TO F35.
           MOVE TP-DLPARVHELD-ADJ TO F36.
           MOVE TP-APOT2          TO F37.
           MOVE TP-USERID         TO F38.
           MOVE TP-POSTDATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F39.

      *---------------------------------------------------------------
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ****************************************************************
       DISPLAY-KEY SECTION.
      ****************************************************************

           MOVE SAVE-BRNO        TO D-BRANCH.

           MOVE LPWSTRP-TP-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO D-MM.
           MOVE DATE-YYYYMMDD-YY TO D-YY.


           MOVE DATE-YYYYMMDD-DD       TO D-DAY.
           MOVE LPWSTRP-TP-BRNO        TO D-BRNO.
           MOVE SPACES                 TO D-CODE.
           MOVE LPWSTRP-TP-CLASS       TO D-CLASS.
           MOVE LPWSTRP-TP-ACCTNO      TO D-ACCTNO.
           MOVE LPWSTRP-TP-LINE        TO D-LINE.

           MOVE KEY-BUF TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           MOVE EXT-CONAME-CONAME TO TRPMN1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRPMN1-SYSDATE.
           DISPLAY SPACES UPON TRPMIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRPMN1-SCREEN UPON TRPMIN-WINDOW-HANDLE.
           MOVE TRPMN1-FORM-FIELDS TO WR-BUF.
           MOVE TRPMN1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE TRPMN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRPMN1-HELP--DIR TO HELP-LINK-DIR.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRPMN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       DISPLAY-SCAN-KEY SECTION.
      ******************************************************************

           MOVE "SNNN040BRANCH." TO VDU.
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

           MOVE "S NA010CODE."  TO VDU.
           MOVE SPACES          TO VDUBUF.
           PERFORM SCREENIO.

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
     
      ****************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ****************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTRPCL.CPY".

      ****************************************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************************************
       COPY "LIBGB/ACCESS.CPY".
      *---------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *---------------------------------------------------------------
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *---------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *---------------------------------------------------------------
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *---------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *---------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ****************************************************************
       IO-ROUTINES SECTION.
      ****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPTRP1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ****************************************************************
       END-ROUTINE SECTION.
      ****************************************************************
       END-RUN.
           MOVE BR-NO TO SAVEBRNO.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TRPMN1-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE TRPMIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
