       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRLMN1.
       DATE-WRITTEN.    11-17-94.
      *************************************************************************
      *   TRANSACTION FILE MAINTENANCE (DIRECT LOAN)
      *              PAGE 1
      *
      * REV:
      * BLV 020698 MAKE FIELD # 8 TL-GIFTNET
      * BAH 062998 TILLNO IS NOW ALPHANUMERIC
      * JTG 020620 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *            ADDED TL-MIP-FG
      * JTG 040520 ADDED PAGE 4                                   REGENCY #2287
      * MJD 131203 A30 CONVERSION COMPLETED
      *  CS 160921 HOLD-KEY TO HOLD-TRL-KEY,SAVE KEY FROM X(14) TO X(27)
      *            (TRLMIN-LINK).
      *            REMOVE ENTER-SEQNO AND REFERENCES TO TRL-SEQ 
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *         ADDED LIBLP/LPFRTR; REMOVED LIBLP/LPTRTMPW.
      *         ADDED LPFRTR-UPDATE-CHGLOG; REMOVED WRITE-TR-CHANGE-LOG.
      * BAH 2016.1103 CHANGE TILLNO TO 9(2)
      * BAH 2019.0322 NEW TRLFILE 
      * BAH 2019.0508 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      *               POPULATE WITH VALUES IN SAVEKEY
      * BAH 2025.0124 VALIDATE MONTH ENTERED S35Q-159
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/TRLMN1.CBL S34 11/15/21-14:44:39 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".

      *-----------------------------------------------------------------
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  OVER-20-ELE     PIC 9(2).
       01  OVER-40-ELE     PIC 9(2).

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

       01  TRLMIN-LINK VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(28).
           03  FILLER REDEFINES SAVEKEY.
               05  FILLER     PIC X(6).
               05  SAVEKEY-YY PIC 9(2).
               05  SAVEKEY-MM PIC 9(2).
               05  FILLER     PIC X(18).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL        PIC 99.
           03  FILLER REDEFINES FORMSEL.
               05  FILLER    PIC 9.
               05  FORMNUM   PIC 9.
           03  SAVEBRNO      PIC 9(4).
           03  TRLMIN-WINDOW-HANDLE PIC X(10).

       01  ORIGINAL-TRL-REC    PIC X(TRL-SIZE).
       01  CHANGED-TRL-REC     PIC X(TRL-SIZE).

      ******************************************************************
      *    *      SCREEN DISPLAY BUFFER
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC XXX.
           03  F02         PIC XX.
           03  F03         PIC XX.
           03  F04         PIC X.
           03  F05         PIC X.
           03  F06         PIC ZZ9.
           03  F07         PIC Z9.
           03  F08         PIC X.
           03  F09         PIC X.
           03  F10         PIC X.
           03  F11         PIC X.
           03  F12         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F13         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F14         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F15         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F16         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F17         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F18         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F19         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F20         PIC ZZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F21         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F22         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F23         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F24         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F25         PIC XX.
           03  F26         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F27         PIC XX.
           03  F28         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F29         PIC XX.
           03  F30         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F31         PIC XX.
           03  F32         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F33         PIC XX.
           03  F34         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F35         PIC XX.
           03  F36         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F37         PIC XX.
           03  F38         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F39         PIC XX.
           03  F40         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F41         PIC XX.
           03  F42         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F43         PIC XX.
           03  F44         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.

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

       01  MAX             PIC 99   VALUE 44.

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS
      ******************************************************************
       01  SPEC-TABLE.
      * 1. PURCD
           03  FILLER      PIC X(8) VALUE "E NA030 ".
      * 2. SRCD
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      * 3. COLLCD
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      * 4. SERV ACCT
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 5. LOAN TYPE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 6. NO REG PAY
           03  FILLER      PIC X(8) VALUE "E NN030 ".
      * 7. TILL NUMBER
           03  FILLER      PIC X(8) VALUE "E NN020 ".
      * 8. WBCB (GIFTNET) FLAG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 9. TL-MIP-FG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *10. N/A
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *11. LPEOD5-FG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *12. PROCEEDS
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *13. LOAN AMOUNT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *14. FINANCE CHARGE
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *15. INTEREST CHARGE
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *16. REGULAR PAYMENT AMOUNT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *17. ODD PAYMENT AMOUNT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *18. PRO CASH
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *19. STATE WORKER
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *20. PR BALANCE
           03  FILLER      PIC X(8) VALUE "E NS062 ".
      *21. SER CHARGE
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *22. EXT CHARGE
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *23. PRINT DUE
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *24. PFINCHGRB
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *25. FEE CODE (1)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *26. FEE AMOUNT (1)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *27. FEE CODE (2)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *28. FEE AMOUNT (2)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *29. FEE CODE (3)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *30. FEE AMOUNT (3)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *31. FEE CODE (4)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *32. FEE AMOUNT (4)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *33. FEE CODE (5)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *34. FEE AMOUNT (5)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *35. FEE CODE (6)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *36. FEE AMOUNT (6)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *37. FEE CODE (7)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *38. FEE AMOUNT (7)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *39. FEE CODE (8)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *40. FEE AMOUNT (8)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *41. FEE CODE (9)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *42. FEE AMOUNT (9)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *43. FEE CODE (10)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *44. FEE AMOUNT (10)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 44 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

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
           03  D-LAWCODE   PIC 99.
           03  D-LINE      PIC 99.
       01  KEY-LIST.
           03  FILLER      PIC X(39)   VALUE
               "BRANCH MMYY DAY BRNO CODE CLASS NUMBER ".
           03  FILLER      PIC X(13)   VALUE
               "LAWCODE LINE.".

      ******************************************************************
       COPY "LIBWI/TRLSCNW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TRLMN1_DEF.CPY".
       COPY "LIBMN/TRLMN1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/TRLMN1_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N    P R O G R A M
      *
      ******************************************************************
        MAIN-PROGRAM SECTION.
      D    STOP MESS.

        COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TRLMN1" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO TRLMIN-LINK.
           MOVE SAVEKEY-MM TO SAVE-MM.
           MOVE SAVEKEY-YY TO SAVE-YY.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           IF SAVEKEY NOT = " "
              MOVE "ON" TO ACTIVE-FILE-SW.

           MOVE "OFF" TO LEAVE-BRANCH-MMYY-SW.

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
                  MOVE LPWSTRL-TRL1-KEY TO SAVEKEY
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

           MOVE SAVEKEY  TO LPWSTRL-TRL1-KEY TRL1-KEY.
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

           PERFORM OPEN-TRL1-FILE.

       EXIT-BRANCH-MMYY-MODULE.
           EXIT.

      ******************************************************************
       ENTER-KEYS-MODULE SECTION.
      ******************************************************************
       ENTER-DAY.
           MOVE "F6 - SCAN"   TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPACES        TO IO-TYPE.

           MOVE 0 TO LPWSTRL-TL-DATE LPWSTRL-TL-BRNO
                     LPWSTRL-TL-CLASS LPWSTRL-TL-ACCTNO
                     LPWSTRL-TL-LAWCODE LPWSTRL-TL-LINE.
           MOVE 0 TO NEW-FLAG.

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
              MOVE SAVE-BRNO   TO TRLSCN-BRNO
              MOVE SAVE-MMYY   TO TRLSCNW-MMYY
              MOVE "Y"         TO TRLSCN-RETURN
              MOVE "WI/TRLSCN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH TRLSCN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF TRLSCN-STATUS NOT = "00"
                 GO TO ENTER-DAY
              ELSE
                 MOVE TRLSCN-TRL1-KEY TO LPWSTRL-TRL1-KEY
                 PERFORM DISPLAY-KEY
                 GO TO READ-TRANS-FILE.

           IF VDUSTATUS = "1>"
              MOVE "X"   TO ERRCD
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DAY.

           MOVE VDUNUM0 TO DATE-MMDDYY-DD.
           MOVE SAVE-MM TO DATE-MMDDYY-MM.
           MOVE SAVE-YY TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.

           MOVE DATE-YYYYMMDD TO LPWSTRL-TL-DATE.

      *-----------------------------------------------------------------
       ENTER-BRNO.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRNO." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO LPWSTRL-TL-BRNO
                                      TL-BRNO.
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

      ***********************************************************
      *    DO *NOT* SET FILPATH-WORKERS FROM THIS READ-BR-FILE.
      *    THIS BRNO MAYBE DIFFERENT FROM THE ONE USED FOR THE
      *    PREVIOUS READ-BR-FILE USED TO GET TRANS-PATH.
      ***********************************************************
      *    MOVE VDUNUM0 TO BR-NO LPWSTRL-TL-BRNO TL-BRNO.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           MOVE "S  A200BRDESC." TO VDU.
           MOVE BR-NAME          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S NA010CODE." TO VDU.
           MOVE "L"            TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-CLASS.
           MOVE "EN N020CLASS." TO VDU.
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

           MOVE VDUNUM0 TO LPWSTRL-TL-CLASS.

      *-----------------------------------------------------------------
       ENTER-NUMBER.
           MOVE "EN N060NUMBER." TO VDU.
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

           MOVE VDUNUM0 TO LPWSTRL-TL-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-LAWCODE.
           MOVE "EN N020LAWCODE." TO VDU.
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
              GO TO ENTER-LAWCODE.

           MOVE VDUNUM0 TO LPWSTRL-TL-LAWCODE.

      *-----------------------------------------------------------------
       ENTER-LINE.
           MOVE "EN N020LINE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-LAWCODE.

           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-LINE.

           MOVE VDUNUM0 TO LPWSTRL-TL-LINE.

      *-----------------------------------------------------------------
       READ-TRANS-FILE.
           PERFORM OPEN-TRL1-FILE.
           MOVE LPWSTRL-TRL1-KEY TO TRL1-KEY.
           PERFORM READ-TRL1-FILE.
           IF IO-FG = 0
              MOVE TRL-REC TO ORIGINAL-TRL-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-ENTER-KEYS-MODULE.
           EXIT.

      *****************************************************
      *    E N T R Y    M O D U L E
      *
      *         NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                    1 - INITIAL NEW RECORD ENTRY
      *                    2 - MODIFY NEW RECORD
      *****************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1         TO ONE.
           MOVE 0         TO ELE.

       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE = 10 )
              GO TO CREATE-ROUTINE.

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
              IF FORM-SEL1 < 2 OR = 1 OR > 4
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

           IF (CHNO = 10)
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

           MOVE SPEC (ELE) TO VDU.
           MOVE ELE        TO VDUROW.
           MOVE 0          TO VDUCOL.
           MOVE "."        TO VDUDECIMAL.
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
      *           SET RECORD ELEMENTS
      *****************************************************************
       F-01.
           MOVE VDUBUF TO TL-PURCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO TL-SRCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO TL-COLLCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO TL-SERVACCT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO TL-LOANTYPE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUNUM0 TO TL-NOREGPY.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUNUM0 TO TL-TILLNO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO TL-GIFTNET.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUBUF TO TL-MIP-FG.
           IF NOT (TL-MIP-FG = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUBUF TO TL-LPEOD5-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDUNUM2 TO TL-PROCEEDS.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           MOVE VDUNUM2 TO TL-LNAMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUNUM2 TO TL-FINCHG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUNUM2 TO TL-INTCHG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUNUM2 TO TL-REGPYAMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUNUM2 TO TL-1STPYAMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           MOVE VDUNUM2 TO TL-PROCASH.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
           MOVE VDUNUM2 TO TL-STATE-WORKER.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           MOVE VDUNUM2 TO TL-PRBAL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           MOVE VDUNUM2 TO TL-SERCHG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           MOVE VDUNUM2 TO TL-EXTCHG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           MOVE VDUNUM2 TO TL-PRINTDUE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           MOVE VDUNUM2 TO TL-PRFINCHGRB.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           MOVE VDUBUF TO TL-FEECODE(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           MOVE VDUNUM2 TO TL-FEEAMT(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-27.
           MOVE VDUBUF TO TL-FEECODE(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-28.
           MOVE VDUNUM2 TO TL-FEEAMT(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-29.
           MOVE VDUBUF TO TL-FEECODE(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-30.
           MOVE VDUNUM2 TO TL-FEEAMT(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-31.
           MOVE VDUBUF TO TL-FEECODE(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-32.
           MOVE VDUNUM2 TO TL-FEEAMT(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-33.
           MOVE VDUBUF TO TL-FEECODE(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-34.
           MOVE VDUNUM2 TO TL-FEEAMT(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-35.
           MOVE VDUBUF TO TL-FEECODE(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-36.
           MOVE VDUNUM2 TO TL-FEEAMT(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-37.
           MOVE VDUBUF TO TL-FEECODE(7).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-38.
           MOVE VDUNUM2 TO TL-FEEAMT(7).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-39.
           MOVE VDUBUF TO TL-FEECODE(8).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-40.
           MOVE VDUNUM2 TO TL-FEEAMT(8).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-41.
           MOVE VDUBUF TO TL-FEECODE(9).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-42.
           MOVE VDUNUM2 TO TL-FEEAMT(9).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-43.
           MOVE VDUBUF TO TL-FEECODE(10).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-44.
           MOVE VDUNUM2 TO TL-FEEAMT(10).
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TRL1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE TRL-REC TO CHANGED-TRL-REC.

           IF ORIGINAL-TRL-REC = CHANGED-TRL-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-TRL1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF TRL-REC NOT = ORIGINAL-TRL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-TRL-REC TO TRL-REC.
           PERFORM REWRITE-TRL1-FILE.

           MOVE "C"                        TO ERRLOGW-ACTION.
           MOVE TRL1-KEY                    TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME              TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

      *-----------------------------------------------------------------
       WRITE-RECORD.
           PERFORM WRITE-TRL1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.

       END-WRITE-MODULE.
           PERFORM CLOSE-TRL1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-TRL1-FILE.
           PERFORM READ-TRL1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF TRL-REC NOT = ORIGINAL-TRL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-TRL1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-TRL1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1             TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-TRL-FILE.
           MOVE LPWSTRL-TRL1-KEY TO TRL1-KEY.
           MOVE 0 TO ELE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES          TO DISPLAY-BUF.
           MOVE TL-PURCD        TO F01.
           MOVE TL-SRCD         TO F02.
           MOVE TL-COLLCD       TO F03.
           MOVE TL-SERVACCT     TO F04.
           MOVE TL-LOANTYPE     TO F05.
           MOVE TL-NOREGPY      TO F06.
           MOVE TL-TILLNO       TO F07.
           MOVE TL-GIFTNET      TO F08.
           MOVE TL-MIP-FG       TO F09.
      *--->MOVE TL-INSELIGIBLE  TO F10.

           IF TL-LPEOD5-FG <= " "
              MOVE " " TO F11
           ELSE
              MOVE TL-LPEOD5-FG TO F11.

           MOVE TL-PROCEEDS     TO F12.
           MOVE TL-LNAMT        TO F13.
           MOVE TL-FINCHG       TO F14.
           MOVE TL-INTCHG       TO F15.
           MOVE TL-REGPYAMT     TO F16.
           MOVE TL-1STPYAMT     TO F17.
           MOVE TL-PROCASH      TO F18.
           MOVE TL-STATE-WORKER TO F19.
           MOVE TL-PRBAL        TO F20.
           MOVE TL-SERCHG       TO F21.
           MOVE TL-EXTCHG       TO F22.
           MOVE TL-PRINTDUE     TO F23.
           MOVE TL-PRFINCHGRB   TO F24.
           MOVE TL-FEECODE(1)   TO F25.
           MOVE TL-FEEAMT(1)    TO F26.
           MOVE TL-FEECODE(2)   TO F27.
           MOVE TL-FEEAMT(2)    TO F28.
           MOVE TL-FEECODE(3)   TO F29.
           MOVE TL-FEEAMT(3)    TO F30.
           MOVE TL-FEECODE(4)   TO F31.
           MOVE TL-FEEAMT(4)    TO F32.
           MOVE TL-FEECODE(5)   TO F33.
           MOVE TL-FEEAMT(5)    TO F34.
           MOVE TL-FEECODE(6)   TO F35.
           MOVE TL-FEEAMT(6)    TO F36.
           MOVE TL-FEECODE(7)   TO F37.
           MOVE TL-FEEAMT(7)    TO F38.
           MOVE TL-FEECODE(8)   TO F39.
           MOVE TL-FEEAMT(8)    TO F40.
           MOVE TL-FEECODE(9)   TO F41.
           MOVE TL-FEEAMT(9)    TO F42.
           MOVE TL-FEECODE(10)  TO F43.
           MOVE TL-FEEAMT(10)   TO F44.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO DISPLAY-FIELDS-EXIT.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************

           MOVE SAVE-BRNO        TO D-BRANCH.
           MOVE LPWSTRL-TL-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO D-MM.
           MOVE DATE-YYYYMMDD-YY TO D-YY.


           MOVE DATE-YYYYMMDD-DD     TO D-DAY.
           MOVE LPWSTRL-TL-BRNO      TO D-BRNO.
           MOVE "L"                  TO D-CODE.
           MOVE LPWSTRL-TL-CLASS     TO D-CLASS.
           MOVE LPWSTRL-TL-ACCTNO    TO D-ACCTNO.
           MOVE LPWSTRL-TL-LAWCODE   TO D-LAWCODE.
           MOVE LPWSTRL-TL-LINE      TO D-LINE.

           MOVE KEY-BUF  TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************************
       FORM-RESET SECTION.
      **************************************************************
           MOVE EXT-CONAME-CONAME TO TRLMN1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRLMN1-SYSDATE.
           DISPLAY SPACES UPON TRLMIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRLMN1-SCREEN UPON TRLMIN-WINDOW-HANDLE.
           MOVE TRLMN1-FORM-FIELDS TO WR-BUF.
           MOVE TRLMN1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************
           MOVE TRLMN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRLMN1-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRLMN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       DISPLAY-SCAN-KEY SECTION.
      ******************************************************************

           MOVE "SNNN040BRANCH." TO VDU
           MOVE BR-NO TO VDUNUM0   SAVE-BRNO.
           PERFORM SCREENIO.

           MOVE LPWSTRL-TL-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO DATE-MMYY-MM.
           MOVE DATE-YYYYMMDD-YY TO DATE-MMYY-YY.
           MOVE "SNNN040MMYY." TO VDU.
           MOVE DATE-MMYY      TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020DAY."    TO VDU.
           MOVE DATE-YYYYMMDD-DD TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BRNO." TO VDU.
           MOVE LPWSTRL-TL-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010CODE." TO VDU.
           MOVE "L"            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N020CLASS." TO VDU.
           MOVE LPWSTRL-TL-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N060NUMBER." TO VDU.
           MOVE LPWSTRL-TL-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020LAWCODE." TO VDU.
           MOVE LPWSTRL-TL-LAWCODE TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020LINE." TO VDU.
           MOVE LPWSTRL-TL-LINE TO VDUNUM0.
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
     
           MOVE "B"                        TO TRL-PATH-OVERRIDE.
     
       LOAD-BRANCH-MODULE-EXIT.
           EXIT.
     
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTRLCL.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.


      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPTRL1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           MOVE BR-NO       TO SAVEBRNO.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TRLMN1-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE TRLMIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
