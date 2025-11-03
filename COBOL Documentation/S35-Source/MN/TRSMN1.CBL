       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRSMN1.
       DATE-WRITTEN.    11-17-94.
      ******************************************************************
      *   TRANSACTION FILE MAINTENANCE (SALES FINANCE)
      *              PAGE 1
      *
      *=================================================================
      * REV:
      * MJD 050995 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      * MJD 063095 ADDED LOGIC FOR PAGE4.
      * JFF 100495 ADDED SPRCLASS AND O3INS-DLPROC FIELDS.
      * MJD 042696 ADDED GP-FILE, GP-DLR-DISP-TYPE.
      * GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * MJD 131203 A30 CONVERSION COMPLETED
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *         ADDED LIBLP/LPFRTR; REMOVED LIBLP/LPTRTMPW.
      *         ADDED LPFRTR-UPDATE-CHGLOG; REMOVED WRITE-TR-CHANGE-LOG.
      *
      * BAH 2019.0508 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      * BLM 2021.1116 SAVE-MMYY ALWAYS SPACES, CAUSES CAST ERRORS,
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
           "@(#)./MN/TRSMN1.CBL S34 11/16/21-14:23:53 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".

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

       01  CHANGED-TRS-REC     PIC X(TRS-SIZE).
       01  ORIGINAL-TRS-REC    PIC X(TRS-SIZE).

       01  DLR-TYP-LIST      PIC X(6)   VALUE "23P:P.".
       01  DLR-TYP-BUF       PIC X(4).

       01  TRSMIN-LINK VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(28).
           03  FILLER REDEFINES SAVEKEY.
               05  FILLER     PIC X(6).
               05  SAVEKEY-YY PIC 9(2).
               05  SAVEKEY-MM PIC 9(2).
               05  FILLER     PIC X(18).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  FILLER REDEFINES FORMSEL.
               05  FILLER    PIC 9.
               05  FORMNUM   PIC 9.
           03  SAVEBRNO      PIC 9(4).
           03  TRSMIN-WINDOW-HANDLE PIC X(10).

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
           03  F07         PIC X.
           03  F08         PIC X.
           03  F09         PIC X.
           03  F10         PIC X.
           03  F11         PIC X.
           03  F12         PIC X.
           03  F13         PIC 99              BLANK WHEN ZERO.
           03  F14         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F15         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F16         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F17         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F18         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F19         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F20         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F21         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F22         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F23         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F24         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F25         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F26         PIC ZZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F27         PIC XX.
           03  F28         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F29         PIC X.
           03  F30         PIC XX.
           03  F31         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F32         PIC X.
           03  F33         PIC XX.
           03  F34         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F35         PIC X.
           03  F36         PIC XX.
           03  F37         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F38         PIC X.
           03  F39         PIC XX.
           03  F40         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F41         PIC X.
           03  F42         PIC XX.
           03  F43         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F44         PIC X.
           03  F45         PIC XX.
           03  F46         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F47         PIC X.
           03  F48         PIC XX.
           03  F49         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F50         PIC X.
           03  F51         PIC XX.
           03  F52         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F53         PIC X.
           03  F54         PIC XX.
           03  F55         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F56         PIC X.
           03  F57         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
           "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(40) VALUE
           "210 220 230 240 250 260 270 280 290 300 ".
           03  FILLER      PIC X(40) VALUE
           "310 320 330 340 350 360 370 380 390 400 ".
           03  FILLER      PIC X(40) VALUE
           "410 420 430 440 450 460 470 480 490 500 ".
           03  FILLER      PIC X(28) VALUE
           "510 520 530 540 550 560 570.".

       01  MAX             PIC 99   VALUE 57.

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
      * 7. DL PARVHELD CODE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 8. DL PARVPAID CODE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 9. DL RECOURSE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *10. DL RESERVE CHARGE CODE #1
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *11. DL RESERVE CHARGE CODE #2
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *12. DL RESERVE CHARGE CODE #3
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *13. DL SPRCLASS
           03  FILLER      PIC X(8) VALUE "E NN020 ".
      *14. PROCEEDS
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *15. LOAN AMOUNT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *16. FINANCE CHARGE
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *17. INTEREST CHARGE
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *18. REG PAYMENT AMOUNT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *19. 1ST PAYMENT AMOUNT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *20. DL RESERVE PRINCIPAL #1
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *21. DL RESERVE PRINCIPAL #2
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *22. DL RESERVE PRINCIPAL #3
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *23. DL DISCOUNT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *24. DL PARTRCMAT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *25. PUR BAL
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *26. DL O3INS DLPROC
           03  FILLER      PIC X(8) VALUE "E NS062 ".
      *27. FEE CODE (1)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *28. FEE AMOUNT (1)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *29. FEE OURS (1)
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *30. FEE CODE (2)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *31. FEE AMOUNT (2)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *32. FEE OURS (2)
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *33. FEE CODE (3)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *34. FEE AMOUNT (3)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *35. FEE OURS (3)
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *36. FEE CODE (4)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *37. FEE AMOUNT (4)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *38. FEE OURS (4)
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *39. FEE CODE (5)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *40. FEE AMOUNT (5)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *41. FEE OURS (5)
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *42. FEE CODE (6)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *43. FEE AMOUNT (6)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *44. FEE OURS (6)
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *45. FEE CODE (6)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *46. FEE AMOUNT (6)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *47. FEE OURS (6)
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *48. FEE CODE (6)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *49. FEE AMOUNT (6)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *50. FEE OURS (6)
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *51. FEE CODE (6)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *52. FEE AMOUNT (6)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *53. FEE OURS (6)
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *54. FEE CODE (6)
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *55. FEE AMOUNT (6)
           03  FILLER      PIC X(8) VALUE "E NS052 ".
      *56. FEE OURS (6)
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *57. LAST PAYMENT AMOUNT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 57 TIMES.
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
       COPY "LIBWI/TRSSCNW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TRSMN1_DEF.CPY".
       COPY "LIBMN/TRSMN1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/TRSMN1_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N    P R O G R A M
      *
      ******************************************************************
        MAIN-PROGRAM SECTION.

      D     STOP MESS.

        COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TRSMN1" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO TRSMIN-LINK.
           MOVE SAVEKEY-MM TO SAVE-MM.
           MOVE SAVEKEY-YY TO SAVE-YY.


           PERFORM GET-GPENV.

            MOVE " " TO VDU-WIMENU-SELECTION
                        VDU-PGMENU-SELECTION.

            IF SAVEKEY NOT = " "
               MOVE "ON" TO ACTIVE-FILE-SW.

            MOVE "OFF" TO LEAVE-BRANCH-MMYY-SW.

      *-----------------------------------------------------------------
        MAIN-MODULE.
           PERFORM FORM-RESET.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE "COMM" TO DLR-TYP-BUF
           ELSE
              MOVE "DISC" TO DLR-TYP-BUF.
           MOVE DLR-TYP-BUF  TO WR-BUF.
           MOVE DLR-TYP-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

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
                  MOVE LPWSTRS-TRS1-KEY TO SAVEKEY
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

           MOVE SAVEKEY  TO LPWSTRS-TRS1-KEY TRS1-KEY.
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

           PERFORM OPEN-TRS1-FILE.

       EXIT-BRANCH-MMYY-MODULE.
           EXIT.

      ******************************************************************
       ENTER-KEYS-MODULE SECTION.
      ******************************************************************
       ENTER-DAY.
           MOVE "F6 - SCAN"   TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPACES        TO IO-TYPE.

           MOVE 0 TO LPWSTRS-TS-DATE LPWSTRS-TS-BRNO
                     LPWSTRS-TS-CLASS LPWSTRS-TS-ACCTNO
                     LPWSTRS-TS-LAWCODE LPWSTRS-TS-LINE.
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
              MOVE SAVE-BRNO   TO TRSSCN-BRNO
              MOVE SAVE-MMYY   TO TRSSCNW-MMYY
              MOVE "Y"         TO TRSSCN-RETURN
              MOVE "WI/TRSSCN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH TRSSCN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF TRSSCN-STATUS NOT = "00"
                 GO TO ENTER-DAY
              ELSE
                 MOVE TRSSCN-TRS1-KEY TO LPWSTRS-TRS1-KEY
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

           MOVE DATE-YYYYMMDD TO LPWSTRS-TS-DATE.

      *-----------------------------------------------------------------
       ENTER-BRNO.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRNO." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO LPWSTRS-TS-BRNO
                                      TS-BRNO.
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
      *    MOVE VDUNUM0 TO BR-NO LPWSTRS-TS-BRNO TS-BRNO.

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
           MOVE "S"            TO VDUBUF.
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

           MOVE VDUNUM0 TO LPWSTRS-TS-CLASS.

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

           MOVE VDUNUM0 TO LPWSTRS-TS-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-LAWCODE.
           MOVE "ENNN020LAWCODE." TO VDU.
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

           MOVE VDUNUM0 TO LPWSTRS-TS-LAWCODE.

      *-----------------------------------------------------------------
       ENTER-LINE.
           MOVE "ENNN020LINE." TO VDU.
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

           MOVE VDUNUM0 TO LPWSTRS-TS-LINE.

      *-----------------------------------------------------------------
       READ-TRANS-FILE.
           PERFORM OPEN-TRS1-FILE.
           MOVE LPWSTRS-TRS1-KEY TO TRS1-KEY.
           PERFORM READ-TRS1-FILE.
           IF IO-FG = 0
              MOVE TRS-REC TO ORIGINAL-TRS-REC
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
              IF FORM-SEL1 < 0 OR = 1 OR > 4
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
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 DEPENDING ON ELE.
           SUBTRACT 20 FROM ELE GIVING OVER-20-ELE.
           GO TO F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34 F-35 F-36 F-37 F-38 F-39 F-40
                 DEPENDING ON OVER-20-ELE.
           SUBTRACT 20 FROM OVER-20-ELE GIVING OVER-40-ELE.
           GO TO F-41 F-42 F-43 F-44 F-45 F-46 F-47 F-48 F-49 F-50
                 F-51 F-52 F-53 F-54 F-55 F-56 F-57
                 DEPENDING ON OVER-40-ELE.
           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *           SET RECORD ELEMENTS
      *****************************************************************
       F-01.
           MOVE VDUBUF TO TS-PURCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO TS-SRCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO TS-COLLCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO TS-SERVACCT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO TS-LOANTYPE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUNUM0 TO TS-NOREGPY.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUNUM0 TO TS-DL-PARVHELD-CODE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUNUM0 TO TS-DL-PARVPAID-CODE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUBUF TO TS-DLRECOURSE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUBUF TO TS-DL-RVCHGS-CODE(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUBUF TO TS-DL-RVCHGS-CODE(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDUBUF TO TS-DL-RVCHGS-CODE(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           MOVE VDUNUM0 TO TS-SPRCLASS.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUNUM2 TO TS-PROCEEDS.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUNUM2 TO TS-LNAMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUNUM2 TO TS-FINCHG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUNUM2 TO TS-INTCHG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           MOVE VDUNUM2 TO TS-REGPYAMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
           MOVE VDUNUM2 TO TS-1STPYAMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           MOVE VDUNUM2 TO TS-DLRVPRIN(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           MOVE VDUNUM2 TO TS-DLRVPRIN(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           MOVE VDUNUM2 TO TS-DLRVPRIN(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           MOVE VDUNUM2 TO TS-DLDISC.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           MOVE VDUNUM2 TO TS-DLPARTRCAMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           MOVE VDUNUM2 TO TS-PURBAL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           MOVE VDUNUM2 TO TS-O3INS-DLPROC.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-27.
           MOVE VDUBUF TO TS-FEECODE(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-28.
           MOVE VDUNUM2 TO TS-FEEAMT(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-29.
           MOVE VDUBUF TO TS-FEEOURS(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-30.
           MOVE VDUBUF TO TS-FEECODE(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-31.
           MOVE VDUNUM2 TO TS-FEEAMT(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-32.
           MOVE VDUBUF TO TS-FEEOURS(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-33.
           MOVE VDUBUF TO TS-FEECODE(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-34.
           MOVE VDUNUM2 TO TS-FEEAMT(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-35.
           MOVE VDUBUF TO TS-FEEOURS(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-36.
           MOVE VDUBUF TO TS-FEECODE(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-37.
           MOVE VDUNUM2 TO TS-FEEAMT(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-38.
           MOVE VDUBUF TO TS-FEEOURS(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-39.
           MOVE VDUBUF TO TS-FEECODE(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-40.
           MOVE VDUNUM2 TO TS-FEEAMT(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-41.
           MOVE VDUBUF TO TS-FEEOURS(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-42.
           MOVE VDUBUF TO TS-FEECODE(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-43.
           MOVE VDUNUM2 TO TS-FEEAMT(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-44.
           MOVE VDUBUF TO TS-FEEOURS(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-45.
           MOVE VDUBUF TO TS-FEECODE(7).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-46.
           MOVE VDUNUM2 TO TS-FEEAMT(7).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-47.
           MOVE VDUBUF TO TS-FEEOURS(7).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-48.
           MOVE VDUBUF TO TS-FEECODE(8).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-49.
           MOVE VDUNUM2 TO TS-FEEAMT(8).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-50.
           MOVE VDUBUF TO TS-FEEOURS(8).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-51.
           MOVE VDUBUF TO TS-FEECODE(9).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-52.
           MOVE VDUNUM2 TO TS-FEEAMT(9).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-53.
           MOVE VDUBUF TO TS-FEEOURS(9).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-54.
           MOVE VDUBUF TO TS-FEECODE(10).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-55.
           MOVE VDUNUM2 TO TS-FEEAMT(10).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-56.
           MOVE VDUBUF TO TS-FEEOURS(10).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-57.
           MOVE VDUNUM2 TO TS-LASTPYAMT.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TRS1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE TRS-REC TO CHANGED-TRS-REC.

           IF ORIGINAL-TRS-REC = CHANGED-TRS-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-TRS1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF TRS-REC NOT = ORIGINAL-TRS-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-TRS-REC TO TRS-REC.
           PERFORM REWRITE-TRS1-FILE.

           MOVE "C"                        TO ERRLOGW-ACTION.
           MOVE TRS1-KEY                   TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME              TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

      *-----------------------------------------------------------------
       WRITE-RECORD.
           PERFORM WRITE-TRS1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.

       END-WRITE-MODULE.
           PERFORM CLOSE-TRS1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-TRS1-FILE.
           PERFORM READ-TRS1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF TRS-REC NOT = ORIGINAL-TRS-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-TRS1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-TRS1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1             TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-TRS-FILE.
           MOVE LPWSTRS-TRS1-KEY TO TRS1-KEY.
           MOVE 0 TO ELE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES               TO DISPLAY-BUF.

           MOVE TS-PURCD             TO F01.
           MOVE TS-SRCD              TO F02.
           MOVE TS-COLLCD            TO F03.
           MOVE TS-SERVACCT          TO F04.
           MOVE TS-LOANTYPE          TO F05.
           MOVE TS-NOREGPY           TO F06.
           MOVE TS-DL-PARVHELD-CODE  TO F07.
           MOVE TS-DL-PARVPAID-CODE  TO F08.
           MOVE TS-DLRECOURSE        TO F09.
           MOVE TS-DL-RVCHGS-CODE(1) TO F10.
           MOVE TS-DL-RVCHGS-CODE(2) TO F11.
           MOVE TS-DL-RVCHGS-CODE(3) TO F12.
           MOVE TS-SPRCLASS          TO F13.
           MOVE TS-PROCEEDS          TO F14.
           MOVE TS-LNAMT             TO F15.
           MOVE TS-FINCHG            TO F16.
           MOVE TS-INTCHG            TO F17.
           MOVE TS-REGPYAMT          TO F18.
           MOVE TS-1STPYAMT          TO F19.
           MOVE TS-DLRVPRIN(1)       TO F20.
           MOVE TS-DLRVPRIN(2)       TO F21.
           MOVE TS-DLRVPRIN(3)       TO F22.
           MOVE TS-DLDISC            TO F23.
           MOVE TS-DLPARTRCAMT       TO F24.
           MOVE TS-PURBAL            TO F25.
           MOVE TS-O3INS-DLPROC      TO F26.
           MOVE TS-FEECODE(1)        TO F27.
           MOVE TS-FEEAMT(1)         TO F28.
           MOVE TS-FEEOURS(1)        TO F29.
           MOVE TS-FEECODE(2)        TO F30.
           MOVE TS-FEEAMT(2)         TO F31.
           MOVE TS-FEEOURS(2)        TO F32.
           MOVE TS-FEECODE(3)        TO F33.
           MOVE TS-FEEAMT(3)         TO F34.
           MOVE TS-FEEOURS(3)        TO F35.
           MOVE TS-FEECODE(4)        TO F36.
           MOVE TS-FEEAMT(4)         TO F37.
           MOVE TS-FEEOURS(4)        TO F38.
           MOVE TS-FEECODE(5)        TO F39.
           MOVE TS-FEEAMT(5)         TO F40.
           MOVE TS-FEEOURS(5)        TO F41.
           MOVE TS-FEECODE(6)        TO F42.
           MOVE TS-FEEAMT(6)         TO F43.
           MOVE TS-FEEOURS(6)        TO F44.
           MOVE TS-FEECODE(7)        TO F45.
           MOVE TS-FEEAMT(7)         TO F46.
           MOVE TS-FEEOURS(7)        TO F47.
           MOVE TS-FEECODE(8)        TO F48.
           MOVE TS-FEEAMT(8)         TO F49.
           MOVE TS-FEEOURS(8)        TO F50.
           MOVE TS-FEECODE(9)        TO F51.
           MOVE TS-FEEAMT(9)         TO F52.
           MOVE TS-FEEOURS(9)        TO F53.
           MOVE TS-FEECODE(10)       TO F54.
           MOVE TS-FEEAMT(10)        TO F55.
           MOVE TS-FEEOURS(10)       TO F56.
           MOVE TS-LASTPYAMT         TO F57.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO DISPLAY-FIELDS-EXIT.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************

           MOVE SAVE-BRNO       TO D-BRANCH.

           MOVE LPWSTRS-TS-DATE  TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO D-MM.
           MOVE DATE-YYYYMMDD-YY TO D-YY.

           MOVE DATE-YYYYMMDD-DD     TO D-DAY.
           MOVE LPWSTRS-TS-BRNO      TO D-BRNO.
           MOVE "S"                  TO D-CODE.
           MOVE LPWSTRS-TS-CLASS     TO D-CLASS.
           MOVE LPWSTRS-TS-ACCTNO    TO D-ACCTNO.
           MOVE LPWSTRS-TS-LAWCODE   TO D-LAWCODE.
           MOVE LPWSTRS-TS-LINE      TO D-LINE.

           MOVE KEY-BUF  TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************************************************
       FORM-RESET SECTION.
      ***************************************************************
           MOVE EXT-CONAME-CONAME TO TRSMN1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRSMN1-SYSDATE.
           DISPLAY SPACES UPON TRSMIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRSMN1-SCREEN UPON TRSMIN-WINDOW-HANDLE.
           MOVE TRSMN1-FORM-FIELDS TO WR-BUF.
           MOVE TRSMN1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***************************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************************
           MOVE TRSMN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRSMN1-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRSMN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       DISPLAY-SCAN-KEY SECTION.
      ******************************************************************

           MOVE "SNNN040BRANCH." TO VDU
           MOVE BR-NO TO VDUNUM0 SAVE-BRNO.
           PERFORM SCREENIO.

           MOVE LPWSTRS-TS-DATE  TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO DATE-MMYY-MM.
           MOVE DATE-YYYYMMDD-YY TO DATE-MMYY-YY.
           MOVE "SNNN040MMYY."   TO VDU.
           MOVE DATE-MMYY        TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020DAY."    TO VDU.
           MOVE DATE-YYYYMMDD-DD TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BRNO." TO VDU.
           MOVE LPWSTRS-TS-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NA010CODE." TO VDU.
           MOVE "S"            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN020CLASS." TO VDU.
           MOVE LPWSTRS-TS-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060NUMBER."  TO VDU.
           MOVE LPWSTRS-TS-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LAWCODE." TO VDU.
           MOVE LPWSTRS-TS-LAWCODE TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LINE." TO VDU.
           MOVE LPWSTRS-TS-LINE TO VDUNUM0.
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
     
           MOVE "B"                        TO TRS-PATH-OVERRIDE.
     
       LOAD-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTRSCL.CPY".

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
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPTRS1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           MOVE BR-NO TO SAVEBRNO.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TRSMN1-SCREEN.
           MOVE FORM-PATH   TO FORM-PATHNAME.
           MOVE TRSMIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
