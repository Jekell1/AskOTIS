       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRXMN1.
       DATE-WRITTEN.    11-17-94.
      ******************************************************************
      *   TRANSACTION FILE MAINTENANCE (CLASS TRANSFER)
      *              PAGE 1
      *
      * REV:
      *   MJD 050995 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *   JFF 100495 ADDED SPRCLASS FIELD
      *   MJD 042696 ADDED GP-FILE, GP-DLR-DISP-TYPE.
      *   GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *   GLF 102797 CHANGED FIELD 23 FROM Z,ZZZ,ZZ9.99 TO ZZZ,ZZ9.99
      *   MJD 131203 A30 CONVERSION COMPLETED
      *    CS 161006 CHANGED HOLD-KEY TO HOLD-TRX-KEY, HOLD-FIELDS TO HOLD-TX
      *            CHANGED HARD CODE SIZE OF ORIGINAL-TR-REC/CHANGED-TRX-REC
      *            TO TR-SIZE, REMOVED TK-SEQNO
      *            SAVEKEY FROM X(14) TO X(27)
      *            REMOVE D-SEQNO FROM KEY-BUF
      *
      * KEC 2016-1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *         ADDED LIBLP/LPFRTR; REMOVED LIBLP/LPTRTMPW.
      *         ADDED LPFRTR-UPDATE-CHGLOG; REMOVED WRITE-TR-CHANGE-LOG.
      *
      * BAH 2019-0508 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      * JKC 2020-0616 ADDED TSTDAT CHECK ON LPWSTRX-TX-DATE.
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
      *
      *    W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/TRXMN1.CBL S34 11/16/21-14:24:55 >".

       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LP01TRX_SQL.CPY".
       COPY "LIBLP/LPWSTRX.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ONE             PIC S9.

       01  ELE             PIC 9(2).
           88  INVALID-ELE VALUE 7 8 9 10 11 12 13.

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

       01  ORIGINAL-TRX-REC    PIC X(TRX-SIZE).
       01  CHANGED-TRX-REC     PIC X(TRX-SIZE).

       01  DLR-TYP-LIST      PIC X(6)   VALUE "21P:P.".
       01  DLR-TYP-BUF       PIC X(4).

       01  TRXMIN-LINK VALUE SPACES.
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
           03  TRXMIN-WINDOW-HANDLE PIC X(10).


      ******************************************************************
      *
      *    SCREEN DISPLAY BUFFER
      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC XXX.
           03  F02         PIC XX.
           03  F03         PIC XX.
           03  F04         PIC X.
           03  F05         PIC X.
           03  F06         PIC X.
           03  F07         PIC X.
           03  F08         PIC X.
           03  F09         PIC X.
           03  F10         PIC X.
           03  F11         PIC X.
           03  F12         PIC X.
           03  F13         PIC X.
           03  F14         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F15         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F16         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F17         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F18         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F19         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F20         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F21         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F22         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F23         PIC ZZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  F24         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F25         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F26         PIC 99              BLANK WHEN ZERO.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
           "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(40) VALUE
           "210 220 230 240 250 260.".

       01  MAX             PIC 99   VALUE 26.

      ******************************************************************
      *
      *    ELEMENT SPECIFICATIONS
      *
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
      * 6. ************* NOT IN USE ***************
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 7. ************* NOT IN USE ***************
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 8. ************* NOT IN USE ***************
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 9. ************* NOT IN USE ***************
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *10. ************* NOT IN USE ***************
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *11. ************* NOT IN USE ***************
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *12. ************* NOT IN USE ***************
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *13. ************* NOT IN USE ***************
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *14. PROCEEDS
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *15. LOAN AMOUNT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *16. FINANCE CHARGE
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *17. INTEREST CHARGE
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *18. DL RESERVE PRINCIPAL #1
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *19. DL RESERVE PRINCIPAL #2
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *20. DL RESERVE PRINCIPAL #1
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *21. DL RESERVE PRINCIPAL #3
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *22. DL DISCOUNT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *23. PUR BAL
           03  FILLER      PIC X(8) VALUE "E NS062 ".
      *24. DL PARTRCMAT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *25. TFRBAL
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *26. SPRCLASS
           03  FILLER      PIC X(8) VALUE "E NN020 ".
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 26 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      ******************************************************************
      *
      *    K E Y - B U F
      *
      ******************************************************************
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
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/TRXSCNW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TRXMN1_DEF.CPY".
       COPY "LIBMN/TRXMN1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/TRXMN1_SCN.CPY".

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
           MOVE "TRXMN1" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO TRXMIN-LINK.
           MOVE SAVEKEY-MM TO SAVE-MM.
           MOVE SAVEKEY-YY TO SAVE-YY.

           PERFORM GET-GPENV.

            MOVE " " TO VDU-WIMENU-SELECTION
                        VDU-PGMENU-SELECTION.

            IF SAVEKEY NOT = " "
               MOVE "ON" TO ACTIVE-FILE-SW.

            MOVE "OFF" TO LEAVE-BRANCH-MMYY-SW.

        MAIN-MODULE.
           PERFORM FORM-RESET.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE "COMM" TO DLR-TYP-BUF
           ELSE
              MOVE "DISC" TO DLR-TYP-BUF.
           MOVE DLR-TYP-BUF TO WR-BUF.
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
                  MOVE LPWSTRX-TRX1-KEY TO SAVEKEY
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

           MOVE SAVEKEY  TO LPWSTRX-TRX1-KEY TRX1-KEY.
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

           PERFORM OPEN-TRX1-FILE.

       EXIT-BRANCH-MMYY-MODULE.
           EXIT.

      ******************************************************************
       ENTER-KEYS-MODULE SECTION.
      ******************************************************************
       ENTER-DAY.
           MOVE "F6 - SCAN"   TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPACES        TO IO-TYPE.

           MOVE 0 TO LPWSTRX-TX-BRNO LPWSTRX-TX-DATE
                     LPWSTRX-TX-CLASS LPWSTRX-TX-ACCTNO
                     LPWSTRX-TX-LAWCODE LPWSTRX-TX-LINE.
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
              MOVE SAVE-BRNO   TO TRXSCN-BRNO
              MOVE SAVE-MMYY   TO TRXSCNW-MMYY
              MOVE "Y"         TO TRXSCN-RETURN
              MOVE "WI/TRXSCN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH TRXSCN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF TRXSCN-STATUS NOT = "00"
                 GO TO ENTER-DAY
              ELSE
                 MOVE TRXSCN-TRX1-KEY TO LPWSTRX-TRX1-KEY
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

           MOVE DATE-YYYYMMDD TO NUM-DATE.
           PERFORM TSTDAT.

           IF ( NUM-DATE = ZEROES )
              GO TO ENTER-DAY.

           MOVE NUM-DATE TO LPWSTRX-TX-DATE.

      *-----------------------------------------------------------------
       ENTER-BRNO.
      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRNO." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO LPWSTRX-TX-BRNO
                                      TX-BRNO.
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
      *    MOVE VDUNUM0 TO BR-NO LPWSTRX-TX-BRNO TX-BRNO.

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
           MOVE "X"            TO VDUBUF.
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

           MOVE VDUNUM0 TO LPWSTRX-TX-CLASS.

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

           MOVE VDUNUM0 TO LPWSTRX-TX-ACCTNO.

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

           MOVE VDUNUM0 TO LPWSTRX-TX-LAWCODE.

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

           MOVE VDUNUM0 TO LPWSTRX-TX-LINE.

      *-----------------------------------------------------------------
       READ-TRANS-FILE.
           PERFORM OPEN-TRX1-FILE.
           MOVE LPWSTRX-TRX1-KEY TO TRX1-KEY.
           PERFORM READ-TRX1-FILE.
           IF IO-FG = 0
              MOVE TRX-REC TO ORIGINAL-TRX-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-ENTER-KEYS-MODULE.
           EXIT.

      ******************************************************************
      *    E N T R Y    M O D U L E
      *
      *         NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                    1 - INITIAL NEW RECORD ENTRY
      *                    2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1         TO ONE.
           MOVE 0         TO ELE.

      *-----------------------------------------------------------------
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.

           IF (INVALID-ELE)
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
              IF FORM-SEL1 < 0 OR = 1 OR > 3
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

           IF (INVALID-ELE)
              GO TO MODIFY-ROUTINE.

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

           GO TO F-21 F-22 F-23 F-24 F-25 F-26 
                 DEPENDING ON OVER-20-ELE.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
      *
      *           SET RECORD ELEMENTS
      *
      *-----------------------------------------------------------------
       F-01.
           MOVE VDUBUF TO TX-PURCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO TX-SRCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO TX-COLLCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO TX-SERVACCT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO TX-LOANTYPE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO TX-DLRECOURSE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
      *- - - - - UNUSED - - - - - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
      *- - - - - UNUSED - - - - - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
      *- - - - - UNUSED - - - - - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
      *- - - - - UNUSED - - - - - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
      *- - - - - UNUSED - - - - - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
      *- - - - - UNUSED - - - - - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
      *- - - - - UNUSED - - - - - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUNUM2 TO TX-PROCEEDS.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUNUM2 TO TX-LNAMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUNUM2 TO TX-FINCHG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUNUM2 TO TX-INTCHG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           MOVE VDUNUM2 TO TX-DLRVPRIN(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
           MOVE VDUNUM2 TO TX-DLRVPRIN(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           MOVE VDUNUM2 TO TX-DLRVPRIN(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           MOVE VDUNUM2 TO TX-DLDISC.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           MOVE VDUNUM2 TO TX-PURBAL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           MOVE VDUNUM2 TO TX-DLPARTRCAMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           MOVE VDUNUM2 TO TX-TFRBAL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           MOVE VDUNUM2 TO TX-PRBAL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           MOVE VDUNUM0 TO TX-SPRCLASS.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TRX1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE TRX-REC TO CHANGED-TRX-REC.

           IF ORIGINAL-TRX-REC = CHANGED-TRX-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-TRX1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF TRX-REC NOT = ORIGINAL-TRX-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-TRX-REC TO TRX-REC.
           PERFORM REWRITE-TRX1-FILE.

           MOVE "C"                        TO ERRLOGW-ACTION.
           MOVE TRX1-KEY                   TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME              TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.
           GO TO END-WRITE-MODULE.

      *-----------------------------------------------------------------
       WRITE-RECORD.
           PERFORM WRITE-TRX1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.

       END-WRITE-MODULE.
           PERFORM CLOSE-TRX1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-TRX1-FILE.
           PERFORM READ-TRX1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF TRX-REC NOT = ORIGINAL-TRX-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-TRX1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-TRX1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1             TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-TRX-FILE.
           MOVE LPWSTRX-TRX1-KEY TO TRX1-KEY.
           MOVE 0 TO ELE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES               TO DISPLAY-BUF.
           MOVE TX-PURCD             TO F01.
           MOVE TX-SRCD              TO F02.
           MOVE TX-COLLCD            TO F03.
           MOVE TX-SERVACCT          TO F04.
           MOVE TX-LOANTYPE          TO F05.
           MOVE TX-DLRECOURSE        TO F06.

           MOVE TX-PROCEEDS          TO F14.
           MOVE TX-LNAMT             TO F15.
           MOVE TX-FINCHG            TO F16.
           MOVE TX-INTCHG            TO F17.
           MOVE TX-DLRVPRIN(1)       TO F18.
           MOVE TX-DLRVPRIN(2)       TO F19.
           MOVE TX-DLRVPRIN(3)       TO F20.
           MOVE TX-DLDISC            TO F21.
           MOVE TX-PURBAL            TO F22.
           MOVE TX-DLPARTRCAMT       TO F23.
           MOVE TX-TFRBAL            TO F24.
           MOVE TX-PRBAL             TO F25.
           MOVE TX-SPRCLASS          TO F26.

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
           MOVE LPWSTRX-TX-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO D-MM.
           MOVE DATE-YYYYMMDD-YY TO D-YY.

           MOVE DATE-YYYYMMDD-DD      TO D-DAY.
           MOVE LPWSTRX-TX-BRNO       TO D-BRNO.
           MOVE "X"                   TO D-CODE.
           MOVE LPWSTRX-TX-CLASS      TO D-CLASS.
           MOVE LPWSTRX-TX-ACCTNO     TO D-ACCTNO.
           MOVE LPWSTRX-TX-LAWCODE    TO D-LAWCODE.
           MOVE LPWSTRX-TX-LINE       TO D-LINE.

           MOVE KEY-BUF TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************************
       FORM-RESET SECTION.
      ********************************************************
           MOVE EXT-CONAME-CONAME TO TRXMN1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRXMN1-SYSDATE.
           DISPLAY SPACES UPON TRXMIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRXMN1-SCREEN UPON TRXMIN-WINDOW-HANDLE.
           MOVE TRXMN1-FORM-FIELDS TO WR-BUF.
           MOVE TRXMN1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
           MOVE TRXMN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRXMN1-HELP--DIR TO HELP-LINK-DIR.

      ********************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRXMN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       DISPLAY-SCAN-KEY SECTION.
      ******************************************************************

           MOVE "SNNN040BRANCH." TO VDU
           MOVE BR-NO TO VDUNUM0   SAVE-BRNO.
           PERFORM SCREENIO.

           MOVE LPWSTRX-TX-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO DATE-MMYY-MM.
           MOVE DATE-YYYYMMDD-YY TO DATE-MMYY-YY.
           MOVE "SNNN040MMYY." TO VDU.
           MOVE DATE-MMYY      TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020DAY."    TO VDU.
           MOVE DATE-YYYYMMDD-DD TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BRNO." TO VDU.
           MOVE LPWSTRX-TX-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NA010CODE." TO VDU.
           MOVE "X"            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN020CLASS." TO VDU.
           MOVE LPWSTRX-TX-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060NUMBER." TO VDU.
           MOVE LPWSTRX-TX-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LAWCODE." TO VDU.
           MOVE LPWSTRX-TX-LAWCODE TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LINE." TO VDU.
           MOVE LPWSTRX-TX-LINE TO VDUNUM0.
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
     
           MOVE "B"                        TO TRX-PATH-OVERRIDE.
     
       LOAD-BRANCH-MODULE-EXIT.
           EXIT.
     
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTRXCL.CPY".

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
           PERFORM CLOSE-TRX1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRXGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPTRX1IN.CPY".
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
           DESTROY TRXMN1-SCREEN.
           MOVE FORM-PATH   TO FORM-PATHNAME.
           MOVE TRXMIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
