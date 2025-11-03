       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRKMN1.
       DATE-WRITTEN.    11/05/96.
      **************************************************************************
      *   TRANSACTION FILE MAINTENANCE (CONVERSION)
      *              PAGE 1
      *
      *
      * REV:
      *   MJD 050995 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *   MJD 042696 ADDED GP-FILE, GP-DLR-DISP-TYPE.
      *   GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *   JTG 080129 CHANGED FOR JUDY/CS/JOHN TO ALLOW BULK-CONVERSION-FG
      *              TO TAKE UPFRONT EARNINGS                 FINAUTO     #3141
      *    CS 101118 ALLOW ACCESS TO EARNINGS FIELDS ELE 21-26 WAS FLAGGED
      *              AS INVALID ELE   SUPPORT
      * MJD 131206 A30 CONVERSION COMPLETED
      *  CS 161005 CHANGED HOLD-KEY TO HOLD-TRK-KEY, HOLD-FIELDS TO HOLD-TK
      *            CHANGED HARD CODE SIZE OF ORIGINAL-TR-REC/CHANGED-TR-REC
      *            TO TR-SIZE, REMOVED TK-SEQNO
      *            SAVEKEY FROM X(14) TO X(27)
      *            REMOVE D-SEQNO FROM KEY-BUF
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
      *
      *    W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/TRKMN1.CBL S34 11/16/21-14:20:50 >".

      ******************************************************************
      *
      *     PROGRAM WORKERS
      *
      ******************************************************************
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

      *-----------------------------------------------------------------
       01  ONE             PIC S9.

       01  ELE             PIC 9(2).
           88  INVALID-ELE VALUE 13 27 THRU 44
                                 45 46 47 48 49 50 51
                                 52 53 54 55 56.

       01  OVER-20-ELE     PIC 9(2).
       01  OVER-40-ELE     PIC 9(2).

       01  ACTIVE-FILE-SW        PIC X(3)    VALUE "OFF".
       01  LEAVE-BRANCH-MMYY-SW  PIC X(3)    VALUE "OFF".

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
           03  CHNO-J      REDEFINES CHNO
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

       01  ORIGINAL-TRK-REC    PIC X(TRK-SIZE).
       01  CHANGED-TRK-REC     PIC X(TRK-SIZE).

       01  DLR-TYP-LIST      PIC X(6)   VALUE "18P:P.".
       01  DLR-TYP-BUF       PIC X(4).

       01  TRKMIN-LINK VALUE SPACES.
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
           03  TRKMIN-WINDOW-HANDLE PIC X(10).

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
           03  F08         PIC 9999            BLANK WHEN ZERO.
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
           03  F20         PIC ZZZ,ZZ9.99      BLANK WHEN ZERO.
           03  F21         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F22         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F23         PIC ZZZ,ZZ9.99      BLANK WHEN ZERO.
           03  F24         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F25         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F26         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.

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
      * 6. DLRECOURSE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 7. REFINANCE-FRT
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 8. REFINANCE-BR
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      * 9. REPOCD
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *10. PLCD
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *11. PLTYPE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *12. ************* NOT IN USE ***************
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *13. ************* NOT IN USE ***************
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *14. PROCEEDS
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *15. LNAME
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *16. FINANCE CHARGE
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *17. INTEREST CHARGE
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *18. DL DISCOUNT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *19. PUR BAL
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *20. DL PARTRCAMT
           03  FILLER      PIC X(8) VALUE "E NN062 ".
      *21. EARNED 1
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *22. EARNED 2
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *23. EARNED 3
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *24. EARNED 4
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *25. EARNED 5
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *26. EARNED 6
           03  FILLER      PIC X(8) VALUE "E NS072 ".
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
       COPY "LIBWI/TRKSCNW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TRKMN1_DEF.CPY".
       COPY "LIBMN/TRKMN1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/TRKMN1_SCN.CPY".

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
           MOVE "TRKMN1" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO TRKMIN-LINK.

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
                  MOVE LPWSTRK-TRK1-KEY TO SAVEKEY
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

           MOVE SAVEKEY  TO LPWSTRK-TRK1-KEY TRK1-KEY.
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

           PERFORM OPEN-TRK1-FILE.

       EXIT-BRANCH-MMYY-MODULE.
           EXIT.

      ******************************************************************
       ENTER-KEYS-MODULE SECTION.
      ******************************************************************
       ENTER-DAY.
           MOVE "F6 - SCAN"   TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPACES        TO IO-TYPE.

           MOVE 0 TO LPWSTRK-TK-DATE LPWSTRK-TK-BRNO
                     LPWSTRK-TK-CLASS LPWSTRK-TK-ACCTNO
                     LPWSTRK-TK-LAWCODE LPWSTRK-TK-LINE.
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
              MOVE SAVE-BRNO   TO TRKSCN-BRNO
              MOVE SAVE-MMYY   TO TRKSCNW-MMYY
              MOVE "Y"         TO TRKSCN-RETURN
              MOVE "WI/TRKSCN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH TRKSCN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF TRKSCN-STATUS NOT = "00"
                 GO TO ENTER-DAY
              ELSE
                 MOVE TRKSCN-TRK1-KEY TO LPWSTRK-TRK1-KEY
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

           MOVE DATE-YYYYMMDD TO LPWSTRK-TK-DATE.

      *-----------------------------------------------------------------
       ENTER-BRNO.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRNO." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO LPWSTRK-TK-BRNO
                                      TK-BRNO.
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
      *    MOVE VDUNUM0 TO BR-NO LPWSTRK-TK-BRNO TK-BRNO.

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
           MOVE "K"            TO VDUBUF.
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

           MOVE VDUNUM0 TO LPWSTRK-TK-CLASS.

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

           MOVE VDUNUM0 TO LPWSTRK-TK-ACCTNO.

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

           MOVE VDUNUM0 TO LPWSTRK-TK-LAWCODE.

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

           MOVE VDUNUM0 TO LPWSTRK-TK-LINE.

      *-----------------------------------------------------------------
       READ-TRANS-FILE.
           PERFORM OPEN-TRK1-FILE.
           MOVE LPWSTRK-TRK1-KEY TO TRK1-KEY.
           PERFORM READ-TRK1-FILE.
           IF IO-FG = 0
              MOVE TRK-REC TO ORIGINAL-TRK-REC
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

           IF CHNO-J NOT NUMERIC
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

           GO TO F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34 F-35 F-36 F-37 F-38 F-39 F-40
                 DEPENDING ON OVER-20-ELE.
           SUBTRACT 20 FROM OVER-20-ELE GIVING OVER-40-ELE.

           GO TO F-41 F-42 F-43 F-44 F-45 F-46 F-47 F-48 F-49 F-50
                 F-51 F-52 F-53 F-54 F-55 F-56
                 DEPENDING ON OVER-40-ELE.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
      *
      *           SET RECORD ELEMENTS
      *
      *-----------------------------------------------------------------
       F-01.
           MOVE VDUBUF TO TK-PURCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO TK-SRCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO TK-COLLCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO TK-SERVACCT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO TK-LOANTYPE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO TK-DLRECOURSE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO TK-REF-FRTO
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUNUM0 TO TK-REFBR
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUBUF TO TK-REPOCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUBUF TO TK-PLCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUBUF TO TK-PLTYPE.
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
           MOVE VDUNUM2 TO TK-PROCEEDS.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUNUM2 TO TK-LNAMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUNUM2 TO TK-FINCHG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUNUM2 TO TK-INTCHG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           MOVE VDUNUM2 TO TK-DLDISC.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
           MOVE VDUNUM2 TO TK-PURBAL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           MOVE VDUNUM2 TO TK-DLPARTRCAMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           MOVE VDUNUM2 TO TK-EARNED(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           MOVE VDUNUM2 TO TK-EARNED(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           MOVE VDUNUM2 TO TK-EARNED(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           MOVE VDUNUM2 TO TK-EARNED(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           MOVE VDUNUM2 TO TK-EARNED(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           MOVE VDUNUM2 TO TK-EARNED(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-27.
      *    MOVE VDUBUF TO TK-FEECODE(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-28.
      *    MOVE VDUNUM2 TO TK-FEEAMT(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-29.
      *    MOVE VDUBUF TO TK-FEEOURS(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-30.
      *    MOVE VDUBUF TO TK-FEECODE(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-31.
      *    MOVE VDUNUM2 TO TK-FEEAMT(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-32.
      *    MOVE VDUBUF TO TK-FEEOURS(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-33.
      *    MOVE VDUBUF TO TK-FEECODE(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-34.
      *    MOVE VDUNUM2 TO TK-FEEAMT(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-35.
      *    MOVE VDUBUF TO TK-FEEOURS(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-36.
      *    MOVE VDUBUF TO TK-FEECODE(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-37.
      *    MOVE VDUNUM2 TO TK-FEEAMT(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-38.
      *    MOVE VDUBUF TO TK-FEEOURS(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-39.
      *    MOVE VDUBUF TO TK-FEECODE(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-40.
      *    MOVE VDUNUM2 TO TK-FEEAMT(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-41.
      *    MOVE VDUBUF TO TK-FEEOURS(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-42.
      *    MOVE VDUBUF TO TK-FEECODE(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-43.
      *    MOVE VDUNUM2 TO TK-FEEAMT(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-44.
      *    MOVE VDUBUF TO TK-FEEOURS(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-45.
      *- - - - -UNUSED- - - - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-46.
      *- - - - -UNUSED- - - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-47.
      *- - - - -UNUSED- - - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-48.
      *- - - - -UNUSED- - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-49.
      *- - - - -UNUSED- - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-50.
      *- - - - -UNUSED- - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-51.
      *- - - - -UNUSED- - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-52.
      *- - - - -UNUSED- - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-53.
      *- - - - -UNUSED- - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-54.
      *- - - - -UNUSED- - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-55.
      *- - - - -UNUSED- - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-56.
      *- - - - -UNUSED- - - - -
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TRK1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE TRK-REC TO CHANGED-TRK-REC.

           IF ORIGINAL-TRK-REC = CHANGED-TRK-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-TRK1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF TRK-REC NOT = ORIGINAL-TRK-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-TRK-REC TO TRK-REC.
           PERFORM REWRITE-TRK1-FILE.

           MOVE "C"                        TO ERRLOGW-ACTION.
           MOVE TRK1-KEY                    TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME              TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-TRK1-FILE.

           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.

       END-WRITE-MODULE.
           PERFORM CLOSE-TRK1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-TRK1-FILE.
           PERFORM READ-TRK1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF TRK-REC NOT = ORIGINAL-TRK-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-TRK1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-TRK1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1             TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-TRK-FILE.
           MOVE LPWSTRK-TRK1-KEY TO TRK1-KEY.
           MOVE 0 TO ELE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES               TO DISPLAY-BUF.
           MOVE TK-PURCD             TO F01.
           MOVE TK-SRCD              TO F02.
           MOVE TK-COLLCD            TO F03.
           MOVE TK-SERVACCT          TO F04.
           MOVE TK-LOANTYPE          TO F05.
           MOVE TK-DLRECOURSE        TO F06.
           MOVE TK-REF-FRTO          TO F07.
           MOVE TK-REFBR             TO F08.
           MOVE TK-REPOCD            TO F09.
           MOVE TK-PLCD              TO F10.
           MOVE TK-PLTYPE            TO F11.
      *----------------------------->TO F12.
      *----------------------------->TO F13.
           MOVE TK-PROCEEDS          TO F14.
           MOVE TK-LNAMT             TO F15.
           MOVE TK-FINCHG            TO F16.
           MOVE TK-INTCHG            TO F17.
           MOVE TK-DLDISC            TO F18.
           MOVE TK-PURBAL            TO F19.
           MOVE TK-DLPARTRCAMT       TO F20.
           MOVE TK-EARNED(1)         TO F21.
           MOVE TK-EARNED(2)         TO F22.
           MOVE TK-EARNED(3)         TO F23.
           MOVE TK-EARNED(4)         TO F24.
           MOVE TK-EARNED(5)         TO F25.
           MOVE TK-EARNED(6)         TO F26.

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
           MOVE LPWSTRK-TK-DATE  TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO D-MM.
           MOVE DATE-YYYYMMDD-YY TO D-YY.


           MOVE DATE-YYYYMMDD-DD      TO D-DAY.
           MOVE LPWSTRK-TK-BRNO       TO D-BRNO.
           MOVE "K"                   TO D-CODE.
           MOVE LPWSTRK-TK-CLASS      TO D-CLASS.
           MOVE LPWSTRK-TK-ACCTNO     TO D-ACCTNO.
           MOVE LPWSTRK-TK-LAWCODE    TO D-LAWCODE.
           MOVE LPWSTRK-TK-LINE       TO D-LINE.

           MOVE KEY-BUF TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE EXT-CONAME-CONAME TO TRKMN1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRKMN1-SYSDATE.
           DISPLAY SPACES UPON TRKMIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRKMN1-SCREEN UPON TRKMIN-WINDOW-HANDLE.
           MOVE TRKMN1-FORM-FIELDS TO WR-BUF.
           MOVE TRKMN1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE TRKMN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRKMN1-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRKMN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       DISPLAY-SCAN-KEY SECTION.
      ******************************************************************

           MOVE "SNNN040BRANCH." TO VDU
           MOVE BR-NO TO VDUNUM0   SAVE-BRNO.
           PERFORM SCREENIO.

           MOVE LPWSTRK-TK-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO DATE-MMYY-MM.
           MOVE DATE-YYYYMMDD-YY TO DATE-MMYY-YY.
           MOVE "SNNN040MMYY." TO VDU.
           MOVE DATE-MMYY      TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020DAY."    TO VDU.
           MOVE DATE-YYYYMMDD-DD TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BRNO." TO VDU.
           MOVE LPWSTRK-TK-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NA010CODE." TO VDU.
           MOVE "K"            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN020CLASS." TO VDU.
           MOVE LPWSTRK-TK-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060NUMBER." TO VDU.
           MOVE LPWSTRK-TK-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LAWCODE." TO VDU.
           MOVE LPWSTRK-TK-LAWCODE TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LINE." TO VDU.
           MOVE LPWSTRK-TK-LINE TO VDUNUM0.
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
     
           MOVE "B"                        TO TRK-PATH-OVERRIDE.
     
       LOAD-BRANCH-MODULE-EXIT.
           EXIT.
     

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTRKCL.CPY".

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
           PERFORM CLOSE-TRK1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRKGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPTRK1IN.CPY".
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
           DESTROY TRKMN1-SCREEN.
           MOVE FORM-PATH   TO FORM-PATHNAME.
           MOVE TRKMIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
