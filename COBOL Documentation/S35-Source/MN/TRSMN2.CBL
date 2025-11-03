       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRSMN2.
       DATE-WRITTEN.    11-25-94.
      ******************************************************************
      *   TRANSACTION SALES FILE MAINTENANCE
      *              PAGE 2
      *
      * REV:
      * MJD 050995 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      * MJD 063095 ADDED LOGIC FOR PAGE 4.
      * JTG 020917 ADDED LN-DLCOMM-03-CMSI LN-DLCOMM-PP-CMSI      REGACC #1846
      *            REMOVED INSURANCE LEVEL 9 AND 10
      *            ADDED TS-DLCOMM-PP-CMSI TS-DLCOMM-O3-CMSI
      * MJD 131203 A30 CONVERSION COMPLETED
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *         ADDED LIBLP/LPFRTR; REMOVED LIBLP/LPTRTMPW.
      *         ADDED LPFRTR-UPDATE-CHGLOG; REMOVED WRITE-TR-CHANGE-LOG.
      * CS 170509 ACTIVATE 8 DIGIT DATES
      * BAH 2019.0508 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
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
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/TRSMN2.CBL S34 11/15/21-11:59:06 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
      *-----------------------------------------------------------------
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).

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
       01  INVALID-FG      PIC X(1)    VALUE "N".
       01  NEW-FLAG        PIC 9.

       01  SAVE-BRNO       PIC 9(4).
       01  SAVE-MMYY       PIC 9(4).
       01  SAVE-MMYYX REDEFINES SAVE-MMYY.
           03  SAVE-MM     PIC 99.
           03  SAVE-YY     PIC 99.

       01  I-LEVEL         PIC 9(2)    VALUE ZERO.

       01  WK-INSURED              PIC X(1).
           88  GOOD-INSURED        VALUE "N" "M" "S" "J".

       01  ORIGINAL-TRS-REC    PIC X(TRS-SIZE).
       01  CHANGED-TRS-REC     PIC X(TRS-SIZE).

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
           03  F01         PIC X.
           03  F02         PIC X.
           03  F021        PIC X(15).
           03  F03         PIC X.
           03  F031        PIC X(15).
           03  F04         PIC ZZ              BLANK WHEN ZERO.
           03  F041        PIC X.
           03  F042        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F043        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F044        PIC ZZ,ZZZ.ZZ-      BLANK WHEN ZERO.
           03  F045        PIC ZZZ,ZZZ.ZZ-     BLANK WHEN ZERO.
           03  F05         PIC ZZ              BLANK WHEN ZERO.
           03  F051        PIC X.
           03  F052        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F053        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F054        PIC ZZ,ZZZ.ZZ-      BLANK WHEN ZERO.
           03  F055        PIC ZZZ,ZZZ.ZZ-     BLANK WHEN ZERO.
           03  F06         PIC ZZ              BLANK WHEN ZERO.
           03  F061        PIC X.
           03  F062        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F063        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F064        PIC ZZ,ZZZ.ZZ-      BLANK WHEN ZERO.
           03  F065        PIC ZZZ,ZZZ.ZZ-     BLANK WHEN ZERO.
           03  F07         PIC ZZ              BLANK WHEN ZERO.
           03  F071        PIC X.
           03  F072        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F073        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F074        PIC ZZ,ZZZ.ZZ-      BLANK WHEN ZERO.
           03  F075        PIC ZZZ,ZZZ.ZZ-     BLANK WHEN ZERO.
           03  F08         PIC ZZ              BLANK WHEN ZERO.
           03  F081        PIC X.
           03  F082        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F083        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F084        PIC ZZ,ZZZ.ZZ-      BLANK WHEN ZERO.
           03  F085        PIC ZZZ,ZZZ.ZZ-     BLANK WHEN ZERO.
           03  F09         PIC ZZ              BLANK WHEN ZERO.
           03  F091        PIC X.
           03  F092        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F093        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F094        PIC ZZ,ZZZ.ZZ-      BLANK WHEN ZERO.
           03  F095        PIC ZZZ,ZZZ.ZZ-     BLANK WHEN ZERO.
           03  F10         PIC ZZ              BLANK WHEN ZERO.
           03  F101        PIC X.
           03  F102        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F103        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F104        PIC ZZ,ZZZ.ZZ-      BLANK WHEN ZERO.
           03  F105        PIC ZZZ,ZZZ.ZZ-     BLANK WHEN ZERO.
           03  F11         PIC ZZ              BLANK WHEN ZERO.
           03  F111        PIC X.
           03  F112        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F113        PIC ZZ/ZZ/ZZ        BLANK WHEN ZERO.
           03  F114        PIC ZZ,ZZZ.ZZ-      BLANK WHEN ZERO.
           03  F115        PIC ZZZ,ZZZ.ZZ-     BLANK WHEN ZERO.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(20) VALUE
           "010 020 021 030 031 ".
           03  FILLER      PIC X(24) VALUE
           "040 041 042 043 044 045 ".
           03  FILLER      PIC X(24) VALUE
           "050 051 052 053 054 055 ".
           03  FILLER      PIC X(24) VALUE
           "060 061 062 063 064 065 ".
           03  FILLER      PIC X(24) VALUE
           "070 071 072 073 074 075 ".
           03  FILLER      PIC X(24) VALUE
           "080 081 082 083 084 085 ".
           03  FILLER      PIC X(24) VALUE
           "090 091 092 093 094 095 ".
           03  FILLER      PIC X(24) VALUE
           "100 101 102 103 104 105 ".
           03  FILLER      PIC X(24) VALUE
           "110 111 112 113 114 115.".

       01  MAX             PIC 99   VALUE 11.

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS
      ******************************************************************
       01  SPEC-TABLE.
      * 1.  INSURED
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 2.  TYPE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 21. TYPE DESCRIPTION
      *    03  FILLER      PIC X(8) VALUE "E NA150 ".
      * 3.  ELIGABLE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 31. ELIGABLE DESCRIPTION
      *    03  FILLER      PIC X(8) VALUE "E NA150 ".
      * 4.   CREDIT LIFE - CO, OURS, EFF, EXP, PREM, COMM
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *    03  FILLER      PIC X(8) VALUE "E NA010 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "E NS052 ".
      *    03  FILLER      PIC X(8) VALUE "E NS062 ".
      * 5.   ACCIDENT/HEALTH - CO, OURS, EFF, EXP, PREM, COMM
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *    03  FILLER      PIC X(8) VALUE "E NA010 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "E NS052 ".
      *    03  FILLER      PIC X(8) VALUE "E NS062 ".
      * 6.   PERSONAL PROPERTY - CO, OURS, EFF, EXP, PREM, COMM
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *    03  FILLER      PIC X(8) VALUE "E NA010 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "E NS052 ".
      *    03  FILLER      PIC X(8) VALUE "E NS062 ".
      * 7.   OTHER INSURANCE #1 - CO, OURS, EFF, EXP, PREM, COMM
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *    03  FILLER      PIC X(8) VALUE "E NA010 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "E NS052 ".
      *    03  FILLER      PIC X(8) VALUE "E NS062 ".
      * 8.   OTHER INSURANCE #2 - CO, OURS, EFF, EXP, PREM, COMM
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *    03  FILLER      PIC X(8) VALUE "E NA010 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "E NS052 ".
      *    03  FILLER      PIC X(8) VALUE "E NS062 ".
      * 9.   OTHER INSURANCE #3 - CO, OURS, EFF, EXP, PREM, COMM
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *    03  FILLER      PIC X(8) VALUE "E NA010 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "E NS052 ".
      *    03  FILLER      PIC X(8) VALUE "E NS062 ".
      *10.   OTHER INSURANCE #4 - CO, OURS, EFF, EXP, PREM, COMM
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *    03  FILLER      PIC X(8) VALUE "E NA010 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "E NS052 ".
      *    03  FILLER      PIC X(8) VALUE "E NS062 ".
      *11.   OTHER INSURANCE #5 - CO, OURS, EFF, EXP, PREM, COMM
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *    03  FILLER      PIC X(8) VALUE "E NA010 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *    03  FILLER      PIC X(8) VALUE "E NS052 ".
      *    03  FILLER      PIC X(8) VALUE "E NS062 ".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 11 TIMES.
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
       COPY "LIBMN/TRSMN2_DEF.CPY".
       COPY "LIBMN/TRSMN2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/TRSMN2_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N   M O D U L E
      *
      ******************************************************************
        MAIN-PROGRAM SECTION.
      D    STOP MESS.

        COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TRSMN2" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO TRSMIN-LINK.
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
       ENTER-SAVE-BRANCH.
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
           MOVE "F6 - SCAN"    TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPACES         TO IO-TYPE.

           MOVE 0 TO LPWSTRS-TS-DATE LPWSTRS-TS-BRNO
                     LPWSTRS-TS-CLASS LPWSTRS-TS-ACCTNO
                     LPWSTRS-TS-LAWCODE LPWSTRS-TS-LINE.
           MOVE 0 TO NEW-FLAG.

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
              MOVE SAVE-BRNO TO TRSSCN-BRNO
              MOVE SAVE-MMYY TO TRSSCNW-MMYY
              MOVE "Y"       TO TRSSCN-RETURN
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
              MOVE "X" TO ERRCD
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
           MOVE BR-NAME TO VDUBUF.
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
           IF IO-FG NOT = 0
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           MOVE TRS-REC TO ORIGINAL-TRS-REC.

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

      ****************************************************************
      *     E N T R Y    M O D U L E
      *
      *        NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                   1 - INITIAL NEW RECORD ENTRY
      *                   2 - MODIFY NEW RECORD
      ****************************************************************
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
              IF FORM-SEL1 < 0 OR = 2 OR > 4
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
                 F-11
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      ****************************************************************
      *    *      SET RECORD ELEMENTS
      ****************************************************************
       F-01.
           MOVE VDUBUF TO WK-INSURED.
           IF NOT GOOD-INSURED
              MOVE "ENTER: 'N' 'M' 'S' 'J'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO TS-INSURED.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO TS-INSTYPES.
           IF NOT TS-INSTYPES-VALID
              GO TO ENTER-ELE.
           MOVE "S  A150 " TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           IF TS-INSTYPES = "B" MOVE "LL" TO VDUBUF
           ELSE IF TS-INSTYPES = "C" MOVE "DL" TO VDUBUF
           ELSE IF TS-INSTYPES = "D" MOVE "A&H" TO VDUBUF
           ELSE IF TS-INSTYPES = "E" MOVE "PROP" TO VDUBUF
           ELSE IF TS-INSTYPES = "F" MOVE "LL, AH" TO VDUBUF
           ELSE IF TS-INSTYPES = "G" MOVE "DL, AH" TO VDUBUF
           ELSE IF TS-INSTYPES = "H" MOVE "LL, PP" TO VDUBUF
           ELSE IF TS-INSTYPES = "J" MOVE "DL, PP" TO VDUBUF
           ELSE IF TS-INSTYPES = "K" MOVE "AH, PP" TO VDUBUF
           ELSE IF TS-INSTYPES = "L" MOVE "LL, AH, PP" TO VDUBUF
           ELSE IF TS-INSTYPES = "M" MOVE "DL, AH, PP" TO VDUBUF
           ELSE IF TS-INSTYPES = "N" MOVE "NONE" TO VDUBUF
           ELSE IF TS-INSTYPES = "R" MOVE "PROP2" TO VDUBUF
           ELSE IF TS-INSTYPES = "S" MOVE "LL, PP2" TO VDUBUF
           ELSE IF TS-INSTYPES = "T" MOVE "DL, PP2" TO VDUBUF
           ELSE IF TS-INSTYPES = "V" MOVE "AH, PP2" TO VDUBUF
           ELSE IF TS-INSTYPES = "W" MOVE "LL, AH, PP2" TO VDUBUF
           ELSE IF TS-INSTYPES = "X" MOVE "DL, AH, PP2" TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO TS-INSELIGIBLE.
           IF NOT TS-INSELIGIBLE-VALID
              GO TO ENTER-ELE.
           MOVE "S  A150 " TO VDU
           MOVE ELE TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           IF TS-INSELIGIBLE = "B" MOVE "LL" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "C" MOVE "DL" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "D" MOVE "A&H" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "E" MOVE "PROP" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "F" MOVE "LL, AH" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "G" MOVE "DL, AH" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "H" MOVE "LL, PP" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "J" MOVE "DL, PP" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "K" MOVE "AH, PP" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "L" MOVE "LL, AH, PP" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "M" MOVE "DL, AH, PP" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "N" MOVE "NONE" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "R" MOVE "PROP2" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "S" MOVE "LL, PP2" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "T" MOVE "DL, PP2" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "V" MOVE "AH, PP2" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "W" MOVE "LL, AH, PP2" TO VDUBUF
           ELSE IF TS-INSELIGIBLE = "X" MOVE "DL, AH, PP2" TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE 1 TO I-LEVEL.

           IF VDUNUM0 = 0
              PERFORM INSURANCE-ENTRY
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM0 TO TS-INSCOMP(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-041.
           MOVE "R NA010041." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-041.

           IF NOT ( VDUBUF = "Y" OR "M" OR "N" )
              MOVE "Y-YES, N-NO OR M-MEMO" TO MESS
              PERFORM SEND-MESS
              GO TO F-041.

           MOVE VDUBUF TO TS-INSOURS(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-042.
           MOVE "ERNM060042." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-042.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEFF-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-043.
           MOVE "ERNM060043." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-043.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEXP-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-044.
           MOVE "E NS052044." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-044.

           MOVE VDUNUM2 TO TS-INSPREM(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-045.
           MOVE "E NS062045." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-045.

           MOVE VDUNUM2 TO TS-INSCOMM(I-LEVEL).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE 2 TO I-LEVEL.

           IF VDUNUM0 = 0
              PERFORM INSURANCE-ENTRY
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM0 TO TS-INSCOMP(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-051.
           MOVE "R NA010051." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-051.

           IF NOT ( VDUBUF = "Y" OR "M" OR "N" )
              MOVE "Y-YES, N-NO OR M-MEMO" TO MESS
              PERFORM SEND-MESS
              GO TO F-051.

           MOVE VDUBUF TO TS-INSOURS(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-052.
           MOVE "ERNM060052." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-052.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEFF-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-053.
           MOVE "ERNM060053." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-053.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEXP-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-054.
           MOVE "E NS052054." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-054.

           MOVE VDUNUM2 TO TS-INSPREM(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-055.
           MOVE "E NS062055." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-055.

           MOVE VDUNUM2 TO TS-INSCOMM(I-LEVEL).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE 3 TO I-LEVEL.

           IF VDUNUM0 = 0
              PERFORM INSURANCE-ENTRY
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM0 TO TS-INSCOMP(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-061.
           MOVE "R NA010061." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-061.

           IF NOT ( VDUBUF = "Y" OR "M" OR "N" )
              MOVE "Y-YES, N-NO OR M-MEMO" TO MESS
              PERFORM SEND-MESS
              GO TO F-061.

           MOVE VDUBUF TO TS-INSOURS(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-062.
           MOVE "ERNM060062." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-062.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEFF-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-063.
           MOVE "ERNM060063." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-063.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEXP-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-064.
           MOVE "E NS052064." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-064.

           MOVE VDUNUM2 TO TS-INSPREM(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-065.
           MOVE "E NS062065." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-065.

           MOVE VDUNUM2 TO TS-INSCOMM(I-LEVEL).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE 4 TO I-LEVEL.

           IF VDUNUM0 = 0
              PERFORM INSURANCE-ENTRY
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM0 TO TS-INSCOMP(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-071.
           MOVE "R NA010071." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-071.

           IF NOT ( VDUBUF = "Y" OR "M" OR "N" )
              MOVE "Y-YES, N-NO OR M-MEMO" TO MESS
              PERFORM SEND-MESS
              GO TO F-071.

           MOVE VDUBUF TO TS-INSOURS(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-072.
           MOVE "ERNM060072." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-072.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEFF-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-073.
           MOVE "ERNM060073." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-073.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEXP-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-074.
           MOVE "E NS052074." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-074.

           MOVE VDUNUM2 TO TS-INSPREM(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-075.
           MOVE "E NS062075." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-075.

           MOVE VDUNUM2 TO TS-INSCOMM(I-LEVEL).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE 5 TO I-LEVEL.

           IF VDUNUM0 = 0
              PERFORM INSURANCE-ENTRY
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM0 TO TS-INSCOMP(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-081.
           MOVE "R NA010081." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-081.

           IF NOT ( VDUBUF = "Y" OR "M" OR "N" )
              MOVE "Y-YES, N-NO OR M-MEMO" TO MESS
              PERFORM SEND-MESS
              GO TO F-081.

           MOVE VDUBUF TO TS-INSOURS(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-082.
           MOVE "ERNM060082." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-082.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEFF-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-083.
           MOVE "ERNM060083." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-083.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEXP-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-084.
           MOVE "E NS052084." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-084.

           MOVE VDUNUM2 TO TS-INSPREM(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-085.
           MOVE "E NS062085." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-085.

           MOVE VDUNUM2 TO TS-INSCOMM(I-LEVEL).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE 6 TO I-LEVEL.

           IF VDUNUM0 = 0
              PERFORM INSURANCE-ENTRY
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM0 TO TS-INSCOMP(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-091.
           MOVE "R NA010091." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-091.

           IF NOT ( VDUBUF = "Y" OR "M" OR "N" )
              MOVE "Y-YES, N-NO OR M-MEMO" TO MESS
              PERFORM SEND-MESS
              GO TO F-091.

           MOVE VDUBUF TO TS-INSOURS(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-092.
           MOVE "ERNM060092." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-092.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEFF-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-093.
           MOVE "ERNM060093." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-093.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEXP-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-094.
           MOVE "E NS052094." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-094.

           MOVE VDUNUM2 TO TS-INSPREM(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-095.
           MOVE "E NS062095." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-095.

           MOVE VDUNUM2 TO TS-INSCOMM(I-LEVEL).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE 7 TO I-LEVEL.

           IF VDUNUM0 = 0
              PERFORM INSURANCE-ENTRY
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM0 TO TS-INSCOMP(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-101.
           MOVE "R NA010101." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-101.

           IF NOT ( VDUBUF = "Y" OR "M" OR "N" )
              MOVE "Y-YES, N-NO OR M-MEMO" TO MESS
              PERFORM SEND-MESS
              GO TO F-101.

           MOVE VDUBUF TO TS-INSOURS(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-102.
           MOVE "ERNM060102." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-102.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEFF-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-103.
           MOVE "ERNM060103." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-103.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEXP-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-104.
           MOVE "E NS052104." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-104.

           MOVE VDUNUM2 TO TS-INSPREM(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-105.
           MOVE "E NS062105." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-105.

           MOVE VDUNUM2 TO TS-INSCOMM(I-LEVEL).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE 8 TO I-LEVEL.

           IF VDUNUM0 = 0
              PERFORM INSURANCE-ENTRY
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM0 TO TS-INSCOMP(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-111.
           MOVE "R NA010111." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-111.

           IF NOT ( VDUBUF = "Y" OR "M" OR "N" )
              MOVE "Y-YES, N-NO OR M-MEMO" TO MESS
              PERFORM SEND-MESS
              GO TO F-111.

           MOVE VDUBUF TO TS-INSOURS(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-112.
           MOVE "ERNM060112." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-112.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEFF-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-113.
           MOVE "ERNM060113." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-113.

           MOVE VDU-DATE-YYYYMMDD TO TS-INSEXP-DATE(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-114.
           MOVE "E NS052114." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-114.

           MOVE VDUNUM2 TO TS-INSPREM(I-LEVEL).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-115.
           MOVE "E NS062115." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           PERFORM CHECK-ENTRY.

           IF ( INVALID-FG = "Y" )
              GO TO F-115.

           MOVE VDUNUM2 TO TS-INSCOMM(I-LEVEL).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       CHECK-ENTRY.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF (VDUSTATUS = "1U")
              GO TO ENTER-ELE.

           IF NOT (VDUSTATUS = "00" OR "1D")
              MOVE "Y" TO INVALID-FG
           ELSE
              MOVE "N" TO INVALID-FG.

       END-ENTRY-MODULE.
           EXIT.

      *****************************************************************
       INSURANCE-ENTRY SECTION.
      *****************************************************************
           MOVE 0 TO IO-FG.
           IF VDUNUM0 = 0
              MOVE "SN N020  0." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO TS-INSCOMP(I-LEVEL) VDUNUM0
              PERFORM SCREENIO
              MOVE "S  A010  1." TO VDU
              MOVE ELE TO VDUROW
              MOVE SPACES TO TS-INSOURS(I-LEVEL)
              PERFORM SCREENIO
              MOVE "S  M060  2." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO TS-INSEFF-DATE(I-LEVEL) VDUNUM0
              PERFORM SCREENIO
              MOVE "S  M060  3." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO TS-INSEXP-DATE(I-LEVEL) VDUNUM0
              PERFORM SCREENIO
              MOVE "SN N052  4." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO TS-INSPREM(I-LEVEL) VDUNUM2
              PERFORM SCREENIO
              MOVE "SN N062  5." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO TS-INSCOMM(I-LEVEL) VDUNUM2
              PERFORM SCREENIO
              GO TO EXIT-ENTER-INSURANCE.

       EXIT-ENTER-INSURANCE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TRS1-FILE.
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
              PERFORM DISPLAY-FIELDS
              MOVE ORIGINAL-TRS-REC TO TRS-REC
              PERFORM DISPLAY-FIELDS
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-TRS-REC TO TRS-REC.
           PERFORM REWRITE-TRS1-FILE.

           MOVE "C"                        TO ERRLOGW-ACTION.
           MOVE TRS1-KEY                   TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME              TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

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
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE TS-INSURED     TO F01.
           MOVE TS-INSTYPES    TO F02.

           IF TS-INSTYPES = "B" MOVE "LL" TO F021
           ELSE IF TS-INSTYPES = "C" MOVE "DL" TO F021
           ELSE IF TS-INSTYPES = "D" MOVE "A&H" TO F021
           ELSE IF TS-INSTYPES = "E" MOVE "PROP" TO F021
           ELSE IF TS-INSTYPES = "F" MOVE "LL, AH" TO F021
           ELSE IF TS-INSTYPES = "G" MOVE "DL, AH" TO F021
           ELSE IF TS-INSTYPES = "H" MOVE "LL, PP" TO F021
           ELSE IF TS-INSTYPES = "J" MOVE "DL, PP" TO F021
           ELSE IF TS-INSTYPES = "K" MOVE "AH, PP" TO F021
           ELSE IF TS-INSTYPES = "L" MOVE "LL, AH, PP" TO F021
           ELSE IF TS-INSTYPES = "M" MOVE "DL, AH, PP" TO F021
           ELSE IF TS-INSTYPES = "N" MOVE "NONE" TO F021
           ELSE IF TS-INSTYPES = "R" MOVE "PROP2" TO F021
           ELSE IF TS-INSTYPES = "S" MOVE "LL, PP2" TO F021
           ELSE IF TS-INSTYPES = "T" MOVE "DL, PP2" TO F021
           ELSE IF TS-INSTYPES = "V" MOVE "AH, PP2" TO F021
           ELSE IF TS-INSTYPES = "W" MOVE "LL, AH, PP2" TO F021
           ELSE IF TS-INSTYPES = "X" MOVE "DL, AH, PP2" TO F021.

           MOVE TS-INSELIGIBLE TO F03.
           IF TS-INSELIGIBLE = "B" MOVE "LL" TO F031
           ELSE IF TS-INSELIGIBLE = "C" MOVE "DL" TO F031
           ELSE IF TS-INSELIGIBLE = "D" MOVE "A&H" TO F031
           ELSE IF TS-INSELIGIBLE = "E" MOVE "PROP" TO F031
           ELSE IF TS-INSELIGIBLE = "F" MOVE "LL, AH" TO F031
           ELSE IF TS-INSELIGIBLE = "G" MOVE "DL, AH" TO F031
           ELSE IF TS-INSELIGIBLE = "H" MOVE "LL, PP" TO F031
           ELSE IF TS-INSELIGIBLE = "J" MOVE "DL, PP" TO F031
           ELSE IF TS-INSELIGIBLE = "K" MOVE "AH, PP" TO F031
           ELSE IF TS-INSELIGIBLE = "L" MOVE "LL, AH, PP" TO F031
           ELSE IF TS-INSELIGIBLE = "M" MOVE "DL, AH, PP" TO F031
           ELSE IF TS-INSELIGIBLE = "N" MOVE "NONE" TO F031
           ELSE IF TS-INSELIGIBLE = "R" MOVE "PROP2" TO F031
           ELSE IF TS-INSELIGIBLE = "S" MOVE "LL, PP2" TO F031
           ELSE IF TS-INSELIGIBLE = "T" MOVE "DL, PP2" TO F031
           ELSE IF TS-INSELIGIBLE = "V" MOVE "AH, PP2" TO F031
           ELSE IF TS-INSELIGIBLE = "W" MOVE "LL, AH, PP2" TO F031
           ELSE IF TS-INSELIGIBLE = "X" MOVE "DL, AH, PP2" TO F031.

           MOVE TS-INSCOMP(1)  TO F04.
           MOVE TS-INSOURS(1)  TO F041.
           MOVE TS-INSEFF-DATE(1)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F042.
           MOVE TS-INSEXP-DATE(1)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F043.
           MOVE TS-INSPREM(1)  TO F044.
           MOVE TS-INSCOMM(1)  TO F045.

           MOVE TS-INSCOMP(2)  TO F05.
           MOVE TS-INSOURS(2)  TO F051.
           MOVE TS-INSEFF-DATE(2)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F052.
           MOVE TS-INSEXP-DATE(2)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F053.
           MOVE TS-INSPREM(2)  TO F054.
           MOVE TS-INSCOMM(2)  TO F055.

           MOVE TS-INSCOMP(3)  TO F06.
           MOVE TS-INSOURS(3)  TO F061.
           MOVE TS-INSEFF-DATE(3)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F062.
           MOVE TS-INSEXP-DATE(3)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F063.
           MOVE TS-INSPREM(3)  TO F064.
           MOVE TS-INSCOMM(3)  TO F065.

           MOVE TS-INSCOMP(4)  TO F07.
           MOVE TS-INSOURS(4)  TO F071.
           MOVE TS-INSEFF-DATE(4)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F072.
           MOVE TS-INSEXP-DATE(4)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F073.
           MOVE TS-INSPREM(4)  TO F074.
           MOVE TS-INSCOMM(4)  TO F075.

           MOVE TS-INSCOMP(5)  TO F08.
           MOVE TS-INSOURS(5)  TO F081.
           MOVE TS-INSEFF-DATE(5)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F082.
           MOVE TS-INSEXP-DATE(5)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F083.
           MOVE TS-INSPREM(5)  TO F084.
           MOVE TS-INSCOMM(5)  TO F085.

           MOVE TS-INSCOMP(6)  TO F09.
           MOVE TS-INSOURS(6)  TO F091.
           MOVE TS-INSEFF-DATE(6)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F092.
           MOVE TS-INSEXP-DATE(6)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F093.
           MOVE TS-INSPREM(6)  TO F094.
           MOVE TS-INSCOMM(6)  TO F095.

           MOVE TS-INSCOMP(7)  TO F10.
           MOVE TS-INSOURS(7)  TO F101.
           MOVE TS-INSEFF-DATE(7)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F102.
           MOVE TS-INSEXP-DATE(7)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F103.
           MOVE TS-INSPREM(7)  TO F104.
           MOVE TS-INSCOMM(7)  TO F105.

           MOVE TS-INSCOMP(8)  TO F11.
           MOVE TS-INSOURS(8)  TO F111.
           MOVE TS-INSEFF-DATE(8)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F112.
           MOVE TS-INSEXP-DATE(8)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F113.
           MOVE TS-INSPREM(8)  TO F114.
           MOVE TS-INSCOMM(8)  TO F115.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************

           MOVE SAVE-BRNO        TO D-BRANCH.
           MOVE LPWSTRS-TS-DATE  TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO D-MM.
           MOVE DATE-YYYYMMDD-YY TO D-YY.

           MOVE DATE-YYYYMMDD-DD      TO D-DAY.
           MOVE LPWSTRS-TS-BRNO       TO D-BRNO.
           MOVE "S"                   TO D-CODE.
           MOVE LPWSTRS-TS-CLASS      TO D-CLASS.
           MOVE LPWSTRS-TS-ACCTNO     TO D-ACCTNO.
           MOVE LPWSTRS-TS-LAWCODE    TO D-LAWCODE.
           MOVE LPWSTRS-TS-LINE       TO D-LINE.

           MOVE KEY-BUF TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************************************
       FORM-RESET SECTION.
      ********************************************************************
           MOVE EXT-CONAME-CONAME TO TRSMN2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRSMN2-SYSDATE.
           DISPLAY SPACES UPON TRSMIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRSMN2-SCREEN UPON TRSMIN-WINDOW-HANDLE.
           MOVE TRSMN2-FORM-FIELDS TO WR-BUF.
           MOVE TRSMN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************************
           MOVE TRSMN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRSMN2-HELP--DIR TO HELP-LINK-DIR.

      ********************************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRSMN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       DISPLAY-SCAN-KEY SECTION.
      ******************************************************************

           MOVE "SNNN040BRANCH." TO VDU
           MOVE BR-NO TO VDUNUM0   SAVE-BRNO.
           PERFORM SCREENIO.

           MOVE LPWSTRS-TS-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO DATE-MMYY-MM.
           MOVE DATE-YYYYMMDD-YY TO DATE-MMYY-YY.
           MOVE "SNNN040MMYY." TO VDU.
           MOVE DATE-MMYY      TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020DAY." TO VDU.
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

           MOVE "SNNN060NUMBER." TO VDU.
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
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

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
           DESTROY TRSMN2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE TRSMIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
