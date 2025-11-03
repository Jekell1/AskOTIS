       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRBMN3.
       DATE-WRITTEN.    04-24-95.
      ******************************************************************
      *   TRANSACTION BRACH TRANSFER FILE MAINTENANCE
      *              PAGE 3
      *
      *
      * REV:
      *  MJD 050995 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *  MJD 062995 ADDED TX-USERID, TX-POSTDATE, TX-POSTTIME.
      *             SET NO RESET ON ALL DISPLAY FIELDS.
      *  MJD 111396 MOVED "B" TO TRSCAN-TYPE.
      *  JTG 102297 CHANGED FIELD 6 TO BE DYNAMIC PROMPT,
      *               FROM BRANCH WILL DISPLAY '6. TO ACCT:'
      *               TO   BRANCH WILL DISPLAY '6. FR ACCT:'
      * MJD 131204 A30 CONVERSION COMPLETED
      *  CS 161003 REMOVED D-SEQNO FROM KEY-BUF,
      *            CHANGED HOLD-TRB-KEY TO HOLD-TRB-KEY, HOLD-FIELDS TO HOLD-TB
      *            ORIGINAL/CHANGED REC TO ORIGINAL-TR-REC/CHANGED-TR-REC
      *            REMOVED HARD CODE OF SIZE OF THESE.
      *            REMOVED ANY REFERENCE TO SEQNO
      *            CHANGED SAVEKEY FROM X(14) TO X(27)
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *               REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *         ADDED LIBLP/LPFRTR; REMOVED LIBLP/LPTRTMPW.
      *         ADDED LPFRTR-UPDATE-CHGLOG; REMOVED WRITE-TR-CHANGE-LOG.
      *  CS 2017.0508 ACTIVATE 8 DIGIT DATES
      * BAH 2019.0507 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      * BLM 2021.1115 SAVE-MMYY ALWAYS SPACES, CAUSES CAST ERRORS,
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
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/TRBMN3.CBL S34 11/15/21-11:25:44 >".

       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *-----------------------------------------------------------------
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
           88 INVALID-ELE  VALUE 5 33.
       01  OVER-20-ELE     PIC 9(2).

       01  ACTIVE-FILE-SW        PIC X(3)    VALUE "OFF".
       01  LEAVE-BRANCH-MMYY-SW  PIC X(3)    VALUE "OFF".

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
           03  CHNO-B      REDEFINES CHNO
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

       01  SOCSEC-KEY      PIC 9(9).
       01  SS-KEY REDEFINES SOCSEC-KEY.
           03  SS-1        PIC 999.
           03  SS-2        PIC 99.
           03  SS-3        PIC 9999.

       01  WK-MAKERCD       PIC X            VALUE " ".
           88  GOOD-MAKERCD                  VALUE  " " "M" "J" "E".

       01  ORIGINAL-TRB-REC    PIC X(TRB-SIZE).
       01  CHANGED-TRB-REC     PIC X(TRB-SIZE).

       01  TRBMIN-LINK VALUE SPACES.
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
           03  TRBMIN-WINDOW-HANDLE PIC X(10).


      ******************************************************************
      *
      *    SCREEN DISPLAY BUFFER
      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC 999/99/9999     BLANK WHEN ZERO.
           03  F011        PIC X.
           03  F02         PIC 999/99/9999     BLANK WHEN ZERO.
           03  F021        PIC X.
           03  F03         PIC 999/99/9999     BLANK WHEN ZERO.
           03  F031        PIC X.
           03  F04         PIC 999/99/9999     BLANK WHEN ZERO.
           03  F041        PIC X.
           03  F05         PIC X.
           03  F06         PIC Z(8)            BLANK WHEN ZERO.
           03  F07         PIC ZZZ9.99         BLANK WHEN ZERO.
           03  F08         PIC ZZ9.99-         BLANK WHEN ZERO.
           03  F09         PIC ZZ9             BLANK WHEN ZERO.
           03  F10         PIC 99/99/99        BLANK WHEN ZERO.
           03  F11         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F12         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F13         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F14         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F15         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F16         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F17         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F18         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F19         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F20         PIC ZZ,ZZZ,ZZ9.99-  BLANK WHEN ZERO.
           03  F21         PIC ZZ,ZZZ,ZZ9.99-  BLANK WHEN ZERO.
           03  F22         PIC ZZ,ZZZ,ZZ9.99-  BLANK WHEN ZERO.
           03  F23         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F24         PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  F25         PIC X(10).
           03  F26         PIC 9(6)            BLANK WHEN ZERO.
           03  F27         PIC 99/99/99        BLANK WHEN ZERO.
           03  F28         PIC ZZ,ZZZ,ZZ9.99-  BLANK WHEN ZERO.
           03  F29         PIC ZZ,ZZZ,ZZ9.99-  BLANK WHEN ZERO.
           03  F30         PIC ZZ,ZZZ,ZZ9.99-  BLANK WHEN ZERO.
           03  F31         PIC ZZ,ZZZ,ZZ9.99-  BLANK WHEN ZERO.
           03  F32         PIC ZZ,ZZZ,ZZ9.99-  BLANK WHEN ZERO.
           03  F33         PIC ZZ,ZZZ,ZZ9.99-  BLANK WHEN ZERO.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
           "010 011 020 021 030 031 040 041 050 060 ".
           03  FILLER      PIC X(40) VALUE
           "070 080 090 100 110 120 130 140 150 160 ".
           03  FILLER      PIC X(40) VALUE
           "170 180 190 200 210 220 230 240 250 260 ".
           03  FILLER      PIC X(28) VALUE
           "270 280 290 300 310 320 330.".

       01  MAX             PIC 99   VALUE 33.

      ******************************************************************
      *
      *    ELEMENT SPECIFICATIONS
      *
      ******************************************************************
       01  SPEC-TABLE.
      * 1.  SOC SEC NUMBER 1
            03  FILLER      PIC X(8) VALUE "ERNX090 ".
      * 11. MAKER CODE 1
      *     03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 2.  SOC SEC NUMBER 2
            03  FILLER      PIC X(8) VALUE "ERNX090 ".
      * 21. MAKER CODE 2
      *     03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 3.  SOC SEC NUMBER 3
            03  FILLER      PIC X(8) VALUE "ERNX090 ".
      * 31. MAKER CODE 3
      *     03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 4.  SOC SEC NUMBER 4
            03  FILLER      PIC X(8) VALUE "ERNX090 ".
      * 41. MAKER CODE 4
      *     03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 5.  ********** NOT IN USE *****************
            03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 6.  PRLNNO
            03  FILLER      PIC X(8) VALUE "ENNN080 ".
      * 7.  DLNO
            03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 8.  MAINTENANCE FEE
            03  FILLER      PIC X(8) VALUE "E NS032 ".
      * 9.  ORGTERM
            03  FILLER      PIC X(8) VALUE "E NN030 ".
      *10.  LOAN DATE
            03  FILLER      PIC X(8) VALUE "E NM060 ".
      *11.  DLPARV
            03  FILLER      PIC X(8) VALUE "E NS072 ".
      *12.  DLRV
            03  FILLER      PIC X(8) VALUE "E NS072 ".
      *13.  DLSPRV
            03  FILLER      PIC X(8) VALUE "E NS072 ".
      *14.  UNEARN 1
            03  FILLER      PIC X(8) VALUE "E NS072 ".
      *15.  UNEARN 2
            03  FILLER      PIC X(8) VALUE "E NS072 ".
      *16.  UNEARN 3
            03  FILLER      PIC X(8) VALUE "E NS072 ".
      *17.  UNEARN 4
            03  FILLER      PIC X(8) VALUE "E NS072 ".
      *18.  UNEARN 5
            03  FILLER      PIC X(8) VALUE "E NS072 ".
      *19.  UNEARN 6
            03  FILLER      PIC X(8) VALUE "E NS072 ".
      *20.  DLRVPRIN 1
            03  FILLER      PIC X(8) VALUE "E NS082 ".
      *21.  DLRVPRIN 2
            03  FILLER      PIC X(8) VALUE "E NS082 ".
      *22.  DLRVPRIN 3
            03  FILLER      PIC X(8) VALUE "E NS082 ".
      *23.  EXTENSION CHARGE
            03  FILLER      PIC X(8) VALUE "E NS052 ".
      *24.  SERVICE CHARGE
            03  FILLER      PIC X(8) VALUE "E NS052 ".
      *25.  POSTING USER
            03  FILLER      PIC X(8) VALUE "E NA100 ".
      *26.  POST TIME
            03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *27.  POST DATE
            03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *28.  PARVPAID
            03  FILLER      PIC X(8) VALUE "E NS082 ".
      *29.  PARVHELD
            03  FILLER      PIC X(8) VALUE "E NS082 ".
      *30.  DEALER RESERVE CHARGES #1
            03  FILLER      PIC X(8) VALUE "E NS082 ".
      *31.  DEALER RESERVE CHARGES #2
            03  FILLER      PIC X(8) VALUE "E NS082 ".
      *32.  DEALER RESERVE CHARGES #3
            03  FILLER      PIC X(8) VALUE "E NS082 ".
      *33.  ***********NOT IN USE******************
            03  FILLER      PIC X(8) VALUE "E NS082 ".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 36 TIMES.
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
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/TRBSCNW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TRBMN3_DEF.CPY".
       COPY "LIBMN/TRBMN3_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/TRBMN3_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N   M O D U L E
      *
      ******************************************************************
        MAIN-PROGRAM SECTION.

      D     STOP MESS.

        COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TRBMN3"      TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO TRBMIN-LINK.
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
               MOVE "N" TO NEW-REC
               MOVE "P" TO FORM-P1
               MOVE 1 TO FORM-SEL1.

            IF FORM-SEL1 NOT = 0
               IF FORM-P1 = "P"
                  MOVE BR-NO           TO SAVEBRNO
                  MOVE LPWSTRB-TRB1-KEY TO SAVEKEY
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

           MOVE SAVEKEY  TO LPWSTRB-TRB1-KEY TRB1-KEY.
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

           PERFORM OPEN-TRB1-FILE.

       EXIT-BRANCH-MMYY-MODULE.
           EXIT.

      ******************************************************************
       ENTER-KEYS-MODULE SECTION.
      ******************************************************************
       ENTER-DAY.
           MOVE "F6 - SCAN"    TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPACES         TO IO-TYPE.

           MOVE 0 TO LPWSTRB-TB-DATE LPWSTRB-TB-BRNO
                     LPWSTRB-TB-CLASS LPWSTRB-TB-ACCTNO
                     LPWSTRB-TB-LAWCODE LPWSTRB-TB-LINE.
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
              MOVE SAVE-BRNO  TO TRBSCN-BRNO
              MOVE SAVE-MMYY  TO TRBSCNW-MMYY
              MOVE "Y"        TO TRBSCN-RETURN
              MOVE "WI/TRBSCN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH TRBSCN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF TRBSCN-STATUS NOT = "00"
                 GO TO ENTER-DAY
              ELSE
                 MOVE TRBSCN-TRB1-KEY TO LPWSTRB-TRB1-KEY
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

           MOVE DATE-YYYYMMDD TO LPWSTRB-TB-DATE.

      *-----------------------------------------------------------------
       ENTER-BRNO.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRNO." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO LPWSTRB-TB-BRNO
                                      TB-BRNO.
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

      *  DO *NOT* SET FILPATH-WORKERS FROM THIS READ-BR-FILE.
      * THIS BRNO MAYBE DIFFERENT FROM THE ONE USED FOR THE
      * PREVIOUS READ-BR-FILE USED TO GET TRANS-PATH.
      *    MOVE VDUNUM0 TO BR-NO LPWSTRB-TB-BRNO TB-BRNO.

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
           MOVE "B"            TO VDUBUF.
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

           MOVE VDUNUM0 TO LPWSTRB-TB-CLASS.

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

           MOVE VDUNUM0 TO LPWSTRB-TB-ACCTNO.

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

           MOVE VDUNUM0 TO LPWSTRB-TB-LAWCODE.

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

           MOVE VDUNUM0 TO LPWSTRB-TB-LINE.

      *-----------------------------------------------------------------
       READ-TRANS-FILE.
           PERFORM OPEN-TRB1-FILE.
           MOVE LPWSTRB-TRB1-KEY TO TRB1-KEY.
           PERFORM READ-TRB1-FILE.
           IF IO-FG NOT = 0
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           MOVE TRB-REC TO ORIGINAL-TRB-REC.

           IF NEW-REC = "Y"
              MOVE "S  A000NEW."        TO VDU
              MOVE "*** NEW RECORD ***" TO VDUBUF
              PERFORM SCREENIO
              MOVE 1 TO NEW-FLAG
           ELSE
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG.

      * DISPLAY DYNAMIC PROMPT FOR FIELD 060:
           MOVE "S  A110P060:S." TO VDU.
           IF TB-REF-FRTO = "F"
              MOVE "6. FR ACCT:" TO VDUBUF
           ELSE
              MOVE "6. TO ACCT:" TO VDUBUF.
           PERFORM SCREENIO.

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
              IF FORM-SEL1 < 0 OR >= 3
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF CHNO-B NOT NUMERIC
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
                 F-31 F-32 F-33
                 DEPENDING ON OVER-20-ELE.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS
      *
      *-----------------------------------------------------------------
       F-01.
           MOVE VDUNUM0 TO SOCSEC-KEY.

           IF SS-KEY IS NOT NUMERIC
              GO TO ENTER-ELE.

           MOVE SOCSEC-KEY TO TB-SSNO(1).


      *-----------------------------------------------------------------
       F-011.
           MOVE "E  A010011." TO VDU.
           MOVE ELE           TO VDUROW.
           MOVE 1             TO VDUCOL.
           MOVE "."           TO VDUDECIMAL.
           PERFORM SCREENIO.
           MOVE VDUBUF        TO WK-MAKERCD.

           IF NOT GOOD-MAKERCD
              MOVE "ENTER: 'M' 'J' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO F-011.

           MOVE VDUBUF     TO TB-MAKERCD(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUNUM0 TO SOCSEC-KEY.

           IF SS-KEY IS NOT NUMERIC
              GO TO ENTER-ELE.

           MOVE SOCSEC-KEY    TO TB-SSNO(2).

      *-----------------------------------------------------------------
       F-021.
           MOVE "E  A010021." TO VDU.
           MOVE ELE           TO VDUROW.
           MOVE 1             TO VDUCOL.
           MOVE "."           TO VDUDECIMAL.
           PERFORM SCREENIO.
           MOVE VDUBUF        TO WK-MAKERCD.

           IF NOT GOOD-MAKERCD
              MOVE "ENTER: 'M' 'J' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO F-021.

           MOVE VDUBUF        TO TB-MAKERCD(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUNUM0 TO SOCSEC-KEY.

           IF SS-KEY IS NOT NUMERIC
              GO TO ENTER-ELE.

           MOVE SOCSEC-KEY TO TB-SSNO(3).

      *-----------------------------------------------------------------
       F-031.
           MOVE "E  A010031." TO VDU.
           MOVE ELE           TO VDUROW.
           MOVE 1             TO VDUCOL.
           MOVE "."           TO VDUDECIMAL.
           PERFORM SCREENIO.
           MOVE VDUBUF        TO WK-MAKERCD.

           IF NOT GOOD-MAKERCD
              MOVE "ENTER: 'M' 'J' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO F-031.

           MOVE VDUBUF        TO TB-MAKERCD(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUNUM0 TO SOCSEC-KEY.

           IF SS-KEY IS NOT NUMERIC
              GO TO ENTER-ELE.

           MOVE SOCSEC-KEY TO TB-SSNO(4).

      *-----------------------------------------------------------------
       F-041.
           MOVE "E  A010041." TO VDU.
           MOVE ELE           TO VDUROW.
           MOVE 1             TO VDUCOL.
           MOVE "."           TO VDUDECIMAL.
           PERFORM SCREENIO.
           MOVE VDUBUF     TO WK-MAKERCD.

           IF NOT GOOD-MAKERCD
              MOVE "ENTER: 'M' 'J' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO F-041.

           MOVE VDUBUF     TO TB-MAKERCD(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUNUM0 TO TB-REF-ACCTNO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUNUM2 TO TB-DLNO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUNUM2 TO TB-MAINTFEE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUNUM0 TO TB-ORGTERM.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDU-DATE-YYYYMMDD TO TB-LOANDATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUNUM2 TO TB-DLPARV.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDUNUM2 TO TB-DLRV.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           MOVE VDUNUM2 TO TB-DLSPRV.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUNUM2 TO TB-UNEARN(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUNUM2 TO TB-UNEARN(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUNUM2 TO TB-UNEARN(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUNUM2 TO TB-UNEARN(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           MOVE VDUNUM2 TO TB-UNEARN(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
           MOVE VDUNUM2 TO TB-UNEARN(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           MOVE VDUNUM2 TO TB-DLRVPRIN(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           MOVE VDUNUM2 TO TB-DLRVPRIN(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           MOVE VDUNUM2 TO TB-DLRVPRIN(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           MOVE VDUNUM2  TO TB-EXTCHG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           MOVE VDUNUM2  TO TB-SERCHG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           MOVE VDUBUF TO TB-USERID.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           MOVE VDUNUM0 TO TB-POSTTIME.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-27.
           MOVE VDU-DATE-YYYYMMDD TO TB-POSTDATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-28.
           MOVE VDUNUM2  TO TB-DLPARVPAID.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-29.
           MOVE VDUNUM2  TO TB-DLPARVHELD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-30.
           MOVE VDUNUM2  TO TB-DLRVCHGS(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-31.
           MOVE VDUNUM2  TO TB-DLRVCHGS(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-32.
           MOVE VDUNUM2  TO TB-DLRVCHGS(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-33.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TRB1-FILE.
           MOVE TRB-REC TO CHANGED-TRB-REC.

           IF ORIGINAL-TRB-REC = CHANGED-TRB-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-TRB1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF TRB-REC NOT = ORIGINAL-TRB-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              PERFORM DISPLAY-FIELDS
              MOVE ORIGINAL-TRB-REC TO TRB-REC
              PERFORM DISPLAY-FIELDS
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-TRB-REC TO TRB-REC.
           PERFORM REWRITE-TRB1-FILE.

           MOVE "C"                        TO ERRLOGW-ACTION.
           MOVE TRB1-KEY                   TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME              TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-WRITE-MODULE.
           PERFORM CLOSE-TRB1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-TRB1-FILE.
           PERFORM READ-TRB1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF TRB-REC NOT = ORIGINAL-TRB-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-TRB1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-TRB1-FILE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES TO DISPLAY-BUF.

           IF TB-SSNO(1) > ZERO
              MOVE TB-SSNO(1) TO F01
              INSPECT F01 REPLACING ALL "/" BY "-"
              MOVE TB-MAKERCD(1)  TO F011.

           IF TB-SSNO(2) > ZERO
              MOVE TB-SSNO(2) TO F02
              INSPECT F02 REPLACING ALL "/" BY "-".
              MOVE TB-MAKERCD(2)  TO F021.

           IF TB-SSNO(3) > ZERO
              MOVE TB-SSNO(3) TO F03
              INSPECT F03 REPLACING ALL "/" BY "-"
              MOVE TB-MAKERCD(3)  TO F031.

           IF TB-SSNO(4) > ZERO
              MOVE TB-SSNO(4) TO F04
              INSPECT F04 REPLACING ALL "/" BY "-"
              MOVE TB-MAKERCD(4)  TO F041.

      *----------------------->TO F05.
           MOVE TB-REF-ACCTNO      TO F06.
           MOVE TB-DLNO        TO F07.
           MOVE TB-MAINTFEE    TO F08.
           MOVE TB-ORGTERM     TO F09.
           MOVE TB-LOANDATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO F10.
           MOVE TB-DLPARV      TO F11.
           MOVE TB-DLRV        TO F12.
           MOVE TB-DLSPRV      TO F13.
           MOVE TB-UNEARN(1)   TO F14.
           MOVE TB-UNEARN(2)   TO F15.
           MOVE TB-UNEARN(3)   TO F16.
           MOVE TB-UNEARN(4)   TO F17.
           MOVE TB-UNEARN(5)   TO F18.
           MOVE TB-UNEARN(6)   TO F19.
           MOVE TB-DLRVPRIN(1) TO F20.
           MOVE TB-DLRVPRIN(2) TO F21.
           MOVE TB-DLRVPRIN(3) TO F22.
           MOVE TB-EXTCHG      TO F23.
           MOVE TB-SERCHG      TO F24.
           MOVE TB-USERID      TO F25.
           MOVE TB-POSTTIME    TO F26.
           MOVE TB-POSTDATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO F27.

           MOVE TB-DLPARVPAID  TO F28.
           MOVE TB-DLPARVHELD  TO F29.
           MOVE TB-DLRVCHGS(1) TO F30.
           MOVE TB-DLRVCHGS(2) TO F31.
           MOVE TB-DLRVCHGS(3) TO F32.
      *----------------------->TO F33.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************

           MOVE SAVE-BRNO        TO D-BRANCH.
           MOVE LPWSTRB-TB-DATE  TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO D-MM.
           MOVE DATE-YYYYMMDD-YY TO D-YY.

           MOVE DATE-YYYYMMDD-DD      TO D-DAY.
           MOVE LPWSTRB-TB-BRNO       TO D-BRNO.
           MOVE "B"                   TO D-CODE.
           MOVE LPWSTRB-TB-CLASS      TO D-CLASS.
           MOVE LPWSTRB-TB-ACCTNO     TO D-ACCTNO.
           MOVE LPWSTRB-TB-LAWCODE    TO D-LAWCODE.
           MOVE LPWSTRB-TB-LINE       TO D-LINE.

           MOVE KEY-BUF TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************
       FORM-RESET SECTION.
      ******************************************************
           MOVE EXT-CONAME-CONAME TO TRBMN3-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRBMN3-SYSDATE.
           DISPLAY SPACES UPON TRBMIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRBMN3-SCREEN UPON TRBMIN-WINDOW-HANDLE.
           MOVE TRBMN3-FORM-FIELDS TO WR-BUF.
           MOVE TRBMN3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************
           MOVE TRBMN3-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRBMN3-HELP--DIR TO HELP-LINK-DIR.

      ******************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRBMN3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       DISPLAY-SCAN-KEY SECTION.
      ******************************************************************

           MOVE "SNNN040BRANCH." TO VDU
           MOVE BR-NO TO VDUNUM0   SAVE-BRNO.
           PERFORM SCREENIO.

           MOVE LPWSTRB-TB-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO DATE-MMYY-MM.
           MOVE DATE-YYYYMMDD-YY TO DATE-MMYY-YY.
           MOVE "SNNN040MMYY." TO VDU.
           MOVE DATE-MMYY      TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020DAY."    TO VDU.
           MOVE DATE-YYYYMMDD-DD TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BRNO." TO VDU.
           MOVE LPWSTRB-TB-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NA010CODE." TO VDU.
           MOVE "B"            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN020CLASS." TO VDU.
           MOVE LPWSTRB-TB-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060NUMBER." TO VDU.
           MOVE LPWSTRB-TB-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LAWCODE." TO VDU.
           MOVE LPWSTRB-TB-LAWCODE TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LINE." TO VDU.
           MOVE LPWSTRB-TB-LINE TO VDUNUM0.
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
     
           MOVE "B"                        TO TRB-PATH-OVERRIDE.
     
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
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPTRB1IN.CPY".
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
           DESTROY TRBMN3-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE TRBMIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
