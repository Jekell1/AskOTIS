       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRLMN4.
       DATE-WRITTEN.    05-20-2004.
      ******************************************************************
      *   TRANSACTION LOAN FILE MAINTENANCE
      *              PAGE 4
      *
      * REV:
      *  JTG 040520 *** NEW ***
      * MJD 131203 A30 CONVERSION COMPLETED
      *  CS 160922 REMOVED TL-SEQNO      
      *  CS 160922 REMOVED TL-SEQNO
      *            MOVE HOLD-KEY TO HOLD-TRL-KEY, SAVEKEY X(14) TO X(27)
      *            REMOVED ENTER-SEQNO AND REFERENCES TO TRL-SEQ
      *            CHANGED ORIGINAL/CHANGED-REC TO
      *                    ORIGINAL-TRL-REC/CHANGED-TRL-REC
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *         ADDED LIBLP/LPFRTR; REMOVED LIBLP/LPTRTMPW.
      *         ADDED LPFRTR-UPDATE-CHGLOG; REMOVED WRITE-TR-CHANGE-LOG.
      *
      * BAH 180215 ADDED 2. TL-DEBIT-CARD-AMT NETSPEND WORLD PR#1306 
      * BAH 2019.0508 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      * BLM 2021.1109 SAVE-MMYY ALWAYS SPACES, CAUSES CAST ERRORS,
      *               POPULATE WITH VALUES IN SAVEKEY
      * BAH 20220420 ADDED TP-ACTIONCD2, SECURITIZATION #1551
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/TRLMN4 S35 05/25/22-08:27:51 >".

      ******************************************************************
      *    *      PROGRAM WORKERS
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *-----------------------------------------------------------------
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
      *-----------------------------------------------------------------
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

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
       01  NEW-FLAG        PIC 9.

       01  SAVE-BRNO       PIC 9(4).
       01  SAVE-MMYY       PIC 9(4).
       01  SAVE-MMYYX REDEFINES SAVE-MMYY.
           03  SAVE-MM     PIC 99.
           03  SAVE-YY     PIC 99.

       01  ORIGINAL-TRL-REC    PIC X(TRL-SIZE).
       01  CHANGED-TRL-REC     PIC X(TRL-SIZE).

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


      ******************************************************************
      *    *      SCREEN DISPLAY BUFFER
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC Z,ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  F02         PIC Z(7).99-.
           03  F03         PIC X(4).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
           "010 020 030.".

       78  MAX             VALUE 03.

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS
      ******************************************************************
       01  SPEC-TABLE.
      *01.  PRIOR DLDISC UNEARNED (LOAN COSTS)
            03  FILLER      PIC X(8) VALUE "E NS072 ".
      *02.  TL-DEBIT-CARD-AMT
            03  FILLER      PIC X(8) VALUE "E NS072 ".
      *03.  TL-ACTIONCD2
            03  FILLER      PIC X(8) VALUE "E  A040N".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS MAX TIMES.
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
           03  D-SEQNO     PIC 99.
           03  D-LAWCODE   PIC 99.
           03  D-LINE      PIC 99.
       01  KEY-LIST.
           03  FILLER      PIC X(39)   VALUE
               "BRANCH MMYY DAY BRNO CODE CLASS NUMBER ".
           03  FILLER      PIC X(13)   VALUE
               "LAWCODE LINE.".

      ******************************************************************
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/TRLSCNW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TRLMN4_DEF.CPY".
       COPY "LIBMN/TRLMN4_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/TRLMN4_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N   M O D U L E
      *
      ******************************************************************
        MAIN-PROGRAM SECTION.

        COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TRLMN4" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO TRLMIN-LINK.
           MOVE SAVEKEY-MM TO SAVE-MM.
           MOVE SAVEKEY-YY TO SAVE-YY.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           IF SAVEKEY NOT = " "
              MOVE "ON" TO ACTIVE-FILE-SW.

            MOVE "OFF" TO LEAVE-BRANCH-MMYY-SW.

        MAIN-MODULE.
      D     STOP MESS.
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
       ENTER-SAVE-BRANCH.
           MOVE "ON" TO LEAVE-BRANCH-MMYY-SW.
           MOVE 0    TO NEW-FLAG.

           IF ( ACTIVE-FILE-SW NOT = "ON" )
              GO TO ENTER-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET KEY THEN DISPLAY IF KEY FOR TRFILE EXISTS OTHERWISE
      *    FORCE USER TO ENTER KEY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *
      *     E N T E R    K E Y S   M O D U L E
      *
      ******************************************************************
       ENTER-KEYS-MODULE SECTION.
       ENTER-DAY.
           MOVE "F6 - SCAN"    TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPACES         TO IO-TYPE.

           MOVE 0 TO LPWSTRL-TL-DATE LPWSTRL-TL-BRNO
                     LPWSTRL-TL-CLASS LPWSTRL-TL-ACCTNO
                     LPWSTRL-TL-LAWCODE LPWSTRL-TL-LINE.
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
              MOVE SAVE-BRNO  TO TRLSCN-BRNO
              MOVE SAVE-MMYY  TO TRLSCNW-MMYY
              MOVE "Y"        TO TRLSCN-RETURN
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
              MOVE "X" TO ERRCD
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DO *NOT* SET FILPATH-WORKERS FROM THIS READ-BR-FILE.
      *    THIS BRNO MAYBE DIFFERENT FROM THE ONE USED FOR THE
      *    PREVIOUS READ-BR-FILE USED TO GET TRANS-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S NA010CODE." TO VDU.
           MOVE "L"            TO VDUBUF.
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

           MOVE VDUNUM0 TO LPWSTRL-TL-CLASS.

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

           MOVE VDUNUM0 TO LPWSTRL-TL-ACCTNO.

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

           MOVE VDUNUM0 TO LPWSTRL-TL-LAWCODE.

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

           MOVE VDUNUM0 TO LPWSTRL-TL-LINE.

      *-----------------------------------------------------------------
       READ-TRANS-FILE.

           PERFORM OPEN-TRL1-FILE.
           MOVE LPWSTRL-TRL1-KEY TO TRL1-KEY.
           PERFORM READ-TRL1-FILE.
           IF IO-FG NOT = 0
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           MOVE TRL-REC TO ORIGINAL-TRL-REC.

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
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.

      *-----------------------------------------------------------------
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
              IF FORM-SEL1 < 0 OR >= 4
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
       TEST-F6.
           IF VDUSTATUS = "1<"
              IF ELE = 3
                 MOVE "A2" TO CDSCAN-TYPE
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    MOVE 1 TO ONE
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE.

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
           GO TO F-01 F-02 F-03
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      ****************************************************************
      *    *      SET RECORD ELEMENTS
      ****************************************************************
       F-01.
           MOVE VDUNUM2  TO TL-PRDLDISC-UNEARN.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM2  TO TL-DEBIT-CARD-AMT.
           GO TO CREATE-ROUTINE.
       F-03.
           IF VDUBUF NOT = SPACES
              MOVE VDUBUF TO CD-CODE
              MOVE "A2"   TO CD-TYPE
              PERFORM GET-CD
              IF IO-FG NOT = 0
                 MOVE "INVALID CODE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE VDUBUF TO TL-ACTIONCD2.
           GO TO CREATE-ROUTINE.


      *-----------------------------------------------------------------
       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TRL1-FILE.
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
              PERFORM DISPLAY-FIELDS
              MOVE ORIGINAL-TRL-REC TO TRL-REC
              PERFORM DISPLAY-FIELDS
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-TRL-REC TO TRL-REC.
           PERFORM REWRITE-TRL1-FILE.

           MOVE "C"                        TO ERRLOGW-ACTION.
           MOVE TRL1-KEY                   TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME              TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

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
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE TL-PRDLDISC-UNEARN   TO F01.
           MOVE TL-DEBIT-CARD-AMT    TO F02.
           MOVE TL-ACTIONCD2         TO F03.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************

           MOVE SAVE-BRNO        TO D-BRANCH.
           MOVE LPWSTRL-TL-DATE  TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO D-MM.
           MOVE DATE-YYYYMMDD-YY TO D-YY.

           MOVE DATE-YYYYMMDD-DD      TO D-DAY.
           MOVE LPWSTRL-TL-BRNO       TO D-BRNO.
           MOVE "L"                   TO D-CODE.
           MOVE LPWSTRL-TL-CLASS      TO D-CLASS.
           MOVE LPWSTRL-TL-ACCTNO     TO D-ACCTNO.
           MOVE LPWSTRL-TL-LAWCODE    TO D-LAWCODE.
           MOVE LPWSTRL-TL-LINE       TO D-LINE.

           MOVE KEY-BUF  TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************************************
       CODE-SCAN SECTION.
      *****************************************************************

           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A040   ." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0    TO VDUCOL.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.
       EXIT-CODE-SCAN.
           EXIT.

      ********************************************************
       FORM-RESET SECTION.
      ********************************************************
           MOVE EXT-CONAME-CONAME TO TRLMN4-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRLMN4-SYSDATE.
           DISPLAY SPACES UPON TRLMIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRLMN4-SCREEN UPON TRLMIN-WINDOW-HANDLE.
           MOVE TRLMN4-FORM-FIELDS TO WR-BUF.
           MOVE TRLMN4-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
           MOVE TRLMN4-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRLMN4-HELP--DIR TO HELP-LINK-DIR.

      ********************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRLMN4_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    D I S P L A Y - S C A N - K E Y 
      *
      ******************************************************************
       DISPLAY-SCAN-KEY SECTION.

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

           MOVE "S NA010CODE." TO VDU.
           MOVE "L"            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN020CLASS." TO VDU.
           MOVE LPWSTRL-TL-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060NUMBER." TO VDU.
           MOVE LPWSTRL-TL-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LAWCODE." TO VDU.
           MOVE LPWSTRL-TL-LAWCODE TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LINE." TO VDU.
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

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPTRL1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           MOVE BR-NO TO SAVEBRNO.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN |MJD001
           DESTROY TRLMN4-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE TRLMIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
