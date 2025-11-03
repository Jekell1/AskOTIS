       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRCMAN.
       DATE-WRITTEN. 08-31-94
      ******************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   TRANSACTION FILE MAINTENANCE
      *                   CHECKS/CASH
      *
      *  REV:
      *   MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *   MJD 050895 ADDED TC-USERID, TC-POSTTIME, AND TC-POSTDATE.
      *              SET NO RESET ON ALL DISPLAY FIELDS.
      *   JFF 100495 ADDED TC-SPRCLASS.
      *   KEC 990929 ALLOW 'CA' FOR A TYPE ( 88  VALIDP VALUE "CA" )
      *              ALLOW 'C' FOR A PAID TYPE
      *               THIS PER CINDY!!!
      *   BLV 030402 ADD DD (DRAWER DEPOSIT) TO LIST OF VALID TYPES,
      *              WORLD PR# 320
      *   BLF 061015 ADD FIELD #27, TC-PAYEE-NO, HOME PR# 3078
      *   BLF 061015 ADD FIELD #28, TC-ACH-FG, REGACC PR# 185
      *   BAH 102114 ADD FIELD #29, TC-TILLNO, CFSC PL#863
      *    CS 150331 CS DUPLICATE TRSCAN(NP/WI)CHANGED NP TO TRSCAN
      *    CS 161003 CHANGED HOLD-KEY TO HOLD-TRC-KEY, HOLD-FIELDS TO HOLD-TC
      *            ORIGINAL/CHANGED REC TO ORIGINAL-TR-REC/CHANGED-TR-REC
      *            REMOVED HARD CODE OF SIZE OF THESE.
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *         ADDED LIBLP/LPFRTR; REMOVED LIBLP/LPTRTMPW.
      *         ADDED LPFRTR-UPDATE-CHGLOG; REMOVED WRITE-TR-CHANGE-LOG.
      *
      * BAH 2016.1103 CHANGE TILLNO TO 9(2)
      *
      * KEC 2017.0110 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED WK-VOIDE-DATE [PIC 9(5)] SINCE NOT USED.
      *  CS 170508 ACTIVATE 8 DIGIT DATES
      * BAH 2019.0507 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      * BAH 20190930 REMOVED ACCESS-CALL TRCFILE
      * BAH 20210112 ADDED 30. EXTRACTED. TC-EXTRACT-FG #1486
      * BAH 20210305 ADDED 31. DEBIT REFCD. TC-DEBIT-REFCD #1490
      * BLM 20211022 FIX GL ACCT ENTRY SO YOU CAN ENTER OR NOT ENTER
      *              DASH AND LEADING ZEROES
      * BAH 20220511 ADD CLOSE-TR1-FILE AFTER THE OPEN/READ LIKE
      *              ALL OTHER MAINTENANCES DO! WAS CAUSING LOCKING
      *              ISSUES POSITIVE PAY EXTRACT. #1554
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/TRCMAN S34 11/08/21-11:47:06 >".

      ******************************************************************
      *           PROGRAM WORKERS
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

      ******************************************************************
       01  ERRCD           PIC X      VALUE " ".
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  SAVE-BRNO       PIC 9(4) VALUE 0.
       01  SAVE-MMYY       PIC 9(4) VALUE 0.
       01  SAVE-MMYYX REDEFINES SAVE-MMYY.
           03  SAVE-MM     PIC 99.
           03  SAVE-YY     PIC 99.

       01  ORIGINAL-TRC-REC    PIC X(TRC-SIZE).
       01  CHANGED-TRC-REC     PIC X(TRC-SIZE).

       01  TEST-PAYEE      PIC XX.
           88  VALIDP VALUE
               "CA" "LN" "DL" "DR" "DS" "DP" "MS" "BK" "DD".

      ******************************************************************
      *    *     SCREEN DISPLAY BUFFER      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC XX.
           03  F02         PIC 99/99/99.
           03  F03         PIC Z,ZZZ,ZZZ.99-.
           03  F04         PIC X(30).
           03  F05         PIC 9(6).
           03  F06         PIC X(30).
           03  F07         PIC X(12).
           03  F08         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F09         PIC X(12).
           03  F10         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F11         PIC X(12).
           03  F12         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F13         PIC X(12).
           03  F14         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F15         PIC X(12).
           03  F16         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F17         PIC X.
           03  F18         PIC Z(7)9  BLANK WHEN ZERO.
           03  F19         PIC X.
           03  F20         PIC XX.
           03  F21         PIC 9.
           03  F22         PIC Z(4).99     BLANK ZERO.
           03  F23         PIC X(10).
           03  F24         PIC 9(6)        BLANK ZERO.
           03  F25         PIC 99/99/99    BLANK ZERO.
           03  F26         PIC 99          BLANK ZERO.
           03  F27         PIC Z(6)        BLANK ZERO.
           03  F28         PIC X.
           03  F29         PIC 99.
           03  F30         PIC X.
           03  F31         PIC X(5).
       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
               "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(44) VALUE
               "210 220 230 240 250 260 270 280 290 300 310.".

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS
      ******************************************************************
       01  MAX             PIC 99   VALUE 31.
       01  SPEC-TABLE.
      * 1. TYPE
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      * 2. DATE
           03  FILLER      PIC X(8) VALUE "E NM060 ".
      * 3. AMT
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 4. PAYEE
           03  FILLER      PIC X(8) VALUE "E NA300 ".
      * 5. REFERENCE
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 6. REASON
           03  FILLER      PIC X(8) VALUE "E NA300 ".
      * 7. GL-1
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      * 8. GL-AMT-1
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 9. GL-2
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      *10. GL-AMT-2
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *11. GL-3
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      *12. GL-AMT-3
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *13. GL-4
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      *14. GL-AMT-4
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *15. GL-5
           03  FILLER      PIC X(8) VALUE "E NA120 ".
      *16. GL-AMT-5
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      *17. VOID
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *18. ACCTNO
           03  FILLER      PIC X(8) VALUE "ENNN080 ".
      *19. PAID
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *20. PGM
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *21. DEALER RESERVE NUMBER
           03  FILLER      PIC X(8) VALUE "E NN010 ".
      *22. DEALER NUMBER
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      *23. POSTING USERID
           03  FILLER      PIC X(8) VALUE "E NA100 ".
      *24. POSTTIME
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *25. POSTDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *26. SPRCLASS
           03  FILLER      PIC X(8) VALUE "E NN020 ".
      *27. PAYEE #
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *28. ACH FLAG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *29. TILL (DRAWER)
           03  FILLER      PIC X(8) VALUE "E NN020 ".
      *30. EXTRACTED FLAG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *31. DEBIT REFCD
           03  FILLER      PIC X(8) VALUE "E NA050 ".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 31 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      *
       01  KEY-BUF.
           03  D-BRANCH    PIC ZZZ9.
           03  D-MMYY.
               05  D-MM    PIC 99.
               05  D-YY    PIC 99.
           03  D-DAY       PIC 99.
           03  D-BRNO      PIC ZZZ9.
           03  D-CODE      PIC X.
           03  D-CKNO      PIC Z(5)9.
           03  D-CLASS     PIC Z9.
           03  D-SEQ       PIC ZZZ9.
       01  KEY-LIST.
           03  FILLER      PIC X(41) VALUE
               "BRANCH MMYY DAY BRNO CODE CKNO CLASS SEQ.".

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

       COPY "LIBWI/TRCSCNW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       01 TRCMAN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TRCMAN_DEF.CPY".
       COPY "LIBMN/TRCMAN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/TRCMAN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N    P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "TRCMAN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS TRCMAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       MAIN-MODULE.

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
              GO TO END-ROUTINE.

       ENTER-MONTH.
           MOVE "ENNN040MMYY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MONTH.

           MOVE VDUNUM0 TO SAVE-MMYY.
           IF SAVE-MM < 1 OR > 12
              MOVE "INVALID MMYY, PLEASE RE-ENTER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-MONTH.

       RESTART-MAINT.
           PERFORM FORM-RESET.

           PERFORM ENTER-KEYS-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO RESTART-MAINT.

      ******************************************************************
       ENTER-KEYS-MODULE SECTION.
      ******************************************************************
       ENTER-DAY.
           MOVE "F6 - SCAN" TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPACES TO IO-TYPE.

           MOVE 0 TO LPWSTRC-TC-KEY-DATE LPWSTRC-TC-BRNO
                     LPWSTRC-TC-CLASS LPWSTRC-TC-CKNO
                     LPWSTRC-TC-SEQ.

           MOVE 0 TO NEW-FLAG.

           MOVE "ENNN020DAY." TO VDU.
           PERFORM SCREENIO.

           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1U"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1<"
      *       MOVE "DAY."      TO WINDOW-FIT-TO
              MOVE SAVE-BRNO   TO TRCSCN-BRNO
              MOVE SAVE-MMYY   TO TRCSCNW-MMYY
              MOVE "Y"         TO TRCSCN-RETURN
              MOVE "WI/TRCSCN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH TRCSCN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF TRCSCN-STATUS NOT = "00"
                 GO TO ENTER-DAY
              ELSE
                 MOVE TRCSCN-TRC1-KEY TO LPWSTRC-TRC1-KEY
                 PERFORM DISPLAY-KEY
                 GO TO READ-TRANS-FILE.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DAY.

           MOVE VDUNUM0 TO DATE-MMDDYY-DD.
           MOVE SAVE-MM TO DATE-MMDDYY-MM.
           MOVE SAVE-YY TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.

           MOVE DATE-YYYYMMDD TO LPWSTRC-TC-KEY-DATE.

      *----------------------------------------------------------------
       ENTER-BRNO.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRNO." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO LPWSTRC-TC-BRNO
                                      TC-BRNO.
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
      *       MOVE "END" TO IO-TYPE
      *       GO TO END-ENTER-KEYS-MODULE.

      *    IF VDUSTATUS NOT = "00"
      *       GO TO ENTER-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DO *NOT* SET FILPATH-WORKERS FROM THIS READ-BR-FILE.
      *    THIS BRNO MAYBE DIFFERENT FROM THE ONE USED FOR THE
      *    PREVIOUS READ-BR-FILE USED TO GET TRANS-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE VDUNUM0 TO BR-NO LPWSTRC-TC-BRNO TC-BRNO.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           MOVE "S NA200BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S NA010CODE." TO VDU.
           MOVE "C"            TO VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-CKNO.
           MOVE "ENNN060CKNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-DAY.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CKNO.

           MOVE VDUNUM0 TO LPWSTRC-TC-CKNO.

      *----------------------------------------------------------------
       ENTER-CLASS.
           MOVE "ENNN020CLASS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-CKNO.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CLASS.

           MOVE VDUNUM0 TO LPWSTRC-TC-CLASS.

      *----------------------------------------------------------------
       ENTER-SEQ.
           MOVE "ENNN040SEQ." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-CLASS.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SEQ.

           MOVE VDUNUM0 TO LPWSTRC-TC-SEQ.

      *----------------------------------------------------------------
       READ-TRANS-FILE.
           PERFORM OPEN-TRC1-FILE.
           MOVE LPWSTRC-TRC1-KEY TO TRC1-KEY.
           PERFORM READ-TRC1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           IF IO-FG = 0
              MOVE TRC-REC TO ORIGINAL-TRC-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-ENTER-KEYS-MODULE.
           EXIT.

      *****************************************************
      *    E N T R Y    M O D U L E
      *
      *        NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                   1 - INITIAL NEW RECORD ENTRY
      *                   2 - MODIFY NEW RECORD
      *****************************************************
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

      *----------------------------------------------------------------
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

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *----------------------------------------------------------------
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       TEST-F7.
           IF VDUSTATUS = "1>"
             IF ELE > 6 AND < 17
                MOVE 17 TO ELE
                GO TO ENTER-ELE.

      *----------------------------------------------------------------
       TEST-F4.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *----------------------------------------------------------------
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *----------------------------------------------------------------
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *    *      SET RECORD ELEMENTS
      *****************************************************************
       F-01.
           MOVE VDUBUF TO TC-TYPE.
           MOVE TC-TYPE TO TEST-PAYEE.

           IF VALIDP
              GO TO CREATE-ROUTINE.

           PERFORM INVALID-CHECK-TYPE.
           GO TO ENTER-ELE.

      *----------------------------------------------------------------
       F-02.
           MOVE VDU-DATE-YYYYMMDD TO TC-DATE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-03.
           MOVE VDUNUM2 TO TC-AMT.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO TC-PAYEE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-05.
           MOVE VDUNUM0 TO TC-REF.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO TC-REASON.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-07.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO TC-GLNO(1).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-08.
           IF TC-GLNO(1) = 0
              MOVE 0 TO TC-GLAMT(1)
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM2 TO TC-GLAMT(1).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-09.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO TC-GLNO(2).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-10.
           IF TC-GLNO(2) = 0
              MOVE 0 TO TC-GLAMT(2)
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM2 TO TC-GLAMT(2).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-11.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO TC-GLNO(3).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-12.
           IF TC-GLNO(3) = 0
              MOVE 0 TO TC-GLAMT(3)
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM2 TO TC-GLAMT(3).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-13.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO TC-GLNO(4).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-14.
           IF TC-GLNO(4) = 0
              MOVE 0 TO TC-GLAMT(4)
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM2 TO TC-GLAMT(4).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-15.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO TC-GLNO(5).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-16.
           IF TC-GLNO(5) = 0
              MOVE 0 TO TC-GLAMT(5)
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM2 TO TC-GLAMT(5).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-17.
           MOVE VDUBUF TO TC-VOID.

           IF TC-VOID NOT = "Y" AND TC-VOID NOT = "N"
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-18.
           MOVE VDUNUM0 TO TC-ACCTNO.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-19.
           MOVE VDUBUF TO TC-PAID.

           IF TC-PAID NOT = "M"
              IF TC-PAID NOT = "B"
                 IF TC-PAID NOT = "S"
                 IF TC-PAID NOT = "C"
           MOVE "M - MANUAL, B - BATCH, S - SYSTEM" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-20.
           MOVE VDUBUF TO TC-PGM.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-21.
           MOVE VDUNUM0 TO TC-DLRVNO.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUNUM2 TO TC-DLNO.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-23.
           MOVE VDUBUF TO TC-USERID.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-24.
           MOVE VDUNUM0 TO TC-POSTTIME
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-25.
           MOVE VDU-DATE-YYYYMMDD TO TC-POSTDATE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-26.
           MOVE VDUNUM0 TO TC-SPRCLASS.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-27.
           MOVE VDUNUM0 TO TC-PAYEE-NO.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-28.
           MOVE VDUBUF TO TC-ACH-FG.

           IF TC-ACH-FG NOT = "Y" AND TC-ACH-FG NOT = "N"
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-29.
           MOVE VDUNUM0 TO TC-TILLNO.
           GO TO CREATE-ROUTINE.

       F-30.
           MOVE VDUBUF TO TC-EXTRACT-FG.
           IF TC-EXTRACT-FG NOT = "Y" AND
               TC-EXTRACT-FG NOT = "N"
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-31.
           MOVE VDUBUF TO TC-DEBIT-REFCD.

           PERFORM OPEN-CD1-FILE.
           MOVE "BR"     TO CD-TYPE.
           MOVE TC-DEBIT-REFCD TO CD-CODE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF (IO-BAD) OR (CD-BR-FILE-TYPE NOT = "C")
               MOVE "INVALID REFERENCE CODE" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TRC1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE TRC-REC TO CHANGED-TRC-REC.

           IF ORIGINAL-TRC-REC = CHANGED-TRC-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-TRC1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF TRC-REC NOT = ORIGINAL-TRC-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-TRC-REC TO TRC-REC.
           PERFORM REWRITE-TRC1-FILE.

           MOVE "C"                        TO ERRLOGW-ACTION.
           MOVE TRC1-KEY                    TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME              TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-TRC1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-TRC1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-TRC1-FILE.
           PERFORM READ-TRC1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF TRC-REC NOT = ORIGINAL-TRC-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-TRC1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-TRC1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-TRC-FILE.
           MOVE LPWSTRC-TRC1-KEY TO TRC1-KEY.
           MOVE 0 TO ELE.

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
       READ-GL.
           MOVE WORK-GL TO GL-BR-ACCOUNT.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.
           IF IO-BAD
              MOVE "INVALID G/L#" TO MESS
              PERFORM SEND-MESS.

           IF GL-BR-ACCOUNT = 0
              MOVE 0 TO IO-FG.
       EXIT-CHECK-GLNO.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE TC-TYPE TO F01.
           MOVE TC-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F02.

           MOVE TC-AMT TO F03.
           MOVE TC-PAYEE TO F04.
           MOVE TC-REF TO F05.
           MOVE TC-REASON TO F06.

           IF TC-GLAMT(1) NOT = 0
              MOVE TC-GLNO(1) TO WORK-GL
              MOVE WORK-BR TO DISP-BR
              MOVE WORK-NO TO DISP-NO
              MOVE DISP-GL TO F07
              MOVE TC-GLAMT(1) TO F08.

           IF TC-GLAMT(2) NOT = 0
              MOVE TC-GLNO(2) TO WORK-GL
              MOVE WORK-BR TO DISP-BR
              MOVE WORK-NO TO DISP-NO
              MOVE DISP-GL TO F09
              MOVE TC-GLAMT(2) TO F10.

           IF TC-GLAMT(3) NOT = 0
              MOVE TC-GLNO(3) TO WORK-GL
              MOVE WORK-BR TO DISP-BR
              MOVE WORK-NO TO DISP-NO
              MOVE DISP-GL TO F11
              MOVE TC-GLAMT(3) TO F12.

           IF TC-GLAMT(4) NOT = 0
              MOVE TC-GLNO(4) TO WORK-GL
              MOVE WORK-BR TO DISP-BR
              MOVE WORK-NO TO DISP-NO
              MOVE DISP-GL TO F13
              MOVE TC-GLAMT(4) TO F14.

           IF TC-GLAMT(5) NOT = 0
              MOVE TC-GLNO(5) TO WORK-GL
              MOVE WORK-BR TO DISP-BR
              MOVE WORK-NO TO DISP-NO
              MOVE DISP-GL TO F15
              MOVE TC-GLAMT(5) TO F16.

           IF TC-VOID < " "
              MOVE " " TO F17
           ELSE
              MOVE TC-VOID TO F17.

           MOVE TC-ACCTNO TO F18.
           MOVE TC-PAID TO F19.
           MOVE TC-PGM TO F20.
           MOVE TC-DLRVNO TO F21.
           MOVE TC-DLNO TO F22.
           MOVE TC-USERID TO F23.
           MOVE TC-POSTTIME TO F24.
           MOVE TC-POSTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F25.

           MOVE TC-SPRCLASS TO F26.
           IF TC-PAYEE-NO = 202020
              MOVE 0 TO TC-PAYEE-NO.
           MOVE TC-PAYEE-NO       TO F27.
           MOVE TC-ACH-FG         TO F28.
           MOVE TC-TILLNO         TO F29.
           MOVE TC-EXTRACT-FG     TO F30.
           MOVE TC-DEBIT-REFCD    TO F31.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************

           MOVE SAVE-BRNO           TO D-BRANCH.

           MOVE LPWSTRC-TC-KEY-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM    TO D-MM.
           MOVE DATE-YYYYMMDD-YY    TO D-YY.


           MOVE DATE-YYYYMMDD-DD      TO D-DAY.
           MOVE LPWSTRC-TC-BRNO       TO D-BRNO.
           MOVE "C"                   TO D-CODE.
           MOVE LPWSTRC-TC-CKNO       TO D-CKNO.
           MOVE LPWSTRC-TC-CLASS      TO D-CLASS.
           MOVE LPWSTRC-TC-SEQ        TO D-SEQ.
           MOVE KEY-BUF TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO TRCMAN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRCMAN-SYSDATE.
           DISPLAY SPACES UPON TRCMAN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRCMAN-SCREEN UPON TRCMAN-WINDOW-HANDLE.
           IF SAVE-BRNO NOT = 0
              MOVE SAVE-BRNO TO TRCMAN-BRANCH
              MOVE SAVE-MMYY TO TRCMAN-MMYY.

           MOVE TRCMAN-FORM-FIELDS TO WR-BUF.
           MOVE TRCMAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE TRCMAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRCMAN-HELP--DIR TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRCMAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

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
     
           MOVE "B"                        TO TRC-PATH-OVERRIDE.
     
       LOAD-BRANCH-MODULE-EXIT.
           EXIT.
     
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTRCCL.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
      *----------------------------------------------------------------
       INVALID-CHECK-TYPE.
           MOVE "TYPE MUST BE  LN, DL, DR, DS, DP, MS" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPTRC1IN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY TRCMAN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
