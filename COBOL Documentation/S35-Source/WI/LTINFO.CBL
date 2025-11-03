       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LTINFO.
       DATE-WRITTEN.  10-11-2001.
      ******************************************************************
      *
      * IDENTIFICATION:  WI/LTINFO.C
      *
      * DESCRIPTION   :  ADDON INSURANCE INFORMATION
      *
      * CALLED FROM   :  WI/RVLINE.C
      *
      *-----------------------------------------------------------------
      * REV:
      * MG  2001.1011 NEW
      * BLF 071009 DO A FLDVAL FOR VALUE OF ARCHIVE-FG; IF "Y", PASSED ACCT.
      *            IS IN ARCHIVE DIRECTORY, SO SET UP ARCHIVE PATHS FOR LN,
      *            LT, LX, WORLD PR# 547
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  KEC 2016.0318 CHANGED CD-FEE???? TO CD-GC-????.
      *  BAH 2017.0420 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 2019.0116 REMOVED LXFILE
      *  BAH 2019.0128 SPLIT CDFILE
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
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/LTINFO.CBL S35 02/23/24-11:42:48 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

      *********************************************************
      *   LNFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LN-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/LP/LNFILE".

      *********************************************************
      *   LTIFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LTI-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-DIR1       PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-DIRECTORY  PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-MACHINE    PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-DATA       PIC X(6).
      *    03  FILLER                 PIC X(11) VALUE "/LP/LTIFILE".


      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  LEGEND                      PIC X(75)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

       01  LEGEND-OPTIONS.
           03  FILLER PIC X(30) VALUE " F7-EXIT                      ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(15) VALUE "               ".

       01  SUB                         PIC 9(6)  COMP-3.

       01  ORIGINAL-GL                 PIC X(73)    VALUE SPACES.
       01  ORIGINAL-EARN               PIC X(73)    VALUE SPACES.
       01  CHANGED-GL                  PIC X(73)    VALUE SPACES.
       01  CHANGED-EARN                PIC X(73)    VALUE SPACES.

      ******************************************************************
      *
      *    D I S P L A Y - W O R K E R S
      *
      ******************************************************************
       01  DISPLAY-BUF.
           03  F01        PIC ZZ/ZZ/ZZ.
           03  F02        PIC Z9.
           03  F03        PIC Z9.
           03  F04        PIC XX.
           03  F05        PIC Z(5).ZZ-.
           03  F06        PIC Z(5).ZZ-.
           03  F07        PIC Z(5).ZZ-.
           03  F08        PIC ZZ.
           03  F081       PIC X.
           03  F082       PIC ZZ/ZZ/ZZ.
           03  F083       PIC ZZ/ZZ/ZZ.
           03  F084       PIC ZZZ.ZZ.
           03  F085       PIC Z(6).ZZ-.
           03  F086       PIC Z(4).ZZ-.
           03  F087       PIC Z(4).ZZ-.
           03  F088       PIC X.
           03  F09        PIC XX.
           03  F091       PIC X(17).
           03  F092       PIC Z(4).ZZ.
           03  F10        PIC XX.
           03  F101       PIC X(17).
           03  F102       PIC Z(4).ZZ.
           03  F11        PIC ZZZ.
           03  F111       PIC X(3).
           03  F12        PIC ZZ/ZZ/ZZ.
           03  F13        PIC X.
           03  F14        PIC Z(5).ZZ-.
           03  F15        PIC ZZZZZZ.ZZ-.
           03  F151       PIC ZZZZZZ.ZZ-.
           03  F152       PIC ZZZZZZ.ZZ-.
           03  F153       PIC ZZZZZZ.ZZ-.
           03  F154       PIC ZZZZZZ.ZZ-.
           03  F154-ALP   REDEFINES F154
                          PIC X(10).
           03  F155       PIC ZZZZZZ.ZZ.


      *-----------------------------------------------------------------
       01  DISPLAY-LIST.

           03  FILLER     PIC X(28) VALUE
               "010 020 030 040 050 060 070 ".
           03  FILLER     PIC X(36) VALUE
               "080 081 082 083 084 085 086 087 088 ".
           03  FILLER     PIC X(44) VALUE
               "090 091 092 100 101 102 110 111 120 130 140 ".
           03  FILLER     PIC X(24) VALUE
               "150 151 152 153 154 155.".

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".

       01  LTINFO-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LTINFO_DEF.CPY".
       COPY "LIBWI/LTINFO_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  LTINFO-BRNO                PIC 9(4).
       01  LTINFO-ACCTNO              PIC 9(8).
       01  LTINFO-SEQNO               PIC 9(3).
       01  RVLINE-ARCHIVE-FG             PIC X.

       SCREEN SECTION.
       COPY "LIBWI/LTINFO_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME LTINFO-BRNO LTINFO-ACCTNO LTINFO-SEQNO
                               RVLINE-ARCHIVE-FG EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE SPACES TO ERRCD.
           MOVE SPACES TO VDU-WIMENU-SELECTION.
           MOVE "LTINFO" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT SCREEN LINE 01 * VDU-WIN-L-OFFSET
               AT SCREEN COL 01 * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE 80 + VDU-WIN-W-MOD
               LINES 22 + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS LTINFO-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM PRIMARY-DRIVER.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.


      ******************************************************************
       PRIMARY-DRIVER SECTION.
      ******************************************************************
      *    IF ARCHIVE-FG = "Y"
      *       PERFORM SETUP-ARCHIVE-PATHS
      *       IF IO-BAD
      *          GO TO END-PROGRAM.

           MOVE LTINFO-BRNO   TO LN-OWNBR.
           MOVE LTINFO-ACCTNO TO LN-ACCTNO.
           PERFORM GET-LN-RECORD.
           IF ( IO-BAD )
              GO TO IO-ERROR.

      * NEED THIS IN CASE OF ARCHIVE
           MOVE LTINFO-BRNO      TO LTI-BRNO.
           MOVE LTINFO-ACCTNO    TO LTI-ACCTNO.
           MOVE LTINFO-SEQNO     TO LTI-SEQNO.
           PERFORM GET-LTI1-RECORD.
           IF ( IO-BAD )
              MOVE " ADDON INFO RECORD NOT FOUND!" TO MESS
              PERFORM SEND-MESS
              GO TO PRIMARY-DRIVER-EXIT.

           PERFORM DISPLAY-RECORD.
           MOVE LEGEND-OPTIONS TO LEGEND.
           PERFORM SEND-LEGEND.

       PRIMARY-DRIVER-OPTIONS.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF NOT ( F7-KEY )
              GO TO PRIMARY-DRIVER-OPTIONS.

       PRIMARY-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-RECORD SECTION.
      ******************************************************************

           MOVE SPACES                     TO DISPLAY-BUF.

           MOVE LTI-ORIG-1STPYDATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                 TO F01.
           MOVE LTI-PRIOR-NODEF             TO F02.
           MOVE LTI-INS-TYPE                TO F03.
           MOVE LTI-TRCD                    TO F04.
           MOVE LTI-INS-PAYMNT-INCR         TO F05.
           MOVE LTI-INS-TOT-ADDON           TO F06.
           MOVE LTI-ANTICERN(3)             TO F07.
           MOVE LTI-INSCOMP                 TO F08.
           MOVE LTI-INSOURS                 TO F081.
           MOVE LTI-INSEFF-DATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                 TO F082.
           MOVE LTI-INSEXP-DATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                 TO F083.

           MOVE LTI-INSEFF-DATE      TO NUM-DATE.
           MOVE LTI-INSEXP-DATE      TO SYS-DATE.
           PERFORM TIM360.

           MOVE ELAPSED-MONTHS              TO F084.
           MOVE LTI-INSCOVR                 TO F085.
           MOVE LTI-INSPREM                 TO F086.
           MOVE LTI-INSCOMM                 TO F087.
           MOVE LTI-INS-REBATE              TO F088.

           MOVE LTI-INSFEECD(1)             TO F09 CDB-CODE.
           PERFORM GET-CODE.
           MOVE CDB-DESC                     TO F091.
           MOVE LTI-INSFEE(1)               TO F092.

           MOVE LTI-INSFEECD(2)             TO F10 CDB-CODE.
           PERFORM GET-CODE.
           MOVE CDB-DESC                     TO F101.
           MOVE LTI-INSFEE(2)               TO F102.

           MOVE 0                           TO SUB.
           PERFORM DISPLAY-FIELDS-COMPONENTS 1 TIMES.

           MOVE LTI-ANTICTERM               TO F11.
           MOVE LTI-CANCEL-REASON-CD        TO F111.
           MOVE LTI-ANTIC-START-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                 TO F12.
           MOVE LTI-INT-REBATE              TO F13.
           MOVE LTI-INT-LAST-EARNED         TO F14.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-RECORD-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS-COMPONENTS SECTION.
      ******************************************************************
           ADD 1 TO SUB.

           ADD  LTI-ANTICADJ(SUB) LTI-ANTICERN(SUB) GIVING F15.
           MOVE LTI-ANTICADJ(SUB)   TO F151.
           MOVE LTI-ANTICERN(SUB)   TO F152.
           MOVE LTI-ACCERN  (SUB)   TO F153.

           IF ( LN-PLCD = " "         ) AND
              ( LN-SERVACCT NOT = "Y" )
              SUBTRACT LTI-ACCERN(SUB) FROM LTI-ANTICERN(SUB)
                                               GIVING F154
              IF ( F154 NOT = " " )
                 PERFORM DISPLAY-FIELDS-COMPONENTS-RB
              ELSE
                 NEXT SENTENCE
           ELSE
              IF ( SUB = 1 )
                 IF ( LN-PLCD NOT = " " )
                    MOVE "NONE (PL)" TO F154-ALP
                 ELSE
                    IF ( LN-SERVACCT = "Y" )
                       MOVE "NONE (SA)" TO F154-ALP.
           MOVE LTI-UFERN(SUB) TO F155.

       DISPLAY-FIELDS-COMPONENTS-EXIT.
           EXIT.


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
      *COPY "LIBWI/PFSWINDOWS.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY SPACES UPON LTINFO-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY LTINFO-SCREEN UPON LTINFO-WINDOW-HANDLE.
           MOVE LTINFO-FORM-FIELDS TO WR-BUF.
           MOVE LTINFO-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE LTINFO-HELP--FILE TO HELP-LINK-FILE.
           MOVE LTINFO-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LTINFO_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *-----------------------------------------------------------------
       GET-LTI1-RECORD.
           PERFORM OPEN-LTI1-FILE.
           PERFORM READ-LTI1-FILE.
           PERFORM CLOSE-LTI1-FILE.

      *-----------------------------------------------------------------
       GET-LN-RECORD.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

      *-----------------------------------------------------------------
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       DISPLAY-FIELDS-COMPONENTS-RB.
      * SKIP REBATED COMPONENTS:
           IF ( SUB = 1 ) AND
              ( LTI-ANTICADJ(1) NOT = 0
                      OR (NOT (LTI-INT-REBATE = " " OR "C")) )
              MOVE "  REBATED?" TO F154-ALP.

      *-----------------------------------------------------------------
       GET-CODE.
           MOVE "GC"      TO CDB-TYPE.
           MOVE LN-OWNBR  TO CDB-BRANCH.
      *--> SET BEFORE        CDB-CODE.

           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "INVALID" TO CDB-DESC.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CDB1-FILE.
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.
       EXIT-PROG.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LTINFO-SCREEN.
           DESTROY LTINFO-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
