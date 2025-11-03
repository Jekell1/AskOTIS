       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LHMAIN.
      ******************************************************************
      *
      *  DESCRIPTION:   LOAN HISTORY FILE MAINTENANCE
      *
      *=================================================================
      * REV:
      * GLF 992009 CHANGE SSNO ENTRY TO USE NEW VDUCLASS "X" EDIT, MAKE SSNO
      *            ENTRY CONSISTENT THROUGHOUT
      * BLF 070719 ADD FIELD #16, LH-ARCHIVE-FG, WORLD PR# 547
      * MJD 131119 A30 CONVERSION COMPLETED
      *  CS 170424 ACTIVATE 8 DIGIT DATES
      *  CS 170424 CHANGED ENTRY OF LOAN DATE FROM "EN M060N" TO
      *            03  FILLER      PIC X(8) VALUE "ER M060N".
      *            DIDNT ALLOW 0'S IN DATE FIELD
      *
      * KEC 2017.0324 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LH' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      * KEC 2017.0530 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          FIXED THE USE OF LN-KEY, LN-1 & LOAN-KEY.
      *          NOW IS JUST USING HOLD-LN1-KEY & HOLD-LN-ACCTNO.
      * BAH 181004 GLOBAL BWFILE
      * BAH 181127 GLOBAL LHFILE
      * BAH 181211 GLOBAL LNFILE
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/LHMAIN.CBL S35 02/21/24-12:31:15 >".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSLH.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".

       01 LHMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       01  ERRCD           PIC X       VALUE " ".
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  NAME-WORKER     PIC X(34).
       01  NAME            PIC X(16).
       01  CDDESC          PIC X(16).
       01  HOLD-LH1-KEY.
           03  HOLD-LH-BRNO    PIC 9(4).
           03  HOLD-LH-SSNO    PIC 9(10).
           03  HOLD-LH-ACCTNO  PIC 9(8).
       01  WORK-KEY.
           03  WK-1        PIC 999.
           03  DASH-1      PIC X.
           03  WK-2        PIC 99.
           03  DASH-2      PIC X.
           03  WK-3        PIC 9(4).
       01  SS-KEY.
           03  SS-1        PIC 999.
           03  SS-2        PIC 99.
           03  SS-3        PIC 9(4).
       01  SOCSEC-KEY REDEFINES SS-KEY  PIC 9(9).

       01  HOLD-LN-ACCTNO          PIC 9(08).

      *=================================================================
      *   LHCH-REC FROM 'REPLACING LEADING' ON LH-REC. [CHANGED-REC]
      *   LHOR-REC FROM 'REPLACING LEADING' ON LH-REC. [ORIGINAL-REC]
      *=================================================================
       COPY "LIBLP/LP01LH.CPY" REPLACING LEADING "LH" BY "LHCH".
       COPY "LIBLP/LP01LH.CPY" REPLACING LEADING "LH" BY "LHOR".
      *=================================================================
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(10).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(12).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(11)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(8).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(12).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(11)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(8).99-.

       01  ALP-PROFILE     PIC X(12)   VALUE SPACES.
       01  NUM-PROFILE     REDEFINES ALP-PROFILE
                           PIC 9(12).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01         PIC Z9.
           03  F02         PIC Z9.
           03  F03         PIC ZZZ,ZZ9.99.
           03  F04         PIC 99/99/99.
           03  F05         PIC ZZ9.
           03  F06         PIC Z(3)9.99.
           03  F07         PIC XX.
           03  F08         PIC X.
           03  F09         PIC X(12).
           03  F10         PIC XXX.
           03  F101        PIC X(40).
           03  F11         PIC 99/99/99.
           03  F12         PIC ZZ9.
           03  F13         PIC 99/99/99.
           03  F14         PIC 99/99/99.
           03  F15         PIC X(10).
           03  F16         PIC X.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(28) VALUE
           "101 110 120 130 140 150 160.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 16.
       01  SPEC-TABLE.
      * 1. CLASS
           03  FILLER      PIC X(8) VALUE "EN N020N".
      * 2. LAW CODE
           03  FILLER      PIC X(8) VALUE "EN N020N".
      * 3. LOAN AMOUNT
           03  FILLER      PIC X(8) VALUE "EN N062N".
      * 4. LOAN DATE
           03  FILLER      PIC X(8) VALUE "ER M060N".
      * 5. ORIGINAL TERMS
           03  FILLER      PIC X(8) VALUE "EN N030N".
      * 6. DEALER NO
           03  FILLER      PIC X(8) VALUE "ENNN042N".
      * 7. COLLATERAL CODE
           03  FILLER      PIC X(8) VALUE "E RA020N".
      * 8. MAKER CODE
           03  FILLER      PIC X(8) VALUE "E RA010N".
      * 9. PAYMENT PROFILE
           03  FILLER      PIC X(8) VALUE "E  A120N".
      *10. PAY-OFF CODE
           03  FILLER      PIC X(8) VALUE "E RA030N".
      *11. PAY-OFF DATE
           03  FILLER      PIC X(8) VALUE "ER M060N".
      *12. PAY-OFF TERMS
           03  FILLER      PIC X(8) VALUE "EN N030N".
      *13. PL DATE
           03  FILLER      PIC X(8) VALUE "ER M060N".
      *14. JUDGMENT DATE
           03  FILLER      PIC X(8) VALUE "ER M060N".
      *15. STATUS
           03  FILLER      PIC X(8) VALUE "E RA100N".
      *16. ARCHIVE FLAG
           03  FILLER      PIC X(8) VALUE "E RA100N".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 16 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      *
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LHMAIN_DEF.CPY".
       COPY "LIBMN/LHMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/LHMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      *************** ***************************
       MAIN-PROGRAM SECTION.
      *************** ***************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "LHMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LHMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
      D    STOP MESS.

           PERFORM FORM-RESET.

           PERFORM REC-LH-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.
           GO TO MAIN-MODULE.

      ******************************************
       REC-LH-MODULE SECTION.
      ******************************************
       REC-LH-MODULE-NO.
           MOVE 0             TO HOLD-LH-SSNO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "ERNX110SSNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LH-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO ENTER-LOAN.
           IF VDUSTATUS NOT = "00"
              GO TO REC-LH-MODULE-NO.
           IF VDUNUM0 = ZEROES
              GO TO REC-LH-MODULE.

           MOVE VDUNUM0 TO SOCSEC-KEY BW-SSNO HOLD-LH-SSNO.

           IF SOCSEC-KEY = ZERO
              GO TO REC-LH-MODULE.

       READ-BORROWER.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "S  A350SSDESC." TO VDU
              MOVE "*** RECORD NOT ON BORROWER FILE ***" TO VDUBUF
              PERFORM SCREENIO
              GO TO REC-LH-MODULE.

           PERFORM DISPLAY-NAME.
       ENTER-LOAN.
           IF IO-TYPE = "CLR"
              GO TO END-REC-LH-MODULE.
           IF HOLD-LH-SSNO = 0
              GO TO REC-LH-MODULE.

           MOVE "EN N060LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM SCAN-HISTORY
              IF IO-TYPE = "CLR"
                 MOVE " " TO IO-TYPE
                 GO TO ENTER-LOAN
              ELSE
                 GO TO END-REC-LH-MODULE.
           IF VDUSTATUS = "1>"
               MOVE "END" TO IO-TYPE
               GO TO END-REC-LH-MODULE.
           IF VDUSTATUS = "1U"
               GO TO REC-LH-MODULE.
           IF VDUSTATUS NOT = "00"
               GO TO ENTER-LOAN.
           IF VDUBUF = SPACES OR VDUNUM0 = 0
               GO TO ENTER-LOAN.

           MOVE VDUNUM0 TO HOLD-LN-ACCTNO.

      *    MOVE "EN N020LNSEQ." TO VDU.
      *    PERFORM SCREENIO.
      *    IF VDUSTATUS NOT = "00"
      *       GO TO ENTER-LOAN.
      *    IF VDUBUF = SPACES
      *       GO TO ENTER-LOAN.
      *    MOVE VDUNUM0 TO LN-2.

           IF HOLD-LN-ACCTNO IS NOT NUMERIC
              GO TO ENTER-LOAN.
           IF HOLD-LN-ACCTNO = ZERO
              GO TO ENTER-LOAN.

           PERFORM OPEN-LH1-FILE.

      * HAS TO BE AFTER THE OPEN
           MOVE LH-PATH-OWNBR  TO HOLD-LH-BRNO.
           MOVE HOLD-LN-ACCTNO TO HOLD-LH-ACCTNO.
           MOVE HOLD-LH1-KEY  TO LH1-KEY.

           PERFORM READ-LH1-FILE.
           PERFORM CLOSE-LH1-FILE.
           IF IO-FG = 0
              MOVE LH-REC TO LHOR-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-LH-MODULE.
           EXIT.

      *************************************************
       ENTER-SCANKEY SECTION.
      *************************************************

           MOVE " ENTER BORROWER LAST NAME OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-BW2-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE SPACES        TO BW-NAME.


           PERFORM START-BW2-FILE.
       ENTER-KEY.
           MOVE "E RA160SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              GO TO CLOSE-SCANKEY.

           MOVE VDUBUF TO BW-NAME.


           PERFORM START-BW2-FILE.
       READ-FILES-NEXT.
           PERFORM READ-BW2-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
             MOVE "CLR" TO IO-TYPE
             GO TO CLOSE-SCANKEY.

           PERFORM DISPLAY-SS.
           PERFORM DISPLAY-NAME.

           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           PERFORM CLOSE-BW2-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       EXIT-SCANKEY.
           EXIT.

      *************************************************
       SCAN-HISTORY SECTION.
      *************************************************
           MOVE "F6 TO SCAN NEXT LOAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-LH1-FILE.
           MOVE BW-OWNBR TO LH-BRNO.
           MOVE BW-SSNO  TO LH-SSNO.
           MOVE 0        TO LH-ACCTNO.

           PERFORM START-LH1-FILE.
       ENTER-LH1-KEY.
           MOVE "E RA010LH1KEY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-LH1-SORTKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-LH1-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-LH1-KEY.

           MOVE LH-REC TO LHOR-REC.

           GO TO CLOSE-LH1-SORTKEY.
       READ-LH1-NEXT.
           PERFORM READ-LH1-FILE-NEXT.
           IF (IO-FG = 9) OR (LH-BRNO NOT = LH-PATH-OWNBR)
                           OR (LH-SSNO NOT = BW-SSNO)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-LH1-SORTKEY.
         
           MOVE LH-PATH-OWNBR TO HOLD-LH-BRNO.
           MOVE LH-ACCTNO     TO HOLD-LH-ACCTNO HOLD-LN-ACCTNO.

           MOVE "SN N060LNNO." TO VDU.
           MOVE HOLD-LN-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           GO TO ENTER-LH1-KEY.
       CLOSE-LH1-SORTKEY.
           PERFORM CLOSE-BW2-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
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
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 F-16
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUNUM0 TO LH-CLASS.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM0 TO LH-LAWCD.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM2 TO LH-LNAMT.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDU-DATE-YYYYMMDD TO LH-LOANDATE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM0 TO LH-ORGTERM.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM2 TO DL-DLNO LH-DLNO.
           IF DL-DLNO = 0
               GO TO ENTER-ELE.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG NOT = 0
              MOVE "*  NOT ON FILE  *" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO LH-COLLCD.
           GO TO CREATE-ROUTINE.
       F-08.
           IF VDUBUF = "M" OR "J" OR "E"
               MOVE VDUBUF TO LH-MAKERCD
               GO TO CREATE-ROUTINE
           ELSE
               MOVE " ENTER 'M', 'J', OR 'E'" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.
       F-09.
           MOVE VDUBUF TO ALP-PROFILE.
           INSPECT ALP-PROFILE REPLACING ALL "-" BY "9".
           INSPECT ALP-PROFILE REPLACING ALL " " BY "9".
           IF NUM-PROFILE NOT NUMERIC
              MOVE " ENTER: 0 - 8  OR  '-'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE NUM-PROFILE TO LH-PYPROFILE.
           INSPECT ALP-PROFILE REPLACING ALL "9" BY "-".
           MOVE "S  A120090." TO VDU.
           MOVE ALP-PROFILE TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-10.
           PERFORM OPEN-CD1-FILE.
           MOVE VDUBUF TO CD-CODE LH-POCD.
           MOVE "PO" TO CD-TYPE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG = 9
               MOVE "** NOT ON FILE **" TO CDDESC
               PERFORM DISPLAY-DESC
               GO TO ENTER-ELE.
           MOVE CD-DESC TO CDDESC.
           PERFORM DISPLAY-DESC.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDU-DATE-YYYYMMDD TO LH-POFFDATE.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM0 TO LH-POTERMS.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDU-DATE-YYYYMMDD TO LH-PLDATE.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDU-DATE-YYYYMMDD TO LH-JDDATE.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUBUF TO LH-STATUS.
           GO TO CREATE-ROUTINE.
       F-16.
           IF VDUBUF = " " OR "Y" OR "N"
               MOVE VDUBUF TO LH-ARCHIVE-FG
               GO TO CREATE-ROUTINE
           ELSE
               MOVE "PLEASE ENTER 'Y', OR 'N'" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      *********************************************
       WRITE-MODULE SECTION.
      *********************************************
           PERFORM OPEN-LH1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE LH-REC TO LHCH-REC.

           IF LHOR-REC = LHCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LH1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LH-REC NOT = LHOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE LHCH-REC TO LH-REC.

           PERFORM REWRITE-LH1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-LH1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-LH1-FILE.

      *********************************************
       DELETE-MODULE SECTION.
      *********************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LH1-FILE.
           PERFORM READ-LH1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF LH-REC NOT = LHOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LH1-FILE.
      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING LH1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.
       END-DELETE-MODULE.
           PERFORM CLOSE-LH1-FILE.

      **********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      **********************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LH-BRNO   TO WORK-ERRLOG-BR.
           MOVE LH-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LHOR-CLASS NOT = LHCH-CLASS
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "CLASS     "            TO WORK-ERRLOG-DESC
             MOVE LHOR-CLASS              TO WORK-ERRLOG-ORG-N0
             MOVE LHCH-CLASS              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-LAWCD NOT = LHCH-LAWCD
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "LAWCD     "            TO WORK-ERRLOG-DESC
             MOVE LHOR-LAWCD              TO WORK-ERRLOG-ORG-N0
             MOVE LHCH-LAWCD              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-LNAMT NOT = LHCH-LNAMT
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "LNAMT     "            TO WORK-ERRLOG-DESC
             MOVE LHOR-LNAMT              TO WORK-ERRLOG-ORG-N2
             MOVE LHCH-LNAMT              TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-LOANDATE NOT = LHCH-LOANDATE
             MOVE "04 "                   TO WORK-ERRLOG-FIELD
             MOVE "EFFDATE   "            TO WORK-ERRLOG-DESC
             MOVE LHOR-LOANDATE           TO WORK-ERRLOG-ORG-N0
             MOVE LHCH-LOANDATE           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-ORGTERM NOT = LHCH-ORGTERM
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "ORG TERMS "            TO WORK-ERRLOG-DESC
             MOVE LHOR-ORGTERM           TO WORK-ERRLOG-ORG-N0
             MOVE LHCH-ORGTERM           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-DLNO NOT = LHCH-DLNO
             MOVE "06 "               TO WORK-ERRLOG-FIELD
             MOVE "DEALER NO "        TO WORK-ERRLOG-DESC
             MOVE LHOR-DLNO           TO WORK-ERRLOG-ORG-N2
             MOVE LHCH-DLNO           TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-COLLCD NOT = LHCH-COLLCD
             MOVE "07 "               TO WORK-ERRLOG-FIELD
             MOVE "COLLAT CD "        TO WORK-ERRLOG-DESC
             MOVE LHOR-COLLCD           TO WORK-ERRLOG-ORG-ALP
             MOVE LHCH-COLLCD           TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-MAKERCD NOT = LHCH-MAKERCD
             MOVE "08 "               TO WORK-ERRLOG-FIELD
             MOVE "MAKER CD  "        TO WORK-ERRLOG-DESC
             MOVE LHOR-MAKERCD           TO WORK-ERRLOG-ORG-ALP
             MOVE LHCH-MAKERCD           TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-PYPROFILE NOT = LHCH-PYPROFILE
             MOVE "09 "               TO WORK-ERRLOG-FIELD
             MOVE "PY PROFILE"        TO WORK-ERRLOG-DESC
             MOVE LHOR-PYPROFILE      TO WORK-ERRLOG-ORG-ALP
             MOVE LHCH-PYPROFILE      TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-POCD NOT = LHCH-POCD
             MOVE "10 "               TO WORK-ERRLOG-FIELD
             MOVE "PAYOFF CD "        TO WORK-ERRLOG-DESC
             MOVE LHOR-POCD      TO WORK-ERRLOG-ORG-ALP
             MOVE LHCH-POCD      TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-POFFDATE NOT = LHCH-POFFDATE
             MOVE "11 "               TO WORK-ERRLOG-FIELD
             MOVE "PAYOFF DT "        TO WORK-ERRLOG-DESC
             MOVE LHOR-POFFDATE      TO WORK-ERRLOG-ORG-N0
             MOVE LHCH-POFFDATE      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-POTERMS NOT = LHCH-POTERMS
             MOVE "12 "               TO WORK-ERRLOG-FIELD
             MOVE "PAYOFF TRM"        TO WORK-ERRLOG-DESC
             MOVE LHOR-POTERMS      TO WORK-ERRLOG-ORG-N0
             MOVE LHCH-POTERMS      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-PLDATE NOT = LHCH-PLDATE
             MOVE "13 "               TO WORK-ERRLOG-FIELD
             MOVE "P&L DATE  "        TO WORK-ERRLOG-DESC
             MOVE LHOR-PLDATE      TO WORK-ERRLOG-ORG-N0
             MOVE LHCH-PLDATE      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-JDDATE NOT = LHCH-JDDATE
             MOVE "14 "               TO WORK-ERRLOG-FIELD
             MOVE "JD DATE   "        TO WORK-ERRLOG-DESC
             MOVE LHOR-JDDATE      TO WORK-ERRLOG-ORG-N0
             MOVE LHCH-JDDATE      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-STATUS NOT = LHCH-STATUS
             MOVE "15 "               TO WORK-ERRLOG-FIELD
             MOVE "STATUS    "        TO WORK-ERRLOG-DESC
             MOVE LHOR-STATUS      TO WORK-ERRLOG-ORG-ALP
             MOVE LHCH-STATUS      TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LHOR-ARCHIVE-FG NOT = LHCH-ARCHIVE-FG
             MOVE "16 "               TO WORK-ERRLOG-FIELD
             MOVE "ARCHIVE-FG    "        TO WORK-ERRLOG-DESC
             MOVE LHOR-ARCHIVE-FG      TO WORK-ERRLOG-ORG-ALP
             MOVE LHCH-ARCHIVE-FG      TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.


      ************************************
       LOG-FIELD-CHANGE SECTION.
      ************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ******************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LH-FILE.

           MOVE HOLD-LH1-KEY TO LH1-KEY.

           MOVE 0 TO ELE.

      ******************************************
       DISPLAY-FIELDS SECTION.
      ******************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE LH-CLASS TO F01.
           MOVE LH-LAWCD TO F02.
           MOVE LH-LNAMT TO F03.
           MOVE LH-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F04.
           MOVE LH-ORGTERM TO F05.
           MOVE LH-DLNO TO F06.
           MOVE LH-COLLCD TO F07.
           MOVE LH-MAKERCD TO F08.
           MOVE LH-PYPROFILE TO NUM-PROFILE.
           PERFORM PROFILE-DASH.
           MOVE ALP-PROFILE TO F09.
           MOVE LH-POCD TO F10 CD-CODE.

           PERFORM OPEN-CD1-FILE.
           MOVE "PO" TO CD-TYPE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG = 9
               MOVE "**INVALID CODE**" TO F101
           ELSE
               MOVE CD-DESC TO F101.

           MOVE LH-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY TO F11.
           MOVE LH-POTERMS TO F12.
           MOVE LH-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F13.
           MOVE LH-JDDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F14.
           MOVE LH-STATUS TO F15.
           MOVE LH-ARCHIVE-FG TO F16.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO LHMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LHMAIN-SYSDATE.
           DISPLAY SPACES UPON LHMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LHMAIN-SCREEN UPON LHMAIN-WINDOW-HANDLE.
           MOVE LHMAIN-FORM-FIELDS TO WR-BUF.
           MOVE LHMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE LHMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE LHMAIN-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LHMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPLHCL.CPY".

      ***************************************
       MISC-PARAGRAPHS SECTION.
      ***************************************
       COPY "LIBGB/ACCESS.CPY".
       DISPLAY-SS.
           MOVE "S  A110SSNO." TO VDU.
           MOVE BW-SSNO       TO HOLD-LH-SSNO SOCSEC-KEY.
           MOVE BW-OWNBR      TO HOLD-LH-BRNO.
           MOVE SS-1 TO WK-1.
           MOVE SS-2 TO WK-2.
           MOVE SS-3 TO WK-3.
           MOVE "-" TO DASH-1 DASH-2.
           MOVE WORK-KEY TO VDUBUF.
           PERFORM SCREENIO.
       DISPLAY-NAME.
           MOVE "S  A350SSDESC." TO VDU.
           MOVE SPACES TO NAME-WORKER.
           MOVE BW-LNAME TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           MOVE BW-FNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "                "
             BY NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO VDUBUF.
           PERFORM SCREENIO.
       DISPLAY-DESC.
           MOVE "S  A400101." TO VDU.
           MOVE CDDESC TO VDUBUF.
           PERFORM SCREENIO.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       PROFILE-DASH.
           INSPECT ALP-PROFILE REPLACING ALL "9" BY "-".

      **********************************************
       IO-ROUTINES SECTION.
      **********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LH1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLH1IN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      **********************************************
       END-ROUTINE SECTION.
      **********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY LHMAIN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
