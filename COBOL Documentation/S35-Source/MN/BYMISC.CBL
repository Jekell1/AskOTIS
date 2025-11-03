       IDENTIFICATION DIVISION.
       PROGRAM-ID.  BYMISC.
      ******************************************************************
      *
      *                 (MN)
      *  DESCRIPTION:   ENTER MISC BRANCH & BRANCH STAT FIELDS FOR
      *                 PROGRESS REPORT
      *
      *  REV:
      *  BLV 081297 ADDED ENTRY OF BY-STD-POS (LOAN VOLUME VS STANDARD)
      *  BLV 031598 ADDED ENTRY OF BY-REPO-NO, BY REPO-AMT FOR GIFTNET
      *             REPO TRACKING-STORE IN CURRENT GAIN-LOSS ON-HAND
      *  GLF 990715 ADDED BR-TAX-PREBOOK-CUST AND BR-TAX-PREBOOK-NONCUST
      *             WORLD PR#211
      *  MJD 131127 A30 CONVERSION COMPLETED. COMMENTED OUT NON-DIPLAY FIELD
      *             CHANGEBRNO. DID SOME GREPS AND IT IS NEVER UTILIZED.
      *  BAH 140501 FIXED EXIT-PATHNAME, WAS USING EXITPROG, VERYANT
      *  BAH 140822 SET BRANCH-NO TO EXT-FILPATH-BRANCH, IT GOT REMOVED
      *
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *
      *  BAH 180614 ERRLOG
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/BYMISC.CBL S34 06/03/21-15:41:44 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".


       01  ERRCD           PIC X       VALUE SPACES.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  BRANCH-NO       PIC 9(4)    VALUE 0.
       01  CHANGE-BRNO-FG  PIC X       VALUE "N".

      ******************************************************************
      *    H O L D - K E Y
      ******************************************************************
       01  HOLD-BY1-KEY.
           03  HOLD-BY-DATE-CCYYMM         PIC 9(6).
           03  HOLD-BY-BRNO                PIC 9(4).
           03  HOLD-BY-CLASS               PIC 9(3).

       01  HOLD-FIELDS.
           03  HOLD-NO-FT-EMP            PIC 9(3).
           03  HOLD-NO-PT-EMP            PIC 99V99.
           03  HOLD-PROJ-INCREASE        PIC S9(6).
           03  HOLD-PROJ-PROFIT          PIC S9(6).
           03  HOLD-30-DAY-MOVED         PIC S9(6).
           03  HOLD-STD-POS              PIC S9(6).
           03  HOLD-REPO-NO              PIC S9(4).
           03  HOLD-REPO-AMT             PIC S9(6).
           03  HOLD-TAX-PREBOOK-CUST     PIC 9(4).
           03  HOLD-TAX-PREBOOK-NONCUST  PIC 9(4).

       01  DISPLAY-DATE    PIC 9(4).
       01  DISPLAY-DATEX   REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO  PIC 99.
           03  DISPLAY-YR  PIC 99.
       01  DATE-LIST       PIC X(5)  VALUE "MMYY.".

      *=================================================================
      *   BYCH-REC FROM 'REPLACING LEADING' ON BY-REC. (CHANGED-REC)
      *   BYOR-REC FROM 'REPLACING LEADING' ON BY-REC. (ORIGINAL-REC)
      *=================================================================
       01  BYCH-REC.
           03  BYCH1-KEY.
               05  BYCH-DATE-CCYYMM      PIC 9(6).
               05  BYCH-BRNO             PIC 9(4).
               05  BYCH-CLASS            PIC 9(3).
       COPY "LIBLP/LP01BYA_AREA.CPY" REPLACING LEADING
                                         "BYA-" BY "BYCH-".
       COPY "LIBLP/LP01BYB_AREA.CPY" REPLACING LEADING
                                         "BYB-" BY "BYCH-".

      *-----------------------------------------------------------------
       01  BYOR-REC.
           03  BYOR1-KEY.
               05  BYOR-DATE-CCYYMM      PIC 9(6).
               05  BYOR-BRNO             PIC 9(4).
               05  BYOR-CLASS            PIC 9(3).
       COPY "LIBLP/LP01BYA_AREA.CPY" REPLACING LEADING
                                         "BYA-" BY "BYOR-".
       COPY "LIBLP/LP01BYB_AREA.CPY" REPLACING LEADING
                                         "BYB-" BY "BYOR-".

      *-----------------------------------------------------------------
       COPY "LIBGB/GB01BR.CPY" REPLACING LEADING "BR" BY "BROR".
       COPY "LIBGB/GB01BR.CPY" REPLACING LEADING "BR" BY "BRCH".

      ******************************************************************
      * 55
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BY-DATE      PIC 9(4).
           03  FILLER                   PIC X(1).
           03  WORK-ERRLOG-BY-BRNO      PIC 9(4).
           03  FILLER                   PIC X(1).
           03  WORK-ERRLOG-BY-CLASS     PIC 9(3).
           03  FILLER                   PIC X(1).
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(11).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(11).
               05  WORK-ERRLOG-ORG-N0 REDEFINES WORK-ERRLOG-ORG-ALP 
                                        PIC ZZZZZZ9-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES WORK-ERRLOG-ORG-ALP 
                                        PIC Z(7).99-.
               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(11).
               05  WORK-ERRLOG-CHG-N0 REDEFINES WORK-ERRLOG-CHG-ALP 
                                        PIC ZZZZZZ9-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES WORK-ERRLOG-CHG-ALP 
                                        PIC Z(7).99-.

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC ZZ9.
           03  F02         PIC ZZ.Z9.
           03  F03         PIC ZZZZZ9-.
           03  F04         PIC ZZZZZ9-.
           03  F05         PIC ZZZZZ9-.
           03  F06         PIC ZZZZZ9-.
           03  F07         PIC ZZZ9-.
           03  F08         PIC ZZZZZ9-.
           03  F09         PIC ZZZ9.
           03  F10         PIC ZZZ9.


      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(40)   VALUE
           "010 020 030 040 050 060 070 080 090 100.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       01  MAX             PIC 99      VALUE 10.
       01  SPEC-TABLE.
      * 1. FULL-TIME EMPLOYEES
           03  FILLER      PIC X(8)    VALUE "ENNN030 ".
      * 2. PART-TIME EMPLOYEES
           03  FILLER      PIC X(8)    VALUE "ENNN022 ".
      * 3. PROJECTED INCREASE
           03  FILLER      PIC X(8)    VALUE "ENNS060 ".
      * 4. PROJECTED PROFIT
           03  FILLER      PIC X(8)    VALUE "ENNS060 ".
      * 5. 30 DAY + DOLLARS MOVED THIS WEEK
           03  FILLER      PIC X(8)    VALUE "ENNS060 ".
      * 6. LOAN VOLUME VS STANDARD POSITION
           03  FILLER      PIC X(8)    VALUE "ENNS060 ".
      * 7. NO OF WCBC REPOS
           03  FILLER      PIC X(8)    VALUE "ENNS040 ".
      * 8. AMT OF WCBC REPOS
           03  FILLER      PIC X(8)    VALUE "ENNS060 ".
      * 9. TAX PREBOOK CUSTOMERS
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".
      *10. TAX PREBOOK NONCUSTOMERS
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".

      *-----------------------------------------------------------------
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 10.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       01 BYMISC-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/BYMISC_DEF.CPY".
       COPY "LIBMN/BYMISC_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/BYMISC_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           MOVE EXT-FILPATH-BRANCH TO BRANCH-NO.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "BYMISC" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS BYMISC-WINDOW-HANDLE.

      *-----------------------------------------------------------------
       MAIN-MODULE.

           PERFORM FORM-RESET.

           PERFORM REC-KEY-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-KEY-MODULE SECTION.
      ******************************************************************
       REC-KEY-MODULE-NO.

           MOVE 0 TO HOLD-BY-BRNO HOLD-BY-CLASS.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.

           IF CHANGE-BRNO-FG = "N"
              MOVE "SN N040BRNO." TO VDU
              MOVE BRANCH-NO TO HOLD-BY-BRNO VDUNUM0
              PERFORM SCREENIO
              PERFORM VERIFY-BRNO.

      *----------------------------------------------------------------
       SEND-MMYY.
           PERFORM GET-TODAYS-DATE.
           GO TO SETUP-BY-DATE.

      *----------------------------------------------------------------
       ENTER-DATE.
           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-KEY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-KEY-MODULE-NO.

           IF VDUNUM0 = 0
              GO TO REC-KEY-MODULE-NO.

           MOVE VDUNUM0 TO DISPLAY-DATE.

      *----------------------------------------------------------------
       SETUP-BY-DATE.
           MOVE DISPLAY-MO TO DATE-MMDDYY-MM.
           MOVE DISPLAY-YR TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM HOLD-BY-DATE-CCYYMM.

           IF CHANGE-BRNO-FG = "N"
              GO TO REC-CLASS.
      *----------------------------------------------------------------
       ENTER-BRANCH.
           MOVE "ENTER BRANCH #, F7 - EXIT" TO MESS.
           PERFORM SEND-PROMPT.

           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-KEY-MODULE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-KEY-MODULE-NO.

           MOVE VDUNUM0 TO HOLD-BY-BRNO.
           PERFORM VERIFY-BRNO.

           IF IO-FG NOT = 0
              GO TO REC-KEY-MODULE-NO.

      *-----------------------------------------------------------------
       REC-CLASS.
           MOVE "ENTER CLASS, F6-SCAN CLASS, F7 - EXIT" TO MESS.
           PERFORM SEND-PROMPT.


           MOVE "S  A200CLSDESC." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "EN N020CLS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO REC-KEY-MODULE-NO.

           IF VDUSTATUS = "10" OR "1U"
              IF CHANGE-BRNO-FG = "N"
                 GO TO ENTER-DATE
              ELSE
                 GO TO ENTER-BRANCH.

           IF ( VDUSTATUS = "1<" )
              MOVE 0      TO LCSCAN-KEY
              MOVE SPACES TO LCSCAN-VERIFY
              MOVE "WI/LCSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF LCSCAN-STATUS NOT = "00" 
                 GO TO REC-CLASS
              ELSE
                 MOVE "SN N020CLS." TO VDU
                 MOVE LCSCAN-KEY TO VDUNUM0
                 PERFORM SCREENIO
                 GO TO GET-CLASS.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-KEY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-CLASS.

       GET-CLASS.
           MOVE VDUNUM0 TO HOLD-BY-CLASS.
           PERFORM VERIFY-CLASS-CODE.

           IF IO-FG NOT = 0
              GO TO REC-CLASS.

      *-----------------------------------------------------------------
       READ-BYFILE.

           MOVE HOLD-BY1-KEY TO BY1-KEY.
           PERFORM OPEN-BY1-FILE.
           PERFORM READ-BY1-FILE.
           PERFORM CLOSE-BY1-FILE.

           IF IO-FG NOT = 0
              MOVE "BRANCH STAT RECORD DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              GO TO REC-KEY-MODULE-NO
            ELSE
              MOVE BY-REC TO BYOR-REC
              MOVE BR-REC TO BROR-REC
              PERFORM DISPLAY-FIELDS.

           MOVE BR-NO-FT-EMP TO HOLD-NO-FT-EMP.
           MOVE BR-NO-PT-EMP TO HOLD-NO-PT-EMP.
           MOVE BR-TAX-PREBOOK-CUST TO HOLD-TAX-PREBOOK-CUST.
           MOVE BR-TAX-PREBOOK-NONCUST TO HOLD-TAX-PREBOOK-NONCUST.
           MOVE BY-PROJ-INCREASE TO HOLD-PROJ-INCREASE.
           MOVE BY-PROJ-PROFIT TO HOLD-PROJ-PROFIT.
           MOVE BY-30-DAY-MOVED TO HOLD-30-DAY-MOVED.
           MOVE BY-NO-REPO(1 1 1) TO HOLD-REPO-NO.
           MOVE BY-AMT-REPO(1 1 1) TO HOLD-REPO-AMT.

      *-----------------------------------------------------------------
       END-REC-KEY-MODULE.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 2 TO ELE.
           MOVE 1 TO NEW-FLAG.

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
           MOVE "ENTER-ACCEPT, F1-CLEAR, FIELD # TO CHANGE"
             TO MESS.
           PERFORM SEND-PROMPT.
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
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
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

           IF ELE = 1 MOVE
              "ENTER # FULL TIME EMPLOYEES, F1-CANCEL" TO MESS.
           IF ELE = 2 MOVE
              "ENTER # PART TIME EMPLOYEES, F1-CANCEL, ^ - PREV FIELD"
              TO MESS.
           IF ELE = 3 MOVE
              "ENTER PROJECTED INCREASE AMT, F1-CANCEL, ^ - PREV FIELD"
              TO MESS.
           IF ELE = 4 MOVE
              "ENTER PROJECTED PROFIT AMT, F1-CANCEL, ^ - PREV FIELD"
              TO MESS.
           IF ELE = 5 MOVE
            "ENTER 30 DAY MOVED THIS WK AMT, F1-CANCEL, ^ - PREV FIELD"
              TO MESS.
           IF ELE = 6 MOVE
            "ENTER STANDARD POSITION, F1-CANCEL, ^ - PREV FIELD"
              TO MESS.
           IF ELE = 7 MOVE
            "ENTER # WCBC REPO ACCOUNTS, F1-CANCEL, ^ - PREV FIELD"
              TO MESS.
           IF ELE = 8 MOVE
            "ENTER $ AMT WCBC REPO ACCOUNTS, F1-CANCEL, ^ - PREV FIELD"
              TO MESS.
           IF ELE = 9 MOVE
           "ENTER # TAX PREBOOKING CUSTOMERS, F1-CANCEL, ^ - PREV FIELD"
              TO MESS.
           IF ELE = 10 MOVE
            "ENTER # TAX PREBOOKING NONCUST, F1-CANCEL, ^ - PREV FIELD"
              TO MESS.
           PERFORM SEND-PROMPT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
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
                 DEPENDING ON ELE.

      *=================================================================
      *
      *    SET RECORD ELEMENTS
      *
      *=================================================================
       F-01.
           MOVE VDUNUM0 TO HOLD-NO-FT-EMP.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUNUM2 TO HOLD-NO-PT-EMP.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUNUM0 TO HOLD-PROJ-INCREASE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUNUM0 TO HOLD-PROJ-PROFIT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUNUM0 TO HOLD-30-DAY-MOVED.
           GO TO CREATE-ROUTINE.

       F-06.
           MOVE VDUNUM0 TO HOLD-STD-POS.
           GO TO CREATE-ROUTINE.

       F-07.
           MOVE VDUNUM0 TO HOLD-REPO-NO.
           GO TO CREATE-ROUTINE.

       F-08.
           MOVE VDUNUM0 TO HOLD-REPO-AMT.
           GO TO CREATE-ROUTINE.

       F-09.
           MOVE VDUNUM0 TO HOLD-TAX-PREBOOK-CUST.
           GO TO CREATE-ROUTINE.

       F-10.
           MOVE VDUNUM0 TO HOLD-TAX-PREBOOK-NONCUST.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *******************************************
       WRITE-MODULE SECTION.
      *******************************************
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              PERFORM BR-RECORD-DELETED
              GO TO END-WRITE-MODULE.

           MOVE HOLD-NO-FT-EMP           TO BR-NO-FT-EMP.
           MOVE HOLD-NO-PT-EMP           TO BR-NO-PT-EMP.
           MOVE HOLD-TAX-PREBOOK-CUST    TO BR-TAX-PREBOOK-CUST.
           MOVE HOLD-TAX-PREBOOK-NONCUST TO BR-TAX-PREBOOK-NONCUST.

           MOVE BR-REC TO BRCH-REC.

           PERFORM REWRITE-BR-FILE.

           PERFORM OPEN-BY1-FILE.
           PERFORM READ-BY1-FILE.
           IF IO-FG NOT = 0
              PERFORM BY-RECORD-DELETED
              GO TO END-WRITE-MODULE.

           MOVE HOLD-PROJ-INCREASE TO BY-PROJ-INCREASE.
           MOVE HOLD-PROJ-PROFIT   TO BY-PROJ-PROFIT.
           MOVE HOLD-30-DAY-MOVED  TO BY-30-DAY-MOVED.
           MOVE HOLD-STD-POS       TO BY-STD-POS.
           MOVE HOLD-REPO-NO       TO BY-NO-REPO(1 1 1).
           MOVE HOLD-REPO-AMT      TO BY-AMT-REPO(1 1 1).

           MOVE BY-REC TO BYCH-REC.

           PERFORM REWRITE-BY1-FILE.
  
           IF BY-REC NOT = BYOR-REC OR
               BR-REC NOT = BROR-REC
              PERFORM LOG-MAINTENANCE-CHGS.

       END-WRITE-MODULE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-BY1-FILE.

      **************************************
       LOG-MAINTENANCE-CHGS SECTION.
      **************************************
           MOVE SPACES          TO WORK-ERRLOG-DETAIL.
           MOVE BY-DATE-CCYYMM  TO WORK-ERRLOG-BY-DATE.
           MOVE BY-BRNO   TO WORK-ERRLOG-BY-BRNO.
           MOVE BY-CLASS  TO WORK-ERRLOG-BY-CLASS.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF BROR-NO-FT-EMP NOT = BRCH-NO-FT-EMP
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "FULL TIME  "           TO WORK-ERRLOG-DESC
             MOVE BROR-NO-FT-EMP          TO WORK-ERRLOG-ORG-N0
             MOVE BRCH-NO-FT-EMP          TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-NO-PT-EMP NOT = BRCH-NO-PT-EMP
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "PART TIME  "           TO WORK-ERRLOG-DESC
             MOVE BROR-NO-PT-EMP          TO WORK-ERRLOG-ORG-N0
             MOVE BRCH-NO-PT-EMP          TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF BYOR-PROJ-INCREASE NOT = BYCH-PROJ-INCREASE
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "PROJ INCREA"           TO WORK-ERRLOG-DESC
             MOVE BYOR-PROJ-INCREASE      TO WORK-ERRLOG-ORG-N0
             MOVE BYCH-PROJ-INCREASE      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF BYOR-PROJ-PROFIT NOT = BYCH-PROJ-PROFIT
             MOVE "04 "                  TO WORK-ERRLOG-FIELD
             MOVE "PROJ PROFIT"          TO WORK-ERRLOG-DESC
             MOVE BYOR-PROJ-PROFIT      TO WORK-ERRLOG-ORG-N0
             MOVE BYCH-PROJ-PROFIT      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF BYOR-30-DAY-MOVED NOT = BYCH-30-DAY-MOVED
             MOVE "05 "                  TO WORK-ERRLOG-FIELD
             MOVE "30DAY MOVED"          TO WORK-ERRLOG-DESC
             MOVE BYOR-30-DAY-MOVED      TO WORK-ERRLOG-ORG-N0
             MOVE BYCH-30-DAY-MOVED      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF BYOR-STD-POS NOT = BYCH-STD-POS
             MOVE "06 "                  TO WORK-ERRLOG-FIELD
             MOVE "LOAN VOL MV"          TO WORK-ERRLOG-DESC
             MOVE BYOR-STD-POS      TO WORK-ERRLOG-ORG-N0
             MOVE BYCH-STD-POS      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF BYOR-NO-REPO(1 1 1) NOT = BYCH-NO-REPO(1 1 1)
             MOVE "07 "                  TO WORK-ERRLOG-FIELD
             MOVE "# WCBC REPO"          TO WORK-ERRLOG-DESC
             MOVE BYOR-NO-REPO(1 1 1)      TO WORK-ERRLOG-ORG-N0
             MOVE BYCH-NO-REPO(1 1 1)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF BYOR-AMT-REPO(1 1 1) NOT = BYCH-AMT-REPO(1 1 1)
             MOVE "08 "                  TO WORK-ERRLOG-FIELD
             MOVE "# WCBC REPO"          TO WORK-ERRLOG-DESC
             MOVE BYOR-AMT-REPO(1 1 1)      TO WORK-ERRLOG-ORG-N2
             MOVE BYCH-AMT-REPO(1 1 1)      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-TAX-PREBOOK-CUST NOT = BRCH-TAX-PREBOOK-CUST
             MOVE "09 "                  TO WORK-ERRLOG-FIELD
             MOVE "ELECTR TAX "          TO WORK-ERRLOG-DESC
             MOVE BROR-TAX-PREBOOK-CUST  TO WORK-ERRLOG-ORG-N0
             MOVE BRCH-TAX-PREBOOK-CUST  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-TAX-PREBOOK-NONCUST NOT = BRCH-TAX-PREBOOK-NONCUST
             MOVE "10 "                  TO WORK-ERRLOG-FIELD
             MOVE "PAPER  TAX "          TO WORK-ERRLOG-DESC
             MOVE BROR-TAX-PREBOOK-NONCUST  TO WORK-ERRLOG-ORG-N0
             MOVE BRCH-TAX-PREBOOK-NONCUST  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

      **********************************************
       LOG-FIELD-CHANGE SECTION.
      **********************************************
           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE BR-NO-FT-EMP            TO F01.
           MOVE BR-NO-PT-EMP            TO F02.
           MOVE BY-PROJ-INCREASE        TO F03.
           MOVE BY-PROJ-PROFIT          TO F04.
           MOVE BY-30-DAY-MOVED         TO F05.
           MOVE BY-STD-POS              TO F06.
           MOVE BY-NO-REPO(1 1 1)       TO F07.
           MOVE BY-AMT-REPO(1 1 1)      TO F08.
           MOVE BR-TAX-PREBOOK-CUST     TO F09.
           MOVE BR-TAX-PREBOOK-NONCUST  TO F10.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.



      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO BYMISC-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BYMISC-SYSDATE.
           DISPLAY SPACES UPON BYMISC-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BYMISC-SCREEN UPON BYMISC-WINDOW-HANDLE.
           MOVE BYMISC-FORM-FIELDS TO WR-BUF.
           MOVE BYMISC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BYMISC-HELP--FILE TO HELP-LINK-FILE.
           MOVE BYMISC-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/BYMISC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


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
       COPY "LIBGB/GETENV.CPY".
       GET-TODAYS-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           MOVE ALP-MO TO DISPLAY-MO.
           MOVE ALP-YR TO DISPLAY-YR.
           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       VERIFY-BRNO.
           MOVE HOLD-BY-BRNO TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO BR-NAME.
           MOVE "S  A200BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.
       VERIFY-CLASS-CODE.
           MOVE HOLD-BY-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO LC-DESC.
           MOVE "S  A020CLSDESC." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
       BR-RECORD-DELETED.
           MOVE "BRANCH RECORD HAS BEEN DELETED!" TO MESS.
           PERFORM SEND-MESS.
       BY-RECORD-DELETED.
           MOVE "BRANCH STAT RECORD HAS BEEN DELETED!" TO MESS.
           PERFORM SEND-MESS.
       SEND-PROMPT.
           MOVE "S  A000PROMPT." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *******************************************
       IO-ROUTINES SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPBY1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *******************************************
       END-ROUTINE SECTION.
      *******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BYMISC-SCREEN.
           DESTROY BYMISC-WINDOW-HANDLE.
           EXIT PROGRAM.
