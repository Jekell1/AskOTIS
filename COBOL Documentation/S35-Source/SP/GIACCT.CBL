       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GIACCT.
       DATE-WRITTEN.    112116.
      ******************************************************************
      *
      *                    (SP/GIACCT.C)
      *
      *  CHANGE ONE ACCOUNT IN GIFILE WITH BRANCH & CLASS RANGE
      *  WILL ENTER LINE# OF ACCOUNT IN GI ACCOUNT TABLE
      *  WORLD PR# 1204
      *               
      * REV:
      * BLM 161221 DISPLAY COUNT OF CHANGED RECORDS WHEN COMPLETE,
      *            WORLD PR# 1204
      * KEC 2017-0213 ADDED 'GI' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * KEC 2019-0717 REMOVED REVOLVING CREDIT; ADJUST THE LINE-DESCRIPTIONS
      *               WORKERS TO MATCH THE ACTUAL LAYOUT OF GI-ACCOUNTS WHICH
      *               REVOLVING CREDIT FIELDS WERE REMOVED (115-127 IN A30);
      *               GI-PAYLBOX (127 IN A30; 115 IN A15); GI-ACCOUNTS AND
      *               LINE-DESCRIPTIONS CHANGED FROM 'OCCURS 133' TO
      *               'OCCURS 121'.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
 
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/GIACCT.CBL S34 06/19/21-12:22:21 >".

      ******************************************************************
      *
      *    P R O G R A M    W O R K E R S   
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       01  PR-IFN                   PIC X(2)       VALUE "PR".

       01  HOLD-BRANCH       PIC 9999.
       01  LN-FEED           PIC 9(2) VALUE ZERO.

       01  SPLIT-BR-ACCT     PIC 9(11)  VALUE 0.
       01  FILLER            REDEFINES SPLIT-BR-ACCT.
           03  SPLIT-BRNO    PIC 9(4).
           03  SPLIT-ACCT    PIC 9(7).
       01  DISPLAY-ACCT      PIC 9999/9999999.

       01  MESS-LIST         PIC X(5)  VALUE "MESS.".
       01  MESS-BUF          PIC X(40) VALUE SPACES.

       01  ORIGINAL-GI-REC   PIC X(GI-SIZE) VALUE " ".
       01  CHANGED-GI-REC    PIC X(GI-SIZE) VALUE " ".
       01  CHANGED-COUNT     PIC 9(8) VALUE 0.
       01  CHANGED-COUNTZ    PIC ZZ,ZZZ,ZZZ.

       01  LINE-DESCRIPTIONS.
           03  FILLER PIC X(46) VALUE
               "CASH ON HAND".
           03  FILLER PIC X(46) VALUE
              "CASH IN BANK".
           03  FILLER PIC X(46) VALUE
              "NSF".
           03  FILLER PIC X(46) VALUE
              "AR".
           03  FILLER PIC X(46) VALUE
              "PL RECEIVABLES".
           03  FILLER PIC X(46) VALUE
              "PL #2 RECEIVABLE".
           03  FILLER PIC X(46) VALUE
              "PL #3 RECEIVABLE".
           03  FILLER PIC X(46) VALUE
              "PL #4 RECEIVABLE".
           03  FILLER PIC X(46) VALUE
              "PL #5 RECEIVABLE".
           03  FILLER PIC X(46) VALUE
              "DEALER INSURANCE REBATE".
           03  FILLER PIC X(46) VALUE
              "IB ACCRUE INT AR".
      * -------------INTER BRANCH  12---------------
           03  FILLER PIC X(46) VALUE
               "OTHER BRANCH RECEIVABLE".
           03  FILLER PIC X(46) VALUE
               "OTHER BRANCH PAYABLE".
      * -------------UNEARNED CHARGES 14---------------
           03  FILLER PIC X(46) VALUE
               "UNEARNED INTEREST INCOME".
           03  FILLER PIC X(46) VALUE
               "UNEARNED ADD-ON INTEREST INCOME".
           03  FILLER PIC X(46) VALUE
               "UNEARNED SERVICE CHARGE INCOME".
           03  FILLER PIC X(46) VALUE
               "UNEARNED MAINTENANCE FEE INCOME".
           03  FILLER PIC X(46) VALUE
               "UNEARNED DEALER DISCOUNT OR COMMISSION".
           03  FILLER PIC X(46) VALUE
               "UNEARNED OTHER INCOME".
           03  FILLER PIC X(46) VALUE
               "UNEARNED 7".
           03  FILLER PIC X(46) VALUE
               "UNEARNED 8".
           03  FILLER PIC X(46) VALUE
               "UNEARNED 9".
           03  FILLER PIC X(46) VALUE
               "UNEARNED 10".
           03  FILLER PIC X(46) VALUE
               "UNEARNED CREDIT LIFE DECLINING".
           03  FILLER PIC X(46) VALUE
               "UNEARNED CREDIT LIFE LEVEL".
           03  FILLER PIC X(46) VALUE
               "UNEARNED A&H".
           03  FILLER PIC X(46) VALUE
               "UNEARNED PERSONAL PROPERTY".
           03  FILLER PIC X(46) VALUE
               "UNEARNED OTHER 1 INSURANCE".
           03  FILLER PIC X(46) VALUE
               "UNEARNED OTHER 2 INSURANCE".
           03  FILLER PIC X(46) VALUE
               "UNEARNED OTHER 3 INSURANCE".
           03  FILLER PIC X(46) VALUE
               "UNEARNED OTHER 4 INSURANCE".
           03  FILLER PIC X(46) VALUE
               "UNEARNED OTHER 5 INSURANCE".
           03  FILLER PIC X(46) VALUE
               "UNEARNED OTHER 6 INSURANCE".
           03  FILLER PIC X(46) VALUE
               "UNEARNED OTHER 7 INSURANCE".
           03  FILLER PIC X(46) VALUE
               "UNEARNED INTEREST REPO CHARGE".
           03  FILLER PIC X(46) VALUE
               "UNEARNED ADD-ON INTEREST REPO".
      * -------------EXPENSES  37---------------
           03  FILLER PIC X(46) VALUE
               "P&L EXPENSE".
           03  FILLER PIC X(46) VALUE
               "P&L #2 EXPENSE".
           03  FILLER PIC X(46) VALUE
               "P&L #3 EXPENSE".
           03  FILLER PIC X(46) VALUE
               "P&L #4 EXPENSE".
           03  FILLER PIC X(46) VALUE
               "P&L #5 EXPENSE".
           03  FILLER PIC X(46) VALUE
               "DEALER PARTICIPATION RESERVE EXPENSE".
      * -------------OFFSETS  43---------------
           03  FILLER PIC X(46) VALUE
               "P&L OFFSET".
           03  FILLER PIC X(46) VALUE
               "P&L #2 OFFSET".
           03  FILLER PIC X(46) VALUE
               "P&L #3 OFFSET".
           03  FILLER PIC X(46) VALUE
               "P&L #4 OFFSET".
           03  FILLER PIC X(46) VALUE
               "P&L #5 OFFSET".
           03  FILLER PIC X(46) VALUE
               "SERVICE ACCOUNT OFFSET".
      * -------------PAYABLES  49---------------
           03  FILLER PIC X(46) VALUE
               "CREDIT LIFE DECLINING PAYABLE".
           03  FILLER PIC X(46) VALUE
               "CREDIT LIFE LEVEL PAYABLE".
           03  FILLER PIC X(46) VALUE
               "ACCIDENT/HEALTH PAYABLE".
           03  FILLER PIC X(46) VALUE
               "PERSONAL PROPERTY PAYABLE".
           03  FILLER PIC X(46) VALUE
               "OTHER 1 INSURANCE PAYABLE".
           03  FILLER PIC X(46) VALUE
               "OTHER 2 INSURANCE PAYABLE".
           03  FILLER PIC X(46) VALUE
               "OTHER 3 INSURANCE PAYABLE".
           03  FILLER PIC X(46) VALUE
               "OTHER 4 INSURANCE PAYABLE".
           03  FILLER PIC X(46) VALUE
               "OTHER 5 INSURANCE PAYABLE".
           03  FILLER PIC X(46) VALUE
               "OTHER 6 INSURANCE PAYABLE".
           03  FILLER PIC X(46) VALUE
               "OTHER 7 INSURANCE PAYABLE".
           03  FILLER PIC X(46) VALUE
               "DEALER PAYABLE".
      * -------------RESERVES  61---------------
           03  FILLER PIC X(46) VALUE
               "DEALER RESERVE PRINCIPAL 1".
           03  FILLER PIC X(46) VALUE
               "DEALER RESERVE PRINCIPAL 2".
           03  FILLER PIC X(46) VALUE
               "DEALER RESERVE PRINCIPAL 3".
           03  FILLER PIC X(46) VALUE
               "DEALER RESERVE CHARGES 1".
           03  FILLER PIC X(46) VALUE
               "DEALER RESERVE CHARGES 2".
           03  FILLER PIC X(46) VALUE
               "DEALER RESERVE CHARGES 3".
      * ---------------------  67---------------
           03  FILLER PIC X(46) VALUE
               "DEALER PARTICIPATING RESERVE".
           03  FILLER PIC X(46) VALUE
               "ALLOTMENTS PAYABLE".
           03  FILLER PIC X(46) VALUE
               "DIRECT PAYABLE".
      * --------------INCOME  70----------------
           03  FILLER PIC X(46) VALUE
               "ANTICIPATED INTEREST".
           03  FILLER PIC X(46) VALUE
               "ANTICIPATED ADD-ON F/C".
           03  FILLER PIC X(46) VALUE
               "ANTICIPATED SERVICE CHARGES".
           03  FILLER PIC X(46) VALUE
               "ANTICIPATED MAINTENANCE FEES".
           03  FILLER PIC X(46) VALUE
               "ANTICIPATED DEALER DISCOUNT/COMMISSION".
           03  FILLER PIC X(46) VALUE
               "ANTICIPATED OTHER".
           03  FILLER PIC X(46) VALUE
               "EARN 7".
           03  FILLER PIC X(46) VALUE
               "EARN 8".
           03  FILLER PIC X(46) VALUE
               "EARN 9".
           03  FILLER PIC X(46) VALUE
               "EARN 10".
           03  FILLER PIC X(46) VALUE
               "LATE CHARGES".
           03  FILLER PIC X(46) VALUE
               "INTEREST".
           03  FILLER PIC X(46) VALUE
               "DEFERMENT INCOME".
           03  FILLER PIC X(46) VALUE
               "SERVICE ACCOUNT COMMISSION".
      * -------------------  84----------------
           03  FILLER PIC X(46) VALUE
               "P&L INTEREST".
           03  FILLER PIC X(46) VALUE
               "P&L #2 INTEREST/LATE CHARGES".
           03  FILLER PIC X(46) VALUE
               "P&L #3 INTEREST/LATE CHARGES".
           03  FILLER PIC X(46) VALUE
               "P&L #4 INTEREST/LATE CHARGES".
           03  FILLER PIC X(46) VALUE
               "P&L #5 INTEREST/LATE CHARGES".
           03  FILLER PIC X(46) VALUE
               "P&L RECOVERED".
           03  FILLER PIC X(46) VALUE
               "P&L #2 RECOVERED".
           03  FILLER PIC X(46) VALUE
               "P&L #3 RECOVERED".
           03  FILLER PIC X(46) VALUE
               "P&L #4 RECOVERED".
           03  FILLER PIC X(46) VALUE
               "P&L #5 RECOVERED".
           03  FILLER PIC X(46) VALUE
               "CREDIT LIFE DECLINING INSURANCE INCOME".
           03  FILLER PIC X(46) VALUE
               "CREDIT LIFE LEVEL INSURANCE INCOME".
           03  FILLER PIC X(46) VALUE
               "ACCIDENT/HEALTH INSURANCE INCOME".
           03  FILLER PIC X(46) VALUE
               "PERSONAL PROPERTY INSURANCE INCOME".
           03  FILLER PIC X(46) VALUE
               "OTHER 1 INSURANCE INCOME".
           03  FILLER PIC X(46) VALUE
               "OTHER 2 INSURANCE INCOME".
           03  FILLER PIC X(46) VALUE
               "OTHER 3 INSURANCE INCOME".
           03  FILLER PIC X(46) VALUE
               "OTHER 4 INSURANCE INCOME".
           03  FILLER PIC X(46) VALUE
               "OTHER 5 INSURANCE INCOME".
           03  FILLER PIC X(46) VALUE
               "OTHER 6 INSURANCE INCOME".
           03  FILLER PIC X(46) VALUE
               "OTHER 7 INSURANCE INCOME".
      * -------------------  105----------------
           03  FILLER PIC X(46) VALUE
               "P&L NEW AR/EXP".
           03  FILLER PIC X(46) VALUE
               "P&L #2 NEW AR/EXP".
           03  FILLER PIC X(46) VALUE
               "P&L #3 NEW AR/EXP".
           03  FILLER PIC X(46) VALUE
               "P&L #4 NEW AR/EXP (DLCOM GAP PP)".
           03  FILLER PIC X(46) VALUE
               "P&L #5 NEW AR/EXP (DLCOMM WARRANTY)".
      * -------------------  110----------------
           03  FILLER PIC X(46) VALUE
               "REPO A/R".
           03  FILLER PIC X(46) VALUE
               "REPO INTEREST".
           03  FILLER PIC X(46) VALUE
               "IB ACCRUED INTEREST A/R".
           03  FILLER PIC X(46) VALUE
               "CONVERSION OFFSET".
           03  FILLER PIC X(46) VALUE
               "PARTIAL CHARGEOFF EXPENSE".
      * ---REVOLVING OTHBAL PAYMENT GL TABLE  115-----------
      *    03  FILLER PIC X(46) VALUE
      *        "REVOLVING OTHER BALANCE CREDIT LIFE".
      *    03  FILLER PIC X(46) VALUE
      *        "REVOLVING OTHER BALANCE ACCIDENT/HEALTH".
      *    03  FILLER PIC X(46) VALUE
      *        "REVOLVING OTHER BALANCE PERSONAL PROPERTY".
      *    03  FILLER PIC X(46) VALUE
      *        "REVOLVING OTHER BALANCE OTHER 1".
      *    03  FILLER PIC X(46) VALUE
      *        "REVOLVING OTHER BALANCE MEMBERSHIP FEES".
      *    03  FILLER PIC X(46) VALUE
      *        "REVOLVING OTHER BALANCE NSF".
      *    03  FILLER PIC X(46) VALUE
      *        "REVOLVING OTHER BALANCE MISC 1".
      *    03  FILLER PIC X(46) VALUE
      *       "REVOLVING OTHER BALANCE MISC 2".
      *    03  FILLER PIC X(46) VALUE
      *       "REVOLVING OTHER BALANCE MISC 3".
      *    03  FILLER PIC X(46) VALUE
      *       "REVOLVING OTHER BALANCE MISC 4".
      *    03  FILLER PIC X(46) VALUE
      *       "REVOLVING OTHER BALANCE MISC 5".
      *    03  FILLER PIC X(46) VALUE
      *       "REVOLVING OTHER BALANCE MISC 6".
      * ------------------------- 127 --------------

      * ------------------------- 115 --------------
           03  FILLER PIC X(46) VALUE
              "LOCKBOX PAYABLE".
           03  FILLER PIC X(46) VALUE
              "PENALTY INTEREST".
           03  FILLER PIC X(46) VALUE
              "TPAY PAYABLE".
           03  FILLER PIC X(46) VALUE
              "TCOLL PAYABLE".
           03  FILLER PIC X(46) VALUE
              "MONEYGRAM PAYABLE".
           03  FILLER PIC X(46) VALUE
              "REPO ADD-ON EXPENSE".
           03  FILLER PIC X(46) VALUE
              "ACH BANK".
       01  LINE-DESC REDEFINES LINE-DESCRIPTIONS PIC X(46) OCCURS 121.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S   
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BR          PIC 9(4).
           03  END-BR          PIC 9(4).
           03  BEG-CL          PIC 9(2).
           03  END-CL          PIC 9(2).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  ACCT-LINE       PIC 9(3).
           03  NEW-BRNO        PIC 9(4).
           03  NEW-ACCT        PIC 9(7).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9(4)    VALUE 9999.
       01  SV-END-BR           PIC 9(4)    VALUE 9999.
      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S 
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBMN/MNLGGIW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGL/GLSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/GIACCT_DEF.CPY".
       COPY "LIBSP/GIACCT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/GIACCT_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
               
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *     M A I N    M O D U L E.
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GIACCT" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "UPDATE IN PROGRESS" TO MESS-BUF.
           PERFORM SEND-LEGEND.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.

           IF IO-FG = 9 OR BR-NO > END-BR
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           PERFORM PROCESS-GI-FILE.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           MOVE "UPDATE COMPLETED: &&&&&&&&&& RECORDS CHANGED." TO MESS.
           MOVE CHANGED-COUNT TO CHANGED-COUNTZ.
           INSPECT MESS REPLACING FIRST "&&&&&&&&&&" BY CHANGED-COUNTZ.
           PERFORM SEND-MESS.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE 0 TO Q-POS.
           MOVE 0 TO C-POS.
           MOVE 0 TO P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF ( EXT-EOBUF-ERRCD NOT = " " )
              GO TO ENDIT.
 
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-GI1-FILE.
 
           IF ( BEG-BR = 0 )
              IF ( END-BR = 0 )
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                   END-BR.
           GO TO INITIALIZE-EXIT.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO CL-01.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR NOT > END-BR GO TO CL-01.
           PERFORM INVALID-RANGE.
           GO TO BR-01.
      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
      *-----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CL.
      *-----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CL.
           IF BEG-CL NOT > END-CL GO TO ENTER-LINE.
           PERFORM INVALID-RANGE.
           GO TO CL-01.
      *-----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CL VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       ENTER-LINE.
           MOVE "ENNN030LINE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-LINE.
           MOVE VDUNUM0 TO ACCT-LINE.
           IF ACCT-LINE = 0 OR ACCT-LINE > GI-ACCT-IX-MAX
              GO TO ENTER-LINE.
           MOVE "S  A046LINEDESC." TO VDU.
           MOVE LINE-DESC(ACCT-LINE) TO VDUBUF.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       ENTER-NEW-BRNO.
           MOVE "ENNN040NEWBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-LINE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-NEW-BRNO.
           MOVE VDUNUM0 TO NEW-BRNO.
      *-----------------------------------------------------------------
       ENTER-NEW-ACCT.
           MOVE "ENNN070ACCT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "1<" 
              PERFORM SCAN-GL-ACCOUNTS
              IF GLSCAN-STATUS NOT = "00"
                 GO TO ENTER-NEW-ACCT.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-NEW-BRNO.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-NEW-ACCT.
           MOVE VDUNUM0 TO NEW-ACCT.
           PERFORM VERIFY-GL-ACCTS.
           IF IO-BAD
              MOVE "INVALID GL#: &&&&&&&&&&&&" TO MESS
              MOVE GL-BR-ACCOUNT TO DISPLAY-ACCT
              INSPECT DISPLAY-ACCT REPLACING FIRST "/" BY "-"
              INSPECT MESS REPLACING FIRST "&&&&&&&&&&&&" BY
                                           DISPLAY-ACCT
              PERFORM SEND-MESS
              GO TO ENTER-NEW-ACCT.
           GO TO EXIT-PARM.
       ENDIT-PARM.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-PARM.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

      ******************************************************************
       VERIFY-GL-ACCTS SECTION.
      ******************************************************************
           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.
           IF NEW-BRNO NOT = 0
              MOVE NEW-BRNO TO GL-BRANCH
              MOVE NEW-ACCT TO GL-ACCOUNT
              PERFORM READ-GL1-FILE
              GO TO EXIT-VERIFY-GL.

           MOVE "GL VERIFICATION IN PROGRESS" TO MESS-BUF.
           PERFORM SEND-LEGEND.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO VERIFY-GL-NEXT-BRNO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       VERIFY-GL-NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO VERIFY-GL-NEXT-BRNO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              MOVE 0 TO IO-FG
              GO TO EXIT-VERIFY-GL.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO VERIFY-GL-READ-GL.

      *-----------------------------------------------------------------
       VERIFY-GL-NEXT-BRNO.
           PERFORM READ-BR-FILE-NEXT.

           IF IO-FG = 9 OR BR-NO > END-BR
              MOVE 0 TO IO-FG
              GO TO EXIT-VERIFY-GL.

       VERIFY-GL-READ-GL.
           MOVE BR-NO TO GL-BRANCH.
           MOVE NEW-ACCT TO GL-ACCOUNT.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
              GO TO EXIT-VERIFY-GL.
           GO TO VERIFY-GL-NEXT-GROUP-LOC.

       EXIT-VERIFY-GL.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.

      ***********************************
       SCAN-GL-ACCOUNTS SECTION.
      ***********************************
           MOVE 0           TO GLSCAN-BRANCH
                               GLSCAN-ACCOUNT.
           IF NEW-BRNO NOT = 0
              MOVE NEW-BRNO TO GLSCAN-BRANCH.

           MOVE "WI/GLSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF GLSCAN-ACCOUNT NOT = 0
              MOVE "SNNN070ACCT." TO VDU
              MOVE GLSCAN-ACCOUNT TO VDUNUM0
              PERFORM SCREENIO.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.
      
              MOVE "SNNN020CL1." TO VDU.
              MOVE BEG-CL TO VDUNUM0.
              PERFORM SCREENIO.
              MOVE "SNNN020CL2." TO VDU.
              MOVE END-CL TO VDUNUM0.
              PERFORM SCREENIO.
      
              MOVE "SNNN030LINE." TO VDU.
              MOVE ACCT-LINE TO VDUNUM0.
              PERFORM SCREENIO.
      
              MOVE "SNNN040NEWBR." TO VDU.
              MOVE NEW-BRNO TO VDUNUM0.
              PERFORM SCREENIO.
      
              MOVE "SNNN070ACCT." TO VDU.
              MOVE NEW-ACCT TO VDUNUM0.
              PERFORM SCREENIO.

      ******************************************************************
       PROCESS-GI-FILE SECTION.
      ******************************************************************
           MOVE BR-NO  TO GI-BRANCH HOLD-BRANCH.
           MOVE BEG-CL TO GI-CLASS.

           PERFORM START-GI1-FILE.
       NEXT-GI.
           PERFORM READ-GI1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-PROCESS-GI-FILE.

           IF ( GI-BRANCH NOT = HOLD-BRANCH )
              GO TO EXIT-PROCESS-GI-FILE.

           IF ( GI-CLASS > END-CL )
              GO TO EXIT-PROCESS-GI-FILE.

      * ADD YOUR CHANGES HERE:

           IF NEW-BRNO = 0
              MOVE BR-NO       TO SPLIT-BRNO
           ELSE
              MOVE NEW-BRNO    TO SPLIT-BRNO.
           MOVE NEW-ACCT       TO SPLIT-ACCT.
           IF SPLIT-BR-ACCT NOT = GI-BR-ACCOUNT(ACCT-LINE)
              MOVE GI-REC TO ORIGINAL-GI-REC
              MOVE SPLIT-BR-ACCT TO GI-BR-ACCOUNT(ACCT-LINE)
              MOVE GI-REC TO CHANGED-GI-REC
              PERFORM MNLGGI-MODULE
              ADD 1 TO CHANGED-COUNT
              PERFORM REWRITE-GI1-FILE.

           GO TO NEXT-GI.

       EXIT-PROCESS-GI-FILE.
           EXIT.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO GIACCT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GIACCT-SYSDATE.
           DISPLAY GIACCT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GIACCT-FORM-FIELDS TO WR-BUF.
           MOVE GIACCT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE GIACCT-HELP--FILE TO HELP-LINK-FILE.
           MOVE GIACCT-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/GIACCT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y     S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBMN/MNLGGI.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
      *-----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGL/GLGI1IN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY GIACCT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
