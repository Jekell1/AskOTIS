       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DLLIST.
       DATE-WRITTEN. 02-22-84
      ******************************************************************
      *
      *    DESCRIPTION:  DEALER MASTER FILE LIST
      *
      *=================================================================
      * REV:
      * 010989 BAH REWROTE
      * 052391 BAH CHANGED FOR MULLEN, PRINT SINGLE LINE
      *            WITH NAME, ADR, CSZ, SIGN 3
      * 101995 JFF ADDED DEALER SCAN
      * 031097 BAH ADDED DL-RECOURSE-FMLA FOR FAIRCO
      * JTG 102098 ADDED DL-PARVPAID-FRMLA-IB & DL-PARVPAID-CODE-IB "C"
      *            INSTANT AUTO #744
      * 111198 GLF MADE MEMO DATE 2000 COMPATIBLE
      * 020104 GLB ADDED DL-ACH-FG, DL-BANK-ROUTING-NO, DL-BANK-ACCTNO
      * 021207 AMM UPDATED LISTING
      * BAH 110919 REPLACED RRFORM WITH SCREEN
      * BAH 111212 ADDED ACTIVE/INACTIVE/BOTH FOR MULLEN PR# 3920
      *  CS 160225 REMOVED USE OF DL-BRANCH,REPLACE WITH DL-BRNO
      *
      * KEC 2016.1222 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FIELD WORKERS TO CONTAIN FILE PREFIXES (DM).
      *
      * BAH 170425 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181011 GLOBAL DLFILE
      * BAH 181015 GLOBAL DMFILE
      * BAH 20190627 REMOVED DL-REVOLVING
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

      ******************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/DLLIST.CBL S35 02/21/24-12:02:50 >".
      ******************************

       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01DM.CPY".
       COPY "LIBLP/LP01DM_SQL.CPY".
       COPY "LIBLP/LPWSDM.CPY".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  TODAY-DATE      PIC 9(8).
       01  C-S-Z           PIC X(85).
       01  STATE           PIC XX.
       01  ZIP             PIC X(10).
       01  WK-BUF-1        PIC X(30).
       01  WK-BUF-2        PIC X(30).
       01  WK-BUF-3        PIC X(30).
       01  WK-BUF-4        PIC X(30).

       01  PRINT-LINE.
           03  RESULT-BUF.
               05  RESULT-BUF1  PIC X(68).
               05  RESULT-BUF2  PIC X(65).
       01  FIELD-BUF.
           03  FLD-BUF.
               05  FILLER       PIC X(14).
               05  FILLER       PIC X VALUE "!".
           03  FILLER REDEFINES FLD-BUF.
               05  FILLER       PIC X(13).
               05  CHAR-1-X.
                   07  CHAR-1       PIC X  JUST.
                   07  FILLER       PIC X.
           03  FILLER REDEFINES FLD-BUF.
               05  FILLER       PIC X(12).
               05  CHAR-2-X.
                   07  CHAR-2       PIC XX  JUST.
                   07  FILLER       PIC X.
           03  FILLER REDEFINES FLD-BUF.
               05  FILLER       PIC X(11).
               05  CHAR-3-X.
                   07  CHAR-3       PIC XXX  JUST.
                   07  FILLER       PIC X.
           03  FILLER REDEFINES FLD-BUF.
               05  FILLER       PIC X(7).
               05  CHAR-7-X.
                   07  CHAR-7       PIC X(7)  JUST.
                   07  FILLER       PIC X.
           03  FILLER REDEFINES FLD-BUF.
               05  FILLER       PIC X(5).
               05  CHAR-9-X.
                   07  CHAR-9       PIC X(9)  JUST.
                   07  FILLER       PIC X.

       01  WK-MONEY             PIC ZZZZZZZZZ.ZZ-.
       01  WK-ROUTING-NO        PIC Z(9).

       01  W-RATE           PIC ZZ9.999      BLANK ZERO.

       01  WK-NUM.
           03  WK-NUM-Z         PIC ZZZZZZZZZZZZZZZ.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  MEMO-DATE            PIC 9(8).
       01  MEMO-SW              PIC 9        VALUE ZERO.
       01  R-CNT                PIC 9.

       01  HOLD-DM1-KEY.
           03  HOLD-DM-DL-BRNO         PIC 9(4).
           03  HOLD-DM-DL-DLNO         PIC 9(4)V99.
           03  HOLD-DM-DATE            PIC 9(8).
           03  HOLD-DM-SEQ             PIC 9(3).

       01  DEALER-FG           PIC X(3)      VALUE "   ".
           88  DEALER-BEG                    VALUE "BEG".
           88  DEALER-END                    VALUE "END".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  SEQ             PIC 9.
           03  BEG-DLNO        PIC 9(4)V99.
           03  END-DLNO        PIC 9(4)V99.
           03  BEG-NAME        PIC X(16).
           03  END-NAME        PIC X(16).
           03  SINGLE-LINE     PIC X.
           03  MEMOS           PIC X.
           03  ACTIVE          PIC X.

       01  ACCEPT-BUF          PIC X.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           " - - - DEALER MASTER FILE LISTING - - -".
           03  FILLER           PIC X(45)    VALUE " ".

       01  HEAD3.
           03  FILLER      PIC X(7) VALUE "PAGE : ".
           03  D-PAGE      PIC 9.
           03  FILLER      PIC X(60)    VALUE SPACE.
           03  FILLER      PIC X(7) VALUE "PAGE : ".
           03  D-PAGE2     PIC 9.
           03  FILLER      PIC X(49)    VALUE SPACE.

       01  HEAD8.
           03  FILLER      PIC X(9)     VALUE " ".
           03  FILLER      PIC X(41)    VALUE
               "- - - - - - - - - - - - - - - - M E M O S".
           03  FILLER      PIC X(32)    VALUE
               " - - - - - - - - - - - - - - - -".

       01  HEAD9.
           03  FILLER      PIC X(11)    VALUE " ".
           03  FILLER      PIC XXXX     VALUE "DATE".
           03  FILLER      PIC XXXX     VALUE " ".
           03  FILLER      PIC XXX      VALUE "SEQ".
           03  FILLER      PIC XX       VALUE " ".
           03  FILLER      PIC X(11)    VALUE "DESCRIPTION".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-HEADTAG    PIC X(9).
           03  D-DLNO       PIC Z(4).99.
      *    03  FILLER       PIC X(3).
      *    03  FILLER       PIC X(1).
      *    03  D-DLNAME     PIC X(25).
           03  D-DLNAME     PIC X(16).
           03  FILLER       PIC X(1).
           03  D-ADR        PIC X(49).
           03  FILLER       PIC X.
           03  D-PHONE      PIC 999/999/9999  BLANK ZERO.
           03  FILLER       PIC X.
           03  D-PROMPT-FAX PIC X(4).
           03  D-FAX        PIC 999/999/9999  BLANK ZERO.
           03  FILLER       PIC X.
           03  D-SIGN       PIC X(19).

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(9).
           03  D-MDATE     PIC 99/99/99.
           03  FILLER      PIC XXX.
           03  D-SEQ       PIC Z9.
           03  FILLER      PIC XX.
           03  D-MEMO      PIC X(60).

       01  D-LINE3 REDEFINES DETAIL-LINE.
           03  GROUP-1                         PIC X(33).
           03  GROUP-1-A   REDEFINES GROUP-1.
               05  D-PROMPT1                   PIC X(9).
               05  FILLER                      PIC X(1).
               05  D-COLON1                    PIC X(1).
               05  FILLER                      PIC X(1).
               05  GROUP-1-A-A                 PIC X(12).
               05  GROUP-1-A-A-A               REDEFINES GROUP-1-A-A.
                   07  D-TERM1                 PIC Z(3).
                   07  FILLER                  PIC X(2).
                   07  D-RATE1                 PIC Z9.9999.
               05  GROUP-1-A-A-B               REDEFINES GROUP-1-A-A.
                   07  D-FORMULA1              PIC Z(2).
                   07  FILLER                  PIC X(10).
               05  GROUP-1-A-A-C               REDEFINES GROUP-1-A-A.
                   07  D-CODE1                 PIC X(1).
                   07  FILLER                  PIC X(11).
               05  GROUP-1-A-A-D               REDEFINES GROUP-1-A-A.
                   07  D-ADDON1                PIC ZZZZ9.99-.
                   07  FILLER                  PIC X(3).
               05  FILLER                      PIC X(9).
           03  GROUP-2                         PIC X(33).
           03  GROUP-2-A   REDEFINES GROUP-2.
               05  D-PROMPT2                   PIC X(9).
               05  FILLER                      PIC X(1).
               05  D-COLON2                    PIC X(1).
               05  FILLER                      PIC X(1).
               05  GROUP-2-A-A                 PIC X(12).
               05  GROUP-2-A-A-A               REDEFINES GROUP-2-A-A.
                   07  D-TERM2                 PIC Z(3).
                   07  FILLER                  PIC X(2).
                   07  D-RATE2                 PIC Z9.9999.
               05  GROUP-2-A-A-A-A             REDEFINES GROUP-2-A-A.
                   07  D-NUM2                  PIC Z(10).
               05  GROUP-2-A-A-B               REDEFINES GROUP-2-A-A.
                   07  D-FORMULA2              PIC Z(2).
                   07  FILLER                  PIC X(10).
               05  GROUP-2-A-A-C               REDEFINES GROUP-2-A-A.
                   07  D-CODE2                 PIC X(1).
                   07  FILLER                  PIC X(11).
               05  GROUP-2-A-A-D               REDEFINES GROUP-2-A-A.
                   07  D-ADDON2                PIC ZZZZ9.99-.
                   07  FILLER                  PIC X(3).
               05  GROUP-2-A-A-E               REDEFINES GROUP-2-A-A.
                   07  D-NAME2                 PIC X.
                   07  FILLER                  PIC X(11).
               05  FILLER                      PIC X(9).
           03  GROUP-3                         PIC X(33).
           03  GROUP-3-A   REDEFINES GROUP-3.
               05  D-PROMPT3                   PIC X(9).
               05  FILLER                      PIC X(1).
               05  D-COLON3                    PIC X(1).
               05  FILLER                      PIC X(1).
               05  GROUP-3-A-A                 PIC X(12).
               05  GROUP-3-A-A-A               REDEFINES GROUP-3-A-A.
                   07  D-TERM3                 PIC Z(3).
                   07  FILLER                  PIC X(2).
                   07  D-RATE3                 PIC Z9.9999.
               05  GROUP-3-A-A-B               REDEFINES GROUP-3-A-A.
                   07  D-FORMULA3              PIC Z(2).
                   07  FILLER                  PIC X(3).
                   07  D-EARN-FC3              PIC Z(4).
                   07  FILLER                  PIC X(3).
               05  GROUP-3-A-A-C               REDEFINES GROUP-3-A-A.
                   07  D-CODE3                 PIC X(1).
                   07  FILLER                  PIC X(11).
               05  GROUP-3-A-A-D               REDEFINES GROUP-3-A-A.
                   07  D-ADDON3                PIC ZZZZ9.99-.
                   07  FILLER                  PIC X(3).
               05  GROUP-3-A-A-E               REDEFINES GROUP-3-A-A.
                   07  FILLER                  PIC X(2).
                   07  D-TERM3-A               PIC Z(4).
                   07  D-PERCENT3              PIC X(1).
                   07  FILLER                  PIC X(5).
               05  FILLER                      PIC X(9).
           03  GROUP-4                         PIC X(33).
           03  GROUP-4-A   REDEFINES GROUP-4.
               05  D-PROMPT4                   PIC X(9).
               05  FILLER                      PIC X(1).
               05  D-COLON4                    PIC X(1).
               05  FILLER                      PIC X(1).
               05  GROUP-4-A-A                 PIC X(12).
               05  GROUP-4-A-A-A               REDEFINES GROUP-4-A-A.
                   07  D-TERM4                 PIC Z(3).
                   07  FILLER                  PIC X(2).
                   07  D-RATE4                 PIC Z9.9999.
               05  GROUP-4-A-A-B               REDEFINES GROUP-4-A-A.
                   07  D-FORMULA4              PIC Z(2).
                   07  FILLER                  PIC X(10).
               05  GROUP-4-A-A-C               REDEFINES GROUP-4-A-A.
                   07  D-CODE4                 PIC X(1).
                   07  FILLER                  PIC X(6).
                   07  D-CODE4-IB-PROMPT       PIC X(4).
                   07  D-CODE4-IB              PIC X.
                   07  FILLER                 REDEFINES D-CODE4-IB.
                       09  D-FRMLA4-IB         PIC X.
               05  GROUP-4-A-A-D               REDEFINES GROUP-4-A-A.
                   07  D-ADDON4                PIC ZZZZ9.99-.
                   07  FILLER                  PIC X(3).
               05  FILLER                      PIC X(9).

       01  D-LINE4 REDEFINES DETAIL-LINE.
           03  GROUP-5                         PIC X(66).
           03  GROUP-5-A   REDEFINES GROUP-5.
               05  D-PROMPT5-A                 PIC X(9).
               05  FILLER                      PIC X(1).
               05  D-COLON5-A                  PIC X(1).
               05  FILLER                      PIC X(1).
               05  GROUP-5-A-A                 PIC X(25).
               05  GROUP-5-A-A-A               REDEFINES GROUP-5-A-A.
                   07  D-NAME-FIELD            PIC X(25).
               05  GROUP-5-A-A-C               REDEFINES GROUP-5-A-A.
                   07  D-NUM-FIELD-3X          PIC Z(3).
                   07  FILLER                  PIC X(22).
               05  GROUP-5-A-A-D               REDEFINES GROUP-5-A-A.
                   07  D-NUM-FIELD-4X          PIC Z(4).
                   07  FILLER                  PIC X(21).
               05  GROUP-5-A-A-E               REDEFINES GROUP-5-A-A.
                   07  D-NUM-FIELD-10X         PIC Z(10).
                   07  FILLER                  PIC X(15).
               05  GROUP-5-A-A-F               REDEFINES GROUP-5-A-A.
                   07  D-NUM-FIELD-16X         PIC Z(16).
                   07  FILLER                  PIC X(9).
               05  GROUP-5-A-A-G               REDEFINES GROUP-5-A-A.
                   07  D-DATE-FIELD            PIC 99/99/99.
                   07  FILLER                  PIC X(17).
               05  FILLER                      PIC X(29).
           03  GROUP-5-B   REDEFINES GROUP-5.
               05  D-PROMPT5-B                 PIC X(21).
               05  FILLER                      PIC X(1).
               05  D-COLON5-B                  PIC X(1).
               05  FILLER                      PIC X(1).
               05  D-CEILING-NUM               PIC ZZZZZZ9.99.
               05  FILLER                      PIC X(32).
           03  GROUP-5-C   REDEFINES GROUP-5.
               05  FILLER                      PIC X(24).
               05  D-LPO-TXT                   PIC X(04).
           03  FILLER                          PIC X(66).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(25).
           03  RANGE-SELECTION  PIC X(16).
           03  RANGE-NUM REDEFINES RANGE-SELECTION  PIC 9(16).

       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  DLLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/DLLIST_DEF.CPY".
       COPY "LIBLS/DLLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/DLLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-DM1-FILE.
           PERFORM CLOSE-DL2-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DLLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS DLLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-DL.
           IF SEQ = 1
               PERFORM READ-DL1-FILE-NEXT
               IF IO-FG = 0
                   IF DL-DLNO > END-DLNO
                       GO TO END-ROUTINE.
           IF SEQ = 2
               PERFORM READ-DL2-FILE-NEXT
               IF IO-FG = 0
                   IF DL-SHORT > END-NAME
                       GO TO END-ROUTINE.

           IF IO-FG = 9 OR (DL-BRNO NOT = DL-PATH-OWNBR)
              GO TO END-ROUTINE.

      * INACTIVE ONLY
           IF ACTIVE = "I"
              IF DL-TERMDATE = 0
                 GO TO NEXT-DL
              ELSE
      * IF TERMINATION DATE IS GREATER THAN TODAY, STILL ACTIVE, SO SKIP
                 IF DL-TERMDATE > TODAY-DATE
                    GO TO NEXT-DL.

      * ACTIVE ONLY
           IF ACTIVE = "A"
              IF DL-TERMDATE NOT = 0
      * IF TERMINATION DATE IS LESS/EQUAL TO TODAY, ITS TERMINATED
                 IF DL-TERMDATE NOT > TODAY-DATE
                    GO TO NEXT-DL.

           PERFORM DETAIL-LINE-PROCESS.

           PERFORM MEMO-PROCESS.

           GO TO NEXT-DL.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE 0 TO SEQ.
           MOVE " " TO ERRCD.
       SETRP.
           MOVE " " TO ERRCD.
           MOVE DLLIST-QUEUE-POS TO Q-POS.
           MOVE DLLIST-COPIES-POS TO C-POS.
           MOVE DLLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00" GO TO ENDIT.
       RRSEQ.
           MOVE "ENNN010SEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RRSEQ.
           MOVE VDUNUM0 TO SEQ.
           IF SEQ NOT = "1"
              IF SEQ NOT = "2"
                 GO TO RRSEQ.
           IF SEQ = 1 GO TO DL-01.

           GO TO NM-01.

       DL-01.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO RRSEQ.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF IO-FG NOT = 0
                 GO TO DL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-01.
           MOVE VDUNUM2 TO BEG-DLNO.

       DL-02.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF IO-FG NOT = 0
                 GO TO DL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-02.
           MOVE VDUNUM2 TO END-DLNO.
           IF END-DLNO = 0
              GO TO DL-02.
           IF BEG-DLNO > END-DLNO
              MOVE "INVALID DEALER NUMBER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DL-01.
           GO TO SLINE.
       ALL-DL.
           MOVE "SNNN042DL1." TO VDU.
           MOVE 0 TO BEG-DLNO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99 TO END-DLNO VDUNUM2.
           PERFORM SCREENIO.
           GO TO SLINE.

       NM-01.
           MOVE "E  A160NM1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-NM.
           IF VDUSTATUS = "1U" GO TO RRSEQ.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO NM-01.
           MOVE VDUBUF TO BEG-NAME.
           IF BEG-NAME = "                " GO TO NM-01.

       NM-02.
           MOVE "E  A160NM2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO NM-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO NM-02.
           MOVE VDUBUF TO END-NAME.
           IF END-NAME = "                " GO TO NM-02.
           IF BEG-NAME > END-NAME
              MOVE "INVALID NAME RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO NM-01.

           GO TO SLINE.

       ALL-NM.
           MOVE "S  A160NM1." TO VDU.
           MOVE "                " TO BEG-NAME VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A160NM2." TO VDU.
           MOVE "ZZZZZZZZZZZZZZZZ" TO END-NAME VDUBUF.
           PERFORM SCREENIO.

       SLINE.
           MOVE "E  A010LINE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO RRSEQ.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SLINE.
           MOVE VDUBUF TO SINGLE-LINE.
           IF SINGLE-LINE = "Y" GO TO MEMO.
           IF SINGLE-LINE NOT = "N" GO TO SLINE.

       MEMO.
           MOVE "E  A010MEMOS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO SLINE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO MEMO.
           MOVE VDUBUF TO MEMOS.
           IF MEMOS = "Y" GO TO ACTIVE-AIB.
           IF MEMOS NOT = "N" GO TO MEMO.

       ACTIVE-AIB.
           MOVE "E  A010ACTIVE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO MEMO.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ACTIVE-AIB.
           MOVE VDUBUF TO ACTIVE.
           IF ACTIVE NOT = "A" AND NOT = "I" AND NOT = "B"
              GO TO ACTIVE-AIB.
       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "Y" GO TO EXPR.
           IF ACCEPT-BUF NOT = "N" GO TO OK-YN.

           GO TO RRSEQ.

       EXPR.
           MOVE 0 TO R-CNT SUB.
           PERFORM OPEN-DM1-FILE.
           PERFORM OPEN-DL1-FILE.

           MOVE DL-PATH-OWNBR TO DL-BRNO.

           IF SEQ = 1
              MOVE BEG-DLNO TO DL-DLNO
              PERFORM START-DL1-FILE
              GO TO SET-HEAD.
           IF SEQ = 2
              MOVE BEG-NAME TO DL-SHORT
              PERFORM START-DL2-FILE
              GO TO SET-HEAD.

           MOVE "1" TO ERRCD.
           GO TO ENDIT.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO TODAY-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************

           IF SINGLE-LINE = "Y"
              PERFORM CHECK-SINGLE-LINE
              MOVE DL-NAME TO D-DLNAME
              PERFORM STRING-ADDRESS
              MOVE DL-PHONE TO D-PHONE
              INSPECT D-PHONE REPLACING ALL "/" BY "-"
              MOVE "FAX " TO D-PROMPT-FAX
              MOVE DL-FAX-NUMBER TO D-FAX
              INSPECT D-FAX REPLACING ALL "/" BY "-"
              MOVE DL-SIGN3 TO D-SIGN
           ELSE
              PERFORM CHECK-PAGE
              MOVE "DEALER : " TO D-HEADTAG.
           MOVE DL-DLNO TO D-DLNO.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF SINGLE-LINE = "Y"
              GO TO END-DETAIL-LN-PROCESS.

           MOVE 1 TO D-PAGE.
           MOVE 2 TO D-PAGE2.
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 2 TO LN-FEED.

           MOVE "NAME     "   TO D-PROMPT5-A.
           MOVE ":"           TO D-COLON5-A.
           MOVE DL-NAME       TO D-NAME-FIELD.
           MOVE "MX PUR TM"   TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-MAXPURTRM  TO D-TERM3-A.
           MOVE "------------TERM--RATE--" TO GROUP-4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ADDR 1   "   TO D-PROMPT5-A.
           MOVE ":"           TO D-COLON5-A.
           MOVE DL-ADR1       TO D-NAME-FIELD.
           MOVE "MX PUR % "   TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-MAXPURPER  TO D-TERM3-A.
           MOVE "%"           TO D-PERCENT3.
           MOVE "BREAK1   "   TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-TERM(2, 1) TO D-TERM4.
           MOVE DL-RVPRIN-RATE(2, 1) TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ADDR 2   "   TO D-PROMPT5-A.
           MOVE ":"           TO D-COLON5-A.
           MOVE DL-ADR2       TO D-NAME-FIELD.
           MOVE "ERN CD/FC"   TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-EARNCD     TO D-FORMULA3.
           MOVE DL-EARNFAC    TO D-EARN-FC3.
           MOVE "BREAK2   "   TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-TERM(2, 2) TO D-TERM4.
           MOVE DL-RVPRIN-RATE(2, 2) TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CITY     "   TO D-PROMPT5-A.
           MOVE ":"           TO D-COLON5-A.
           MOVE DL-CITY       TO D-NAME-FIELD.
           MOVE "--RESERVE FROM PRIN #1--" TO GROUP-3.
           MOVE "BREAK3   "                TO D-PROMPT4.
           MOVE ":"                        TO D-COLON4.
           MOVE DL-RVPRIN-TERM(2, 3)       TO D-TERM4.
           MOVE DL-RVPRIN-RATE(2, 3)       TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "STATE   "    TO D-PROMPT5-A.
           MOVE ":"           TO D-COLON5-A.
           MOVE DL-STATE      TO D-NAME-FIELD.
           MOVE "FORMULA "    TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-RVPRIN-FRMLA(1) TO D-FORMULA3.
           MOVE "BREAK4  "    TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-TERM(2, 4) TO D-TERM4.
           MOVE DL-RVPRIN-RATE(2, 4) TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ZIP     "    TO D-PROMPT5-A.
           MOVE ":"           TO D-COLON5-A.
           MOVE DL-ZIP        TO D-NUM-FIELD-10X.
           MOVE "CODE    "    TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-RVPRIN-CODE(1) TO D-CODE3.
           MOVE "BREAK5  "    TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-TERM(2, 5) TO D-TERM4.
           MOVE DL-RVPRIN-RATE(2, 5) TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PHONE   "    TO D-PROMPT5-A.
           MOVE ":"           TO D-COLON5-A.
           MOVE DL-PHONE      TO D-NUM-FIELD-10X.
           MOVE "FAX     "    TO D-PROMPT2.
           MOVE ":"           TO D-COLON2.
           MOVE DL-FAX-NUMBER TO D-NUM2.
           MOVE "ADDON   "    TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-RVPRIN-ADDON(1) TO D-ADDON3.
           MOVE "BREAK6  "    TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-TERM(2, 6) TO D-TERM4.
           MOVE DL-RVPRIN-RATE(2, 6) TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ABRV-NM "    TO D-PROMPT5-A.
           MOVE ":"           TO D-COLON5-A.
           MOVE DL-SHORT      TO D-NUM-FIELD-16X.
           MOVE "------------TERM--RATE--" TO GROUP-3.
           MOVE "--RESERVE FROM PRIN #3--" TO GROUP-4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BRANCH  "    TO D-PROMPT5-A.
           MOVE ":"           TO D-COLON5-A.
           MOVE DL-BRNO       TO D-NUM-FIELD-4X.
           MOVE "BREAK1  "    TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-RVPRIN-TERM(1, 1) TO D-TERM3.
           MOVE DL-RVPRIN-RATE(1, 1) TO D-RATE3.
           MOVE "FORMULA "    TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-FRMLA(3) TO D-FORMULA4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PURPOSE"     TO D-PROMPT5-A.
           MOVE ":"           TO D-COLON5-A.
           MOVE DL-PURPOSE    TO D-NUM-FIELD-3X.
           MOVE "BREAK2 "     TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-RVPRIN-TERM(1, 2) TO D-TERM3.
           MOVE DL-RVPRIN-RATE(1, 2) TO D-RATE3.
           MOVE "CODE   "     TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-CODE(3) TO D-CODE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ORIG-DT"     TO D-PROMPT5-A.
           MOVE ":"           TO D-COLON5-A.
           MOVE DL-ORIGDATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-DATE-FIELD.
           MOVE "BREAK3 "     TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-RVPRIN-TERM(1, 3) TO D-TERM3.
           MOVE DL-RVPRIN-RATE(1, 3) TO D-RATE3.
           MOVE "ADDON  "     TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-ADDON(3)   TO D-ADDON4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TERM-DT"     TO D-PROMPT5-A.
           MOVE ":"           TO D-COLON5-A.
           MOVE DL-TERMDATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-DATE-FIELD.
           MOVE "BREAK4 "     TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-RVPRIN-TERM(1, 4) TO D-TERM3.
           MOVE DL-RVPRIN-RATE(1, 4) TO D-RATE3.
           MOVE "------------TERM--RATE--" TO GROUP-4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MFG-CD "     TO D-PROMPT5-A.
           MOVE ":"           TO D-COLON5-A.
           MOVE DL-MFGCODE    TO D-NAME-FIELD.
           MOVE "BREAK5 "     TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-RVPRIN-TERM(1, 5) TO D-TERM3.
           MOVE DL-RVPRIN-RATE(1, 5) TO D-RATE3.
           MOVE "BREAK1 "     TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-TERM(3, 1) TO D-TERM4.
           MOVE DL-RVPRIN-RATE(3, 1) TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AFP " TO D-PROMPT5-A.
           MOVE ":" TO D-COLON5-A.
           MOVE DL-AFP-DEALER TO D-NAME-FIELD.
           MOVE SPACES        TO D-PROMPT2 D-COLON2 D-NAME2.
           MOVE "BREAK6 "     TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-RVPRIN-TERM(1, 6) TO D-TERM3.
           MOVE DL-RVPRIN-RATE(1, 6) TO D-RATE3.
           MOVE "BREAK2 "     TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-TERM(3, 2) TO D-TERM4.
           MOVE DL-RVPRIN-RATE(3, 2) TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECOURSE FRMLA " TO D-PROMPT5-A.
           MOVE ":" TO D-COLON5-A.
           MOVE DL-RECOURSE-FRMLA TO D-NAME-FIELD.
           MOVE "---RESERVE FROM PRINCIPAL #2--" TO GROUP-3.
           MOVE "BREAK3 "     TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-TERM(3, 3) TO D-TERM4.
           MOVE DL-RVPRIN-RATE(3, 3) TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FULL RECOURSE CEILING" TO D-PROMPT5-B.
           MOVE ":"           TO D-COLON5-B.
           MOVE DL-FULLCEIL   TO D-CEILING-NUM.
           MOVE "FORMULA "    TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-RVPRIN-FRMLA(2) TO D-FORMULA3.
           MOVE "BREAK4  "    TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-TERM(3, 4) TO D-TERM4.
           MOVE DL-RVPRIN-RATE(3, 4) TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PART RECOURSE CEILING" TO D-PROMPT5-B.
           MOVE ":"           TO D-COLON5-B.
           MOVE DL-PARTCEIL   TO D-CEILING-NUM.
           MOVE "CODE    "    TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-RVPRIN-CODE(2) TO D-CODE3.
           MOVE "BREAK5  "    TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-TERM(3, 5) TO D-TERM4.
           MOVE DL-RVPRIN-RATE(3, 5) TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NON  RECOURSE CEILING" TO D-PROMPT5-B.
           MOVE ":"           TO D-COLON5-B.
           MOVE DL-NONCEIL    TO D-CEILING-NUM.
           MOVE "ADDON   "    TO D-PROMPT3.
           MOVE ":"           TO D-COLON3.
           MOVE DL-RVPRIN-ADDON(2) TO D-ADDON3.
           MOVE "BREAK6  "    TO D-PROMPT4.
           MOVE ":"           TO D-COLON4.
           MOVE DL-RVPRIN-TERM(3, 6) TO D-TERM4.
           MOVE DL-RVPRIN-RATE(3, 6) TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FULL RECOURSE BALANCE" TO D-PROMPT5-B.
           MOVE ":"             TO D-COLON5-B.
           MOVE DL-FULLCEIL-BAL TO D-CEILING-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PART RECOURSE BALANCE" TO D-PROMPT5-B.
           MOVE ":"             TO D-COLON5-B.
           MOVE DL-PARTCEIL-BAL TO D-CEILING-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NON  RECOURSE BALANCE" TO D-PROMPT5-B.
           MOVE ":"             TO D-COLON5-B.
           MOVE DL-NONCEIL-BAL  TO D-CEILING-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN PROCESS. OFFICE " TO D-PROMPT5-B.
           MOVE ":"             TO D-COLON5-B.
           MOVE DL-LPO          TO D-LPO-TXT.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CHECK-PAGE.
           MOVE 3 TO D-PAGE.
           MOVE 4 TO D-PAGE2.
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 2 TO LN-FEED.


           MOVE "----RESERVE FROM CHARGES #1----" TO GROUP-1.
           MOVE "------------TERM--RATE---------" TO GROUP-2.
           MOVE "----PARTIC. RESERVE WITHHELD---" TO GROUP-3.
           MOVE "------PARTIC. RESERVE PAID-----" TO GROUP-4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FORMULA"             TO D-PROMPT1.
           MOVE ":"                   TO D-COLON1.
           MOVE DL-RVCHGS-FRMLA(1)    TO D-FORMULA1.
           MOVE "BREAK 1"             TO D-PROMPT2.
           MOVE ":"                   TO D-COLON2.
           MOVE DL-RVCHGS-TERM(2, 1)  TO D-TERM2.
           MOVE DL-RVCHGS-RATE(2, 1)  TO D-RATE2.
           MOVE "FORMULA"             TO D-PROMPT3.
           MOVE ":"                   TO D-COLON3.
           MOVE DL-PARVHELD-FRMLA     TO D-FORMULA3.
           MOVE "FORMULA"             TO D-PROMPT4.
           MOVE ":"                   TO D-COLON4.
           MOVE DL-PARVPAID-FRMLA     TO D-FORMULA4.
           MOVE "IB: "                TO D-CODE4-IB-PROMPT.
           MOVE DL-PARVPAID-FRMLA-IB  TO D-FRMLA4-IB.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CODE   "             TO D-PROMPT1.
           MOVE ":"                   TO D-COLON1.
           MOVE DL-RVCHGS-CODE(1)     TO D-CODE1.
           MOVE "BREAK 2"             TO D-PROMPT2.
           MOVE ":"                   TO D-COLON2.
           MOVE DL-RVCHGS-TERM(2, 2)  TO D-TERM2.
           MOVE DL-RVCHGS-RATE(2, 2)  TO D-RATE2.
           MOVE "CODE   "             TO D-PROMPT3.
           MOVE ":"                   TO D-COLON3.
           MOVE DL-PARVHELD-CODE      TO D-CODE3.
           MOVE "CODE   "             TO D-PROMPT4.
           MOVE ":"                   TO D-COLON4.
           MOVE DL-PARVPAID-CODE      TO D-CODE4.
           MOVE "IB: "                TO D-CODE4-IB-PROMPT.
           MOVE DL-PARVPAID-CODE-IB   TO D-CODE4-IB.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ADDON  "             TO D-PROMPT1.
           MOVE ":"                   TO D-COLON1.
           MOVE DL-RVCHGS-ADDON(1)    TO D-ADDON1.
           MOVE "BREAK 3"             TO D-PROMPT2.
           MOVE ":"                   TO D-COLON2.
           MOVE DL-RVCHGS-TERM(2, 3)  TO D-TERM2.
           MOVE DL-RVCHGS-RATE(2, 3)  TO D-RATE2.
           MOVE "ADDON  "             TO D-PROMPT3.
           MOVE ":"                   TO D-COLON3.
           MOVE DL-PARVHELD-ADDON     TO D-ADDON3.
           MOVE "ADDON  "             TO D-PROMPT4.
           MOVE ":"                   TO D-COLON4.
           MOVE DL-PARVPAID-ADDON     TO D-ADDON4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BREAK 4"             TO D-PROMPT2.
           MOVE ":"                   TO D-COLON2.
           MOVE DL-RVCHGS-TERM(2, 4)  TO D-TERM2.
           MOVE DL-RVCHGS-RATE(2, 4)  TO D-RATE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "            TERM--RATES          " TO GROUP-1.
           MOVE "BREAK 5"             TO D-PROMPT2.
           MOVE ":"                   TO D-COLON2.
           MOVE DL-RVCHGS-TERM(2, 5)  TO D-TERM2.
           MOVE DL-RVCHGS-RATE(2, 5)  TO D-RATE2.
           MOVE "            TERM--RATES          " TO GROUP-3.
           MOVE "            TERM--RATES          " TO GROUP-4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BREAK 1"               TO D-PROMPT1.
           MOVE ":"                     TO D-COLON1.
           MOVE DL-RVCHGS-TERM(1, 1)    TO D-TERM1.
           MOVE DL-RVCHGS-RATE(1, 1)    TO D-RATE1.
           MOVE "BREAK 6"               TO D-PROMPT2.
           MOVE ":"                     TO D-COLON2.
           MOVE DL-RVCHGS-TERM(2, 6)    TO D-TERM2.
           MOVE DL-RVCHGS-RATE(2, 6)    TO D-RATE2.
           MOVE "BREAK 1"               TO D-PROMPT3.
           MOVE ":"                     TO D-COLON3.
           MOVE DL-PARVHELD-TERM(1)     TO D-TERM3.
           MOVE DL-PARVHELD-RATE(1)     TO D-RATE3.
           MOVE "BREAK 1"               TO D-PROMPT4.
           MOVE ":"                     TO D-COLON4.
           MOVE DL-PARVPAID-TERM(1)     TO D-TERM4.
           MOVE DL-PARVPAID-RATE(1)     TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BREAK 2"               TO D-PROMPT1.
           MOVE ":"                     TO D-COLON1.
           MOVE DL-RVCHGS-TERM(1, 2)    TO D-TERM1.
           MOVE DL-RVCHGS-RATE(1, 2)    TO D-RATE1.
           MOVE "----RESERVE FROM CHARGES #3----" TO GROUP-2.
           MOVE "BREAK 2"               TO D-PROMPT3.
           MOVE ":"                     TO D-COLON3.
           MOVE DL-PARVHELD-TERM(2)     TO D-TERM3.
           MOVE DL-PARVHELD-RATE(2)     TO D-RATE3.
           MOVE "BREAK 2"               TO D-PROMPT4.
           MOVE ":"                     TO D-COLON4.
           MOVE DL-PARVPAID-TERM(2)     TO D-TERM4.
           MOVE DL-PARVPAID-RATE(2)     TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BREAK 3"               TO D-PROMPT1.
           MOVE ":"                     TO D-COLON1.
           MOVE DL-RVCHGS-TERM(1, 3)    TO D-TERM1.
           MOVE DL-RVCHGS-RATE(1, 3)    TO D-RATE1.
           MOVE "FORMULA"               TO D-PROMPT2.
           MOVE ":"                     TO D-COLON2.
           MOVE DL-RVCHGS-FRMLA(3)      TO D-FORMULA2.
           MOVE "BREAK 3"               TO D-PROMPT3.
           MOVE ":"                     TO D-COLON3.
           MOVE DL-PARVHELD-TERM(3)     TO D-TERM3.
           MOVE DL-PARVHELD-RATE(3)     TO D-RATE3.
           MOVE "BREAK 3"               TO D-PROMPT4.
           MOVE ":"                     TO D-COLON4.
           MOVE DL-PARVPAID-TERM(3)     TO D-TERM4.
           MOVE DL-PARVPAID-RATE(3)     TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BREAK 4"               TO D-PROMPT1.
           MOVE ":"                     TO D-COLON1.
           MOVE DL-RVCHGS-TERM(1, 4)    TO D-TERM1.
           MOVE DL-RVCHGS-RATE(1, 4)    TO D-RATE1.
           MOVE "CODE   "               TO D-PROMPT2.
           MOVE ":"                     TO D-COLON2.
           MOVE DL-RVCHGS-CODE(3)       TO D-CODE2.
           MOVE "BREAK 4"               TO D-PROMPT3.
           MOVE ":"                     TO D-COLON3.
           MOVE DL-PARVHELD-TERM(4)     TO D-TERM3.
           MOVE DL-PARVHELD-RATE(4)     TO D-RATE3.
           MOVE "BREAK 4"               TO D-PROMPT4.
           MOVE ":"                     TO D-COLON4.
           MOVE DL-PARVPAID-TERM(4)     TO D-TERM4.
           MOVE DL-PARVPAID-RATE(4)     TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BREAK 5"               TO D-PROMPT1.
           MOVE ":"                     TO D-COLON1.
           MOVE DL-RVCHGS-TERM(1, 5)    TO D-TERM1.
           MOVE DL-RVCHGS-RATE(1, 5)    TO D-RATE1.
           MOVE "ADDON  "               TO D-PROMPT2.
           MOVE ":"                     TO D-COLON2.
           MOVE DL-RVCHGS-ADDON(3)      TO D-ADDON2.
           MOVE "BREAK 5"               TO D-PROMPT3.
           MOVE ":"                     TO D-COLON3.
           MOVE DL-PARVHELD-TERM(5)     TO D-TERM3.
           MOVE DL-PARVHELD-RATE(5)     TO D-RATE3.
           MOVE "BREAK 5"               TO D-PROMPT4.
           MOVE ":"                     TO D-COLON4.
           MOVE DL-PARVPAID-TERM(5)     TO D-TERM4.
           MOVE DL-PARVPAID-RATE(5)     TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BREAK 6"               TO D-PROMPT1.
           MOVE ":"                     TO D-COLON1.
           MOVE DL-RVCHGS-TERM(1, 6)    TO D-TERM1.
           MOVE DL-RVCHGS-RATE(1, 6)    TO D-RATE1.
           MOVE "            TERM--RATES          " TO GROUP-2.
           MOVE "BREAK 6"               TO D-PROMPT3.
           MOVE ":"                     TO D-COLON3.
           MOVE DL-PARVHELD-TERM(6)     TO D-TERM3.
           MOVE DL-PARVHELD-RATE(6)     TO D-RATE3.
           MOVE "BREAK 6"               TO D-PROMPT4.
           MOVE ":"                     TO D-COLON4.
           MOVE DL-PARVPAID-TERM(6)     TO D-TERM4.
           MOVE DL-PARVPAID-RATE(6)     TO D-RATE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BREAK 1"               TO D-PROMPT2.
           MOVE ":"                     TO D-COLON2.
           MOVE DL-RVCHGS-TERM(3, 1)    TO D-TERM2.
           MOVE DL-RVCHGS-RATE(3, 1)    TO D-RATE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "----RESERVE FROM CHARGES #2----" TO GROUP-1.
           MOVE "BREAK 2"               TO D-PROMPT2.
           MOVE ":"                     TO D-COLON2.
           MOVE DL-RVCHGS-TERM(3, 2)    TO D-TERM2.
           MOVE DL-RVCHGS-RATE(3, 2)    TO D-RATE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FORMULA"               TO D-PROMPT1.
           MOVE ":"                     TO D-COLON1.
           MOVE DL-RVCHGS-FRMLA(2)      TO D-FORMULA1.
           MOVE "BREAK 3"               TO D-PROMPT2.
           MOVE ":"                     TO D-COLON2.
           MOVE DL-RVCHGS-TERM(3, 3)    TO D-TERM2.
           MOVE DL-RVCHGS-RATE(3, 3)    TO D-RATE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CODE   "               TO D-PROMPT1.
           MOVE ":"                     TO D-COLON1.
           MOVE DL-RVCHGS-CODE(2)       TO D-CODE1.
           MOVE "BREAK 4"               TO D-PROMPT2.
           MOVE ":"                     TO D-COLON2.
           MOVE DL-RVCHGS-TERM(3, 4)    TO D-TERM2.
           MOVE DL-RVCHGS-RATE(3, 4)    TO D-RATE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ADDON  "               TO D-PROMPT1.
           MOVE ":"                     TO D-COLON1.
           MOVE DL-RVCHGS-ADDON(2)      TO D-ADDON1.
           MOVE "BREAK 5"               TO D-PROMPT2.
           MOVE ":"                     TO D-COLON2.
           MOVE DL-RVCHGS-TERM(3, 5)    TO D-TERM2.
           MOVE DL-RVCHGS-RATE(3, 5)    TO D-RATE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BREAK 6"               TO D-PROMPT2.
           MOVE ":"                     TO D-COLON2.
           MOVE DL-RVCHGS-TERM(3, 6)    TO D-TERM2.
           MOVE DL-RVCHGS-RATE(3, 6)    TO D-RATE2.
           PERFORM WRITE-DETAIL-LINE.


           PERFORM CHECK-PAGE.
           MOVE 5 TO D-PAGE.
           MOVE 6 TO D-PAGE2.
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 2 TO LN-FEED.


           MOVE "DEALER DISC FORM:          !" TO WK-BUF-1.
           MOVE DL-DISC-FRMLA TO WK-NUM-Z.
           MOVE WK-NUM TO CHAR-2.
           MOVE CHAR-2-X TO WK-BUF-2.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE "SIGNATURE #1 :" TO RESULT-BUF2.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEALER DISC CODE:          !" TO WK-BUF-1.
           MOVE DL-DISC-CODE TO CHAR-1.
           MOVE CHAR-1-X TO WK-BUF-2.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE "       !" TO WK-BUF-1.
           MOVE DL-SIGN1 TO WK-BUF-2.
           STRING WK-BUF-1 WK-BUF-2
                  DELIMITED BY "!"
                  INTO RESULT-BUF2.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACE TO RESULT-BUF.
           MOVE "-----------------TERM--RATE-- " TO RESULT-BUF1.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEAL DISC BREAK1: !" TO WK-BUF-1.
           MOVE DL-DISC-TERM(1) TO WK-NUM-Z.
           MOVE WK-NUM TO CHAR-3.
           MOVE CHAR-3-X TO WK-BUF-2.
           MOVE " !" TO WK-BUF-3.
           MOVE DL-DISC-RATE(1) TO W-RATE.
           MOVE W-RATE TO CHAR-7.
           MOVE CHAR-7-X TO WK-BUF-4.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2 WK-BUF-3 WK-BUF-4
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE "SIGNATURE #2 :" TO RESULT-BUF2.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.


           MOVE "DEAL DISC BREAK2: !" TO WK-BUF-1.
           MOVE DL-DISC-TERM(2) TO WK-NUM-Z.
           MOVE WK-NUM TO CHAR-3.
           MOVE CHAR-3-X TO WK-BUF-2.
           MOVE " !" TO WK-BUF-3.
           MOVE DL-DISC-RATE(2) TO W-RATE.
           MOVE W-RATE TO CHAR-7.
           MOVE CHAR-7-X TO WK-BUF-4.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2 WK-BUF-3 WK-BUF-4
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE "       !" TO WK-BUF-1.
           MOVE DL-SIGN2 TO WK-BUF-2.
           STRING WK-BUF-1 WK-BUF-2
                  DELIMITED BY "!"
                  INTO RESULT-BUF2.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEAL DISC BREAK3: !" TO WK-BUF-1.
           MOVE DL-DISC-TERM(3) TO WK-NUM-Z.
           MOVE WK-NUM TO CHAR-3.
           MOVE CHAR-3-X TO WK-BUF-2.
           MOVE " !" TO WK-BUF-3.
           MOVE DL-DISC-RATE(3) TO W-RATE.
           MOVE W-RATE TO CHAR-7.
           MOVE CHAR-7-X TO WK-BUF-4.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2 WK-BUF-3 WK-BUF-4
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEAL DISC BREAK4: !" TO WK-BUF-1.
           MOVE DL-DISC-TERM(4) TO WK-NUM-Z.
           MOVE WK-NUM TO CHAR-3.
           MOVE CHAR-3-X TO WK-BUF-2.
           MOVE " !" TO WK-BUF-3.
           MOVE DL-DISC-RATE(4) TO W-RATE.
           MOVE W-RATE TO CHAR-7.
           MOVE CHAR-7-X TO WK-BUF-4.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2 WK-BUF-3 WK-BUF-4
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE "SIGNATURE #3 :" TO RESULT-BUF2.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEAL DISC BREAK5: !" TO WK-BUF-1.
           MOVE DL-DISC-TERM(5) TO WK-NUM-Z.
           MOVE WK-NUM TO CHAR-3.
           MOVE CHAR-3-X TO WK-BUF-2.
           MOVE " !" TO WK-BUF-3.
           MOVE DL-DISC-RATE(5) TO W-RATE.
           MOVE W-RATE TO CHAR-7.
           MOVE CHAR-7-X TO WK-BUF-4.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2 WK-BUF-3 WK-BUF-4
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE "       !" TO WK-BUF-1.
           MOVE DL-SIGN3 TO WK-BUF-2.
           STRING WK-BUF-1 WK-BUF-2
                  DELIMITED BY "!"
                  INTO RESULT-BUF2.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEAL DISC BREAK6: !" TO WK-BUF-1.
           MOVE DL-DISC-TERM(6) TO WK-NUM-Z.
           MOVE WK-NUM TO CHAR-3.
           MOVE CHAR-3-X TO WK-BUF-2.
           MOVE " !" TO WK-BUF-3.
           MOVE DL-DISC-RATE(6) TO W-RATE.
           MOVE W-RATE TO CHAR-7.
           MOVE CHAR-7-X TO WK-BUF-4.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2 WK-BUF-3 WK-BUF-4
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SERV ACCTS FORM :          !" TO WK-BUF-1.
           MOVE DL-SVACCT-FRMLA TO WK-NUM-Z.
           MOVE WK-NUM TO CHAR-2.
           MOVE CHAR-2-X TO WK-BUF-2.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE "DEALER DIRECT DEPOSIT ACH: !" TO WK-BUF-1.
           MOVE DL-ACH-FG TO WK-BUF-2.
           STRING WK-BUF-1 WK-BUF-2
                  DELIMITED BY "!"
                  INTO RESULT-BUF2.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SERV ACCTS CODE :           !" TO WK-BUF-1.
           MOVE DL-SVCODE TO CHAR-1.
           MOVE CHAR-1-X TO WK-BUF-2.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE "BANK ROUTING NO. :         !" TO WK-BUF-1.
           MOVE DL-BANK-ROUTING-NO TO WK-ROUTING-NO.
           STRING WK-BUF-1 WK-ROUTING-NO
                  DELIMITED BY "!"
                  INTO RESULT-BUF2.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SERV ACCTS RATE :     !" TO WK-BUF-1.
           MOVE DL-SVACCT-RATE TO W-RATE.
           MOVE W-RATE TO CHAR-7.
           MOVE CHAR-7-X TO WK-BUF-2.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE "BANK ACCOUNT NUMBER:       !" TO WK-BUF-1.
           MOVE DL-BANK-ACCTNO TO WK-BUF-2.
           STRING WK-BUF-1 WK-BUF-2
                  DELIMITED BY "!"
                  INTO RESULT-BUF2.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SERV ACCTS ADDON:    !" TO WK-BUF-1.
           MOVE DL-SVACCT-AMT TO WK-MONEY.
           MOVE WK-MONEY TO CHAR-9.
           MOVE CHAR-9-X TO WK-BUF-2.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SERV ACCTS LCHG%:          !" TO WK-BUF-1.
           MOVE DL-SVACCT-LCHGPER TO WK-NUM-Z.
           MOVE WK-NUM TO CHAR-3.
           MOVE CHAR-2-X TO WK-BUF-2.
           MOVE "%!" TO WK-BUF-3.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2 WK-BUF-3
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SERV ACCTS INT %:         !" TO WK-BUF-1.
           MOVE DL-SVACCT-INTPER TO WK-NUM-Z.
           MOVE WK-NUM TO CHAR-3.
           MOVE CHAR-3-X TO WK-BUF-2.
           MOVE "%!" TO WK-BUF-3.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2 WK-BUF-3
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SERV ACCTS DEF %:         !" TO WK-BUF-1.
           MOVE DL-SVACCT-DEFPER TO WK-NUM-Z.
           MOVE WK-NUM TO CHAR-3.
           MOVE CHAR-3-X TO WK-BUF-2.
           MOVE "%!" TO WK-BUF-3.
           MOVE SPACE TO RESULT-BUF.
           STRING WK-BUF-1 WK-BUF-2 WK-BUF-3
                  DELIMITED BY "!"
                  INTO RESULT-BUF1.
           MOVE RESULT-BUF TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ********************************
       MEMO-PROCESS SECTION.
      ********************************

           IF MEMOS NOT = "Y"
              GO TO END-MEMO-PROCESS.

           MOVE DL-BRNO TO DM-BRNO.
           MOVE DL-DLNO TO DM-DL-DLNO.
           MOVE 0       TO DM-DATE DM-SEQ.


           PERFORM START-DM1-FILE.
           PERFORM READ-DM1-FILE-NEXT.
           IF (IO-FG = 9) OR (DM-BRNO NOT = DL-BRNO)
              GO TO END-MEMO-PROCESS.

           IF DL-DLNO NOT = DM-DL-DLNO
              GO TO END-MEMO-PROCESS.

           MOVE 1 TO MEMO-SW.
           ADD 7 LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
               PERFORM HEAD-PAGE
           ELSE
               MOVE HEAD8 TO DETAIL-LINE
               MOVE 2 TO LN-FEED
               PERFORM WRITE-DETAIL-LINE
               MOVE HEAD9 TO DETAIL-LINE
               PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.

           MOVE DM1-KEY TO HOLD-DM1-KEY.
           PERFORM MEMO-DATE-PROCESS.

       PRINT-MEMO.
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
               PERFORM HEAD-PAGE
               MOVE MEMO-DATE TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY TO D-MDATE.

           MOVE DM-SEQ  TO D-SEQ.
           MOVE DM-MEMO TO D-MEMO.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM READ-DM1-FILE-NEXT.
           IF IO-FG = 9
               GO TO END-MEMO-PROCESS.

           IF DM-DL-DLNO NOT = HOLD-DM-DL-DLNO
              GO TO END-MEMO-PROCESS.

           IF DM-DATE NOT = HOLD-DM-DATE
              MOVE DM-DATE TO HOLD-DM-DATE
              PERFORM MEMO-DATE-PROCESS.

           GO TO PRINT-MEMO.

       END-MEMO-PROCESS.
           MOVE 0 TO MEMO-SW.
           IF SINGLE-LINE NOT = "Y"
              MOVE ALL "*" TO DETAIL-LINE
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      **************************
       MEMO-DATE-PROCESS SECTION.
      **************************

           IF DM-DATE = ZERO
               MOVE DM-DATE TO MEMO-DATE
           ELSE
               SUBTRACT DM-DATE FROM 99999999 GIVING MEMO-DATE.
           MOVE MEMO-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-MDATE.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************

           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF MEMO-SW = 1
               MOVE HEAD8 TO PR-REC
               MOVE 2 TO LN-FEED
               PERFORM WRITE-PR-REC
               MOVE HEAD9 TO PR-REC
               MOVE 1 TO LN-FEED
               PERFORM WRITE-PR-REC
               MOVE 5 TO LN-COUNT
               MOVE 2 TO LN-FEED
               GO TO END-HEAD-PAGE.

           MOVE 7 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 2 TO LN-FEED.
           IF SEQ = 2
              GO TO NAME-RANGE.

           MOVE "BEGINNING DEALER" TO RANGE-LINE.
           MOVE BEG-DLNO TO RANGE-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING DEALER" TO RANGE-LINE.
           MOVE END-DLNO TO RANGE-NUM.
           PERFORM WRITE-DETAIL-LINE.
           GO TO FINISH-RANGE.

       NAME-RANGE.
           MOVE "BEGINNING NAME" TO RANGE-LINE.
           MOVE BEG-NAME TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING NAME" TO RANGE-LINE.
           MOVE END-NAME TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       FINISH-RANGE.
           MOVE "SINGLE LINE" TO RANGE-LINE.
           MOVE SINGLE-LINE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "PRINT MEMOS" TO RANGE-LINE.
           MOVE MEMOS TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ACTIVE/INACTIVE/BOTH" TO RANGE-LINE.
           MOVE ACTIVE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-RANGE.
           EXIT.

      *********************************
       DEALER-SCAN SECTION.
      *********************************
           MOVE 0 TO DLSCAN-NUM.
           MOVE "WI/DLSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DLSCAN-STATUS NOT = "00" OR DLSCAN-NUM = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-DEALER-SCAN.
           IF ( DEALER-FG = "BEG" )
               MOVE "SNNN042DL1." TO VDU
           ELSE
               MOVE "SNNN042DL2." TO VDU.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE DLSCAN-DLNO TO VDUNUM2.

           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO DLLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DLLIST-SYSDATE.
           DISPLAY DLLIST-SCREEN UPON DLLIST-WINDOW-HANDLE.
           MOVE DLLIST-FORM-FIELDS TO WR-BUF.
           MOVE DLLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE DLLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE DLLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/DLLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       STRING-ADDRESS.
           MOVE DL-CITY TO C-S-Z.
           MOVE DL-STATE TO STATE.
           MOVE DL-ZIP TO ZIP.
           PERFORM CAT-CSZ.
           IF DL-ADR2 NOT = SPACES
              STRING DL-ADR1,", "DL-ADR2,"  ",C-S-Z DELIMITED
                 BY "   " INTO D-ADR
           ELSE
              STRING DL-ADR1,"  ",C-S-Z DELIMITED
                 BY "   " INTO D-ADR.
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.
       CHECK-SINGLE-LINE.
           ADD 2 LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
       CHECK-PAGE.
           ADD 1 TO LN-COUNT.
           IF LN-COUNT > 40
               PERFORM HEAD-PAGE.
           IF LN-COUNT = 2
               MOVE 6 TO LN-FEED
           ELSE
               MOVE 4 TO LN-FEED.

      ******************************
       IO-ROUTINE SECTION.
      ******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPDMGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPDL2RN.CPY".
       COPY "LIBLP/LPDM1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************
       END-PROGRAM SECTION.
      ******************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY DLLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
