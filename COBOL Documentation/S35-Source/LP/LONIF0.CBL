       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONIF0.
       DATE-WRITTEN.    013184.
      **************************************************************
      *                 (LP)
      *  DESCRIPTION:  BALANCE FORWARD CONVERSION
      * REV:
      *  MJD 071096 ADDED LOGIC TO PRINT LCPDTH IN MMDD FORMAT IF
      *             WEEKLY PAYFREQ.
      *  JTG 071596 CHANGED TO DISPLAY 'W', 'B', 'S' WITH 1STPYDATE
      *  KEC 112596 CHANGED LN-POCD FOR RESCISSION.
      *  BAH 090398 HAD TO EXPAND H16 TO Z(5).99 FOR NATION FUNDING, LAURA
      *  JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      *  MJD 990803 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *   CS 991011 ADDED TEST OF BULK-CONVERSION-FG TO DISPLAY BULK OR
      *             CONVERSION
      *   CS 991130 ADD TEST OF LN-REPODATE AND LN-PLDATE.  REPO AND PL
      *             ARE NOT ALLOWED THROUGH BULK PURCHASE OPTION
      *  MJD 001012 ADDED F6 WINDOW SCAN. MOVED OLD SCAN TO F5 AND
      *             MADE FIELDS HIDDEN ON THE SCREEN. FIXED BUG, ACCOUNTS
      *             ENTERRED VIA SCAN WERE NOT BEING TESTED AGINST
      *             BULK-CONVERSION-FG LOGIC.
      *  JTG 010723 ADDED NEW RATE TYPE 'Z' AMORTIZED WHERE RUNTIME COMPUTES
      *             INTEREST ON A 30 DAY METHOD (REAL ESTATE) AND LOAN CALCS
      *             ARE DONE SIMPLE, REMOVED TIE TO CTBL, FORCING TESTS ON (1)
      *             NOTE: STOP LOAN'S WITH CLASS HAVING SP-CAL-RATETYPE(1) = 'Z'
      *                   NEED TO THINK ABOUT MIDSTREAM/CONVERTED ACCOUNTS 'LFP'
      *                                             LENDMARK PR#1045 & PR#1380
      *  JTG 030630 ACCOUNTS BYPASS LATE CHARGE ASSESS ROUTINE WHEN
      *             SP-LCFRMLA = ' ' BUT ACCOUNT IS SETUP FOR DEALER LATE
      *             CHARGE FORMULA                               MIDATL #J029
      *  BAH 080131 DISPLAY "MTD REC AMT" INSTEAD OF "EXCEPT PMTS" FOR
      *             GP-I-AM-WORLD, WORLD PR# 566
      *  CS 20081112 ADD TEST GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' EXCLUDE
      *             OTHER(ANTIC(6) AND MAINTFEE(ANTIC(4) FROM EARNED/UNEARNED
      *                                                        REGACC PR#210
      *  CS 20110824 IF SP-BANKRULE = 'F', ASK FOR LAST FULL PAYMENT DATE, THE
      *              SAME AS SP-BANKRULE 'D'   WORLD PR#752
      * BAH 20160414 ALLOW LN-CONVERCD "R" TO BE PULLED UP HERE FOR 
      *              MID-ATLANTIC AUTO CONVERTED ACCOUNTS, PR# 5047
      * BAH 20170609 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 20180928 GLOBAL BWFILE
      * BAH 20181210 GLOBAL LNFILE
      * BAH 20190627 REMOVED LN-REVOLVING, PT0153
      * BAH 20230713 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBLP/LPFDWK.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONIF0.CBL S34 06/09/21-15:11:38 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       01  WK-IFN            PIC X(2) VALUE "WK".

       01  WK-KEY            PIC 9(3).

       01  IO-TYPE           PIC X(3).
       01  NEW-FLAG          PIC 9.
       01  ELE               PIC 99    VALUE 0.
       01  FIRST-TIME        PIC 9     VALUE 0.
       01  FIRST-ELE         PIC 99    VALUE 0.
       01  FIRST-SCAN        PIC 9     VALUE 0.
       01  SCAN-FLAG         PIC 9     VALUE 0.
       01  SCAN              PIC X(2)  VALUE SPACES.
           88  SCAN-ON       VALUE "ON".
       01  NAME-WORKER       PIC X(34) VALUE SPACES.
       01  NAME              PIC X(16) VALUE SPACES.
       01  MAKER-CODE        PIC X     VALUE SPACES.
       01  TYP               PIC 9     VALUE 0.
       01  HOLD-LN-OWNBR     PIC 9(4)  VALUE ZERO.

       01  NUM-BW            PIC 9(9).
       01  NUM-BW-WK           REDEFINES NUM-BW.
           03  NUM-BW1       PIC 999.
           03  NUM-BW2       PIC 99.
           03  NUM-BW3       PIC 9999.
       01  ALP-BW.
           03  ALP-BW1       PIC 999.
           03  ALP-HYPHEN1   PIC X.
           03  ALP-BW2       PIC 99.
           03  ALP-HYPHEN2   PIC X.
           03  ALP-BW3       PIC 9999.

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BORR-BUF  VALUE " ".
           03  D-SOC         PIC X(11).
           03  D-LOANS       PIC ZZ.
           03  D-NAME-1      PIC X(33).
           03  D-NAME-2      PIC X(33).
           03  D-NAME-3      PIC X(33).
           03  D-NAME-4      PIC X(22).
           03  D-SP0         PIC X(7).
           03  D-SP1         PIC X(11).
           03  D-SP2         PIC X(22).

       01  DISPLAY-LOAN-BUF  VALUE " ".
           03  D-LOAN-NUM    PIC X(16).
           03  D-MAKER       PIC X(8).
           03  D-H1          PIC 99/99/99  BLANK ZERO.
           03  D-H2          PIC X(8).
           03  D-H3          PIC 99/99/99  BLANK ZERO.
           03  D-H4          PIC ZZZ.
           03  D-H5          PIC ZZZ.ZZZ.
           03  D-H6          PIC ZZZZZZ.ZZ.
           03  D-H7          PIC ZZZZZZ.ZZ.
           03  D-H8          PIC ZZZZZZ.ZZ.
           03  D-H9          PIC ZZZZZZ.ZZ.
           03  D-H10         PIC ZZZZZZ.ZZ.
           03  D-B99         PIC X(11).
           03  D-H99         PIC ZZZZZZ.ZZ.
           03  D-B1          PIC X(7).
           03  D-H11         PIC ZZZZZ.ZZ-.
           03  D-H12X        PIC X(8).
           03  D-H12         PIC ZZZZZ.ZZ-.
           03  D-H13         PIC ZZZZZ.ZZ-.
           03  D-H14         PIC ZZZZZZ.ZZ.
           03  D-H15         PIC ZZ.
           03  D-H151        PIC ZZ.
           03  D-H152        PIC 9999          BLANK ZERO.
           03  D-B3          PIC X(11).
           03  D-H16         PIC ZZZZZ.ZZ.
           03  D-B2          PIC X(8).
           03  D-H17         PIC ZZZZZZ.ZZ.
           03  D-H18         PIC ZZZZZZ.ZZ.
           03  D-H19         PIC ZZZZZZ.ZZ.
           03  D-H20         PIC ZZZZZZ.
           03  D-H21         PIC 9(6)         BLANK ZERO.
           03  D-COCODE      PIC X(2).
           03  D-COLLID      PIC X(20).
           03  D-DLNO        PIC 9(4).99.
           03  D-MESSAGE     PIC X(30).

       01  DISPLAY-BORR-LIST.
           03  FILLER        PIC X(34) VALUE
           "SOC LOANS A1 A2 A3 A4 SP0 SP1 SP2.".

       01  DISPLAY-LOAN-LIST.
           03  FILLER        PIC X(50) VALUE
           "LOANNO LNSEQ MAKER H1 H2 H3 H4 H5 H6 H7 H8 H9 H10 ".
           03  FILLER        PIC X(57) VALUE
           "B99:S H99 B1:S H11 H12X:S H12 H13 H14 H15 H151 H152 B3:S ".
           03  FILLER        PIC X(13) VALUE
           "H16 B2:S H17 ".
           03  FILLER        PIC X(42) VALUE
           "H18 H19 H20 H21 COCODE COLLID DLR MESSAGE.".

       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMNUIW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONIF_DEF.CPY".
       COPY "LIBLP/LPCMNU_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/LONIF_WKS.CPY".
       COPY "LIBLP/LPCMNU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLONIW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONIF_SCN.CPY".
       COPY "LIBLP/LPCMNU_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ************************************
       MAIN-PROGRAM SECTION.
      ************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONIF" TO VDU-FORM-NAME.
      D    STOP MESS.

           MOVE USER-PID TO TEMP-PID.
           MOVE SPACES TO ERRCD LN-REC.

           PERFORM RESTORE-SCREEN.

           PERFORM GET-GPENV.

           PERFORM FORM-RESET.

           MOVE "S  A160TITLE." TO VDU.
           IF BULK-CONVERSION-FG = "B"
              MOVE "BULK PUR. BALFWD"    TO VDUBUF
           ELSE
              MOVE "BALANCE FORWARD:"    TO VDUBUF.
           PERFORM SCREENIO.

           IF ROUTE-CD = "2"
              MOVE "1" TO ROUTE-CD
              MOVE SPACES TO WK-SAVE-RECORD
              MOVE 0 TO LOAN-KEY BORROWER-KEY
              GO TO MAIN-MODULE.

           MOVE SPACES TO ROUTE-CD.

           MOVE "S  8000TRDATE." TO VDU.
           MOVE TRANS-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8000MONTHEND." TO VDU.
           MOVE MONTH-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       MAIN-MODULE.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
           ELSE
              PERFORM FORM-RESET.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "PST"
              MOVE MENU-PROG-FORM(4) TO NEW-PROG-FORM
              MOVE 1 TO NEW-PROG-SUB ROUTE-CD
              MOVE "P" TO ERRCD
              PERFORM SAVE-SCREEN
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ************************************
       ENTER-KEYS SECTION.
      ************************************
       ENTER-KEYS-NO.
           IF WK-SAVE-RECORD NOT = SPACES
              MOVE WK-SAVE-RECORD TO LN-REC
              GO TO READ-LNFILE1.

           MOVE SPACES TO IO-TYPE LN-REC.
           MOVE "S  A000MSEL." TO VDU.
           MOVE "ENTER LOAN #, F3-CHG MONTH-END, F6-SCAN, F7-MENU"
             TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "EN N140LOANNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE 0 TO LOAN-KEY BORROWER-KEY
              GO TO MENU-KEY.
           IF VDUSTATUS = "16"
              PERFORM MODIFY-MONTH-END
              GO TO ENTER-KEYS-NO.
           IF VDUSTATUS = "1<"
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
                 MOVE " " TO WK-SAVE-RECORD
                 MOVE 0 TO LOAN-KEY
                 GO TO ENTER-KEYS-NO
              ELSE
                 GO TO READ-LNFILE1.
           IF VDUSTATUS = "1:"
              PERFORM ENTER-SCANKEY
              IF IO-TYPE = "CLR"
                 PERFORM FORM-RESET
                 GO TO ENTER-KEYS-NO
              ELSE
                 GO TO READ-LNFILE1.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEYS-NO.
           IF VDUNUM0 = 0
              GO TO ENTER-KEYS-NO.

           MOVE BRANCH  TO LN-OWNBR.
           MOVE VDUNUM0 TO LN-ACCTNO.
       READ-LNFILE1.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE " INVALID ACCOUNT NUMBER" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO WK-SAVE-RECORD
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS IN RESCISSION" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( SP-CAL-RATETYPE(1) = "Z" )
              MOVE "NOT ALLOWED WITH MORTGAGE INT. RATETYPE 'Z'"
                                                         TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.
      *--{CS}---
           IF BULK-CONVERSION-FG = "B"
              IF LN-CONVERCD NOT = "B"
                 MOVE "ACCOUNT NOT INPUT AS BULK PURCHASE ACCOUNT"
                                                       TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-KEYS-NO
              ELSE
              IF LN-REPODATE NOT = ZEROS
                 MOVE "REPO NOT ALLOWED ON BULK PURCHASE ACCOUNT"
                                                         TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-KEYS-NO
              ELSE
              IF LN-PLDATE NOT = ZEROS
                 MOVE "P&&L NOT ALLOWED ON BULK PURCHASE ACCOUNT"
                                                         TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-KEYS-NO.

           IF BULK-CONVERSION-FG = "C"
              IF LN-CONVERCD NOT = "Y" AND NOT = "R"
                 MOVE "ACCOUNT NOT INPUT AS CONVERSION ACCOUNT"
                                                       TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-KEYS-NO.

           PERFORM GET-BW1.
           PERFORM GET-LC.
           PERFORM DISPLAY-BORROWER.
           PERFORM DISPLAY-LOAN.
           MOVE "PST" TO IO-TYPE.
           GO TO END-ENTER-KEYS.
       MENU-KEY.
      *  DO NOT KNOW WHY THE DESTROY LONIF-SCREEN DOES NOT WORK HERE!
           DESTROY ALL CONTROLS.
           PERFORM MENU-SELECTION.
           MOVE 0 TO ELE.
           PERFORM SAVE-SCREEN.
           IF MENU-FG = 99
              GO TO ENTER-KEYS-NO.
           MOVE "END" TO IO-TYPE.
       END-ENTER-KEYS.
           EXIT.

      *****************************************
       BWLN-SCAN SECTION.
      *****************************************
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE TO BWLNSC-SYSDATE.
           MOVE 0        TO BWLNSC-ACCTNO.
           MOVE "WI/BWLNSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWLNSC-STATUS NOT = "00"
               MOVE 9 TO IO-FG
               GO TO EXIT-BWLN-SCAN.

           MOVE BWLNSC-ACCTNO TO LN-ACCTNO.
           MOVE BWLNSC-SSNO   TO BW-SSNO BORROWER-KEY.
           MOVE 0 TO IO-FG.

       EXIT-BWLN-SCAN.
           EXIT.

      **************************************
       ENTER-SCANKEY SECTION.
      **************************************
       ENTER-SCAN.
           MOVE 1 TO SCAN-FLAG.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.

           MOVE "S  A000MSEL." TO VDU.
           MOVE "1-SS#, 2-NAME, 3-SP SS#, 4-SP NAME" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050STYP." TO VDU.
           MOVE "SKEY:" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N010TYP." TO VDU.
           MOVE 2 TO TYP VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A160SORTKEY." TO VDU.
           MOVE "________________" TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-TYPE.
           MOVE "EN N010TYP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              MOVE "S" TO VDUACTION
              MOVE 1 TO TYP VDUNUM0
              PERFORM SCREENIO
              PERFORM DISPLAY-TYPE
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO TYP.
           IF TYP < 1 OR TYP > 4
              GO TO ENTER-TYPE.
       DISPLAY-TYPE.
           MOVE "S  A000MSEL." TO VDU.
           IF TYP = 1
              MOVE 0 TO BW-SSNO
              PERFORM START-BW1-FILE
              MOVE "SS#, F6-SCAN LN, F6-SCAN SS#" TO VDUBUF.
           IF TYP = 2
              MOVE SPACES TO BW-NAME
              PERFORM START-BW2-FILE
              MOVE "LAST NAME, F6-SCAN LN, F6-SCAN LAST NAME"
                TO VDUBUF.
           IF TYP = 3
              MOVE 0 TO BW-SPSSNO
              PERFORM START-BW3-FILE 
              MOVE "SP SS#, F6-SCAN LN, F6-SCAN SP SS#"
                TO VDUBUF.
           IF TYP = 4
              MOVE SPACES TO BW-SPNAME
              PERFORM START-BW4-FILE
              MOVE "SP NAME, F6-SCAN LN, F6-SCAN SP NAME"
                TO VDUBUF.
           PERFORM SCREENIO.
       ENTER-KEY.
           MOVE "E RA160SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1U"
              PERFORM CLOSE-SCANKEY
              GO TO ENTER-SCAN.
           IF VDUSTATUS = "1:"
              IF FIRST-SCAN = 0
                 MOVE " ENTER BORROWER # FIRST" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-KEY.
           IF VDUSTATUS = "1:"
              PERFORM SCAN-LOANS
              IF IO-TYPE = "CLR"
                 MOVE SPACES TO IO-TYPE
                 MOVE SPACES TO WR-BUF
                 MOVE DISPLAY-LOAN-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-KEY
              ELSE
                 GO TO ENTER-KEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF SCAN-ON
                 MOVE "PST" TO IO-TYPE
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           IF TYP = 2 OR 4
              MOVE VDUBUF TO BW-NAME BW-SPNAME
           ELSE
              IF TYP = 1 OR 3
                 MOVE VDUBUF TO BW-SSNO BW-SPSSNO.

           IF TYP = 2
              PERFORM START-BW2-FILE
           ELSE
              IF TYP = 1
                 PERFORM START-BW1-FILE
              ELSE
                 IF TYP = 3
                    PERFORM START-BW3-FILE
                 ELSE
                    IF TYP = 4
                       PERFORM START-BW4-FILE.
           MOVE 0 TO SCAN-FLAG.
       READ-FILES-NEXT.
           MOVE 0 TO FIRST-SCAN.
           IF TYP = 1
              PERFORM READ-BW1-FILE-NEXT
      *       UNLOCK BW-FILE
           ELSE
           IF TYP = 2
              PERFORM READ-BW2-FILE-NEXT
           ELSE
           IF TYP = 3
              PERFORM READ-BW3-FILE-NEXT
           ELSE
              PERFORM READ-BW4-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           PERFORM DISPLAY-BORROWER.

           MOVE SPACES TO WR-BUF.
           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 1 TO SCAN-FLAG.
           MOVE 1 TO FIRST-SCAN.
           MOVE SPACES TO SCAN.
           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
       END-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE "S  A000MSEL." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A050STYP." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010TYP." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A160SORTKEY." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
       EXIT-ENTER-SCANKEY.
           EXIT.

      *********************************
       MODIFY-MONTH-END SECTION.
      *********************************
       MOD-MONTH-END.
           MOVE "RNNM060MONTHEND." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO EXIT-MOD-MONTH-END.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO MOD-MONTH-END.
           MOVE VDU-DATE-YYYYMMDD TO MONTH-DATE.
       EXIT-MOD-MONTH-END.
           EXIT.

      **************************************
       SCAN-LOANS SECTION.
      **************************************
      *    GO TO START-LN4 START-LN5 START-LN6 START-LN7
           GO TO START-LN4 START-LN5 
                 DEPENDING ON SCAN-FLAG.
       START-LN4.
           IF SCAN-ON
              GO TO READ-LN4.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN4-WBEG-OWNBR
                                 QLN4-WEND-OWNBR.
           MOVE BW-SSNO       TO LN-SSNO(1) 
                                 QLN4-WBEG-SSNO1
                                 QLN4-WEND-SSNO1.
           PERFORM START-LN4-FILE.
           MOVE "ON" TO SCAN.
       READ-LN4.
           PERFORM READ-LN4-FILE-NEXT.
           IF IO-FG = 0
              IF LN-SSNO(1) = BW-SSNO AND (LN-OWNBR = LN-PATH-OWNBR)
                 PERFORM DISPLAY-LOAN
                 GO TO END-SCAN-LOANS.
           ADD 1 TO SCAN-FLAG.
           MOVE SPACES TO SCAN.
       START-LN5.
           IF SCAN-ON
              GO TO READ-LN5.
           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN5-WBEG-OWNBR
                                 QLN5-WEND-OWNBR.
           MOVE BW-SSNO       TO LN-SSNO(2) 
                                 QLN5-WBEG-SSNO2
                                 QLN5-WEND-SSNO2.
           PERFORM START-LN5-FILE.
           MOVE "ON" TO SCAN.
       READ-LN5.
           PERFORM READ-LN5-FILE-NEXT.
           IF IO-FG = 0
              IF LN-SSNO(2) = BW-SSNO AND (LN-OWNBR = LN-PATH-OWNBR)
                 PERFORM DISPLAY-LOAN
                 GO TO END-SCAN-LOANS.

      *    ADD 1 TO SCAN-FLAG.
      *    MOVE SPACES TO SCAN.
      *START-LN6.
      *    IF SCAN-ON
      *       GO TO READ-LN6.
      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN6-WBEG-OWNBR
      *                          QLN6-WEND-OWNBR.
      *    MOVE BW-SSNO       TO LN-SSNO(3) 
      *                          QLN6-WBEG-SSNO3
      *                          QLN6-WEND-SSNO3.
      *    PERFORM START-LN6-FILE.
      *    MOVE "ON" TO SCAN.
      *READ-LN6.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-FG = 0
      *       IF LN-SSNO(3) = BW-SSNO AND (LN-OWNBR = LN-PATH-OWNBR)
      *          PERFORM DISPLAY-LOAN
      *          GO TO END-SCAN-LOANS.
      *    ADD 1 TO SCAN-FLAG.
      *    MOVE SPACES TO SCAN.
      *START-LN7.
      *    IF SCAN-ON
      *       GO TO READ-LN7.
      *    MOVE BW-SSNO TO LN-SSNO(4).
      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN7-WBEG-OWNBR
      *                          QLN7-WEND-OWNBR.
      *    MOVE BW-SSNO       TO LN-SSNO(4) 
      *                          QLN7-WBEG-SSNO4
      *                          QLN7-WEND-SSNO4.
      *    PERFORM START-LN7-FILE.
      *    MOVE "ON" TO SCAN.
      *READ-LN7.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-FG = 0
      *       IF LN-SSNO(4) = BW-SSNO AND (LN-OWNBR = LN-PATH-OWNBR)
      *          PERFORM DISPLAY-LOAN
      *          GO TO END-SCAN-LOANS.

           MOVE "CLR" TO IO-TYPE.
           MOVE 0 TO SCAN-FLAG.
           MOVE SPACES TO SCAN LN-REC.
       END-SCAN-LOANS.
           EXIT.

      *****************************************
       DISPLAY-BORROWER SECTION.
      *****************************************

           MOVE " " TO DISPLAY-BORR-BUF.
           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO D-SOC.
           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
             BY NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME-1.
           MOVE BW-ADR1 TO D-NAME-2.
           IF BW-ADR2 NOT = SPACES
              MOVE BW-ADR2 TO D-NAME-3.
           MOVE " " TO NAME-WORKER.
           MOVE BW-CITY TO NAME-WORKER.
           MOVE BW-STATE TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ", *".
           INSPECT NAME-WORKER REPLACING FIRST "*               "
             BY NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY "  *".
           MOVE BW-ZIP TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "*               "
             BY NAME.
           IF BW-ADR2 = SPACES
              MOVE NAME-WORKER TO D-NAME-3
           ELSE
              MOVE NAME-WORKER TO D-NAME-4.
           MOVE BW-LOANS TO D-LOANS.
           IF BW-SPSSNO = 0
            IF BW-SPLNAME = SPACES
             IF BW-SPFNAME = SPACES
              GO TO END-DISPLAY-BORR.
           MOVE "SPOUSE:" TO D-SP0.
           IF BW-SPSSNO NOT = 0
              MOVE BW-SPSSNO TO NUM-BW
              PERFORM NUMBW-TO-ALPBW
              MOVE ALP-BW TO D-SP1.
           MOVE BW-SPLNAME TO NAME-WORKER.
           MOVE BW-SPFNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
             BY NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-SP2.
       END-DISPLAY-BORR.
           MOVE DISPLAY-BORR-BUF TO WR-BUF.
           MOVE DISPLAY-BORR-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE BW-SSNO TO BORROWER-KEY.

      *****************************************
       DISPLAY-LOAN SECTION.
      *****************************************

           MOVE LN-OWNBR TO BR-NO.
           IF HOLD-LN-OWNBR NOT = BR-NO
              MOVE BR-NO TO HOLD-LN-OWNBR
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              ELSE
                 MOVE BR-AVAIL-CREDIT TO BRAVAIL-BUF.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM OPEN-SP1-FILE
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              ELSE
                 PERFORM CLOSE-SP1-FILE.

           IF SP-BANKRULE = "D" OR SP-BANKRULE = "F"
              MOVE "S  A180H152P:S." TO VDU
              MOVE "LAST-FULL-PMT-DATE" TO VDUBUF
              PERFORM SCREENIO.

           MOVE " " TO DISPLAY-LOAN-BUF.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           IF LN-SSNO(1) = BW-SSNO
              MOVE LN-MAKERCD(1) TO MAKER-CODE
              PERFORM DISPLAY-MAKER.
           IF LN-SSNO(2) = BW-SSNO
              MOVE LN-MAKERCD(2) TO MAKER-CODE
              PERFORM DISPLAY-MAKER.
           IF LN-SSNO(3) = BW-SSNO
              MOVE LN-MAKERCD(3) TO MAKER-CODE
              PERFORM DISPLAY-MAKER.
           IF LN-SSNO(4) = BW-SSNO
              MOVE LN-MAKERCD(4) TO MAKER-CODE
              PERFORM DISPLAY-MAKER.

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-H1.

           MOVE LN-1STPYDATE TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-H2.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-H3.
           MOVE LN-SUM-ORGTERM TO D-H4.
           MOVE LN-APRATE TO D-H5.
           MOVE LN-SUM-LNAMT TO D-H6.
           MOVE LN-SUM-FINCHG TO D-H7.
           MOVE LN-1STPYAMT TO D-H8.
           MOVE LN-REGPYAMT TO D-H9.
           MOVE LN-TOTPAYMNTD TO D-H10.

           MOVE "MTD REC AMT" TO D-B99.
           MOVE LN-MTD-PAYS-RECDEL TO D-H99.

           MOVE TRANS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.

           IF IBPC-FG = "I" AND SP-LCFRMLA = " "
              MOVE "PRI-INT"       TO D-B1
              COMPUTE D-H12 =
                   LN-TOTINTPD - LN-LYRINTPD - LN-YTDINTPD
              MOVE "LYR-INTR"      TO D-H12X
              MOVE LN-LYRINTPD     TO D-H12
           ELSE
              MOVE "PRI-INT"       TO D-H12X
              COMPUTE D-H12 = LN-TOTINTPD - LN-YTDINTPD
              IF SP-LCFRMLA NOT = "A"
                 MOVE "LC-PAID"     TO D-B1
                 MOVE LN-LCPAID     TO D-H11
              ELSE
                 MOVE "LC-PART"     TO D-B1
                 MOVE LN-LCPARTIALS TO D-H11.
           MOVE LN-YTDINTPD TO D-H13.
           MOVE LN-TOTDEF TO D-H14.
           MOVE LN-TOTNODEF TO D-H15.
           MOVE LN-TOTNOALDEL TO D-H151.
           MOVE LN-PY-YYYYMM-RECDEL TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY TO D-H152.
           IF IBPC-FG = "P"
              MOVE " " TO D-B3
           ELSE
              MOVE "IB-INT-COLL" TO D-B3
              MOVE LN-TOTINT TO D-H16.
           MOVE "TOT-LCHG" TO D-B2.
           MOVE LN-TOTLCHG TO D-H17.


           IF GP-INCL-OTH-IN-EARNED-N
              ADD LN-ANTICERN(1) LN-ANTICERN(2) LN-ANTICERN(3)
                  LN-ANTICERN(4) LN-ANTICERN(5) GIVING D-H18
              ADD LN-ACCERN(1) LN-ACCERN(2) LN-ACCERN(3)
                  LN-ACCERN(4) LN-ACCERN(5) GIVING D-H19
           ELSE
           IF GP-INCL-OTH-IN-EARNED-R
              ADD LN-ANTICERN(1) LN-ANTICERN(2) LN-ANTICERN(3)
                  LN-ANTICERN(5) GIVING D-H18
              ADD LN-ACCERN(1) LN-ACCERN(2) LN-ACCERN(3)
                  LN-ACCERN(5) GIVING D-H19
           ELSE
              ADD LN-ANTICERN(1) LN-ANTICERN(2) LN-ANTICERN(3)
                  LN-ANTICERN(4) LN-ANTICERN(5) LN-ANTICERN(6)
                                                    GIVING D-H18
              ADD LN-ACCERN(1) LN-ACCERN(2) LN-ACCERN(3)
                  LN-ACCERN(4) LN-ACCERN(5) LN-ACCERN(6)
                                                    GIVING D-H19.


           MOVE LN-CREDITLIM TO D-H20.
           MOVE LN-LCPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-H21.
           MOVE LN-COLLCD TO D-COCODE.
           MOVE LN-COLLID TO D-COLLID.
           MOVE LN-DLNO TO D-DLNO.
           MOVE LN-MESSAGE TO D-MESSAGE.

           MOVE DISPLAY-LOAN-BUF TO WR-BUF.
           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SEND-LOAN-MESS.
       DISPLAY-LOAN-EXIT.
           EXIT.

      *************************************
       SAVE-SCREEN SECTION.
      *************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           IF VDUSTATUS = "16"
              IF WK-OLD-PROG-FORM = "  "
                 MOVE 99 TO MENU-FG
                 GO TO EXIT-SAVE-SCREEN
              ELSE
                 MOVE WK-OLD-PROG-FORM TO NEW-PROG-FORM OLD-PROG-FORM
                 MOVE " " TO WK-OLD-PROG-FORM.
           MOVE ELE TO WK-ELE.
           MOVE LN-REC TO WK-SAVE-RECORD.
           PERFORM REWRITE-WK-FILE.
       EXIT-SAVE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      *************************************
       RESTORE-SCREEN SECTION.
      *************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF NEW-PROG-FORM NOT = OLD-PROG-FORM
              MOVE OLD-PROG-FORM TO WK-OLD-PROG-FORM.
           MOVE NEW-PROG-FORM TO OLD-PROG-FORM.
           IF IO-FG = 0
              GO TO RESTORE-CLEAR.
           MOVE " " TO WK-SAVE-RECORD.
           MOVE 0 TO WK-ELE.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           GO TO EXIT-RESTORE-SCREEN.
       RESTORE-CLEAR.
           IF WK-ELE NOT = 0
              SUBTRACT 1 FROM WK-ELE GIVING FIRST-ELE.
           MOVE WK-SAVE-RECORD TO LN-REC.
           MOVE 0 TO WK-ELE.
           PERFORM REWRITE-WK-FILE.
       EXIT-RESTORE-SCREEN.
           IF FIRST-ELE = 0
              MOVE 0 TO NEW-FLAG
           ELSE
              MOVE 1 TO NEW-FLAG.
           PERFORM CLOSE-WK-FILE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMNUI.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           DISPLAY LONIF-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONIF-FORM-FIELDS TO WR-BUF.
           MOVE LONIF-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE LONIF-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONIF-HELP--DIR TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONIF_EVA.CPY".
              COPY "LIBLP/LPCMNU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       GET-BW1.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO BW-REC
              MOVE 0 TO BW-SSNO
              MOVE "BORROWER " TO BW-LNAME
              MOVE "DOES NOT EXIST" TO BW-FNAME.
       GET-LC.
           MOVE LN-CLASS TO LC-CODE.
           MOVE "INVALID CLASS" TO LC-DESC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           MOVE "S  A010TYP." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A160SORTKEY." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
       BLINK-STARS.
           MOVE "S  A000STAR1." TO VDU.
           MOVE "***" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000STAR2." TO VDU.
           MOVE "***" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000STAR1." TO VDU.
           MOVE " " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000STAR2." TO VDU.
           MOVE " " TO VDUBUF.
           PERFORM SCREENIO.
       SEND-LOAN-MESS.
           IF LN-MESSAGE NOT = SPACES
              PERFORM ALARM
              PERFORM BLINK-STARS 3 TIMES
              PERFORM ALARM.
       DISPLAY-MAKER.
           IF MAKER-CODE = "M"
              MOVE "MAKER" TO D-MAKER.
           IF MAKER-CODE = "J"
              MOVE "JOINT" TO D-MAKER.
           IF MAKER-CODE = "E"
              MOVE "ENDORSER" TO D-MAKER.
       NUMBW-TO-ALPBW.
           MOVE NUM-BW1 TO ALP-BW1.
           MOVE NUM-BW2 TO ALP-BW2.
           MOVE NUM-BW3 TO ALP-BW3.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPWKI.CPY".

       COPY "LIBGB/FERRORS.CPY".

           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           IF NEW-PROG-SUB = 1 AND ERRCD = "P"
              NEXT SENTENCE
           ELSE
              DESTROY ALL CONTROLS.
       STOP-RUN.
           EXIT PROGRAM.
