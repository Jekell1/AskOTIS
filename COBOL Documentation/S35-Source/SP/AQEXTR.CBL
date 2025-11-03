       IDENTIFICATION DIVISION.
       PROGRAM-ID.      AQEXTR.
       DATE-WRITTEN. 02-10-84
      ******************************************************************
      *    DIR :  LP
      *    DESC:  AUDITOR INQUIRY INDEX FILE CREATION PROGRAM
      *           RESTRICTED INPUT SCREEN: DATE RANGE ONLY.
      *              CREATE /USR/DATA/R1/SP/AQFILE
      *
      *=================================================================
      * THIS PROGRAM HAS BRANCH-GROUP LOGIC IN IT BUT IT IS NOT BEING
      * USED.  IN 1998-0224, MJD ADDED LOGIC OF FORCING THE BEG-BRANCH,
      * END-BRANCH AND GROUP-FLAG.  IN THE AUDITOR-DEFAULT SECTION,
      * WHICH IS CALLED FROM THE INITIALIZATION SECTION, IT FORCES THE
      * BRANCH RANGE TO BE THE USER'S LOGIN BRANCH (EXT-CONAME-USER-LOC)
      * AND TURNS OFF THE GROUP LOGIC.  THUS, THIS EXTRACTION PROGRAM
      * ONLY CREATES AQ-REC FOR ONE BRANCH ONLY.  FOR A32 & A34,
      * THIS PROGRAM DOES NOT DO AN OPEN-OUTPUT TO THE AQFILE (LOCAL)
      * BUT NOW JUST PURGES ALL AQ-REC FOR THE USER'S BRANCH THEN
      * CREATES NEW ONES.                                [KEC 2019-0131]
      *=================================================================
      * REV:
      *  042995 SLC ADDED PLTYPE LOGIC.
      *  071295 MJD CHANGED TOTALS,AND DET LINE SO THAT:
      *             CURRENT BALANCE = LN-CURBAL - LN-OT2BAL,
      *             OTHER BALANCE   = LN-OTHBAL + LN-OT2BAL.
      *  073195 MJD ADDED INCLUDE/SKIP/ONLY OPTIONS FOR POTS
      *                AND MISS PAYS.
      *  090795 JTG STOPPED F10 INTERRUPT
      *  MJD 010896 CORRECTED TEST OF LN-PLTYPE. IF "A" FOR ALL WAS ENTERED
      *              THEN THE PROGRAM WAS INCORRECTLY TESTING LN-PLTYPE
      *              FOR AN "A" INSTEAD OF FOR ALL TYPES.
      *  MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  GLF 090397 REMOVED ALL USE OF GLOBAL
      *  GLF 101697 ADDED COPY MEMBER: LIBLP/LPPDUE LIBLP/LPPDUEW
      *  MJD 022498 ALLOW ENTRY OF CLASS RANGE.
      *  MJD 990802 ADDED GPENV(W). NEEDED FOR CHANGES TO LPEARN.
      *  BLF 070920 ADDED LOGIC TO READ LN & LT FROM ARCHIVE FILES TO
      *             INCLUDE ARCHIVED LOANS IN EXTRACTION, WORLD PR# 547
      *  BLM 120423 CHECK ON ARCHIVED LOANS WAS MESSING UP, WAS LOOKING
      *             FOR ARCHIVE FILES IF GP-PURGE-TO-ARCHIVE-FG = "N",
      *             CHANGED TO SAY NOT = "Y".  THEN OUR ACCESS-CALL WAS
      *             GIVING A GOOD ACCESS ON /USR/    /R1/LP/LNFILE, &
      *             CAUSING OPEN ERROR OF 35.  CHANGED TO USE ACU
      *             FILE-INFO CALL.  WESTERN CREDIT PL# ??
      *  BAH 160203 SPLIT OUT LTFILE, PD0003
      *   CS 170413 ACTIVATE 8 DIGIT DATES, LEFT THE FORCE OF PARAMETERS
      *             NOT ENTERED ON THE SCREEN.
      *  BAH 181004 GLOBAL BWFILE
      *  BAH 181212 GLOBAL LNFILE, REMOVED ARCHIVE
      *  AMM 190124 CHANGES AQ FILE
      *  BAH 20190723 ADDED CLEAR-AQ-FILE, SPAQCL, FUTURE FIELDS NOT CLEARED
      *  BAH 20190723 ADDED CLEAR-AQS-FILE, SPAQSCL, FUTURE FIELDS NOT CLEARED
      *  BAH 20191004 REMOVED ACCESS-CALL LNFILE
      *  BAH 20200831 REMOVED RESET ON BR1 
      *  BAH 20220815 HARD CODED START
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
           "@(#)SP/AQEXTR.CBL S35 02/22/24-08:17:25 >".

      ******************************************************************
      *
      *     F I L E   S P E C S  *
      *
      ******************************************************************
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBSP/SP01AQ.CPY".
       COPY "LIBSP/SP01AQ_SQL.CPY".
       COPY "LIBSP/SPWSAQ.CPY".
       COPY "LIBSP/SP01AQS.CPY".
       COPY "LIBSP/SP01AQS_SQL.CPY".
       COPY "LIBSP/SPWSAQS.CPY".
      *----------------------------------------------------------------
       01  PR-IFN        PIC XX       VALUE "PR".


      ******************************************************************
      *
      *     M I S C E L L E N O U S     W O R K E R S
      *
      ******************************************************************
       01  FIRST-TIME           PIC X        VALUE "Y".

       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  BAL-WK               PIC 9(7)     COMP-3.

       01  HOLD-TOT-REC         PIC 9(6)     VALUE 0.

       01  HOLD-CURBAL       PIC S9(9)V99.
       01  HOLD-OTHBAL       PIC S9(7)V99.


      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  AGE-DATE        PIC 9(8).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  BEG-DEALER      PIC 9(4)V99.
           03  END-DEALER      PIC 9(4)V99.
           03  BEG-AGE         PIC 9(4).
           03  END-AGE         PIC 9(4).
           03  CPOT            PIC X.
           03  CMISS           PIC X.
           03  BEG-BAL         PIC 9(7).
           03  END-BAL         PIC 9(7).
           03  BEG-RAGE        PIC 9(4).
           03  END-RAGE        PIC 9(4).
           03  RPOT            PIC X.
           03  OWNED           PIC X.
           03  PL              PIC X.
           03  PLACTIVE        PIC X.
           03  JD              PIC X.
           03  REPO            PIC X.
           03  LEGAL           PIC X.
           03  DEBT            PIC X.
           03  BANK            PIC X.
           03  RC              PIC X.
           03  ZBAL            PIC X.
           03  ALPHA-SUMMARY   PIC X.
           03  SUM-ONLY        PIC X.
           03  BEG-LNAMT       PIC 9(6).
           03  END-LNAMT       PIC 9(6).
           03  PLTYPE          PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "A".

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/AQEXTR_DEF.CPY".
       COPY "LIBSP/AQEXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/AQEXTR_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-AQS1-FILE.
           PERFORM CLOSE-AQ1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *     M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "AQEXTR" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"          TO LN-PATH-OVERRIDE
                                AQ-PATH-OVERRIDE
                                LTP-PATH-OVERRIDE.

           PERFORM OPEN-LN3-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF LN-LOANDATE < BEG-DATE OR LN-LOANDATE > END-DATE
              GO TO NEXT-LN.

           IF LN-ORGST NOT = BR-STATE
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO NEXT-LN.

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-DLNO < BEG-DEALER OR LN-DLNO > END-DEALER
              GO TO NEXT-LN.

           IF LN-SUM-LNAMT < BEG-LNAMT OR LN-SUM-LNAMT > END-LNAMT
              GO TO NEXT-LN.

           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.

           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "A"
                 IF LN-PLCD NOT = "P"
                    GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "I"
                 IF LN-PLCD NOT = "I"
                    IF LN-PLCD NOT = "W"
                       GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLTYPE NOT = "A"
                 IF LN-PLTYPE NOT = PLTYPE
                    GO TO NEXT-LN.

           IF LN-SALEFIN NOT = "Y"
              GO TO CONT-LN.

           IF RC = "F"
              IF LN-DLRECOURSE NOT = "F"
                 GO TO NEXT-LN.

           IF RC = "P"
              IF LN-DLRECOURSE NOT = "P"
                 GO TO NEXT-LN.

           IF RC = "N"
              IF LN-DLRECOURSE NOT = "N"
                 GO TO NEXT-LN.

       CONT-LN.
           IF OWNED = "O"
              IF LN-SERVACCT = "Y"
                 GO TO NEXT-LN.

           IF OWNED = "S"
              IF LN-SERVACCT = "N"
                 GO TO NEXT-LN.

           IF JD = "O"
              IF LN-JDDATE = 0
                 GO TO NEXT-LN.

           IF JD = "S"
              IF LN-JDDATE NOT = 0
                 GO TO NEXT-LN.

           IF REPO = "O"
              IF NOT (LN-REPOCD = "X" OR "S")
                 GO TO NEXT-LN.

           IF REPO = "S"
              IF LN-REPOCD = "X" OR "S"
                 GO TO NEXT-LN.

           IF BANK = "O"
              IF LN-BNKRPTDATE = 0
                 GO TO NEXT-LN.

           IF BANK = "S"
              IF LN-BNKRPTDATE NOT = 0
                 GO TO NEXT-LN.

           IF LEGAL = "O"
              IF LN-LEGALDATE = 0
                 GO TO NEXT-LN.

           IF LEGAL = "S"
              IF LN-LEGALDATE NOT = 0
                 GO TO NEXT-LN.

           IF DEBT = "O"
              IF LN-DEBTAIDATE = 0
                 GO TO NEXT-LN.

           IF DEBT = "S"
              IF LN-DEBTAIDATE NOT = 0
                 GO TO NEXT-LN.

           IF ZBAL = "O"
              IF ( LN-CURBAL NOT = 0 ) OR
                 ( LN-INTBAL NOT = 0 ) OR
                 ( LN-LCBAL  NOT = 0 ) OR
                 ( LN-OTHBAL NOT = 0 )
                     GO TO NEXT-LN.

           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                          GO TO NEXT-LN.

           COMPUTE BAL-WK ROUNDED =
                          LN-CURBAL + LN-INTBAL + LN-LCBAL + LN-OTHBAL.

           IF ( BAL-WK < BEG-BAL ) OR ( BAL-WK > END-BAL )
              GO TO NEXT-LN.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-CLASS     TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           PERFORM COMPUTE-RECENCY.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-PAID-AHEAD.

           IF CONTRACTUAL < BEG-AGE OR CONTRACTUAL > END-AGE
              GO TO NEXT-LN.

      * TEST CONTRACTUAL: POTENTIALS AND MISS PAYS.
           IF CPOT = "S"
              IF CPOT30 NOT = " "
                  GO TO NEXT-LN
              END-IF
           ELSE
              IF CPOT = "O"
                 IF CPOT30 = " "
                    GO TO NEXT-LN
                 ELSE
                 IF CMISS = "O"
                    IF CPOT30 NOT = "M"
                       GO TO NEXT-LN
                    END-IF
                 ELSE
                 IF CMISS = "S"
                    IF CPOT30 = "M"
                       GO TO NEXT-LN
                    END-IF
                 END-IF
              ELSE
                 IF CPOT = "I"
                    IF CMISS = "O"
                       IF CPOT30 NOT = "M"
                          GO TO NEXT-LN
                       END-IF
                    ELSE
                    IF CMISS = "S"
                       IF CPOT30 = "M"
                          GO TO NEXT-LN
                       END-IF
                    END-IF.

           IF RECDEL < BEG-RAGE OR RECDEL > END-RAGE
              GO TO NEXT-LN.

      * TEST RECENCY: POTENTIALS
           IF RPOT = "S"
              IF RDPOT30 NOT = " "
                  GO TO NEXT-LN
              END-IF
           ELSE
              IF RPOT = "O"
                 IF RDPOT30 = " "
                    GO TO NEXT-LN.


           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.

           GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM WRITE-HEADER-REC.
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

      *******************************************
      *    A T T E N T I O N
      *    -----------------
      *     AUDITOR DEFAULT HAS BEEN CHANGED NOT SET CLASS TO 30.
      *     CLASS RANGE WILL BE ENTERRED VIA ENTER-PARAMETERS.
      *
           PERFORM AUDITOR-DEFAULT.

           MOVE AGE-DATE TO AGEING-DATE.

           MOVE 0 TO HOLD-TOT-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CANNOT DO OPEN-OUTPUT ON FILES; CANNOT NUKE TABLES IN SQL.
      *    IN PRIOR RELEASES, WE WOULD REMOVE THE FILES VIA OPEN-OUTPUT.
      *    IN NEW RELEASES (A34+), WE CANNOT REMOVE TABLES.  WE NOW NEED
      *    TO PURGE THE FILE (TABLE) BY REMOVING EVERY RECORD (ROW)
      *    INDIVIDUALLY THEN NEED TO REBUILD THE FILE.
      *    THIS PROGRAM ONLY PURGES RECORDS FOR THE USER'S LOGIN BRANCH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *KC  PERFORM OPEN-AQ1-FILE-OUTPUT.

           PERFORM PURGE-OPEN-OUTPUT-AQ.  
           PERFORM OPEN-AQ1-FILE.
           PERFORM OPEN-AQS1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS................" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE AGE-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

       ENTER-PARM.

           IF VDUSTATUS = "1U" GO TO CL-02.

       AGEDT.
           GO TO BEGDT.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060AGEDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGEDT.

           MOVE VDU-DATE-YYYYMMDD TO AGE-DATE.

           IF AGE-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              MOVE "S  8000AGEDATE." TO VDU
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD AGE-DATE
              PERFORM SCREENIO.

       BEGDT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BATCH WILL USE DATE RAN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGEDT.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDT.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

           IF BEG-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              MOVE "S  A000BEGDATE." TO VDU
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-MMDDYY TO VDU-DATE-YYYYMMDD BEG-DATE 
              PERFORM SCREENIO .

       ENDDT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BATCH WILL USE DATE RAN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGDT.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDDT.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  A000ENDDATE." TO VDU
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD END-DATE
              PERFORM SCREENIO.

           IF BEG-DATE > END-DATE
              PERFORM INVALID-RANGE
              GO TO ENDDT.

      ******************************
      *
      *         A T T E N T I O N
      *         - - - - - - - - -
      * REMOVE THIS GO TO IF YOU WANT TO USE ALL OPTIONS.
      *           ALSO REMOVE "PERFORM AUDITOR-DEFAULT" IN
      *           INITIALIZATION ROUTINE.
      *         CHANGED TO SKIP BRANCH AND ALLOW ENTRY OF CLASS. MJD 022498
      *           TO ALLOW BRANCH, CHANGE UP ARROW FROM CL-01
      *    GO TO EXIT-PARM.
           GO TO CL-01.
      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENDDT.

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
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CL-01.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
      *--->IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.

           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              PERFORM INVALID-RANGE
              GO TO BR-01.

           GO TO CL-01.

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.

      *----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
      *--->IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1U" GO TO ENDDT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.

           MOVE VDUNUM0 TO BEG-CLASS.

      *----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.

           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.

      ******************************
      *
      *         A T T E N T I O N
      *         - - - - - - - - -
      * REMOVE THIS GO TO IF YOU WANT TO USE ALL OPTIONS.
      *           ALSO REMOVE "PERFORM AUDITOR-DEFAULT" IN
      *           INITIALIZATION ROUTINE.
      *    GO TO DL-01.
           GO TO EXIT-PARM.

      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0             TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99            TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      ******************************
      *
      *         A T T E N T I O N
      *         - - - - - - - - -
      * REMOVE THIS GO TO IF YOU WANT TO USE ALL OPTIONS.
      *           ALSO REMOVE "PERFORM AUDITOR-DEFAULT" IN
      *           INITIALIZATION ROUTINE.
           GO TO EXIT-PARM.
      *----------------------------------------------------------------
       DL-01.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-01.

           MOVE VDUNUM2 TO BEG-DEALER.

      *----------------------------------------------------------------
       DL-02.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-02.

           MOVE VDUNUM2 TO END-DEALER.

           IF BEG-DEALER > END-DEALER
              PERFORM INVALID-RANGE
              GO TO DL-01.

           GO TO AGE-01.

      *----------------------------------------------------------------
       ALL-DL.
           MOVE "SNNN042DL1." TO VDU.
           MOVE 0             TO BEG-DEALER VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99       TO END-DEALER VDUNUM2.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       AGE-01.
           MOVE "ENNN040AGE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLAGE.
           IF VDUSTATUS = "1U" GO TO DL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGE-01.

           MOVE VDUNUM0 TO BEG-AGE.

      *----------------------------------------------------------------
       AGE-02.
           MOVE "ENNN040AGE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGE-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGE-02.

           MOVE VDUNUM0 TO END-AGE.

           IF BEG-AGE > END-AGE
              PERFORM INVALID-RANGE
              GO TO AGE-01.

           IF NOT ( BEG-AGE < 30)
               MOVE "SNNA010C-POT." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               MOVE "SNNA010C-MISS." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               GO TO RAGE01.
           GO TO CPOTYN.

      *----------------------------------------------------------------
       ALLAGE.
           MOVE "SNNN040AGE1." TO VDU.
           MOVE 0              TO BEG-AGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040AGE2." TO VDU.
           MOVE 9999           TO END-AGE VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       CPOTYN.
           MOVE "E  A010C-POT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGE-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CPOTYN.

           MOVE VDUBUF TO CPOT.
           IF CPOT = "S"
                 MOVE "SNNA010C-MISS." TO VDU
                 MOVE "S" TO VDUBUF
                 PERFORM SCREENIO
                 GO TO RAGE01.
           IF CPOT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO CPOTYN.

      *----------------------------------------------------------------
       CMISSYN.
           MOVE "E  A010C-MISS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CPOTYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CMISSYN.

           MOVE VDUBUF TO CMISS.

           IF CMISS NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO CMISSYN.

      *----------------------------------------------------------------
       RAGE01.
           MOVE "ENNN040RAGE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALRAGE.
           IF VDUSTATUS = "1U"
                IF NOT (BEG-AGE < 30)
                   GO TO AGE-02
                ELSE
                IF CPOT = "S"
                    GO TO CPOTYN
                ELSE
                    GO TO CMISSYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RAGE01.

           MOVE VDUNUM0 TO BEG-RAGE.

      *----------------------------------------------------------------
       RAGE02.
           MOVE "ENNN040RAGE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RAGE01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RAGE02.

           MOVE VDUNUM0 TO END-RAGE.

           IF BEG-RAGE > END-RAGE
              PERFORM INVALID-RANGE
              GO TO RAGE01.

           IF NOT (BEG-RAGE < 30)
               MOVE "SNNA010R-POT." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               GO TO BAL01.
           GO TO RPOTYN.

      *----------------------------------------------------------------
       ALRAGE.
           MOVE "SNNN040RAGE1." TO VDU.
           MOVE 0               TO BEG-RAGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040RAGE2." TO VDU.
           MOVE 9999            TO END-RAGE VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       RPOTYN.
           MOVE "E  A010R-POT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RAGE02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RPOTYN.

           MOVE VDUBUF TO RPOT.

           IF RPOT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO RPOTYN.

      *----------------------------------------------------------------
       BAL01.
           MOVE "ENNN070BB1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLBAL.
           IF VDUSTATUS = "1U"
                IF NOT (BEG-RAGE < 30)
                   GO TO RAGE02
                ELSE
                   GO TO RPOTYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BAL01.

           MOVE VDUNUM0 TO BEG-BAL.

      *----------------------------------------------------------------
       BAL02.
           MOVE "ENNN070BB2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BAL01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BAL02.

           MOVE VDUNUM0 TO END-BAL.

           IF BEG-BAL > END-BAL
              PERFORM INVALID-RANGE
              GO TO BAL01.

           GO TO AMT01.

      *----------------------------------------------------------------
       ALLBAL.
           MOVE "SNNN070BB1." TO VDU.
           MOVE 0             TO BEG-BAL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN070BB2." TO VDU.
           MOVE 9999999       TO END-BAL VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       AMT01.
           MOVE "ENNN060LA1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLAMT.
           IF VDUSTATUS = "1U" GO TO BAL02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT01.

           MOVE VDUNUM0 TO BEG-LNAMT.

      *----------------------------------------------------------------
       AMT02.
           MOVE "ENNN060LA2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT02.

           MOVE VDUNUM0 TO END-LNAMT.

           IF BEG-LNAMT > END-LNAMT
              PERFORM INVALID-RANGE
              GO TO AMT01.

           GO TO TYPEYN.

      *----------------------------------------------------------------
       ALLAMT.
           MOVE "SNNN060LA1." TO VDU.
           MOVE 0             TO BEG-LNAMT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060LA2." TO VDU.
           MOVE 999999        TO END-LNAMT VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       TYPEYN.
           MOVE "E  A010TYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TYPEYN.

           MOVE VDUBUF TO OWNED.

           IF OWNED NOT = "O" AND NOT = "S" AND NOT = "B"
              GO TO TYPEYN.

      *----------------------------------------------------------------
       PLYN.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TYPEYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLYN.

           MOVE VDUBUF TO PL.

           IF PL = "I" OR "O"
              GO TO PLTYPEYN.

           IF PL NOT = "S" GO TO PLYN.

           MOVE "S  A010ACTIVE." TO VDU.
           MOVE "B"              TO PLACTIVE VDUBUF.
           PERFORM SCREENIO.
           GO TO RECRSE.

      *----------------------------------------------------------------
       PLTYPEYN.
           MOVE "E  A010PLTYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLTYPEYN.

           MOVE VDUBUF TO PLTYPE.

           IF NOT (PLTYPE = "A" OR "P" OR "2" OR "3" OR "4" OR "5")
              GO TO PLTYPEYN.

      *----------------------------------------------------------------
       ACTVYN.
           MOVE "E  A010ACTIVE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLTYPEYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ACTVYN.

           MOVE VDUBUF TO PLACTIVE.

           IF PLACTIVE NOT = "I" AND NOT = "A" AND NOT = "B"
              GO TO ACTVYN.

      *----------------------------------------------------------------
       RECRSE.
           MOVE "E  A010RC." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ACTVYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RECRSE.

           MOVE VDUBUF TO RC.

           IF RC NOT = "F"
              AND NOT = "P"
               AND NOT = "N"
                AND NOT = "A"
                    GO TO RECRSE.

      *----------------------------------------------------------------
       JDYN.
           MOVE "E  A010JD." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RECRSE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO JDYN.

           MOVE VDUBUF TO JD.

           IF JD NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO JDYN.

      *----------------------------------------------------------------
       REPOYN.
           MOVE "E  A010RP." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO JDYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REPOYN.

           MOVE VDUBUF TO REPO.

           IF REPO NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO REPOYN.

      *----------------------------------------------------------------
       BANKYN.
           MOVE "E  A010BK." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REPOYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BANKYN.

           MOVE VDUBUF TO BANK.

           IF BANK NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO BANKYN.

      *----------------------------------------------------------------
       LEGLYN.
           MOVE "E  A010LG." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BANKYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LEGLYN.

           MOVE VDUBUF TO LEGAL.

           IF LEGAL NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO LEGLYN.

      *----------------------------------------------------------------
       DEBTYN.
           MOVE "E  A010DA." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LEGLYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DEBTYN.

           MOVE VDUBUF TO DEBT.

           IF DEBT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO DEBTYN.

      *----------------------------------------------------------------
       ZBALYN.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DEBTYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ZBALYN.

           MOVE VDUBUF TO ZBAL.

           IF ZBAL = "I"     GO TO ALPYN.
           IF ZBAL = "S"     GO TO ALPYN.
           IF ZBAL NOT = "O" GO TO ZBALYN.

      *----------------------------------------------------------------
       ALPYN.
           MOVE "E  A010ALPSUM." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ZBALYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ALPYN.

           MOVE VDUBUF TO ALPHA-SUMMARY.

           IF ALPHA-SUMMARY = "Y"     GO TO SUMYN.
           IF ALPHA-SUMMARY NOT = "N" GO TO ALPYN.

      *----------------------------------------------------------------
       SUMYN.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ALPYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SUMYN.

           MOVE VDUBUF TO SUM-ONLY.

           IF SUM-ONLY = "Y"     GO TO EXIT-PARM.
           IF SUM-ONLY NOT = "N" GO TO SUMYN.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH    TO VDUNUM0
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BR2." TO VDU.
           MOVE END-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N042DL1." TO VDU.
           MOVE BEG-DEALER    TO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N042DL2." TO VDU.
           MOVE END-DEALER    TO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040AGE1." TO VDU.
           MOVE BEG-AGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040AGE2." TO VDU.
           MOVE END-AGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN A010C-POT." TO VDU.
           MOVE CPOT            TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN A010C-MISS." TO VDU.
           MOVE CMISS            TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040RAGE1." TO VDU.
           MOVE BEG-RAGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040RAGE2." TO VDU.
           MOVE END-RAGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN A010R-POT." TO VDU.
           MOVE RPOT            TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N070BB1." TO VDU.
           MOVE BEG-BAL       TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N070BB2." TO VDU.
           MOVE END-BAL       TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N060LA1." TO VDU.
           MOVE BEG-LNAMT     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N060LA2." TO VDU.
           MOVE END-LNAMT     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010TYPE." TO VDU.
           MOVE OWNED          TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PL." TO VDU.
           MOVE PL           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PLTYPE." TO VDU.
           MOVE PLTYPE           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ACTIVE." TO VDU.
           MOVE PLACTIVE         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RC." TO VDU.
           MOVE RC           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010JD." TO VDU.
           MOVE JD           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RP." TO VDU.
           MOVE REPO         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010BK." TO VDU.
           MOVE BANK         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010LG." TO VDU.
           MOVE LEGAL        TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010DA." TO VDU.
           MOVE DEBT         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ALPSUM." TO VDU.
           MOVE ALPHA-SUMMARY    TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY          TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           IF FIRST-TIME = "Y"
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME.

            ADD 1 TO HOLD-TOT-REC.

            PERFORM CLEAR-AQ-FILE.

            MOVE LN-OWNBR     TO AQ-BRNO.
            MOVE LN-ACCTNO    TO AQ-ACCTNO.
            MOVE HOLD-TOT-REC TO AQ-SEQNO.
            MOVE LN-LOANDATE  TO AQ-LOANDATE.

            PERFORM WRITE-AQ1-FILE.

      * SET UP CURRENT BALANCE.
           MOVE 0 TO HOLD-CURBAL
                     HOLD-OTHBAL.

           ADD LN-OTHBAL LN-OT2BAL TO HOLD-OTHBAL.
           ADD LN-CURBAL           TO HOLD-CURBAL.
           SUBTRACT LN-OT2BAL    FROM HOLD-CURBAL.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ******************************************************************
       WRITE-HEADER-REC SECTION.
      ******************************************************************

           PERFORM CLEAR-AQS-FILE.

           MOVE BEG-BRANCH   TO AQS-BRNO.
           MOVE 1            TO AQS-NEXT-SEQNO.
           MOVE HOLD-TOT-REC TO AQS-TOT-REC.
           MOVE 0            TO AQS-WORKED-REC.

           PERFORM WRITE-AQS1-FILE.
           PERFORM CLOSE-AQS1-FILE.

      *******************************************
       AUDITOR-DEFAULT SECTION.
      *******************************************

      * SET AGEING DATE TO TODAYS DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO AGE-DATE.

      * FORCE USER BRANCH ONLY
           MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                       END-BRANCH.
           MOVE "N" TO GROUP-FLAG.

      * INCLUDE ALL!!
           MOVE 0 TO BEG-DEALER
                     BEG-AGE
                     BEG-RAGE
                     BEG-BAL
                     BEG-LNAMT.

           MOVE 9999 TO END-AGE
                     END-AGE
                     END-RAGE
                     END-BAL
                     END-LNAMT.

           MOVE 9999.99 TO END-DEALER.

           MOVE "I" TO CPOT
                       CMISS
                       RPOT
                       PL
                       JD
                       REPO
                       BANK
                       LEGAL
                       DEBT
                       ZBAL.

           MOVE "B" TO OWNED
                       PLACTIVE.

           MOVE "A" TO RC
                       PLTYPE.


      ******************************************************************
       PURGE-OPEN-OUTPUT-AQ SECTION.
      ******************************************************************

           MOVE EXT-CONAME-USER-LOC        TO AQ-BRNO.
           MOVE ZEROES                     TO AQ-ACCTNO.
           PERFORM OPEN-AQ1-FILE.

           PERFORM START-AQ1-FILE.

       PURGE-OPEN-OUTPUT-AQ-NEXT.

           PERFORM READ-AQ1-FILE-NEXT.
           IF ( IO-BAD ) OR ( AQ-BRNO NOT = AQ-PATH-OWNBR )
              PERFORM CLOSE-AQ1-FILE
              GO TO PURGE-OPEN-OUTPUT-AQ-EXIT.

           PERFORM DELETE-AQ1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-AQ-NEXT.
            
       PURGE-OPEN-OUTPUT-AQ-EXIT.
           EXIT.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    M E M B E R S
      *
      ******************************************************************
       COPY "LIBSP/SPAQSCL.CPY".
       COPY "LIBSP/SPAQCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO AQEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO AQEXTR-SYSDATE.
           DISPLAY AQEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE AQEXTR-FORM-FIELDS TO WR-BUF.
           MOVE AQEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE AQEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE AQEXTR-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/AQEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".

      *----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPAQSGS_SQL.CPY".
       COPY "LIBSP/SPAQGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBSP/SPAQ1IN.CPY".
       COPY "LIBSP/SPAQS1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/AUDIQ" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY AQEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
