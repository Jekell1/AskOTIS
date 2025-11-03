       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWMAN2.
       DATE-WRITTEN.
      **************************************************************************
      *  DESCRIPTION:   BORROWER MASTER FILE MAINTENANCE
      *            PAGE 2  (LIABILITIES)
      * REV:
      *  111990 BAH ADDED BW-RE-INTYTD BW-RE-INTLYR FOR
      *              COLLATERAL WITH "R" REAL ESTATE
      *  082197 JTG ADDED BW-PRIOR-RN-FOR10-DATE, BW-LAST-RN-FOR10-DATE,
      *                   BW-LAST-FORM-NO, BW-LAST-FORM-DATE
      *  102397 GLF REMOVED RESET OF FIELDS
      *  MJD 112497 EXPANDED BW-REC TO 480 BYTES
      *  MJD 120297 EXPANDED BW-LOANS TO PIC 9(2)
      *  MJD 120597 ADDED BW-SRCD
      *  MJD 021798 ADDED SIX MISC PHONE NUMBERS, EXTENSIONS AND CODES.
      *             EXPANDED BW-REC TO 546. ADDED 78 LEVEL BW-SIZE.
      *  JTG 093098 ALLOWED BW-LIASMLN & BW-LIAOTH TO GO NEGATIVE; REGENCY REVOL
      *  GLF 990909 CHANGE SSNO ENTRY TO USE NEW VDUCLASS "X" EDIT
      *  MJD 000501 ADDED LOGIC FOR PAGE 3, REMOVED MISC PHONE NUMBERS
      *             AND PUT THEM ON PAGE 3 (TFC PR#37)
      *  BAH 030127 ADDED CHANGES LOG FOR ANY CHANGES....
      *  BAH 030402 ADDED CREDIT SCORES AND BNKRPT SCORE, LENDMARK PR# 2044
      *             INTO A NEW BORROWER TRAILER (BXFILE)
      *  AMM 070323 CREATED A LOG FIELD CHANGE ROUTINE
      *  BAH 120830 ADDED BW-DEALTRACK-CHG-DATE, TRACKS CHANGES MADE TO LNAME,
      *             FNAME,ADR1,ADR2,CITY,ST,ZIP,PHONE,EMPPHONE REGACC PR# 319
      *  BAH 160513 NEW LAYOUT, REMOVED BX
      *   CS 170413 ACTIVATE 8 DIGIT DATES, BW-LAST-PODATE,BW-PRIOR-RN-FOR10
      *             DATE, BW-LAST-RN-FOR10-DATE,BW-LAST-FROM-DATE, AND BW-DEAL
      *             TRACK-CHG-DATE, ADDED DEALTRACK CHANGE DATE TO SCREEN
      *  BLM 210924 FIX CAST ERROR IF ALPHA ENTERED IN SSNO SCAN
      *  BAH 2025.0106 ADDED F24 AND F25 FOR BW-UMC-EXPIRE-DATE AND
      *                BW-UMC-PR-EXPIRE-DATE #1687 S35Q-146
      **************************************************************************
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
           "@(#)MN/BWMAN2.CBL S35 06/22/22-13:13:06 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

      *=================================================================
      *   BWCH-REC FROM 'REPLACING LEADING' ON BW-REC. [CHANGED-BW-REC]
      *   BWOR-REC FROM 'REPLACING LEADING' ON BW-REC. [ORIGINAL-BW-REC]
      *=================================================================
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "BWOR".
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "BWCH".

      ******************************************************************
      *
      *    L M C - W O R K E R S
      *
      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-BRNO                PIC 9(04).
           03  FILLER                  PIC X(01).
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(09).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(16).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-ORG-DATE            REDEFINES LMC-ORG-TEXT
                                       PIC 99/99/99.
           03  LMC-ORG-DLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-ORG-GLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-ORG-PHONE           REDEFINES LMC-ORG-TEXT
                                       PIC 999/999/9999.
           03  LMC-ORG-SSNO            REDEFINES LMC-ORG-TEXT
                                       PIC 999/99/9999.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(16).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-CHG-DATE            REDEFINES LMC-CHG-TEXT
                                       PIC 99/99/99.
           03  LMC-CHG-DLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-CHG-GLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-CHG-PHONE           REDEFINES LMC-CHG-TEXT
                                       PIC 999/999/9999.
           03  LMC-CHG-SSNO            REDEFINES LMC-CHG-TEXT
                                       PIC 999/99/9999.

      ******************************************************************
       01  IX1             PIC 99      VALUE 0.
       01  IX2             PIC 99      VALUE 0.
       01  SUB             PIC 999     VALUE 0.

       01  TYP             PIC 9       VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
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
       01  FIRST-SCAN      PIC 9       VALUE 0.

       01  HOLD-KEY        PIC 9(9).
       01  SOCSEC-KEY      PIC 9(9).

       01  BW-ALP-TABLE.
           03  BW-ALP-TBL  OCCURS 70 TIMES
                           PIC X.
       01  BWMAIN-LINK       VALUE SPACES.
           03  SAVEKEY       PIC X(9).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  BWMAIN-WINDOW-HANDLE PIC X(10).


      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  D-NAME      PIC X(34).
           03  F01         PIC Z9.
           03  F02         PIC Z9.
           03  F03         PIC XX.
           03  F04         PIC Z,ZZZ,ZZZ.99-.
           03  F05         PIC Z,ZZZ,ZZZ.99-.
           03  F06         PIC ZZ,ZZZ.99-.
           03  F07         PIC ZZ,ZZZ.99-.
           03  F08         PIC Z,ZZZ,ZZZ.99-.
           03  F09         PIC Z,ZZZ,ZZZ.99-.
           03  F10         PIC 99/99/99 BLANK ZERO.
           03  F11         PIC 99/99/99 BLANK ZERO.
           03  F12         PIC 99/99/99 BLANK ZERO.
           03  F13         PIC ZZ9.
           03  F14         PIC 99/99/99 BLANK ZERO.
           03  F-CRSCORE   PIC 9(4) OCCURS 4.
           03  F-BNKSCORE  PIC 9(4) OCCURS 4.
           03  F23         PIC 99/99/99 BLANK ZERO.
           03  F24         PIC 99/99/99 BLANK ZERO.
           03  F25         PIC 99/99/99 BLANK ZERO.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(53) VALUE
           "NAME 010 020 030 040 050 060 070 080 090 100 110 120 ".
           03  FILLER      PIC X(52) VALUE
           "130 140 150 160 170 180 190 200 210 220 230 240 250.".

       78  MAX                      VALUE 25.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  SPEC-TABLE.
      *01. NUMBER OF PRIOR LOANS
           03  FILLER      PIC X(8) VALUE "ENNN020N".
      *02. NUMBER OF CURRENT LOANS
           03  FILLER      PIC X(8) VALUE "ENNN020N".
      *03. SRCD
           03  FILLER      PIC X(8) VALUE "E  A020N".
      *04. Y-T-D INTEREST
           03  FILLER      PIC X(8) VALUE "E NS072N".
      *05. L-Y-R INTEREST
           03  FILLER      PIC X(8) VALUE "E NS072N".
      *06. REAL ESTATE INTEREST YTD
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *07. REAL ESTATE INTEREST LYR
           03  FILLER      PIC X(8) VALUE "E NS052N".
      *08. SMALL LOAN LIABILITY
           03  FILLER      PIC X(8) VALUE "E NS072N".
      *09. OTHER LOAN LIABILITY
           03  FILLER      PIC X(8) VALUE "E NS072N".
      *10. LAST PAYOFF DATE
           03  FILLER      PIC X(8) VALUE "ERNM080N".
      *11. PRIOR RENEWAL FOR 10%
           03  FILLER      PIC X(8) VALUE "ERNM080N".
      *12. LAST RENEWAL FOR 10%
           03  FILLER      PIC X(8) VALUE "ERNM080N".
      *13. LAST FORM NO. SENT
           03  FILLER      PIC X(8) VALUE "ENNN030N".
      *14. LAST FORM SENT DATE
           03  FILLER      PIC X(8) VALUE "ERNM080N".
      *15. CREDIT SCORE Q1
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *16. CREDIT SCORE Q2
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *17. CREDIT SCORE Q3
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *18. CREDIT SCORE Q4
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *19. BNKRPT SCORE Q1
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *20. BNKRPT SCORE Q2
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *21. BNKRPT SCORE Q3
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *22. BNKRPT SCORE Q4
           03  FILLER      PIC X(8) VALUE "EN N040N".
      *23. DEALER TRACK CHANGE DATE
           03  FILLER      PIC X(8) VALUE "ERNM080N".
      *24. UMC EXPIRATION
           03  FILLER      PIC X(8) VALUE "ERNM080N".
      *25. UMC PRIOR EXPIRATION
           03  FILLER      PIC X(8) VALUE "ERNM080N".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/BWMAN2_DEF.CPY".
       COPY "LIBMN/BWMAN2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA   PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/BWMAN2_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *******************************************************
       MAIN-PROGRAM SECTION.
      *******************************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BWMAN2" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO BWMAIN-LINK.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-BW-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF NEW-REC = "Y"
              MOVE "N" TO NEW-REC
              MOVE "P" TO FORM-P1
              MOVE 1 TO FORM-SEL1.

           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                MOVE BW-SSNO TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ***********************************
       REC-BW-MODULE SECTION.
      ***********************************

           MOVE SPACES TO IO-TYPE.

           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO BW-SSNO SOCSEC-KEY
              MOVE SPACES TO SAVEKEY
              MOVE "S  X110BWNO." TO VDU
              MOVE BW-SSNO TO VDU-SSNO-EDIT
              PERFORM SCREENIO
              GO TO READ-BWFILE.
           MOVE 0 TO NEW-FLAG.
       REC-BW-SSNO.
           MOVE "ERNX110BWNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-TYPEKEY
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-BW-SSNO.
           MOVE VDUNUM0 TO BW-SSNO HOLD-KEY.

           IF BW-SSNO = ZERO
              GO TO REC-BW-SSNO.
       READ-BWFILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO REC-BW-SSNO.

           MOVE BW-REC TO BWOR-REC.

           PERFORM DISPLAY-FIELDS.

           MOVE 0 TO NEW-FLAG.

           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.
       END-REC-BW-MODULE.
           EXIT.

      *    ************************************************
      *    *      ENTER SORTKEY ROUTINES
      *    *      TYPE = 1 - BORROWER SOCIAL SEC. #
      *    *             2 - BORROWER NAME
      *    *             3 - SPOUSE SOCIAL SEC. #
      *    *             4 - SPOUSE NAME
      *    ************************************************
       ENTER-TYPEKEY SECTION.
       ENTER-TYPE.
           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.

           MOVE " 1=SS#, 2=NAME, 3=SPOUSE SS#, 4=SPOUSE NAME, F6=SCAN"
              TO MESS.
           PERFORM CALL-TMMSG.

           MOVE "EN N010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
             MOVE "CLR" TO IO-TYPE
             GO TO CLOSE-SORTKEY.
           IF VDUSTATUS = "1<"
              MOVE "SN N010TYPE." TO VDU
              MOVE 1 TO TYP VDUNUM0
              PERFORM SCREENIO
              PERFORM DISPLAY-SCAN-TYPE
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
             GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO TYP.
           IF TYP < 1 OR TYP > 4
             GO TO ENTER-TYPE.
       DISPLAY-SCAN-TYPE.
           IF TYP = 1
             MOVE 0 TO BW-SSNO
              PERFORM START-BW1-FILE
              MOVE " ENTER SS#, OR F6 TO SCAN" TO MESS
              PERFORM CALL-TMMSG.
           IF TYP = 2 
             MOVE SPACES TO BW-NAME
              PERFORM START-BW2-FILE
              MOVE " ENTER NAME (LAST), OR F6 TO SCAN" TO MESS
              PERFORM CALL-TMMSG.
           IF TYP = 3 
             MOVE 0 TO BW-SPSSNO
              PERFORM START-BW3-FILE
              MOVE " ENTER SPOUSE SS#, OR F6 TO SCAN" TO MESS
              PERFORM CALL-TMMSG.
           IF TYP = 4 
             MOVE SPACES TO BW-SPNAME
              PERFORM START-BW4-FILE
              MOVE " ENTER SPOUSE NAME (LAST), OR F6 TO SCAN" TO MESS
              PERFORM CALL-TMMSG.
       ENTER-KEY.
           IF TYP = 2 OR TYP = 4
              MOVE "E RA240SORTKEY." TO VDU
           ELSE
              MOVE "E RA110SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SORTKEY.
           IF VDUSTATUS = "1U"
              PERFORM CLOSE-SORTKEY
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE BW-REC TO BWOR-REC
                 GO TO CLOSE-SORTKEY
              ELSE
                 GO TO ENTER-KEY.
           IF TYP = 1 OR TYP = 3
              INSPECT VDUBUF REPLACING ALL " " BY "0"
              MOVE VDUBUF TO SOCSEC-KEY
              IF SOCSEC-KEY NOT NUMERIC
                 GO TO ENTER-KEY.
           IF TYP = 1 OR TYP = 3
             MOVE VDUBUF TO BW-SSNO BW-SPSSNO.
           IF TYP = 2 OR TYP = 4
              PERFORM ARRANGE-BW-KEY
             MOVE BW-ALP-TABLE TO BW-NAME BW-SPNAME.
           IF TYP = 1 PERFORM START-BW1-FILE.
           IF TYP = 2 PERFORM START-BW2-FILE.
           IF TYP = 3 PERFORM START-BW3-FILE.
           IF TYP = 4 PERFORM START-BW4-FILE.
       READ-FILES-NEXT.
           IF TYP = 1 PERFORM READ-BW1-FILE-NEXT.
      *               UNLOCK BW-FILE.
           IF TYP = 2 PERFORM READ-BW2-FILE-NEXT.
           IF TYP = 3 PERFORM READ-BW3-FILE-NEXT.
           IF TYP = 4 PERFORM READ-BW4-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SORTKEY.

           MOVE "S  X110BWNO." TO VDU.
           MOVE BW-SSNO TO HOLD-KEY SOCSEC-KEY.
           MOVE BW-SSNO TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       CLOSE-SORTKEY.
           IF TYP = 1 PERFORM CLOSE-BW1-FILE.
           IF TYP = 2 PERFORM CLOSE-BW2-FILE.
           IF TYP = 3 PERFORM CLOSE-BW3-FILE.
           IF TYP = 4 PERFORM CLOSE-BW4-FILE.
       END-SORTKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
           MOVE "S  A010TYPE." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A160SORTKEY." TO VDU.
           PERFORM SCREENIO.

      ***********************************
       ARRANGE-BW-KEY SECTION.
      ***********************************
           MOVE 1 TO IX1 IX2.
           MOVE SPACES TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF IX1 NOT < 35
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(IX1) NOT = ","
              MOVE VDUTAB(IX1) TO BW-ALP-TBL(IX2)
            ELSE
              MOVE 35 TO IX2.
           ADD 1 TO IX1.
           ADD 1 TO IX2.
           GO TO ARR-BW-KEY-AGAIN.
       ARR-BW-KEY-EXIT.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
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
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR = 2
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF NOT (FORM-SEL1 = 1 OR 3 OR 4)
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE "RWR" TO IO-TYPE
                    GO TO EXIT-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0 OR CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
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
                 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUNUM0 TO BW-PRLOANS.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM0 TO BW-LOANS.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO BW-SRCD.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO BW-INTYTD.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO BW-INTLYR.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM2 TO BW-RE-INTYTD.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM2 TO BW-RE-INTLYR.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO BW-LIASMLN.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO BW-LIAOTH.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDU-DATE-YYYYMMDD TO BW-LASTPODATE.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDU-DATE-YYYYMMDD TO BW-PRIOR-RN-FOR10-DATE.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDU-DATE-YYYYMMDD TO BW-LAST-RN-FOR10-DATE.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUNUM0 TO BW-LAST-FORM-NO.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDU-DATE-YYYYMMDD TO BW-LAST-FORM-DATE.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUNUM0 TO BW-CRED-SCORE(1).
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUNUM0 TO BW-CRED-SCORE(2).
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUNUM0 TO BW-CRED-SCORE(3).
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUNUM0 TO BW-CRED-SCORE(4).
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM0 TO BW-BNKRPT-SCORE(1).
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUNUM0 TO BW-BNKRPT-SCORE(2).
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUNUM0 TO BW-BNKRPT-SCORE(3).
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUNUM0 TO BW-BNKRPT-SCORE(4).
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDU-DATE-YYYYMMDD TO BW-DEALTRACK-CHG-DATE.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDU-DATE-YYYYMMDD TO BW-UMC-EXPIRE-DATE.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDU-DATE-YYYYMMDD TO BW-UMC-PR-EXPIRE-DATE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      **************************************
       WRITE-MODULE SECTION.
      **************************************
           PERFORM OPEN-BW1-FILE.

           MOVE BW-REC TO BWCH-REC.

           IF BWOR-REC = BWCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF BW-REC NOT = BWOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE BWCH-REC TO BW-REC.
           PERFORM REWRITE-BW1-FILE.

       END-WRITE-MODULE.
           PERFORM CLOSE-BW1-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

           IF BWOR-PRLOANS NOT = BWCH-PRLOANS
             MOVE "01 "                   TO LMC-FIELD
             MOVE "PRLOANS "              TO LMC-DESC
             MOVE BWOR-PRLOANS            TO LMC-ORG-NUM
             MOVE BWCH-PRLOANS            TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-LOANS NOT = BWCH-LOANS
             MOVE "02 "                   TO LMC-FIELD
             MOVE "LOANS   "              TO LMC-DESC
             MOVE BWOR-LOANS              TO LMC-ORG-NUM
             MOVE BWCH-LOANS              TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-SRCD NOT = BWCH-SRCD
             MOVE "03 "                   TO LMC-FIELD
             MOVE "SRCD    "              TO LMC-DESC
             MOVE BWOR-SRCD               TO LMC-ORG-TEXT
             MOVE BWCH-SRCD               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-INTYTD NOT = BWCH-INTYTD
             MOVE "04 "                   TO LMC-FIELD
             MOVE "INTYTD  "              TO LMC-DESC
             MOVE BWOR-INTYTD             TO LMC-ORG-AMT
             MOVE BWCH-INTYTD             TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-INTLYR NOT = BWCH-INTLYR
             MOVE "05 "                   TO LMC-FIELD
             MOVE "INTLYR  "              TO LMC-DESC
             MOVE BWOR-INTLYR             TO LMC-ORG-AMT
             MOVE BWCH-INTLYR             TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-RE-INTYTD NOT = BWCH-RE-INTYTD
             MOVE "06 "                   TO LMC-FIELD
             MOVE "REINTYTD"              TO LMC-DESC
             MOVE BWOR-RE-INTYTD          TO LMC-ORG-AMT
             MOVE BWCH-RE-INTYTD          TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-RE-INTLYR NOT = BWCH-RE-INTLYR
             MOVE "07 "                   TO LMC-FIELD
             MOVE "REINTLYR"              TO LMC-DESC
             MOVE BWOR-RE-INTLYR          TO LMC-ORG-AMT
             MOVE BWCH-RE-INTLYR          TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-LIASMLN NOT = BWCH-LIASMLN
             MOVE "08 "                   TO LMC-FIELD
             MOVE "LIASMLN "              TO LMC-DESC
             MOVE BWOR-LIASMLN            TO LMC-ORG-AMT
             MOVE BWCH-LIASMLN            TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-LIAOTH NOT = BWCH-LIAOTH
             MOVE "09 "                   TO LMC-FIELD
             MOVE "LIAOTH  "              TO LMC-DESC
             MOVE BWOR-LIAOTH             TO LMC-ORG-AMT
             MOVE BWCH-LIAOTH             TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-LASTPODATE NOT = BWCH-LASTPODATE
             MOVE "10 "                   TO LMC-FIELD
             MOVE "LSTPODTE"              TO LMC-DESC
             MOVE BWOR-LASTPODATE          TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO  LMC-ORG-DATE
             MOVE BWCH-LASTPODATE          TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO  LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-PRIOR-RN-FOR10-DATE NOT = BWCH-PRIOR-RN-FOR10-DATE
             MOVE "11 "                   TO LMC-FIELD
             MOVE "PR RNF10"              TO LMC-DESC
             MOVE BWOR-PRIOR-RN-FOR10-DATE TO DATE-YYYYMMDD 
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY              TO LMC-ORG-DATE
             MOVE BWCH-PRIOR-RN-FOR10-DATE TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY               TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-LAST-RN-FOR10-DATE NOT = BWCH-LAST-RN-FOR10-DATE
             MOVE "12 "                   TO LMC-FIELD
             MOVE "LAST RN "              TO LMC-DESC
             MOVE BWOR-LAST-RN-FOR10-DATE      TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO      LMC-ORG-DATE
             MOVE BWCH-LAST-RN-FOR10-DATE TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-LAST-FORM-NO NOT = BWCH-LAST-FORM-NO
             MOVE "13 "                   TO LMC-FIELD
             MOVE "LASTFMNO"              TO LMC-DESC
             MOVE BWOR-LAST-FORM-NO       TO LMC-ORG-NUM
             MOVE BWCH-LAST-FORM-NO       TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-LAST-FORM-DATE NOT = BWCH-LAST-FORM-DATE
             MOVE "14 "                   TO LMC-FIELD
             MOVE "LSTFMDTE"              TO LMC-DESC
             MOVE BWOR-LAST-FORM-DATE     TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-ORG-DATE
             MOVE BWCH-LAST-FORM-DATE     TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
             MOVE DATE-MMDDYY             TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-CRED-SCORE(1) NOT = BWCH-CRED-SCORE(1)
             MOVE "15 "                   TO LMC-FIELD
             MOVE "CRED SCR"              TO LMC-DESC
             MOVE BWOR-CRED-SCORE(1)    TO LMC-ORG-NUM
             MOVE BWCH-CRED-SCORE(1)    TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-CRED-SCORE(2) NOT = BWCH-CRED-SCORE(2)
             MOVE "16 "                   TO LMC-FIELD
             MOVE "CRED SCR"              TO LMC-DESC
             MOVE BWOR-CRED-SCORE(2)    TO LMC-ORG-NUM
             MOVE BWCH-CRED-SCORE(2)    TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-CRED-SCORE(3) NOT = BWCH-CRED-SCORE(3)
             MOVE "17 "                   TO LMC-FIELD
             MOVE "CRED SCR"              TO LMC-DESC
             MOVE BWOR-CRED-SCORE(3)    TO LMC-ORG-NUM
             MOVE BWCH-CRED-SCORE(3)    TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-CRED-SCORE(4) NOT = BWCH-CRED-SCORE(4)
             MOVE "18 "                   TO LMC-FIELD
             MOVE "CRED SCR"              TO LMC-DESC
             MOVE BWOR-CRED-SCORE(4)    TO LMC-ORG-NUM
             MOVE BWCH-CRED-SCORE(4)    TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-BNKRPT-SCORE(1) NOT = BWCH-BNKRPT-SCORE(1)
             MOVE "19 "                   TO LMC-FIELD
             MOVE "BANK SCR"              TO LMC-DESC
             MOVE BWOR-BNKRPT-SCORE(1)    TO LMC-ORG-NUM
             MOVE BWCH-BNKRPT-SCORE(1)    TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-BNKRPT-SCORE(2) NOT = BWCH-BNKRPT-SCORE(2)
             MOVE "20 "                   TO LMC-FIELD
             MOVE "BANK SCR"              TO LMC-DESC
             MOVE BWOR-BNKRPT-SCORE(2)    TO LMC-ORG-NUM
             MOVE BWCH-BNKRPT-SCORE(2)    TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-BNKRPT-SCORE(3) NOT = BWCH-BNKRPT-SCORE(3)
             MOVE "21 "                   TO LMC-FIELD
             MOVE "BANK SCR"              TO LMC-DESC
             MOVE BWOR-BNKRPT-SCORE(3)    TO LMC-ORG-NUM
             MOVE BWCH-BNKRPT-SCORE(3)    TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-BNKRPT-SCORE(4) NOT = BWCH-BNKRPT-SCORE(4)
             MOVE "22 "                   TO LMC-FIELD
             MOVE "BANK SCR"              TO LMC-DESC
             MOVE BWOR-BNKRPT-SCORE(4)    TO LMC-ORG-NUM
             MOVE BWCH-BNKRPT-SCORE(4)    TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-DEALTRACK-CHG-DATE NOT = BWCH-DEALTRACK-CHG-DATE
             MOVE "23 "                   TO LMC-FIELD
             MOVE "DLRCHGDT"              TO LMC-DESC
             MOVE BWOR-DEALTRACK-CHG-DATE TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-ORG-DATE
             MOVE BWCH-DEALTRACK-CHG-DATE TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-UMC-EXPIRE-DATE NOT = BWCH-UMC-EXPIRE-DATE
             MOVE "24 "                   TO LMC-FIELD
             MOVE "UMCEXPIR"              TO LMC-DESC
             MOVE BWOR-UMC-EXPIRE-DATE TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-ORG-DATE
             MOVE BWCH-UMC-EXPIRE-DATE TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-UMC-PR-EXPIRE-DATE NOT = BWCH-UMC-PR-EXPIRE-DATE
             MOVE "25 "                   TO LMC-FIELD
             MOVE "UMCPREXPIR"            TO LMC-DESC
             MOVE BWOR-UMC-PR-EXPIRE-DATE TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-ORG-DATE
             MOVE BWCH-UMC-PR-EXPIRE-DATE TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

       LOG-MAINTENANCE-CHGS-EXIT.
           EXIT.

      ******************************************************************
       LOG-FIELD-CHANGE SECTION.
      ******************************************************************

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       LOG-FIELD-CHANGE-EXIT.
           EXIT.

      ***********************************
       DISPLAY-FIELDS SECTION.
      ***********************************

           MOVE SPACES TO DISPLAY-BUF.
           
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                                         INTO D-NAME.
            
           MOVE BW-PRLOANS    TO F01.
           MOVE BW-LOANS      TO F02.
           MOVE BW-SRCD       TO F03.
           MOVE BW-INTYTD     TO F04.
           MOVE BW-INTLYR     TO F05.
           MOVE BW-RE-INTYTD  TO F06.
           MOVE BW-RE-INTLYR  TO F07.
           MOVE BW-LIASMLN    TO F08.
           MOVE BW-LIAOTH     TO F09.
           MOVE BW-LASTPODATE TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY   TO F10.
       
           MOVE BW-PRIOR-RN-FOR10-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO F11.

           MOVE BW-LAST-RN-FOR10-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO F12.

           MOVE BW-LAST-FORM-NO TO F13.

           MOVE BW-LAST-FORM-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F14.

           MOVE 1 TO SUB.
       DISPLAY-NEXT-SCORE.
           MOVE BW-CRED-SCORE(SUB) TO F-CRSCORE(SUB).
           MOVE BW-BNKRPT-SCORE(SUB) TO F-BNKSCORE(SUB).
           IF SUB < 4
              ADD 1 TO SUB
              GO TO DISPLAY-NEXT-SCORE.

           MOVE BW-DEALTRACK-CHG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F23.

           MOVE BW-UMC-EXPIRE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F24.

           MOVE BW-UMC-PR-EXPIRE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F25.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO BWMAN2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWMAN2-SYSDATE.
           DISPLAY BWMAN2-SCREEN UPON BWMAIN-WINDOW-HANDLE.
           MOVE BWMAN2-FORM-FIELDS TO WR-BUF.
           MOVE BWMAN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BWMAN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWMAN2-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/BWMAN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-PARAGRAPHS SECTION.
      *************************************
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
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *************************************
       IO-ROUTINE SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *************************************
       END-ROUTINE SECTION.
      *************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWMAN2-SCREEN.
           MOVE BWMAIN-LINK TO FORM-LINK-AREA.
       STOP-RUN.
           EXIT PROGRAM.
