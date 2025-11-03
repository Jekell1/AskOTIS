       IDENTIFICATION DIVISION.
       PROGRAM-ID.      OUTCHK.
       DATE-WRITTEN. 10-09-84
      ***************************************************************
      *
      *  DESCRIPTION:  OUTSTANDING CHECK LISTING
      *  REV:
      *  BLV 990323 REMOVE ROUTINES TO COMBINE LOAN & NOTE CKS: NOTE
      *             CKS ARE NOW IN LOAN CKFILE WITH TYPE OF "N"
      *  BLV 990323 PRINT CK-TYPE ON DETAIL-LINE
      *  GLF 990629 ADD BEGINNING AND ENDING DATE RANGE (WAC#210)
      *  BAH 020305 ADDED TEST OF CK-VOID = "R", REPLACEMENT VOID
      *  BAH 020412 ALLOW TO WALK ACROSS BRANCHES
      *  GLB 030623 SUBSTITUTE WORKER FIELD "CK-TOTAL" TO "CHECK-TOTAL"
      *             TO AVOID CONFUSION WITH MASTER RECORD
      *  GLB 030623 ADD/PRINT "CHECK-VOIDS" IN ORDER TO BALANCE
      *  GLB 030623 ADD/PRINT CHECK COUNTER
      *  BLV 031017 ADD OPTION TO PRINT AP CHECKS, SKIP, INCLUDE,
      *             ONLY - IF SELECTED WILL PRINT SEPARATELY AT END
      *             OF REPORT, FOR WORLD, CAROLINA FIRST CHECK REC
      *  BLV 031111 FIX AP CHECKS TO RECOGINIZE "R" (REPRINT) AS
      *             BEING VOIDED
      *  GLB 031205 ADD VN-FILE, VMFILE; CHECK FOR VN-NAME-FLAG = "Y"
      *             AND USE VM-MEMO INSTEAD OF VN-NAME
      *  BAH 060104 DO NOT SKIP CHECKS THAT WERE VOIDED AFTER THE
      *             ENDING DATE RANGE. EXAMPLE: CHECK WRITTEN 10/28/05
      *             THEN VOIDED 11/03/05, RUNNING OUTCHK FOR 10/01/05 THRU
      *             10/31/05 SKIPPING VOIDS, DO NOT SKIP THAT VOIDED CHECK
      *             BECAUSE IT WAS VOIDED AFTER THE ENDING DATE RANGE
      *             COSMO PR# 2677
      *  BLM 100115 SKIP AP CONTINUATION CKS (TYPE = C, AMT = 0),
      *             THESE GET DON'T GET CLEARED & CONTINUE TO PRINT
      *             ON THIS REPORT, WORLD PR# 665
      *  BAH 131120 ADDED BEGIN AND END CLEAR DATE, MAS PR# 4415
      *
      * KEC 2015.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED LONG-NAME COMING FROM VMFILE;
      *         WAS MERGED INTO VNFILE IN A15-TO-A30 CONVERSION; UG1530
      *  BAH 170707 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 180402 PRESSING F3 SETS BEG-CLEAR-DATE TO 1950 AND
      *             CK-CLEARED-DATE IS < THAT, DONT TEST ON F3
      *  BAH 181008 GLOBAL CKFILE
      *
      * KEC 2015-1026 REMOVE NOTES PAYABLE A32 - RMNP32
      *          REMOVED THE REMAINING LIBNP COPY MEMBERS WHICH
      *          WERE FOR CHECKS THAT WERE IN ACCOUNTS PAYABLE (AP);
      *          USING THE AP CHECKS COPY MEMBERS BUT NEED TO DO
      *          A 'REPLACING LEADING' OF "CK" TO "APCK" SO IT CAN
      *          WORK WITH LOAN PROCESSING (LP) CHECKS.
      *
      * BAH 20190919 REMOVED ACCESS-CALL ON CKFILE
      * BAH 20191023 REMOVED INCL-NOTES, INCL-AP AND ALL THINGS RELATED
      * BAH 20200827 REMOVED RESET ON BR1
      * RMZ 20230921 PERFORMANCE TUNING - ADDED STORED PROCEDURE 
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/OUTCHK.CBL S35 12/22/23-11:40:14 >".

       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

      ***********************************
      *   LINE WORKERS / SSP WORKERS    *
      ***********************************

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS1-WBEG-DATE       PIC X(10).                   
       01  QWS1-WEND-DATE       PIC X(10).                
       01  QWS1-WBEG-CDATE      PIC X(10).                 
       01  QWS1-WEND-CDATE      PIC X(10).               
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  CK1-CONN-OPEN-SW     PIC X        VALUE "N".

       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6) COMP-3.
       01  DTL-CNT              PIC 9 VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  WK-GLNO              PIC 9(11).
       01  WK-GL REDEFINES WK-GLNO.
           03  WK-GLBR          PIC 9(4).
           03  WK-GLNUM         PIC 9(7).

       01  LAST-CKNO            PIC 9(6)     VALUE 0.

      * GLB  06/23/03
       01  CHECK-TOTAL          PIC S9(9)V99 COMP-3 VALUE 0.
       01  CHECK-VOIDS          PIC S9(9)V99 COMP-3 VALUE 0.
       01  CHECK-COUNT          PIC 9(9).
       01  VOID-COUNT           PIC 9(9).

       01  GRAND-TOTAL          PIC S9(9)V99 COMP-3 VALUE 0.
       01  GRAND-VOIDS          PIC S9(9)V99 COMP-3 VALUE 0.
       01  GRAND-COUNT          PIC 9(9).
       01  VOID-GRAND           PIC 9(9).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CHECK       PIC 9(6).
           03  END-CHECK       PIC 9(6).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  INCL-CLEARED    PIC X.
           03  INCL-VOIDED     PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  BEG-CLEAR-DATE  PIC 9(8).
           03  END-CLEAR-DATE  PIC 9(8).

       01 VERSION-CONTROL      PIC X VALUE "D".

       01  SV-BEG-BR                          PIC 9(4)     VALUE 9999.
       01  SV-END-BR                          PIC 9(4)     VALUE 9999.
       01  SV-GROUP-FLAG                      PIC X(1)     VALUE SPACES.
       01  SV-ENT-GROUP                       PIC X(4)     VALUE SPACES.

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

       01  HEAD2-CLEARED        PIC X(41) VALUE
           "- - - - - CLEARED CHECK LISTING - - - - -".
       01  HEAD2-BOTH           PIC X(41) VALUE
           "- - - - - - - CHECK LISTING - - - - - - -".

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE          PIC X(41)    VALUE
           "- - - - OUTSTANDING CHECK LISTING - - - -".
           03  FILLER           PIC X(21)    VALUE  " ".
           03  FILLER           PIC X(16)    VALUE  "# = SEQ BREAK   ".
           03  FILLER           PIC X(8)     VALUE "* = VOID".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC X(2)     VALUE " ".
           03  HD-BRNO     PIC 9(4).
           03  FILLER      PIC X        VALUE " ".
           03  HD-BRNAME   PIC X(20).
           03  FILLER      PIC X(68)    VALUE " ".
           03  FILLER      PIC X(29)    VALUE
               "(B-BATCH, M-MANUAL, S-SYSTEM)".

       01  HEAD4.
           03  FILLER      PIC X(23) VALUE "TYPE  CHECK#   DATE  ".
           03  FILLER      PIC X(4)  VALUE " ".
           03  FILLER      PIC X(14) VALUE "AMOUNT   PAYEE".
           03  FILLER      PIC X(27) VALUE " ".
           03  FILLER      PIC X(14) VALUE "REF #  REASON ".
           03  FILLER      PIC X(27) VALUE " ".
           03  FILLER      PIC X(9)  VALUE "GL-NUMBER".
           03  FILLER      PIC X(7)  VALUE " ".
           03  FILLER      PIC X(7)  VALUE "AMOUNT".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-PAID      PIC X.
           03  D-VOID      PIC X.
           03  FILLER      PIC X.
           03  D-TYPE      PIC XX.
           03  D-BREAK     PIC X.
           03  D-CKNO      PIC ZZZZZ9.
           03  FILLER      PIC X.
           03  D-CKDATE    PIC 99/99/99.
           03  FILLER      PIC X.
           03  D-CKAMT     PIC ZZZZ,ZZZ.99-.
           03  D-CLEARED   PIC X.
           03  FILLER      PIC X.
           03  D-PAYEE     PIC X(30).
           03  FILLER      PIC X.
           03  D-REFNO     PIC ZZZZZ9.
           03  FILLER      PIC XX.
           03  D-REASON    PIC X(30).
           03  FILLER      PIC X.
           03  D-GLBR      PIC 9(4).
           03  D-DASH      PIC X.
           03  D-GLNUM     PIC 9(7).
           03  FILLER      PIC XX.
           03  D-GLAMT     PIC ZZZZ,ZZZ.99-.

       01  T-LINE REDEFINES DETAIL-LINE.
           03  T-TEXT1     PIC X(7).
           03  T-BRNO      PIC Z(4).
           03  T-TEXT2     PIC X(8).
      * GLB 06/23/03
           03  T-CKTOTAL-AREA.
               05  T-CKTOTAL   PIC ZZZ,ZZZ,ZZZ.99-.
           03  FILLER          PIC X(4).
           03  T-CKTOTAL-AREA2.
               05  T-CKPAREN1  PIC X.
               05  T-CKCOUNT   PIC Z(9)9.
               05  T-CKPAREN2  PIC X.
           03  FILLER          PIC X(83).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC 9(8).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/OUTCHK_DEF.CPY".
       COPY "LIBLP/OUTCHK_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/OUTCHK_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-CK1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "OUTCHK" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

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

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "REPORT IN PROGRESS...............", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO CK-PATH-OVERRIDE.

           PERFORM OPEN-CK1-FILE.

           MOVE BR-NO     TO CK-BRNO
                             QCK1-WBEG-BRNO
                             QCK1-WEND-BRNO.
           MOVE BEG-CHECK TO CK-NO
                             QCK1-WBEG-CKNO.
           MOVE END-CHECK TO QCK1-WEND-CKNO.

           PERFORM START-CK1-FILE. *> SSP - CKFILE

       NEXT-CK.
           PERFORM READ-CK1-FILE-NEXT. *> EMBEDDED - CKFILE
           IF IO-FG = 9 OR (CK-BRNO NOT = CK-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF CK-NO < BEG-CHECK OR CK-NO > END-CHECK
              GO TO NEXT-CK.

           IF INCL-CLEARED = "O"
              IF CK-CLEARED-DATE = 0
                 GO TO NEXT-CK.

           IF INCL-CLEARED = "S"
              IF CK-CLEARED-DATE NOT = 0
                 GO TO NEXT-CK.

           IF INCL-VOIDED = "O"
              IF NOT (CK-VOID = "R" OR "Y")
                 GO TO NEXT-CK.

      * EXAMPLE: CHECK WRITTEN 10/28/05, VOIDED 11/03/05, RUN THIS FOR
      *          10/01/05 THRU 10/31/05 SKIPPING VOIDS, THIS CHECK SHOULD
      *          PRINT ! COSMO PR# 2677
           IF INCL-VOIDED = "S"
              IF CK-VOID = "R" OR "Y"
      * IF ALL DATE RANGE, OR MAX ENDING DATE, SKIP VOIDS IF ASKED TO
                 IF END-DATE = EXT-JULIAN-END
                    GO TO NEXT-CK
                 ELSE
      * IF AN ENDING DATE ENTERED, TEST WHEN CHECK WAS VOIDED
      * DONT SKIP CHECKS THAT WERE VOIDED AFTER ENDING DATE RANGE
                    IF CK-VOID-DATE NOT > END-DATE
                       GO TO NEXT-CK.

           IF CK-DATE < BEG-DATE
              GO TO NEXT-CK.

           IF CK-DATE > END-DATE
              GO TO NEXT-CK.

      * CK-CLEARED-DATE = 0 IS < BEG-CLEAR-DATE WHEN F3 ALL
      * IF F3 WAS NOT PRESSED, THEN TEST THE DATE
           IF NOT (BEG-CLEAR-DATE  = EXT-JULIAN-BEG AND
                    END-CLEAR-DATE = EXT-JULIAN-END)
              IF CK-CLEARED-DATE   < BEG-CLEAR-DATE
                 GO TO NEXT-CK
              ELSE
                 IF CK-CLEARED-DATE > END-CLEAR-DATE
                    GO TO NEXT-CK.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-CK.

       END-OF-GROUP.
           PERFORM CLOSE-CK1-FILE.

            
      * ADDED THIS IN A32/A34 BECAUSE A15 WAS DOING AN ACCESS CALL 
      * AND NOT PERFORMING ANY OF IT IF NO CHECK FILE, HERE IT WAS PRINTING
      * PAGES OF 0 TOTALS. 
           IF CHECK-TOTAL NOT = 0 OR
               CHECK-VOIDS NOT = 0 OR
                CHECK-COUNT NOT = 0 OR
                 VOID-COUNT NOT = 0
              PERFORM BRANCH-TOTALS.

           MOVE "Y" TO FIRST-TIME.
           MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE "GRAND TOTAL       :" TO DETAIL-LINE.
           MOVE GRAND-TOTAL TO T-CKTOTAL.
           MOVE "(" TO T-CKPAREN1.
           MOVE GRAND-COUNT TO T-CKCOUNT.
           MOVE ")" TO T-CKPAREN2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " VOIDS:  " TO T-TEXT2.
           MOVE GRAND-VOIDS TO T-CKTOTAL.
           MOVE "(" TO T-CKPAREN1.
           MOVE VOID-GRAND  TO T-CKCOUNT.
           MOVE ")" TO T-CKPAREN2.
           PERFORM WRITE-DETAIL-LINE.


           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      /**********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.
           MOVE OUTCHK-QUEUE-POS TO Q-POS.
           MOVE OUTCHK-COPIES-POS TO C-POS.
           MOVE OUTCHK-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE " " TO EXT-EOBUF-ERRCD.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS..............." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.


           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           MOVE 0 TO CHECK-TOTAL CHECK-VOIDS
                     CHECK-COUNT VOID-COUNT
                     GRAND-TOTAL GRAND-VOIDS
                     GRAND-COUNT VOID-GRAND.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE BEG-CLEAR-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE END-CLEAR-DATE.

      ******************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO INC-CLEARED.

       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

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
              GO TO CHECK-BRANCH-SECURITY.

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
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.

           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              GO TO BR-01.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       BEG-CK.
           MOVE "ENNN060BEGCK." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CK.
           IF VDUSTATUS = "1U" GO TO BR-02..
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEG-CK.
           MOVE VDUNUM0 TO BEG-CHECK.
       END-CK.
           MOVE "ENNN060ENDCK." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEG-CK.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO END-CK.
           MOVE VDUNUM0 TO END-CHECK.
           IF BEG-CHECK > END-CHECK
              MOVE "INVALID CHECK RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-CK.
           GO TO DT1.
       ALL-CK.
           MOVE "SNNN060BEGCK." TO VDU.
           MOVE 0 TO BEG-CHECK VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ENDCK." TO VDU.
           MOVE 999999 TO END-CHECK VDUNUM0.
           PERFORM SCREENIO.
       DT1.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BEG-CLR.
           MOVE "ERNM080BEGDT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO BEG-CK.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO DT1.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       DT2.
           MOVE "ERNM080ENDDT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO DT1.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO DT2.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE !!! " TO MESS
              PERFORM SEND-MESS
              GO TO DT1.
           GO TO BEG-CLR.
       ALL-DT.
           MOVE "S  8080BEGDT." TO VDU.
           MOVE EXT-JULIAN-BEG  TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080ENDDT." TO VDU.
           MOVE EXT-JULIAN-END  TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
       BEG-CLR.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO INC-CLEARED.
           MOVE "ERNM080CLRDT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CLEAR.
           IF VDUSTATUS = "1U" GO TO DT2.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO BEG-CLR.
           MOVE VDU-DATE-YYYYMMDD TO BEG-CLEAR-DATE.
       END-CLR.
           MOVE "ERNM080CLRDT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BEG-CLR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO END-CLR.
           MOVE VDU-DATE-YYYYMMDD TO END-CLEAR-DATE.

           IF END-CLEAR-DATE < BEG-CLEAR-DATE
              MOVE "INVALID DATE RANGE !!! " TO MESS
              PERFORM SEND-MESS
              GO TO BEG-CLR.

           GO TO INC-CLEARED.

       ALL-CLEAR.
           MOVE "S  8080CLRDT1." TO VDU.
           MOVE EXT-JULIAN-BEG   TO BEG-CLEAR-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080CLRDT2." TO VDU.
           MOVE EXT-JULIAN-END   TO END-CLEAR-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       INC-CLEARED.
           MOVE "E  A010CLEARED." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" 
              IF EXT-ROUTE-BUF = "40" OR "50"
                 GO TO END-CK
              ELSE
                 GO TO END-CLR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO INC-CLEARED.
           MOVE VDUBUF TO INCL-CLEARED.
           IF NOT (INCL-CLEARED = "I" OR "S" OR "O")
              GO TO INC-CLEARED.

       INC-VOIDED.
           MOVE "E  A010VOIDED." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO INC-CLEARED.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO INC-VOIDED.
           MOVE VDUBUF TO INCL-VOIDED.
           IF NOT (INCL-VOIDED = "I" OR "S" OR "O")
              GO TO INC-VOIDED.
       EXIT-PARM.
           EXIT.

       DISPLAY-PARAMETERS SECTION.

           MOVE "SNNN040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE END-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060BEGCK." TO VDU.
           MOVE BEG-CHECK TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ENDCK." TO VDU.
           MOVE END-CHECK TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A010CLEARED." TO VDU.
           MOVE INCL-CLEARED TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010VOIDED." TO VDU.
           MOVE INCL-VOIDED TO VDUBUF.
           PERFORM SCREENIO.

      /***************************
      *   DETAIL LINE PROCESS    *
      ****************************
       DETAIL-LINE-PROCESS SECTION.

           IF FIRST-TIME = "Y"
      * DO NOT WANT A BREAK IN SEQUENCE ON FIRST CHECK !!
              COMPUTE LAST-CKNO = CK-NO - 1
              MOVE 1 TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           MOVE CK-TYPE TO D-TYPE.
           IF CK-VOID = "Y" OR "R"
              MOVE "*" TO D-VOID.
           IF (LAST-CKNO + 1) NOT = CK-NO
              MOVE "#" TO D-BREAK.
           MOVE CK-NO TO LAST-CKNO.

           MOVE CK-PAID TO D-PAID.
           MOVE CK-NO TO D-CKNO.
           MOVE CK-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-CKDATE.
           MOVE CK-AMT TO D-CKAMT.

      *  GLB  06/23/03
           IF NOT (CK-VOID = "Y" OR "R")
              ADD CK-AMT TO CHECK-TOTAL GRAND-TOTAL
              ADD 1 TO CHECK-COUNT GRAND-COUNT
           ELSE
             ADD CK-AMT TO CHECK-VOIDS GRAND-VOIDS
             ADD 1 TO VOID-COUNT VOID-GRAND.

           IF INCL-CLEARED = "I"
              IF CK-CLEARED-FG NOT = " "
                 MOVE "C" TO D-CLEARED.
           MOVE CK-PAYEE TO D-PAYEE.
           MOVE CK-REF TO D-REFNO.
           MOVE CK-REASON TO D-REASON.
           IF CK-TYPE = "MS"
             MOVE CK-GLNO(1) TO WK-GLNO
             MOVE WK-GLBR TO D-GLBR
             MOVE "-" TO D-DASH
             MOVE WK-GLNUM TO D-GLNUM
             MOVE CK-GLAMT(1) TO D-GLAMT
             MOVE 1 TO DTL-CNT
             MOVE 2 TO SUB.
           PERFORM WRITE-DETAIL-LINE.

           IF CK-TYPE NOT = "MS"
             GO TO END-DETAIL-LN-PROCESS.
       PRINT-GLDIST.
           IF SUB > 5
      *      IF DTL-CNT > 1
      *        MOVE 1 TO LN-FEED
      *        PERFORM WRITE-DETAIL-LINE
               GO TO END-DETAIL-LN-PROCESS.
           IF CK-GLNO(SUB) = 0
             OR CK-GLAMT(SUB) = 0
               ADD 1 TO SUB
               GO TO PRINT-GLDIST.
           MOVE CK-GLNO(SUB) TO WK-GLNO.
           MOVE WK-GLBR TO D-GLBR.
           MOVE "-" TO D-DASH.
           MOVE WK-GLNUM TO D-GLNUM.
           MOVE CK-GLAMT(SUB) TO D-GLAMT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           ADD 1 TO SUB.
           ADD 1 TO DTL-CNT.
           GO TO PRINT-GLDIST.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ****************************
      *      BRANCH TOTALS       *
      ****************************
       BRANCH-TOTALS SECTION.
           MOVE "BRANCH " TO T-TEXT1.
           MOVE BR-NO TO T-BRNO.
           MOVE " TOTAL:  " TO T-TEXT2.
           MOVE CHECK-TOTAL TO T-CKTOTAL.
           MOVE "(" TO T-CKPAREN1.
           MOVE CHECK-COUNT TO T-CKCOUNT.
           MOVE ")" TO T-CKPAREN2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " VOIDS:  " TO T-TEXT2.
           MOVE CHECK-VOIDS TO T-CKTOTAL.
           MOVE "(" TO T-CKPAREN1.
           MOVE VOID-COUNT  TO T-CKCOUNT.
           MOVE ")" TO T-CKPAREN2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 0 TO CHECK-TOTAL CHECK-VOIDS
                     CHECK-COUNT VOID-COUNT.

      ****************************
      *    WRITE DETAIL LINE     *
      ****************************
       WRITE-DETAIL-LINE SECTION.
       WRITE-DET-LINE.
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
             PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
      *        HEAD PAGE ROUTINE    *
      *******************************
       HEAD-PAGE SECTION.
       HEAD-ROUTINE.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF INCL-CLEARED = "O"
              MOVE HEAD2-CLEARED TO H-TITLE
           ELSE
              IF INCL-CLEARED = "I"
                 MOVE HEAD2-BOTH TO H-TITLE.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE BR-NO TO HD-BRNO.
           MOVE BR-NAME TO HD-BRNAME.
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *******************************
      *       RANGE LINES           *
      *******************************
       PRINT-RANGE-LINES SECTION.
           MOVE 4 TO LN-FEED.
           MOVE "BEGINNING BRANCH" TO RANGE-LINE.
           MOVE BEG-BRANCH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING BRANCH" TO RANGE-LINE.
           MOVE END-BRANCH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEGINNING CHECK #" TO RANGE-LINE.
           MOVE BEG-CHECK TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING CHECK #" TO RANGE-LINE.
           MOVE END-CHECK TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEGINNING DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING DATE" TO RANGE-LINE.
           MOVE END-DATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CLEAR" TO RANGE-LINE.
           MOVE BEG-CLEAR-DATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING CLEAR" TO RANGE-LINE.
           MOVE END-CLEAR-DATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CLEARED CHECKS" TO RANGE-LINE.
           MOVE INCL-CLEARED TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "VOIDED CHECKS" TO RANGE-LINE.
           MOVE INCL-VOIDED TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO OUTCHK-CONAME.
           MOVE EXT-CONAME-SYSDATE TO OUTCHK-SYSDATE.
           DISPLAY OUTCHK-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE OUTCHK-FORM-FIELDS TO WR-BUF.
           MOVE OUTCHK-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE OUTCHK-HELP--FILE TO HELP-LINK-FILE.
           MOVE OUTCHK-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/OUTCHK_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************************
       MISC-ROUTINES SECTION.
      *********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".

      *********************************************************
       IO-ROUTINE SECTION.
      *********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
   
      * COPY "LIBLP/LPCK1RN.CPY".
      * COPYMEMBER: LIBLP/LPCK1RN
       LOAD-CK1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CK-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CK-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CK-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CK-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CK-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CK-ALL-BASE
              MOVE FILPATH-MACHINE         TO CK-ALL-MACHINE
              MOVE FILPATH-DATA            TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH.

      *-----------------------------------------------------------------
       OPEN-CK1-FILE.
           PERFORM LOAD-CK1-FILE.
           PERFORM OPEN-IT.
           MOVE CK-PATH-SQL TO E-FILE.

           IF CK1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO CK1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS CK1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.


      *-----------------------------------------------------------------
       START-CK1-FILE. *> SSP - CKFILE
           PERFORM START-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO.
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.

           IF ( CK1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CK1-FILE.

           IF ( QCK1-WEND-BRNO = ZEROES    ) OR
              ( QCK1-WEND-BRNO NOT NUMERIC )
              MOVE CK-PATH-OWNBR        TO QCK1-WBEG-BRNO
              MOVE CK-PATH-OWNBR        TO QCK1-WEND-BRNO
              MOVE CK-NO                TO QCK1-WBEG-CKNO
              MOVE ALL "9"              TO QCK1-WEND-CKNO.

              MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
              PERFORM SQL-SET-DATE.
              MOVE SQL-DATE-YYYY-MM-DD  TO QWS1-WBEG-DATE.
              MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
              PERFORM SQL-SET-DATE.
              MOVE SQL-DATE-YYYY-MM-DD  TO QWS1-WEND-DATE.

              MOVE BEG-CLEAR-DATE       TO SQL-DATE-YYYYMMDD.
              PERFORM SQL-SET-DATE.
              MOVE SQL-DATE-YYYY-MM-DD  TO QWS1-WBEG-CDATE.
              MOVE END-CLEAR-DATE       TO SQL-DATE-YYYYMMDD.
              PERFORM SQL-SET-DATE.
              MOVE SQL-DATE-YYYY-MM-DD  TO QWS1-WEND-CDATE.

      *  STORED PROCEDURE:
      *
      *   SELECT CKFILE.CK_BRNO,
      *          CKFILE.CK_NO,
      *          CKFILE.CK_TYPE,
      *          CAST(CKFILE.CK_DATE AS VARCHAR(10)),
      *          CKFILE.CK_AMT,
      *          CKFILE.CK_PAYEE,
      *          CKFILE.CK_REF,
      *          CKFILE.CK_REASON,
      *          CKFILE.CK_GLNO_1,
      *          CKFILE.CK_GLNO_2,
      *          CKFILE.CK_GLNO_3,
      *          CKFILE.CK_GLNO_4,
      *          CKFILE.CK_GLNO_5,
      *          CKFILE.CK_GLAMT_1,
      *          CKFILE.CK_GLAMT_2,
      *          CKFILE.CK_GLAMT_3,
      *          CKFILE.CK_GLAMT_4,
      *          CKFILE.CK_GLAMT_5,
      *          CKFILE.CK_VOID, 
      *          CKFILE.CK_PAID, 
      *          CAST(CKFILE.CK_VOID_DATE    AS VARCHAR(10)),
      *          CKFILE.CK_CLEARED_FG,
      *          CAST(CKFILE.CK_CLEARED_DATE AS VARCHAR(10))
      *    FROM DBO.CKFILE
      *   WHERE CKFILE.CK_BRNO               = @QCK1_WBEG_BRNO
      *     AND CKFILE.CK_NO           BETWEEN @QCK1_WBEG_CKNO
      *                                    AND @QCK1_WEND_CKNO
      *     AND CKFILE.CK_DATE         BETWEEN @QWS1_WBEG_DATE
      *                                    AND @QWS1_WEND_DATE
      *     AND CKFILE.CK_CLEARED_DATE BETWEEN @QWS1_WBEG_CDATE
      *                                    AND @QWS1_WEND_CDATE
      *   ORDER BY CKFILE.CK_BRNO,
      *            CKFILE.CK_NO

           EXEC SQL SET CONNECTION CK1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE CK1_CURSOR CURSOR FOR
                :CK1-RETURN-CODE = EXEC SSP_OUTCHK_CKFILE_SELECT 
              ( :QCK1-WBEG-BRNO,  
                :QCK1-WBEG-CKNO,  
                :QCK1-WEND-CKNO, 
                :QWS1-WBEG-DATE, 
                :QWS1-WEND-DATE,  
                :QWS1-WBEG-CDATE, 
                :QWS1-WEND-CDATE )
           END-EXEC.

           EXEC SQL
               OPEN CK1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCK1-WBEG-BRNO 
                          QCK1-WEND-BRNO
                          QCK1-WBEG-CKNO
                          QCK1-WEND-CKNO.

           MOVE STAT---GOOD TO CK1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-CK1-FILE-NEXT. *> EMBEDDED - CKFILE
           PERFORM READ-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO.
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.
           INITIALIZE QCK-REC.

           EXEC SQL SET CONNECTION CK1_CONNECT END-EXEC.

           IF ( CK1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH CK1_CURSOR 
                  INTO 
                    :QCK-BRNO,
                    :QCK-NO,
                    :QCK-TYPE,
                    :QCK-DATE,
                    :QCK-AMT,
                    :QCK-PAYEE,
                    :QCK-REF,
                    :QCK-REASON,
                    :QCK-GLNO-1,
                    :QCK-GLNO-2,
                    :QCK-GLNO-3,
                    :QCK-GLNO-4,
                    :QCK-GLNO-5,
                    :QCK-GLAMT-1,
                    :QCK-GLAMT-2,
                    :QCK-GLAMT-3,
                    :QCK-GLAMT-4,
                    :QCK-GLAMT-5,
                    :QCK-VOID, 
                    :QCK-PAID, 
                    :QCK-VOID-DATE,
                    :QCK-CLEARED-FG,
                    :QCK-CLEARED-DATE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CK1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CK-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-CK1-FILE.

           PERFORM CLOSE-IT.
           IF ( CK1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION CK1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE CK1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC .
              MOVE STAT---BAD TO CK1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-CK1-CONNECTION.

           IF CK1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION CK1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT CK1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO CK1-CONN-OPEN-SW.


       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *********************************************************
       END-PROGRAM SECTION.
      *********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY OUTCHK-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
