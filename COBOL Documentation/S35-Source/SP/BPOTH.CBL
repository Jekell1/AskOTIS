       IDENTIFICATION   DIVISION.
       PROGRAM-ID.        BPOTH.
       DATE-WRITTEN.  2002/10/15.
      **************************************************************************
      *
      *                   ( SP/BPOTH )
      *
      *                  FROM PAYMENT REFERENCE CODE
      *                  TO POST  O T H E R  C H A R G E S
      *                  VIA: LP/LONPWA.C
      *
      *     DESCRIPTION:  BREAK OUT THE GLOBAL BATCH FILE
      *                   INTO EACH INDIVIDUAL BRANCH FILE
      *
      *     NOTE: TO RUN IN BATCH, BPOTH WILL BREAK OUT THE FILES, BUT YOU
      *           THEN NEED SP/OTHUPD TO CALL LONPWA WHICH CALLS LONPF7 (OT)
      *           AND LONPF2 TO ACTUALLY POST THE "OT"
      *
      * REV.
      * JTG 021015 *     * COPIED FROM BPBKB4.C                     REGACC #0006
      * JTG 021114 ADDED TEST FOR CD-BR-FILE-TYPE = "LC" WHICH INDICATES
      *            THIS IS NOT A VALID BATCH PAYMENT REFCD FOR PROCESSING
      *            BUT ONLY USED FOR TESTING IF LATE CHARGES WILL BE
      *            ASSESED RE: CD-BR-LC-FG                          REGACC #0006
      * JTG 021216 CHANGED TO TRUNCATE CD-BR-FILE-NAME TO 5 CHARCTERS
      *            EX. CCFILE TO CCFIL                              REGACC #0006
      * JTG 030321 REMOVED BT-BATCH-NO & BT-SEQNO FROM RECORD FORMAT
      *            FIELDS WERE NOT BEING USED                       JTG-JR
      * JTG 040407 *     *
      *            ADDED GP-OTREF-NEXT-BATCH  900-909              CENTRIX #2254
      * BLF 070212 TOOK "MELLON BANK" OUT OF DESC OF TRAILER RECORD
      *  CS 140602 REPLACED MKDIR-CMD WITH C$MAKEDIR
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * BLM 160718 ADDED BP-SEQNO TO BP KEY SO WE CAN HAVE MORE THAN 1
      *            PAYMENT FOR A LOAN, PD0003
      * BAH 170808 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0912 {PR#01246} WORLD <A30> [JAY CISZEWSKI]
      *          WHEN ADDING THIS PR# TO A30, I NOTICED THAT A COPY
      *          MEMBER WAS NOT PROPERLY SETUP.  TO FIX IT I DID:
      *          - COPIED LIBLP/LPBP.CPY TO ../SV/LPBP.NO
      *          - MOVED  LIBLP/LPBP.CPY TO LIBLP/LPBP_CMD.CPY
      *          - EDITED LIBLP/LPBP_CMD.CPY TO LOWERCASE WHEN NEEDED
      * BAH 181004 GLOBAL BWFILE
      * BAH 181212 GLOBAL LNFILE
      * BAH 190130 SPLIT CDFILE
      * BAH 20191007 REMOVED ACCESS-CALL LNFILE
      *  CS 20201008 ADD BT-FROZEN-OVERRIDE PR#1465
      *  CS 20201209 MOVE SPACES TO BT-REC BEFORE READ OF BT AS IT HAD VALUE
      *              THAT WAS FROM A PRIOR RECORD PR#1465A
      * BAH 20210205 DO NOT ALLOW CD-BR-FILE-TYPE "C", UMC DEBIT CARD PR#1490
      *  CS 20220404 ADDED BT-PAYDATE   PR#1550
      * BAH 20220519 MAKE BATCHABLE, AND USE TRANS-DATE IF PAYDATE IS 0 PR#1550B
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * ALLOTMENT BATCH TAPE FILE
           SELECT BT-FILE ASSIGN TO BT-PATH
                          ORGANIZATION LINE SEQUENTIAL
                          ACCESS SEQUENTIAL
                          LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                          FILE STATUS FILE-STAT.
       COPY "LIBLP/LPFSBP.CPY".
           SELECT OTH-LOG-FILE ASSIGN OTH-LOG-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.
      *-----------------------------------------------------------------
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      ***************************************************
      *    MAGNETIC BATCH PROCESSING FILE
      *        (  MELLON BANK VERSION  )
      *
      * RECORD SIZE: 80 BYTES
      *  BT-TYPE = "D" = DETAIL RECORD
      *            "T" = TRAILER RECORD
      *
      ***************************************************
       FD  BT-FILE
           LABEL RECORDS ARE STANDARD.
       01  BT-REC.
           03  BT-TYPE                   PIC X(01).
           03  BT-REC-AREA               PIC X(79).
           03  BT-REC-DATA REDEFINES BT-REC-AREA.
               05  BT-BRANCH             PIC 9(04).
               05  BT-ACCTNO             PIC 9(06).
               05  BT-AMT                PIC 9(08)V9(02).
               05  BT-FROZEN-OVERRIDE    PIC X.
               05  BT-PAYDATE            PIC 9(6).
               05  FILLER                PIC X(52).
           03  BT-REC-TRAILER REDEFINES BT-REC-AREA.
               05  BT-REC-COUNT          PIC 9(10).
               05  BT-TOTAL-AMOUNT       PIC 9(10)V9(02).
               05  FILLER                PIC X(57).

       COPY "LIBLP/LPFDBP.CPY".

      **************************************************
      *  OTHCODE BATCH LOG FILE                      *
      **************************************************
       FD  OTH-LOG-FILE
           LABEL RECORDS ARE STANDARD.
       01  OTH-LOG-REC.
           03  OTH-LOG-TIME      PIC X(8).
           03  FILLER            PIC X.
           03  OTH-LOG-USERID    PIC X(9).
           03  FILLER            PIC X.
           03  OTH-LOG-PROMPT1   PIC X(5).
           03  OTH-LOG-REFCD     PIC X(7).
           03  OTH-LOG-PROMPT2   PIC X(5).
           03  OTH-LOG-BATCH     PIC 9(3).
           03  FILLER            PIC X.
           03  OTH-LOG-MSG       PIC X(40).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(36) VALUE
           "@(#)SP/BPOTH S35 06/16/22-08:47:09 >".

      ******************************************************************
      *
      *     P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".
      *********************************************************
      *    /USR/DATA/R#/ED/CCFILE
      ******************************************************************
       01  BT-PATH.
           03  BT-PATH-BASE                   PIC X(09).
           03  FILLER                         PIC X(01) VALUE "/".
           03  BT-PATH-DISPLAY.
               05  BT-PATH-MACHINE            PIC X(02).
               05  FILLER                     PIC X(4) VALUE "/ED/".
               05  BT-PATH-BATCH-NAME         PIC X(6).
       01  BT-PATH-SHORT                 REDEFINES BT-PATH.
           03  BT-PATH-SHORT-NAME        PIC X(21).
           03  FILLER                    PIC X(01).

      ******************************************************************
      *    /USR/DATA/R#/XX####/ED/B####MMDD???REFCD
      ******************************************************************
       01  LOCAL-PATH.
           03  LOCAL-PATH-BASE          PIC X(09).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  LOCAL-PATH-MACHINE       PIC X(02).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  LOCAL-PATH-DATA          PIC X(06).
           03  FILLER                   PIC X(05)   VALUE "/ED/B".
           03  LOCAL-PATH-BRANCH.
               05  LOCAL-PATH-BRANCH-NO PIC 9(04).
           03  LOCAL-PATH-DATE-BATCH-REF.
               05  LOCAL-PATH-DATE      PIC 9(4).
               05  LOCAL-PATH-BATCH     PIC 9(3).
               05  LOCAL-PATH-REFCD     PIC X(5).
       01  LOCAL-PATH-DISPLAY-SCREEN    REDEFINES LOCAL-PATH.
           03  FILLER                   PIC X(20).
           03  LOCAL-PATH-DISPLAY       PIC X(30).

       01  OTH-LOG-PATH.
           03  OTH-LOG-SPOOL-DIR      PIC X(32).
           03  OTH-LOG-PATHNAME.
               05  FILLER             PIC X(10) VALUE
                   "/0000LOG".

      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(02) VALUE "PR".
       01  BP1-IFN              PIC X(03) VALUE "BP1".

       78  WS_QUOTE                       VALUE X'22'.

      ******************************************************************
      *     S C R E E N   F I E L D S
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  REFCD           PIC X(5).
           03  TRANS-DATE      PIC 9(8).
           03  FILLER REDEFINES TRANS-DATE.
               05  TRANS-CC    PIC 99.
               05  TRANS-YY    PIC 99.
               05  TRANS-MM    PIC 99.
               05  TRANS-DD    PIC 99.

       01  VERSION-CONTROL     PIC X VALUE "A".

      *=================================================================
      *
      *    L I N E - W O R K E R S
      *
      *=================================================================
       01  LN-FEED         PIC 9(02)  VALUE 0.
       01  LN-COUNT        PIC 9(03)  VALUE 99.
       01  PAGE-NUMBER         PIC 9(04)  VALUE 0.

      *=================================================================
      *
      *    M I S C - W O R K E R S
      *
      *=================================================================
       01  GRAND-TOTAL-FG       PIC X(01)      VALUE "N".
       01  SAVE-TOTAL-COUNT     PIC 9(6)       VALUE 0.
       01  FIRST-TIME           PIC X(01)      VALUE "Y".
       01  MESS-LIST            PIC X(05)      VALUE "MESS.".
       01  MESS-BUF.
           03  FILLER           PIC X(41) VALUE
           "BATCH BRANCH FILE CREATE IN PROGRESS.....".
           03  MESS-BR          PIC 9(04) VALUE 0.

       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  BATCH-NO             PIC 9(03).
       01  BATCH-DATE           PIC 9(04).
       01  FILLER REDEFINES BATCH-DATE.
           03  BATCH-MM         PIC 9(02).
           03  BATCH-DD         PIC 9(02).

       01  DUP-FLAG             PIC  X     VALUE "N".
      *-----------------------------------------------------------------
      *TOTAL COUNTS 1=BRANCH, 2=GRAND TOTALS
      *-----------------------------------------------------------------
       01  TOTAL-READ           OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMT-READ       OCCURS 2 PIC S9(09)V99 VALUE 0.
       01  TOTAL-OM             OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMT-OM         OCCURS 2 PIC S9(09)V99 VALUE 0.
       01  TOTAL-SKIP           OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMT-SKIP       OCCURS 2 PIC S9(09)V99 VALUE 0.
       01  TOTAL-DUPS           OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMT-DUP        OCCURS 2 PIC S9(09)V99 VALUE 0.
       01  TOTAL-ZERO           OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMT-ZERO       OCCURS 2 PIC S9(09)V99 VALUE 0.
       01  TOTAL-RECORDS        OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMOUNT         OCCURS 2 PIC S9(09)V99 VALUE 0.

       01  HOLD-BRANCH          PIC 9(05) VALUE 10000.
       01  HOLD-BP-REC          PIC X(BP-SIZE).
       01  HOLD-BP-SEQNO        PIC 99.

      *-----------------------------------------------------------------
      * SPOOL DIRECTORY/FILENAME FOR EXCEPTION REPORT
      *-----------------------------------------------------------------
       01  EX-SPOOL.
           03  EX-SPOOL-BASE          PIC X(09).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  EX-SPOOL-MACHINE       PIC X(02).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  EX-SPOOL-DATA-PATH     PIC X(6).
           03  FILLER                 PIC X(04)  VALUE "/EO/".
           03  EX-SPOOL-PCODE         PIC X(03)  VALUE "OTH".
           03  EX-SPOOL-DATE          PIC 9(06).
           03  EX-SPOOL-FILE.
               05  FILLER         PIC X(01).
               05  EX-SPOOL-STEP  PIC 9(04).
               05  EX-SPOOL-NAME  PIC X(06).
               05  EX-SPOOL-SUF   PIC X(02).

       01  STEP-COUNT             PIC 9999 VALUE 1.

       01  ERRCD                  PIC X.
      *=================================================================
      *
      *    P R I N T - B U F F E R S
      *
      *=================================================================
       01  HEAD1.
           03  H-DATE             PIC X(08)   VALUE " ".
           03  FILLER             PIC X(02)   VALUE " ".
           03  H-HR               PIC 9(02)   VALUE 0.
           03  FILLER             PIC X(01)   VALUE ":".
           03  H-MIN              PIC 9(02)   VALUE 0.
           03  FILLER             PIC X(16)   VALUE " ".
           03  H-NAME             PIC X(40)   VALUE " ".
           03  FILLER             PIC X(21)   VALUE " ".
           03  FILLER             PIC X(30)   VALUE " ".
           03  FILLER             PIC X(05)   VALUE "PAGE ".
           03  H-PAGE-NO          PIC ZZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER             PIC X(10)   VALUE "USER ID:".
           03  H-USERID           PIC X(10).
           03  FILLER             PIC X(11)   VALUE " ".
           03  FILLER             PIC X(52)   VALUE
           " - BATCH OTHER CHARGES PAYMENTS FOR REFERENCE CODE: ".
           03  H-TITLE-REFCD      PIC X(5).
           03  FILLER             PIC X(2)    VALUE " -".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER             PIC X(31)   VALUE " ".
           03  FILLER             PIC X(22)   VALUE
               "   BRANCH BATCH FILE: ".
           03  H-TITLE-LOCAL      PIC X(23).

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER             PIC X(40)   VALUE
               "          NAME                    AMOUNT".
           03  FILLER             PIC X(40)   VALUE
               "      SSNO       ACCOUNT #              ".
           03  FILLER             PIC X(40)   VALUE
               "DATE        BRNO    MESSAGE             ".
           03  FILLER             PIC X(12)   VALUE
               "            ".

      *=================================================================
      *
      *    D E T A I L - L I N E
      *
      *=================================================================
       01  DETAIL-LINE                   PIC X(132) VALUE SPACES.
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-NAME                    PIC X(25).
           03  FILLER                    PIC X(05).
           03  D-AMOUNT                  PIC Z(06)9.99.
           03  FILLER                    PIC X(03).
           03  D-SSNO                    PIC 999/99/9999.
           03  FILLER                    PIC X(03).
           03  D-DUP                     PIC X(01).
           03  D-ACCTNO                  PIC X(17).
           03  FILLER    REDEFINES D-ACCTNO.
               05  FILLER                PIC Z(04).
               05  D-ACCTNO1             PIC Z(06).
               05  FILLER                PIC X(07).
           03  FILLER                    PIC X(03).
           03  D-DATE                    PIC 99/99/99.
           03  FILLER                    PIC X(04).
           03  D-BRANCH                  PIC Z(06).
           03  FILLER                    PIC X(04).
           03  D-MESS                    PIC X(32).
       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOT-PROMPT              PIC X(15).
           03  D-TOT-NUMBER              PIC Z(10).
           03  FILLER                    PIC X(02).
           03  D-TOT-AMOUNT              PIC Z(10).99.
           03  FILLER                    PIC X(04).
           03  D-TOT-MESSAGE             PIC X(56).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBLP/LPBP_CMD.CPY".
       COPY "LIBLP/LPBPPATH.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  BPOTH-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BPOTH_DEF.CPY".
       COPY "LIBSP/BPOTH_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/BPOTH_SCN.CPY".


      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               BT-FILE PR-FILE BP-FILE OTH-LOG-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GP-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               BT-FILE BP-FILE OTH-LOG-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "BPOTH" TO VDU-FORM-NAME FORM-PROG.

      D    STOP MESS.
           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-BT.
           MOVE SPACES TO BT-REC.
           PERFORM READ-BT-FILE.
           IF IO-FG = 9 OR BT-REC(1:1) = X'1A'
              GO TO END-ROUTINE.

      * CHECK RECORD TYPE:  "D" = DETAIL | "T" = TRAILER
      
           IF BT-TYPE NOT = "D"
              PERFORM DETAIL-LINE-PROCESS
              GO TO NEXT-BT.

      * GET BRANCH FILE FOR EACH NEW BRANCH
      
           IF BT-BRANCH NOT = HOLD-BRANCH
              MOVE BT-BRANCH TO MESS-BR
              PERFORM SEND-LEGEND
              PERFORM GET-BR
              IF IO-FG NOT = 0
                 MOVE "NO BRANCH RECORD FOUND" TO D-MESS
                 PERFORM DETAIL-LINE-PROCESS
                 ADD 1 TO TOTAL-SKIP(1) TOTAL-READ(1)
                          TOTAL-SKIP(2) TOTAL-READ(2)
                 ADD BT-AMT TO TOTAL-AMT-SKIP(1) TOTAL-AMT-READ(1)
                               TOTAL-AMT-SKIP(2) TOTAL-AMT-READ(2)
                 GO TO NEXT-BT
              END-IF
      * IF BRANCH IS NOT ON THIS MACHINE DON'T BOTHER PRINTING ON
      * EXCEPTION, IT WILL SHOW UP ON THE OTHER MACHINES REPORT
              IF BR-MACHINE NOT = EXT-FILPATH-MACHINE
                 ADD 1 TO TOTAL-OM(1) TOTAL-READ(1)
                          TOTAL-OM(2) TOTAL-READ(2)
                 ADD BT-AMT TO TOTAL-AMT-OM(1) TOTAL-AMT-READ(1)
                               TOTAL-AMT-OM(2) TOTAL-AMT-READ(2)
                 GO TO NEXT-BT
              END-IF
      * SETUP BRANCH FILE PATHS
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA

              MOVE "B"   TO LN-PATH-OVERRIDE
                            BW-PATH-OVERRIDE.


           IF FIRST-TIME = "Y"
              MOVE BT-BRANCH TO HOLD-BRANCH
              PERFORM CREATE-BP-FILE.

           IF BT-BRANCH NOT = HOLD-BRANCH
              PERFORM WRITE-BP-TRAILER
              PERFORM CLOSE-BP1-FILE
              PERFORM TOTAL-ROUTINE
              MOVE BT-BRANCH TO HOLD-BRANCH
              PERFORM CREATE-BP-FILE.

           ADD 1 TO TOTAL-RECORDS(1) TOTAL-RECORDS(2)
                    TOTAL-READ(1)    TOTAL-READ(2).
           ADD BT-AMT TO TOTAL-AMOUNT(1) TOTAL-AMOUNT(2)
                         TOTAL-AMT-READ(1) TOTAL-AMT-READ(2).

      * GET ACCOUNT INFO NOT SUPPLIED FROM MELLON BANK (NAME & SSNO)
      
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE BT-ACCTNO    TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO LN-SSNO(1).

           MOVE LN-SSNO(1)   TO BW-SSNO.
           PERFORM PROCESS-BORROWER.

           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.

      * VALID ACCOUNT NUMBER
      
           IF BT-ACCTNO NOT = 0
              PERFORM WRITE-BP-RECORDS
           ELSE
              PERFORM WRITE-ZERO-RECORD
              MOVE "ZERO ACCOUNT #    " TO D-MESS
              PERFORM DETAIL-LINE-PROCESS
              ADD 1 TO TOTAL-ZERO(1) TOTAL-ZERO(2)
              ADD BT-AMT TO TOTAL-AMT-ZERO(1) TOTAL-AMT-ZERO(2).

      * PRINT CREATED STATUS IN EXCEPTION REPORT
      
           IF DUP-FLAG = "Y"
              MOVE "DUPLICATE ACCOUNT NO" TO D-MESS
           ELSE
              MOVE "CREATED BP" TO D-MESS.
           PERFORM DETAIL-LINE-PROCESS.

           MOVE "N" TO FIRST-TIME.
           GO TO NEXT-BT.

       END-ROUTINE.
           IF TOTAL-RECORDS(1) NOT = 0
              PERFORM WRITE-BP-TRAILER.
           PERFORM CLOSE-BP1-FILE.
           PERFORM CLOSE-BR-FILE.

           MOVE "Y" TO GRAND-TOTAL-FG.
           PERFORM TOTAL-ROUTINE.

      * RENAME BTFILE TO BTFILE{BATCH DATE}{BATCH NO}
      * LONPW1 HAS BEEN CHANGED TO GET THE GLOBAL BATCH DATE/NO HERE
      *        " /USR/DATA/R#/ED/OTFILE  /USR/DATA/R#/ED/OTFIL"
     
           MOVE SPACES TO SYSTEM-BUF.
           MOVE BT-PATH-SHORT-NAME TO MESS.
           INSPECT MESS REPLACING FIRST "  "   BY "+ ".
           INSPECT MESS REPLACING FIRST "+   " BY BATCH-DATE.
           INSPECT MESS REPLACING FIRST "  "   BY "+ ".
           INSPECT MESS REPLACING FIRST "+  "  BY BATCH-NO.
           STRING MV-CMD
                  " "
                  BT-PATH
                  " "
                  MESS
                  DELIMITED BY SIZE INTO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF EXT-ROUTE-BUF = "70"
              MOVE "BREAKOUT BATCH &&& COMPLETED ###### RECS"
                                         TO OTH-LOG-MSG
              INSPECT OTH-LOG-MSG REPLACING FIRST "&&&" BY
                                                   EXT-EOBUF-PCODE
              INSPECT OTH-LOG-MSG REPLACING FIRST "######"
                                               BY SAVE-TOTAL-COUNT
              PERFORM CREATE-LOG.

           GO TO END-PROGRAM.

      ******************************************************************
       CREATE-BP-FILE SECTION.
      ******************************************************************

      * /USR/DATA/R#/ED/B####MMDD???REFCD    LOCAL-PATH

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B" TO BP-PATH-OVERRIDE.

           MOVE BR-NO         TO BP-ALL-BRANCH.
           MOVE BATCH-DATE    TO BP-ALL-DATE.
           MOVE BATCH-NO      TO BP-ALL-BATCH-NO.
           MOVE CD-CODE       TO BP-ALL-REFCD.
           PERFORM OPEN-BP1-FILE-OUTPUT.
           PERFORM CLOSE-BP1-FILE.
           PERFORM OPEN-BP1-FILE.

      ******************************************************************
       WRITE-BP-RECORDS SECTION.
      ******************************************************************
           MOVE SPACES       TO BP-REC.
           MOVE "N"          TO DUP-FLAG.
           MOVE LN-ACCTNO    TO BP-LNNO.
           MOVE 1            TO BP-SEQNO.
           MOVE LN-SSNO(1)   TO BP-SSNO
           MOVE "PA"         TO BP-TRCD.
           MOVE 4            TO BP-SIGN.
           MOVE BT-AMT       TO BP-TRAMT.
           MOVE BT-FROZEN-OVERRIDE TO BP-FROZEN-OVERRIDE.
           IF (BT-PAYDATE = 202020 OR 020202 OR 0)
              MOVE TRANS-DATE TO BP-PAYDATE
           ELSE
              MOVE BT-PAYDATE TO DATE-MMDDYY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO BP-PAYDATE.
           PERFORM WRITE-THE-BP-RECORD.

      ******************************************************************
       WRITE-THE-BP-RECORD SECTION.
      ******************************************************************
           MOVE BP-REC TO HOLD-BP-REC.
           MOVE 1 TO HOLD-BP-SEQNO.
       TRY-TO-WRITE-BP.
           MOVE HOLD-BP-REC TO BP-REC.
           MOVE HOLD-BP-SEQNO TO BP-SEQNO.
           PERFORM WRITE-BP1-FILE.
           IF IO-FG NOT = 0
              IF E-CODE NOT = "22"
                 GO TO IO-ERROR
              ELSE
                 IF HOLD-BP-SEQNO < 99
                    ADD 1 TO HOLD-BP-SEQNO
                    GO TO TRY-TO-WRITE-BP
                 ELSE
                    GO TO IO-ERROR.
           IF BP-SEQNO > 1
              MOVE "Y" TO DUP-FLAG
              ADD 1 TO TOTAL-DUPS(1) TOTAL-DUPS(2)
              ADD BT-AMT           TO TOTAL-AMT-DUP(1)
                                   TOTAL-AMT-DUP(2).
      ******************************************************************
       WRITE-BP-TRAILER SECTION.
      ******************************************************************
           MOVE SPACES           TO BP-REC.
           MOVE 0                TO BP-LNNO.
           MOVE 1                TO BP-SEQNO.
           MOVE "M"              TO BP-TYPE.
           MOVE TOTAL-RECORDS(1) TO BP-TOTAL-COUNT.
           MOVE TOTAL-AMOUNT(1)  TO BP-TOTAL-AMOUNT.
           PERFORM WRITE-BP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

      ******************************************************************
       WRITE-ZERO-RECORD SECTION.
      ******************************************************************
           MOVE 99999999 TO BP-LNNO.
           MOVE 1        TO BP-SEQNO.
           PERFORM READ-BP1-FILE.
           IF IO-FG = 0
              ADD BT-AMT TO BP-TRAMT
              PERFORM REWRITE-BP1-FILE
           ELSE
              MOVE SPACES   TO BP-REC
              MOVE 99999999 TO BP-LNNO
              MOVE 1        TO BP-SSNO
              MOVE BT-AMT  TO BP-TRAMT
              PERFORM WRITE-BP1-FILE.

      ******************************************************************
       TOTAL-ROUTINE SECTION.
      ******************************************************************
           MOVE "TOTAL BP'S CREATED  :" TO DETAIL-LINE.
           SUBTRACT TOTAL-DUPS(1) TOTAL-ZERO(1)
               FROM TOTAL-RECORDS(1) GIVING D-ACCTNO1.
           SUBTRACT TOTAL-AMT-DUP(1) TOTAL-AMT-ZERO(1)
               FROM TOTAL-AMOUNT(1) GIVING D-AMOUNT.
           MOVE 3                       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL DUP ACCTNO'S  :" TO DETAIL-LINE.
           MOVE TOTAL-DUPS(1)           TO D-ACCTNO1.
           MOVE TOTAL-AMT-DUP(1)        TO D-AMOUNT.
           MOVE 1                       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL ZERO ACCTNO'S :" TO DETAIL-LINE.
           MOVE TOTAL-ZERO(1)           TO D-ACCTNO1.
           MOVE TOTAL-AMT-ZERO(1)       TO D-AMOUNT.
           MOVE 1                       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL ACCTS SKIPPED :" TO DETAIL-LINE.
           MOVE TOTAL-SKIP(1)           TO D-ACCTNO1.
           MOVE TOTAL-AMT-SKIP(1)       TO D-AMOUNT.
           MOVE 1                       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL ON OTHER MACH :" TO DETAIL-LINE.
           MOVE TOTAL-OM(1)             TO D-ACCTNO1.
           MOVE TOTAL-AMT-OM(1)         TO D-AMOUNT.
           MOVE 1                       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS READ :" TO DETAIL-LINE.
           MOVE TOTAL-READ(1)          TO D-ACCTNO1.
           MOVE TOTAL-AMT-READ(1)      TO D-AMOUNT.
           MOVE 2                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF GRAND-TOTAL-FG = "Y"
              MOVE "GRAND TOTALS" TO DETAIL-LINE
              MOVE 3              TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "TOTAL BP'S CREATED  :" TO DETAIL-LINE
              SUBTRACT TOTAL-DUPS(2) TOTAL-ZERO(2)
                  FROM TOTAL-RECORDS(2) GIVING D-ACCTNO1
              SUBTRACT TOTAL-AMT-DUP(2) TOTAL-AMT-ZERO(2)
                  FROM TOTAL-AMOUNT(2) GIVING D-AMOUNT
              MOVE 2                       TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "TOTAL DUP ACCTNO'S  :" TO DETAIL-LINE
              MOVE TOTAL-DUPS(2)           TO D-ACCTNO1
              MOVE TOTAL-AMT-DUP(2)        TO D-AMOUNT
              MOVE 1                       TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "TOTAL ZERO ACCTNO'S :" TO DETAIL-LINE
              MOVE TOTAL-ZERO(2)           TO D-ACCTNO1
              MOVE TOTAL-AMT-ZERO(2)       TO D-AMOUNT
              MOVE 1                       TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "TOTAL ACCTS SKIPPED :" TO DETAIL-LINE
              MOVE TOTAL-SKIP(2)           TO D-ACCTNO1
              MOVE TOTAL-AMT-SKIP(2)       TO D-AMOUNT
              MOVE 1                       TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "TOTAL ON OTHER MACH :" TO DETAIL-LINE
              MOVE TOTAL-OM(2)             TO D-ACCTNO1
              MOVE TOTAL-AMT-OM(2)         TO D-AMOUNT
              MOVE 1                       TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "TOTAL RECORDS READ :" TO DETAIL-LINE
              MOVE TOTAL-READ(2)          TO D-ACCTNO1
              MOVE TOTAL-AMT-READ(2)      TO D-AMOUNT
              MOVE 2                      TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE 0  TO TOTAL-RECORDS(1) TOTAL-AMOUNT(1)
                      TOTAL-DUPS(1)    TOTAL-AMT-DUP(1)
                      TOTAL-ZERO(1)    TOTAL-AMT-ZERO(1)
                      TOTAL-SKIP(1)    TOTAL-AMT-SKIP(1)
                      TOTAL-OM(1)      TOTAL-AMT-OM(1)
                      TOTAL-READ(1)    TOTAL-AMT-READ(1).
           MOVE 99 TO LN-COUNT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
      ******************************************************************
           MOVE 0 TO Q-POS.
           MOVE 0 TO C-POS.
           MOVE 0 TO P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.

           MOVE EXT-CONAME-USER-ID  TO H-USERID.

      * CREATE SPOOL DIRECTORY AND OPEN SPOOL FILE IN 'EO'

           IF (EXT-ROUTE-BUF = "00" OR "70")
              PERFORM CREATE-SPOOL-DIR
              IF ERRCD NOT = " "
                 GO TO ENDIT
              END-IF
              MOVE EX-SPOOL TO PRU-FILE.

           MOVE TRANS-MM TO BATCH-MM.
           MOVE TRANS-DD TO BATCH-DD.

           IF EXT-ROUTE-BUF NOT = "70"
              GO TO FIX-NEXT-BATCH.

           MOVE PRU-FILE TO OTH-LOG-SPOOL-DIR.
           MOVE "BEGIN BREAKOUT FOR BATCH &&&" TO OTH-LOG-MSG.
           INSPECT OTH-LOG-MSG REPLACING FIRST "&&&" BY
                                              EXT-EOBUF-PCODE.
           PERFORM CREATE-LOG.

           PERFORM OPEN-CD1-FILE.
           MOVE "BR"   TO CD-TYPE.
           MOVE REFCD  TO CD-CODE
                          H-TITLE-REFCD.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF (IO-BAD ) OR (CD-BR-FILE-TYPE = "LC" OR "C" )
              MOVE "REF CODE &&&&& NOT ON FILE, ABORTED" TO OTH-LOG-MSG
              INSPECT OTH-LOG-MSG REPLACING FIRST "&&&&&" BY REFCD
              PERFORM CREATE-LOG
              GO TO ENDIT.

      * SETUP BT-PATH:
           MOVE EXT-FILPATH-BASE      TO BT-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE   TO BT-PATH-MACHINE.
           MOVE CD-BR-FILE-NAME       TO BT-PATH-BATCH-NAME.

      * CHECK THAT THE GLOBAL BATCH FILE IS THERE

           MOVE BT-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "GLOBAL BATCH &&&&&&&&&&&&&&& NOT FOUND!"
                                                    TO OTH-LOG-MSG
              INSPECT OTH-LOG-MSG REPLACING FIRST
                       "&&&&&&&&&&&&&&&&&&&&&&" BY BT-PATH
              PERFORM CREATE-LOG
              GO TO ENDIT.

      * FIX NEXT BATCH NUMBER FROM GPFILE IF INVALID

       FIX-NEXT-BATCH.
           PERFORM OPEN-GP-FILE.
           MOVE 1 TO GP-KEY.
           PERFORM READ-GP-FILE.
           IF NOT GP-OTREF-NEXT-BATCH-VALID
              MOVE GP-OTREF-NEXT-BATCH-START TO GP-OTREF-NEXT-BATCH.
           PERFORM REWRITE-GP-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "SNNN030BATCHID." TO VDU
              MOVE GP-OTREF-NEXT-BATCH TO VDUNUM0
                                          BATCH-NO
              PERFORM SCREENIO
           ELSE
              MOVE GP-OTREF-NEXT-BATCH TO BATCH-NO.

      * SET UP LOCAL BRANCH BATCH FILE PATH
      *  /USR/DATA/R#/XX????/ED/B####MMDD???REFCD

           MOVE EXT-FILPATH-BASE    TO LOCAL-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO LOCAL-PATH-MACHINE.
           MOVE EXT-FILPATH-DATA    TO LOCAL-PATH-DATA.
           MOVE "####"              TO LOCAL-PATH-BRANCH.
           MOVE BATCH-DATE          TO LOCAL-PATH-DATE.
           MOVE BATCH-NO            TO LOCAL-PATH-BATCH.
           MOVE CD-CODE             TO LOCAL-PATH-REFCD.
           MOVE "S  A200LOCALFILE."  TO VDU.
           MOVE LOCAL-PATH-DISPLAY   TO VDUBUF
                                        H-TITLE-LOCAL.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              PERFORM SCREENIO.

           MOVE "S  A050REFCDALP."   TO VDU.
           MOVE CD-CODE              TO VDUBUF.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              PERFORM SCREENIO.

           MOVE "S  A050BFREFCD."    TO VDU.
           MOVE CD-CODE              TO VDUBUF.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              PERFORM SCREENIO.

      * CHECK THAT NO B####MMDD???REFCD FILES EXIST
      * /$REL/SH/BPLIST_REF.SH
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO SKIP-FILES-EXIST-CHECK.

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO LIST-REF-REL LIST-REF-REL2.
           CALL "C$TOLOWER" USING LIST-REF-EC, VALUE 3.
           CALL "C$TOLOWER" USING LIST-REF-SHELL, VALUE 14.
           MOVE LOCAL-PATH-DATE   TO LIST-REF-DATE.
           MOVE LOCAL-PATH-REFCD  TO LIST-REF-REFCD.
           MOVE EXT-FILPATH-FIL TO LIST-REF-DATA.
           MOVE LIST-REF-CMD      TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           MOVE LIST-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              IF EXT-ROUTE-BUF = "00"
                 MOVE "BRANCH BATCH FILES EXIST FOR TODAY !!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENDIT
              ELSE
                 MOVE "BRANCH BATCH FILES EXIST FOR TODAY!"
                                                        TO OTH-LOG-MSG
                 PERFORM CREATE-LOG
                 GO TO ENDIT.

       SKIP-FILES-EXIST-CHECK.

           MOVE " " TO ERRCD.

           MOVE "BATCH BRANCH FILE CREATE IN PROGRESS....." TO MESS-BUF.

           PERFORM OPEN-PR-FILE.
           IF IO-FG NOT = 0
              GO TO ENDIT.


           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.

           PERFORM OPEN-BT-FILE.

      * REWRITE GP FILE HERE, IF WE DO NOT ACCEPT THEN WE DO NOT WANT
      * TO INCREMENT THE NEXT BATCH NUMBER
           IF EXT-ROUTE-BUF = "00" OR "70"
              PERFORM OPEN-GP-FILE
              MOVE 1 TO GP-KEY
              PERFORM READ-GP-FILE
              ADD 1 TO GP-OTREF-NEXT-BATCH
              IF NOT GP-OTREF-NEXT-BATCH-VALID
                 MOVE GP-OTREF-NEXT-BATCH-START TO GP-OTREF-NEXT-BATCH
              END-IF
              PERFORM REWRITE-GP-FILE
              PERFORM CLOSE-GP-FILE.


           PERFORM OPEN-BR-FILE.
           MOVE 10000 TO HOLD-BRANCH.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO TRANS-DATE.
       EXIT-EO-EXEC-AUDIT.
           EXIT.


      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-REFCD.
           MOVE "E  A050REFCD." TO VDU.
           PERFORM SCREENIO.
           IF F6-KEY
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-REFCD
              ELSE
                 MOVE "00" TO VDUSTATUS.
           IF F7-KEY
              GO TO EXIT-PARM.
           IF NOT ENTER-KEY
              GO TO ENTER-REFCD.

           PERFORM OPEN-CD1-FILE.
           MOVE "BR"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE
                          REFCD
                          H-TITLE-REFCD.
           PERFORM READ-CD1-FILE.
           IF (IO-BAD) OR (CD-BR-FILE-TYPE = "LC" OR "C")
              MOVE "REFERENCE CODE NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-REFCD.

      * SETUP BT-PATH:
           MOVE EXT-FILPATH-BASE     TO BT-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE  TO BT-PATH-MACHINE.
           MOVE CD-BR-FILE-NAME      TO BT-PATH-BATCH-NAME.

           MOVE "S  A170GLOBALFILE." TO VDU.
           MOVE BT-PATH-DISPLAY      TO VDUBUF.
           PERFORM SCREENIO.

      * CHECK THAT THE GLOBAL BATCH FILE IS THERE
           IF EXT-ROUTE-BUF = "00"
              MOVE BT-PATH TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF STAT NOT = "00"
                 MOVE "GLOBAL BATCH FILE DOES NOT EXIST !!!" TO MESS
                 PERFORM SEND-MESS
                 MOVE "1>" TO VDUSTATUS
                 MOVE "X" TO ERRCD
                 GO TO EXIT-PARM.


           MOVE " " TO ERRCD.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           MOVE "S  A050REFCD." TO VDU.
           MOVE REFCD           TO VDUBUF.
           PERFORM SCREENIO.


      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************
           EVALUATE BT-TYPE
              WHEN   "D" PERFORM DETAIL-LINE-DETAIL
              WHEN   "T" PERFORM DETAIL-LINE-TRAILER
              WHEN OTHER PERFORM DETAIL-LINE-INVALID
           END-EVALUATE.

      ******************************************************************
       DETAIL-LINE-DETAIL SECTION.
      ******************************************************************
           MOVE BT-BRANCH    TO D-BRANCH.
           MOVE BT-ACCTNO    TO D-ACCTNO.

           IF DUP-FLAG = "Y" OR BT-ACCTNO = 0
              MOVE "*"       TO D-DUP.
           MOVE LN-SSNO(1)   TO D-SSNO.
           INSPECT D-SSNO REPLACING ALL "/" BY "-".
           MOVE BT-AMT       TO D-AMOUNT.
           IF (BT-PAYDATE = 202020 OR 020202 OR 0)
              MOVE TRANS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-DATE
           ELSE
              MOVE BT-PAYDATE TO D-DATE.

           MOVE NAME-WORKER  TO D-NAME.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       DETAIL-LINE-TRAILER SECTION.
      ******************************************************************
           MOVE "TOTALS        :" TO D-TOT-PROMPT.
           MOVE BT-REC-COUNT      TO D-TOT-NUMBER.
           MOVE BT-TOTAL-AMOUNT   TO D-TOT-AMOUNT.
           MOVE "TOTALS TRAILER RECORD" TO D-TOT-MESSAGE.

           IF (BT-REC-COUNT NOT = TOTAL-READ(2)) AND
              (BT-TOTAL-AMOUNT NOT = TOTAL-AMT-READ(2))
              MOVE "TOTALS DO NOT MATCH DETAIL" TO D-MESS.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       DETAIL-LINE-INVALID SECTION.
      ******************************************************************
           STRING WS_QUOTE BT-REC WS_QUOTE
                  DELIMITED BY SIZE INTO DETAIL-LINE.
           STRING "UNKNOWN RECORD TYPE " WS_QUOTE BT-TYPE WS_QUOTE
                  DELIMITED BY SIZE INTO D-MESS.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD3       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD4       TO PR-REC.
           MOVE 2           TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4           TO LN-COUNT.
           MOVE 2           TO LN-FEED.

      ******************************************************************
       CREATE-SPOOL-DIR SECTION.
      ******************************************************************

      * STORE SPOOL FILE NAME AS /XXX/XXXX/R$/EO/....
      * WE NEED A FULL PATH NAME HERE ELSE DIR WILL CHANGE WHEN WE
      * CHANGE DIRECTORIES TO ANOTHER BRANCH.

           MOVE " " TO ERRCD.

           MOVE EXT-FILPATH-BASE    TO EX-SPOOL-BASE.
           MOVE EXT-FILPATH-MACHINE TO EX-SPOOL-MACHINE.
           MOVE EXT-FILPATH-DATA    TO EX-SPOOL-DATA-PATH.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO EX-SPOOL-DATE.
           MOVE " " TO EX-SPOOL-FILE.
           MOVE EX-SPOOL TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING DIR-ACCESS-BUF GIVING STAT-99
              IF STAT NOT = "00"
                 MOVE "E" TO ERRCD
                 IF EXT-ROUTE-BUF = "00"
                    MOVE "CANNOT CREATE SPOOL DIRECTORY" TO MESS
                    PERFORM SEND-MESS
                    MOVE DIR-ACCESS-BUF TO MESS
                    PERFORM SEND-MESS
                    GO TO EXIT-CREATE-SPOOL-DIR
                 ELSE
                   MOVE "CANNOT CREATE SPOOL DIRECTORY" TO OTH-LOG-MSG
                   PERFORM CREATE-LOG
                   GO TO EXIT-CREATE-SPOOL-DIR.

           MOVE "/"     TO EX-SPOOL-FILE.
           MOVE "BPOTH" TO EX-SPOOL-NAME.
           MOVE "  "    TO EX-SPOOL-SUF.

      *****************************************************************
      * THEY ARE ALLOWED TO RUN THIS  MORE THAN ONCE ON THE
      * SAME TRANSACTION DATE, SO CHECK TO SEE IF REPORT EXISTS, IF
      * SO, INCREMENT THE STEP # SO WE DON'T WIPE OUT PRIOR REPORTS
      *****************************************************************
           MOVE 1 TO STEP-COUNT.
       TRY-NEXT-STEP-NO.
           MOVE STEP-COUNT TO EX-SPOOL-STEP.
           MOVE EX-SPOOL TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              IF STEP-COUNT < 999
                 ADD 1 TO STEP-COUNT
                 GO TO TRY-NEXT-STEP-NO.

       EXIT-CREATE-SPOOL-DIR.
           EXIT.

      ******************************************************************
       PROCESS-BORROWER SECTION.
      ******************************************************************
           PERFORM READ-BW1-FILE.
           IF IO-BAD
               MOVE "*** BORROWER NOT ON FILE ***" TO NAME-WORKER
               GO TO PROCESS-BORROWER-EXIT.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
                   BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".

       PROCESS-BORROWER-EXIT.
           EXIT.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************

           MOVE "BR"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE 0 TO IO-FG.

           MOVE "S  A050REFCD." TO VDU.
           MOVE CDSCAN-CODE     TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.

       EXIT-CODE-SCAN.
           EXIT.

      ******************************************
       CREATE-LOG SECTION.
      ******************************************
           MOVE OTH-LOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              PERFORM OPEN-OTH-LOG-FILE-EXTEND
           ELSE
              PERFORM OPEN-OTH-LOG-FILE-OUTPUT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT          TO OTH-LOG-TIME.
           MOVE EXT-CONAME-USER-ID TO OTH-LOG-USERID.
           MOVE "OTH: "            TO OTH-LOG-PROMPT1.
           MOVE REFCD              TO OTH-LOG-REFCD.
           IF BATCH-NO NOT = 0
              MOVE "BAT: "         TO OTH-LOG-PROMPT2.
           MOVE BATCH-NO           TO OTH-LOG-BATCH.
           PERFORM WRITE-OTH-LOG-FILE.
           PERFORM CLOSE-OTH-LOG-FILE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO BPOTH-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BPOTH-SYSDATE.
           DISPLAY BPOTH-SCREEN UPON BPOTH-WINDOW-HANDLE.
           MOVE BPOTH-FORM-FIELDS TO WR-BUF.
           MOVE BPOTH-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE BPOTH-HELP--FILE TO HELP-LINK-FILE.
           MOVE BPOTH-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/BPOTH_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
      *-----------------------------------------------------------------
       GET-BR.
           MOVE BT-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           IF EXT-ROUTE-BUF = "00"
              MOVE MESS-BUF TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBP1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBGB/GBGPIN.CPY".


       OPEN-BT-FILE.
           PERFORM OPEN-IT.
           MOVE BT-PATH TO E-FILE.
           OPEN INPUT BT-FILE.
           IF IO-FG = 8
              GO TO OPEN-BT-FILE.
           IF IO-FG = 7
              CLOSE BT-FILE
              GO TO OPEN-BT-FILE.
       READ-BT-FILE.
           PERFORM READ-IT.
           MOVE BT-PATH TO E-FILE.
           READ BT-FILE.
           IF IO-FG = 8
              GO TO READ-BT-FILE.
       CLOSE-BT-FILE.
           PERFORM CLOSE-IT.
           CLOSE BT-FILE.
       OPEN-OTH-LOG-FILE-EXTEND.
           OPEN EXTEND OTH-LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTH-LOG-FILE-EXTEND.
           IF IO-FG = 7
              CLOSE OTH-LOG-FILE
              GO TO OPEN-OTH-LOG-FILE-EXTEND.
       OPEN-OTH-LOG-FILE-OUTPUT.
           OPEN OUTPUT OTH-LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTH-LOG-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE OTH-LOG-FILE
              GO TO OPEN-OTH-LOG-FILE-OUTPUT.
       OPEN-OTH-LOG-FILE.
           OPEN INPUT OTH-LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTH-LOG-FILE.
           IF IO-FG = 7
              CLOSE OTH-LOG-FILE
              GO TO OPEN-OTH-LOG-FILE.
       WRITE-OTH-LOG-FILE.
           WRITE OTH-LOG-REC.
       CLOSE-OTH-LOG-FILE.
           CLOSE OTH-LOG-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH   TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BPOTH-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
