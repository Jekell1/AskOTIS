       IDENTIFICATION DIVISION.
       PROGRAM-ID.        OTHUPD.
       AUTHOR.               BAH.
       DATE-WRITTEN.  2022/05/20.
      ******************************************************************
      *
      *                   ( SP/OTHUPD ) - COPIED FORM REFUPD
      *
      *     DESCRIPTION: BATCH PAYMENT UPDATE DRIVER TO BE RUN FROM LPCRON
      *                  ENTER REFCD, LOWEST BATCH FILE WILL BE SELECTED.
      *                  SO THIS KIND OF REPLACES LONPW1, SET BATCH TO
      *                  PROCESS, THEN CALLS LONPWA WHICH WILL CALL
      *                  LONPF7 (OT) & LONPF2
      *
      *     BATCH: FIRST CALL SP/BPOTH TO BREAK OUT THE BRANCH FILES THEN
      *                  CALL SP/OTHUPD TO RUN THE UPDATE/POST
      *
      *
      * REV.
      * BAH 20220520 NEW SO BPOTH CAN RUN IN LPCRON BATCH #1550B
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKS.CPY".
       COPY "LIBGB/GBFSWK2I.CPY".
           SELECT OTH-LOG-FILE ASSIGN OTH-LOG-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.
      *-----------------------------------------------------------------
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      **************************************************
      *    TEMP WORK RECORD FOR BATCH FILES
      *                                  BRNOMMDDBID
      *  WK-REC: /USR/XXXX/R#/XX####/ED/B###########
      *          FILLER(23).............
      **************************************************
       FD  WK-FILE
           LABEL RECORD IS STANDARD.
       01  WK-REC.
           03  FILLER             PIC X(23).
           03  WK-REFCD.
               05  WK-BP          PIC X.
               05  WK-BRANCH      PIC 9(4).
           03  WK-DATE            PIC 9(4).
           03  WK-BATCH-NO        PIC 999.
           03  WK-BATCH-REFCD     PIC X(5).

      ******************************************************
      * WORK REC BY BATCH#, BY REFCD, WILL USE LOWEST BATCH#
      ******************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BATCH-NO   PIC 999.
               05  WK2-REFCD      PIC X(5).

      **************************************************
      *  REFCODE BATCH LOG FILE                      *
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/OTHUPD S35 05/20/22-12:03:52 >".

      ******************************************************************
      *
      *     P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       01  WK-IFN                  PIC XX  VALUE "WK".
       01  WK2-IFN                 PIC XXX VALUE "WK2".
       01  PR-IFN                  PIC X(02) VALUE "PR".
       01  WK2PATH                 PIC X(35).

       01  OTH-LOG-PATH.
           03  OTH-LOG-SPOOL-DIR      PIC X(32).
           03  OTH-LOG-PATHNAME.
               05  OTH-LOG-NAME       PIC X(10) VALUE "/0000LOG".


      ******************************************************************
      *     S C R E E N   F I E L D S
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  REFCD           PIC X(5).

       01  VERSION-CONTROL     PIC X VALUE "A".
       01  LN-FEED             PIC 9(02)  VALUE 0.
 
      *=================================================================
      *
      *    M I S C - W O R K E R S
      *
      *=================================================================
       01  WSS-TRANS-DATE              PIC 9(08)     VALUE ZEROES.
       01  FILLER            REDEFINES WSS-TRANS-DATE.
           03  WSS-TRANS-DATE-CCYY     PIC 9(04).
           03  WSS-TRANS-DATE-MMDD     PIC 9(04).

      *-----------------------------------------------------------------
      * SPOOL DIRECTORY/FILENAME FOR EXCEPTION REPORT
      *-----------------------------------------------------------------
       01  EX-SPOOL.
           03  EX-SPOOL-BASE      PIC X(9).
           03  FILLER             PIC X(1) VALUE "/".
           03  EX-SPOOL-MACHINE   PIC X(2).
           03  FILLER             PIC X    VALUE "/".
           03  EX-SPOOL-DATA-PATH PIC X(6).
           03  FILLER             PIC X(04)  VALUE "/EO/".
           03  EX-SPOOL-PCODE     PIC X(03)  VALUE "OTH".
           03  EX-SPOOL-DATE      PIC 9(06).
           03  EX-SPOOL-FILE.
               05  FILLER         PIC X(01).
               05  EX-SPOOL-STEP  PIC 9(04).
               05  EX-SPOOL-NAME  PIC X(06).
               05  EX-SPOOL-SUF   PIC X(02).

       01  STEP-COUNT             PIC 9999 VALUE 1.

      *-----------------------------------------------------------------
      * FIL AS IT WAS WHEN USER ENTERED PROGRAM
      *-----------------------------------------------------------------
       01  ORIG-FIL.
           03  ORIG-XXX        PIC X(3).
           03  ORIG-SLASH1     PIC X.
           03  ORIG-XXXX       PIC X(4).
           03  ORIG-SLASH2     PIC X.
           03  ORIG-MACHINE    PIC X(2).
           03  ORIG-SLASH3     PIC X.
           03  ORIG-DATA-PATH  PIC X(6).
 
       01  UNSORTED-FILE       PIC X(20) VALUE " ".
       01  UNSORTED-LOCAL-FILE PIC X(20) VALUE " ".
       01  SORTED-FILE         PIC X(20) VALUE " ".

       01  SED-BUF.
           03  FILLER        PIC X(6)  VALUE X'736564202773'.
           03  FILLER        PIC X(01) VALUE "/".
           03  SED-DATE1     PIC 9(4)  VALUE 0000.
           03  FILLER        PIC X(01) VALUE "/".
           03  SED-DATE2     PIC 9(04) VALUE 0000.
           03  FILLER REDEFINES SED-DATE2.
               05  FILLER    PIC X(03).
               05  SED-DELIM PIC X(01).
           03  FILLER        PIC X(02)  VALUE "/'".

       01  SED-BUF2.
           03  FILLER        PIC X(6)  VALUE X'736564202773'.
           03  FILLER        PIC X(01) VALUE "/".
           03  SED-FILE-NAME PIC X(5)  VALUE " ".
           03  FILLER        PIC X(01) VALUE "/".
           03  SED-REFCD     PIC X(5)  VALUE " ".
           03  FILLER        PIC X(02)  VALUE "/'".

      *-----------------------------------------------------------------
      *    IN A15, THE FILE PATHS WERE RELATIVE (../../XX####/GB).
      *    WHEN LOADING GLOBAL FILES AS LOCAL FILES INTO THE
      *    UNSORTED-FILE TO SORTED-FILE (VIA CAT WITH SED), THE LEADING
      *    "../" IS REMOVED (../../ED/XXXXX BECOMES ../ED/XXXXX).
      *
      *    IN A30, THE FILE PATHS ARE FULL (/USR/DATA/R#/XX####/GB).
      *    WHEN LOADING GLOBAL FILES AS LOCAL FILES INTO THE
      *    UNSORTED-FILE TO SORTED-FILE (VIA CAT WITH SED), THE
      *    DATA PATH (STATEBRNO; XX####) NEEDS TO BE INSERTED. 
      *    "/USR/DATA/R1/ED/XXXX" BECOMES "/USR/DATA/R1/XX####/ED/XXXX".
      *    EXAMPLE (NEED \ BEFORE / FOR LINUX TO KNOW TO USE /):
      *    SED 'S/R#\/ED/R#\/XX####\/ED/'                KEC 2017-1103
      *-----------------------------------------------------------------
       01  SED-LOCAL-BUF.
           03  FILLER               PIC X(06)   VALUE X'736564202773'.
           03  FILLER               PIC X(01)   VALUE "/".
           03  SED-SOURCE-MACHINE   PIC X(02)   VALUE " ".
           03  FILLER               PIC X(05)   VALUE "\/ED/".
           03  SED-TARGET-MACHINE   PIC X(02)   VALUE " ".
           03  FILLER               PIC X(02)   VALUE "\/".
           03  SED-TARGET-STBRNO    PIC X(06)   VALUE " ".
           03  FILLER               PIC X(06)   VALUE "\/ED/'".

      *-----------------------------------------------------------------
      *    [CAT]
      *    [SORT -U +1N - T:]
       78  SORT-CMD          VALUE X'736F7274202D75202B316E202D743A'.
      *    [SORT -UK1 -T:]
       78  LINUX-SORT-CMD    VALUE X'736F7274202D756B31202D743A'.

       01  OPSYS-BUF         PIC X(4) VALUE SPACES.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPBP_CMD.CPY".
       COPY "LIBLP/LPBPPATH.CPY".
       COPY "LIBLP/LPPFBUF.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
           03  LS-BUF         REDEFINES SYSTEM-BUF.
               05  FILLER      PIC X(3).
               05  LS-DIR      PIC X(50).
               05  FILLER      PIC X.
               05  LS-DIRECT   PIC X.
               05  LS-DEST     PIC X(50).
               05  LS-DEV-NULL PIC X(45).
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/OTHUPD_DEF.CPY".
       COPY "LIBSP/OTHUPD_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/OTHUPD_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE OTH-LOG-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               WK-FILE WK2-FILE OTH-LOG-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "OTHUPD"      TO VDU-FORM-NAME.   

           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE TRANS-DATE                 TO WSS-TRANS-DATE.

           PERFORM GET-BATCHES-FOR-TODAY.

           PERFORM FIND-LOWEST-BATCH.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "LP/LONPWA" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                 FORM-PATHNAME
                                 PROG-BUF
                                 UPDATE-BUF.
           CANCEL FORM-PROGX.

       END-ROUTINE.
           IF EXT-ROUTE-BUF = "70"
              MOVE "OTHER PAYMENT POST BATCH &&& COMPLETED."
                 TO OTH-LOG-MSG
              INSPECT OTH-LOG-MSG REPLACING FIRST "&&&"
                                               BY EXT-EOBUF-PCODE
              PERFORM CREATE-LOG.

           GO TO END-PROGRAM.

      *****************************************************
       GET-BATCHES-FOR-TODAY SECTION.
      *****************************************************

      * GET LIST OF BATCH FILES
      * 
      * CREATE TEMP FILE TO HOLD RESULT OF "LS" COMMAND
      * CREATE FILE ON DISK SO THAT MKTEMP WILL GENERATE UNIQUE
      * NAMES FOR EACH FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO UNSORTED-FILE.
           MOVE " "    TO SYSTEM-BUF.
           STRING "> " UNSORTED-FILE DEV-NULL 
                  DELIMITED BY SIZE INTO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF
                         GIVING STAT-99.

      * CREATE TEMP FILE TO HOLD RESULT OF "CAT SED LOCAL" COMMAND
      * CREATE FILE ON DISK SO THAT MKTEMP WILL GENERATE UNIQUE
      * NAMES FOR EACH FILE.                          KEC 2017-1103

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO UNSORTED-LOCAL-FILE.
           MOVE " "    TO SYSTEM-BUF.
           STRING "> " UNSORTED-LOCAL-FILE DEV-NULL 
                  DELIMITED BY SIZE INTO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF
                         GIVING STAT-99.

      * CREATE TEMP FILE TO HOLD SORTED RESULT OF "LS" COMMAND
      *  (SANS DUPLICATES)

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO SORTED-FILE.

      * LIST ALL BATCH FILES IN CURRENT BRANCHS LOCAL ../ED DIR

           MOVE LS-CMD      TO SYSTEM-BUF.
           MOVE BRANCH      TO BP-ALL-BRANCH.
           MOVE WSS-TRANS-DATE-MMDD TO BP-ALL-DATE.
           MOVE "*"         TO BP-ALL-BATCH-NOX.
           MOVE BP-ALL-PATH TO LS-DIR.
           MOVE ">"         TO LS-DIRECT.
           MOVE SORTED-FILE TO LS-DEST.
           MOVE DEV-NULL    TO LS-DEV-NULL.
           CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99.

      * LIST ALL BATCH FILES IN CURRENT BRANCHS GLOBAL ../../ED DIR
      * TO TEMP-FILE. USE THE CUT COMMAND TO MAKE FILENAMES LINE UP
      * WITH LS OF LOCAL FILES.
      *   EXAMPLE:
      *    ../../ED/BTFIL0999100 |CUT -C4-80 == ../ED/BTFIL0999100
      
           MOVE " "    TO SYSTEM-BUF.
           MOVE BP-WK-BT-PATH TO BP-WK-GB-FILE-NAME.
           MOVE WSS-TRANS-DATE-MMDD TO BP-WK-GB-DATE.
           MOVE "*" TO BP-WK-GB-BATCH-NOX.
           STRING LS-CMD
                  " -1 "
                  BP-WK-GB-PATH
                  DEV-NULL
                  ">>"
                  SORTED-FILE
                  DELIMITED BY SIZE INTO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99.

      * LIST ALL BATCH FILES IN CURRENT BRANCHS GLOBAL ../../ED DIR
      * TO TEMP-FILE. USE THE CUT COMMAND TO MAKE FILENAMES LINE UP
      * WITH LS OF LOCAL FILES.
      *   EXAMPLE:
      *    ../../ED/LBFIL0999100 |CUT -C4-80 == ../ED/LBFIL0999100
      
           MOVE " "    TO SYSTEM-BUF.
           MOVE BP-WK-LB-PATH TO BP-WK-GB-FILE-NAME.
           MOVE WSS-TRANS-DATE-MMDD TO BP-WK-GB-DATE.
           MOVE "*"           TO BP-WK-GB-BATCH-NOX.
           STRING LS-CMD
                  " -1 "
                  BP-WK-GB-PATH
                  DEV-NULL
                  ">>"
                  SORTED-FILE
                  DELIMITED BY SIZE INTO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99.

      * LIST ALL BATCH FILES IN CURRENT BRANCHS GLOBAL ../../ED DIR
      * TO TEMP-FILE. USE THE CUT COMMAND TO MAKE FILENAMES LINE UP
      * WITH LS OF LOCAL FILES.
      *   EXAMPLE:
      *    ../../ED/CCARD1231700 |CUT -C4-80 == ../ED/CCARD1231700
      
           PERFORM OPEN-CD1-FILE.

           MOVE "BR"   TO CD-TYPE.
           MOVE SPACES TO CD-CODE.
           PERFORM START-CD1-FILE.
       NEXT-CD1.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-GOOD
              IF CD-TYPE = "BR"
                 IF (CD-BR-FILE-NAME = SPACES) OR
                     (CD-BR-FILE-TYPE = "LC" OR "C")
                    GO TO NEXT-CD1
                 ELSE
                    MOVE " "             TO SYSTEM-BUF
                    MOVE CD-BR-FILE-NAME TO BP-WK-GB-FILE-NAME
                    MOVE WSS-TRANS-DATE-MMDD  TO BP-WK-GB-DATE
                    MOVE "*"             TO BP-WK-GB-BATCH-NOX
                    STRING LS-CMD
                           " -1 "
                           BP-WK-GB-PATH
                           DEV-NULL
                           ">"
                           UNSORTED-FILE
                           DELIMITED BY SIZE INTO SYSTEM-BUF
                    CALL "SYSTEM" USING SYSTEM-BUF
                                  GIVING STAT-99
      * SED                             
                    MOVE EXT-FILPATH-MACHINE TO SED-SOURCE-MACHINE
                                                SED-TARGET-MACHINE
                    MOVE EXT-FILPATH-DATA    TO SED-TARGET-STBRNO
                    MOVE CD-BR-FILE-NAME     TO SED-FILE-NAME
                    MOVE CD-CODE             TO SED-REFCD
                    MOVE " " TO SYSTEM-BUF
                    STRING CAT-CMD " " UNSORTED-FILE "|" 
                           SED-LOCAL-BUF ">" UNSORTED-LOCAL-FILE
                           DEV-NULL
                           DELIMITED BY SIZE INTO SYSTEM-BUF
                    CALL "SYSTEM" USING SYSTEM-BUF
                                  GIVING STAT-99
                    MOVE CD-BR-FILE-NAME TO SED-FILE-NAME
                    MOVE CD-CODE         TO SED-REFCD
                    MOVE " " TO SYSTEM-BUF
                    STRING CAT-CMD " " UNSORTED-LOCAL-FILE "|" 
                           SED-BUF2 ">>" SORTED-FILE
                           DEV-NULL
                           DELIMITED BY SIZE INTO SYSTEM-BUF
                    CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99
                    GO TO NEXT-CD1.
           PERFORM CLOSE-CD1-FILE.
           
      * REMOVE DUPLICATE BATCH NUMBERS FROM FILE:
      * CAT SORT-FILE|SED 'S/MMYY/MMY:/'|SORT -U +1N -T: > MKTEMP-BUF
      * REPLACE LAST DIGIT OF DATE TO SPLIT LINE INTO COLON DELIMITED
      * COLUMNS
      * SORT SECOND COLUMN REMOVE DUPLICATES.
      
           MOVE WSS-TRANS-DATE-MMDD TO SED-DATE1 SED-DATE2.
           MOVE ":" TO SED-DELIM.
           MOVE " " TO SYSTEM-BUF.
           STRING CAT-CMD " " SORTED-FILE "|" SED-BUF ">" UNSORTED-FILE
                  DEV-NULL
                  DELIMITED BY SIZE INTO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF
                         GIVING STAT-99.
           MOVE " " TO SYSTEM-BUF.

           IF OPSYS-BUF = "LIN"
              STRING CAT-CMD " " UNSORTED-FILE "|" LINUX-SORT-CMD ">"
                     SORTED-FILE DEV-NULL
                     DELIMITED BY SIZE INTO SYSTEM-BUF 
           ELSE
              STRING CAT-CMD " " UNSORTED-FILE "|" SORT-CMD ">"
                     SORTED-FILE DEV-NULL
                     DELIMITED BY SIZE INTO SYSTEM-BUF.

           CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99.

           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-WK-FILE.

       GET-BATCHES-FOR-TODAY-NEXT.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              GO TO GET-BATCHES-FOR-TODAY-EXIT.

           IF WK-BATCH-NO NOT NUMERIC
              GO TO GET-BATCHES-FOR-TODAY-EXIT.

      * REARRANGE LOCAL 'ED' ENTRIES:

           IF WK-BP = "B" AND WK-BRANCH NUMERIC
              MOVE WK-REFCD       TO MESS
              MOVE WK-BATCH-REFCD TO WK-REFCD
              MOVE MESS           TO WK-BATCH-REFCD.

           MOVE "BR"     TO CD-TYPE.
           MOVE WK-REFCD TO CD-CODE.
           PERFORM READ-CD1-FILE.
           IF IO-BAD OR (CD-BR-FILE-TYPE = "LC" OR "C")
              GO TO GET-BATCHES-FOR-TODAY-NEXT.

           IF WK-REFCD = REFCD
              MOVE WK-BATCH-NO TO WK2-BATCH-NO
              MOVE WK-REFCD TO WK2-REFCD
              PERFORM WRITE-WK2-FILE.

           GO TO GET-BATCHES-FOR-TODAY-NEXT.

       GET-BATCHES-FOR-TODAY-EXIT.
           PERFORM CLOSE-WK-FILE.
           PERFORM CLOSE-CD1-FILE.

           MOVE " " TO SYSTEM-BUF.
           STRING RM-CMD " " UNSORTED-FILE " " DEV-NULL
                  DELIMITED BY SIZE INTO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99.

           MOVE " " TO SYSTEM-BUF.
           STRING RM-CMD " " UNSORTED-LOCAL-FILE " " DEV-NULL
                  DELIMITED BY SIZE INTO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99.

           MOVE " " TO SYSTEM-BUF.
           STRING RM-CMD " " SORTED-FILE " " DEV-NULL
                  DELIMITED BY SIZE INTO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99.


      ******************************************************************
       FIND-LOWEST-BATCH SECTION.
      ******************************************************************

      * FIND LOWEST BATCH NUMBER FOR SELECTED REFCD

           MOVE 0 TO WK2-BATCH-NO BATCH.
           MOVE BATCH-REFCD TO WK2-REFCD.
           PERFORM START-WK2-FILE.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 0
              MOVE WK2-BATCH-NO TO BATCH
              GO TO EXIT-FIND-LOWEST-BATCH.
 
           MOVE "NO FILES FOR REFCD &&&&& , ABORTED" TO OTH-LOG-MSG.
           INSPECT OTH-LOG-MSG REPLACING FIRST "&&&&&" BY REFCD.
           PERFORM CREATE-LOG.

           MOVE "X" TO ERRCD.

       EXIT-FIND-LOWEST-BATCH.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE 0 TO Q-POS C-POS P-POS.     
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF. 
           MOVE "00" TO P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE REFCD               TO BATCH-REFCD.
           MOVE EXT-CONAME-USER-LOC TO BRANCH.

      * SAVE PIECES OF THE USERS ENVIRONMENT

           ACCEPT ORIG-FIL      FROM ENVIRONMENT "FIL".
           
      * CREATE SPOOL DIRECTORY AND OPEN SPOOL FILE IN 'EO'

           IF (EXT-ROUTE-BUF = "00" OR "70")
              PERFORM CREATE-SPOOL-DIR
              IF ERRCD NOT = " "
                 GO TO ENDIT
              END-IF
              MOVE EX-SPOOL TO PRU-FILE.

           MOVE PRU-FILE TO OTH-LOG-SPOOL-DIR.

      * TEST IF LINUX OR AIX OR SCO
           MOVE "OPSYS" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-BAD
              MOVE SPACES TO GETENV-BUF.
           MOVE GETENV-BUF TO OPSYS-BUF.

           MOVE "BEGIN OTHER POSTING FOR BATCH &&&" TO OTH-LOG-MSG.
           INSPECT OTH-LOG-MSG REPLACING FIRST "&&&" BY EXT-EOBUF-PCODE.
           PERFORM CREATE-LOG.

           PERFORM OPEN-CD1-FILE.
           MOVE "BR"   TO CD-TYPE.
           MOVE REFCD  TO CD-CODE.
           PERFORM READ-CD1-FILE.
           IF (IO-BAD) OR (CD-BR-FILE-TYPE = "LC" OR "C")
              MOVE "REF CODE &&&&& NOT ON FILE, ABORTED" TO OTH-LOG-MSG
              INSPECT OTH-LOG-MSG REPLACING FIRST "&&&&&" BY REFCD
              PERFORM CREATE-LOG
              GO TO ENDIT.

           MOVE " " TO ERRCD.

      * SETTING UP BASE/MACHINE/DATA FOR BP PATHS.

           MOVE EXT-FILPATH-BASE        TO BP-ALL-BASE
                                           BP-WK-GB-BASE.
           MOVE EXT-FILPATH-MACHINE     TO BP-ALL-MACHINE
                                           BP-WK-GB-MACHINE.
           MOVE EXT-FILPATH-DATA        TO BP-ALL-DATA.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO TRANS-DATE WSS-TRANS-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-REFCD.
           MOVE "E  A050REFCD." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO EXIT-PARM.
           IF NOT ENTER-KEY
              GO TO ENTER-REFCD.

           PERFORM OPEN-CD1-FILE.
           MOVE "BR"   TO CD-TYPE.
           MOVE VDUBUF TO REFCD 
                          BATCH-REFCD
                          CD-CODE.
           PERFORM READ-CD1-FILE.
           IF (IO-BAD) OR (CD-BR-FILE-TYPE = "LC" OR "C")
              MOVE "REFERENCE CODE NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-REFCD.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           MOVE "S  A050REFCD." TO VDU.
           MOVE REFCD           TO VDUBUF.
           PERFORM SCREENIO.



      ******************************************************************
      *
      *  CREATE SPOOL DIRECTORY IN /XXX/XXXX/R#/EO
      *
      * STORE SPOOL FILE NAME AS /XXX/XXXX/R$/EO/....
      * WE NEED A FULL PATH NAME HERE ELSE DIR WILL CHANGE WHEN WE
      * CHANGE DIRECTORIES TO ANOTHER BRANCH.
      * 
      * CHANGED TO ACCESS AS ../../ORIG/R1/EO/STEPBPREF, WITH ORIG
      * BEING ORIG DATA PATH, FULL PATH TOO LONG FOR ACCESS-BUF
      ******************************************************************
       CREATE-SPOOL-DIR SECTION.

           MOVE " " TO ERRCD.

           MOVE EXT-FILPATH-BASE    TO EX-SPOOL-BASE.
           MOVE EXT-FILPATH-MACHINE TO EX-SPOOL-MACHINE.
           MOVE ORIG-DATA-PATH      TO EX-SPOOL-DATA-PATH.
           MOVE WSS-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO EX-SPOOL-DATE.
           MOVE " "                 TO EX-SPOOL-FILE.
           MOVE EX-SPOOL TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING ACCESS-BUF GIVING STAT-99
              IF STAT NOT = "00"
                 MOVE "E" TO ERRCD
                 GO TO EXIT-CREATE-SPOOL-DIR.  

           MOVE "/"                 TO EX-SPOOL-FILE.
           MOVE "OTHUPD"            TO EX-SPOOL-NAME.
           MOVE "  "                TO EX-SPOOL-SUF.

      * THEY ARE ALLOWED TO RUN THIS  MORE THAN ONCE ON THE
      * SAME TRANSACTION DATE, SO CHECK TO SEE IF REPORT EXISTS, IF
      * SO, INCREMENT THE STEP # SO WE DON'T WIPE OUT PRIOR REPORTS

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
           MOVE TIME-EDIT  TO OTH-LOG-TIME.
           MOVE EXT-CONAME-USER-ID TO OTH-LOG-USERID.
           MOVE "REF: "    TO OTH-LOG-PROMPT1.
           MOVE REFCD      TO OTH-LOG-REFCD.
           IF BATCH NOT = 0
              MOVE "BAT: " TO OTH-LOG-PROMPT2.
           MOVE BATCH      TO OTH-LOG-BATCH.
           PERFORM WRITE-OTH-LOG-FILE.
           PERFORM CLOSE-OTH-LOG-FILE.

      ******************************************************************
       FORM-RESET SECTION.
      ******************************************************************

           MOVE EXT-CONAME-CONAME          TO OTHUPD-CONAME.
           MOVE EXT-CONAME-SYSDATE         TO OTHUPD-SYSDATE.
           DISPLAY OTHUPD-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE OTHUPD-FORM-FIELDS         TO WR-BUF.
           MOVE OTHUPD-FORM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************

           MOVE OTHUPD-HELP--FILE          TO HELP-LINK-FILE.
           MOVE OTHUPD-HELP--DIR           TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************************

           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.

           EVALUATE TRUE
              COPY "LIBSP/OTHUPD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ******************************************************************
       MISC SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/MKTEMP.CPY".

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       OPEN-WK-FILE.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
           OPEN INPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE.
       READ-WK-FILE.
           PERFORM READ-IT.
           MOVE WK-IFN TO E-FILE.
           READ WK-FILE.
           IF IO-FG = 8
              GO TO READ-WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.
       COPY "LIBGB/GBWK2IN.CPY".
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
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY OTHUPD-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
