       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPW1.
       DATE-WRITTEN.    032689.
      ************************************************************
      *                 (LP)
      *  DESCRIPTION:   SET BATCH NO FOR BATCH PAYMENT PROCESSING
      *
      *                 THIS PROGRAM ALLOWS THE ENTRY OF A
      *                 BATCH NO. THAT ALREADY EXISTS, THE
      *                 ESTABLISHMENT OF A NEW ONE OR ASSIGNING
      *                 A MAGNETIC FILE FOR PROCESSING TODAY.
      *
      *            EXPLAINATION OF WHY THIS PROGRAM IS READING FILES
      *            IN GLOBAL (ED) WHEN SETTING A BATCH NO. TO PROCESS:
      *
      *            SOME COMPANIES PROCESS BATCH PAYMENTS FROM A DUMMY
      *            BRANCH THAT DOSEN'T HAVE LOANS, SO TO PROCESS BATCH
      *            PAYMENTS NO LOCAL 'ED' FILE EXISTS.
      *
      *            1. WHEN A FILE IS COPIED FROM SOME MEDIA TO THE
      *               GLOBAL ED DIRECTORY, IT IS GIVEN A FILE NAME
      *               LIKE 'BTFILE' BATCH FILE 'CCFILE' CREDIT CARDS
      *               'EZPAY' EASY PAY 'LBOX' LOCK BOX, ETC.
      *            2. DURING THE CREATE BRANCH BATCH FILE STEP
      *               WHICH BREAKS OUT THE FILE TO BRANCH'S LOCAL ED
      *               DIRECTORYS, THE GLOBAL ED FILE EX. CCFILE IS RENAMED
      *               CCFILE -> CCFIL0430789 (DATE 0430)  (BATCH NO. 789).
      *               THE LOCAL ED DIRECTORY EX. FOR BRANCH 0105
      *               CREATES AND WRITES TO A FILE 'B01050430789CCFILE'.
      *               AFTER THE BRAKE OUT COMPANIES OPEN UP A DAY IN ONE OF
      *               THEIR BRANCHES TO POST. WHEN THEY
      *
      *REV:
      * BAH 072496 ADD 1 TO BR-BP-NEXT-BATCH IF IT IS 0
      * MJD 990311 REMOVED REFERENCE TO BP INDEX FILE.
      * MJD 990923 ADDED LOCIC TO SEARCH GLOBAL ED DIRECTORY TO
      *            GET THE GLOBAL BATCH NUMBERS TO USE. (I.E. MERCURY
      *            RUNS FROM BRANCH 1 WHICH DOES NOT HAVE A LOCAL ../ED)
      * MJD 000912 FIXED BUG, WE WERE NOT LOOKING IN GLOBAL ED FOR LOCK
      *            BOX BATCH NUMBERS.
      * BAH 010824 ADDED DESC08 AND SPACED IT OUT. IT LOOKED UGLY !!
      * JTG 021011 ADDED NEW CODE FILE TYPE 'BR' - BATCH PAYMENT REFERENCE CODES
      *            BATCH PAYMENT REVISIONS                          REGACC #0006
      * JTG 021212 CHANGED SYSTEM-CALL TO ACU "SYSTEM" CALL       REGACC #0006
      *            LIST ALL BATCH FILES IN CURRENT BRANCHS GLOBAL ../../ED DIR
      *            WHEN THERE ARE NO FILES;
      *              IN ACU 311  SYSTEM-CALL PUTS LS -ERROR IN ACU_TCLP
      *              IN ACU 5104 PUT ERROR MESSAGE ON SCREEN
      * JTG 021226 ADDED GP BATCH RANGES FOR DISPLAY               REGACC #0006
      * JTG 030502 REARRANGED LOCAL 'ED' ENTRIES IN WK-REC TO BE CONSISTENT
      *            WITH GLOBAL 'ED' ENTRIES                        CENTRIX #JTGX
      * BLF 070305 ADDED CHECK IN LOOP WHICH READS CDFILE TO SKIP IF
      *            CD-BR-FILE-NAME IS BLANK.  MAINTENANCE DOESN'T ALLOW BLANK
      *            BUT CENTRIX SET UP A BATCH PMT CODE AS TYPE "LC" WITH
      *            BLANK FILENAME & CHANGED TO A STANDARD.  THEN WHEN YOU
      *            WENT INTO "SET BATCH TO PROCESS", NO FILES WOULD DISPLAY
      *            UNTIL YOU DELETED THE INVALID GUY WITH SPACES IN THE
      *            FILE NAME.
      *  CS 070418 CHANGED SIZE OF BP-FILE TO 116 BYTES REGACC PR#168
      * BLF 080131 EXPANDED DESC-BUF TABLE, WAS DISPLAYING SELECTION 11 FROM
      *            BATCH PROCESSING MENU (11. PROCEDURE REPORT PRINT) ON
      *            SET BATCH TO PROCESS SCREEN)
      * BLF 080204 INVESTIGATED SCREEN GETTING GARBLED WHEN THIS IS RUN WITH
      *            ENV VARIABLE TCLP_NODASHB = Y.  I THINK WE ONLY RUN WITH
      *            THIS SET IN DEVELOPMENT.  WHEN Y, CURSOR USUALLY DROPS
      *            DOWN A LINE FOR ENTRY, & SCREEN IS SKEWED EVEN AFTER YOU
      *            LEAVE THIS PROGRAM.  TESTED & FOUND THAT JUST ABOUT ALL
      *            OF THE CALL "SYSTEM" CALLS CAUSE THE PROBLEM.  THE
      *            PROBLEM IS INCONSISTENT.  IT SEEMED BETTER WHEN I PUT
      *            IN CALL FRESET AFTER EACH SYSTEM CALL, SO I LEFT THESE
      *            IN, BUT STILL SEE A LITTLE PROBLEM SOMETIMES.  OLD
      *            VERSION SAVED IN SV/LONPW1.080102, THIS VERSION SAVED
      *            IN SV/LONPW1.FRESETS.
      *  CS 100518 ADDED GETENV ON OPSYS TO FLAG,IF RUNNING LINUX, THE SORT
      *            COMMAND DID NOT RECOGNIZE +1N
      *            SORT      :SORT -U +1N - T:
      *            LINUX-SORT: SORT -UK1 -T:    "LINUX PR# 0001"
      * BLM 140505 REPLACED ALL C$SYSTEM CALLS BY PERFORM SYSTEM-CALL,
      *            VERYANT CHANGE
      * BAH 150317 ADDED SED-VIX TO NOT HAVE DUPLICATE BATCH NUMBERS FOR THE
      *            .VIX FILE. 
      * BLM 161018 MOVE 1 TO BP-SEQNO
      * BAH 170703 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0912 {PR#01246} WORLD <A30> [JAY CISZEWSKI]
      *          WHEN ADDING THIS PR# TO A30, I NOTICED THAT A COPY
      *          MEMBER WAS NOT PROPERLY SETUP.  TO FIX IT I DID:
      *          - COPIED LIBLP/LPBP.CPY TO ../SV/LPBP.NO
      *          - MOVED  LIBLP/LPBP.CPY TO LIBLP/LPBP_CMD.CPY
      *          - EDITED LIBLP/LPBP_CMD.CPY TO LOWERCASE WHEN NEEDED
      * BAH 190124 SPLIT CDFILE
      * BAH 20210202 SKIP CD-BR-FILE-TYPE = "C", UMC DEBIT CARD #PR1490
      * BAH 2024.0628 REMOVED SELECTIONS 2. BATCH PAYMENT ENTRY (LONPW2),
      *               3. BATCH PAYMENT FILE LISTING (LONPW3) AND
      *               4. BATCH PAYMENT FILE UPDATE (LONPW4) PER KEVIN S35Q-78
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSBP.CPY".
       COPY "LIBGB/GBFSWKS.CPY".
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDBP.CPY".
      **************************************************
      *    TEMP WORK RECORD FOR BATCH FILES
      **************************************************
       FD  WK-FILE
           LABEL RECORD IS STANDARD.
       01  WK-REC.
      * /US2/NEWS/R1/GA0001/ED/B0001051:007
      * 12345678901234567890123
      * /US2/NEWS/R1/ED/B0001051:007
      * 12345678901234567890123
      * THE WORK FILE IS 'CUT' TO START WITH THE "B" IN THE BATCH FILE NAME
      * B0001051:007
           03  WK-REFCD.
               05  WK-BP          PIC X.
               05  WK-BRANCH      PIC 9(4).
           03  WK-DATE            PIC 9(4).
           03  WK-BATCH-NO        PIC 999.
           03  WK-BATCH-REFCD     PIC X(5).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPW1.CBL S35 02/23/24-10:14:54 >".

      ***********************************
      *    PROGRAM WORKERS
      ***********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       01  WK-IFN            PIC X(2)    VALUE "WK".
       01  BP1-IFN           PIC X(3)    VALUE "BP1".

       01  ONE               PIC S9.
       01  ELE               PIC 9(2).
       01  SUB               PIC 9(2).
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).
       01  NEW-FLAG          PIC 9.
       01  TRANS-MMDD        PIC 9(4).
       01  UNASSIGNED-BATCH  PIC X.

       01  BATCH-COUNTER              PIC 99.
       01  BATCH-TABLE.
           03  BATCH-TBL              OCCURS 18.
               05  BATCH-ELE          PIC 999.
               05  BATCH-ELE-REFCD    PIC X(5).

       01  MESSAGE-LIST      PIC X(5)  VALUE "MESS.".
       01  MSEL-LIST         PIC X(5)  VALUE "MSEL.".
       01  UNSORTED-FILE     PIC X(20) VALUE " ".
       01  SORTED-FILE       PIC X(20) VALUE " ".

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

      * SED 'S/.VIX//
       01  SED-VIX.
           03  FILLER        PIC X(6)  VALUE X'736564202773'.
           03  FILLER        PIC X(2)  VALUE "/.".
           03  FILLER        PIC X(3)  VALUE X'766978'.
           03  FILLER        PIC X(3)  VALUE "//'".

       01  SED-BUF2.
           03  FILLER        PIC X(6)  VALUE X'736564202773'.
           03  FILLER        PIC X(01) VALUE "/".
           03  SED-FILE-NAME PIC X(5)  VALUE " ".
           03  FILLER        PIC X(01) VALUE "/".
           03  SED-REFCD     PIC X(5)  VALUE " ".
           03  FILLER        PIC X(02)  VALUE "/'".

      *    [SORT -U +1N - T:]
       78  SORT-CMD          VALUE X'736F7274202D75202B316E202D743A'.
      *    [SORT -UK1 -T:]
       78  LINUX-SORT-CMD    VALUE X'736F7274202D756B31202D743A'.

       01  OPSYS-BUF         PIC X(4) VALUE SPACES.

      ************************************************************
      *    SCREEN DISPLAY BUFFER
      ************************************************************
       01  RANGE-LIST.
           03  FILLER                   PIC X(40) VALUE
           "RANGE05 RANGE06 RANGE07 RANGE08 RANGE09.".

       01  DESC-BUF                     VALUE " ".
           03  DESC-ELE                 OCCURS 11
                                        PIC X(43).
       01  DESC-LIST.
           03  FILLER                   PIC X(49) VALUE
           "DESC01 DESC02 DESC03 DESC04 DESC05 DESC06 DESC07 ".
           03  FILLER                   PIC X(28) VALUE
           "DESC08 DESC09 DESC10 DESC11.".

       01  DISPLAY-BUF.
           03  UNASSIGNED               PIC X(22)   VALUE " ".
           03  D-OPEN                   PIC X(22)   VALUE
               "OPEN BATCHES FOR TODAY".
           03  D-DASH                   PIC X(22)   VALUE ALL "-".
           03  D-BUF                    VALUE " ".
               05  D-BATCHX             OCCURS 18.
                   07  D-BATCH          PIC 999.
                   07  D-BATCH-REFCD    PIC X(5).
       01  DISPLAY-LIST.
           03  FILLER                   PIC X(9)  VALUE
           "B B0 B00 ".
           03  FILLER                   PIC X(45) VALUE
           "B01 B011 B02 B021 B03 B031 B04 B041 B05 B051 ".
           03  FILLER                   PIC X(45) VALUE
           "B06 B061 B07 B071 B08 B081 B09 B091 B10 B101 ".
           03  FILLER                   PIC X(45) VALUE
           "B11 B111 B12 B121 B13 B131 B14 B141 B15 B151 ".
           03  FILLER                   PIC X(27) VALUE
           "B16 B161 B17 B171 B18 B181.".

       01  999-ENTER                    PIC X(43) VALUE
           "ENTER AVAILABLE BATCH NO. TO PROCESS".
       01  F3-MEDIA                     PIC X(43) VALUE
           "F3 - ASSIGN ALLOTMENT MEDIA FILE TO A BATCH".
       01  F5-ASSIGN                    PIC X(43) VALUE
           "F5 - ESTABLISH NEW BATCH NO. TO PROCESS".
       01  F7-EXIT                      PIC X(43) VALUE
           "F7 - EXIT".

      **********************************
      *    ELEMENT SPECIFICATIONS
      **********************************
       01  MAX               PIC 99   VALUE 1.
       01  SPEC-TABLE.
      *01. NEW SOCIAL SECURITY NUMBER
           03  FILLER        PIC X(8) VALUE "E  A110 ".
       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 1 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

       COPY "LIBLP/LPBP_CMD.CPY".
       COPY "LIBLP/LPBPPATH.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  LOCAL-SEQ           PIC 9(2)   VALUE 00.
       01  GLOBAL-SEQ          PIC 9(2)   VALUE 01.

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPW_DEF.CPY".
       COPY "LIBLP/LONPW_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPW_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               BP-FILE WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
            CLOSE
               BP-FILE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ********************************
      *    MAIN PROGRAM MODULE
      ********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPW" TO VDU-FORM-NAME.

           CALL "C$TOLOWER" USING CUT-CMD, VALUE 11.
           CALL "C$TOLOWER" USING CUT-LOCAL-CMD, VALUE 11.
           CALL "C$TOLOWER" USING DEV-NULL, VALUE 12.

      *    MOVE CONAME  TO C-CONAME.
      *    MOVE EXT-CONAME-SYSDATE TO C-SYSDATE.
      *    CALL XFER   USING STAT CONAME-LIST CONAME-BUF.
      *    CALL WRFORM USING STAT CONAME-LIST.

           MOVE SPACES TO WR-BUF.
           MOVE RANGE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE DESC-BUF TO WR-BUF.
           MOVE DESC-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * TEST IF LINUX OR AIX OR SCO
           MOVE "OPSYS" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-BAD
              MOVE SPACES TO GETENV-BUF.
           MOVE GETENV-BUF TO OPSYS-BUF.


           PERFORM SET-BP-DATE.

           PERFORM TEST-FOR-UNASSIGNED-BATCH.
           PERFORM GET-BATCHES-FOR-TODAY.
           PERFORM DISPLAY-BATCH.

           MOVE " " TO ERRCD.

       MAIN-MODULE.

           PERFORM REC-BP-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           GO TO MAIN-MODULE.

      ***********************************
       REC-BP-MODULE SECTION.
      ***********************************
           PERFORM ALARM.
           IF UNASSIGNED-BATCH = "Y"
              IF BATCH-COUNTER > 0
                 MOVE 999-ENTER TO DESC-ELE(1)
                 MOVE F3-MEDIA  TO DESC-ELE(2)
                 MOVE F5-ASSIGN TO DESC-ELE(3)
                 MOVE F7-EXIT   TO DESC-ELE(4)
                 MOVE " "       TO DESC-ELE(5) DESC-ELE(6)
                                   DESC-ELE(7) DESC-ELE(8)
              ELSE
                 MOVE F3-MEDIA  TO DESC-ELE(1)
                 MOVE F5-ASSIGN TO DESC-ELE(2)
                 MOVE F7-EXIT   TO DESC-ELE(3)
                 MOVE " "       TO DESC-ELE(4)
                 MOVE " "       TO DESC-ELE(5) DESC-ELE(6)
                                   DESC-ELE(7) DESC-ELE(8)
           ELSE
              IF BATCH-COUNTER > 0
                 MOVE 999-ENTER TO DESC-ELE(1)
                 MOVE F5-ASSIGN TO DESC-ELE(2)
                 MOVE F7-EXIT   TO DESC-ELE(3)
                 MOVE " "       TO DESC-ELE(4)
                 MOVE " "       TO DESC-ELE(5) DESC-ELE(6)
                                   DESC-ELE(7) DESC-ELE(8)
              ELSE
                 MOVE F5-ASSIGN TO DESC-ELE(1)
                 MOVE F7-EXIT   TO DESC-ELE(2)
                 MOVE " "       TO DESC-ELE(3)
                 MOVE " "       TO DESC-ELE(4)
                 MOVE " "       TO DESC-ELE(5)
                 MOVE " "       TO DESC-ELE(5) DESC-ELE(6)
                                   DESC-ELE(7) DESC-ELE(8).
           MOVE DESC-BUF TO WR-BUF.
           MOVE DESC-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " "    TO MESS.
           PERFORM SEND-LEGEND.

           MOVE " "    TO IO-TYPE.
           MOVE 1 TO NEW-FLAG.
           MOVE "E  N030BATCHNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16"
              IF UNASSIGNED-BATCH NOT = "Y"
                 GO TO REC-BP-MODULE
              ELSE
                 PERFORM SET-UNASSIGNED-BATCH
                 IF IO-TYPE NOT = "END"
                    MOVE 0 TO BATCH
                    GO TO REC-BP-MODULE
                 ELSE
                    GO TO END-REC-BP-MODULE.
           IF VDUSTATUS = "1:"
              PERFORM SET-NEW-BATCH-NO
              IF IO-TYPE NOT = "END"
                 GO TO REC-BP-MODULE
              ELSE
                 GO TO END-REC-BP-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BP-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-BP-MODULE.
           IF VDUNUM0 = 0
              GO TO REC-BP-MODULE.
           MOVE VDUNUM0 TO BATCH.

           PERFORM SET-EXISTING-BATCH.
           IF IO-TYPE NOT = "END"
              GO TO REC-BP-MODULE
           ELSE
              GO TO END-REC-BP-MODULE.
       END-REC-BP-MODULE.
           EXIT.

      *************************************************
      *    ENTRY-MODULE
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
      *************************************************
           MOVE " "    TO IO-TYPE.
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
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = " "
                 MOVE "CLR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE
              ELSE
                 MOVE "UPD" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL " "
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
           GO TO F-01.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ********************************************************
      *    TEST-FOR-UNASSIGNED-BATCH
      *
      *    TEST ACCESS TO UNASSIGNED BATCH IN LOCAL 'ED'
      *         '../ED/BBBB0000000     '
      *                BRNOMMDDBATREFCD
      ********************************************************
       TEST-FOR-UNASSIGNED-BATCH SECTION.
      ********************************************************
           MOVE " "        TO UNASSIGNED-BATCH.
           MOVE BRANCH     TO BP-ALL-BRANCH.
           MOVE 0          TO BP-ALL-DATE.
           MOVE 0          TO BP-ALL-BATCH-NO.
           MOVE " "        TO BP-ALL-REFCD.
           PERFORM LOAD-BP1-FILE.
           MOVE BP-PATH    TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE "Y" TO UNASSIGNED-BATCH.

      **********************************
       GET-BATCHES-FOR-TODAY SECTION.
      **********************************

      * MKTEMP WILL GENERATE UNIQUE NAMES FOR EACH FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO UNSORTED-FILE.

           MOVE SPACES TO SYSTEM-BUF.
           STRING "> " UNSORTED-FILE DEV-NULL
                  DELIMITED BY SIZE INTO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      * CREATE TEMP FILE TO HOLD SORTED RESULT OF "LS" COMMAND

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO SORTED-FILE.

      * LIST ALL BATCH FILES IN CURRENT BRANCHS LOCAL
      *      /USR/DATA/R1/GA0001/ED/B000106140016
      *                                 ^BRANCH
      *                                  MMDD
      *                                      ^ BATCH NO

           MOVE BRANCH              TO BP-ALL-BRANCH.
           PERFORM SET-BP-DATE.
           MOVE TRANS-MMDD          TO BP-ALL-DATE.
           MOVE "*"                 TO BP-ALL-BATCH-NOX.
           PERFORM LOAD-BP1-FILE.

           MOVE SPACES TO SYSTEM-BUF.
           STRING LS-CMD
                  " -1 "
                  BP-ALL-PATH
                  DEV-NULL
                  "|"
                  CUT-LOCAL-CMD
                  ">"
                  SORTED-FILE
                  DELIMITED BY SIZE INTO SYSTEM-BUF.

           PERFORM SYSTEM-CALL.

      * LIST ALL BATCH FILES IN CURRENT BRANCHS GLOBAL ../../ED DIR
      * TO TEMP-FILE. USE THE CUT COMMAND TO MAKE FILENAMES LINE UP
      * WITH LS OF LOCAL FILES.
      *   EXAMPLE:
      *    ../../ED/BTFIL0999100 |CUT -C4-80 == ../ED/BTFIL0999100

           PERFORM LOAD-BP-WK-GB-PATH.
           MOVE BP-WK-BT-PATH TO BP-WK-GB-FILE-NAME.
           PERFORM SET-BP-DATE.
           MOVE TRANS-MMDD TO BP-WK-GB-DATE.
           MOVE "*" TO BP-WK-GB-BATCH-NOX.

           MOVE SPACES TO SYSTEM-BUF.
           STRING LS-CMD
                  " -1 "
                  BP-WK-GB-PATH
                  DEV-NULL
                  "|"
                  CUT-CMD
                  ">>"
                  SORTED-FILE
                  DELIMITED BY SIZE INTO SYSTEM-BUF.

           PERFORM SYSTEM-CALL.

      * LIST ALL BATCH FILES IN CURRENT BRANCHS GLOBAL ../../ED DIR
      * TO TEMP-FILE. USE THE CUT COMMAND TO MAKE FILENAMES LINE UP
      * WITH LS OF LOCAL FILES.
      *   EXAMPLE:
      *       ../../ED/LBFIL0999100 |CUT -C4-80 == ../ED/LBFIL0999100

           PERFORM LOAD-BP-WK-GB-PATH.
           MOVE BP-WK-LB-PATH TO BP-WK-GB-FILE-NAME.
           PERFORM SET-BP-DATE.
           MOVE TRANS-MMDD    TO BP-WK-GB-DATE.
           MOVE "*"           TO BP-WK-GB-BATCH-NOX.
           MOVE SPACES TO SYSTEM-BUF.
           STRING LS-CMD
                  " -1 "
                  BP-WK-GB-PATH
                  DEV-NULL
                  "|"
                  CUT-CMD
                  ">>"
                  SORTED-FILE
                  DELIMITED BY SIZE INTO SYSTEM-BUF.

           PERFORM SYSTEM-CALL.

      * LIST ALL BATCH FILES IN CURRENT BRANCHS GLOBAL ../../ED DIR
      * TO TEMP-FILE. USE THE CUT COMMAND TO MAKE FILENAMES LINE UP
      * WITH LS OF LOCAL FILES.
      *   EXAMPLE:
      *      ../../ED/CCARD1231700 |CUT -C4-80 == ../ED/CCARD1231700

           PERFORM OPEN-CD1-FILE.

           MOVE "BR"    TO CD-TYPE
                           QCD1-WBEG-TYPE
                           QCD1-WEND-TYPE.
           MOVE SPACES  TO CD-CODE
                           QCD1-WBEG-CODE.
           MOVE ALL "Z" TO QCD1-WEND-CODE.

           PERFORM START-CD1-FILE.
       NEXT-CD1.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-GOOD
              IF (CD-TYPE = "BR")
                 IF (CD-BR-FILE-TYPE = "LC" OR
                      CD-BR-FILE-TYPE = "C" OR
                       CD-BR-FILE-NAME = SPACES)
                    GO TO NEXT-CD1
                 ELSE
                    MOVE SPACES TO SYSTEM-BUF
                    PERFORM LOAD-BP-WK-GB-PATH
                    MOVE CD-BR-FILE-NAME TO BP-WK-GB-FILE-NAME
                    PERFORM SET-BP-DATE
                    MOVE TRANS-MMDD      TO BP-WK-GB-DATE
                    MOVE "*"             TO BP-WK-GB-BATCH-NOX
                    STRING LS-CMD
                           " -1 "
                           BP-WK-GB-PATH
                           DEV-NULL
                           "|"
                           CUT-CMD
                           ">"
                           UNSORTED-FILE
                           DELIMITED BY SIZE INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    MOVE CD-BR-FILE-NAME TO SED-FILE-NAME
                    MOVE CD-CODE         TO SED-REFCD
                    MOVE SPACES TO SYSTEM-BUF
                    STRING CAT-CMD " " UNSORTED-FILE "|"
                           SED-BUF2 ">>" SORTED-FILE
                           DEV-NULL
                           DELIMITED BY SIZE INTO SYSTEM-BUF
                    PERFORM SYSTEM-CALL
                    GO TO NEXT-CD1.

           PERFORM CLOSE-CD1-FILE.

      * REMOVE DUPLICATE BATCH NUMBERS FROM FILE:
      * CAT SORT-FILE|SED 'S/MMYY/MMY:/'| SED 'S/.VIX//|SORT -U +1N -T:
      *                  > MKTEMP-BUF
      * REPLACE LAST DIGIT OF DATE TO SPLIT LINE INTO COLON DELIMITED COLUMNS
      * SORT SECOND COLUMN REMOVE DUPLICATES.

           MOVE TRANS-MMDD TO SED-DATE1 SED-DATE2.
           MOVE ":" TO SED-DELIM.
           MOVE SPACES TO SYSTEM-BUF.
           STRING CAT-CMD " " SORTED-FILE "|" SED-BUF "|" SED-VIX
                    ">" UNSORTED-FILE DEV-NULL
                  DELIMITED BY SIZE INTO SYSTEM-BUF.

           PERFORM SYSTEM-CALL.

           MOVE SPACES TO SYSTEM-BUF.
           IF OPSYS-BUF = "LIN"
              STRING CAT-CMD " " UNSORTED-FILE "|" LINUX-SORT-CMD ">"
                     SORTED-FILE DEV-NULL
                     DELIMITED BY SIZE INTO SYSTEM-BUF
           ELSE
              STRING CAT-CMD " " UNSORTED-FILE "|" SORT-CMD ">"
                     SORTED-FILE DEV-NULL
                     DELIMITED BY SIZE INTO SYSTEM-BUF.

           PERFORM SYSTEM-CALL.

      * READ TEMP FILE TO DISPLAY OPEN BATCH FILES:

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 18
               MOVE 0   TO BATCH-ELE(SUB)
               MOVE " " TO BATCH-ELE-REFCD(SUB)
           END-PERFORM.

           MOVE 0 TO BATCH-COUNTER.
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-WK-FILE.
       GET-BATCHES-FOR-TODAY-NEXT.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              GO TO GET-BATCHES-FOR-TODAY-EXIT.
           IF WK-BATCH-NO NOT NUMERIC
              GO TO GET-BATCHES-FOR-TODAY-EXIT.
           IF BATCH-COUNTER > 17
              GO TO GET-BATCHES-FOR-TODAY-EXIT.
      * REARRANGE LOCAL 'ED' ENTRIES:
           IF WK-BP = "B" AND
              WK-BRANCH NUMERIC
              MOVE WK-REFCD       TO MESS
              MOVE WK-BATCH-REFCD TO WK-REFCD
              MOVE MESS           TO WK-BATCH-REFCD.
           ADD 1 TO BATCH-COUNTER.
           MOVE WK-BATCH-NO    TO BATCH-ELE(BATCH-COUNTER).

           MOVE "BR"     TO CD-TYPE.
           MOVE WK-REFCD TO CD-CODE.
           PERFORM READ-CD1-FILE.
           IF (IO-BAD) OR (CD-BR-FILE-TYPE = "LC") OR
                            (CD-BR-FILE-TYPE = "C")
              MOVE " "      TO BATCH-ELE-REFCD(BATCH-COUNTER)
           ELSE
              MOVE WK-REFCD TO BATCH-ELE-REFCD(BATCH-COUNTER).

           GO TO GET-BATCHES-FOR-TODAY-NEXT.

       GET-BATCHES-FOR-TODAY-EXIT.
           PERFORM CLOSE-WK-FILE.
           PERFORM CLOSE-CD1-FILE.

      *    MOVE SPACES TO SYSTEM-BUF.
      *    STRING RM-CMD " " UNSORTED-FILE " "
      *           DELIMITED BY SIZE INTO SYSTEM-BUF.
      *    PERFORM SYSTEM-CALL.
           CALL "CBL_DELETE_FILE" USING UNSORTED-FILE GIVING STAT-99.

      *    MOVE " " TO SYSTEM-BUF.
      *    STRING RM-CMD " " SORTED-FILE " "
      *           DELIMITED BY SIZE INTO SYSTEM-BUF.
      *    PERFORM SYSTEM-CALL.
           CALL "CBL_DELETE_FILE" USING SORTED-FILE GIVING STAT-99.

      *********************************
       DISPLAY-BATCH SECTION.
      *********************************
           IF UNASSIGNED-BATCH = "Y"
              MOVE "MEDIA BATCH UNASSIGNED" TO UNASSIGNED.
           IF BATCH-COUNTER = 0
              MOVE " " TO D-OPEN D-DASH
              GO TO DISPLAY-BATCH-EXIT.
           MOVE 0 TO SUB.
           PERFORM DISPLAY-BATCH-BATCH-NO 17 TIMES.
       DISPLAY-BATCH-BATCH-NO.
           ADD 1 TO SUB.
           IF BATCH-ELE(SUB) NOT = 0
              MOVE BATCH-ELE(SUB)       TO D-BATCH(SUB)
              MOVE BATCH-ELE-REFCD(SUB) TO D-BATCH-REFCD(SUB).
       DISPLAY-BATCH-EXIT.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************
       SET-UNASSIGNED-BATCH SECTION.
      *********************************

      * GET NEXT AVAILABLE BATCH NO:

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "S  N030BATCHNO." TO VDU.
           IF BR-BP-NEXT-BATCH = 0
              ADD 1 TO BR-BP-NEXT-BATCH
              IF BR-BP-NEXT-BATCH > 99
                 MOVE 1 TO BR-BP-NEXT-BATCH.
           MOVE BR-BP-NEXT-BATCH TO VDUNUM0 BATCH.
           PERFORM SCREENIO.
           MOVE "S  A050BATCHREFCD." TO VDU.
           MOVE " "                  TO VDUBUF BATCH-REFCD.
           PERFORM SCREENIO.

       SET-UNASSIGNED-BATCH-ACCEPT.
           MOVE " " TO IO-TYPE.
           MOVE "CONFIRM PROCESSING OF MEDIA FILE TODAY (YES/NO)?"
                                                         TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM ALARM.
           PERFORM ALARM.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO SET-UNASSIGNED-BATCH-ACCEPT.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "    NO"
              GO TO SET-UNASSIGNED-BATCH-EXIT.
           IF ACTION NOT = "   YES"
              GO TO SET-UNASSIGNED-BATCH-ACCEPT.

           IF BATCH >= 100
              MOVE GLOBAL-SEQ TO PASSWDW-SEQ
              MOVE "TO PROCESS GLOBAL BATCHES" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE " CANNOT PROCESS GLOBAL BATCH FILES" TO MESS
                 PERFORM SEND-MESS
                 GO TO SET-UNASSIGNED-BATCH-ACCEPT.

      * RETEST FOR UNASSIGNED BATCH FILE:
           PERFORM TEST-FOR-UNASSIGNED-BATCH.
           IF UNASSIGNED-BATCH NOT = "Y"
              MOVE "MEDIA FILE WAS ALREADY ASSIGNED" TO MESS
              PERFORM SEND-MESS
              GO TO SET-UNASSIGNED-BATCH-EXIT.

      * ESTABLISH NEXT FREE BATCH NO:
           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           IF BR-BP-NEXT-BATCH NOT = BATCH
              MOVE "S  N030BATCHNO." TO VDU
              IF BR-BP-NEXT-BATCH = 0
                 ADD 1 TO BR-BP-NEXT-BATCH
              END-IF
              IF BR-BP-NEXT-BATCH > 99
                 MOVE 1 TO BR-BP-NEXT-BATCH
              END-IF
              MOVE BR-BP-NEXT-BATCH TO VDUNUM0 BATCH
              PERFORM SCREENIO.
           ADD 1 TO BR-BP-NEXT-BATCH.
           IF BR-BP-NEXT-BATCH = 0
              ADD 1 TO BR-BP-NEXT-BATCH.
           IF BR-BP-NEXT-BATCH > 99
              MOVE 1 TO BR-BP-NEXT-BATCH.
           PERFORM REWRITE-BR-FILE.
           PERFORM CLOSE-BR-FILE.

      * ASSIGN MEDIA FILE TO THE BATCH FOR TODAY:
      * DATA FILE:
           MOVE MV-CMD     TO SYSTEM-BUF.

           MOVE BRANCH     TO BP-ALL-BRANCH.
           MOVE 0          TO BP-ALL-DATE
                              BP-ALL-BATCH-NO.
           MOVE " "        TO BP-ALL-REFCD.
           PERFORM LOAD-BP1-FILE.
           MOVE BP-ALL-PATH TO MV-FILE1.

           MOVE BRANCH     TO BP-ALL-BRANCH.
           MOVE TRANS-MMDD TO BP-ALL-DATE.
           MOVE BATCH      TO BP-ALL-BATCH-NO.
           MOVE " "        TO BP-ALL-REFCD.
           PERFORM LOAD-BP1-FILE.
           MOVE BP-ALL-PATH TO MV-FILE2 ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE "UNABLE TO ASSIGN MEDIA FILE TO BATCH" TO MESS
              PERFORM SEND-MESS
              GO TO SET-UNASSIGNED-BATCH-EXIT.

           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "MEDIA FILE ALREADY ASSIGNED" TO MESS
              PERFORM SEND-MESS
              GO TO SET-UNASSIGNED-BATCH-EXIT.

           MOVE "END" TO IO-TYPE.
       SET-UNASSIGNED-BATCH-EXIT.
           EXIT.

      **********************************************
       SET-EXISTING-BATCH SECTION.
      **********************************************
           MOVE 0 TO SUB.
           MOVE " " TO IO-TYPE.
       SET-EXISTING-BATCH-AGAIN.
           ADD 1 TO SUB.
           IF SUB > 18
              MOVE "BATCH HAS NOT BEEN ESTABLISHED FOR TODAY" TO MESS
              PERFORM SEND-MESS
              MOVE 0 TO BATCH
              GO TO SET-EXISTING-BATCH-EXIT.

           IF BATCH-ELE(SUB) NOT = BATCH
              GO TO SET-EXISTING-BATCH-AGAIN.

           MOVE "S  A050BATCHREFCD." TO VDU.
           MOVE BATCH-ELE-REFCD(SUB) TO VDUBUF BATCH-REFCD.
           PERFORM SCREENIO.

       SET-EXISTING-BATCH-ACCEPT.
           MOVE "CONFIRM SETTING THIS BATCH FOR PROCESSING (YES/NO)?"
                                                         TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM ALARM.
           PERFORM ALARM.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO SET-EXISTING-BATCH-ACCEPT.
           MOVE VDUBUF TO ACTION.

           IF ACTION = "    NO"
              MOVE 0 TO BATCH
              GO TO SET-EXISTING-BATCH-EXIT.
           IF ACTION NOT = "   YES"
              GO TO SET-EXISTING-BATCH-ACCEPT.

           IF BATCH >= 100
              MOVE GLOBAL-SEQ TO PASSWDW-SEQ
              MOVE "TO PROCESS GLOBAL BATCHES" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE " CANNOT PROCESS GLOBAL BATCH FILES" TO MESS
                 PERFORM SEND-MESS
                 GO TO SET-EXISTING-BATCH-ACCEPT.

           MOVE "END" TO IO-TYPE.
       SET-EXISTING-BATCH-EXIT.
           EXIT.

      **********************************************
       SET-NEW-BATCH-NO SECTION.
      **********************************************

      * GET NEXT AVAILABLE BATCH NO:

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "S  N030BATCHNO." TO VDU.
           IF BR-BP-NEXT-BATCH = 0
              ADD 1 TO BR-BP-NEXT-BATCH.
           IF BR-BP-NEXT-BATCH > 99
              MOVE 1 TO BR-BP-NEXT-BATCH.
           MOVE BR-BP-NEXT-BATCH TO VDUNUM0 BATCH.
           PERFORM SCREENIO.

           MOVE "S  A050BATCHREFCD." TO VDU.
           MOVE " "                  TO VDUBUF BATCH-REFCD.
           PERFORM SCREENIO.

       SET-NEW-BATCH-NO-ACCEPT.
           MOVE " " TO IO-TYPE.
           MOVE "CONFIRM SETUP OF NEW BATCH FILE TODAY (YES/NO)?"
                                                         TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS NOT = "00"
              GO TO SET-NEW-BATCH-NO-ACCEPT.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "    NO"
              MOVE 0 TO BATCH
              GO TO SET-NEW-BATCH-NO-EXIT.
           IF ACTION NOT = "   YES"
              GO TO SET-NEW-BATCH-NO-ACCEPT.

           IF BATCH >= 100
              MOVE GLOBAL-SEQ TO PASSWDW-SEQ
              MOVE "TO PROCESS GLOBAL BATCHES" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE " CANNOT PROCESS GLOBAL BATCH FILES" TO MESS
                 PERFORM SEND-MESS
                 GO TO SET-NEW-BATCH-NO-ACCEPT.

      * ESTABLISH NEXT FREE BATCH NO:
           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           IF BR-BP-NEXT-BATCH NOT = BATCH
              MOVE "S  N030BATCHNO." TO VDU
              IF BR-BP-NEXT-BATCH = 0
                 ADD 1 TO BR-BP-NEXT-BATCH
              END-IF
              IF BR-BP-NEXT-BATCH > 99
                 MOVE 1 TO BR-BP-NEXT-BATCH
              END-IF
              MOVE BR-BP-NEXT-BATCH TO VDUNUM0 BATCH
              PERFORM SCREENIO.

           ADD 1 TO BR-BP-NEXT-BATCH.
           IF BR-BP-NEXT-BATCH > 99
              MOVE 1 TO BR-BP-NEXT-BATCH.
           PERFORM REWRITE-BR-FILE.
           PERFORM CLOSE-BR-FILE.


      * CREATE NEW BATCH FILE:
           MOVE BRANCH TO BP-ALL-BRANCH.
           PERFORM SET-BP-DATE.
           MOVE TRANS-MMDD TO BP-ALL-DATE.
           MOVE BATCH TO BP-ALL-BATCH-NO.
           PERFORM OPEN-BP1-FILE-OUTPUT.
           PERFORM CLOSE-BP1-FILE.

      * WRITE HEADER RECORD FOR 'INPUT'
           PERFORM OPEN-BP1-FILE.
           MOVE SPACES TO BP-REC.
           MOVE 0 TO BP-LNNO BP-TOTAL-COUNT BP-TOTAL-AMOUNT.
           MOVE 1 TO BP-SEQNO.
           MOVE "I" TO BP-TYPE.
           PERFORM WRITE-BP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "END" TO IO-TYPE.
       SET-NEW-BATCH-NO-EXIT.
           EXIT.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LONPW1" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE LPLOAN-SYSDATE TO LONPW-SYSDATE.
           MOVE EXT-CONAME-CONAME TO LONPW-CONAME.
           DISPLAY LONPW-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPW-FORM-FIELDS TO WR-BUF.
           MOVE LONPW-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE LONPW-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONPW-HELP--DIR TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPW_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       MISC-PARAGRAPHS SECTION.
      *******************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SET-BP-DATE.
           MOVE TRANS-DATE TO NUM-DATE.
           MOVE 0          TO NUM-CCYY.
           MOVE NUM-DATE   TO TRANS-MMDD.
       SEND-MESSAGE.
           MOVE MESS TO WR-BUF.
           MOVE MESSAGE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPBP1IN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/GBBRIN.CPY".
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

       COPY "LIBGB/FERRORS.CPY".

      *********************************
       END-ROUTINE SECTION.
      *********************************

      * THIS SHARES THE SAME FORM AS THE CALLING PROGRAM BUT REDISPLAYS A
      * DIFFERENT SET OF PROMPTS, THIS WAS THE ONLY WAY I COULD GET IT TO
      * CLEAR AND REDISPLAY LONPW0 MENU WHEN IT EXITED... BAH 5/17/2013
           DESTROY ALL CONTROLS
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
