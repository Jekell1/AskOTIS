       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EOQRPT.
       DATE-WRITTEN.    8/1/88.
      *****************************************************
      *  DIR : SP
      *  SPOOL "ALL" IS ONLY FOR THAT PAGE, MUST DO ALL PAGES
      *  DESC: BATCH REPORT QUEUE/RELEASE/TRANSFER
      * REV:
      *  070694 BAH ADDED PIGGYBACK PRINT OPTION "I"
      *  110395 BAH STOP PRINTING ANYTHING WITH STEP = "0000", AS PER
      *             BRINCAT NOT WANTING BRANCHES TO SEE RECAP
      *  MJD 112995 ADDED VIEW REPORT OPTION.
      *  BAH 072696 ADDED GP-PRINT-GL-RECAP FOR INSTANT TO PRINT THE
      *             RECAP THAT BRINCAT WOULDNT LET MERCURY PRINT !!
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  BLV 052098 CHECK FOR 0000ERF TO STOP RELEASE, NOT 0000ERR
      *  BLV 060198 CHECK FOR PASSWORD ON BATCH TO PRINT/RELEASE
      *             BATCH (PASSWD KEY = SP/BATCHID/1)
      *  BAH 000425 CHANGED UUCP-COMMAND AND UUCP-END-COMMAND BECAUSE IF YOUR
      *             HOME OFFICE NAME WAS 7 CHARACTERS, IT WAS LEAVING A
      *             SPACE BETWEEN NAME AND !, CAUSING AN ERROR, CARFIN1
      *  CS  010530 PASSWORD TO RELEASE BATCH SP/BATCHID/2 BF MOTOR PR#1494
      *  BAH 051205 REPLACED THE SYSTEM-CALL WITH THE ACU SYSTEM CALL
      *             OR PERUSE WOULD NOT WORK, IT WOULD GIVE YOU ^M AND
      *             YOU WOULD HAVE TO USE THE CONTROL J METHOD.
      *             THIS IS DUE TO THE TCLP_NODASHB OPTION IN OUR
      *             LOCALPROFILE TO START USING ACCEPT/DISPLAY IN THE
      *             NEW RELEASES....
      *  BLM 100507 ADDED GETENV ON OPSYS TO FLAG IF RUNNING LINUX.  THE LS -S
      *             COMMAND YIELDS DIFFERENT RESULTS IN LINUX, FOR EXAMPLE:
      *
      *             NOTE, THE NUMBER OF BLOCKS IN LINUX IS BASED ON THE
      *             NUMBER OF BLOCKS WITH THE MOST DIGITS, FOR EXAMPLE:
      *
      *            SO IF LINUX, I LEFT JUSTFIED, DID AN UNSTRING TO WORKING
      *            STORAGE FIELDS & MOVE THE FIELDS BACK INTO LS-REC.  ALSO,
      *            DON'T UNDERSTAND THE NUMBER OF BLOCKS IN LINUX.  A FILE WITH
      *            ZERO BYTES GIVES 4 BLOCKS ON DU OR LS -S, SO I FORCED 0 TO
      *            THE NUMBER OF BYTES.  IF A FILE HAS 1 BYTES, IT SAYS 8
      *            BLOCKS.  LOOKING A BIGGER FILES, IT SEEMS LIKE THE BLOCK
      *            SIZE IS 1024, SO I USED THAT FACTOR FOR LINUX. LINUX PR# 0001
      *  BAH 130809 PROBLEM WITH LS -S GIVING LARGER BYTES RESULTS THAN WHAT
      *             OUR LS-BYTES HANDLES, BUT CANT CHANGE THE RECORD BECAUSE
      *             THE STUPID COMMAND ONLY SHIFTS THE LARGER LINES!
      *             SO THE EMTRRG SHIFTS AND DISPLAYS WRONG. CHANGE TO TEST
      *             IF THE FILLER BETWEEN 4 DIGIT BYTE AND PATHNAME IS NOT
      *             SPACES, USE THE 5BYTE-LS-REC TO DISPLAY CORRECTLY AND COUNT
      *             BYTES,SECURITY AUTO PL# 807
      * BAH 130812 JIM ASKED ME TO FIX FOR WORLD, UP TO A 6 DIGIT COUNT
      *            SAME PROBLEM AS ABOVE
      * BAH 130821 MESSED UP LINUX, NEEDED TO CLEAR THE LS-REC BEFORE MOVING
      *            THE LINUX-LS-BYTES AND PATHNAME BACK INTO IT... PL#810
      * BAH 130905 REMOVED THE MOVE 0 TO BYTES IF BLOCK = 4 FOR LINUX
      *            SUPPORT PL#811
      *  CS 131226 ADDED EOQRPT2-WINDOW-HANDLE TO DISPLAY REPORT, WHEN I TRIED
      *            TO F7 FROM THE SCREEN, IT KEPT THE VIEW REPORTS SCREEN BUT
      *            PUT THE CURSOR WHERE IT WOULD BE IN EOMENU.  IF I PRESSED
      *            F7 AGAIN, I COULD SEE THE 'ALTERNATE BRANCH LOGIN' DISPLAY
      *            INTERMIXED WITH THE VIEW REPORTS SCREEN
      * BLM 140505 PERFORM SYSTEM-CALL INSTEAD OF CALL C$SYSTEM, VERYANT CHANGE
      * BLM 140506 CHANGED BACK TO C$SYSTEM, DIFFERENT CALL & ONLY USING
      *            SYSTEM-BUF, NOT SYSTEM-BUFFER (SO NO >/DEV/NULL).
      *            USING CALL "SYSTEM" BOTH WITH & WITHOUT DEV/NULL ARGUMENT
      *            DOESN'T WORK, PRINTS 80 CHARACTERS INSTEAD OF 132 &
      *            PRINTS GARBAGE CHARACTERS
      *  20170905 BAH LEAVE ALL DATES 6 IN HERE! ALL REFERENCES TO DATES ARE
      *               THE BATCH DIRECTORY WHICH IS 6
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LS-FILE ASSIGN TO MKTEMP-BUF
              ORGANIZATION LINE SEQUENTIAL
              FILE STATUS IS STAT.
           SELECT DIR-FILE ASSIGN TO DIR-PATH
              ORGANIZATION LINE SEQUENTIAL
              FILE STATUS IS STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       FD  LS-FILE   LABEL RECORDS ARE STANDARD.
       01  LS-REC.
           03  LS-BYTES        PIC X(4).
           03  LS-FILLER       PIC X(1).
      * /USR/DATA/R1/GA0001/EO/TSTMMDDYY/9999LNAGED
           03  LS-PATHNAME     PIC X(43).
           03  LS-PATH-REDEF      REDEFINES LS-PATHNAME.
               05  LS-PATHNAME-1  PIC X.
               05  FILLER         PIC X(22).
               05  LS-PCODE       PIC XXX.
               05  LS-DATE        PIC 9(6).
               05  LS-DATE-X      REDEFINES LS-DATE.
                   07  LS-MM      PIC 99.
                   07  LS-DD      PIC 99.
                   07  LS-YY      PIC 99.
               05  FILLER         PIC X.
               05  LS-RPTNAME.
                   07  LS-STEP    PIC 9999.
                   07  LS-STEP-X  REDEFINES LS-STEP
                                  PIC X(4).
                   07  LS-NAME    PIC X(6).
           03  FILLER             PIC XX.


       FD  DIR-FILE   LABEL RECORDS ARE STANDARD.
       01  DIR-REC.
           03  FILLER          PIC X(23).
           03  DIR-PCODE       PIC X(3).
           03  DIR-DATE        PIC 9(6).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/EOQRPT.CBL S34 06/19/21-12:02:35 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01EO.CPY".
       COPY "LIBGB/GB01EO_SQL.CPY".
       COPY "LIBGB/GBWSEO.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       01  LS-IFN          PIC X(2)    VALUE "LS".
       01  DIR-IFN         PIC X(3)    VALUE "DIR".
       01  DIR-PATH        PIC X(35)   VALUE SPACES.

       01  EOQRPT-WINDOW-HANDLE PIC X(10).
       01  EOQRPT2-WINDOW-HANDLE PIC X(10).
       01  ERRCD           PIC X       VALUE SPACES.
       01  SUB             PIC 999     VALUE 0.

       01  5BYTE-OR-6BYTE-FLAG    PIC X.
       01  5BYTE-LS-REC.
           03  5BYTE-LS-BYTES     PIC X(5).
           03  5BYTE-LS-FILLER    PIC X.
           03  5BYTE-LS-PATHNAME  PIC X(43).
       01  6BYTE-LS-REC.
           03  6BYTE-LS-BYTES     PIC X(6).
           03  6BYTE-LS-FILLER    PIC X.
           03  6BYTE-LS-PATHNAME  PIC X(43).

       01  LINUX-LS-REC.
           03  LINUX-LS-BYTES      PIC X(4).
           03  LINUX-LS-PATHNAME   PIC X(43).

       01  BLOCKS-TO-BYTES-FACTOR  PIC 999 VALUE 512.
       01  BYTES           PIC 9(8).
       01  DU-FLAG         PIC X.
       01  DU-TYPE         PIC X.

       01  REMOVE-PATH.
           03  REMOVE-PATH-BASE          PIC X(09).
           03  FILLER                    PIC X(01)   VALUE "/".
           03  REMOVE-PATH-MACHINE       PIC X(02).
           03  FILLER                    PIC X(01)   VALUE "/".
           03  REMOVE-PATH-DATA          PIC X(06).
           03  FILLER                    PIC X(04)   VALUE "/EO/".
           03  REMOVE-PATH-PCODE         PIC X(03).
           03  REMOVE-PATH-PDATE         PIC 9(06).
           03  REMOVE-PATH-FILE          PIC X(11).


       01  PATH-TABLE.
           03  PATH-NAMES  OCCURS 30 TIMES.
               05  PATH-NAME       PIC X(43).
      * /USR/DATA/R1/GA0001/EO/TST051413/0001LNAGED
               05  PATH-NM2        REDEFINES PATH-NAME.
                   07  FILLER      PIC X(28).
                   07  PATH-DAY    PIC 9(2).
                   07  FILLER      PIC X(3).
                   07  PATH-FILE   PIC X(10).
                   07  PATH-FILEX REDEFINES PATH-FILE.
                       09  PATH-STEP PIC X(4).
                       09  FILLER    PIC X(6).
               05  PATH-COPIES     PIC 9.

       01  PCODE           PIC XXX VALUE SPACES.
       01  PDATE           PIC 9(8).
       01  SAVE-PCODE      PIC XXX VALUE SPACES.
       01  PRINT-REL       PIC X.
       01  REPORT-NO-X.
           03  REPORT-NO   PIC 999.

       01  PCODE-DISPLAY-WORKERS.
           03  ACCUM-BYTES       PIC 9(8).
           03  ACCUM-COUNT       PIC 999.
           03  RJ-NUM            PIC 9(10).
           03  RJ-IN             REDEFINES RJ-NUM
                                 PIC X(10).
           03  RJ-WORK           REDEFINES RJ-NUM.
               05  RJ-FIRST      PIC X(9).
               05  RJ-LAST       PIC X.
           03  RJ-WORK2          REDEFINES RJ-NUM.
               05  RJ-ZERO       PIC X.
               05  RJ-SHIFT      PIC X(9).
           03  RJ-SWAP           PIC X(9).

       01  CASE-BUF                 PIC X(4).
       01  OPSYS-BUF                PIC X(4) VALUE SPACES.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  MESS-LIST       PIC X(5)   VALUE "MESS.".
       01  D-ACTION        PIC X(9)   VALUE SPACES.
       01  ACTION-LIST     PIC X(8)   VALUE "ACTION2.".
       01  ACTION-LIST-R   PIC X(10)  VALUE "ACTION2:R.".
       01  FLD-LIST        PIC X(5)   VALUE "FLD.".
       01  HEADING-LIST    PIC X(12)  VALUE "HD1 HD2 HD3.".
       01  DATE-LIST       PIC X(5)   VALUE "DATE.".
       01  CODE-HEADING.
           03  FILLER      PIC X(20)  VALUE "PROC  #DIRS".
           03  FILLER      PIC X(20)  VALUE "PROC  #DIRS".
           03  FILLER      PIC X(20)  VALUE "PROC  #DIRS".
       01  DATE-HEADING.
           03  FILLER      PIC X(20)  VALUE "    DATE".
           03  FILLER      PIC X(20)  VALUE "    DATE".
           03  FILLER      PIC X(20)  VALUE "    DATE".
       01  REPORT-HEADING.
           03  FILLER      PIC X(20)  VALUE "STEP NAME     #BYTES".
           03  FILLER      PIC X(20)  VALUE "STEP NAME     #BYTES".
           03  FILLER      PIC X(20)  VALUE "STEP NAME     #BYTES".

       01  DISPLAY-BUF          VALUE SPACES.
           03  D-FIELD          OCCURS 30 TIMES.
             04  D-FLD04.
               05  D-STNAM.
                   07  D-STEP   PIC ZZZ9-.
                   07  D-NAME   PIC X(6).
                   07  FILLER   PIC X.
               05  D-DATE-AREA REDEFINES D-STNAM.
                   07  FILLER   PIC X(2).
                   07  D-DATE   PIC 99/99/99.
                   07  FILLER   PIC X(2).
               05  D-BYTES      PIC Z(8).
             04  D-PCODE-INFO REDEFINES D-FLD04.
               05  FILLER         PIC X.
               05  D-PCODE        PIC XXXX.
               05  D-PCODE-COUNT  PIC ZZZ9-.
               05  D-PCODE-BYTES  PIC Z(8)9-.

           03  D-LABEL OCCURS 30 TIMES.
               05  D-NO    PIC Z9.
               05  D-DOT   PIC X.
       03  D-END-MORE      PIC X(10).

       01  DISPLAY-LIST.
           03  FILLER   PIC X(20)  VALUE "01F 02F 03F 04F 05F ".
           03  FILLER   PIC X(20)  VALUE "06F 07F 08F 09F 10F ".
           03  FILLER   PIC X(20)  VALUE "11F 12F 13F 14F 15F ".
           03  FILLER   PIC X(20)  VALUE "16F 17F 18F 19F 20F ".
           03  FILLER   PIC X(20)  VALUE "21F 22F 23F 24F 25F ".
           03  FILLER   PIC X(20)  VALUE "26F 27F 28F 29F 30F ".
           03  FILLER   PIC X(20)  VALUE "01P 02P 03P 04P 05P ".
           03  FILLER   PIC X(20)  VALUE "06P 07P 08P 09P 10P ".
           03  FILLER   PIC X(20)  VALUE "11P 12P 13P 14P 15P ".
           03  FILLER   PIC X(20)  VALUE "16P 17P 18P 19P 20P ".
           03  FILLER   PIC X(20)  VALUE "21P 22P 23P 24P 25P ".
           03  FILLER   PIC X(20)  VALUE "26P 27P 28P 29P 30P ".
           03  FILLER   PIC X(8)   VALUE "ENDMORE.".

      *
      *   COMMANDS FOR QUEUE/RELEASE REPORTS PROGRAM
      *
      * "WC -L ".
       01  LS-DIRECTORY-CMD.
      * "LS -D ".
           03  FILLER             PIC X(06)  VALUE X'6C73202D6420'.
           03  LL-FILPATH-BASE    PIC X(09).
           03  FILLER             PIC X(01)  VALUE "/".
           03  LL-FILPATH-MACHINE PIC X(02).
           03  FILLER             PIC X(01)  VALUE "/".
           03  LL-FILPATH-DATA    PIC X(06).
           03  FILLER             PIC X(04)  VALUE "/EO/".
           03  LL-PCODE           PIC X(03).
           03  FILLER             PIC X(03)  VALUE "* >".
           03  LL-TEMPFILE        PIC X(35).

       01  DU-REPORTS-CMD.
      * "LS -S ".
           03  DR-CMD            PIC X(6)   VALUE X'6C73202D7320'.
           03  DR-DIRPATH.
               05  DR-FILPATH-BASE     PIC X(09).
               05  FILLER              PIC X(01)  VALUE "/".
               05  DR-FILPATH-MACHINE  PIC X(02).
               05  FILLER              PIC X(01)  VALUE "/".
               05  DR-FILPATH-DATA     PIC X(06).
               05  FILLER              PIC X(04)  VALUE "/EO/".
               05  DR-PCODE            PIC X(03).
               05  DR-DATE             PIC 9(06).
           03  FILLER            PIC X(4)   VALUE "/* >".
           03  DR-TEMPFILE       PIC X(35).

       01  REMOVE-ALL-CMD.
      * "RM -R ".
           03  FILLER             PIC X(06)  VALUE X'726D202D7220'.
           03  RA-FILPATH-BASE    PIC X(09).
           03  FILLER             PIC X(01)  VALUE "/".
           03  RA-FILPATH-MACHINE PIC X(02).
           03  FILLER             PIC X(01)  VALUE "/".
           03  RA-FILPATH-DATA    PIC X(06).
           03  FILLER             PIC X(04)  VALUE "/EO/".
           03  RA-PCODE           PIC X(03).
           03  RA-DATE            PIC 9(6).

       01  PERUSE-COMMAND.
      * "PERUSE ".
           03  FILLER            PIC X(7) VALUE X'70657275736520'.
           03  PERUSE-REPORT     PIC X(43).
            
      *****************************************************************
       01  UUCP-COMMAND.
      * "UUCP ".
           03  FILLER       PIC X(05)  VALUE X'6565637020'.
           03  UUCP-NAME    PIC X(50).
           03  UUCP-DEST    PIC X(50).

       01  UUCP-END-COMMAND.
      * "!/USR/PD/".
           03  UUCP-TILDA   PIC X(9)   VALUE X'212F7573722F70642F'.
           03  UUCP-DAY     PIC 9(2).
           03  FILLER       PIC X      VALUE "/".
           03  UUCP-BRNO    PIC 9(4).
           03  UUCP-FILE    PIC X(10).

       01  UUCP-END-COMMAND-TRW.
           03  UUCP-TILDA   PIC X(11)   VALUE "!~/TRWFILE.".
           03  UUCP-TRW-BRNO PIC 9(4).

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GRWORKW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       01  PASSPRT-COMMAND.
           03  FILLER                PIC X(8) VALUE "PASSPRT ".
           03  PASSPRT-FILENAME      PIC X(1320).

       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78    RELEASE-BATCH-SEQ    VALUE 2.
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".

       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/EOQRPT_DEF.CPY".
       COPY "LIBSP/EOQRPT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".


       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/EOQRPT_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               LS-FILE DIR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-EO1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               LS-FILE DIR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-PROGRAM SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EOQRPT" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
      *            CELL WIDTH 10
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS EOQRPT-WINDOW-HANDLE.

           PERFORM FORM-RESET.


           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO LL-TEMPFILE DIR-PATH DR-TEMPFILE.

      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
           PERFORM GET-GPENV.

      * TEST IF LINUX OR AIX OR SCO
           MOVE "OPSYS" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-BAD
              MOVE SPACES TO GETENV-BUF.
           MOVE GETENV-BUF TO OPSYS-BUF.

       DISP-PCODES.
           MOVE "???" TO LL-PCODE.
           MOVE EXT-FILPATH-BASE TO LL-FILPATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO LL-FILPATH-MACHINE.
           MOVE EXT-FILPATH-DATA TO LL-FILPATH-DATA.
      * LS-D /USR/DATA/R1/XX0001/EO/>>>* >/USR/TMP/XXXXX
           MOVE LS-DIRECTORY-CMD TO SYSTEM-BUF.
           MOVE "I" TO DU-FLAG.
           MOVE "C" TO DU-TYPE.

           PERFORM SP-CLEAR-DISPLAY.

           MOVE CODE-HEADING TO WR-BUF.
           MOVE HEADING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * DISPLAYS ALL SPL??? DIRECTORIES AVAILABLE
           PERFORM DISPLAY-DIRECTORY.

       REC-PCODE.
           MOVE "ENTER PROCEDURE CODE TO PROCESS, (F7)-EXIT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A030PCODE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-PROGRAM.
           IF VDUSTATUS NOT = "00"
              GO TO REC-PCODE.
           MOVE VDUBUF TO PCODE EO-PCODE.
           MOVE ZERO TO EO-STEP.
           PERFORM OPEN-EO1-FILE.
           PERFORM READ-EO1-FILE.
           PERFORM CLOSE-EO1-FILE.
           IF IO-FG = 9
              GO TO REC-PCODE.

           MOVE "S  A000PDESC:R." TO VDU.
           MOVE EO-COMMENT TO VDUBUF.
           PERFORM SCREENIO.

       CHECK-BATCH-PASSWORD.
           MOVE PCODE TO PASSWDW-PGM.
           MOVE 1 TO PASSWDW-SEQ.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE
           "ACCESSING THIS BATCH IS NOT ALLOWED WITHOUT PASSWORD!!!"
                                                TO MESS
              PERFORM SEND-MESS
              GO TO REC-PCODE.

      * YOU CANT ENTER EO-LEVEL OTHER THAN NUMERIC IN MAINTENANCE
      * THIS WAS FOR WHEN IT WAS ADDED MIDSTREAM. WORLD IS REBUILDING ALL
      * THEIR BATCHES IN A30 SO IT WILL BE NUMERIC. 
      *    IF EO-LEVEL-X NOT NUMERIC
      *       GO TO GET-AVAIL-DATES.
           IF EO-LEVEL = 9
              GO TO GET-AVAIL-DATES.

           MOVE EXT-CONAME-USER-GROUP TO CASE-BUF.
           CALL "C$TOUPPER" USING CASE-BUF, VALUE 4.
           MOVE CASE-BUF TO WGR-WORD.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              GO TO PASSWORD-REQUIRED.
           IF WGR-LEVEL NOT > EO-LEVEL
              GO TO GET-AVAIL-DATES.

       PASSWORD-REQUIRED.
           MOVE "STATS" TO PASSWDW-PGM.
           MOVE EO-LEVEL TO PASSWDW-SEQ.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE
           "YOU ARE NOT AUTHORIZED TO ACCESS THIS BATCH!!"
                                                TO MESS
              PERFORM SEND-MESS
              GO TO REC-PCODE.

       GET-AVAIL-DATES.
           MOVE PCODE TO LL-PCODE.
           MOVE LS-DIRECTORY-CMD TO SYSTEM-BUF.

           MOVE "I" TO DU-FLAG.
           MOVE "D" TO DU-TYPE.

           PERFORM CLEAR-DISPLAY.

           MOVE DATE-HEADING TO WR-BUF.
           MOVE HEADING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * DISPLAYS ALL SPL(PCODE) DIRECTORIES AVAILABLE
           PERFORM DISPLAY-DIRECTORY.

       REC-DATE.
           MOVE "F1, ENTER DATE OF REPORTS TO PROCESS:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" OR VDUSTATUS = "10"
              GO TO DISP-PCODES.
           IF VDUSTATUS = "1>"
              GO TO END-PROGRAM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO REC-DATE.
           IF VDUNUM0 = ZERO
              PERFORM DISPLAY-DIRECTORY
              GO TO REC-DATE.
           MOVE VDU-DATE-YYYYMMDD TO PDATE.

           MOVE EXT-FILPATH-BASE TO DR-FILPATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO DR-FILPATH-MACHINE.
           MOVE EXT-FILPATH-DATA TO DR-FILPATH-DATA.
           MOVE PCODE TO DR-PCODE.
           MOVE PDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO DR-DATE.
           MOVE DR-DIRPATH TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              GO TO REC-DATE.

      * LS -S
           MOVE EXT-FILPATH-BASE TO DR-FILPATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO DR-FILPATH-MACHINE.
           MOVE EXT-FILPATH-DATA TO DR-FILPATH-DATA.
           MOVE DU-REPORTS-CMD TO SYSTEM-BUF.

           PERFORM DIRECTORY-SCAN.

           MOVE "R" TO DU-TYPE.

           PERFORM CLEAR-DISPLAY.

           MOVE REPORT-HEADING TO WR-BUF.
           MOVE HEADING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


           PERFORM DISPLAY-SCROLL.

       ENTER-PRINT-OR-REL.
           MOVE
           "F1, P-PRINT,R-RELEASE,T-TRANSFER,I-PIGGYBACK, (RTN)-SCROLL:"
                TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010ACTION." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-PROGRAM.
           IF VDUSTATUS = "10"
              GO TO DISP-PCODES.
           IF VDUSTATUS = "1U"
              MOVE SPACES TO WR-BUF
              MOVE ACTION-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO GET-AVAIL-DATES.
           IF VDUSTATUS NOT = "00" GO TO ENTER-PRINT-OR-REL.
           MOVE VDUBUF TO PRINT-REL.
           IF PRINT-REL = " "
              PERFORM DISPLAY-SCROLL
              GO TO ENTER-PRINT-OR-REL.
           IF NOT (PRINT-REL = "P" OR "R" OR "T" OR "I" OR "V")
              GO TO ENTER-PRINT-OR-REL.
           IF PRINT-REL = "V"
              MOVE "VIEW" TO D-ACTION
              PERFORM SEND-ACTION
              GO TO REC-RPTNO.
           IF PRINT-REL = "R"
              MOVE "RELEASE" TO D-ACTION
              PERFORM SEND-ACTION
              MOVE  PCODE  TO PASSWDW-PGM
              MOVE RELEASE-BATCH-SEQ  TO PASSWDW-SEQ
              PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
              MOVE
           "RELEASING THIS BATCH IS NOT ALLOWED WITHOUT PASSWORD!!!"
                                                TO MESS
                    PERFORM SEND-MESS
                    GO TO  ENTER-PRINT-OR-REL
                 ELSE
                    GO TO REC-RPTNO.
           IF PRINT-REL = "T"
              MOVE "TRANSFER" TO D-ACTION
              PERFORM SEND-ACTION

              PERFORM OPEN-GB-FILE
              MOVE GB-PATH-OWNBR TO GB-BRNO
              MOVE 1             TO GB-NO
              PERFORM READ-GB-FILE
              PERFORM CLOSE-GB-FILE
              IF GB-UUCP-HOME = SPACES
                 MOVE "HOME MACHINE NOT SET IN GLOBAL" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-PRINT-OR-REL
              ELSE
                 GO TO REC-RPTNO.

           IF PRINT-REL = "I"
              MOVE "PIGGYBACK" TO D-ACTION
              PERFORM SEND-ACTION
              GO TO REC-RPTNO.

       SETPR.
           MOVE "PRINT" TO D-ACTION.
           PERFORM SEND-ACTION.
           MOVE "^, ENTER PRINTER #:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "Y" TO STAT.
           MOVE 000000 TO Q-POS C-POS.
           MOVE "N"    TO Q-BUF.
           MOVE "01"   TO C-BUF.
           MOVE EOQRPT-PRU-POS TO P-POS.
           PERFORM PRU-IN.
           IF STAT = "1U"
              GO TO ENTER-PRINT-OR-REL.
           IF STAT NOT = "00"
              GO TO SETPR.

       REC-RPTNO.
           IF PRINT-REL = "P" OR "I"
            MOVE "ENTER REPORT # OR 'ALL' TO PRINT, (RTN)-SCROLL:"
              TO MESS
           ELSE
              IF PRINT-REL = "T"
              MOVE "F7-EXIT, ENTER REPORT # TO TRANSFER, (RTN)-SCROLL:"
                   TO MESS
           ELSE
              IF PRINT-REL = "V"
              MOVE "F7-EXIT, ENTER REPORT # TO VIEW, (RTN)-SCROLL:"
                   TO MESS
           ELSE
            MOVE "ENTER REPORT # OR 'ALL' TO RELEASE, (RTN)-SCROLL:"
                TO MESS.
           PERFORM SEND-LEGEND.
       GET-RPTNO.
           MOVE "E RA030FLD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-PROGRAM.
           IF VDUSTATUS = "1U"
              IF PRINT-REL = "P"
                 GO TO SETPR
              ELSE
                 GO TO ENTER-PRINT-OR-REL.
           IF VDUSTATUS = "10" GO TO GET-AVAIL-DATES.
           IF VDUSTATUS NOT = "00" GO TO GET-RPTNO.

           MOVE VDUBUF TO REPORT-NO-X.
           IF REPORT-NO-X = " "
              PERFORM DISPLAY-SCROLL
              GO TO GET-RPTNO.

           IF REPORT-NO-X = "ALL"
              IF PRINT-REL = "V"
                 MOVE "CANNOT VIEW ALL REPORTS" TO MESS
                 PERFORM SEND-MESS
                 GO TO GET-RPTNO
              ELSE
              IF PRINT-REL = "T"
                 MOVE "CANNOT TRANSFER ALL REPORTS" TO MESS
                 PERFORM SEND-MESS
                 GO TO GET-RPTNO
              ELSE
                 IF PRINT-REL = "R"
                    GO TO RELEASE-ALL-REPORTS
                 ELSE
                   IF PRINT-REL = "I"
                      GO TO PIGGYBACK-ALL-REPORTS
                   ELSE
                      GO TO PRINT-ALL-REPORTS.

           INSPECT REPORT-NO-X REPLACING LEADING SPACES BY ZEROES.
           IF REPORT-NO-X IS NOT NUMERIC
              GO TO GET-RPTNO.
           IF REPORT-NO < 1  OR REPORT-NO > 30
              GO TO GET-RPTNO.
           IF PATH-NAME(REPORT-NO) = SPACES
              GO TO GET-RPTNO.
           IF PRINT-REL = "R"
              GO TO RELEASE-REPORT.
           IF PRINT-REL = "T"
              GO TO TRANSFER-REPORT.
           IF PRINT-REL = "P"
              GO TO PRINT-REPORT.
           IF PRINT-REL = "V"
              GO TO VIEW-REPORT.

       PIGGYBACK-REPORT.
           MOVE "REPORT IS PIGGYBACK PRINTING......" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM PIGGYBACK-REPORT-FILES.
           GO TO REC-RPTNO.

      *----------------------------------------------------------------
      * VIEW REPORT:
      *    CALLS PERUSE VIA SYSTEM CALL.
      *    PERUSE-REPORT AND PERUSE-COMMAND ARE DEFINED IN LIBGB/EOQRPT.CMD
      *----------------------------------------------------------------
       VIEW-REPORT.
      *    PERFORM ENDWIN-CALL.

      * OPEN A DUMMY BLANK WINDOW TO DISPLAY THE REPORT IN
           DISPLAY FLOATING GRAPHICAL WINDOW
                      AT SCREEN LINE 01 * VDU-WIN-L-OFFSET
                      AT SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE 80 + VDU-WIN-W-MOD
                      LINES 24 + VDU-WIN-H-MOD
                      BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                      HANDLE IS EOQRPT2-WINDOW-HANDLE.

           MOVE PATH-NAME(REPORT-NO) TO PERUSE-REPORT.
           MOVE PERUSE-COMMAND TO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99.

           DESTROY EOQRPT2-WINDOW-HANDLE.
      *    DESTROY EOQRPT-WINDOW-HANDLE.

           GO TO REC-RPTNO.

       PRINT-REPORT.
           MOVE "SPOOLING REPORT........... " TO MESS.
           PERFORM SEND-LEGEND.

           MOVE PATH-NAME(REPORT-NO) TO PRU-FILE.

      *  COPIES ARE NOT WORKING! DONT KNOW HOW TO PASS IT
      *    MOVE PATH-COPIES(REPORT-NO) TO WR-BUF.
      *    MOVE "COPIES." TO WR-LIST.
      *    PERFORM CALL-WRFORM.

           PERFORM PRU-OUT.

           GO TO REC-RPTNO.

       RELEASE-REPORT.
      * TEST ACCESS ON ERROR FILE, DONT ALLOW A REMOVE IF IT EXISTS
           MOVE EXT-FILPATH-BASE    TO REMOVE-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO REMOVE-PATH-MACHINE.
           MOVE EXT-FILPATH-DATA    TO REMOVE-PATH-DATA.
           MOVE PCODE               TO REMOVE-PATH-PCODE.
           MOVE PDATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO REMOVE-PATH-PDATE.
           MOVE "/0000ERF   "       TO REMOVE-PATH-FILE.
           MOVE REMOVE-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE "CANT RELEASE, ERR REPORT EXISTS" TO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE "RELEASING REPORT........... " TO MESS
              PERFORM SEND-LEGEND
              CALL "CBL_DELETE_FILE" USING PATH-NAME(REPORT-NO)
                                GIVING STAT-99
              MOVE SPACES TO PATH-NAME(REPORT-NO) D-FIELD(REPORT-NO)
                    D-LABEL(REPORT-NO)
      *       MOVE 0 TO PATH-COPIES(REPORT-NO)
              MOVE DISPLAY-BUF TO WR-BUF
              MOVE DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE DU-REPORTS-CMD TO SYSTEM-BUF.
           MOVE "I" TO DU-FLAG.
           MOVE "R" TO DU-TYPE.
           GO TO REC-RPTNO.

       RELEASE-ALL-REPORTS.
      * TEST ACCESS ON ERROR FILE, DONT ALLOW A REMOVE IF IT EXISTS
           MOVE EXT-FILPATH-BASE    TO REMOVE-PATH-BASE
                                       RA-FILPATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO REMOVE-PATH-MACHINE
                                       RA-FILPATH-MACHINE.
           MOVE EXT-FILPATH-DATA    TO REMOVE-PATH-DATA
                                       RA-FILPATH-DATA.
           MOVE PCODE               TO REMOVE-PATH-PCODE
                                       RA-PCODE.
           MOVE PDATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO REMOVE-PATH-PDATE
                                       RA-DATE.
           MOVE "/0000ERF   "       TO REMOVE-PATH-FILE.
           MOVE REMOVE-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE "CANT RELEASE, ERR REPORT EXISTS" TO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE "RELEASING ALL REPORTS.......... " TO MESS
              PERFORM SEND-LEGEND
              MOVE REMOVE-ALL-CMD TO SYSTEM-BUF
              PERFORM SYSTEM-CALL.

           MOVE PCODE TO LL-PCODE.
           MOVE LS-DIRECTORY-CMD TO SYSTEM-BUF.
           MOVE "I" TO DU-FLAG.
           MOVE "D" TO DU-TYPE.
           PERFORM DISPLAY-DIRECTORY.

           PERFORM CLEAR-LEGEND.
           GO TO REC-DATE.

       PRINT-ALL-REPORTS.
           MOVE "SPOOLING ALL REPORTS........... " TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 1 TO REPORT-NO.
           PERFORM SPOOL-REPORT
              UNTIL REPORT-NO > 30
                OR PATH-NAME(REPORT-NO) = " ".

           GO TO REC-RPTNO.

       PIGGYBACK-ALL-REPORTS.
           MOVE "REPORTS ARE PIGGYBACK PRINTING......" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM PIGGYBACK-REPORT-FILES.
           GO TO REC-RPTNO.

       TRANSFER-REPORT.
           MOVE "TRANSFER REQUEST IN PROGRESS.... " TO MESS.
           PERFORM SEND-LEGEND.

      * TRANSFER TO /USR/EO/(DAY)/(STEP)(NAME)(BRNO)
           MOVE PATH-NAME(REPORT-NO) TO UUCP-NAME.
           MOVE PATH-DAY(REPORT-NO) TO UUCP-DAY.
           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE 1 TO UUCP-BRNO
           ELSE
              MOVE EXT-CONAME-USER-LOC TO UUCP-BRNO.
           MOVE PATH-FILE(REPORT-NO) TO UUCP-FILE.
           MOVE SPACES TO UUCP-DEST.
           STRING GB-UUCP-HOME,UUCP-END-COMMAND DELIMITED BY " "
             INTO UUCP-DEST.
           MOVE UUCP-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE
              "UNABLE TO INVOKE FILE TRANSFER TO HOME OFFICE"
              TO MESS PERFORM SEND-MESS.

           MOVE DU-REPORTS-CMD TO SYSTEM-BUF.
           MOVE "I" TO DU-FLAG.
           MOVE "R" TO DU-TYPE.
           GO TO REC-RPTNO.

      ******************************************
       DIRECTORY-SCAN SECTION.
      ******************************************
           MOVE SPACES TO SAVE-PCODE.
           MOVE ZERO TO ACCUM-BYTES ACCUM-COUNT.

           MOVE "SCANNING DIRECTORY..." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM SYSTEM-CALL.

           PERFORM CLEAR-LEGEND.
           MOVE "S" TO DU-FLAG.

      *************************************
       DISPLAY-SCROLL SECTION.
      *************************************
           IF DU-FLAG = "I"
              PERFORM DIRECTORY-SCAN.

           IF DU-FLAG = "E"
              GO TO DISPLAY-SCROLL-EXIT.

           MOVE SPACES TO DISPLAY-BUF PATH-TABLE.
           MOVE 1 TO SUB.
       DISPLAY-SCROLL-AGAIN.
           IF SUB > 30
              MOVE "MORE..." TO D-END-MORE
              GO TO DISPLAY-SCROLL-EXIT.
           IF DU-FLAG NOT = "S"
              PERFORM READ-LS-FILE-NEXT
           ELSE
              PERFORM CLOSE-LS-FILE
              PERFORM OPEN-LS-FILE
              PERFORM READ-LS-FILE-NEXT
              MOVE " " TO DU-FLAG
              IF IO-FG = 9
                 MOVE "** END **" TO D-END-MORE
                 MOVE "E" TO DU-FLAG
                 GO TO DISPLAY-SCROLL-EXIT.
           IF IO-FG = 9
              PERFORM CLOSE-LS-FILE
              PERFORM OPEN-LS-FILE
              MOVE "** END **" TO D-END-MORE
              GO TO DISPLAY-SCROLL-EXIT.
           IF DU-TYPE = "R"
              IF OPSYS-BUF = "LIN"
                 MOVE SPACES TO LINUX-LS-REC
                 CALL "C$JUSTIFY" USING LS-REC, "L"
                 UNSTRING LS-REC DELIMITED BY ALL " " INTO
                      LINUX-LS-BYTES
                      LINUX-LS-PATHNAME
                 MOVE SPACES TO LS-REC
                 MOVE LINUX-LS-BYTES TO LS-BYTES
                 MOVE LINUX-LS-PATHNAME TO LS-PATHNAME.

           IF DU-TYPE = "R"
              MOVE SPACES TO 5BYTE-OR-6BYTE-FLAG
      * SECURITY AUTO HAD A 5 DIGIT BLOCK SIZE ON THE EMTRRG!
              IF LS-FILLER NOT = SPACES
                 IF LS-PATHNAME-1 = SPACES
                    MOVE LS-REC TO 5BYTE-LS-REC
                    MOVE 5BYTE-LS-PATHNAME TO LS-PATHNAME
                    MOVE "5" TO 5BYTE-OR-6BYTE-FLAG
                 ELSE
                    MOVE LS-REC TO 6BYTE-LS-REC
                    MOVE 6BYTE-LS-PATHNAME TO LS-PATHNAME
                    MOVE "6" TO 5BYTE-OR-6BYTE-FLAG
                 END-IF
              END-IF
              MOVE LS-PATHNAME TO PATH-NAME(SUB)
              MOVE LS-STEP TO D-STEP(SUB)
              MOVE LS-NAME TO D-NAME(SUB)
              MOVE SUB TO D-NO(SUB)
              MOVE "." TO D-DOT(SUB)
              PERFORM GET-EO-REC
      *       MOVE EO-COPIES TO PATH-COPIES(SUB)
              PERFORM BLOCKS-TO-BYTES
              MOVE BYTES TO D-BYTES(SUB)
              ADD 1 TO SUB
           ELSE
              IF LS-PCODE = SAVE-PCODE OR SAVE-PCODE = SPACES
                 MOVE LS-PCODE TO SAVE-PCODE
                 PERFORM ACCUMULATE-BYTES
              ELSE
                 MOVE SAVE-PCODE TO D-PCODE(SUB)
                 MOVE ACCUM-BYTES TO D-PCODE-BYTES(SUB)
                 MOVE ACCUM-COUNT TO D-PCODE-COUNT(SUB)
                 ADD 1 TO SUB
                 MOVE ZERO TO ACCUM-BYTES ACCUM-COUNT
                 MOVE LS-PCODE TO SAVE-PCODE
                 PERFORM ACCUMULATE-BYTES.
           GO TO DISPLAY-SCROLL-AGAIN.

       DISPLAY-SCROLL-EXIT.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       DISPLAY-DIRECTORY SECTION.
      *********************************************
           IF DU-FLAG = "I"
              PERFORM DIRECTORY-SCAN.

           MOVE SPACES TO DISPLAY-BUF PATH-TABLE.
           MOVE 1 TO SUB.
       DISPLAY-DIRECTORY-AGAIN.
           IF DU-FLAG NOT = "S"
              PERFORM READ-DIR-FILE-NEXT
           ELSE
              PERFORM CLOSE-DIR-FILE
              PERFORM OPEN-DIR-FILE
              PERFORM READ-DIR-FILE-NEXT
              MOVE " " TO DU-FLAG
              IF IO-FG = 9
                 MOVE "** END **" TO D-END-MORE
                 MOVE "E" TO DU-FLAG
                 GO TO DISPLAY-DIRECTORY-EXIT.
           IF IO-FG = 9
              PERFORM CLOSE-DIR-FILE
              PERFORM OPEN-DIR-FILE
              MOVE "** END **" TO D-END-MORE
              GO TO DISPLAY-DIRECTORY-EXIT.
           IF SUB > 30
              MOVE "MORE..." TO D-END-MORE
              GO TO DISPLAY-DIRECTORY-EXIT.
           IF DU-TYPE = "D"
              MOVE DIR-DATE TO D-DATE(SUB)
              ADD 1 TO SUB
           ELSE
              IF DIR-PCODE = SAVE-PCODE OR SAVE-PCODE = SPACES
                 MOVE DIR-PCODE TO SAVE-PCODE
                 ADD 1 TO ACCUM-COUNT
              ELSE
                 MOVE SAVE-PCODE TO D-PCODE(SUB)
                 MOVE ACCUM-COUNT TO D-PCODE-COUNT(SUB)
                 MOVE 1 TO ACCUM-COUNT
                 ADD 1 TO SUB
                 MOVE DIR-PCODE TO SAVE-PCODE.
           GO TO DISPLAY-DIRECTORY-AGAIN.

       DISPLAY-DIRECTORY-EXIT.
           IF DU-TYPE = "C" AND SUB NOT > 30
              MOVE SAVE-PCODE TO D-PCODE(SUB)
              MOVE ACCUM-COUNT TO D-PCODE-COUNT(SUB)
              ADD 1 TO SUB.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       ACCUMULATE-BYTES SECTION.
      *********************************************
           MOVE LS-BYTES TO RJ-IN.
           INSPECT RJ-IN REPLACING LEADING SPACES BY ZEROES.
       RJ-LOOP.
           IF RJ-IN NOT NUMERIC
              MOVE RJ-FIRST TO RJ-SWAP
              MOVE ZERO TO RJ-ZERO
              MOVE RJ-SWAP TO RJ-SHIFT
              GO TO RJ-LOOP.
           MOVE RJ-NUM TO BYTES.
           ADD BYTES TO ACCUM-BYTES.
           ADD 1 TO ACCUM-COUNT.
       ACCUM-EXIT.
           EXIT.

      ********************************
       BLOCKS-TO-BYTES SECTION.
      ********************************
           IF LS-FILLER NOT = SPACES
              IF 5BYTE-OR-6BYTE-FLAG = "5"
                 MOVE 5BYTE-LS-BYTES TO RJ-IN
              ELSE
                 MOVE 6BYTE-LS-BYTES TO RJ-IN
           ELSE
              MOVE LS-BYTES TO RJ-IN.
           INSPECT RJ-IN REPLACING LEADING SPACES BY ZEROES.
       BLOCKS-TO-BYTES-LOOP.
           IF RJ-IN NOT NUMERIC
              MOVE RJ-FIRST TO RJ-SWAP
              MOVE ZERO TO RJ-ZERO
              MOVE RJ-SWAP TO RJ-SHIFT
              GO TO BLOCKS-TO-BYTES-LOOP.
           IF OPSYS-BUF NOT = "LIN"
              MULTIPLY RJ-NUM BY BLOCKS-TO-BYTES-FACTOR
                                             GIVING BYTES
           ELSE
              MULTIPLY RJ-NUM BY 1024 GIVING BYTES.

      ***********************************
       PIGGYBACK-REPORT-FILES SECTION.
      ***********************************

      * /USR/REL/SH/PASSPRT (TPUT MC5;CAT *; TPUC MC4
      * PASSPRT FILENAME
           IF REPORT-NO-X NOT = "ALL"
               PERFORM PIGGYBACK-CAT
           ELSE
              MOVE 1 TO REPORT-NO
              PERFORM PIGGYBACK-CAT
                 UNTIL REPORT-NO > 30
                      OR PATH-NAME(REPORT-NO) = " ".


       PIGGYBACK-REPORT-FILES-EXIT.
           EXIT.

      ************************************
       PIGGYBACK-CAT SECTION.
      ************************************

           IF (PATH-STEP(REPORT-NO) NOT = "0000") OR
              (PATH-FILE(REPORT-NO) = "0000RECAP"
                     AND GP-PRINT-GL-RECAP = "Y")
              CALL "C$TOLOWER" USING PASSPRT-COMMAND, VALUE 7
              MOVE PATH-NAME(REPORT-NO) TO PASSPRT-FILENAME
              MOVE PASSPRT-COMMAND TO SYSTEM-BUF
              CALL "SYSTEM" USING SYSTEM-BUF.

           ADD 1 TO REPORT-NO.
           PERFORM SUSPEND 1 TIMES.

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           IF PASSWDW-SEQ = RELEASE-BATCH-SEQ
              MOVE "TO RELEASE THIS BATCH:" TO PASSWDW-PROMPT2
           ELSE
              MOVE "TO ACCESS THIS BATCH:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO EOQRPT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EOQRPT-SYSDATE.
           DISPLAY EOQRPT-SCREEN UPON EOQRPT-WINDOW-HANDLE.
           MOVE EOQRPT-FORM-FIELDS TO WR-BUF.
           MOVE EOQRPT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE EOQRPT-HELP--FILE TO HELP-LINK-FILE.
           MOVE EOQRPT-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/EOQRPT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************
       MISC-PARAGRAPHS SECTION.
      *************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SPOOL-REPORT.
           IF (PATH-STEP(REPORT-NO) NOT = "0000") OR
              (PATH-FILE(REPORT-NO) = "0000RECAP"
                     AND GP-PRINT-GL-RECAP = "Y")
              MOVE PATH-NAME(REPORT-NO) TO PRU-FILE
              PERFORM PRU-OUT.

           ADD 1 TO REPORT-NO.
       GET-EO-REC.
           PERFORM OPEN-EO1-FILE.
           MOVE PCODE TO EO-PCODE.
           MOVE LS-STEP TO EO-STEP.
           PERFORM READ-EO1-FILE.
           IF IO-FG NOT = 0 OR EO-STEP = 0
              MOVE 1 TO EO-COPIES.
           PERFORM CLOSE-EO1-FILE.
       SEND-ACTION.
           MOVE D-ACTION TO WR-BUF.
           MOVE ACTION-LIST-R TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CLEAR-LEGEND.
           MOVE SPACES TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES TO WR-BUF.
           MOVE FLD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SP-CLEAR-DISPLAY.
           MOVE SPACES TO WR-BUF.
           MOVE ACTION-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES TO WR-BUF.
           MOVE HEADING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CLEAR-DISPLAY.
           MOVE SPACES TO WR-BUF.
           MOVE HEADING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************
       IO-ROUTINES SECTION.
      ***************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBEOGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBEO1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       OPEN-LS-FILE.
           PERFORM OPEN-IT.
           MOVE LS-IFN TO E-FILE.
           OPEN INPUT LS-FILE.
           IF IO-FG = 8
              GO TO OPEN-LS-FILE.
           IF IO-FG = 7
              CLOSE LS-FILE
              GO TO OPEN-LS-FILE.
       READ-LS-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LS-IFN TO E-FILE.
           MOVE SPACES TO E-KEYX.
           MOVE SPACES TO LS-REC.
           READ LS-FILE AT END MOVE 9 TO IO-FG.
           IF IO-FG = 0 AND LS-REC = SPACES
              GO TO READ-LS-FILE-NEXT.
       CLOSE-LS-FILE.
           PERFORM CLOSE-IT.
           CLOSE LS-FILE.
       OPEN-DIR-FILE.
           PERFORM OPEN-IT.
           MOVE DIR-IFN TO E-FILE.
           OPEN INPUT DIR-FILE.
           IF IO-FG = 8
              GO TO OPEN-DIR-FILE.
           IF IO-FG = 7
              CLOSE DIR-FILE
              GO TO OPEN-DIR-FILE.
       READ-DIR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DIR-IFN TO E-FILE.
           MOVE SPACES TO E-KEYX.
           MOVE SPACES TO DIR-REC.
           READ DIR-FILE AT END MOVE 9 TO IO-FG.
           IF IO-FG = 0 AND DIR-REC = SPACES
              GO TO READ-DIR-FILE-NEXT.
       CLOSE-DIR-FILE.
           PERFORM CLOSE-IT.
           CLOSE DIR-FILE.

       COPY "LIBGB/FERRORS.CPY".

      ***************************
       END-PROGRAM SECTION.
      ***************************
           PERFORM CLOSE-FILES.
           MOVE DIR-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           MOVE EXIT-PATHNAME TO FORM-PATHNAME.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EOQRPT-SCREEN.
           DESTROY EOQRPT-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
