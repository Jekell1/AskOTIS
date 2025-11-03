       IDENTIFICATION DIVISION.
       PROGRAM-ID.   LPBATP.
       DATE-WRITTEN. 042484.
      ******************************************************************
      *
      * DESCRIPTION:   BATCH END OF DAY FORM PRINT DRIVER
      *
      *=================================================================
      * REV:
      *  BLV 040897 CHANGE IP-BEG-FMNO & IP-END-FMNO FROM ZZZ TO 999
      *             SO FORM # RANGE WOULD WORK
      *             (HAD LEADING SPACE WITH ZZZ)
      *
      *  KEC 020698 ADDED LOGIC TO PRINT FORMS THROUGH FORMSXPRESS.
      *  BAH 091898 CHANGED F2-CHANGE PRINT/REPRINT, NOT A TOGGLE ANYMORE
      *  BAH 990723 REMOVED FORMSEXPRESS, ADDED SMART PORT
      *  BAH 020613 ALLOW A SMARTPORT TYPE "E" (EXTRACTION) TO ENTER
      *             A FORM NUMBER RANGE THAT DOES NOT EXIST IN THE
      *             WORK FILE, FORCE "EX" TO THE PROGRAM., REGACC PR# 1800
      *
      * KEC 071024 {PR#03075} REGENCY FINANCE <A15> [LORI MCBEE]
      *            HAD TO FIX A PROBLEM FOR USING LASER (PCL)
      *            DOC DESIGN LETTERS.
      *            PCL001 WAS EQUAL TO THE CHECK PROGRAM (GB) SO IT
      *            WOULD NOT PRINT THE LETTER.
      *
      * KEC 111003 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *            ADDED 'PST' (POSTSCRIPT) OPTION.
      *
      * KEC 2016.0121 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *         REMOVED FT-FILE; MERGED INTO FD-FILE.
      *
      * KEC 2016.0211 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      * KEC 2017.0119 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      *
      * BAH 181115 GLOBAL FMFILE
      * BAH 190313 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * BAH 2022.0622 ONLY ADMIN LOGIN "A" CAN SELECT LONPEX FORMS
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSWKI.CPY".
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *    W K - F I L E
      *
      *=================================================================
      *    DESC: FORM REC SUMMARY WORK FILE
      ******************************************************************
       FD  WK-FILE
           LABEL RECORD IS STANDARD.

      *=================================================================
       01  WK-REC.
           03  WK-KEY.
               05  WK-PRFORM                  PIC 9(04).

      *-----------------------------------------------------------------
           03  WK-RECORD.
               05  WK-ACCTNO                  PIC 9(08)     COMP-3.

               05  WK-COUNTERS.
                   07  WK-PRT-CNT             PIC 9(06)     COMP-3.
                   07  WK-REPRT-CNT           PIC 9(06)     COMP-3.

               05  WK-DESC                    PIC X(30).
               05  WK-PRCONT                  PIC X(01).
               05  WK-PROGRAM-ID              PIC X(02).

      *-----------------------------------------------------------------
               05  WK-PROG-PREFIX             PIC X(01).
                   88  WK-PROG-PREFIX-LONP        VALUE " ".
                   88  WK-PROG-PREFIX-PCL         VALUE "0" "1" "2" "3"
                                                        "4" "5" "6" "7"
                                                        "8" "9".
                   88  WK-PROG-PREFIX-PST         VALUE "0" "1" "2" "3"
                                                        "4" "5" "6" "7"
                                                        "8" "9".
                   88  WK-PROG-PREFIX-PRT         VALUE "0" "1" "2" "3"
                                                        "4" "5" "6" "7"
                                                        "8" "9".
                   88  WK-PROG-PREFIX-VALID       VALUE " " "0" "1" "2"
                                                        "3" "4" "5" "6"
                                                        "7" "8" "9".

      *-----------------------------------------------------------------
               05  WK-FD-OPTION               PIC X(01).
                   88  WK-FD-OPTION-CUSTMT        VALUE "C".
                   88  WK-FD-OPTION-EXTRACTION    VALUE "E".
                   88  WK-FD-OPTION-STANDARD      VALUE "N" " ".
                   88  WK-FD-OPTION-PCL-LASER     VALUE "P".
                   88  WK-FD-OPTION-PST-LASER     VALUE "T".
                   88  WK-FD-OPTION-SMARTPORT     VALUE "Y".
                   88  WK-FD-OPTION-VALID         VALUE "C" "E" "N"
                                                        "P" "Y".

      *-----------------------------------------------------------------
               05  WK-FORM-TYPE               PIC X(04).
                   88  WK-FORM-TYPE-LONP              VALUE "LONP".
                   88  WK-FORM-TYPE-PCL-DOC-DESIGN    VALUE "PCL0".
                   88  WK-FORM-TYPE-PCL               VALUE "PCL1"
                                                            "PCL2"
                                                            "PCL3"
                                                            "PCL4"
                                                            "PCL5"
                                                            "PCL6"
                                                            "PCL7"
                                                            "PCL8"
                                                            "PCL9".
                   88  WK-FORM-TYPE-PST-DOC-DESIGN    VALUE "PST0".
                   88  WK-FORM-TYPE-PST               VALUE "PST1"
                                                            "PST2"
                                                            "PST3"
                                                            "PST4"
                                                            "PST5"
                                                            "PST6"
                                                            "PST7"
                                                            "PST8"
                                                            "PST9".
                   88  WK-FORM-TYPE-PRT               VALUE "PRT0"
                                                            "PRT1"
                                                            "PRT2"
                                                            "PRT3"
                                                            "PRT4"
                                                            "PRT5"
                                                            "PRT6"
                                                            "PRT7"
                                                            "PRT8"
                                                            "PRT9".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LPBATP.CBL S35 02/20/24-15:27:38 >".

       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       01  WK-IFN            PIC X(2)   VALUE "WK".

      *=================================================================
      *
      *    M I S C - W O R K E R S
      *
      *=================================================================
       01  1STFILE-BUF           PIC 99     VALUE 0.
      *01  1STFILE-LIST          PIC X(8)   VALUE "1STFILE.".

       01  ACTION.
           03  FILLER            PIC X(2).
           03  CHNO              PIC 9999.

       01  HOLD-WK-KEYS          VALUE ZEROS.
           03  HOLD-WK-KEY       PIC 9(4)   OCCURS 10.

       01  LEGEND                PIC X(61)  VALUE " ".

       01  MSEL-LIST             PIC X(5)     VALUE "MSEL.".

       01  WK-EOF                PIC X VALUE SPACES.

       01  HOLD-FD1-KEY      PIC X(4)     VALUE "::::".
       01  SUB               PIC 9(6) COMP-3     VALUE 0.
       01  SCROLL-FG         PIC X      VALUE "U".
       01  FORM-FOUND        PIC X      VALUE "N".

      *=================================================================
      *
      *    S C R E E N - B U F F E R
      *
      *=================================================================
       01  SCREEN-BUF.
           03  SCREEN-TABLE    OCCURS 10.
               05  S-FMNO        PIC ZZZ9.
               05  FILLER        PIC X(02).
               05  S-DESCRIPTION PIC X(30).
               05  FILLER        PIC X(02).
               05  S-PROG-PREFIX PIC X(01).
               05  FILLER        PIC X(01).
               05  S-PROGRAM     PIC X(02).
               05  FILLER        PIC X(04).
               05  S-PRT-CNT     PIC Z(06).
               05  FILLER        PIC X(04).
               05  S-REPRT-CNT   PIC Z(06).
               05  FILLER        PIC X(03).
               05  S-PRINT-TYPE  PIC X(10).

      *-----------------------------------------------------------------
       01  SCREEN-LIST.
           03  FILLER            PIC X(40)  VALUE
               "010 011 020 021 030 031 040 041 050 051 ".
           03  FILLER            PIC X(40)  VALUE
               "060 061 070 071 080 081 090 091 100 101.".

       01  MAIN-MESS.
           03  FILLER            PIC X(23) VALUE
               "F1,F2-COMPILE,F3-P/R,^/".
           03  FILLER            PIC X     VALUE X'76'.
           03  FILLER            PIC X(36) VALUE
               ",F5-RANGE,F6-PAPER,F7-EXIT, FORM # :".
       01  MAIN-MESS2.
           03  FILLER            PIC X(15) VALUE
               "F7-EXIT, ^-UP, ".
           03  FILLER            PIC X     VALUE X'76'.
           03  FILLER            PIC X(28) VALUE
               "-DOWN, F2-CHANGE PAPER TYPE:".

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
      *-----------------------------------------------------------------
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LPPBAT_DEF.CPY".
       COPY "LIBLP/LPPBAT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  LPBATP-LINK.
           03  LINK-MKTEMP-BUF            PIC X(35).
           03  FIRST-FLAG            PIC X.
               88  FIRST-TIME        VALUE "Y".
           03  LPPBAT-WINDOW-HANDLE  PIC X(10).

       SCREEN SECTION.
       COPY "LIBLP/LPPBAT_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME PROG-BUF IP-BUF PRU-PATH
                                LPBATP-LINK EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              WK-FILE PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-FM2-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-GB-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LPPBAT" TO VDU-FORM-NAME.

           MOVE LINK-MKTEMP-BUF TO MKTEMP-BUF.

           IF MKTEMP-BUF = SPACES
              MOVE MKTEMP-DEFAULT TO MKTEMP-BUF
              PERFORM MKTEMP-CALL
              MOVE MKTEMP-BUF TO LINK-MKTEMP-BUF.

           PERFORM OPEN-FD1-FILE.
           PERFORM OPEN-FM2-FILE.

           PERFORM OPEN-GB-FILE.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.

      *=================================================================
       TEST-CUT-SHEET-BATCH.

           IF LOAN-KEY NOT = 0
              PERFORM SETUP-CUTSHEET-PRINT
              IF FORM-FOUND = "Y"
                 GO TO FORM-LOADED-YN
              ELSE
                 GO TO TEST-CUT-SHEET-BATCH.

           IF FIRST-TIME
               MOVE "COMPILING FORM PRINT REQUESTS . . ." TO LEGEND
               PERFORM SEND-LEGEND
               MOVE "N" TO FIRST-FLAG
               PERFORM OPEN-WK-FILE-OUTPUT
               PERFORM CLOSE-WK-FILE
               PERFORM OPEN-WK-FILE
               PERFORM SETUP-FORM-WKFILE
               PERFORM CLOSE-WK-FILE.

           PERFORM OPEN-WK-FILE.
           MOVE "U" TO SCROLL-FG.
           PERFORM DISPLAY-FORM-WKFILE.

      *-----------------------------------------------------------------
      *       ENTER FORM NUMBER
      *-----------------------------------------------------------------
       DISPLAY-SEL-PROMPT.
           MOVE MAIN-MESS TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE SPACES TO ERRCD.

      *-----------------------------------------------------------------
       ENTER-FORM.
           MOVE 0 TO 1STFILE-BUF.
           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           IF VDUSTATUS = "10"
              MOVE SPACES TO WR-BUF
              MOVE SCREEN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE "B" TO ERRCD
              GO TO END-ROUTINE.

           IF VDUSTATUS = "12"
              PERFORM OPEN-FM2-FILE
              MOVE FM-PATH-OWNBR TO FM-BRNO
              MOVE 0             TO FM-FMNO
              MOVE SPACES        TO FM-ZIP
              PERFORM START-FM2-FILE
              MOVE "Y" TO FIRST-FLAG
              GO TO TEST-CUT-SHEET-BATCH.

           IF VDUSTATUS = "16"
              PERFORM CHANGE-PRINT-REPRINT
              GO TO DISPLAY-SEL-PROMPT.

           IF VDUSTATUS = "1U"
              MOVE "U" TO SCROLL-FG
              PERFORM DISPLAY-FORM-WKFILE
              GO TO ENTER-FORM.

           IF VDUSTATUS = "1D"
              IF WK-EOF NOT = "Y"
                 MOVE "D" TO SCROLL-FG
                 PERFORM DISPLAY-FORM-WKFILE
                 GO TO ENTER-FORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * F5 FORM # RANGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF VDUSTATUS = "1:"
              PERFORM ENTER-FORM-RANGE
              IF ERRCD = "U"
                 MOVE SPACES TO ERRCD
                 GO TO ENTER-FORM
              ELSE
                 MOVE IP-BEG-FMNO TO CHNO
                 GO TO READ-FORM-NUMBER.

           IF VDUSTATUS = "1<"
              PERFORM CHANGE-PRINT-TYPE
              GO TO DISPLAY-SEL-PROMPT.

      *--->IF VDUSTATUS = "1>"
      *       MOVE "X" TO ERRCD
      *--->   GO TO END-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CLEAR THESE OUT IF F5-FMNO RANGE WAS NOT SELECTED
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO ACTION.
           INSPECT ACTION REPLACING ALL " " BY "0".

           IF CHNO NOT NUMERIC
              GO TO ENTER-FORM.

           PERFORM CLEAR-FORM-RANGE.

      *-----------------------------------------------------------------
       READ-FORM-NUMBER.

           MOVE CHNO TO WK-PRFORM.
           PERFORM READ-WK-FILE.

           IF IO-FG NOT = 0
      * NOT A FORM RANGE
              IF ( IP-BEG-FMNO = ZEROES ) AND
                 ( IP-END-FMNO = ZEROES )
                 GO TO ENTER-FORM
              ELSE
      * FORM RANGE
      * ALLOW RANGE OF 1 - 999 FOR "E" TYPE (EXTRACTION) FORMS
      * LONPEX ALLOWS FOR THIS
                 IF IP-FMTYPE NOT = "E"
                    MOVE "INVALID BEGINNING FORM NO" TO MESS
                    PERFORM SEND-MESS
                    PERFORM CLEAR-FORM-RANGE
                    GO TO ENTER-FORM
                 ELSE
                    IF EXT-CONAME-USER-PROFILE-CD = X"61"
                       MOVE "EX"  TO WK-PROGRAM-ID IP-PROG
                       MOVE IP-BEG-FMNO TO IP-FMNO WK-PRFORM
                                           LPWSFM-EDIT-FM-FORMNO
                       MOVE "L" TO IP-PROG-PREFIX WK-PROG-PREFIX
                       MOVE "E" TO WK-FD-OPTION IP-FD-OPTION
                       MOVE "Y" TO WK-PRCONT
                       GO TO FORM-LOADED-YN
                    ELSE
                       MOVE "ONLY ADMIN CAN SELECT" TO MESS
                       PERFORM SEND-MESS
                       GO TO ENTER-FORM.

      * GOOD READ OF WKFILE
           IF IP-FMTYPE = "E"
              IF WK-PROGRAM-ID NOT = "EX"
                 MOVE "INVALID BEGINNING FORM FOR 'E'" TO MESS
                 PERFORM SEND-MESS
                 PERFORM CLEAR-FORM-RANGE
                 GO TO ENTER-FORM
              ELSE
                 IF EXT-CONAME-USER-PROFILE-CD = X"61"
                    MOVE "EX"  TO WK-PROGRAM-ID IP-PROG
                    MOVE IP-BEG-FMNO TO IP-FMNO WK-PRFORM
                                        LPWSFM-EDIT-FM-FORMNO
                    MOVE "L" TO IP-PROG-PREFIX WK-PROG-PREFIX
                    MOVE "E" TO WK-FD-OPTION IP-FD-OPTION
                    MOVE "Y" TO WK-PRCONT
                    GO TO FORM-LOADED-YN
                 ELSE
                    MOVE "ONLY ADMIN CAN SELECT" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-FORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    RE-LOAD WK-FORM-TYPE DUE TO THE ABOVE RESETTING OF
      *    WK-PROG-PREFIX & WK-FD-OPTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM LOAD-WK-FORM-TYPE.

           IF ( WK-FORM-TYPE-PCL-DOC-DESIGN )
              MOVE STAT---GOOD             TO STAT
           ELSE
           IF ( WK-FORM-TYPE-PST-DOC-DESIGN )
              MOVE STAT---GOOD             TO STAT
           ELSE
           IF ( WK-PROGRAM-ID = GB-CHECK-PROG )
              MOVE STAT---BAD              TO STAT
           ELSE
              MOVE STAT---GOOD             TO STAT.

           IF ( STAT-BAD )
             MOVE "CHECKS CANNOT BE PRINTED IN THIS PROGRAM!" TO MESS
             PERFORM SEND-MESS
             GO TO ENTER-FORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( WK-PRCONT NOT = "Y" )
              MOVE WK-PRFORM TO FD-FMNO
              PERFORM READ-FD1-FILE
              IF ( IO-FG NOT = ZEROES )
                 GO TO ENTER-FORM
              ELSE
                 MOVE FD-1STFILE TO 1STFILE-BUF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * WK-ACCTNO = THE FIRST FM2 RECORD FOUND FOR CUT-SHEET
      *   THE PROBLEM IS THAT IT COULD ALREADY HAVE BEEN PRINTED !!
      *   YOU CANT CHECK FOR PRINT-REPRINT IN SETUP-WORK BECAUSE
      *   YOU CAN CHANGE THE PRINT STATUS AFTER BUILDING THE WORKFILE!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE WK-ACCTNO TO LPWSFM-EDIT-FM-ACCTNO.
           MOVE WK-PRFORM TO LPWSFM-EDIT-FM-FORMNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SO WE MUST TEST FOR CUT-SHEET, THAT THE ACCTNO YOU FOUND IS
      *   THE CORRECT ACCOUNT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( WK-PRCONT NOT = "Y" )
              MOVE LPWSFM-EDIT-FM-ACCTNO TO LOAN-KEY IP-ACCTNO
              MOVE WK-PROGRAM-ID         TO IP-PROG
              MOVE WK-PRFORM             TO IP-FMNO
              MOVE WK-PROG-PREFIX        TO IP-PROG-PREFIX
              MOVE WK-FD-OPTION          TO IP-FD-OPTION
              PERFORM FIRST-CUTSHEET-PRINT
              IF ( FORM-FOUND NOT = "Y" )
                 GO TO TEST-CUT-SHEET-BATCH.

      *-----------------------------------------------------------------
       FORM-LOADED-YN.

           IF ( WK-PRCONT NOT = "Y" )
              MOVE "LOAD NEW FORM, ^-SELECT, 'P' TO PRINT:" TO LEGEND
           ELSE
              MOVE "^-SELECT, ENTER 'P' TO PRINT:" TO LEGEND.

           PERFORM SEND-LEGEND.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1U" )
              IF ( LOAN-KEY = 0 )
                 GO TO DISPLAY-SEL-PROMPT
              ELSE
                 MOVE 0 TO LOAN-KEY BORROWER-KEY
                 PERFORM OPEN-FM2-FILE
                 MOVE FM-PATH-OWNBR TO FM-BRNO
                 MOVE 0             TO FM-FMNO
                 MOVE SPACES        TO FM-ZIP
                 PERFORM START-FM2-FILE
                 GO TO TEST-CUT-SHEET-BATCH.

           IF ( VDUSTATUS NOT = "00" )
              GO TO FORM-LOADED-YN.

           IF ( VDUBUF NOT = "     P" )
              GO TO FORM-LOADED-YN.

           MOVE LPWSFM-EDIT-FM-ACCTNO TO IP-ACCTNO.
           MOVE WK-PROGRAM-ID         TO IP-PROG.
           MOVE WK-PROG-PREFIX        TO IP-PROG-PREFIX.
           MOVE WK-PRFORM             TO IP-FMNO.
           MOVE WK-FD-OPTION          TO IP-FD-OPTION.

           IF ( WK-PRCONT NOT = "Y" )
              MOVE LPWSFM-EDIT-FM-ACCTNO TO LOAN-KEY
              MOVE "L"                   TO ERRCD
              MOVE "0"                   TO CONT-PRINT
           ELSE
              MOVE "1"                   TO CONT-PRINT
              MOVE 0                     TO LOAN-KEY BORROWER-KEY.

           GO TO END-ROUTINE.

      ******************************************************************
       ENTER-FORM-RANGE SECTION.
      ******************************************************************

           MOVE 0 TO IP-BEG-FMNO IP-END-FMNO.
           MOVE SPACES TO IP-FMTYPE.

           MOVE "S  A018P1." TO VDU.
           MOVE "BEGINNING FORM #: " TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A003FM1." TO VDU.
           MOVE "___" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A018P2." TO VDU.
           MOVE "ENDING    FORM #: " TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A003FM2." TO VDU.
           MOVE "___" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A018P3." TO VDU.
           MOVE "FM TYPE(C/S/ /E):" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A001TYPE." TO VDU.
           MOVE "_" TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-FM1.

           MOVE "ENNN030FM1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              MOVE "U" TO ERRCD
              GO TO EXIT-ENTER-FORM-RANGE.

           IF VDUSTATUS = "1>"
              MOVE 0 TO IP-BEG-FMNO IP-END-FMNO
              MOVE SPACES TO IP-FMTYPE
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           IF VDUSTATUS = "10"
              PERFORM CLEAR-FORM-RANGE
              MOVE SPACES TO WR-BUF
              MOVE SCREEN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE "B" TO ERRCD
              GO TO END-ROUTINE.

           IF VDUSTATUS NOT = "00" GO TO ENTER-FM1.

           MOVE VDUNUM0 TO IP-BEG-FMNO.

           IF VDUNUM0 = 0
              GO TO ENTER-FM1.

      *-----------------------------------------------------------------
       ENTER-FM2.

           MOVE "ENNN030FM2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              MOVE 0 TO IP-BEG-FMNO IP-END-FMNO
              MOVE SPACES TO IP-FMTYPE
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           IF VDUSTATUS = "10"
              PERFORM CLEAR-FORM-RANGE
              MOVE SPACES TO WR-BUF
              MOVE SCREEN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE "B" TO ERRCD
              GO TO END-ROUTINE.

           IF VDUSTATUS = "1U" GO TO ENTER-FM1.
           IF VDUSTATUS NOT = "00" GO TO ENTER-FM2.

           MOVE VDUNUM0 TO IP-END-FMNO.

           IF IP-END-FMNO < IP-BEG-FMNO
              MOVE "INVALID FORM RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FM1.

      *-----------------------------------------------------------------
       ENTER-TYPE.

           MOVE "E  A010TYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              MOVE 0 TO IP-BEG-FMNO IP-END-FMNO
              MOVE SPACES TO IP-FMTYPE
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           IF VDUSTATUS = "10"
              PERFORM CLEAR-FORM-RANGE
              MOVE SPACES TO WR-BUF
              MOVE SCREEN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE "B" TO ERRCD
              GO TO END-ROUTINE.

           IF VDUSTATUS = "1U" GO TO ENTER-FM2.
           IF VDUSTATUS NOT = "00" GO TO ENTER-TYPE.

           MOVE VDUBUF TO IP-FMTYPE.

           IF NOT (IP-FMTYPE = "C" OR "S" OR " " OR "E")
              GO TO ENTER-TYPE.

       EXIT-ENTER-FORM-RANGE.
           EXIT.

      ******************************************************************
       SETUP-CUTSHEET-PRINT SECTION.
      ******************************************************************

           MOVE "N" TO FORM-FOUND.

           MOVE SPACES TO FM-REC.

           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE LOAN-KEY      TO LPWSFM-EDIT-FM-ACCTNO.
           IF 1STFILE-BUF = 2
              MOVE 0          TO LPWSFM-EDIT-FM-FORMNO
           ELSE
              MOVE IP-FMNO    TO LPWSFM-EDIT-FM-FORMNO.
           MOVE IP-FMNO       TO FM-FMNO.
           MOVE SPACES        TO FM-ZIP.

           PERFORM START-FM2-FILE.

       SETUP-CUTSHEET-CONT.

           PERFORM READ-FM2-FILE-NEXT.
           IF IO-FG NOT = 0 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO SETUP-CUTSHEET-EXIT.

           IF FM-FMNO NOT = IP-FMNO
              GO TO SETUP-CUTSHEET-EXIT.

      * FIRST FILE BORROWER, THE INITKEY IS ONLY SSNO, TRUNCATED BY 1 IN
      * LOAN KEY
           IF 1STFILE-BUF = 2
              MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY
              IF LPWSFM-EDIT-FM-ACCTNO NOT = LOAN-KEY
                 GO TO SETUP-CUTSHEET-CONT
              ELSE
                 NEXT SENTENCE
           ELSE
              IF FM-INITKEY NOT = LPWSFM-EDIT-FM-INITKEY
                 GO TO SETUP-CUTSHEET-CONT.

      *-----------------------------------------------------------------
       SETUP-CUTSHEET-NEXT.

           PERFORM READ-FM2-FILE-NEXT.
           IF IO-FG NOT = 0 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO SETUP-CUTSHEET-EXIT.

           IF FM-FMNO NOT = IP-FMNO
              GO TO SETUP-CUTSHEET-EXIT.

           IF IP-PRFG = "P"
              IF FM-PRINT = "Y"
                 GO TO SETUP-CUTSHEET-NEXT.

           IF IP-PRFG = "R"
              IF FM-PRINT NOT = "Y"
                 GO TO SETUP-CUTSHEET-NEXT.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.

           IF 1STFILE-BUF = 2
              MOVE 0 TO LPWSFM-EDIT-FM-FORMNO.

           MOVE IP-FMNO TO WK-PRFORM FD-FMNO.
           PERFORM READ-FD1-FILE.

           IF IO-FG NOT = 0
              GO TO SETUP-CUTSHEET-EXIT.

           MOVE FD-PROG TO WK-PROGRAM-ID.
           MOVE FD-PROG-PREFIX TO WK-PROG-PREFIX.
           MOVE " " TO WK-PRCONT.
           MOVE FD-OPTION TO WK-FD-OPTION.

           MOVE "Y" TO FORM-FOUND.
           GO TO END-SETUP-CUTSHEET.

       SETUP-CUTSHEET-EXIT.

           MOVE 0 TO LOAN-KEY BORROWER-KEY.

           PERFORM OPEN-FM2-FILE.
           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE 0             TO FM-FMNO.
           MOVE SPACES        TO FM-ZIP.

           PERFORM START-FM2-FILE.

       END-SETUP-CUTSHEET.
           EXIT.

      ******************************************************************
       FIRST-CUTSHEET-PRINT SECTION.
      ******************************************************************

           MOVE "N" TO FORM-FOUND.
           MOVE SPACES TO FM-REC.

           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE LOAN-KEY      TO LPWSFM-EDIT-FM-ACCTNO.
           MOVE IP-FMNO       TO LPWSFM-EDIT-FM-FORMNO FM-FMNO.
           MOVE SPACES        TO FM-ZIP.

           PERFORM START-FM2-FILE.

       FIRST-CUTSHEET-NEXT.

           PERFORM READ-FM2-FILE-NEXT.
           IF IO-FG NOT = 0 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO FIRST-CUTSHEET-EXIT.

           IF FM-FMNO NOT = LPWSFM-EDIT-FM-FORMNO
              GO TO FIRST-CUTSHEET-EXIT.

           IF IP-PRFG = "P"
              IF FM-PRINT = "Y"
                 GO TO FIRST-CUTSHEET-NEXT.

           IF IP-PRFG = "R"
              IF FM-PRINT NOT = "Y"
                 GO TO FIRST-CUTSHEET-NEXT.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.

           MOVE "Y" TO FORM-FOUND.
           GO TO END-FIRST-CUTSHEET.

      *-----------------------------------------------------------------
       FIRST-CUTSHEET-EXIT.

           MOVE 0 TO LOAN-KEY BORROWER-KEY.

           PERFORM OPEN-FM2-FILE.
           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE 0             TO FM-FMNO.
           MOVE SPACES        TO FM-ZIP.

           PERFORM START-FM2-FILE.

       END-FIRST-CUTSHEET.
           EXIT.

      ******************************************************************
       SETUP-FORM-WKFILE SECTION.
      ******************************************************************

           MOVE SPACES        TO FM-REC.
           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE 0             TO FM-FMNO.
           MOVE SPACES        TO FM-ZIP.

           PERFORM START-FM2-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO END-SETUP-FORM-WKFILE.

       SETUP-FM-WKFILE.
           PERFORM READ-FM2-FILE-NEXT.
           IF ( IO-FG NOT = ZEROES ) OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO END-SETUP-FORM-WKFILE.

           IF ( BRANCH NOT = ZEROES )
              IF ( BRANCH NOT = FM-BRNO )
                 GO TO SETUP-FM-WKFILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * DONT INCLUDE NOTE CHECKS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( FM-FMNO = 5 )
              MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY
              IF ( LPWSFM-EDIT-FM-TYPE = "N " )
                 GO TO SETUP-FM-WKFILE.

           MOVE FM-FMNO TO WK-PRFORM.
           PERFORM READ-WK-FILE.
           IF ( IO-FG = ZEROES )
              PERFORM UPDATE-WK-CNT
              PERFORM REWRITE-WK-FILE
              IF ( IO-FG = ZEROES )
                 GO TO SETUP-FM-WKFILE
               ELSE
                 GO TO IO-ERROR.

           MOVE SPACES  TO WK-REC.
           MOVE ZEROES  TO WK-PRT-CNT WK-REPRT-CNT.
           MOVE FM-FMNO TO WK-PRFORM  FD-FMNO.

           IF ( HOLD-FD1-KEY NOT = FD1-KEY )
              MOVE FD1-KEY TO HOLD-FD1-KEY
              PERFORM READ-FD1-FILE
              IF IO-FG NOT = 0
                 MOVE "::" TO FD-PROG
                 MOVE "Y" TO FD-PRCONT
                 MOVE "INVALID MISSING FORM RECORD" TO FD-DESC.

              MOVE FD-PROG TO WK-PROGRAM-ID.
      *-----------------------------------------------------------------
      *    ALL STANDARD PRINT PROGRAMS FOR NOTES ARE "N"
      *    ALL STANDARD PRINT PROGRAMS FOR LOANS ARE "L"
      *    ALL STANDARD PRINT PROGRAMS FOR A/P ARE "A"
      *-----------------------------------------------------------------
           IF ( FD-TYPE NOT = IP-FMCODE )
              IF ( FD-TYPE NOT = "A" )
              GO TO SETUP-FM-WKFILE.

           MOVE FD-PROG-PREFIX TO WK-PROG-PREFIX.
           MOVE FD-PRCONT      TO WK-PRCONT.
           MOVE FD-DESC        TO WK-DESC.
           MOVE FD-OPTION      TO WK-FD-OPTION.
           MOVE FM-INITKEY     TO LPWSFM-EDIT-FM-INITKEY.

           IF ( LPWSFM-EDIT-FM-ACCTNO NUMERIC )
             MOVE LPWSFM-EDIT-FM-ACCTNO TO WK-ACCTNO
           ELSE
             MOVE ZEROES         TO WK-ACCTNO.


           PERFORM UPDATE-WK-CNT.
           PERFORM LOAD-WK-FORM-TYPE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SETUP-FM-WKFILE.

      *=================================================================
       UPDATE-WK-CNT.

           IF ( FM-PRINT = "Y" )
              ADD 1 TO WK-REPRT-CNT
           ELSE
              ADD 1 TO WK-PRT-CNT.

       END-SETUP-FORM-WKFILE.
           EXIT.

      ******************************************************************
       DISPLAY-FORM-WKFILE SECTION.
      ******************************************************************

           MOVE SPACES TO WK-REC SCREEN-BUF WK-EOF.
           MOVE ZEROS TO HOLD-WK-KEYS.
           MOVE 0 TO WK-PRFORM SUB.

           IF SCROLL-FG = "D"
              GO TO READ-WKFILE-NEXT.

           PERFORM START-WK-FILE.

       READ-WKFILE-NEXT.

           PERFORM READ-WK-FILE-NEXT.

           IF ( IO-FG NOT = ZEROES )
              MOVE "Y" TO WK-EOF
              GO TO XFER-ACCUM-WKFILE.

           ADD  1              TO SUB.
           MOVE WK-KEY         TO HOLD-WK-KEY   (SUB).
           MOVE WK-PRFORM      TO FD-FMNO S-FMNO(SUB).
           MOVE WK-DESC        TO S-DESCRIPTION (SUB).
           MOVE WK-PROG-PREFIX TO S-PROG-PREFIX (SUB).
           MOVE WK-PROGRAM-ID  TO S-PROGRAM     (SUB).
           MOVE WK-PRT-CNT     TO S-PRT-CNT     (SUB).
           MOVE WK-REPRT-CNT   TO S-REPRT-CNT   (SUB).

           IF ( WK-PRCONT NOT = "Y" )
              MOVE "CUT-SHEET " TO S-PRINT-TYPE(SUB)
           ELSE
              MOVE "CONTINUOUS" TO S-PRINT-TYPE(SUB).

           IF ( SUB < 10 )
              GO TO READ-WKFILE-NEXT.

       XFER-ACCUM-WKFILE.
           MOVE SCREEN-BUF TO WR-BUF.
           MOVE SCREEN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       CHANGE-PRINT-TYPE SECTION.
      ******************************************************************

      * "F7-EXIT, ^-UP, V-DOWN, F2-CHANGE PAPER TYPE:"
           MOVE MAIN-MESS2 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE 0 TO SUB.

       CHANGE-PR-CONT.

           ADD 1 TO SUB.

           IF SUB > 10
              GO TO CHANGE-PR-EXIT.

           MOVE "E  A010" TO VDU.
           MOVE SUB TO VDUROW.
           MOVE 2 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO CHANGE-PR-EXIT.

           IF VDUSTATUS = "1U"
              IF SUB NOT = 1
                 SUBTRACT 2 FROM SUB
                 GO TO CHANGE-PR-CONT
              ELSE
                 SUBTRACT 1 FROM SUB
                 GO TO CHANGE-PR-CONT.

           IF VDUSTATUS = "1D"
              ADD 1 TO SUB
              IF HOLD-WK-KEY(SUB) NOT = ZEROS
                 SUBTRACT 1 FROM SUB
                 GO TO CHANGE-PR-CONT
              ELSE
                 SUBTRACT 2 FROM SUB
                 GO TO CHANGE-PR-CONT.

           IF VDUSTATUS = "12"
              PERFORM CHANGE-PRINT-STATUS
              SUBTRACT 1 FROM SUB
              GO TO CHANGE-PR-CONT.

           SUBTRACT 1 FROM SUB.
           GO TO CHANGE-PR-CONT.

       CHANGE-PRINT-STATUS.

           MOVE HOLD-WK-KEY(SUB) TO WK-KEY.
           PERFORM READ-WK-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           IF WK-PRCONT = "Y"
              MOVE SPACES TO WK-PRCONT
           ELSE
              MOVE "Y" TO WK-PRCONT.

           PERFORM REWRITE-WK-FILE.
           MOVE "S  A010" TO VDU.
           MOVE SUB TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.

           IF WK-PRCONT = "Y"
              MOVE "CONTINUOUS" TO VDUBUF
           ELSE
              MOVE "CUT-SHEET " TO VDUBUF.

           PERFORM SCREENIO.

       CHANGE-PR-EXIT.
           EXIT.

      ******************************************************************
       CHANGE-PRINT-REPRINT SECTION.
      ******************************************************************
       CHANGE-P-R.

           MOVE "ENTER 'P'-PRINT NEW FORMS, 'R'-REPRINT FORMS:"
                                                     TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PRFG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" GO TO CHANGE-P-R.

           MOVE VDUBUF TO IP-PRFG.
           IF IP-PRFG NOT = "P" AND NOT = "R"
              GO TO CHANGE-P-R.

       CHANGE-P-R-EXIT.
           EXIT.

      ******************************************************************
       LOAD-WK-FORM-TYPE SECTION.
      ******************************************************************

           MOVE SPACES                     TO WK-FORM-TYPE.

           IF ( WK-FD-OPTION-PCL-LASER ) AND
              ( WK-PROG-PREFIX-PCL     )
              STRING "PCL", WK-PROG-PREFIX
                     DELIMITED BY SIZE   INTO WK-FORM-TYPE
           ELSE
           IF ( WK-FD-OPTION-PST-LASER ) AND
              ( WK-PROG-PREFIX-PST     )
              STRING "PST", WK-PROG-PREFIX
                     DELIMITED BY SIZE   INTO WK-FORM-TYPE
           ELSE
           IF ( WK-PROG-PREFIX-PRT )
              STRING "PRT", WK-PROG-PREFIX
                     DELIMITED BY SIZE   INTO WK-FORM-TYPE
           ELSE
              MOVE "LONP"                  TO WK-FORM-TYPE.

       LOAD-WK-FORM-TYPE-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY LPPBAT-SCREEN UPON LPPBAT-WINDOW-HANDLE.
           MOVE LPPBAT-FORM-FIELDS TO WR-BUF.
           MOVE LPPBAT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE LPPBAT-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPPBAT-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LPPBAT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       CLEAR-FORM-RANGE.

           MOVE 0 TO IP-BEG-FMNO IP-END-FMNO.
           MOVE SPACES TO IP-FMTYPE.

           MOVE "S  A018P1." TO VDU.
           MOVE " " TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A003FM1." TO VDU.
           MOVE "   " TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A018P2." TO VDU.
           MOVE "  " TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A003FM2." TO VDU.
           MOVE "   " TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A018P3." TO VDU.
           MOVE " " TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A001TYPE." TO VDU.
           MOVE " " TO VDUBUF.
           PERFORM SCREENIO.

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES TO LEGEND.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBWKIN.CPY".
      *COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

           IF ERRCD = "X"
              MOVE MKTEMP-BUF TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE.
       EXIT-PROG.

       STOP-RUN.
           EXIT PROGRAM.
