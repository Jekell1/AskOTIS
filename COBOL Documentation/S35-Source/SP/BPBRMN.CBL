       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BPBRMN.
      **************************************************************************
      *  DIR :   SP
      *  DESC:   BRANCH BATCH FILE FLAG SETUP
      *REV:
      * GLF 102997 REMOVED RESET OF FIELDS
      * MJD 112197 EXPANDED BR-REC TO 850 BYTES
      * JTG 001218 ADDED THE FOLLOWING FIELDS, RE: GP-ALLOW-SET-FOR-NONCASH
      *            MERCURY PR#239
      *            03  BR-BP-PAYOFF-NONCASH      PIC X.
      *            03  BR-BP-PAYOFF-OV           PIC X.
      *            03  BR-LBOX-PAYOFF-NONCASH    PIC X.
      *            03  BR-LBOX-PAYOFF-OV         PIC X.
      * JTG 010119 CHANGED ...PAYOFF-OV FIELDS TO HAVE VALUES OF 'O' OR 'E'
      *            RE: GP-ALLOW-SET-FOR-NONCASH
      * JTG 010814 CREATED NOW OPTION 'M' FOR BR-BP-PAYOFF-OV
      *            AND BR-LBOX-PAYOFF-OV WHICH WILL CREATE 'OV' TRANSACTION
      *            BUT WILL NOT CREATE A CHECK REQUEST            MERCURY PR#256
      * JTG 040826 ALLOW BATCH PAYMENTS TO ACCOUNTS WITH OTHER BALANCE BUT
      *            NOT APPLY APPLY MONIES TO OTHER BALANCE          REGACC #0070
      * JTG 050921 ADDED NEW OPTION (BR-BP-PL = 'P') FOR CHARGE OFFS TO
      *            APPLY ALL MONIES TO PRINCIPAL (DEFERRING LATE CHARGES,
      *            INTEREST AND OTHER TO OWING) AND REJECTING PAYMENT AMOUNT
      *            IF GREATER THAN PAYOFF                           REGACC #0110
      * BLM 080528 CHANGED CHECK ON GP-ALLOW-SET-FOR-NONCASH IN DISPLAY-FIELDS-
      *            IF THIS FLAG WAS NOT "Y", IT FORCED "N" TO THE FIELDS
      *            BR-BP-PAYOFF-NONDASH & BR-LBOX-PAYOFF-NONCASH & DISPLAYED
      *            THEM AS "N".  SO YOU DID NOT KNOW WHAT WAS REALLY IN THE
      *            FIELDS - THEY MIGHT BE Y BUT THE GP FLAG WAS N, SO THEY
      *            DISPLAY AS N, YOU SEE THIS, HIT F1 & THEY ARE STILL Y!
      * BLM 130212 ALLOW VALUE OF "2" FOR BR-BP-ALLOW-PMT-ACCT-OTHBAL &
      *            BR-LBOX-ALLOW-PMT-ACCT-OTHBAL.  THIS WILL WORK EXACTLY
      *            LIKE "N" EXCEPT ACCT WITH AN O2 BALANCE WILL BE REJECTED
      *            IN BATCH PROCESSING, SOAUTO PR# 4145
      * BAH 180515 ADDED ERRLOG
      * BLM 210624 MOVE VDUNUM0 TO BR-NO AND BR-NO
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./SP/BPBRMN.CBL S35 09/20/22-13:06:48 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       01  ERRCD           PIC X       VALUE SPACES.

       01  ONE             PIC S9.
       01  ELE            PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-TIME      PIC 9       VALUE 0.

      * ORIGINAL RECORD BROR-REC
       COPY "LIBGB/GB01BR.CPY" REPLACING LEADING "BR" BY "BROR".
      * CHANGED  RECORD BRCH-REC
       COPY "LIBGB/GB01BR.CPY" REPLACING LEADING "BR" BY "BRCH".

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
           03  LMC-DESC                PIC X(13).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(14).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC Z,ZZZ,ZZ9.99-.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZ9-.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(14).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC Z,ZZZ,ZZ9.99-.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZ9-.




      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  BRNAME      PIC X(20).
           03  F01         PIC 99.
           03  F02         PIC 9(5).
           03  F03         PIC X.
           03  F031        PIC X.
           03  F04         PIC X.
           03  F041        PIC X.
           03  F05         PIC X.
           03  F051        PIC X.
           03  F06         PIC X.
           03  F061        PIC X.
           03  F07         PIC X.
           03  F071        PIC X.
           03  F08         PIC X.
           03  F081        PIC X.
           03  F09         PIC X.
           03  F091        PIC X.
           03  F10         PIC X.
           03  F101        PIC X.
           03  F11         PIC X.
           03  F111        PIC X.
           03  F12         PIC ZZZ.
           03  F121        PIC ZZZ.
           03  F13         PIC ZZZ.
           03  F131        PIC ZZZ.
           03  F14         PIC X.
           03  F141        PIC X.
           03  F15         PIC X.
           03  F151        PIC X.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(48) VALUE
           "BRNAME 010 020 030 031 040 041 050 051 060 061 ".
           03  FILLER      PIC X(48) VALUE
           "070 071 080 081 090 091 100 101 110 111 120 121 ".
           03  FILLER      PIC X(24) VALUE
           "130 131 140 141 150 151.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS #1
      *    **********************************
       01  MAX             PIC 99   VALUE 15.
       01  SPEC-TABLE.
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
           03  FILLER      PIC X(8) VALUE "ENNN050 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
           03  FILLER      PIC X(8) VALUE "E  A010 ".
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 15.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".

       01  BPBRMN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BPBRMN_DEF.CPY".
       COPY "LIBSP/BPBRMN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/BPBRMN_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************************
       MAIN-PROGRAM SECTION.
      ***********************************************
       COPY "LIBGB/CHKSEC.CPY".
           MOVE "BPBRMN" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS BPBRMN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
           PERFORM GET-GPENV.
           IF GP-ALLOW-SET-FOR-NONCASH = "Y"
              MOVE " " TO CHFG(14) CHFG(15)
           ELSE
              MOVE "N" TO CHFG(14) CHFG(15).

       MAIN-MODULE.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
            ELSE
              PERFORM FORM-RESET.

           PERFORM REC-BR-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           GO TO MAIN-MODULE.

      ***********************************************
       REC-BR-MODULE SECTION.
      ***********************************************
       REC-BR-MODULE-NO.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-BR-MODULE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-BR-MODULE-NO.
           IF VDUNUM0 = 0
             GO TO REC-BR-MODULE-NO.

           MOVE VDUNUM0 TO BR-NO BR-NO.
       READ-BRANCH.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 0
              MOVE BR-REC TO BROR-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM BRANCH-NOT-FOUND
              GO TO REC-BR-MODULE-NO.

       END-REC-BR-MODULE.
           EXIT.

      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
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
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
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
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
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
              GO TO GOTO-FIELD1.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD1.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06
                 F-07 F-08 F-09 F-10 F-11 F-12
                 F-13 F-14 F-15
                 DEPENDING ON ELE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUNUM0 TO BR-BP-NEXT-BATCH.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM0 TO BR-BP-BRANCH.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO BR-BP-NOPAYOFF.
           IF BR-BP-NOPAYOFF NOT = "Y" AND
              BR-BP-NOPAYOFF NOT = "N"
                 PERFORM Y-OR-N
                 GO TO ENTER-ELE.
       F-031.
           MOVE "E  A0100 1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-031.
           MOVE VDUBUF TO BR-LBOX-NOPAYOFF.
           IF NOT (BR-LBOX-NOPAYOFF = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO F-031.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO BR-BP-ODDPAY.
           IF BR-BP-ODDPAY NOT = "Y" AND
              BR-BP-ODDPAY NOT = "N"
                 PERFORM Y-OR-N
                 GO TO ENTER-ELE.
       F-041.
           MOVE "E  A0100 1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-041.
           MOVE VDUBUF TO BR-LBOX-ODDPAY.
           IF NOT (BR-LBOX-ODDPAY = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO F-041.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO BR-BP-LC.
           IF BR-BP-LC NOT = "Y" AND
              BR-BP-LC NOT = "N"
                 PERFORM Y-OR-N
                 GO TO ENTER-ELE.
       F-051.
           MOVE "E  A0100 1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-051.
           MOVE VDUBUF TO BR-LBOX-LC.
           IF NOT (BR-LBOX-LC = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO F-051.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO BR-BP-NOPRIN.
           IF BR-BP-NOPRIN NOT = "Y" AND
              BR-BP-NOPRIN NOT = "N"
                 PERFORM Y-OR-N
                 GO TO ENTER-ELE.
       F-061.
           MOVE "E  A0100 1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-061.
           MOVE VDUBUF TO BR-LBOX-NOPRIN.
           IF NOT (BR-LBOX-NOPRIN = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO F-061.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO BR-BP-FLASH.
           IF BR-BP-FLASH NOT = "Y" AND
              BR-BP-FLASH NOT = "N"
                 PERFORM Y-OR-N
                 GO TO ENTER-ELE.
       F-071.
           MOVE "E  A0100 1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-071.
           MOVE VDUBUF TO BR-LBOX-FLASH.
           IF NOT (BR-LBOX-FLASH = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO F-071.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO BR-BP-ACTIONCD.
           IF BR-BP-ACTIONCD NOT = "Y" AND
              BR-BP-ACTIONCD NOT = "N"
                 PERFORM Y-OR-N
                 GO TO ENTER-ELE.
       F-081.
           MOVE "E  A0100 1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-081.
           MOVE VDUBUF TO BR-LBOX-ACTIONCD.
           IF NOT (BR-LBOX-ACTIONCD = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO F-081.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUBUF TO BR-BP-PL.
           IF NOT (BR-BP-PL = "Y" OR "N" OR "P")
              MOVE "ENTER 'Y' OR 'N' OR 'P' ONLY " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       F-091.
           MOVE "E  A0100 1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-091.
           MOVE VDUBUF TO BR-LBOX-PL.
           IF NOT (BR-LBOX-PL = "Y" OR "N" OR "P")
              MOVE "ENTER 'Y' OR 'N' OR 'P' ONLY " TO MESS
              PERFORM SEND-MESS
              GO TO F-091.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUBUF TO BR-BP-JD.
           IF NOT (BR-BP-JD = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
       F-101.
           MOVE "E  A0100 1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-101.
           MOVE VDUBUF TO BR-LBOX-JD.
           IF NOT (BR-LBOX-JD = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO F-101.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUBUF TO BR-BP-ALLOW-PMT-ACCT-OTHBAL.
           IF NOT (BR-BP-ALLOW-PMT-ACCT-OTHBAL = "Y" OR "N" OR "2")
              MOVE "ENTER 'Y' OR 'N' OR '2' ONLY " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       F-111.
           MOVE "E  A0110 1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-111.
           MOVE VDUBUF TO BR-LBOX-ALLOW-PMT-ACCT-OTHBAL.
           IF NOT (BR-LBOX-ALLOW-PMT-ACCT-OTHBAL = "Y" OR "N" OR "2")
              MOVE "ENTER 'Y' OR 'N' OR '2' ONLY " TO MESS
              PERFORM SEND-MESS
              GO TO F-111.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM0 TO BR-BP-MAXCON.
       F-121.
           MOVE "ENNN0300 1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-121.
           MOVE VDUNUM0 TO BR-LBOX-MAXCON.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUNUM0 TO BR-BP-MAXREC.
       F-131.
           MOVE "ENNN0300 1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-131.
           MOVE VDUNUM0 TO BR-LBOX-MAXREC.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUBUF TO BR-BP-PAYOFF-NONCASH.
           IF GP-ALLOW-SET-FOR-NONCASH NOT = "Y"
              IF BR-BP-PAYOFF-NONCASH NOT = "N"
                 MOVE "BASED ON GLOBAL PARAMETER YOU MUST ENTER 'N'"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF NOT (BR-BP-PAYOFF-NONCASH = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
       F-141.
           MOVE "E  A0100 1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-141.
           MOVE VDUBUF TO BR-LBOX-PAYOFF-NONCASH.
           IF GP-ALLOW-SET-FOR-NONCASH NOT = "Y"
              IF BR-LBOX-PAYOFF-NONCASH NOT = "N"
                 MOVE "BASED ON GLOBAL PARAMETER YOU MUST ENTER 'N'"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO F-141.
           IF NOT (BR-LBOX-PAYOFF-NONCASH = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO F-141.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUBUF TO BR-BP-PAYOFF-OV.
           IF NOT (BR-BP-PAYOFF-OV = "O" OR "M" OR "E")
              MOVE "ENTER 'O' OR 'M' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       F-151.
           MOVE "E  A0100 1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-151.
           MOVE VDUBUF TO BR-LBOX-PAYOFF-OV.
           IF NOT (BR-LBOX-PAYOFF-OV = "O" OR "M" OR "E")
              MOVE "ENTER 'O' OR 'M' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO F-151.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ************************************************
       WRITE-MODULE SECTION.
      ************************************************
           PERFORM OPEN-BR-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE BR-REC TO BRCH-REC.
           IF BROR-REC = BRCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF BR-REC NOT = BROR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE BRCH-REC TO BR-REC.
           PERFORM REWRITE-BR-FILE.

           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           MOVE BR-NO TO BR-NO.
           PERFORM WRITE-BR-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-BR-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

           IF BROR-BP-NEXT-BATCH NOT = BRCH-BP-NEXT-BATCH
             MOVE "01 "                    TO LMC-FIELD
             MOVE "NEXT BATCH # "          TO LMC-DESC
             MOVE BROR-BP-NEXT-BATCH       TO LMC-ORG-NUM
             MOVE BRCH-BP-NEXT-BATCH       TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-BRANCH NOT = BRCH-BP-BRANCH
             MOVE "02 "                    TO LMC-FIELD
             MOVE "TAPE BRANCH #"          TO LMC-DESC
             MOVE BROR-BP-BRANCH       TO LMC-ORG-NUM
             MOVE BRCH-BP-BRANCH       TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-NOPAYOFF NOT = BRCH-BP-NOPAYOFF
             MOVE "03 "                    TO LMC-FIELD
             MOVE "ALLOW PAYOFF "          TO LMC-DESC
             MOVE BROR-BP-NOPAYOFF         TO LMC-ORG-TEXT
             MOVE BRCH-BP-NOPAYOFF         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-LBOX-NOPAYOFF NOT = BRCH-LBOX-NOPAYOFF
             MOVE "031"                    TO LMC-FIELD
             MOVE "ALLOW PAYOFF "          TO LMC-DESC
             MOVE BROR-LBOX-NOPAYOFF         TO LMC-ORG-TEXT
             MOVE BRCH-LBOX-NOPAYOFF         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-ODDPAY NOT = BRCH-BP-ODDPAY
             MOVE "04 "                    TO LMC-FIELD
             MOVE "ALLOW IRREGPY"          TO LMC-DESC
             MOVE BROR-BP-ODDPAY         TO LMC-ORG-TEXT
             MOVE BRCH-BP-ODDPAY         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-LBOX-ODDPAY NOT = BRCH-LBOX-ODDPAY
             MOVE "041"                    TO LMC-FIELD
             MOVE "ALLOW IRREGPY"          TO LMC-DESC
             MOVE BROR-LBOX-ODDPAY         TO LMC-ORG-TEXT
             MOVE BRCH-LBOX-ODDPAY         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-LC NOT = BRCH-BP-LC
             MOVE "05 "                    TO LMC-FIELD
             MOVE "ALLOW LATECHG"          TO LMC-DESC
             MOVE BROR-BP-LC         TO LMC-ORG-TEXT
             MOVE BRCH-BP-LC         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-LBOX-LC NOT = BRCH-LBOX-LC
             MOVE "051"                    TO LMC-FIELD
             MOVE "ALLOW LATECHG"          TO LMC-DESC
             MOVE BROR-LBOX-LC         TO LMC-ORG-TEXT
             MOVE BRCH-LBOX-LC         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-NOPRIN NOT = BRCH-BP-NOPRIN
             MOVE "06 "                    TO LMC-FIELD
             MOVE "NO PRIN REDUC"          TO LMC-DESC
             MOVE BROR-BP-NOPRIN         TO LMC-ORG-TEXT
             MOVE BRCH-BP-NOPRIN         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-LBOX-NOPRIN NOT = BRCH-LBOX-NOPRIN
             MOVE "061"                    TO LMC-FIELD
             MOVE "NO PRIN REDUC"          TO LMC-DESC
             MOVE BROR-LBOX-NOPRIN         TO LMC-ORG-TEXT
             MOVE BRCH-LBOX-NOPRIN         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-FLASH NOT = BRCH-BP-FLASH
             MOVE "07 "                    TO LMC-FIELD
             MOVE "FLASH MESSAGE"          TO LMC-DESC
             MOVE BROR-BP-FLASH         TO LMC-ORG-TEXT
             MOVE BRCH-BP-FLASH         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-LBOX-FLASH NOT = BRCH-LBOX-FLASH
             MOVE "071"                    TO LMC-FIELD
             MOVE "FLASH MESSAGE"          TO LMC-DESC
             MOVE BROR-LBOX-FLASH         TO LMC-ORG-TEXT
             MOVE BRCH-LBOX-FLASH         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-ACTIONCD NOT = BRCH-BP-ACTIONCD
             MOVE "08 "                    TO LMC-FIELD
             MOVE "WITH ACTIONCD"          TO LMC-DESC
             MOVE BROR-BP-ACTIONCD         TO LMC-ORG-TEXT
             MOVE BRCH-BP-ACTIONCD         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-LBOX-ACTIONCD NOT = BRCH-LBOX-ACTIONCD
             MOVE "081"                    TO LMC-FIELD
             MOVE "WITH ACTIONCD"          TO LMC-DESC
             MOVE BROR-LBOX-ACTIONCD         TO LMC-ORG-TEXT
             MOVE BRCH-LBOX-ACTIONCD         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-PL NOT = BRCH-BP-PL
             MOVE "09 "                    TO LMC-FIELD
             MOVE "ALLOW PMT PL "          TO LMC-DESC
             MOVE BROR-BP-PL         TO LMC-ORG-TEXT
             MOVE BRCH-BP-PL         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-LBOX-PL NOT = BRCH-LBOX-PL
             MOVE "091"                    TO LMC-FIELD
             MOVE "ALLOW PMT PL "          TO LMC-DESC
             MOVE BROR-LBOX-PL         TO LMC-ORG-TEXT
             MOVE BRCH-LBOX-PL         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-JD NOT = BRCH-BP-JD
             MOVE "10 "                    TO LMC-FIELD
             MOVE "ALLOW PMT JDG"          TO LMC-DESC
             MOVE BROR-BP-JD         TO LMC-ORG-TEXT
             MOVE BRCH-BP-JD         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-LBOX-JD NOT = BRCH-LBOX-JD
             MOVE "101"                    TO LMC-FIELD
             MOVE "ALLOW PMT JDG"          TO LMC-DESC
             MOVE BROR-LBOX-JD         TO LMC-ORG-TEXT
             MOVE BRCH-LBOX-JD         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-ALLOW-PMT-ACCT-OTHBAL NOT =
                                   BRCH-BP-ALLOW-PMT-ACCT-OTHBAL
             MOVE "11 "                    TO LMC-FIELD
             MOVE "PMT W/ OTHBAL"          TO LMC-DESC
             MOVE BROR-BP-ALLOW-PMT-ACCT-OTHBAL  TO LMC-ORG-TEXT
             MOVE BRCH-BP-ALLOW-PMT-ACCT-OTHBAL  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-LBOX-ALLOW-PMT-ACCT-OTHBAL NOT =
                                     BRCH-LBOX-ALLOW-PMT-ACCT-OTHBAL
             MOVE "111"                    TO LMC-FIELD
             MOVE "PMT W/ OTHBAL"          TO LMC-DESC
             MOVE BROR-LBOX-ALLOW-PMT-ACCT-OTHBAL   TO LMC-ORG-TEXT
             MOVE BRCH-LBOX-ALLOW-PMT-ACCT-OTHBAL   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-MAXCON NOT = BRCH-BP-MAXCON
             MOVE "12 "                    TO LMC-FIELD
             MOVE "MAX CONTRACTL"          TO LMC-DESC
             MOVE BROR-BP-MAXCON         TO LMC-ORG-NUM
             MOVE BRCH-BP-MAXCON         TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-LBOX-MAXCON NOT = BRCH-LBOX-MAXCON
             MOVE "121"                    TO LMC-FIELD
             MOVE "MAX CONTRACTL"          TO LMC-DESC
             MOVE BROR-LBOX-MAXCON         TO LMC-ORG-NUM
             MOVE BRCH-LBOX-MAXCON         TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-MAXREC NOT = BRCH-BP-MAXREC
             MOVE "13 "                    TO LMC-FIELD
             MOVE "MAX RECENCY  "          TO LMC-DESC
             MOVE BROR-BP-MAXREC         TO LMC-ORG-NUM
             MOVE BRCH-BP-MAXREC         TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-LBOX-MAXREC NOT = BRCH-LBOX-MAXREC
             MOVE "131"                    TO LMC-FIELD
             MOVE "MAX RECENCY  "          TO LMC-DESC
             MOVE BROR-LBOX-MAXREC         TO LMC-ORG-NUM
             MOVE BRCH-LBOX-MAXREC         TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-PAYOFF-NONCASH NOT = BRCH-BP-PAYOFF-NONCASH
             MOVE "14 "                    TO LMC-FIELD
             MOVE "POFF NONCASH "          TO LMC-DESC
             MOVE BROR-BP-PAYOFF-NONCASH         TO LMC-ORG-TEXT
             MOVE BRCH-BP-PAYOFF-NONCASH         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-LBOX-PAYOFF-NONCASH NOT = BRCH-LBOX-PAYOFF-NONCASH
             MOVE "141"                    TO LMC-FIELD
             MOVE "POFF NONCASH "          TO LMC-DESC
             MOVE BROR-LBOX-PAYOFF-NONCASH         TO LMC-ORG-TEXT
             MOVE BRCH-LBOX-PAYOFF-NONCASH         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-BP-PAYOFF-OV NOT = BRCH-BP-PAYOFF-OV
             MOVE "15 "                    TO LMC-FIELD
             MOVE "OV OVER POFF "          TO LMC-DESC
             MOVE BROR-BP-PAYOFF-OV         TO LMC-ORG-TEXT
             MOVE BRCH-BP-PAYOFF-OV         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF BROR-LBOX-PAYOFF-OV NOT = BRCH-LBOX-PAYOFF-OV
             MOVE "151"                    TO LMC-FIELD
             MOVE "OV OVER POFF "          TO LMC-DESC
             MOVE BROR-LBOX-PAYOFF-OV         TO LMC-ORG-TEXT
             MOVE BRCH-LBOX-PAYOFF-OV         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

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

      ************************************************
       DISPLAY-FIELDS SECTION.
      ************************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE BR-NAME                TO BRNAME.
           MOVE BR-BP-NEXT-BATCH       TO F01.
           MOVE BR-BP-BRANCH           TO F02.
           MOVE BR-BP-NOPAYOFF         TO F03.
           MOVE BR-LBOX-NOPAYOFF       TO F031.
           MOVE BR-BP-ODDPAY           TO F04.
           MOVE BR-LBOX-ODDPAY         TO F041.
           MOVE BR-BP-LC               TO F05.
           MOVE BR-LBOX-LC             TO F051.
           MOVE BR-BP-NOPRIN           TO F06.
           MOVE BR-LBOX-NOPRIN         TO F061.
           MOVE BR-BP-FLASH            TO F07.
           MOVE BR-LBOX-FLASH          TO F071.
           MOVE BR-BP-ACTIONCD         TO F08.
           MOVE BR-LBOX-ACTIONCD       TO F081.
           MOVE BR-BP-PL               TO F09.
           MOVE BR-LBOX-PL             TO F091.
           MOVE BR-BP-JD               TO F10.
           MOVE BR-LBOX-JD             TO F101.
           MOVE BR-BP-ALLOW-PMT-ACCT-OTHBAL   TO F11.
           MOVE BR-LBOX-ALLOW-PMT-ACCT-OTHBAL TO F111.
           MOVE BR-BP-MAXCON           TO F12.
           MOVE BR-LBOX-MAXCON         TO F121.
           MOVE BR-BP-MAXREC           TO F13.
           MOVE BR-LBOX-MAXREC         TO F131.
           MOVE BR-BP-PAYOFF-NONCASH   TO F14.
           MOVE BR-LBOX-PAYOFF-NONCASH TO F141.
           MOVE BR-BP-PAYOFF-OV        TO F15.
           MOVE BR-LBOX-PAYOFF-OV      TO F151.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO BPBRMN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BPBRMN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BPBRMN-SCREEN UPON BPBRMN-WINDOW-HANDLE.
           MOVE BPBRMN-FORM-FIELDS TO WR-BUF.
           MOVE BPBRMN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE BPBRMN-HELP--FILE TO HELP-LINK-FILE.
           MOVE BPBRMN-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/BPBRMN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************************************
       MISC-PARAGRAPHS SECTION.
      *******************************************************
       COPY "LIBGB/ACCESS.CPY".
       Y-OR-N.
           MOVE "ENTER 'Y' OR 'N' ONLY " TO MESS.
           PERFORM SEND-MESS.
       BRANCH-NOT-FOUND.
           MOVE "BRANCH NOT FOUND!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *******************************************************
       IO-ROUTINES SECTION.
      *******************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *******************************************************
       END-ROUTINE SECTION.
      *******************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BPBRMN-SCREEN.
           DESTROY BPBRMN-WINDOW-HANDLE.
           EXIT PROGRAM.
