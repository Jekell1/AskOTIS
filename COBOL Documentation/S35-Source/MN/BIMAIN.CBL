       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BIMAIN.
       DATE-WRITTEN.    11-02-95.
      ******************************************************************
      *
      *                 (MN)
      *
      *  DESCRIPTION:   BORROWER INDEX FILE MAINTENANCE
      *
      * REV:
      *  BLV 111596 FIXED SCAN; LOTSA BOO-BOOS
      *  BLV 020698 ADDED BACK BI-SALEFIN
      *  MJD ADDED BI-PLCD AND BI-CONVERCD
      *  AMM 070329 CREATED A LOG FIELD CHANGE ROUTINE
      *  BAH 160422 ADDED 11. BI-SMALLOAN FOR DUAL TEST, REGENCY PR#5030
      *   CS 170414 ACTIIVATE 8 DIGIT DATES, BI-POFFDATE,BI-ENTDATE
      *  BAH 171211 ALLOW "X" AND "A" FOR BI-GIFTNET FOR DUAL LOAN TESTING
      *                                               WORLD PR#1288
      *  BAH 180316 ADDED BI-PLDATE 12. CHARGE OFF DATE, WORLD #1308
      *  BLM 211004 INITIALIZE BI KEY FIELDS TO STOP CAST ERROR AND FIX 
      *             CAST ERROR IF ALPHA ENTERED IN SSNO SCAN, ALSO FIX
      *             SCAN KEY TO RESET
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/BIMAIN.CBL S34 10/05/21-13:11:52 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL BI RECORD - BIOR
      *    CHANGED BI RECORD - BICH
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBLP/LP01BI.CPY" REPLACING LEADING "BI" BY "BIOR".
       COPY "LIBLP/LP01BI.CPY" REPLACING LEADING "BI" BY "BICH".

      ******************************************************************
      *
      *    L M C - W O R K E R S
      *
      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-BRNO                PIC 9(4).
           03  LMC-SSNO                PIC 9(9).
           03  LMC-ACCTNO              PIC 9(8).
           03  FILLER                  PIC X(01).
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(8).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(13).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC Z,ZZZ,ZZ9.99-.
           03  LMC-ORG-DATE            REDEFINES LMC-ORG-TEXT
                                       PIC 99/99/99.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(16).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC Z,ZZZ,ZZ9.99-.
           03  LMC-CHG-DATE            REDEFINES LMC-CHG-TEXT
                                       PIC 99/99/99.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.

      ******************************************************************
      *
      *    P R O G R A M - W O R K E R S
      *
      ******************************************************************
       01  ERRCD           PIC X        VALUE SPACES.
       01  CHAN-DATA       VALUE SPACES.
           03  CHAN        PIC X(4).
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).

       01  IO-TYPE         PIC X(3)     VALUE SPACES.
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9        VALUE 0.

       01  HOLD-BI1-KEY.
           03  HOLD-SSNO   PIC 9(9).          
           03  HOLD-ACCTNO PIC 9(8).          
           03  HOLD-OWNBR  PIC 9(4).           
       01  WS-HOLD-KEY.
           03  WS-HOLD-SSNO    PIC 9(9).
           03  WS-HOLD-ACCTNO  PIC 9(8).
           03  WS-HOLD-OWNBR   PIC 9(4).


      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(16).
           03  F02         PIC X(16).
           03  F03         PIC X.
           03  F04         PIC XX.
           03  F05         PIC X.
           03  F06         PIC X.
           03  F07         PIC 99/99/99.
           03  F08         PIC 99/99/99.
           03  F09         PIC X.
           03  F10         PIC X.
           03  F11         PIC X.
           03  F12         PIC 99/99/99.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(32) VALUE
           "010 020 030 040 050 060 070 080".
           03  FILLER      PIC X(16) VALUE
           "090 100 110 120.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       01  MAX             PIC 99   VALUE 12.

       01  SPEC-TABLE.
      * 1. BI-LNAME
           03  FILLER      PIC X(8) VALUE "E  A160 ".
      * 2. BI-FNAME
           03  FILLER      PIC X(8) VALUE "E  A160 ".
      * 3. BI-MAKERCD
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 4. BI-ORGST
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 5. BI-SALEFIN
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 6. BI-GIFTNET
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 7. BI-POFFDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 8. BI-ENTDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 9. BI-PLCD
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 10. BI-CONVERCD
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 11. BI-SMALLOAN
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 12. BI-PLDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 12 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/BIMAIN_DEF.CPY".

       COPY "LIBMN/BIMAIN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  BIMAIN-WINDOW-HANDLE PIC X(10).

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBMN/BIMAIN_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           MOVE "BIMAIN" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS BIMAIN-WINDOW-HANDLE.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-BI-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-BI-MODULE SECTION.
      ******************************************************************

       REC-BI-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.
           MOVE 0 TO BI-SSNO
                     BI-ACCTNO
                     BI-OWNBR.

       REC-BI-SSNO.
           MOVE "EN N090SSNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BI-MODULE.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-BI-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-BI-MODULE-NO.

           IF VDUNUM0 = 0
              GO TO REC-BI-SSNO.

           MOVE VDUNUM0 TO BI-SSNO HOLD-SSNO.

       REC-BI-ACCTNO.
           MOVE "EN N080ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BI-MODULE.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-BI-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-BI-MODULE-NO.

           IF VDUSTATUS = "10" OR "1U"
              GO TO REC-BI-SSNO.

           IF VDUNUM0 = 0
              GO TO REC-BI-ACCTNO.

           MOVE VDUNUM0 TO BI-ACCTNO HOLD-ACCTNO.

       REC-BI-OWNBR.
           MOVE "EN N040OWNBR." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" OR "1U"
              GO TO REC-BI-ACCTNO.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BI-MODULE.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-BI-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-BI-OWNBR.

           IF VDUNUM0 = 0
              GO TO REC-BI-OWNBR.

           MOVE VDUNUM0 TO BI-OWNBR HOLD-OWNBR.
           MOVE BI1-KEY TO HOLD-BI1-KEY.

       READ-BIFILE.
           PERFORM OPEN-BI1-FILE.
           PERFORM READ-BI1-FILE.
           PERFORM CLOSE-BI1-FILE.
           IF IO-FG = 0
              MOVE BI-REC TO BIOR-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-BI-MODULE.
           EXIT.

      *****************************************************
       ENTER-SCANKEY SECTION.
      ********************************************************
           MOVE 0 TO FIRST-SCAN.
           MOVE " ENTER LOAN #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-BI1-FILE.
           PERFORM START-BI1-FILE.

       ENTER-KEY.
           MOVE "E RA210SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.

           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE BI1-KEY TO HOLD-BI1-KEY
                 MOVE BI-REC TO BIOR-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           INSPECT VDUBUF REPLACING ALL " " BY "0".
           MOVE VDUBUF TO WS-HOLD-KEY.
           IF WS-HOLD-KEY NOT NUMERIC
              GO TO ENTER-KEY.
           MOVE WS-HOLD-SSNO   TO HOLD-SSNO   BI-SSNO.
           MOVE WS-HOLD-ACCTNO TO HOLD-ACCTNO BI-ACCTNO.
           MOVE WS-HOLD-OWNBR  TO HOLD-OWNBR  BI-OWNBR.

           PERFORM START-BI1-FILE.

       READ-FILES-NEXT.
           PERFORM READ-BI1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK BI-FILE.

           MOVE "SN N090SSNO." TO VDU.
           MOVE BI-SSNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N080ACCTNO." TO VDU.
           MOVE BI-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040OWNBR." TO VDU.
           MOVE BI-OWNBR TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           PERFORM CLOSE-BI1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

       END-SCANKEY.
           EXIT.

      ******************************************************************
      *
      *  E N T R Y - M O D U L E
      *
      *-----------------------------------------------------------------
      *
      *  NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *             1 - INITIAL NEW RECORD ENTRY
      *             2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.
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
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
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
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

       
           IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
                 IF CHKEY NOT = CHAN
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
                 F-11 F-12
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.
           MOVE VDUBUF TO BI-LNAME.
           GO TO CREATE-ROUTINE.

       F-02.
           MOVE VDUBUF TO BI-FNAME.
           GO TO CREATE-ROUTINE.

       F-03.
           MOVE VDUBUF TO BI-MAKERCD.
           GO TO CREATE-ROUTINE.

       F-04.
           MOVE VDUBUF TO BI-ORGST.
           GO TO CREATE-ROUTINE.

       F-05.
           MOVE VDUBUF TO BI-SALEFIN.
           IF NOT (BI-SALEFIN = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-06.
           MOVE VDUBUF TO BI-GIFTNET.
           IF NOT (BI-GIFTNET = "Y" OR "N" OR "X" OR "A")
              MOVE "ENTER 'Y', 'N' OR 'X'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-07.
           MOVE VDU-DATE-YYYYMMDD TO BI-POFFDATE.
           GO TO CREATE-ROUTINE.

       F-08.
           MOVE VDU-DATE-YYYYMMDD TO BI-ENTDATE.
           GO TO CREATE-ROUTINE.

       F-09.
           IF VDUBUF NOT = "P"
                  AND NOT = "I"
                   AND NOT = "W"
                    AND NOT = " "
              MOVE " ENTER: 'RTN' 'P' 'I' OR 'W'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BI-PLCD.
           GO TO CREATE-ROUTINE.

       F-10.
           IF VDUBUF NOT = "Y"
                  AND NOT = "N"
                   AND NOT = "X"
              MOVE "ENTER: 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BI-CONVERCD.
           GO TO CREATE-ROUTINE.

       F-11.
           IF VDUBUF NOT = "Y"
                  AND NOT = "N"
              MOVE "ENTER: 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BI-SMALLOAN.
           GO TO CREATE-ROUTINE.

       F-12.
           MOVE VDU-DATE-YYYYMMDD TO BI-PLDATE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-BI1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE BI-REC TO BICH-REC.

           IF BIOR-REC = BICH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-BI1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF BI-REC NOT = BIOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE BICH-REC TO BI-REC.
           PERFORM REWRITE-BI1-FILE.
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-BI1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-BI1-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BI-OWNBR                   TO LMC-BRNO.
           MOVE BI-SSNO                    TO LMC-SSNO.
           MOVE BI-ACCTNO                  TO LMC-ACCTNO.
           MOVE " TO "                     TO LMC-TO-X.

       
           IF BIOR-LNAME NOT = BICH-LNAME
             MOVE "01 "                   TO LMC-FIELD
             MOVE "BI LNAME"              TO LMC-DESC
             MOVE BIOR-LNAME              TO LMC-ORG-TEXT
             MOVE BICH-LNAME              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BIOR-FNAME NOT = BICH-FNAME
             MOVE "02 "                   TO LMC-FIELD
             MOVE "BI FNAME"              TO LMC-DESC
             MOVE BIOR-FNAME              TO LMC-ORG-TEXT
             MOVE BICH-FNAME              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BIOR-MAKERCD NOT = BICH-MAKERCD
             MOVE "03 "                   TO LMC-FIELD
             MOVE "MAKERCD "              TO LMC-DESC
             MOVE BIOR-MAKERCD            TO LMC-ORG-TEXT
             MOVE BICH-MAKERCD            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BIOR-ORGST NOT = BICH-ORGST
             MOVE "04 "                   TO LMC-FIELD
             MOVE "ORGST   "              TO LMC-DESC
             MOVE BIOR-ORGST              TO LMC-ORG-TEXT
             MOVE BICH-ORGST              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BIOR-SALEFIN NOT = BICH-SALEFIN
             MOVE "05 "                   TO LMC-FIELD
             MOVE "SALEFIN "              TO LMC-DESC
             MOVE BIOR-SALEFIN            TO LMC-ORG-TEXT
             MOVE BICH-SALEFIN            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BIOR-GIFTNET NOT = BICH-GIFTNET
             MOVE "06 "                   TO LMC-FIELD
             MOVE "GIFTNET "              TO LMC-DESC
             MOVE BIOR-GIFTNET            TO LMC-ORG-TEXT
             MOVE BICH-GIFTNET            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BIOR-POFFDATE NOT = BICH-POFFDATE
             MOVE "07 "                   TO LMC-FIELD
             MOVE "POFFDATE"              TO LMC-DESC
             MOVE BIOR-POFFDATE           TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-ORG-DATE
             MOVE BICH-POFFDATE           TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BIOR-ENTDATE NOT = BICH-ENTDATE
             MOVE "08 "                   TO LMC-FIELD
             MOVE "ENTDATE "              TO LMC-DESC
             MOVE BIOR-ENTDATE            TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-ORG-DATE
             MOVE BICH-ENTDATE            TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BIOR-PLCD NOT = BICH-PLCD
             MOVE "09 "                   TO LMC-FIELD
             MOVE "PLCD    "              TO LMC-DESC
             MOVE BIOR-PLCD               TO LMC-ORG-TEXT
             MOVE BICH-PLCD               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

      D    STOP MESS.
           IF BIOR-CONVERCD NOT = BICH-CONVERCD
             MOVE "10 "                   TO LMC-FIELD
             MOVE "CONVERCD"              TO LMC-DESC
             MOVE BIOR-CONVERCD           TO LMC-ORG-TEXT
             MOVE BICH-CONVERCD           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BIOR-SMALLOAN NOT = BICH-SMALLOAN
             MOVE "11 "                   TO LMC-FIELD
             MOVE "SMALLOAN"              TO LMC-DESC
             MOVE BIOR-SMALLOAN           TO LMC-ORG-TEXT
             MOVE BICH-SMALLOAN           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BIOR-PLDATE NOT = BICH-PLDATE
             MOVE "12 "                   TO LMC-FIELD
             MOVE "PLDATE"                TO LMC-DESC
             MOVE BIOR-PLDATE             TO LMC-ORG-DATE
             MOVE BICH-PLDATE             TO LMC-CHG-DATE
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

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
       DELETE-RECORD.
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-BI1-FILE.
           PERFORM READ-BI1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF ( BI-REC NOT = BIOR-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-BI1-FILE.

      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING BI1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-BI1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-BI-FILE.
           MOVE HOLD-BI1-KEY TO BI1-KEY.
           MOVE 0 TO ELE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE BI-LNAME TO F01.
           MOVE BI-FNAME TO F02.
           MOVE BI-MAKERCD TO F03.
           MOVE BI-ORGST TO F04.
           MOVE BI-SALEFIN TO F05.
           MOVE BI-GIFTNET TO F06.
           MOVE BI-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F07.
           MOVE BI-ENTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F08.
           MOVE BI-PLCD TO F09.
           MOVE BI-CONVERCD TO F10.
           MOVE BI-SMALLOAN TO F11.
           MOVE BI-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F12.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE EXT-CONAME-CONAME TO BIMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BIMAIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BIMAIN-SCREEN UPON BIMAIN-WINDOW-HANDLE.
           MOVE BIMAIN-FORM-FIELDS TO WR-BUF.
           MOVE BIMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE BIMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE BIMAIN-HELP--DIR  TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/BIMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPBICL.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           PERFORM CALL-TMMSG.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BI1-FILE.
       COPY "LIBLP/LPBIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPBI1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BIMAIN-SCREEN.
           DESTROY BIMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
