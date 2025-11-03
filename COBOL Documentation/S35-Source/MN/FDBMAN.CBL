       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FDBMAN.
       DATE-WRITTEN.    061484.
      ******************************************************************
      *
      * IDENTIFICATION:  FORM DATA BASE FILE MAINTENANCE
      *
      * DESCRIPTION   :  FORM DATA BASE FILE
      *                  CONTAINS DATA BASE FOR 2 FILES      
      *                  100 FIELDS PER RECORD                       
      *
      *=================================================================
      * VALID DATA BASE RECORDS (FDB-DBNO):
      *
      *   1 - LN
      *   2 - BW
      *   3 - BR
      *   4 - NO
      *   5 - LP
      *   6 - DL
      *   7 - COLLATERAL TRAILER (LT)
      *   8 - AUTO INSURANCE TRAILER (LT)
      *   9 - GB
      *  10 - LS (REPO TRAILER)
      *  11 - LA (LONG APP)
      *  12 - BA (BORROWER APP)
      *  13 - MA (MINI APP)
      *  21 - NT
      *  22 - IV
      *  23 - NC
      *
      *=================================================================
      * REV:                                            
      * MJD 112597 EXPANDED FD-REC TO 2828 BYTES (ADDED 30)
      *
      * KEC 2016.0118 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/FDBMAN.CBL S34 06/03/21-20:51:43 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01FDB.CPY".
       COPY "LIBLP/LP01FDB_SQL.CPY".
       COPY "LIBLP/LPWSFDB.CPY".

      *=================================================================
       01  CHANGED-FDB-REC             PIC X(FDB-SIZE).
       01  ORIGINAL-FDB-REC            PIC X(FDB-SIZE).

       01  HOLD-FDB1-KEY.
           03  HOLD-FDB-DBNO           PIC 9(02).

       01  SAVE-FD-DBNO                PIC 9(02)     VALUE ZEROES.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ELE             PIC 9(6).
       01  ACTION          PIC X(6).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  SUB             PIC 9(4).
       01  ROW             PIC 99.
       01  MAX-ROW         PIC 99   VALUE 15.
       01  LINE-WK         PIC S9(4) VALUE +0.

       01  FIELD-NO        PIC 9(4) VALUE 0.
       01  TAB-NO          PIC 9(4) VALUE 0.
       01  SAVE-DB-SUB     PIC 9    VALUE 0.
       01  SAVE-FIELD1     PIC 9(4) VALUE 0.
       01  SAVE-FIELD2     PIC 9(4) VALUE 0.
       01  SAVE-TAB1       PIC 9(4) VALUE 0.
       01  SAVE-TAB2       PIC 9(4) VALUE 0.
       01  Q-DB            PIC 99   VALUE 0.
       01  R-DB            PIC 99   VALUE 0.
       01  HOLD-DNAME      PIC X(8) VALUE " ".
       01  HOLD-DOFFSET    PIC 9(4) COMP-3 VALUE 0.
       01  HOLD-DTYPE      PIC 9(2) COMP-3 VALUE 0.
       01  HOLD-DDIGITS    PIC 9(2) COMP-3 VALUE 0.
       01  HOLD-DIGIT      PIC 9(2) COMP-3 VALUE 0.

      ***************************************
      *    RECORD FIELD DESCRIPTIONS        *
      ***************************************
       01  FIELD-BUF               VALUE " ".
           03  FILLER              PIC X(8).
           03  L-010               PIC ZZZZ.
           03  FILLER              PIC X(6).
           03  L-020               PIC X(8).
           03  FILLER              PIC X(8).
           03  L-030               PIC ZZZZ.
           03  FILLER              PIC X(6).
           03  L-040               PIC ZZ.
           03  FILLER              PIC X(8).
           03  L-050               PIC ZZ.
           03  FILLER              PIC X(10).
           03  L-060               PIC ZZ.
           03  FILLER              PIC X(12).

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  D-010       PIC X(2).
           03  D-020       PIC ZZZ9.
           03  D-030       PIC ZZZ9.
           03  D-040       PIC ZZZ9.
           03  D-LINES.
               05  D-050   PIC X(80) OCCURS 15.
       01  DISPLAY-LISTS.
           03  FILLER      PIC X(44)  VALUE
           "CL KO F1 F2 010 020 030 040 050 060 070 080 ".
           03  FILLER      PIC X(28)  VALUE
           "090 100 110 120 130 140 150.".
       01  DISPLAY-LIST REDEFINES DISPLAY-LISTS
                           PIC X(72).

       01  LINE-BUF        VALUE " ".
           03  D-LN        PIC X(80) OCCURS 15.
       01  LINE-LISTS.
           03  FILLER      PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(20) VALUE
           "110 120 130 140 150.".
       01  LINE-LIST REDEFINES LINE-LISTS
                           PIC X(60).

       01  ENTRY-BUF       VALUE " ".
           03  X-060       PIC ZZ.
           03  X-050       PIC ZZ.
           03  X-040       PIC ZZ.
           03  X-030       PIC ZZZZ.
           03  X-020       PIC X(8).
           03  X-010       PIC ZZZZ.
       01  ENTRY-LIST      PIC X(24) VALUE
           "FDE FDI FTY FOF FNM FNO.".

       01  ERRCD           PIC X.

       01  LEGEND-01.
           03  FILLER                  PIC X(23) VALUE
           "F1-DEL, F3-FREE, ^-UP, ".
           03  FILLER                  PIC X     VALUE X'76'. 
           03  FILLER                  PIC X(12) VALUE
           "-DN, F7-DONE".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  FDBMAN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/FDBMAN_DEF.CPY".
       COPY "LIBMN/FDBMAN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/FDBMAN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "FDBMAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FDBMAN-WINDOW-HANDLE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ************************************
       ENTER-KEYS SECTION.
      ************************************
       ENTER-FMNO.
           MOVE "F7-EXIT, ENTER FILE CODE" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           MOVE "EN N020FMNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FMNO.
           IF VDUNUM0 = 0 OR VDUNUM0 > 90
              MOVE "1 TO 90" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FMNO.
           PERFORM GET-KEY-RANGES.
           MOVE SAVE-FD-DBNO TO FDB-DBNO HOLD-FDB-DBNO.
           PERFORM OPEN-FDB1-FILE.
           PERFORM READ-FDB1-FILE.
           PERFORM CLOSE-FDB1-FILE.
           MOVE 1 TO ROW.
           IF IO-FG = 0
              MOVE FDB-REC TO ORIGINAL-FDB-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP
              MOVE "S  A000NEW." TO VDU
              MOVE "*** NEW ***" TO VDUBUF
              PERFORM SCREENIO.
       END-ENTER-KEYS.
           EXIT.

      **********************************
       GET-KEY-RANGES SECTION.
      **********************************
           DIVIDE VDUNUM0 BY 2 GIVING Q-DB REMAINDER R-DB.

           COMPUTE SAVE-DB-SUB = 2 - R-DB.
      * VALUE IS 1 OR 2

           COMPUTE SAVE-FD-DBNO = R-DB + Q-DB.
      * VALUE IS 1,1,2,2,3,3,....45,45

           COMPUTE SAVE-FIELD2 = VDUNUM0 * 100.
      * VALUE IS 100,200,300,....9000.

           COMPUTE SAVE-FIELD1 = SAVE-FIELD2 - 99.
      * VALUE IS 1,101,201,......8901.

           COMPUTE SAVE-TAB2 = SAVE-DB-SUB * 100.
      * VALUE IS 100 OR 200

           COMPUTE SAVE-TAB1 = SAVE-TAB2 - 99
      * VALUE IS 1 OR 101

           MOVE "SN N040F1." TO VDU.
           MOVE SAVE-FIELD1 TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040F2." TO VDU.
           MOVE SAVE-FIELD2 TO VDUNUM0.
           PERFORM SCREENIO.

      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
       DEFINE-FIELDS.
           PERFORM ENTER-FIELD.
       OPTION-ENTRY.
           MOVE "F1-CODE, F2-ID, F7-ENTRY, DELETE, S-SAVE" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "12"
              GO TO OPTION-ENTRY.
           IF VDUSTATUS = "1>"
              GO TO DEFINE-FIELDS.
           IF VDUSTATUS NOT = "00"
              GO TO OPTION-ENTRY.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "     S"
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           GO TO OPTION-ENTRY.
       END-ENTRY-MODULE.
           EXIT.

      **********************************
       ENTER-FIELD SECTION.
      **********************************
       GET-NEXT-FIELD.
           PERFORM NEXT-FREE-FIELD.
       ENTER-FIELDNO.

      * MOVE "F1-DEL, F3-FREE, ^-UP, V-DN, F7-DONE" TO MESS.
           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.

           COMPUTE FIELD-NO = TAB-NO + (SAVE-FD-DBNO - 1) * 200.
           MOVE "S  A040FNO." TO VDU.
           MOVE FIELD-NO TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "ENNN040FNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE SPACE TO FDB-DNAME(TAB-NO)
              MOVE 0 TO FDB-DOFFSET(TAB-NO) FDB-DDIGITS(TAB-NO)
                 FDB-DTYPE(TAB-NO) FDB-DDECIMAL(TAB-NO)
              MOVE SPACES TO WR-BUF
              MOVE ENTRY-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO ENTER-FIELDNO.
           IF VDUSTATUS = "16"
              GO TO GET-NEXT-FIELD.
           IF VDUSTATUS = "1U"
              PERFORM SCROLL-DOWN
              GO TO ENTER-FIELDNO.
           IF VDUSTATUS = "1D"
              PERFORM SCROLL-UP
              GO TO ENTER-FIELDNO.
           IF VDUSTATUS = "1>"
              GO TO EXIT-ENTER-FIELD.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FIELDNO.
           IF VDUNUM0 = 0
              MOVE "S  A040FNO." TO VDU
              MOVE FIELD-NO TO VDUNUM0
              PERFORM SCREENIO
              MOVE FIELD-NO TO VDUNUM0.
           IF VDUNUM0 = 0
              GO TO ENTER-FIELDNO.
           IF VDUNUM0 < SAVE-FIELD1 OR VDUNUM0 > SAVE-FIELD2
              MOVE "OUT OF FIELD # RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FIELDNO.
           MOVE VDUNUM0 TO FIELD-NO.
           COMPUTE TAB-NO =
              FIELD-NO - (FDB-DBNO - 1) * 200.
           PERFORM SETUP-LINE.
           MOVE ENTRY-BUF TO WR-BUF.
           MOVE ENTRY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       ENTER-DNAME.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A080FNM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-FIELDNO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DNAME.
           IF VDUBUF = " " OR VDUBUF = "--FREE--"
              GO TO ENTER-DNAME.
           MOVE VDUBUF TO HOLD-DNAME.
       ENTER-DOFFSET.
           MOVE "ENNN040FOF." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DNAME.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DOFFSET.
           IF VDUNUM0 = 0
              GO TO ENTER-DOFFSET.
           MOVE VDUNUM0 TO HOLD-DOFFSET.
       ENTER-DTYPE.
           MOVE "ENNN020FTY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DOFFSET.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DTYPE.
           IF VDUNUM0 < 1 OR VDUNUM0 > 7
              MOVE "1 TO 7" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-DTYPE.
           MOVE VDUNUM0 TO HOLD-DTYPE.
       ENTER-DDIGITS.
           MOVE "ENNN020FDI." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DTYPE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DTYPE.
           IF VDUNUM0 = 0
              GO TO ENTER-DDIGITS.
           IF HOLD-DTYPE NOT = 1
              IF VDUNUM0 > 12
                 MOVE "NOT > 12 (NUMERIC)" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-DDIGITS.
           MOVE VDUNUM0 TO HOLD-DDIGITS.
       ENTER-DDECIMAL.
           MOVE "ENNN020FDE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DDIGITS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DDECIMAL.
           IF VDUNUM0 > 5
              MOVE "NOT > 5" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-DDECIMAL.
           IF HOLD-DTYPE = 1
              IF VDUNUM0 > 0
                 MOVE "MUST = 0" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-DDECIMAL.
      *    ENTERED AS IN COBOL, STORED AS FORTRAN.
           ADD VDUNUM0 TO HOLD-DDIGITS.
           MOVE HOLD-DNAME TO FDB-DNAME(TAB-NO).
           MOVE HOLD-DOFFSET TO FDB-DOFFSET(TAB-NO).
           MOVE HOLD-DTYPE TO FDB-DTYPE(TAB-NO).
           MOVE HOLD-DDIGITS TO FDB-DDIGITS(TAB-NO).
           MOVE VDUNUM0 TO FDB-DDECIMAL(TAB-NO).
           GO TO GET-NEXT-FIELD.
       EXIT-ENTER-FIELD.
           EXIT.

      **********************************
       NEXT-FREE-FIELD SECTION.
      **********************************
           MOVE SAVE-TAB1 TO TAB-NO.

       NEXT-FIELD-CHECK.
           IF FDB-DNAME(TAB-NO) = " "
              GO TO SEND-FIELDNO.
           IF TAB-NO < SAVE-TAB2
              ADD 1 TO TAB-NO
              GO TO NEXT-FIELD-CHECK.

           MOVE SAVE-TAB1 TO TAB-NO.
       SEND-FIELDNO.
           EXIT.

      **********************************
       SCROLL-UP SECTION.
      **********************************
           IF SAVE-TAB2 < MAX-ROW
              GO TO EXIT-SCROLL-UP.
           COMPUTE LINE-WK = TAB-NO - ROW + MAX-ROW + 15.
           IF LINE-WK > SAVE-TAB2
              MOVE SAVE-TAB2 TO LINE-WK.
           COMPUTE LINE-WK = LINE-WK - MAX-ROW + 1.
           MOVE LINE-WK TO TAB-NO.
           PERFORM SET-LINE-BUF.
           PERFORM DISPLAY-ALL-LINES.
       EXIT-SCROLL-UP.
           EXIT.

      **********************************
       SCROLL-DOWN SECTION.
      **********************************
           IF SAVE-TAB2 < MAX-ROW
              GO TO EXIT-SCROLL-DOWN.

           COMPUTE LINE-WK = TAB-NO - ROW - 15 + 1.
           IF LINE-WK < SAVE-TAB1
              MOVE SAVE-TAB1 TO LINE-WK.
           MOVE LINE-WK TO TAB-NO.
           PERFORM SET-LINE-BUF.
           PERFORM DISPLAY-ALL-LINES.
       EXIT-SCROLL-DOWN.
           EXIT.

      **********************************
       SET-LINE-BUF SECTION.
      **********************************
           MOVE " " TO LINE-BUF.
           MOVE 1 TO ROW.
       NEXT-LINE-IN.
           PERFORM SETUP-LINE.
           IF ROW = MAX-ROW
              GO TO EXIT-SET-LINE-BUF.
           ADD 1 TO ROW.
           IF LINE-WK < SAVE-TAB2
              ADD 1 TO LINE-WK
              MOVE LINE-WK TO TAB-NO
              GO TO NEXT-LINE-IN.
       EXIT-SET-LINE-BUF.
           EXIT.

      **********************************
       SETUP-LINE SECTION.
      **********************************
           MOVE " " TO FIELD-BUF.

           COMPUTE FIELD-NO = TAB-NO + (SAVE-FD-DBNO - 1) * 200.
           MOVE FIELD-NO TO L-010 X-010.
           MOVE FDB-DNAME(TAB-NO) TO L-020 X-020.
           IF L-020 = " "
              MOVE "--FREE--" TO L-020.
           MOVE FDB-DOFFSET(TAB-NO) TO L-030 X-030.
           MOVE FDB-DTYPE(TAB-NO) TO L-040 X-040.
           SUBTRACT FDB-DDECIMAL(TAB-NO) FROM FDB-DDIGITS(TAB-NO)
              GIVING HOLD-DIGIT.
           MOVE HOLD-DIGIT TO L-050 X-050.
           MOVE FDB-DDECIMAL(TAB-NO) TO L-060 X-060.
           MOVE FIELD-BUF TO D-LN(ROW).

      ***********************************
       WRITE-MODULE SECTION.
      ***********************************
           MOVE "UPDATE IN PROGRESS............" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-FDB1-FILE.
           MOVE HOLD-FDB1-KEY TO FDB1-KEY.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE FDB-REC TO CHANGED-FDB-REC.
           IF ORIGINAL-FDB-REC = CHANGED-FDB-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-FDB1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF FDB-REC NOT = ORIGINAL-FDB-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-FDB-REC TO FDB-REC.
           PERFORM REWRITE-FDB1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-FDB1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-FDB1-FILE.

      ***********************************
       DELETE-MODULE SECTION.
      ***********************************
           MOVE "DELETE IN PROGRESS............" TO MESS.
           PERFORM SEND-LEGEND.

           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM OPEN-FDB1-FILE.
           PERFORM READ-FDB1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF FDB-REC NOT = ORIGINAL-FDB-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM TABLE-CLEAR.
              PERFORM REWRITE-FDB1-FILE
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-FDB1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-FDB1-FILE.

      ***********************************
       TABLE-CLEAR SECTION.
      ***********************************
           MOVE 0 TO FDB-KEYLOC(SAVE-DB-SUB).
           MOVE SAVE-TAB1 TO SUB.
       NEXT-TAB-CLEAR.
           MOVE " " TO FDB-DNAME(SUB).
           MOVE 0 TO FDB-DOFFSET(SUB) FDB-DDIGITS(SUB)
              FDB-DTYPE(SUB) FDB-DDECIMAL(SUB).
           IF SUB < SAVE-TAB2
              ADD 1 TO SUB
              GO TO NEXT-TAB-CLEAR.
       EXIT-TAB-CLEAR.
           EXIT.

      *********************************
       NEW-RECORD-SETUP SECTION.
      *********************************
           MOVE 1 TO NEW-FLAG.
           MOVE SPACES TO FDB-REC.
           MOVE 0 TO FDB-KEYLOC(1) FDB-KEYLOC(2) SUB.
       NEXT-FIELD-CLEAR.
           ADD 1 TO SUB.
           MOVE 0 TO FDB-DOFFSET(SUB) FDB-DDIGITS(SUB) FDB-DTYPE(SUB)
              FDB-DDECIMAL(SUB).
           IF SUB < 200
              GO TO NEXT-FIELD-CLEAR.
           MOVE HOLD-FDB1-KEY TO FDB1-KEY.
           MOVE 0 TO ELE.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************

           MOVE SPACES TO DISPLAY-BUF.

           MOVE FDB-CALLID(SAVE-DB-SUB) TO D-010.
           MOVE FDB-KEYLOC(SAVE-DB-SUB) TO D-020.
           MOVE SAVE-FIELD1             TO D-030.
           MOVE SAVE-FIELD2             TO D-040.

           MOVE SAVE-TAB1 TO TAB-NO LINE-WK.
           PERFORM SET-LINE-BUF.
           MOVE LINE-BUF    TO D-LINES.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************

           DISPLAY SPACES UPON FDBMAN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FDBMAN-SCREEN UPON FDBMAN-WINDOW-HANDLE.
           MOVE FDBMAN-FORM-FIELDS TO WR-BUF.
           MOVE FDBMAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE FDBMAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE FDBMAN-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBMN/FDBMAN_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       DISPLAY-ALL-LINES.
           MOVE LINE-BUF TO WR-BUF.
           MOVE LINE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
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

      *************************************
       IO-ROUTINE SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-FDB1-FILE.
       COPY "LIBLP/LPFDBGS_SQL.CPY".
       COPY "LIBLP/LPFDB1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *************************************
       END-ROUTINE SECTION.
      *************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/FDMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FDBMAN-SCREEN.
           DESTROY FDBMAN-WINDOW-HANDLE.
           EXIT PROGRAM.
