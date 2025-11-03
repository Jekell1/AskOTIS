       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FTMAIN.
      ******************************************************************
      *
      *  DESC:   FORM TRAILER FILE MAINTENANCE
      *           THE TRAILER IS NOW PART OF FDFILE, BUT I LEFT THIS
      *           MAINTENANCE AND IT IS READING FDFILE!
      *
      * REV:
      *  BAH 130325 NEW, REGACC PR# 335
      *  BAH 161031 CHANGED TO FDFILE IN A30, LEFT THIS MAINTENANCE WHERE
      *             IT WAS NO THE MENU, BUT IT NOW READS FDFILE
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/FTMAIN.CBL S34 06/03/21-20:54:49 >".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBLP/LP01FD.CPY" REPLACING
            LEADING "FD" BY "FDOR".
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHANGED RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBLP/LP01FD.CPY" REPLACING
            LEADING "FD" BY "FDCH".
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


       01  ERRCD           PIC X VALUE " ".
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).

       01  ELE             PIC 9(3).
       01  HOLD-FD1-KEY        PIC 9(4).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  ONE             PIC S9.
       01  SUB             PIC 999     VALUE 0.
       01  SUB-MAX         PIC 999     VALUE 0.

      *-----------------------------------------------------------------
       01  ORIGINAL-FD-REC      PIC X(FD-SIZE).
       01  CHANGED-FD-REC       PIC X(FD-SIZE).


      ******************************************************************
      *     SCREEN DISPLAY BUFFER
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F-DESC      PIC X(25).
           03  F01         PIC X.
           03  F02         PIC X.
           03  F-STATE     PIC XX OCCURS 50 TIMES.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(13) VALUE
           "DESC 010 020 ".
           03  FILLER      PIC X(40) VALUE
           "030 031 032 033 034 035 036 037 038 039 ".
           03  FILLER      PIC X(40) VALUE
           "040 041 042 043 044 045 046 047 048 049 ".
           03  FILLER      PIC X(40) VALUE
           "050 051 052 053 054 055 056 057 058 059 ".
           03  FILLER      PIC X(40) VALUE
           "060 061 062 063 064 065 066 067 068 069 ".
           03  FILLER      PIC X(40) VALUE
           "070 071 072 073 074 075 076 077 078 079.".

      ******************************************************************
      *     ELEMENT SPECIFICATIONS
      ******************************************************************
       01  MAX             PIC 99   VALUE 7.
       01  SPEC-TABLE.
      * 1. IS FORM RIGHT TO CURE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 2. WHICH STATE TO TEST
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 3. STATES ALLOWED
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 4. STATES ALLOWED
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 5. STATES ALLOWED
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 6. STATES ALLOWED
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 7. STATES ALLOWED
           03  FILLER      PIC X(8) VALUE "E  A020 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 7 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.


      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  FTMAIN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/FTMAIN_DEF.CPY".
       COPY "LIBMN/FTMAIN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/FTMAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N   P R O G R A M    M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "FTMAIN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FTMAIN-WINDOW-HANDLE.


       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM OPEN-ST1-FILE.

           PERFORM REC-FD-MODULE.
           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.

           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.


           GO TO MAIN-MODULE.

      ******************************************************************
       REC-FD-MODULE SECTION.
      ******************************************************************
       REC-FD-MODULE-NO.
           MOVE 0      TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "F7-EXIT, ENTER FORM CODE #:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040FORM." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-FD-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO REC-FD-MODULE-NO.

           IF ( VDUNUM0 = 0 )
              GO TO REC-FD-MODULE-NO.

           MOVE VDUNUM0 TO FD-FMNO HOLD-FD1-KEY.

           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
           IF IO-FG NOT = 0
              MOVE "MASTER FORM DESCRIPTOR RECORD MUST EXIST" TO MESS
              PERFORM SEND-MESS
              GO TO REC-FD-MODULE-NO.

           MOVE "S  A250DESC." TO VDU.
           MOVE FD-DESC TO VDUBUF.
           PERFORM SCREENIO.

           MOVE FD-REC TO ORIGINAL-FD-REC.
           PERFORM DISPLAY-FIELDS.

       END-REC-FD-MODULE.
           EXIT.


      ******************************************************************
      *       NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                  1 - INITIAL NEW RECORD ENTRY
      *                  2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES  TO IO-TYPE.
           MOVE 1       TO ONE.

       CREATE-ROUTINE.
           IF ( NEW-FLAG NOT = 1 )
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1 )
              MOVE 1 TO ELE ONE.

           IF ( ELE NOT > MAX )
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.
           MOVE "F1, 'DELETE', ENTER FIELD # :" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( ACTION = "DELETE" )
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF ( CHNO NOT NUMERIC )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO = 0 )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO > MAX )
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

       ENTER-ELE.

           IF ( CHKEY NOT = ZEROS )
              IF ( CHFG (ELE) = " " )
                 IF ( NEW-FLAG NOT = 1 )
                    GO TO MODIFY-ROUTINE.

           IF ( CHFG(ELE) = "N" )
              IF ( NEW-FLAG = 1 )
                 GO TO CREATE-ROUTINE
              ELSE
                 IF ( CHKEY NOT = EXT-CONAME-DAILY-PASSWORD )
                    GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE 0         TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-F4.
           IF ( VDUSTATUS = "1U" )
             IF ( NEW-FLAG = 1 )
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-F5.
           IF ( VDUSTATUS = "1D" )
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-F6.
           IF ( VDUSTATUS NOT = "1<" )
              GO TO TEST-F1.

      *-----------------------------------------------------------------
       TEST-F1.
           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS
      *
      *-----------------------------------------------------------------
       F-01.
           MOVE VDUBUF TO FD-FORM-RIGHT-TO-CURE.
           IF FD-FORM-RIGHT-TO-CURE NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO FD-TEST-WHICH-STATE.
           IF FD-TEST-WHICH-STATE NOT = "B" AND NOT = "L" AND NOT = " "
              MOVE "B = BORROWER STATE, L = LOAN ORGST" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO FD-STATE(1) ST-CODE.
           PERFORM VERIFY-STATE.
           IF IO-FG NOT = 0
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE 2 TO SUB.
           MOVE 10 TO SUB-MAX.

           IF FD-STATE(1) = SPACES
              PERFORM CLEAR-STATES
           ELSE
              PERFORM ENTER-STATES
              IF VDUSTATUS = "1U"
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO FD-STATE(11) ST-CODE.
           PERFORM VERIFY-STATE.
           IF IO-FG NOT = 0
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE 12 TO SUB.
           MOVE 20 TO SUB-MAX.

           IF FD-STATE(11) = SPACES
              PERFORM CLEAR-STATES
           ELSE
              PERFORM ENTER-STATES
              IF VDUSTATUS = "1U"
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO FD-STATE(21) ST-CODE.
           PERFORM VERIFY-STATE.
           IF IO-FG NOT = 0
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE 22 TO SUB.
           MOVE 30 TO SUB-MAX.

           IF FD-STATE(21) = SPACES
              PERFORM CLEAR-STATES
           ELSE
              PERFORM ENTER-STATES
              IF VDUSTATUS = "1U"
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO FD-STATE(31) ST-CODE.
           PERFORM VERIFY-STATE.
           IF IO-FG NOT = 0
              PERFORM SEND-MESS
              GO TO ENTER-ELE.


           MOVE 32 TO SUB.
           MOVE 40 TO SUB-MAX.

           IF FD-STATE(31) = SPACES
              PERFORM CLEAR-STATES
           ELSE
              PERFORM ENTER-STATES
              IF VDUSTATUS = "1U"
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO FD-STATE(41) ST-CODE.
           PERFORM VERIFY-STATE.
           IF IO-FG NOT = 0
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE 42 TO SUB.
           MOVE 50 TO SUB-MAX.

           IF FD-STATE(41) = SPACES
              PERFORM CLEAR-STATES
           ELSE
              PERFORM ENTER-STATES
              IF VDUSTATUS = "1U"
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ********************************
       ENTER-STATES SECTION.
      ********************************

      * 3. STATES ALLOWED: 030 031 032 033 034 035 036 037 038 039
      * 4. STATES ALLOWED: 040 041 042 043 044 045 046 047 048 049
      * 5. STATES ALLOWED: 050 051 052 053 054 055 056 057 058 059
      * 6. STATES ALLOWED: 060 061 062 063 064 065 066 067 068 069
      * 7. STATES ALLOWED: 070 071 072 073 074 075 076 077 078 079

       ENTER-AGAIN.
           MOVE "E  A020   ." TO VDU.
           MOVE ELE TO VDUROW.
           COMPUTE VDUCOL = SUB - 1.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO EXIT-ENTER-STATES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-AGAIN.

           MOVE VDUBUF TO FD-STATE(SUB) ST-CODE.
           PERFORM VERIFY-STATE.
           IF IO-FG NOT = 0
              PERFORM SEND-MESS
              GO TO ENTER-AGAIN.

           IF SUB < SUB-MAX
              ADD 1 TO SUB
              GO TO ENTER-AGAIN.

       EXIT-ENTER-STATES.
           EXIT.

      ******************************************
       CLEAR-STATES SECTION.
      ******************************************

      * 3. STATES ALLOWED: 030 031 032 033 034 035 036 037 038 039
      * 4. STATES ALLOWED: 040 041 042 043 044 045 046 047 048 049
      * 5. STATES ALLOWED: 050 051 052 053 054 055 056 057 058 059
      * 6. STATES ALLOWED: 060 061 062 063 064 065 066 067 068 069
      * 7. STATES ALLOWED: 070 071 072 073 074 075 076 077 078 079

       CLEAR-AGAIN.
           MOVE "S  A020   ." TO VDU.
           MOVE ELE TO VDUROW.
           COMPUTE VDUCOL = SUB - 1.
           MOVE SPACES TO VDUBUF FD-STATE(SUB).
           PERFORM SCREENIO.

           IF SUB < SUB-MAX
              ADD 1 TO SUB
              GO TO CLEAR-AGAIN.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-FD1-FILE.

           IF ( NEW-FLAG NOT = 0 )
              GO TO WRITE-RECORD.

           MOVE FD-REC TO CHANGED-FD-REC.

           IF ( ORIGINAL-FD-REC = CHANGED-FD-REC )
              GO TO END-WRITE-MODULE.

           PERFORM READ-FD1-FILE.
           IF ( IO-FG NOT = 0 )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF ( FD-REC NOT = ORIGINAL-FD-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-FD-REC TO FD-REC.
           PERFORM REWRITE-FD1-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-FD1-FILE.
           IF ( IO-FG NOT = 0 )
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-FD1-FILE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES                 TO DISPLAY-BUF.

           MOVE FD-DESC                TO F-DESC.
           MOVE FD-FORM-RIGHT-TO-CURE  TO F01.
           MOVE FD-TEST-WHICH-STATE    TO F02.

           MOVE 1 TO SUB.
       DISPLAY-NEXT-STATE.
           MOVE FD-STATE(SUB) TO F-STATE(SUB).
           IF SUB < 50
              ADD 1 TO SUB
              GO TO DISPLAY-NEXT-STATE.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *     C O P Y   L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************

           MOVE EXT-CONAME-CONAME   TO FTMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE  TO FTMAIN-SYSDATE.
           DISPLAY SPACES UPON FTMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FTMAIN-SCREEN UPON FTMAIN-WINDOW-HANDLE.
           MOVE FTMAIN-FORM-FIELDS TO WR-BUF.
           MOVE FTMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE FTMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE FTMAIN-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBMN/FTMAIN_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       VERIFY-STATE.
           MOVE 0 TO IO-FG.
           IF ST-CODE NOT = SPACES
              PERFORM READ-ST1-FILE
              IF IO-FG NOT = 0
                 MOVE "INVALID STATE" TO MESS.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A400MSEL."  TO VDU.
           MOVE MESS            TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       NO-DELETE.
           MOVE " LOANS EXIST FOR THIS CLASS - CANNOT DELETE!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
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
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-FD1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPFD1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "LP/FDMENU" TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY FTMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
