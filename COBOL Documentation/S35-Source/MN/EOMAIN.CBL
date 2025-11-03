       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EOMAIN.
       DATE-WRITTEN.    07/7/88.
      ******************************************************************
      *  DIR : MN
      *  DESC: BATCH PROCEDURE FILE MAINTENANCE
      *  REV:
      *  102692 BAH ADDED EO-TRANSFER
      *  010395 BAH ALLOW DELETE OF A PROCEDURE
      *  092595 BLL ADD ENTRY OF EO-LEVEL FOR BATCH SECURITY
      *  KEC 101395 THE DESCRIPTION NOWS COMES UP WHEN ENTERING A
      *             CHANGE TO THE DESCRIPTION. A CAHNGE IN THE
      *             SECURITY LEVEL NOW SHOWS UP WHEN COMING BACK FROM
      *             GOING TO "P2".
      *  KEC 010596 MOVED THE STATEMENT:
      *                   "ELSE MOVE EXT-EOBUF-DATA TO EO-PARAMETERS."
      *             TO BE IN THE CONDITION IF THE PARAMETERS WERE
      *             UNCHANGED IN ORDER TO BE SURE THAT AN ALTERED
      *             PARAMETER FROM ENTERING "F1" ON THE SCREEN DOES NOT
      *             GET SAVED.
      *  BLV 980923 ADDED UNLOCKS ON READ-NEXT; EDITING BATCH WAS
      *             LOCKING NEXT PROCEDURE IN THE FILE!
      *  BLF 051013 ADDED REDEFINITION OF FORM-PATHNAME FOR TEST IN
      *             DECLARATIVES
      *  BAH 140501 ADDED DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN, VERYANT
      *   CS 150225 IF YOU F1 FROM STEP NUMBER MOVED SPACES TO PCODE
      *             ADDED TEST TO FORM-RESET IF PCODE WAS NOT SPACES, REDISPLAY
      *             IT.  WHEN YOU RETURN FROM ENTERING P2 CURSOR IS AT STEP 
      *             NUMBER FOR YOU TO CONTINUE BUT MISSING DISPLAY OF PROCEDURE 
      *             CODE.
      *  BAH 150610 CHANGED ACCESS-CALL TO STRING THE PATHNAME
      * KEC 2015.1116 REMOVED USE OF SE-DIR & SN-DIR
      * KEC 2016.0226 FIXED KEYS & RECS FOR EO-REC (HOLD/ORIG/SAVE/WK).
      *               ALSO CHANGED LOGIC ON HOW THE EO-REC WAS BEING CLEARED
      *               WHICH NOW USES LIBGB/GBEOCL (CLEAR-EO-FILE) TO CLEAR
      *               THE RECORD THEN SET GIVEN FIELDS NEEDED.
      * MJD 2016.0829 FIXED PROBLEM WITH FORM-PROGX.   THE EO-PROGRAM WAS
      *               WAS BEING MOVED TO A WORKER WS-PROGRAM WHICH
      *               WAS IN XX/XXXXXX FORMATY LIKE FORM-NAM.  ON 11-16-15
      *               THE DIR AND SLASH WERE COMMENTED OUT OF THE WORKER
      *               LEAVING ONLY 6 CHARACTER PROGRAM NAME, YET THE
      *               ENTIRE WORKER WAS BEING FOVED TO FORM-NAM SHIFTING
      *               THE ENTIRE PROGRAM NAME 3 CHARACTERS TOT HE LEFT AND
      *               CAUSING THE PROGRAM TO NOT BE FOUND BY LOADCO.
      *               RECONVERTED FORM TO ADJUST PATH0-PATH9 LENGTH FROM 
      *               10 (XX/XXXXXXX) TO 7 (XXXXXXX), THE BUFFER WAS
      *               CHANGED BUT THE SCREEN WAS NOT SO EVERYTING WAS
      *               SHIFTED ON THE SCREEN.
      * BAH 2018.0503 PUT FORM-RESET BACK SO F1 WILL CLEAR THE SCREEN
      * BAH 2024.0628 PROCEDURE DESCRIPTION GETTING WIPED OUT AFTER
      *               COMING BACK FROM P2 RANGES. S35Q-77
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/EOMAIN S35 01/25/24-11:46:17 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01EO.CPY".
       COPY "LIBGB/GB01EO_SQL.CPY".
       COPY "LIBGB/GBWSEO.CPY".
       COPY "LIBGB/GB01SN.CPY".
       COPY "LIBGB/GB01SN_SQL.CPY".
       COPY "LIBGB/GBWSSN.CPY".
      *01  ERRCD            PIC X       VALUE SPACES.

       01  CHAN             PIC X(4)    VALUE "CHAN".
       01  SUB              PIC 999     VALUE 0.
       01  ONE              PIC S9.
       01  ELE              PIC 9(2).
       01  ACTION.
           03  CHKEY        PIC X(4).
           03  CHNO         PIC 9(2).
       01  IO-TYPE          PIC X(3).
       01  NEW-FLAG         PIC 9.
       01  FIRST-SCAN       PIC 9       VALUE 0.
       01  FORM-OK          PIC X.
       01  SINGLE-PAGE      PIC X   VALUE "Y".
       01  PROC-EXISTS      PIC X.
       01  ORIGINAL-EO-REC  PIC X(EO-SIZE).
       01  CHANGED-EO-REC   PIC X(EO-SIZE).
       01  SAVE-PROC-DESC   PIC X(30).
       01  SAVE-STAT        PIC XX.
       01  NEXT-TEXT        PIC XXXX  VALUE X"6E657874".
      *        (LOWER-CASE "NEXT")

       01  WORK-4-CHARS.
           03  WS-X4        PIC XXXX.
           03  WS-NUM4      REDEFINES WS-X4
                            PIC 9999.

       01  DELETE-PROMPT.
           03  FILLER       PIC X(40)  VALUE
           "TO DELETE ALL STEPS OF THE PROCEDURE, TY".
           03  FILLER       PIC X(20)  VALUE
           "PE 'YES' AT STEP NO:".
       01  STEP-PROMPT.
           03  FILLER       PIC X(40)  VALUE
           "F1, 'DESC', 'DEL'-DELETE, (RTN)-SCROLL, ".
           03  FILLER       PIC X(24)  VALUE
           "ENTER PROCEDURE STEP NO:".

       01  PCODE            PIC XXX  VALUE SPACES.
       01  SCAN-PCODE       PIC XXX  VALUE SPACES.
       01  STEP-NUMBER      PIC 9999 VALUE 0.

      *    **********************************
      *    *   SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-LIST.
           03  FILLER       PIC X(24)  VALUE
               "010 01B 020 030 040 050.".
       01  DISPLAY-BUF      VALUE SPACES.
           03  F01          PIC X(6).
           03  F02          PIC 9.
           03  F03          PIC X.
           03  F04          PIC X(30).
           03  F05          PIC ZZZ9.
           03  F05X         REDEFINES F05
                            PIC XXXX.

       01  RESET-LIST       PIC X(38)  VALUE
           "STEP 010 020 030 040 050 MSEL SEL.".

       01  LOCK-BUF         PIC X(48).

       01  S-COMMENT        PIC X(30).
      **********************************************
      *    SCROLL BUFFER
      **********************************************
       01  SCROLL-LIST.
          03  FILLER PIC X(28) VALUE "SEQ0 PATH0 COPY0 TRAN0 COM0 ".
          03  FILLER PIC X(28) VALUE "SEQ1 PATH1 COPY1 TRAN1 COM1 ".
          03  FILLER PIC X(28) VALUE "SEQ2 PATH2 COPY2 TRAN2 COM2 ".
          03  FILLER PIC X(28) VALUE "SEQ3 PATH3 COPY3 TRAN3 COM3 ".
          03  FILLER PIC X(28) VALUE "SEQ4 PATH4 COPY4 TRAN4 COM4 ".
          03  FILLER PIC X(28) VALUE "SEQ5 PATH5 COPY5 TRAN5 COM5 ".
          03  FILLER PIC X(28) VALUE "SEQ6 PATH6 COPY6 TRAN6 COM6 ".
          03  FILLER PIC X(28) VALUE "SEQ7 PATH7 COPY7 TRAN7 COM7 ".
          03  FILLER PIC X(28) VALUE "SEQ8 PATH8 COPY8 TRAN8 COM8 ".
          03  FILLER PIC X(28) VALUE "SEQ9 PATH9 COPY9 TRAN9 COM9 ".
          03  FILLER PIC X(8)  VALUE "ENDMORE.".

       01  SCROLL-BUF             VALUE SPACES.
           03  SCROLL-LN          OCCURS 10 TIMES.
               05  SCROLL-STEP    PIC ZZZ9.
               05  SCROLL-PATH.
                   07  SCROLL-PGM PIC X(7).
               05  SCROLL-COPIES  PIC 9.
               05  SCROLL-TRANSF  PIC X.
               05  SCROLL-COMMENT PIC X(38).
               05  SCROLL-ERROR   PIC ZZZ9.
               05  SCROLL-ERROR-X REDEFINES SCROLL-ERROR
                                  PIC XXXX.
           03  SCROLL-END-MORE    PIC X(10).

       01 EOMAIN-WINDOW-HANDLE    PIC X(10).

       COPY "LIBGB/EOBUF_EXT.CPY".


      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 5.
       01  SPEC-TABLE.
      * 1. PROGRAM NAME
           03  FILLER      PIC X(8) VALUE "E  A060 ". 
      * 2. NUMBER OF COPIES
           03  FILLER      PIC X(8) VALUE "ENNN010 ".
      * 3. TRANSFER
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 4. COMMENT
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      * 5. SKIP STEP
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 5 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.


       COPY "LIBWI/EOSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/EOMAIN_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBMN/EOMAIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       LINKAGE SECTION.
       01  FORM-PATHNAME          PIC X(22).
       01  EXIT-PATHNAME      PIC X(22).

       SCREEN SECTION.
       COPY "LIBMN/EOMAIN_SCN.CPY".

       PROCEDURE DIVISION USING FORM-PATHNAME EXIT-PATHNAME.

      ****************************
       MAIN-PROGRAM SECTION.
      ****************************
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EOMAIN" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS EOMAIN-WINDOW-HANDLE.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM OPEN-EO1-FILE.

           PERFORM REC-EO-MODULE.
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

      ****************************************
       REC-EO-MODULE SECTION.
      ****************************************
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           IF PCODE NOT = SPACES
              IF SINGLE-PAGE = "Y"
                 MOVE 1 TO STEP-NUMBER
                 MOVE "N" TO SINGLE-PAGE
                 PERFORM SCROLL-DISPLAY
                 GO TO REC-EO-STEP
              ELSE
                 PERFORM SCROLL-DISPLAY
                 GO TO REC-EO-STEP.

       REC-EO-PCODE.
           PERFORM FORM-RESET.
           MOVE "S  A600MSEL." TO VDU.
           MOVE "F7, F6-SCAN, ENTER PROCEDURE CODE:" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A030PCODE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO REC-EO-MODULE-EXIT.

           IF VDUSTATUS = "1D"
              PERFORM ENTER-SCANKEY
              IF IO-TYPE NOT = " "
                 GO TO REC-EO-PCODE
              ELSE
                 GO TO REC-EO-GET-STEP.

           IF VDUSTATUS = "1<"
              PERFORM EO-SCAN.

           IF VDUSTATUS NOT = "00"
              GO TO REC-EO-PCODE.

           MOVE VDUBUF TO PCODE EO-PCODE.
           IF PCODE < "A  " OR PCODE > "ZZZ"
              GO TO REC-EO-PCODE.

           INSPECT PCODE REPLACING ALL " " BY "X".
           IF PCODE NOT = EO-PCODE
              MOVE "SPACES NOT ALLOWED IN PROCEDURE CODE!" TO MESS
              PERFORM SEND-MESS
              GO TO REC-EO-PCODE.

           MOVE 0 TO STEP-NUMBER EO-STEP.
           PERFORM READ-EO1-FILE.
       REC-EO-DISP-COMMENT.
           IF IO-FG = 0
              MOVE "S  A000PDESC." TO VDU
              MOVE EO-COMMENT TO VDUBUF SAVE-PROC-DESC
              PERFORM SCREENIO
              MOVE "SNNN010LEVEL." TO VDU
              MOVE EO-LEVEL TO VDUNUM0
              PERFORM SCREENIO
              MOVE "N" TO SINGLE-PAGE
              MOVE "Y" TO PROC-EXISTS
              PERFORM SCROLL-DISPLAY
              GO TO REC-EO-GET-STEP
           ELSE
              MOVE "N" TO PROC-EXISTS.
           MOVE "S  A000MSEL." TO VDU.
           MOVE "PROCEDURE NOT FOUND--ENTER DESCRIPTION:"
                                                    TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM ALARM.
           MOVE SPACES TO EO-COMMENT.
           MOVE 1 TO NEW-FLAG.
       REC-EO-COMMENT.
           MOVE "S  A300040." TO VDU.
           MOVE EO-COMMENT TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "R  A300040." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR "1U"
              MOVE SPACES TO WR-BUF
              MOVE RESET-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO REC-EO-PCODE.
           IF VDUSTATUS NOT = "00"
            OR VDUBUF = SPACES
               GO TO REC-EO-COMMENT.

           PERFORM CLEAR-EO-FILE. 
           MOVE PCODE                      TO EO-PCODE.
           MOVE VDUBUF                     TO EO-COMMENT.
           MOVE ZEROES                     TO STEP-NUMBER.

       REC-EO-LEVEL.
           MOVE "ENNN010LEVEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR "1U"
              MOVE SPACES TO WR-BUF
              MOVE RESET-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO REC-EO-DISP-COMMENT.
           IF VDUSTATUS NOT = "00" 
               GO TO REC-EO-LEVEL.
           MOVE VDUNUM0 TO EO-LEVEL.
           IF NEW-FLAG = 1
              PERFORM WRITE-EO1-FILE
           ELSE
              PERFORM REWRITE-EO1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           MOVE SPACES TO WR-BUF.
           MOVE RESET-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO REC-EO-DISP-COMMENT.

       REC-EO-GET-STEP.
           MOVE "S  A000MSEL." TO VDU.
           IF PROC-EXISTS = "Y"
              MOVE STEP-PROMPT TO VDUBUF
           ELSE
              MOVE "F1, ENTER PROCEDURE STEP NO:" TO VDUBUF.
           PERFORM SCREENIO.
       REC-EO-STEP.
           MOVE "R RA040STEP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO REC-EO-MODULE-EXIT.
           IF VDUSTATUS = "10"
              GO TO REC-EO-PCODE.
           IF VDUSTATUS = "1U"
              GO TO REC-EO-PCODE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-EO-STEP.
           MOVE VDUBUF TO WS-X4.
           IF PROC-EXISTS = "Y"
              IF WS-X4 = "DESC"
                 MOVE PCODE TO EO-PCODE
                 MOVE 0 TO EO-STEP
                 PERFORM READ-EO1-FILE
                 MOVE "S  A000MSEL." TO VDU
                 MOVE "ENTER NEW PROCEDURE DESCRIPTION:" TO VDUBUF
                 PERFORM SCREENIO
                 MOVE 0 TO NEW-FLAG
                 GO TO REC-EO-COMMENT.
           IF PROC-EXISTS = "Y"
              IF WS-X4 = " DEL"
                 PERFORM ALARM
                 MOVE "S  A000MSEL." TO VDU
                 MOVE "TYPE 'YES' TO COMPLETE DELETE!!!" TO VDUBUF
                 PERFORM SCREENIO
                 PERFORM ALARM
                 MOVE "R RA040STEP." TO VDU
                 PERFORM SCREENIO
                 IF VDUBUF NOT = " YES"
                    GO TO REC-EO-GET-STEP
                 ELSE
                    PERFORM DELETE-ALL-STEPS
                    MOVE "CLR" TO IO-TYPE
                    MOVE " " TO PCODE
                    GO TO REC-EO-MODULE-EXIT.

           INSPECT WS-X4 REPLACING LEADING SPACES BY ZEROES.
           IF WS-X4 NOT NUMERIC GO TO REC-EO-STEP.
           IF WS-NUM4 = 0
              PERFORM SCROLL-DISPLAY
              GO TO REC-EO-STEP.
           IF WS-NUM4 = 9999
              GO TO REC-EO-STEP.

           MOVE ZERO TO NEW-FLAG.
           MOVE PCODE TO EO-PCODE.
           MOVE WS-NUM4 TO STEP-NUMBER EO-STEP.
           PERFORM READ-EO1-FILE.
           IF IO-FG = 0
              MOVE EO-REC TO ORIGINAL-EO-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.

       REC-EO-MODULE-EXIT.
           EXIT.

      ****************************************
       EO-SCAN SECTION.
      ****************************************
           PERFORM CLOSE-EO1-FILE.

           MOVE SPACES TO EOSCAN-KEY.
           MOVE "WI/EOSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EOSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF EOSCAN-STATUS NOT = "00" OR EOSCAN-KEY = SPACES
              MOVE "X" TO VDUSTATUS
              GO TO EXIT-EO-SCAN.

           MOVE "S  A030PCODE." TO VDU.
           MOVE EOSCAN-KEY TO VDUBUF.
           PERFORM SCREENIO.
           MOVE EOSCAN-KEY TO VDUBUF.
           MOVE "00" TO VDUSTATUS.

       EXIT-EO-SCAN.
           PERFORM OPEN-EO1-FILE.

      ****************************************
       ENTER-SCANKEY SECTION.
      ****************************************
       ENTER-SCAN.
           MOVE " " TO IO-TYPE.
           MOVE "ENTER PROCEDURE CODE, F6 TO SCAN, (RTN):" TO MESS.
           PERFORM CALL-TMMSG.

           MOVE "S  A050SCANDESC." TO VDU.
           MOVE "SKEY:" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A030SCANKEY." TO VDU.
           MOVE "___" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE " " TO SCAN-PCODE.
       ENTER-SCAN-KEY.
           MOVE "E RA030SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCAN-EXIT.
           IF VDUSTATUS = "1<"
              MOVE SCAN-PCODE TO EO-PCODE
              MOVE 9999 TO EO-STEP
              PERFORM START-EO1-FILE
              GO TO ENTER-SCAN-READ-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCAN-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 GO TO ENTER-SCAN-EXIT
              ELSE
                 GO TO ENTER-SCAN-KEY.
           MOVE VDUBUF TO EO-PCODE.
           MOVE 0 TO EO-STEP.
           PERFORM START-EO1-FILE.
       ENTER-SCAN-READ-NEXT.
           PERFORM READ-EO1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCAN-EXIT.
      *    UNLOCK EO-FILE.
           IF EO-STEP NOT = 0
              GO TO ENTER-SCAN-READ-NEXT.
           MOVE "S  A030PCODE." TO VDU.
           MOVE EO-PCODE TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A300PDESC." TO VDU.
           MOVE EO-COMMENT TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO SINGLE-PAGE.
           MOVE EO-PCODE TO PCODE SCAN-PCODE.
           MOVE 0 TO EO-STEP STEP-NUMBER.
           PERFORM SCROLL-DISPLAY.
           MOVE 1 TO FIRST-SCAN.
           MOVE SCAN-PCODE TO EO-PCODE PCODE.
           GO TO ENTER-SCAN-KEY.
       ENTER-SCAN-EXIT.
           MOVE 0 TO FIRST-SCAN.
           MOVE EO-PCODE TO PCODE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           MOVE "SCANKEY SCANDESC." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
       CHECK-NEW-FLAG.
           MOVE "S  A000MSEL." TO VDU.
           IF NEW-FLAG = 1
              MOVE "* * *  N E W   R E C O R D  * * *" TO VDUBUF
           ELSE
              MOVE "F1, P2-RANGES, DELETE, FIELD #, (RTN)-UPD:"
                                 TO VDUBUF.
           PERFORM SCREENIO.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE " " TO CHFG(4).
           MOVE "40" TO EXT-ROUTE-BUF.
           PERFORM EDIT-REPORT-PARAMETERS.
      * NOT BATCHABLE
           IF ( EXT-ROUTE-BUF = "99" )
              DESTROY EXT-EOBUF-WINDOW-HANDLE
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           MOVE 2 TO NEW-FLAG.
           GO TO CHECK-NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "    P2"
              MOVE "50" TO EXT-ROUTE-BUF
              PERFORM EDIT-REPORT-PARAMETERS
              GO TO MODIFY-ROUTINE.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO NOT NUMERIC
            OR CHNO = 0
             OR CHNO > MAX
                GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
           IF NEW-FLAG = 0
              IF ELE = 1
                 GO TO MODIFY-ROUTINE.
       ENTER-ELE.
      *    IF EO-PROGRAM = "EOTRAN"
      *       MOVE "N" TO EO-TRANSFER CHFG(2) CHFG(3)
      *       MOVE 1 TO EO-COPIES
      *    ELSE
              MOVE " " TO CHFG(2) CHFG(3).
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
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
                GO TO GOTO-FIELD.
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
                 DEPENDING ELE.
           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       F-01.
           MOVE VDUBUF TO EO-PROGRAM.
      D    STOP MESS.
           PERFORM CHECK-FORM-COMPATIBILITY.
           IF FORM-OK = "Y"
              MOVE "S  A300040." TO VDU
              MOVE EO-COMMENT TO VDUBUF
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE
           ELSE
              IF FORM-OK = "N"
                 MOVE "PROGRAM NOT FOUND" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 IF FORM-OK = "L"
                    MOVE SPACES TO MESS
                    STRING "USE '" LOCK-BUF "' INSTEAD"
                     DELIMITED BY LOW-VALUES INTO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE
                 ELSE
                    MOVE SPACES TO MESS
                    STRING
                    "PROGRAM NOT AVAILABLE FOR BATCH (STAT="
                      SAVE-STAT ")"
                    DELIMITED BY SIZE INTO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.

      *-----------------------------------------------------------------
       F-02.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO EO-COPIES.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO EO-TRANSFER.
           IF NOT (EO-TRANSFER = "Y" OR "N")
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO EO-COMMENT.
           GO TO CREATE-ROUTINE.
       F-05.
           IF VDUNUM0 = ZERO
              MOVE SPACES TO EO-ERROR-STEP-X
              MOVE "S  A040050." TO VDU
              MOVE NEXT-TEXT TO VDUBUF
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
           IF VDUNUM0 NOT > EO-STEP
              MOVE "YOU MAY NOT SKIP BACKWARDS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO EO-ERROR-STEP.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *****************************************************
       CHECK-FORM-COMPATIBILITY SECTION.
      *****************************************************
      *    IF EO-PROGRAM = "EOTRAN"
      *       MOVE "Y" TO FORM-OK
      *       GO TO CHECK-FORM-COMPATIBILITY-EXIT.

           MOVE EO-PROGRAM TO FORM-PROGX.
           
           MOVE SPACES TO ACCESS-BUF.
           STRING "/",FORM-REL,"/",FORM-OBJ,"/",EO-PROGRAM
              DELIMITED BY " " INTO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "N" TO FORM-OK
              MOVE STAT-99 TO SAVE-STAT
              GO TO CHECK-FORM-COMPATIBILITY-EXIT.

           PERFORM GET-COMMENT-FROM-TITLE.

           MOVE "Y" TO FORM-OK.

       CHECK-FORM-COMPATIBILITY-EXIT.
           EXIT.

      *****************************************************
       EDIT-REPORT-PARAMETERS SECTION.
      *****************************************************

      *    IF EO-PROGRAM = "EOTRAN"
      *       GO TO EDIT-REPORT-PARAMETERS-EXIT.

           MOVE EO-PROGRAM TO FORM-PROGX
           MOVE FORM-PATH TO FORM-PATHNAME.

      * THIS HAD TO BE FLOATING OR THE BATCH PROGRAM WINDOW THAT YOU ARE
      * ABOUT TO CALL HERE WAS GARBLED WITH EOMAIN BEHIND IT STILL!

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE 1
                   AT COL 1
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK
                   HANDLE IS EXT-EOBUF-WINDOW-HANDLE.

           MOVE EO-PARAMETERS TO EXT-EOBUF-DATA.

       COPY "LIBGB/LOADCO.CPY".
       LOAD-POST.
       LOAD-RESTART.

           MOVE "MN/EOMAIN" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

           IF ( EXT-EOBUF-ERRCD = "C" )
              MOVE "PARAMETERS UNCHANGED" TO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE EXT-EOBUF-DATA TO EO-PARAMETERS.

       EDIT-REPORT-PARAMETERS-EXIT.
           EXIT.

      *****************************************************
       GET-COMMENT-FROM-TITLE SECTION.
      *****************************************************
           PERFORM OPEN-SN1-FILE.
           MOVE EO-PROGRAM TO SN-FORM.
           PERFORM READ-SN1-FILE.
           IF IO-GOOD
              MOVE "S  A000040." TO VDU
              MOVE SN-DESC TO VDUBUF EO-COMMENT S-COMMENT
              PERFORM SCREENIO.
           PERFORM CLOSE-SN1-FILE.

      *****************************************************
       SCROLL-DISPLAY SECTION.
      *****************************************************
           MOVE 0 TO SUB.
           MOVE SPACES TO SCROLL-BUF.
           MOVE PCODE TO EO-PCODE.
           MOVE STEP-NUMBER TO EO-STEP.
           IF EO-STEP < 1
              MOVE 1 TO EO-STEP.
           IF EO-STEP = 1
              MOVE "Y" TO SINGLE-PAGE.
           PERFORM START-EO1-FILE.
       SCROLL-DISPLAY-NEXT.
           PERFORM READ-EO1-FILE-NEXT.
           IF IO-FG = 9 OR EO-PCODE NOT = PCODE
      *       UNLOCK EO-FILE
              MOVE "** END **" TO SCROLL-END-MORE
              MOVE ZERO TO STEP-NUMBER
              GO TO SCROLL-DISPLAY-EXIT.
           IF SUB = 10
      *       UNLOCK EO-FILE
              MOVE "MORE..." TO SCROLL-END-MORE
              MOVE EO-STEP TO STEP-NUMBER
              MOVE "N" TO SINGLE-PAGE
              GO TO SCROLL-DISPLAY-EXIT.
           ADD 1 TO SUB.
           MOVE EO-STEP TO SCROLL-STEP(SUB).
           MOVE EO-PROGRAM TO SCROLL-PGM(SUB).
           MOVE EO-COPIES TO SCROLL-COPIES(SUB).
           MOVE EO-TRANSFER TO SCROLL-TRANSF(SUB).
           MOVE EO-COMMENT TO SCROLL-COMMENT(SUB).
           IF EO-ERROR-STEP-X NOT NUMERIC
              MOVE NEXT-TEXT TO SCROLL-ERROR-X(SUB)
           ELSE
              MOVE EO-ERROR-STEP TO SCROLL-ERROR(SUB).
           GO TO SCROLL-DISPLAY-NEXT.

       SCROLL-DISPLAY-EXIT.
           MOVE SCROLL-BUF TO WR-BUF.
           MOVE SCROLL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************
       WRITE-MODULE SECTION.
      *****************************************
           PERFORM OPEN-EO1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE EO-REC TO CHANGED-EO-REC.
           IF ORIGINAL-EO-REC = CHANGED-EO-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-EO1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF EO-REC NOT = ORIGINAL-EO-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-EO-REC TO EO-REC.
           PERFORM REWRITE-EO1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-EO1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-EO1-FILE.

      *****************************************
       DELETE-MODULE SECTION.
      *****************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-EO1-FILE.
           PERFORM READ-EO1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF EO-REC NOT = ORIGINAL-EO-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-EO1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-EO1-FILE.

      *****************************************
       DELETE-ALL-STEPS SECTION.
      *****************************************
           PERFORM OPEN-EO1-FILE.
           MOVE PCODE TO EO-PCODE.
           MOVE 0 TO EO-STEP.
           PERFORM START-EO1-FILE.
       DELETE-ALL-STEPS-NEXT.
           PERFORM READ-EO1-FILE-NEXT.
           IF IO-FG = 9
                  OR EO-PCODE NOT = PCODE
              GO TO DELETE-ALL-STEPS-EXIT.
           PERFORM DELETE-EO1-FILE.
           GO TO DELETE-ALL-STEPS-NEXT.

       DELETE-ALL-STEPS-EXIT.
           PERFORM CLOSE-EO1-FILE.

      *****************************************
       NEW-RECORD-SETUP SECTION.
      *****************************************

           MOVE 1 TO NEW-FLAG.
           MOVE 0 TO ELE.

           PERFORM CLEAR-EO-FILE. 

           MOVE PCODE                      TO EO-PCODE.
           MOVE STEP-NUMBER                TO EO-STEP.

       NEW-RECORD-SETUP-EXIT.
           EXIT.

      *****************************************
       DISPLAY-FIELDS SECTION.
      *****************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE EO-PROGRAM TO F01.
           MOVE EO-COPIES TO F02.
           MOVE EO-TRANSFER TO F03.
           MOVE EO-COMMENT TO F04.
           IF EO-ERROR-STEP-X NOT NUMERIC
              MOVE NEXT-TEXT TO F05X
           ELSE
              MOVE EO-ERROR-STEP TO F05.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/GBEOCL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           MOVE EXT-CONAME-CONAME TO EOMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EOMAIN-SYSDATE.
           DISPLAY EOMAIN-SCREEN UPON EOMAIN-WINDOW-HANDLE.
           MOVE EOMAIN-FORM-FIELDS TO WR-BUF.
           MOVE EOMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF PCODE NOT = SPACES
              MOVE "S  A030PCODE." TO VDU
              MOVE PCODE TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000PDESC." TO VDU
              MOVE SAVE-PROC-DESC TO VDUBUF
              PERFORM SCREENIO.
       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE EOMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE EOMAIN-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/EOMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************
       MISC-PARAGRAPHS SECTION.
      **************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
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

      ***************************
       IO-ROUTINES SECTION.
      ***************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLRP_SQL.CPY".
           PERFORM CLOSE-SN1-FILE.
           PERFORM CLOSE-EO1-FILE.
       COPY "LIBGB/GBSNGS_SQL.CPY".
       COPY "LIBGB/GBEOGS_SQL.CPY".
       COPY "LIBGB/GBEO1IN.CPY".
       COPY "LIBGB/GBSN1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ***************************
       END-ROUTINE SECTION.
      ***************************
           PERFORM CLOSE-EO1-FILE.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EOMAIN-SCREEN.
           DESTROY EOMAIN-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
